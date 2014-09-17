/** -*- c-basic-offset: 2 -*-
 * nsga2.c - C implementation of NSGA-II
 *
 * Portions of this file are taken from the reference implementation
 * by K. Deb et. al.
 *
 * See http://www.iitk.ac.in/kangal/codes.shtml for the original code.
 *
 * Authors:
 *  Heike Trautmann  <trautmann@statistik.uni-dortmund.de>
 *  Detlef Steuer    <detlef.steuer@hsu-hamburg.de>
 *  Olaf Mersmann    <olafm@statistik.uni-dortmund.de>
 */


/*
 * Bernd Bischl:
 * This is currently a straight-forward copy from
 * http://cran.r-project.org/web/packages/mco/index.html
 * Version 1.0.12
 * I adapted the code in the following way:
 * - only touched "evaluate_pop" in C code
 * - the objective function is expected to do vectorized eval of the whole population
 *   to minimize overhead (we use this to eval via predict / infill crits)
 * - disabled constraint handling (dont need it now and was lazy)
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <R.h>
#include <R_ext/Utils.h>
#include <Rinternals.h>
#include <assert.h>

#define INF 1.0e14
#define EPS 1.0e-14

/* Global structures */
typedef struct {
  int rank;
  double *input;
  double *objective;
  double *constraint;
  double constraint_violation;
  double crowding_distance;
} individual;

typedef struct {
  size_t size;
  individual *ind;
} population;

typedef struct lists {
  int index;
  struct lists *parent;
  struct lists *child;
} list;

typedef struct {
  size_t input_dim;
  size_t objective_dim;
  size_t constraint_dim;
  double crossing_probability;
  double mutation_probability;
  double eta_c;
  double eta_m;
  size_t input_mutations;
  size_t input_crossings;
  double *lower_input_bound;
  double *upper_input_bound;
  SEXP environment;
  SEXP function_call;
  SEXP constraint_call;
} nsga2_ctx;

static void individual_alloc(nsga2_ctx *ctx, individual *i) {
  i->input = (double *)R_alloc(ctx->input_dim, sizeof(double));
  i->objective = (double *)R_alloc(ctx->objective_dim, sizeof(double));
  if (ctx->constraint_dim > 0) {
    i->constraint = (double *)R_alloc(ctx->constraint_dim, sizeof(double));
  } else {
    i->constraint = NULL;
  }
}

static population *population_alloc(nsga2_ctx *ctx, size_t population_size) {
  size_t i;

  population *p = (population *)R_alloc(1, sizeof(population));
  p->size = population_size;
  p->ind = (individual *)R_alloc(p->size, sizeof(individual));
  for (i = 0; i < p->size; ++i)
    individual_alloc(ctx, &(p->ind[i]));
  return p;
}

static void population_initialize(nsga2_ctx *ctx, population *pop) {
  GetRNGstate();
  int i, j;
  for (i = 0; i < pop->size; ++i)  {
    for (j=0; j<ctx->input_dim; ++j) {
      /* Generate random value between lower and upper bound */
      double delta = ctx->upper_input_bound[j] - ctx->lower_input_bound[j];
      pop->ind[i].input[j] = ctx->lower_input_bound[j]  + delta*unif_rand();
    }
  }
  PutRNGstate();
}

static void insert (list *node, int x) {
  list *temp;
  if (node == NULL)
      error("Asked to insert a NULL pointer.");
  temp = (list *)Calloc(1, list);
  temp->index = x;
  temp->child = node->child;
  temp->parent = node;
  if (node->child != NULL) {
    node->child->parent = temp;
  }
  node->child = temp;
  return;
}

static list* del (list *node) {
  list *temp;
  if (node == NULL)
      error("Asked to delete a NULL pointer.");
  temp = node->parent;
  temp->child = node->child;
  if (temp->child!=NULL) {
    temp->child->parent = temp;
  }
  Free (node);
  return (temp);
}

static void q_sort_front_obj(const population *pop, int objcount, int obj_array[], int left, int right) {
  int index;
  int temp;
  int i, j;
  double pivot;
  if (left<right) {
    index = (left + right)/2;
    temp = obj_array[right];
    obj_array[right] = obj_array[index];
    obj_array[index] = temp;
    pivot = pop->ind[obj_array[right]].objective[objcount];
    i = left-1;
    for (j=left; j<right; j++) {
      if (pop->ind[obj_array[j]].objective[objcount] <= pivot) {
        i+=1;
        temp = obj_array[j];
        obj_array[j] = obj_array[i];
        obj_array[i] = temp;
      }
    }
    index=i+1;
    temp = obj_array[index];
    obj_array[index] = obj_array[right];
    obj_array[right] = temp;
    q_sort_front_obj (pop, objcount, obj_array, left, index-1);
    q_sort_front_obj (pop, objcount, obj_array, index+1, right);
  }
}

static void quicksort_front_obj(const population *pop, int objcount, int obj_array[], int obj_array_size) {
  q_sort_front_obj (pop, objcount, obj_array, 0, obj_array_size-1);
}

static void q_sort_dist(const population *pop, int *dist, int left, int right) {
  int index;
  int temp;
  int i, j;
  double pivot;
  if (left<right) {
    /* Not randomized: */
    index = (left + right)/2;
    temp = dist[right];
    dist[right] = dist[index];
    dist[index] = temp;
    pivot = pop->ind[dist[right]].crowding_distance;
    i = left-1;
    for (j=left; j<right; j++) {
      if (pop->ind[dist[j]].crowding_distance <= pivot) {
        i+=1;
        temp = dist[j];
        dist[j] = dist[i];
        dist[i] = temp;
      }
    }
    index=i+1;
    temp = dist[index];
    dist[index] = dist[right];
    dist[right] = temp;
    q_sort_dist (pop, dist, left, index-1);
    q_sort_dist (pop, dist, index+1, right);
  }
  return;
}

static void quicksort_dist(const population *pop, int *dist, int front_size) {
  q_sort_dist (pop, dist, 0, front_size-1);
}

static int rnd (int low, int high) {
  int res;
  if (low >= high) {
    res = low;
  } else {
    GetRNGstate();
    res = (int)(low + (unif_rand() * (high-low+1)));
    PutRNGstate();
    if (res > high) {
      res = high;
    }
  }
  return (res);
}

int check_dominance (nsga2_ctx *ctx, const individual *a, const individual *b) {
  int i;
  if (a->constraint_violation < 0  && b->constraint_violation < 0) {
    if (a->constraint_violation > b->constraint_violation) {
      return 1;
    } else if (a->constraint_violation < b->constraint_violation) {
      return -1;
    }  else {
      return 0;
    }
  } else if (a->constraint_violation < 0 && b->constraint_violation == 0) {
    return -1;
  } else if (a->constraint_violation == 0 && b->constraint_violation < 0) {
    return 1;
  } else  {
    int flag1 = 0;
    int flag2 = 0;
    for (i=0; i < ctx->objective_dim; i++) {
      if (0 == flag1 && a->objective[i] < b->objective[i]) {
        flag1 = 1;
      } else if (0 == flag2 && a->objective[i] > b->objective[i]) {
        flag2 = 1;
      }
    }
    /* OME: Replace 2 cmps with one subtract: */
#define CD_CMP_VARIANT
#ifdef CD_CMP_VARIANT
    if (1 == flag1 && 0 == flag2) {
      return 1;
    } else if (0 == flag1 && 1 == flag2) {
      return -1;
    } else {
      return 0;
    }
#else
    return flag1 - flag2;
#endif
  }
}

static individual* tournament (nsga2_ctx *ctx, individual *ind1, individual *ind2) {
  int flag = check_dominance (ctx, ind1, ind2);
  if (1 == flag) {
    return ind1;
  } else if (-1 == flag) {
    return ind2;
  } else { /* Let crowding distance decide: */
    if (ind1->crowding_distance > ind2->crowding_distance) {
      return ind1;
    } else if (ind2->crowding_distance > ind1->crowding_distance) {
      return ind2;
    } else {  /* Tie breaker: */
      GetRNGstate();
      double r = unif_rand();
      PutRNGstate();
      return (r <= 0.5 ? ind1 : ind2);
    }
  }
}

static void evaluate_pop (nsga2_ctx *ctx, population *pop) {
  size_t i, j;
  SEXP fcall = ctx->function_call;
  SEXP ccall = ctx->constraint_call;
  SEXP s_input, s_fval;

  /* Allocate input vector and copy x into it: */
  PROTECT(s_input = allocMatrix(REALSXP, pop->size, ctx->input_dim));
  double *input = REAL(s_input);

  /* Set input arg for fcall and ccall */
  SETCADR(fcall, s_input);
  if (ctx->constraint_dim > 0)
    SETCADR(ccall, s_input);

  /* copy individuals into SEXP */
  for (i=0; i < pop->size; ++i)
    for (j=0; j < ctx->input_dim; ++j)
      input[i + j * pop->size] = pop->ind[i].input[j];

  /* Evalate function */
  PROTECT(s_fval = eval(fcall, ctx->environment));
  s_fval = coerceVector(s_fval, REALSXP);

  UNPROTECT(1); /* fval */
  UNPROTECT(1); /* s_input */

  /* copy results back into SEXP */
  for (i=0; i < pop->size; ++i) {
    for (j=0; j < ctx->objective_dim; ++j)
      pop->ind[i].objective[j] = REAL(s_fval)[i + j * pop->size];


    /* Possibly evaluate constraints !!!CURRENTLY DISABLED!!!
    pop->ind[i].constraint_violation = 0.0;
    if (ctx->constraint_dim > 0) {
      PROTECT(s_cval = eval(ccall, ctx->environment));
      REPROTECT(s_cval = coerceVector(s_cval, REALSXP), ip);
      for (j = 0; j < ctx->constraint_dim; ++j) {
	pop->ind[i].constraint[j] = REAL(s_cval)[j];
	if (pop->ind[i].constraint[j] < 0.0)
	  pop->ind[i].constraint_violation += pop->ind[i].constraint[j];
      }
      UNPROTECT(1);
    }
    */
  }
}

static void assign_crowding_distance (nsga2_ctx *ctx, const population *pop, int *dist, int **obj_array, const int front_size) {
  int i, j;
  for (i=0; i<ctx->objective_dim; i++) {
    for (j=0; j<front_size; j++) {
      obj_array[i][j] = dist[j];
    }
    quicksort_front_obj (pop, i, obj_array[i], front_size);
  }
  for (j=0; j<front_size; j++) {
    pop->ind[dist[j]].crowding_distance = 0.0;
  }
  for (i=0; i<ctx->objective_dim; i++) {
    pop->ind[obj_array[i][0]].crowding_distance = INF;
  }
  for (i=0; i<ctx->objective_dim; i++) {
    for (j=1; j<front_size-1; j++) {
      if (pop->ind[obj_array[i][j]].crowding_distance != INF) {
        if (pop->ind[obj_array[i][front_size-1]].objective[i] == pop->ind[obj_array[i][0]].objective[i]) {
          pop->ind[obj_array[i][j]].crowding_distance += 0.0;
        } else {
          pop->ind[obj_array[i][j]].crowding_distance += (pop->ind[obj_array[i][j+1]].objective[i] - pop->ind[obj_array[i][j-1]].objective[i])/(pop->ind[obj_array[i][front_size-1]].objective[i] - pop->ind[obj_array[i][0]].objective[i]);
        }
      }
    }
  }
  for (j=0; j<front_size; j++) {
    if (pop->ind[dist[j]].crowding_distance != INF) {
      pop->ind[dist[j]].crowding_distance = (pop->ind[dist[j]].crowding_distance)/ctx->objective_dim;
    }
  }
  return;
}

static void assign_crowding_distance_list (nsga2_ctx *ctx, const population *pop, list *lst, const int front_size) {
  int **obj_array;
  int *dist;
  int i, j;
  list *temp;
  temp = lst;
  if (front_size==1) {
    pop->ind[lst->index].crowding_distance = INF;
    return;
  } else if (front_size==2) {
    pop->ind[lst->index].crowding_distance = INF;
    pop->ind[lst->child->index].crowding_distance = INF;
    return;
  }
  obj_array = (int **)Calloc(ctx->objective_dim, int *);
  dist = (int *)Calloc(front_size, int);
  for (i=0; i<ctx->objective_dim; i++) {
    obj_array[i] = (int *)Calloc(front_size, int);
  }
  for (j=0; j<front_size; j++) {
    dist[j] = temp->index;
    temp = temp->child;
  }
  assign_crowding_distance (ctx, pop, dist, obj_array, front_size);
  Free (dist);
  for (i=0; i<ctx->objective_dim; i++) {
    Free (obj_array[i]);
  }
  Free (obj_array);
  return;
}

static void assign_crowding_distance_indices (nsga2_ctx *ctx, const population *pop, int c1, int c2) {
  int **obj_array;
  int *dist;
  int i, j;
  int front_size;
  front_size = c2-c1+1;
  if (front_size==1) {
    pop->ind[c1].crowding_distance = INF;
    return;
  }
  if (front_size==2) {
    pop->ind[c1].crowding_distance = INF;
    pop->ind[c2].crowding_distance = INF;
    return;
  }
  obj_array = (int **)Calloc(ctx->objective_dim, int *);
  dist = (int *)Calloc(front_size, int);
  for (i=0; i<ctx->objective_dim; i++) {
    obj_array[i] = (int *)Calloc(front_size, int);
  }
  for (j=0; j<front_size; j++) {
    dist[j] = c1++;
  }
  assign_crowding_distance (ctx, pop, dist, obj_array, front_size);
  Free (dist);
  for (i=0; i<ctx->objective_dim; i++) {
    Free (obj_array[i]);
  }
  Free (obj_array);
  return;
}

static void assign_rank_and_crowding_distance (nsga2_ctx *ctx, population *new_pop) {
  int flag;
  int i;
  int end;
  int front_size;
  int rank=1;
  list *orig;
  list *cur;
  list *temp1, *temp2;
  orig = (list *) Calloc(1, list);
  cur = (list *) Calloc(1, list);
  front_size = 0;
  orig->index = -1;
  orig->parent = NULL;
  orig->child = NULL;
  cur->index = -1;
  cur->parent = NULL;
  cur->child = NULL;
  temp1 = orig;
  for (i=0; i< new_pop->size; i++) {
    insert (temp1,i);
    temp1 = temp1->child;
  }
  do {
    if (orig->child->child == NULL) {
      new_pop->ind[orig->child->index].rank = rank;
      new_pop->ind[orig->child->index].crowding_distance = INF;
      break;
    }
    temp1 = orig->child;
    insert (cur, temp1->index);
    front_size = 1;
    temp2 = cur->child;
    temp1 = del (temp1);
    temp1 = temp1->child;
    do {
      temp2 = cur->child;
      do {
        end = 0;
        flag = check_dominance (ctx, &(new_pop->ind[temp1->index]), &(new_pop->ind[temp2->index]));
        if (flag == 1) {
          insert (orig, temp2->index);
          temp2 = del (temp2);
          front_size--;
          temp2 = temp2->child;
        }
        if (flag == 0) {
          temp2 = temp2->child;
        }
        if (flag == -1) {
          end = 1;
        }
      } while (end!=1 && temp2!=NULL);
      if (flag == 0 || flag == 1) {
        insert (cur, temp1->index);
        front_size++;
        temp1 = del (temp1);
      }
      temp1 = temp1->child;
    } while (temp1 != NULL);
    temp2 = cur->child;
    do {
      new_pop->ind[temp2->index].rank = rank;
      temp2 = temp2->child;
    } while (temp2 != NULL);
    assign_crowding_distance_list (ctx, new_pop, cur->child, front_size);
    temp2 = cur->child;
    do {
      temp2 = del (temp2);
      temp2 = temp2->child;
    } while (cur->child !=NULL);
    rank+=1;
  } while (orig->child!=NULL);
  Free (orig);
  Free (cur);
  return;
}

static void crossover (nsga2_ctx *ctx,
		       individual *parent1, individual *parent2,
		       individual *child1, individual *child2) {
  int i;
  double rand;
  double y1, y2, yl, yu;
  double c1, c2;
  double alpha, beta, betaq;

  GetRNGstate();

  if (unif_rand() <= ctx->crossing_probability) {
    ctx->input_crossings++;
    for (i = 0; i < ctx->input_dim; ++i) {
      if (unif_rand() <= 0.5 ) {
        if (fabs(parent1->input[i]-parent2->input[i]) > EPS) {
          if (parent1->input[i] < parent2->input[i]) {
            y1 = parent1->input[i];
            y2 = parent2->input[i];
          } else {
            y1 = parent2->input[i];
            y2 = parent1->input[i];
          }
          yl = ctx->lower_input_bound[i];
          yu = ctx->upper_input_bound[i];
          rand = unif_rand();
          beta = 1.0 + (2.0*(y1-yl)/(y2-y1));
          alpha = 2.0 - pow(beta,-(ctx->eta_c+1.0));
          if (rand <= (1.0/alpha))
            betaq = pow ((rand*alpha),(1.0/(ctx->eta_c+1.0)));
          else
            betaq = pow ((1.0/(2.0 - rand*alpha)),(1.0/(ctx->eta_c+1.0)));
          c1 = 0.5*((y1+y2)-betaq*(y2-y1));
          beta = 1.0 + (2.0*(yu-y2)/(y2-y1));
          alpha = 2.0 - pow(beta,-(ctx->eta_c+1.0));
          if (rand <= (1.0/alpha))
            betaq = pow ((rand*alpha),(1.0/(ctx->eta_c+1.0)));
          else
            betaq = pow ((1.0/(2.0 - rand*alpha)),(1.0/(ctx->eta_c+1.0)));
          c2 = 0.5*((y1+y2)+betaq*(y2-y1));
	  /* Enforce constraints: */
          if (c1 < yl) c1=yl;
          if (c2 < yl) c2=yl;
          if (c1 > yu) c1=yu;
          if (c2 > yu) c2=yu;
          if (unif_rand() <= 0.5) {
            child1->input[i] = c2;
            child2->input[i] = c1;
          } else  {
            child1->input[i] = c1;
            child2->input[i] = c2;
          }
        } else {
          child1->input[i] = parent1->input[i];
          child2->input[i] = parent2->input[i];
        }
      } else {
        child1->input[i] = parent1->input[i];
        child2->input[i] = parent2->input[i];
      }
    }
  } else {
    for (i=0; i<ctx->input_dim; i++) {
      child1->input[i] = parent1->input[i];
      child2->input[i] = parent2->input[i];
    }
  }
  PutRNGstate();
}

static void selection(nsga2_ctx *ctx, population *old_pop, population *new_pop) {
  assert(old_pop->size == new_pop->size);
  int temp;
  int i;
  int rand;
  individual *parent1, *parent2;

  int *a1 = (int *)Calloc(old_pop->size, int);
  int *a2 = (int *)Calloc(old_pop->size, int);

  for (i=0; i < old_pop->size; ++i)
    a1[i] = a2[i] = i;

  for (i=0; i < old_pop->size; ++i) {
    rand = rnd (i, old_pop->size-1);
    temp = a1[rand];
    a1[rand] = a1[i];
    a1[i] = temp;
    rand = rnd (i, old_pop->size-1);
    temp = a2[rand];
    a2[rand] = a2[i];
    a2[i] = temp;
  }
  for (i=0; i < old_pop->size; i+=4) {
    parent1 = tournament (ctx, &old_pop->ind[a1[i]], &old_pop->ind[a1[i+1]]);
    parent2 = tournament (ctx, &old_pop->ind[a1[i+2]], &old_pop->ind[a1[i+3]]);
    crossover (ctx, parent1, parent2, &new_pop->ind[i], &new_pop->ind[i+1]);
    parent1 = tournament (ctx, &old_pop->ind[a2[i]], &old_pop->ind[a2[i+1]]);
    parent2 = tournament (ctx, &old_pop->ind[a2[i+2]], &old_pop->ind[a2[i+3]]);
    crossover (ctx, parent1, parent2, &new_pop->ind[i+2], &new_pop->ind[i+3]);
  }
  Free(a1);
  Free(a2);
  return;
}

static void mutate_ind (nsga2_ctx *ctx, individual *ind) {
  int j;
  double rnd, delta1, delta2, mut_pow, deltaq;
  double y, yl, yu, val, xy;
  GetRNGstate();
  for (j = 0; j < ctx->input_dim; ++j) {
    if (unif_rand() <= ctx->mutation_probability) {
      y = ind->input[j];
      yl = ctx->lower_input_bound[j];
      yu = ctx->upper_input_bound[j];
      delta1 = (y-yl)/(yu-yl);
      delta2 = (yu-y)/(yu-yl);
      rnd = unif_rand();
      mut_pow = 1.0/(ctx->eta_m+1.0);
      if (rnd <= 0.5) {
        xy = 1.0-delta1;
        val = 2.0*rnd+(1.0-2.0*rnd)*(pow(xy,(ctx->eta_m+1.0)));
        deltaq =  pow(val,mut_pow) - 1.0;
      } else {
        xy = 1.0-delta2;
        val = 2.0*(1.0-rnd)+2.0*(rnd-0.5)*(pow(xy,(ctx->eta_m+1.0)));
        deltaq = 1.0 - (pow(val,mut_pow));
      }
      y = y + deltaq*(yu-yl);
      if (y < yl) y = yl;
      if (y > yu) y = yu;
      ind->input[j] = y;
      ctx->input_mutations+=1;
    }
  }
  PutRNGstate();
}

static void mutation_pop (nsga2_ctx *ctx, population *pop) {
  int i;
  for (i=0; i < pop->size; ++i)
    mutate_ind(ctx, &(pop->ind[i]));
}

static void copy_ind (const nsga2_ctx * ctx, const individual *from, individual *to) {
  int i;
  to->rank = from->rank;
#if 0
  if (ctx->constraint_dim == 0) {
    to->constraint_violation = 0.0;
  } else {
    to->constraint_violation = from->constraint_violation;
  }
#else
  to->constraint_violation = from->constraint_violation;
#endif
  to->crowding_distance = from->crowding_distance;

  // Copy real input
  for (i = 0; i < ctx->input_dim; ++i)
    to->input[i] = from->input[i];
  // Copy objective values:
  for (i = 0; i < ctx->objective_dim; ++i)
    to->objective[i] = from->objective[i];
  // Copy constraints
  for (i = 0; i < ctx->constraint_dim; ++i)
    to->constraint[i] = from->constraint[i];
}

/* Routine to merge two populations into one */
void merge(nsga2_ctx *ctx, population *pop1, population *pop2, population *pop3) {
  assert(pop3->size >= (pop1->size + pop2->size));
  int i, k;
  for (i = 0; i < pop1->size; ++i)
    copy_ind (ctx, &(pop1->ind[i]), &(pop3->ind[i]));
  for (i=0, k=pop1->size; i<pop2->size; i++, k++)
    copy_ind (ctx, &(pop2->ind[i]), &(pop3->ind[k]));
}

/*
 * nondominated_sort
 *
 * Fast implementation of nondominated sorting. Stops after the all
 * processed fronts contain at leas nsorted individuals.
 *
 * This function is a memory hog. It allocates S[][] as a big chunk of
 * memory instead of using linked lists or other sequence data
 * structures. Currently we can sort ~5000 individuals in 128MB of
 * memory.
 */
void nondominated_sort(nsga2_ctx *ctx, population *in_pop, const size_t nsorted) {
  size_t i, j;
  const size_t in_size  = in_pop->size;
  size_t out_size = 0;
  int rank;
  unsigned char *S = (unsigned char *)Calloc(in_size*in_size, unsigned char);
  unsigned int *n = (unsigned int *)Calloc(in_size, unsigned int);

  for (i = 0; i < in_size; ++i) {
    n[i] = 0;
    for (j = i+1; j < in_size; ++j) {
      int dom = check_dominance(ctx, &in_pop->ind[i], &in_pop->ind[j]);
      if (1 > dom) { /* i dominates j */
	S[in_size * i + j] = 1;
	S[in_size * j + i] = 0;
	++n[j];
      } else if (-1 < dom) { /* j dominates i */
	S[in_size * i + j] = 0;
	S[in_size * j + i] = 1;
	++n[i];
      } else { /* neither dominates the other */
	S[in_size * i + j] = 0;
	S[in_size * j + i] = 0;
      }
    }
    if (0 == n[i]) { /* Member of first front */
      in_pop->ind[i].rank = 1;
      ++out_size;
    } else { /* Not yet decide what front i belongs to */
      in_pop->ind[i].rank = -1;
    }
  }

  while (out_size < nsorted) {
    rank = 1;
    for (i = 0; i < in_size; ++i) {
      if (rank != in_pop->ind[i].rank)  /* Skip all not in current rank */
	continue;
      for (j = 0; j < in_size; ++j) {
	if (1 == S[in_size * i + j]) { /* j in S_i */
	  --n[j];
	  if (0 == n[j]) { /* n_j == 0 -> assign rank */
	    in_pop->ind[j].rank = rank + 1;
	    ++out_size;
	  }
	}
      }
    }
    ++rank;
  }
  Free(S);
  Free(n);
}

static void crowding_fill (nsga2_ctx *ctx, population *mixed_pop, population *new_pop,
			   int count, int front_size, list *elite) {
  int *dist;
  list *temp;
  int i, j;
  assign_crowding_distance_list (ctx, mixed_pop, elite->child, front_size);
  dist = (int *)Calloc(front_size, int);
  temp = elite->child;
  for (j=0; j<front_size; j++) {
    dist[j] = temp->index;
    temp = temp->child;
  }
  quicksort_dist (mixed_pop, dist, front_size);
  for (i=count, j=front_size-1; i<new_pop->size; i++, j--) {
    copy_ind(ctx, &mixed_pop->ind[dist[j]], &new_pop->ind[i]);
  }
  Free (dist);
  return;
}

static void fill_nondominated_sort (nsga2_ctx *ctx, population *mixed_pop, population *new_pop) {
  int flag;
  int i, j;
  int done;
  size_t front_size = 0;
  size_t archive_size = 0;
  int rank=1;
  list *pool;
  list *elite;
  list *temp1, *temp2;
  pool = (list *)Calloc(1, list);
  elite = (list *)Calloc(1, list);
  pool->index = -1;
  pool->parent = NULL;
  pool->child = NULL;
  elite->index = -1;
  elite->parent = NULL;
  elite->child = NULL;
  temp1 = pool;
  for (i=0; i<mixed_pop->size; i++) {
    insert (temp1,i);
    temp1 = temp1->child;
  }
  i=0;
  do {
    temp1 = pool->child;
    insert (elite, temp1->index);
    front_size = 1;
    temp1 = del (temp1);
    temp1 = temp1->child;
    do {
      temp2 = elite->child;
      if (NULL == temp1) // End of pool reached:
        break;
      do {
        done = 0;
        flag = check_dominance (ctx, &(mixed_pop->ind[temp1->index]), &(mixed_pop->ind[temp2->index]));
	switch (flag) {
	case 1:
          insert (pool, temp2->index);
          temp2 = del (temp2);
          front_size--;
          temp2 = temp2->child;
	  break;
	case 0:
          temp2 = temp2->child;
	  break;
	case -1:
          done = 1;
        }
      } while (!done && temp2 != NULL);
      if (flag == 0 || flag == 1) {
        insert (elite, temp1->index);
        front_size++;
        temp1 = del (temp1);
      }
      temp1 = temp1->child;
    } while (temp1 != NULL);
    temp2 = elite->child;
    j=i;
    if ((archive_size + front_size) <= new_pop->size) {
      do {
        copy_ind (ctx, &mixed_pop->ind[temp2->index], &new_pop->ind[i]);
        new_pop->ind[i].rank = rank;
        ++archive_size;
        temp2 = temp2->child;
        i+=1;
      } while (temp2 != NULL);
      assign_crowding_distance_indices(ctx, new_pop, j, i-1);
      rank+=1;
    } else {
      crowding_fill (ctx, mixed_pop, new_pop, i, front_size, elite);
      archive_size = new_pop->size;
      for (j=i; j< new_pop->size; j++) {
        new_pop->ind[j].rank = rank;
      }
    }
    temp2 = elite->child;
    do {
      temp2 = del (temp2);
      temp2 = temp2->child;
    } while (elite->child !=NULL);
  } while (archive_size < new_pop->size);
  while (pool != NULL) {
    temp1 = pool;
    pool = pool->child;
    Free (temp1);
  }
  while (elite != NULL) {
    temp1 = elite;
    elite = elite->child;
    Free (temp1);
  }
  return;
}

static int on_pareto_front(individual *ind) {
  return (ind->rank == 1 && ind->constraint_violation == 0.0);
}

SEXP do_nsga2_vectorized(SEXP s_function,
	      SEXP s_constraint,
	      SEXP s_env,
	      SEXP s_obj_dim,
	      SEXP s_constr_dim,
	      SEXP s_input_dim,
	      SEXP s_lower_bound,
	      SEXP s_upper_bound,
	      SEXP s_popsize,
	      SEXP s_generations,
	      SEXP s_crossing_prob,
	      SEXP s_crossing_dist,
	      SEXP s_mutation_prob,
	      SEXP s_mutation_dist) {
  nsga2_ctx ctx;
  unsigned int i, j, gen;
  size_t popsize;
  int *generations;
  R_len_t n_generations;

  if (!isFunction(s_function))
    error("Argument 's_function' is not a function.");
  if (!isFunction(s_constraint))
    error("Argument 's_constraint' is not a function.");
  if (!isInteger(s_input_dim))
    error("Argument 's_input_dim' is not an integer.");
  if (!isInteger(s_constr_dim))
    error("Argument 's_constr_dim' is not an integer.");
  if (!isReal(s_lower_bound))
    error("Argument 's_lower_bound' is not a real vector.");
  if (!isReal(s_upper_bound))
    error("Argument 's_upper_bound' is not a real vector.");
  if (!isInteger(s_popsize))
    error("Argument 's_popsize' is not an integer.");
  if (!isInteger(s_generations))
    error("Argument 's_generations' is not an integer.");
  if (!isInteger(s_obj_dim))
    error("Argument 's_obj_dim' is not an integer.");
  if (!isReal(s_crossing_prob))
    error("Argument 's_crossing_prob' is not a real.");
  if (!isInteger(s_crossing_dist))
    error("Argument 's_crossing_dist' is not an integer.");
  if (!isReal(s_mutation_prob))
    error("Argument 's_mutation_prob' is not a real.");
  if (!isInteger(s_mutation_dist))
    error("Argument 's_mutation_dist' is not an integer.");

  PROTECT(ctx.environment = s_env);
  PROTECT(ctx.function_call	= lang2(s_function, R_NilValue));
  PROTECT(ctx.constraint_call	= lang2(s_constraint, R_NilValue));
  ctx.objective_dim		= INTEGER(s_obj_dim)[0];
  ctx.constraint_dim		= INTEGER(s_constr_dim)[0];;
  ctx.input_dim			= INTEGER(s_input_dim)[0];
  ctx.lower_input_bound		= REAL(s_lower_bound);
  ctx.upper_input_bound		= REAL(s_upper_bound);
  popsize			= INTEGER(s_popsize)[0];
  generations			= INTEGER(s_generations);
  n_generations                 = length(s_generations);

  ctx.crossing_probability	= REAL(s_crossing_prob)[0];
  ctx.eta_c			= INTEGER(s_crossing_dist)[0];
  ctx.mutation_probability	= REAL(s_mutation_prob)[0];
  ctx.eta_m			= INTEGER(s_mutation_dist)[0];

  ctx.input_mutations		= 0;
  ctx.input_crossings		= 0;

  /*
   * Allocate parent, child and combined populations
   */
  population *parents, *children, *combined;
  parents  = population_alloc(&ctx, popsize);
  children = population_alloc(&ctx, popsize);
  combined = population_alloc(&ctx, 2*popsize);

  SEXP s_res;
  PROTECT(s_res = allocVector(VECSXP, n_generations));

  /*
   * Initialize parent population, evaluate it and do inital NDS
   */
  population_initialize(&ctx, parents);
  evaluate_pop (&ctx, parents);
  assign_rank_and_crowding_distance (&ctx, parents);
  for (gen=0; gen < n_generations; ++gen) {
    for (i=2; (i-2) <= generations[gen]; ++i) {
      selection (&ctx, parents, children);
      mutation_pop (&ctx, children);
      evaluate_pop(&ctx, children);
      merge (&ctx, parents, children, combined);
      fill_nondominated_sort (&ctx, combined, parents);

      R_CheckUserInterrupt(); /* Allow user interuption */
    }

    /* Copy parent population into SEXPs */
    SEXP s_input, s_objective, s_pareto_optimal;
    double *input, *objective;
    int *pareto_optimal;
    PROTECT(s_input = allocMatrix(REALSXP, popsize, ctx.input_dim));
    input = REAL(s_input);
    PROTECT(s_objective = allocMatrix(REALSXP, popsize, ctx.objective_dim));
    objective = REAL(s_objective);
    PROTECT(s_pareto_optimal = allocVector(LGLSXP, popsize));
    pareto_optimal = LOGICAL(s_pareto_optimal);

    for (i=0; i < popsize; ++i) {
      pareto_optimal[i] = on_pareto_front(&(parents->ind[i]));
      for (j=0; j < ctx.input_dim; ++j)
	input[popsize * j + i] = parents->ind[i].input[j];
      for (j=0; j < ctx.objective_dim; ++j)
	objective[popsize * j + i] = parents->ind[i].objective[j];
    }
    /* Buid result list */
    SEXP s_pres;
    PROTECT(s_pres = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(s_pres, 0, s_input);
    SET_VECTOR_ELT(s_pres, 1, s_objective);
    SET_VECTOR_ELT(s_pres, 2, s_pareto_optimal);
    UNPROTECT(4); /* s_input, s_objective, s_pareto_optimal, s_pres */

    /* Save in result list: */
    SET_VECTOR_ELT(s_res, gen, s_pres);
  }
  UNPROTECT(4); /* ctx.environment, s_function_call, s_constraint_call, s_res */
  return s_res;
}
