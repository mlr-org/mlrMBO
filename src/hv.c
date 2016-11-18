/*************************************************************************

 hypervolume computation

 ---------------------------------------------------------------------

                       Copyright (c) 2010
                  Carlos Fonseca <cmfonsec@ualg.pt>
             Manuel Lopez-Ibanez <manuel.lopez-ibanez@ulb.ac.be>
                    Luis Paquete <paquete@dei.uc.pt>

 This program is free software (software libre); you can redistribute
 it and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, you can obtain a copy of the GNU
 General Public License at:
                 http://www.gnu.org/copyleft/gpl.html
 or by writing to:
           Free Software Foundation, Inc., 59 Temple Place,
                 Suite 330, Boston, MA 02111-1307 USA

 ----------------------------------------------------------------------

 Relevant literature:

 [1]  C. M. Fonseca, L. Paquete, and M. Lopez-Ibanez. An
      improved dimension-sweep algorithm for the hypervolume
      indicator. In IEEE Congress on Evolutionary Computation,
      pages 1157-1163, Vancouver, Canada, July 2006.

 [2]  L. Paquete, C. M. Fonseca and M. Lopez-Ibanez. An optimal
      algorithm for a special case of Klee's measure problem in three
      dimensions. Technical Report CSI-RT-I-01/2006, CSI, Universidade
      do Algarve, 2006.

*************************************************************************/

#include "hv.h"
#include "avl.h"
#include <R.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <assert.h>

typedef struct dlnode {
  double *x;                    /* The data vector              */
  struct dlnode **next;         /* Next-node vector             */
  struct dlnode **prev;         /* Previous-node vector         */
  struct avl_node_t * tnode;
  int ignore;
  double *area;                 /* Area */
  double *vol;                  /* Volume */
} dlnode_t;

static avl_tree_t *tree;
int stop_dimension = 2; /* default: stop on dimension 3 */

static int compare_node( const void *p1, const void* p2) {
    const double x1 = *((*(const dlnode_t **)p1)->x);
    const double x2 = *((*(const dlnode_t **)p2)->x);

    return (x1 < x2) ? -1 : (x1 > x2) ? 1 : 0;
}

static int compare_tree_asc( const void *p1, const void *p2) {
    const double x1 = *((const double *)p1 + 1);
    const double x2 = *((const double *)p2 + 1);

    return (x1 > x2) ? -1 : (x1 < x2) ? 1 : 0;
}


/*
 * Setup circular double-linked list in each dimension
 */

static dlnode_t * setup_cdllist(double *data, int d, int n) {
    dlnode_t *head;
    dlnode_t **scratch;
    int i, j;

    head  = malloc ((n+1) * sizeof(dlnode_t));

    head->x = data;
    head->ignore = 0;  /* should never get used */
    head->next = malloc( d * (n+1) * sizeof(dlnode_t*));
    head->prev = malloc( d * (n+1) * sizeof(dlnode_t*));
    head->tnode = malloc ((n+1) * sizeof(avl_node_t));

    head->area = malloc(d * (n+1) * sizeof(double));
    head->vol = malloc(d * (n+1) * sizeof(double));

    for (i = 1; i <= n; i++) {
        head[i].x = head[i-1].x + d ;/* this will be fixed a few lines below... */
        head[i].ignore = 0;
        head[i].next = head[i-1].next + d;
        head[i].prev = head[i-1].prev + d;
        head[i].tnode = head[i-1].tnode + 1;
        head[i].area = head[i-1].area + d;
        head[i].vol = head[i-1].vol + d;
    }
    head->x = NULL; /* head contains no data */

    scratch = malloc(n * sizeof(dlnode_t*));
    for (i = 0; i < n; i++)
        scratch[i] = head + i + 1;

    for (j = d-1; j >= 0; j--) {
        for (i = 0; i < n; i++)
            scratch[i]->x--;
        qsort(scratch, n, sizeof(dlnode_t*), compare_node);
        head->next[j] = scratch[0];
        scratch[0]->prev[j] = head;
        for (i = 1; i < n; i++) {
            scratch[i-1]->next[j] = scratch[i];
            scratch[i]->prev[j] = scratch[i-1];
        }
        scratch[n-1]->next[j] = head;
        head->prev[j] = scratch[n-1];
    }

    free(scratch);

    for (i = 1; i <= n; i++)
        avl_init_node(head[i].tnode, head[i].x);

    for (i = 0; i < d; i++)
        head->area[i] = 0;

    return head;
}

static void free_cdllist(dlnode_t * head) {
    free(head->tnode); /* Frees _all_ nodes. */
    free(head->next);
    free(head->prev);
    free(head->area);
    free(head->vol);
    free(head);
}

static void delete (dlnode_t *nodep, int dim, double * bound) {
    for (int i = 0; i < dim; i++) {
        nodep->prev[i]->next[i] = nodep->next[i];
        nodep->next[i]->prev[i] = nodep->prev[i];
        if (bound[i] > nodep->x[i])
            bound[i] = nodep->x[i];
  }
}

static void reinsert (dlnode_t *nodep, int dim, double * bound) {
    for (int i = 0; i < dim; i++) {
        nodep->prev[i]->next[i] = nodep;
        nodep->next[i]->prev[i] = nodep;
        if (bound[i] > nodep->x[i])
            bound[i] = nodep->x[i];
    }
}

static double hv_recursive(dlnode_t *list, int dim, int c, const double * ref, double * bound) {
    /* ------------------------------------------------------
       General case for dimensions higher than stop_dimension
       ------------------------------------------------------ */
    if ( dim > stop_dimension ) {
        dlnode_t *p0 = list;
        dlnode_t *p1 = list->prev[dim];
        double hyperv = 0;
        dlnode_t *pp;
        for (pp = p1; pp->x; pp = pp->prev[dim]) {
            if (pp->ignore < dim)
                pp->ignore = 0;
        }
        while (c > 1
               /* We delete all points x[dim] > bound[dim]. In case of
                  repeated coordinates, we also delete all points
                  x[dim] == bound[dim] except one. */
               && (p1->x[dim] > bound[dim]
                   || p1->prev[dim]->x[dim] >= bound[dim])
            ) {
            p0 = p1;
            delete(p0, dim, bound);
            p1 = p0->prev[dim];
            c--;
        }

        if (c > 1) {
            hyperv = p1->prev[dim]->vol[dim] + p1->prev[dim]->area[dim]
                * (p1->x[dim] - p1->prev[dim]->x[dim]);
            p1->vol[dim] = hyperv;
        } else {
            p1->area[0] = 1;
            for (int i = 1; i <= dim; i++)
                p1->area[i] = p1->area[i-1] * (ref[i-1] - p1->x[i-1]);
            p1->vol[dim] = 0;
        }
        if (p1->ignore >= dim) {
            p1->area[dim] = p1->prev[dim]->area[dim];
        } else {
            p1->area[dim] = hv_recursive(list, dim-1, c, ref, bound);
            if (p1->area[dim] <= p1->prev[dim]->area[dim])
                p1->ignore = dim;
        }

        while (p0->x != NULL) {

            hyperv += p1->area[dim] * (p0->x[dim] - p1->x[dim]);
            bound[dim] = p0->x[dim];
            reinsert(p0, dim, bound);
            c++;
            p1 = p0;
            p0 = p0->next[dim];
            p1->vol[dim] = hyperv;
            if (p1->ignore >= dim) {
                p1->area[dim] = p1->prev[dim]->area[dim];
            } else {
                p1->area[dim] = hv_recursive(list, dim-1, c, ref, bound);
                if (p1->area[dim] <= p1->prev[dim]->area[dim])
                    p1->ignore = dim;
            }
        }
        hyperv += p1->area[dim] * (ref[dim] - p1->x[dim]);
        return hyperv;
    }

    /* ---------------------------
       special case of dimension 3
       --------------------------- */
    else if (dim == 2) {
        double hyperv;
        double hypera;
        double height;
        dlnode_t *pp = list->next[2];

        hypera = (ref[0] - pp->x[0]) * (ref[1] - pp->x[1]);

        height = (c == 1)
            ? ref[2] - pp->x[2]
            : pp->next[2]->x[2] - pp->x[2];

        hyperv = hypera * height;

        if (pp->next[2]->x == NULL)
            return hyperv;

        avl_insert_top(tree, pp->tnode);

        pp = pp->next[2];
        do {
            height = (pp == list->prev[2])
                ? ref[2] - pp->x[2]
                : pp->next[2]->x[2] - pp->x[2];
            if (pp->ignore >= 2)
                hyperv += hypera * height;
            else {
                const double * prv_ip, * nxt_ip;
                avl_node_t *tnode;

                if (avl_search_closest(tree, pp->x, &tnode) <= 0) {
                    nxt_ip = (double *)(tnode->item);
                    tnode = tnode->prev;
                } else {
                    nxt_ip = (tnode->next != NULL)
                        ? (double *)(tnode->next->item)
                        : ref;
                }

                if (nxt_ip[0] > pp->x[0]) {

                    avl_insert_after(tree, tnode, pp->tnode);

                    if (tnode != NULL) {
                        prv_ip = (double *)(tnode->item);

                        if (prv_ip[0] > pp->x[0]) {
                            const double * cur_ip;

                            tnode = pp->tnode->prev;
                            /* cur_ip = point dominated by pp with
                               highest [0]-coordinate */
                            cur_ip = (double *)(tnode->item);
                            while (tnode->prev) {
                                prv_ip = (double *)(tnode->prev->item);
                                hypera -= (prv_ip[1] - cur_ip[1])*(nxt_ip[0] - cur_ip[0]);
                                if (prv_ip[0] < pp->x[0])
                                    break; /* prv is not dominated by pp */
                                cur_ip = prv_ip;
                                avl_unlink_node(tree,tnode);
                                tnode = tnode->prev;
                            }

                            avl_unlink_node(tree,tnode);

                            if (!tnode->prev) {
                                hypera -= (ref[1] - cur_ip[1])*(nxt_ip[0] - cur_ip[0]);
                                prv_ip = ref;
                            }
                        }
                    } else
                        prv_ip = ref;

                    hypera += (prv_ip[1] - pp->x[1])*(nxt_ip[0] - pp->x[0]);
                }
                else
                    pp->ignore = 2;

                if (height > 0)
                    hyperv += hypera * height;
            }
            pp = pp->next[2];
        } while (pp->x != NULL);

        avl_clear_tree(tree);
        return hyperv;
    }

    /* special case of dimension 2 */
    else if (dim == 1) {
        const dlnode_t *p1 = list->next[1];
        double hypera = p1->x[0];
        double hyperv = 0;
        const dlnode_t *p0;

        while ((p0 = p1->next[1])->x) {
            hyperv += (ref[0] - hypera) * (p0->x[1] - p1->x[1]);
            if (p0->x[0] < hypera)
                hypera = p0->x[0];
            p1 = p0;
        }
        hyperv += (ref[0] - hypera) * (ref[1] - p1->x[1]);
        return hyperv;
    }

    /* special case of dimension 1 */
    else if (dim == 0) {
        return (ref[0] - list->next[0]->x[0]);
    }
    else {
        error("hv: UNREACHABLE CODE REACHED. Please report this to the package author.");
        return -1.0; /* Never reached. */
    }
}

/*
  Removes the point from the circular double-linked list.
*/
static void filter_delete_node(dlnode_t *node, int d) {
    /* The memory allocated for the deleted node is lost (leaked)
       until the end of the program, but this should not be a problem. */
    for (int i = 0; i < d; i++) {
        node->next[i]->prev[i] = node->prev[i];
        node->prev[i]->next[i] = node->next[i];
    }
}

/*
  Filters those points that do not strictly dominate the reference
  point.  This is needed to assure that the points left are only those
  which are needed to calculate the hypervolume.
*/
static int filter(dlnode_t *list, int d, int n, const double *ref) {
    /* fprintf (stderr, "%d points initially\n", n); */
    for (int i = 0; i < d; i++) {
        dlnode_t *aux = list->prev[i];
        int np = n;
        for (int j = 0; j < np; j++) {
            if (aux->x[i] < ref[i])
                break;
            filter_delete_node (aux, d);
            aux = aux->prev[i];
            n--;
        }
    }
    /* fprintf (stderr, "%d points remain\n", n); */
    return n;
}

double fpli_hv(double *data, int d, int n, const double *ref) {
    dlnode_t *list;
    double hyperv;
    double * bound = NULL;

    bound = malloc (d * sizeof(double));
    for (int i = 0; i < d; i++) bound[i] = -DBL_MAX;

    tree  = avl_alloc_tree ((avl_compare_t) compare_tree_asc,
                            (avl_freeitem_t) NULL);

    list = setup_cdllist(data, d, n);

    n = filter(list, d, n, ref);
    if (n == 0) {
        /* Returning here would leak memory.  */
    hyperv = 0.0;
    } else {
    hyperv = hv_recursive(list, d-1, n, ref, bound);
    }
    /* Clean up.  */
    free_cdllist (list);
    free (tree);  /* The nodes are freed by free_cdllist ().  */
    free (bound);

    return hyperv;
}
