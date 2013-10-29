# Function for plotting 1d numeric respectively discrete functions. 
# 
# FIMXE: check if param names agree with regular plot function
autoplotExampleRun1d = function(x, iters, xlim, ylim, pause, point.size, line.size, trafo, densregion=TRUE...) {
  # extract relevant data from MBOExampleRun
  par.set = x$par.set
  par.types = x$par.types
  names.x = x$names.x
  name.y = x$name.y
  control = x$control
  noisy = control$noisy
  mbo.res = x$mbo.res
  models = mbo.res$models

  # check if standard error is available
  se = (x$learner$predict.type == "se")

  propose.points = control$propose.points
  name.crit = control$infill.crit
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1

  # we need to maximize expected improvement
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }

  # if no iterations provided take the total number of iterations in optimization process
  if(missing(iters)) {
    iters = length(models)
  }

  global.opt = x$global.opt
  if (is.na(global.opt)) {
    global.opt = x$global.opt.estim
  }

  evals = x$evals
  evals.x = evals[, names.x, drop=FALSE]

  opt.path = as.data.frame(mbo.res$opt.path)

  idx.init = which(opt.path$dob == 0)

  plot.sequence = list()

  #stopf("Plotting 1d numeric function.")
  for (i in iters) {
    catf("Iter %i", i)

    # FIXME: the following lines work for discrete parameter as well.
    # FIXME: only the constuction of the "gg" dataframe is special
    model = models[[i]]
    idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
    idx.proposed = which(opt.path$dob == i)
    idx.past = which(opt.path$dob < i)
    idx.pastpresent = which(opt.path$dob <= i)
    idx = c(idx.init, idx.seq, idx.proposed)

    model.ok = !inherits(model, "FailureModel")

    # compute model prediction for current iter
    if (model.ok) {
      evals$yhat = mlrMBO:::infillCritMeanResponse(evals.x, model,
        control, par.set, opt.path[idx.past,])

      if (propose.points == 1L) {
        evals[[name.crit]] = opt.direction *
          critfun(evals.x, model, control, par.set, opt.path[idx.past,])
      } else {
        print("Multipoint proposal")
        # evals[[name.crit]] = ...
      }

      # prepare drawing of standard error (confidence interval)
      if (se) {
        evals$se = -mlrMBO:::infillCritStandardError(evals.x, model,
                                                     control, par.set, opt.path[idx.past,])
        # FIXME: make a parameter out of the constant factor
        evals$se.min = evals$yhat - 0.3 * evals$se
        evals$se.max = evals$yhat + 0.3 * evals$se
      }
    }

    # FIXME: better source the actual ploting out in seperate numeric/discrete files
    if (par.types %in% c("numeric", "numericvector")) {
      # ggplot stuff
      n = nrow(evals)

      # data frame with real fun and model fun evaluations
      gg.fun = data.frame(x=rep(evals[,names.x],2),
        y=c(evals[,name.y],evals[,"yhat"]),
        se.min=if (se) rep(evals[,"se.min"], 2) else NA,
        se.max=if (se) rep(evals[,"se.max"], 2) else NA,
        type=as.factor(rep(c("y", "yhat"), each=n)))

      # FIXME: the constuction of type is ugly. Maybe identify a pattern and make a useful function
      gg.points = data.frame(x=opt.path[idx, names.x],
                             y=opt.path[idx, name.y],
                             type=as.factor(c(rep("init", length(idx.init)),
                                              rep("seq", length(idx.seq)),
                                              rep("prop", length(idx.proposed)))))

      # transform y and yhat values according to trafo function
      if (!is.null(trafo$y)) {
        tr = trafo$y
        gg.fun$y = tr(gg.fun$y) 
        gg.fun$se.min = tr(gg.fun$se.min)
        gg.fun$se.max = tr(gg.fun$se.max)
        gg.points$y = tr(gg.points$y)
      }

      # data frame with optimization criterion stuff
      gg.crit = data.frame(x=evals[,names.x], y=evals[,name.crit])
      if (!is.null(trafo$crit)) {
        gg.crit$y = trafo$crit(gg.crit$y)
      }

      # actual ploting stuff
      pl.fun = ggplot(data=gg.fun)
      # FIXME: add trafo functions to 1D numeric functions
      pl.fun = pl.fun + geom_line(aes(x=x, y=y, linetype=type), size=line.size)
      if (se & densregion) {
        gg.se = subset(gg.fun, type=="yhat")
        pl.fun = pl.fun + geom_ribbon(data=gg.se, aes(x=x, ymin=se.min, ymax=se.max), alpha=0.2)
      }

      pl.fun = pl.fun + geom_point(data=gg.points, aes(x=x, y=y, colour=type, shape=type), size=point.size)
      pl.fun = pl.fun + xlab(NULL)

      ylab = name.y
      if (!is.null(trafo$y)) {
        ylab = paste(name.y, " (", attr(trafo$y, "name"), "-transformed)", sep="")
      }

      pl.fun = pl.fun + scale_y_continuous(name=ylab)
      pl.fun = pl.fun + ggtitle(sprintf("Iter = %i, Gap = %.4e", i,
                                        calculateGap(opt.path[idx.pastpresent,], global.opt, control)))
      pl.fun = pl.fun + theme(legend.position="top",
                              legend.box = "horizontal",
                              axis.text.x=element_blank(),
                              panel.margin=unit(0, "lines"),
                              plot.title=element_text(size=11, face="bold"))

      pl.crit = ggplot(data=gg.crit, aes(x=x, y=y))
      pl.crit = pl.crit + geom_line(linetype="dotted", colour="black", size=line.size)
      pl.crit = pl.crit + geom_vline(xintercept=opt.path[idx.proposed, names.x], linetype="dashed", colour="darkgray", size=line.size)

      ylab = name.crit
      if (!is.null(trafo$crit)) {
        ylab = paste(name.crit, " (", attr(trafo$crit, "name"), "-transformed)", sep="")
      }

      pl.crit = pl.crit + scale_y_continuous(name=ylab)


      # arrange stuff in grid
      pl.all = grid.arrange(pl.fun, pl.crit, nrow=2)

      plot.sequence[[i]] = list(
        "pl.fun" = pl.fun,
        "pl.crit" = pl.crit,
        "pl.all" = pl.all
      )
    } else if (par.types %in% c("discrete")) {
      if (!noisy) {
        stopf("Deterministic 1d function with a single factor parameter are not supported.")
      }

      # FIXME: this is copy and paste (see numeric part)
      gg.points = data.frame(x=opt.path[idx, names.x],
                             y=opt.path[idx, name.y],
                             type=as.factor(c(rep("init", length(idx.init)),
                                              rep("seq", length(idx.seq)),
                                              rep("prop", length(idx.proposed)))))

      pl.fun = ggplot(data=gg.points, aes(x=x, y=y, colour=type, shape=type))
      pl.fun = pl.fun + geom_point(size=point.size)

      if (se & densregion) {
        print("SE is active")
      }

      pl.fun = pl.fun + xlab(names.x)
      pl.fun = pl.fun + ylab(name.y)
      pl.fun = pl.fun + scale_colour_discrete(name="type")
      pl.fun = pl.fun + ggtitle(sprintf("Iter = %i, Gap = %.4e", i,
                                              calculateGap(opt.path[idx.pastpresent,], global.opt, control)))
      pl.fun = pl.fun + theme(legend.position="top",
                              legend.box = "horizontal",
                              plot.title=element_text(size=11, face="bold"))
      print(pl.fun)
      plot.sequence[[i]] = list(
        "pl.fun" = pl.fun
      )
    }

    if (pause) {
      pause()
    }
  }
  return(plot.sequence)
}
