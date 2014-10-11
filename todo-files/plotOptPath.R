
plotOptPath = function(opt.path, control) {
  assertClass(opt.path, "OptPath")
  ps = opt.path$par.set
  xnames = getParamIds(ps, rep = TRUE, with.nr = TRUE)
  opdf = as.data.frame(opt.path)

  opdf$dob = c(rep(0, 50), 1:58)
  yname = opt.path$y.names

  ggy0 = opdf[opdf$dob == 0L,]
  ggy1 = opdf[opdf$dob > 0L,]

  # p1 = ggplot(mapping = aes_string(x = "dob", y = yname))
  # p1 = p1 + geom_boxplot(data = ggy0)
  # p1 = p1 + geom_point(data = ggy1)

  # ggcrit = opdf[opdf$dob > 0L,]
  # print(head(ggcrit))

  # p2 = ggplot(data = ggcrit, mapping = aes_string(x = "dob", y = control$infill.crit))
  # p2 = p2 + geom_point()

  p3 = renderPCPPlot(opdf, xnames, alpha = TRUE)
  # print(p3)

  # grid.arrange(p1, p2)

}

