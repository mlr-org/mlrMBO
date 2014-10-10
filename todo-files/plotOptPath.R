
plotOptPath = function(opt.path, control) {
  assertClass(opt.path, "OptPath")
  opdf = as.data.frame(opt.path)
  opdf$dob = c(rep(0, 50), 1:58)
  print(head(opdf))
  yname = opt.path$y.names

  ggdata0 = opdf[opdf$dob == 0L,]
  ggdata1 = opdf[opdf$dob > 0L,]

  p1 = ggplot(mapping = aes_string(x = "dob", y = yname))
  p1 = p1 + geom_boxplot(data = ggdata0)
  p1 = p1 + geom_point(data = ggdata1)
   # p2 =

  grid.arrange(p1)

}

