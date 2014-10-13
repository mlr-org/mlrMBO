
renderPCPPlot = function(data, xnames, alpha) {
  requirePackages("GGally")

  # ggdata = data[, colnames, drop = FALSE]
  columns = match(xnames, colnames(data))
  args = list(columns = columns)
  if (alpha) {
    data$.alpha = normalize(data$dob, "scale", range = c(0, 1))
    print(data$.alpha)
    args$alphaLines = ".alpha"
  }
  args$data = data
  p = do.call(ggparcoord, args)
  p = p + geom_line()
}
