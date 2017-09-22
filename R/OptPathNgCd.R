# Concept Drift OptPath
OptPathNgCd = R6Class("OptPathNgCd",
  inherit = OptPathNg,
  public = list(
    initialize = function(drift.param, window.function, ...) {
      self$drift.param = drift.param
      self$window.function = window.function
      super$initialize(...)
    },
    drift.param = NULL,
    window.function = NULL
  ),
  active = list(
    data = function() self$window.function(super$data)
  )
)
