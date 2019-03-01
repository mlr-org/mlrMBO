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
    window.function = NULL,
    window.function.active = TRUE
  ),
  active = list(
    data = function() {
      if (self$window.function.active) {
        self$window.function(super$data)
      } else {
        super$data
      }
    }
  )
)
