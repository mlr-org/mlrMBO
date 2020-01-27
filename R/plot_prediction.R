#' @title Plot for Learner Predictions
#'
#' @description
#' Generates plots for [mlr3::Learner], and [mlr3::Task]
#'
#' @param Learner ([mlr3::Learner])
#' @param Task ([mlr3::Task])
#' @param grid_ponints (`integer(1)`)\cr
#'   Resolution of the grid.
#' @param expand_range (`numeric(1)`)\cr
#'   Expand the prediction range over the range of the task.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("spam")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' plot_prediction(learner, task)
plot_prediction = function(learner, task, grid_points = 100L, expand_range = 0) {
  UseMethod("plot_prediction")
}
#' @export
plot_prediction.LearnerClassif = function(learner, task, grid_points = 100L, expand_range = 0) {

  features = task$feature_names
  if (length(features) != 2) {
    stop("plot_prediction only works for tasks with two features!")
  }
  if (!all(task$feature_types$type %in% "numeric")) {
    stop("plot_prediction only works for tasks with purely numeric features!")
  }

  learner = learner$clone()$train(task)

  sequenize = function(x, n) {
    r = range(x, na.rm = TRUE)
    d = diff(r)
    seq(from = r[1L] - expand_range * d, to = r[2L] + expand_range * d, length.out = n)
  }

  grid = cross_join(map(task$data(cols = features), sequenize, n = grid_points), sorted = FALSE)
  grid = cbind(grid, remove_named(as.data.table(learner$predict_newdata(grid)), c("row_id", "truth")))

  if (learner$predict_type == "prob") {
    grid[, prob.response := .SD[, paste0("prob.", response), with = FALSE] , by = response]
    raster_aes = aes_string(fill = "response", alpha = "prob.response")
  } else {
    raster_aes = aes_string(fill = "response")
  }

  ggplot(grid, aes_string(features[1L], features[2L])) +
    geom_raster(raster_aes) +
    geom_point(data = task$data(), aes_string(features[1L], features[2L], fill = task$target_names), shape = 21, size = 4, color = "black")
}
