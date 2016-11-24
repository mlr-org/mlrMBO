trainPredict = function(lrn, task, rin, iter) {
  rin2 = makeFixedHoldoutInstance(rin$train.inds[[iter]], rin$test.inds[[iter]], task$task.desc$size)
  resample(lrn, task, rin2)
}
