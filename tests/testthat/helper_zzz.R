set.seed(1)
configureMlr(show.learner.output = FALSE)
options(mlrMBO.show.info = FALSE, mlrMBO.debug.mode = TRUE)

# Librarys for the models used in the tests. Do we need to load them? Last time
# I looked at the package we did not. But now i get errors without ..
library(DiceKriging)
library(rpart)
library(randomForest)
