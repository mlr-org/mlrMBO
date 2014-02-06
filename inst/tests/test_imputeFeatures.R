context("imputeFeatures")

if (interactive()) {

test_that("imputeFeatures", {
  ps = makeParamSet(
    makeNumericParam("real1", lower=0, upper=1000),
    makeIntegerParam("int1", lower=-100, upper=100),
    makeDiscreteParam("discVecs", values=c("yes", "no")),
    makeNumericVectorParam("realVec", len=10, lower=1:10*-50, upper=1:10*50,
      requires=(quote(discVecs == "yes"))),
    makeIntegerVectorParam("intVec", len=3, lower=0, upper=c(5, 10, 20),
      requires=(quote(discVecs == "yes"))),
    makeNumericParam("real2", lower=-1, upper=1),
    makeDiscreteParam("disc1", values=c("foo", "bar"), requires=quote(real2 < 0)),
    makeNumericParam("real3", lower=-100, upper=100, requires=quote(real2 > 0)),
    makeDiscreteParam("disc2", values=c("a", "b", "c")),
    makeNumericParam("realA", lower=0, upper=100, requires=quote(disc2 == "a")),
    makeIntegerParam("intA", lower=-100, upper=100, requires=quote(disc2 == "a")),
    makeDiscreteParam("discA", values=c("m", "w"), requires=quote(disc2 == "a")),
    makeNumericParam("realB", lower=-100, upper=100, requires=quote(disc2 == "b")),
    makeDiscreteParam("discB", values=c("R", "NR"), requires=quote(disc2 == "b")),
    makeNumericParam("realBR", lower=0, upper=2*pi,
      requires=quote(identical(discB, "R") && identical(disc2, "b"))),
    makeNumericParam("realBNR", lower=0, upper=2*pi,
      requires=quote(identical(discB, "NR") && identical(disc2, "b")))
  )

  des = generateDesign(10, ps, randomLHS)
  des2 = imputeFeatures(des, ps, list(feature.impute="up"))

  na.string = "__miss__"
  imp.values = list(2000, 200, na.string, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000,
    10, 20, 40, 2, na.string, 200, na.string, 200, 200, na.string, 200, na.string, 4*pi, 4*pi)

  for(i in seq_along(colnames(des))) {
    nas = des2[is.na(des[[i]]),i]
    if(length(nas) > 0) {
      if(is.numeric(nas))
        expect_equal(nas, rep(imp.values[[i]], length(nas)))
      else
        expect_equivalent(nas, expected=factor(rep(imp.values[[i]], length(nas))))
    }
  }

  cnd = colnames(des)

  des3 = imputeFeatures(des, ps, list(feature.impute="median"))
  for(i in seq_along(colnames(des))) {
    nas = des3[is.na(des[[i]]), paste(cnd[i], "vals", sep='.'),]
    if(length(nas) > 0) {
      if(is.numeric(nas)) {
        expect_equal(nas, rep(median(des[[i]], na.rm=TRUE), length(nas)))
        navar = des3[, paste(cnd[i], "nas", sep='.'),]
        expect_equal(navar, factor(is.na(des[[i]])))
      } else {
        expect_equivalent(nas, expected=factor(rep(imp.values[[i]], length(nas))))
      }
    }
  }
})

}
