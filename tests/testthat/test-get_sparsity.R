require("rpart")

test_that("correct number of features for rpart", {
  set.seed(1234L)
  rp = rpart(Species ~ ., data = iris)
  no_used1 = length(unique(rp$frame$var[rp$frame$var != "<leaf>"]))
  mod = Predictor$new(rp, data = iris)
  effs = FeatureEffects$new(mod, grid.size = 20L)
  no_used2 = get_sparsity(effects = effs)
  expect_equal(no_used1, no_used2)
})
