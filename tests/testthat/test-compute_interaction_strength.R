require("rpart")
require("iml")

test_that("multiplication works", {
  # model that has no interaction = single split rpart
  set.seed(1234L)
  myiris = iris[1:100,]
  myiris$Species = droplevels(myiris$Species)
  rp = rpart(Species ~ ., data = myiris, )
  mod = Predictor$new(rp, data = iris, class = "setosa")
  effects = FeatureEffects$new(mod, grid.size = 100L)
  ci = compute_interaction_strength(mod, effects)
  expect_true(ci < 0.01)
})
