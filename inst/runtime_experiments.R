######## RUNTIME EXERPERIMENTS ########
## Setup
library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3misc)
library(tictoc)
library(checkmate)
library(devtools)
library(patchwork)
load_all()

n_set = c(50, 100, 500, 1000, 2000)
p_set = c(5, 10, 25, 50, 100)
setup = data.table(expand.grid(n_set, p_set))
names(setup) = c("n", "p")

mod1 = lrn("regr.ranger")
mod2 = lrn("regr.lm")
cv5 = rsmp("cv", folds = 3L)

run_experiment = function(n, p, mod, print = FALSE) {

  assert_integerish(p, lower = 5L)
  assert_integerish(n, lower = 1L)

  set.seed(3100)

  x1 = runif(n = n)
  x2 = runif(n = n)
  x3 = runif(n = n)
  x4 = rbinom(n = n, size = 1, prob = 0.75)
  x5 = as.factor(sample(1:5, size = n, replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.1, 0.05)))
  X = data.table(x1, x2, x3, x4, x5)

  if (p > 5L) {
    pnoise = p - 5L
    Xnoise = data.table(matrix(rnorm(n = n*pnoise), nrow = n, ncol = pnoise))
    X = cbind(X, Xnoise)
  }

  # first 3 features main effects, interaction effect between x3 and x5
  ytrue = 4*x1 + 4*x2 + 4*x4*x3^2
  epsilon = rnorm(n = n, mean = 0, sd = ytrue*0.1)
  y = ytrue + epsilon
  # y = as.factor(ifelse(target < 5, 1, 0))
  dt = data.table(X, y)
  task = TaskRegr$new(id = "binary", backend = dt, target = "y")
  task$set_col_roles("x5", "stratum")
  mod$train(task)
  rr = resample(task, mod, cv5, store_models = TRUE)

  tic()
  sm = summary(object = mod, resample_result = rr)
  exectime = toc()

  if (print) print(sm)

  return(exectime$toc - exectime$tic)

}

runtime1 = pmap_dbl(setup, function(n, p) {
  run_experiment(n, p,mod1)
})

runtime2 =  pmap_dbl(setup, function(n, p) {
  run_experiment(n, p,mod2)
})

results = cbind(setup, runtime1, runtime2)
results$n = as.factor(results$n, levels = sort(unique(results$n)))

plt_rf = ggplot(data = results, aes(x = p, y = runtime1, group = n)) +
  geom_point(aes(colour = n)) +
  geom_line(aes(colour = n)) +
  theme_bw() +
  guides(colour = guide_legend(reverse=T)) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  ylab("runtime (sec)")
  # ylim(c(0, 1100))
  #  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 2))

plt_lm = ggplot(data = results, aes(x = p, y = runtime2, group = n)) +
  geom_point(aes(colour = n)) +
  geom_line(aes(colour = n)) +
  theme_bw() +
  guides(colour = guide_legend(reverse=T)) +
  scale_colour_grey(start = 0.8, end = 0.2) +
  ylab("")
 # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 2))
  # ylim(c(0, 1100))

plt = ggarrange(plt_rf, plt_lm, ncol=2, common.legend = TRUE, legend="right")

ggsave(plot = plt, filename = "inst/runtime.png", width = 6, height = 2)
