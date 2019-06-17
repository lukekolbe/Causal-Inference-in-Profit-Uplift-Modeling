install.packages("testthat")
context("common support diagnostics")
library(testthat)
library(bartCause)

source(system.file("common", "linearData.R", package = "bartCause"))

n.train <- 80L
x <- conf
y <- data$spend
z <- data$treatment

x.new <- b_t.validate[-c(2,3,4,24,25)]
n.test <- nrow(x.new)


n.samples <- 5L
n.chains  <- 2L
fit <- bartc(y, z, x, method.trt = "bart", method.rsp = "bart",
             n.samples = n.samples, n.chains = n.chains, 
             n.burn = 10L,
             n.threads = 1L, n.trees = 500L, keepTrees = TRUE,
             verbose = FALSE)

# check predict for single row
expect_equal(length(predict(fit, x.new[1,], value = "y0")), n.samples * n.chains)

p.score <- predict(fit, x.new, value = "p.score")
#y      <- predict(fit, x.new, value = "y", combineChains = FALSE)
y1      <- predict(fit, x.new, value = "y1", combineChains = TRUE)
y0      <- predict(fit, x.new, value = "y0", combineChains = TRUE)
ite     <- predict(fit, x.new, value = "indiv.diff", combineChains = TRUE)

pred <- data.frame(rowMeans(y1))
write.csv(pred, "bart_f_b_pred.csv")
