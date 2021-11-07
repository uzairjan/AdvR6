

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
obj <- greedy_knapsack()

test_that("Checking for correct objects", {
  expect_silent(result <- obj$greedyKnapSack(knapsack_objects[1:8,], 3000))
  expect_named(result, c("value", "factors"))
})

test_that("Testing for incorrect inputs", {
  expect_error(obj$greedyKnapSack("hej", 3500))
  expect_error(obj$greedyKnapSack(df = knapsack_objects[1:8,], W = -3500))
})

test_that("Should return correct results.", {
  gk <- obj$greedyKnapSack(df = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$factors) %in% c(3, 8)))

  gk <- obj$greedyKnapSack(df = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$factors) %in% c(3, 8)))

  gk <- obj$greedyKnapSack(df = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$factors) %in% c(3, 8)))

  gk <- obj$greedyKnapSack(df = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$factors) %in% c(3, 8)))

  st <- system.time(gk <- obj$greedyKnapSack(df = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  gk <- obj$greedyKnapSack(df = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 192647)

  gk <- obj$greedyKnapSack(df = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 270290)
})
