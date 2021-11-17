suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42)
n <- 2000

knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

btfObc <- brute_force_knapsack()

test_that("Correct object is returned", {
  expect_silent(bfk <- btfObc$bruteForceKnapsack(x = knapsack_objects[1:8, ], weight = 3500))
  expect_named(bfk, c("value", "factors"))
})

test_that("functions rejects errounous input.", {
  expect_error(btfObc$bruteForceKnapsack("hej", 3500))
  expect_error(btfObc$bruteForceKnapsack(x = knapsack_objects[1:8, ], weight = -3500))
})

test_that("Response with correct result.", {
  bfk <- btfObc$bruteForceKnapsack(x = knapsack_objects[1:8, ], weight = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$factors) %in% c(5, 8)))

  bfk <- btfObc$bruteForceKnapsack(x = knapsack_objects[1:12, ], weight = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$factors) %in% c(5, 8)))

  bfk <- btfObc$bruteForceKnapsack(x = knapsack_objects[1:8, ], weight = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$factors) %in% c(3, 8)))

  bfk <- btfObc$bruteForceKnapsack(x = knapsack_objects[1:12, ], weight = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$factors) %in% c(3, 8)))

  st <- system.time(bfk <- btfObc$bruteForceKnapsack(x = knapsack_objects[1:16, ], weight = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})
