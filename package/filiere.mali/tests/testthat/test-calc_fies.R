test_that("calc_fies returns scores between 0 and 8", {
  set.seed(123)
  n <- 100
  fake_data <- data.frame(
    q1 = sample(c(1,2), n, replace = TRUE),
    q2 = sample(c(1,2), n, replace = TRUE),
    q3 = sample(c(1,2), n, replace = TRUE),
    q4 = sample(c(1,2), n, replace = TRUE),
    q5 = sample(c(1,2), n, replace = TRUE),
    q6 = sample(c(1,2), n, replace = TRUE),
    q7 = sample(c(1,2), n, replace = TRUE),
    q8 = sample(c(1,2), n, replace = TRUE)
  )
  res <- calc_fies(fake_data, questions = paste0("q", 1:8))
  expect_true(all(res$score_fies >= 0 & res$score_fies <= 8))
  expect_true(all(res$insecurite_moderee %in% c(0,1)))
  expect_true(all(res$insecurite_severe %in% c(0,1)))
})