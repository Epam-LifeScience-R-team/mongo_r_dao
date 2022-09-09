source("../../model/Answer.R")
source("../../model/Question.R")

test_that("Answer model works", {
  answer <- Answer$new()
  expected_text <- "my answer"
  answer$text <- expected_text
  expect_equal(answer$text, expected_text)
  expect_equal(answer$isCorrect, FALSE)

  answer$isCorrect <- TRUE
  expect_equal(answer$isCorrect, TRUE)
})

test_that("Question model works", {
  question <- Question$new()
  expected_text <- "my question"
  question$text <- expected_text

  first_answer <- Answer$new("yes")
  second_answer <- Answer$new("no", TRUE)

  question$answers_type <- "single-option"
  question$answers <- c(first_answer, second_answer)

  expect_equal(question$text, expected_text)
  expect_equal(length(question$answers), 2)
})