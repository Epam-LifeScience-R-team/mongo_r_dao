library(mongolite)
library(jsonlite)

source("../../model/Answer.R")
source("../../model/Question.R")
source("../../dao/MongoDAO.R")

# Initialize MongoDAO
dao <- MongoDAO$new(connection_string)

# Initialize MongoLite. We will use raw access for testing our DAO
question_collection <- mongo("questions", url = connection_string)

# Fixtures
create_n_random_questions <- function (dao, question, n, env = parent.frame()) {
  i <- 0
  repeat {
    if (i >= n) {
      break
    }

    # Adding salt to Question text if it's repeated
    if (i > 0) {
        question$text <- paste0(question$text, i)
    }

    dao$create(question)

    i <- i + 1
  }

  withr::defer(dao$deleteAll(), env)
}

## Test Create
test_that("We can use DAO create operation", {
  question <- Question$new()
  question$text <- "What will be a resulted vector for c(1, FALSE)"
  question$text_type <- "single-option"
  question$topic_id <- "unkwnown"
  answers <- list(
    Answer$new("[TRUE FALSE]", FALSE),
    Answer$new("[1 0]", TRUE)
  )
  question$answers <- answers

  # Create test fixture
  create_n_random_questions(dao, question, 1)
  raw_question <- question_collection$find()
  expect_equal(question$text, raw_question$text[[1]])
})


test_that("We can use DAO for findAll", {
  question <- Question$new()
  question$text <- "What will be a resulted vector for c(1, FALSE)"
  question$text_type <- "single-option"
  question$topic_id <- "unkwnown"
  answers <- list(
    Answer$new("[TRUE FALSE]", FALSE),
    Answer$new("[1 0]", TRUE)
  )
  question$answers <- answers
  create_n_random_questions(dao, question, 10)

  questions <- dao$findAll()
  expect_equal(typeof(questions), "list")
  expect_equal(length(questions), 10)
  first_question <- questions[[1]]
  expect_equal(first_question$text, "What will be a resulted vector for c(1, FALSE)")
  expect_equal(length(first_question$answers), 2)
})

test_that("We can use DAO for findById", {
  question <- Question$new()
  question$text <- "What will be a resulted vector for c(1, FALSE)"
  question$text_type <- "single-option"
  question$topic_id <- "unkwnown"
  answers <- list(
    Answer$new("[TRUE FALSE]", FALSE),
    Answer$new("[1 0]", TRUE)
  )
  question$answers <- answers
  inserted_id <- dao$create(question)

  found_question <- dao$findById(inserted_id)
  expect_true(question$equals(found_question))
})

test_that("We can use DAO for findByCriteria", {
  random_question <- Question$new()
  random_question$text <- "What will be a resulted vector for c(1, FALSE)"
  random_question$text_type <- "single-option"
  random_question$topic_id <- "unkwnown"
  answers <- list(
    Answer$new("[TRUE FALSE]", FALSE),
    Answer$new("[1 0]", TRUE)
  )
  random_question$answers <- answers
  create_n_random_questions(dao, random_question, 10)

  question <- Question$new()
  question$text <- "What will be a resulted vector for c(1, FALSE)"
  question$text_type <- "single-option"
  question$topic_id <- "xxxx-xxxx-xxxx-xxxx"
  question$level <- 1
  answers <- list(
    Answer$new("[TRUE FALSE]", FALSE),
    Answer$new("[1 0]", TRUE)
  )
  question$answers <- answers
  inserted_id <- dao$create(question)

  found_questions <- dao$findByCriteria(topic_id = question$topic_id)
  expect_length(found_questions, 1)
  expect_equal(question$id, found_questions[[1]]$id)

  found_question <- dao$findByCriteria(level = question$level)
  expect_equal(question$id, found_questions[[1]]$id)

  found_question <- dao$findByCriteria(topic_id = question$topic_id, level = question$level)
  expect_equal(question$id, found_questions[[1]]$id)
})