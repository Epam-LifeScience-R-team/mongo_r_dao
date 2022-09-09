#' Model, see for details: https://github.com/Epam-LifeScience-R-team/questr/wiki/Quiz-DB-schema
#'[
#'    {
#'        "_id": "<MongoDB generated ID>",
#'        "text": "<chr: question text>",
#'        "text_type": "<chr: html, plain text, etc.>",
#'        "level": "<int: question difficulty, weight>",
#'        "topic_id": "<chr: topic ID>",
#'        "answers_type": "<chr: single, multiple or freestyle>",
#'        "answers": [
#'            {
#'                "text": "<chr: answer text>",
#'                "": "<chr: html, plain text, etc>",
#'                "is_correct": "<bool: true or false>",
#'            }
#'        ]
#'    }
#']
Question <- R6::R6Class("Question", list(
  id = uuid::UUIDgenerate(),
  text = NULL,
  text_type = "plain text",
  level = 0,
  topic_id = "unknown",
  answers_type = "single",
  answers = list(),
  initialize = function (id = uuid::UUIDgenerate(), text = NA, text_type = "plain text", level = 0, topic_id = "unknown",
                         answers_type = "single", answers = list()) {
    self$id = id
    self$text = text
    self$text_type = text_type
    self$level = level
    self$topic_id = topic_id
    self$answers_type = answers_type
    self$answers = answers
  },
  toList = function () {
    return(list(
      id = self$id,
      text = self$text,
      text_type = self$text_type,
      level = self$level,
      topic_id = self$topic_id,
      answers_type = self$answers_type,
      answers = purrr::map(self$answers, ~.$toList())
    ))
  },
  equals = function (other) {
    return(
      self$id == other$id && self$text == other$text && self$text_type == other$text_type &&
        self$level == other$level && self$topic_id == other$topic_id && self$answers_type == other$answers_type
    )
  }

))