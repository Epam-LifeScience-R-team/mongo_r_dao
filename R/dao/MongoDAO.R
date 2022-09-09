MongoDAO <- R6::R6Class("MongoDAO",
  public = list(
    connectionString = NULL,
    initialize = function (connectionString) {
      self$connectionString <- connectionString
      private$collection <- mongo("questions", url = self$connectionString)
    },
    deleteAll = function () {
      return(private$collection$remove("{}"))
    },
    findAll = function () {
      df <- private$collection$find("{}")
      return(apply(df, 1, private$fromList))
    },
    findById = function (id) {
      query <- sprintf('{"id": "%s"}', id)
      df <- private$collection$find(query)
      if (nrow(df) == 1) {
        return(apply(df, 1, private$fromList)[[1]])
      }
      else {
        stop(paste0("findById result != 1, query:", query))
      }
    },
    findByCriteria = function (level = NULL, topic_id = NULL) {
      query <- list()
      if (!is.null(level)) {
        query["level"] <- level
      }

      if (!is.null(topic_id)) {
        query["topic_id"] <- topic_id
      }

      df <- private$collection$find(jsonlite::toJSON(query, auto_unbox = TRUE))
      return(apply(df, 1, private$fromList))
    },
    create = function (question) {
      # Insert a new Question to collection
      private$collection$insert(jsonlite::toJSON(question$toList(), auto_unbox = TRUE))
      return(question$id)
    }
  ),
  private = list(
    collection = NULL,
    fromList = function (questionDto) {
      question = Question$new(
        id = questionDto$id,
        text = questionDto$text,
        text_type = questionDto$text_type,
        level = questionDto$level,
        topic_id = questionDto$topic_id,
        answers_type = questionDto$answers_type,
        answers = apply(questionDto$answers, 1, Answer$new)
      )

      return(question)
    }
  )
)