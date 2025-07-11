#' Class for managing running jobs on the redis queue
#'
#' @importFrom R6 R6Class
#'
#' @keywords internal
Queue <- R6::R6Class(
  "Queue", # nolint
  cloneable = FALSE,
  public = list(
    #' @field controller RRQ controller
    controller = NULL,

    #' @description
    #' Initialise redis connection and rrq.
    initialize = function(queue_id = NULL, separate_process = TRUE) {
      logs_dir <- get_logs_dir()
      results_dir <- get_results_dir()

      # Configure rrq to store data > 1KB to disk
      offload_threshold_size <- 1000L

      # Connect to Redis
      con <- get_redis_connection()

      # Create queue
      queue_id <- queue_id %||% get_queue_id()
      self$controller <- rrq::rrq_controller(
        queue_id,
        offload_threshold_size = offload_threshold_size,
        offload_path = results_dir,
        con = con
      )

      dir.create(logs_dir, showWarnings = FALSE)
      dir.create(results_dir, showWarnings = FALSE)
      worker_config <- rrq::rrq_worker_config(
        offload_threshold_size = offload_threshold_size,
        logdir = logs_dir
      )
      rrq::rrq_worker_config_save(
        "localhost",
        worker_config,
        controller = self$controller
      )

      private$separate_process <- separate_process
    },

    #' @description
    #' Submit a model run job to the queue, and return the run id
    #'
    #' @param parameters parameter values for the model run
    #' @modelVersion requested model version to use for the run
    queue_model_run = function(parameters, model_version = NULL) {
      run_args <- list(
        parameters,
        model_version
      )
      rrq::rrq_task_create_call(
        model_run,
        run_args,
        separate_process = private$separate_process,
        controller = self$controller
      )
    },

    #' @description
    #' Get status information for a model run
    #'
    #' @param run_id the run id of the model run
    get_run_status = function(run_id) {
      rrq_status <- rrq::rrq_task_status(
        c(run_id),
        controller = self$controller
      )[1L]
      status <- switch(
        rrq_status,
        PENDING = list(
          runStatus = "queued",
          runSuccess = NULL,
          done = FALSE,
          runErrors = NULL
        ),
        RUNNING = list(
          runStatus = "running",
          runSuccess = NULL,
          done = FALSE,
          runErrors = NULL
        ),
        COMPLETE = list(
          runStatus = "complete",
          runSuccess = TRUE,
          done = TRUE,
          runErrors = NULL
        ),
        list(
          runStatus = "failed",
          runSuccess = FALSE,
          done = TRUE,
          runErrors = NULL
        )
      )
      status$runId <- run_id
      # include errors for failed jobs
      if (status$done[1L] && !status$runSuccess[1L]) {
        se <- rrq::rrq_task_result(
          run_id,
          controller = self$controller,
          error = FALSE
        )
        status$runErrors <- list(
          list(error = "SERVER_TASK_ERROR", detail = conditionMessage(se))
        )
      }
      status
    },

    #' @description
    #' Get results data for a completed model run. Throws an error if the task
    #' was not successful.
    #'
    #' @param run_id the run id of the model run
    get_run_results = function(run_id) {
      rrq::rrq_task_result(run_id, controller = self$controller, error = TRUE)
    }
  ),
  private = list(
    separate_process = NULL
  )
)

get_queue_id <- function() {
  Sys.getenv("DAEDALUS_QUEUE_ID", "daedalus.run.queue")
}

get_redis_connection <- function() {
  host <- Sys.getenv("REDIS_CONTAINER_NAME", "")
  if (!nzchar(host)) {
    host <- NULL
  }
  redux::hiredis(host = host)
}

get_logs_dir <- function() {
  Sys.getenv("DAEDALUS_LOGS_DIR", file.path("logs", "worker"))
}

get_results_dir <- function() {
  Sys.getenv("DAEDALUS_RESULTS_DIR", file.path("daedalus", "results"))
}
