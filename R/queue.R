#' Class for managing running jobs on the redis queue
#' #'
#' @keywords internal
Queue <- R6::R6Class("Queue",
  cloneable = FALSE,
  public = list(
    #' @field controller RRQ controller
    controller = NULL,

    #' @description
    #' Initialise redis connection and rrq.
    initialize = function(logs_dir = "logs/worker", results_dir = "daedalus/results") {
      # Connect to Redis
      con <- redux::hiredis(host = get_redis_host())

      # Configure rrq to store data > 1KB to disk
      queue_id <- get_queue_id()
      rrq::rrq_configure(queue_id, store_max_size = 1000, offload_path = results_dir, con = con)

      # Create queue
      self$controller <- rrq::rrq_controller(queue_id, con = con)
      dir.create(logs_dir, showWarnings = FALSE)
      worker_config <- rrq::rrq_worker_config(logdir = logs_dir)
      rrq::rrq_worker_config_save("localhost", worker_config, controller = self$controller)
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
      rrq::rrq_task_create_call(model_run, run_args,
                                separate_process = TRUE, controller = self$controller)
    },

    #' @description
    #' Get status information for a model run
    #'
    #' @param run_id the run id of the model run
    get_run_status = function(run_id) {
      rrq_status <- rrq::rrq_task_status(c(task_id), controller = self$controller)[1]
      status <- switch(rrq_status,
                       PENDING = list(runStatus = "queued", runSuccess = NULL, done = FALSE),
                       RUNNING = list(runStatus = "running", runSuccess = NULL, done = FALSE),
                       COMPLETE = list(runStatus = "complete", runSuccess = TRUE, done = TRUE),
                       list(runStatus = "faled", runSuccess = FALSE, done = TRUE)
      )
      status$runId <- run_id
      status$runErrors <- NULL # TODO: include errors for failed jobs
      status
    },

    #' @description
    #' Get results data for a completed model run. Throws an error if the task was not successful.
    #'
    #' @param run_id the run id of the model run
    get_run_results = function(run_id) {
      rrq::rrq_task_result(run_id, controller = self$controller, error = TRUE)
    }
  ),
)

get_queue_id <- function() {
  Sys.getenv("DAEDALUS_QUEUE_ID", "daedalus.run.queue")
}

get_redis_host <- function() {
  name <- Sys.getenv("REDIS_CONTAINER_NAME", "")
  if (nzchar(name)) name else NULL
}