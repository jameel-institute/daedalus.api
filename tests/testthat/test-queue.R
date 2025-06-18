check_for_redis()

test_that("can queue model run", {
  queue <- Queue$new()
  mock_rrq_task_create_call <- mockery::mock()
  mockery::stub(
    queue$queue_model_run,
    "rrq::rrq_task_create_call",
    mock_rrq_task_create_call
  )

  params <- list(1, 2)
  queue$queue_model_run(params, "0.1.0")

  mockery::expect_called(mock_rrq_task_create_call, 1)
  expect_identical(
    mockery::mock_args(mock_rrq_task_create_call)[[1]],
    list(
      model_run,
      list(params, "0.1.0"),
      separate_process = TRUE,
      controller = queue$controller
    )
  )
})

expect_status_from_queue <- function(
  rrq_status,
  expected_run_status,
  run_id = "12345"
) {
  queue <- Queue$new()
  mock_rrq_status <- mockery::mock(rrq_status)
  mockery::stub(queue$get_run_status, "rrq::rrq_task_status", mock_rrq_status)

  status <- queue$get_run_status(run_id)

  mockery::expect_called(mock_rrq_status, 1)
  expect_identical(
    mockery::mock_args(mock_rrq_status)[[1]],
    list("12345", controller = queue$controller)
  )

  expect_identical(status, expected_run_status)
}

test_that("can get queued status", {
  expect_status_from_queue(
    "PENDING",
    list(
      runStatus = "queued",
      runSuccess = NULL,
      done = FALSE,
      runErrors = NULL,
      runId = "12345"
    )
  )
})

test_that("can get running status", {
  expect_status_from_queue(
    "RUNNING",
    list(
      runStatus = "running",
      runSuccess = NULL,
      done = FALSE,
      runErrors = NULL,
      runId = "12345"
    )
  )
})

test_that("can get complete status", {
  expect_status_from_queue(
    "COMPLETE",
    list(
      runStatus = "complete",
      runSuccess = TRUE,
      done = TRUE,
      runErrors = NULL,
      runId = "12345"
    )
  )
})

test_that("can get failed status", {
  queue <- Queue$new()

  mock_rrq_status <- mockery::mock("FAILED")
  mockery::stub(queue$get_run_status, "rrq::rrq_task_status", mock_rrq_status)

  mock_rrq_task_result <- mockery::mock(simpleCondition("test error"))
  mockery::stub(
    queue$get_run_status,
    "rrq::rrq_task_result",
    mock_rrq_task_result
  )

  status <- queue$get_run_status("12345")

  expect_identical(
    status,
    list(
      runStatus = "failed",
      runSuccess = FALSE,
      done = TRUE,
      runErrors = list(list(
        error = "SERVER_TASK_ERROR",
        detail = "test error"
      )),
      runId = "12345"
    )
  )
})

test_that("can get run results", {
  queue <- Queue$new()

  task_result <- list("test results")
  mock_rrq_task_result <- mockery::mock(task_result)
  mockery::stub(
    queue$get_run_results,
    "rrq::rrq_task_result",
    mock_rrq_task_result
  )

  result <- queue$get_run_results("12345")
  expect_identical(result, task_result)

  mockery::expect_called(mock_rrq_task_result, 1)
  args <- mockery::mock_args(mock_rrq_task_result)[[1]]
  expect_identical(
    args,
    list(
      "12345",
      controller = queue$controller,
      error = TRUE
    )
  )
})
