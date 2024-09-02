test_that("Can parse arguments", {
  expect_mapequal(
    parse_main(character()),
    list(
      log_level = "info",
      validate = FALSE,
      port = 8001
    )
  )
  expect_mapequal(
    parse_main("--port=8080"),
    list(
      log_level = "info",
      validate = FALSE,
      port = 8080
    )
  )
  expect_mapequal(
    parse_main(c("--port=8080", "--validate")),
    list(
      log_level = "info",
      validate = TRUE,
      port = 8080
    )
  )
  expect_mapequal(
    parse_main(c("--log-level=debug", "--validate")),
    list(
      log_level = "debug",
      validate = TRUE,
      port = 8001
    )
  )
})


test_that("Can construct api", {
  mock_run <- mockery::mock()
  mock_api <- mockery::mock(list(run = mock_run))
  mockery::stub(main, "api", mock_api)
  main("--log-level=debug")

  mockery::expect_called(mock_api, 1L)
  expect_identical(
    mockery::mock_args(mock_api)[[1L]],
    list(FALSE, "debug", configure_queue = TRUE)
  )

  mockery::expect_called(mock_run, 1L)
  expect_identical(
    mockery::mock_args(mock_run)[[1L]],
    list("0.0.0.0", port = 8001L) # mockery::mock_args() returns an integer
  )
})

test_that("Can create worker", {
  mock_loop <- mockery::mock()
  mock_new_worker <- mockery::mock(list(loop = mock_loop))
  mockery::stub(main_worker, "rrq::rrq_worker$new", mock_new_worker)
  main_worker()
  mockery::expect_called(mock_loop, 1)
})
