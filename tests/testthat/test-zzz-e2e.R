test_that("can run server", {
  bg <- porcelain::porcelain_background$new(api)
  bg$start()
  r <- bg$request("GET", "/")
  expect_identical(httr::status_code(r), 200)

  dat <- httr::content(r)
  expect_identical(dat$status, "success")
  expect_null(dat$errors)
  expect_identical(dat$data$daedalus, package_version_string("daedalus"))
  expect_identical(
    dat$data$daedalus.api, package_version_string("daedalus.api")
  )
})
