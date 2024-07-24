test_that("Can uglify es6 code", {
  s <- "class a { };"
  s_ugly <- s # should be "class a{}"
  expect_identical(uglify(s), s_ugly)
  expect_identical(prepare_code(s, TRUE), s)
  expect_identical(prepare_code(s, FALSE), s_ugly)
})
