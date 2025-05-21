daedalus_api_endpoint <- function(..., validate = TRUE,
                                  separate_process = FALSE) {
  queue <- Queue$new(separate_process = separate_process)
  state <- list(queue = queue)
  porcelain::porcelain_package_endpoint(
    "daedalus.api",
    ...,
    state = state,
    validate = validate
  )
}

check_for_redis <- function() {
  available <- redux::redis_available()
  expect_true(available)
}

start_test_queue_with_worker <- function() {
  queue <- Queue$new() # nolint
  rrq::rrq_worker_spawn(
    1L,
    controller = queue$controller,
    offload_path = get_results_dir()
  )
  queue
}

wait_for_task_complete <- function(run_id, controller, n_tries) {
  rrq::rrq_task_wait(
    run_id,
    controller = controller,
    timeout = n_tries
  )
}

test_worker_blocking <- function(...) {
  rrq::rrq_worker$new(
    get_queue_id(),
    offload_path = get_results_dir(),
    con = get_redis_connection(),
    ...
  )
}

daedalus_mock_costs <- function() {
  life_years_lost_age <- stats::setNames(
    c(5, 10, 15, 20),
    c("0-4", "5-19", "20-65", "65+")
  )
  list(
    total_cost = 100,
    economic_costs = list(
      economic_cost_total = 20,
      economic_cost_closures = 5,
      economic_cost_absences = 15
    ),
    education_costs = list(
      education_cost_total = 30,
      education_cost_closures = 10,
      education_cost_absences = 20
    ),
    life_years_lost = list(
      life_years_lost_total = 50,
      life_years_lost_age = life_years_lost_age
    )
  )
}

expect_nested_mock_costs <- function(costs) {
  expect_length(costs, 1L)
  total <- costs[[1]]
  expect_identical(total$id, "total")
  expect_identical(total$value, 100)
  expect_length(total$children, 3L)

  gdp <- total$children[[1]]
  expect_identical(gdp$id, "gdp")
  expect_identical(gdp$value, 20)
  expect_length(gdp$children, 2L)

  gdp_closures <- gdp$children[[1]]
  expect_identical(gdp_closures$id, "gdp_closures")
  expect_identical(gdp_closures$value, 5)
  expect_false("children" %in% names(gdp_closures))

  gdp_absences <- gdp$children[[2]]
  expect_identical(gdp_absences$id, "gdp_absences")
  expect_identical(gdp_absences$value, 15)
  expect_false("children" %in% names(gdp_absences))

  education <- total$children[[2]]
  expect_identical(education$id, "education")
  expect_identical(education$value, 30)
  expect_length(education$children, 2L)

  education_closures <- education$children[[1]]
  expect_identical(education_closures$id, "education_closures")
  expect_identical(education_closures$value, 10)
  expect_false("children" %in% names(education_closures))

  education_absences <- education$children[[2]]
  expect_identical(education_absences$id, "education_absences")
  expect_identical(education_absences$value, 20)
  expect_false("children" %in% names(education_absences))

  life_years <- total$children[[3]]
  expect_identical(life_years$id, "life_years")
  expect_identical(life_years$value, 50)
  expect_length(life_years$children, 4L)

  life_years_pre_school <- life_years$children[[1]]
  expect_identical(life_years_pre_school$id, "life_years_pre_school")
  expect_identical(life_years_pre_school$value, 5)
  expect_false("children" %in% names(life_years_pre_school))

  life_years_school_age <- life_years$children[[2]]
  expect_identical(life_years_school_age$id, "life_years_school_age")
  expect_identical(life_years_school_age$value, 10)
  expect_false("children" %in% names(life_years_school_age))

  life_years_working_age <- life_years$children[[3]]
  expect_identical(life_years_working_age$id, "life_years_working_age")
  expect_identical(life_years_working_age$value, 15)
  expect_false("children" %in% names(life_years_working_age))

  life_years_retirement_age <- life_years$children[[4]]
  expect_identical(life_years_retirement_age$id, "life_years_retirement_age")
  expect_identical(life_years_retirement_age$value, 20)
  expect_false("children" %in% names(life_years_retirement_age))
}
