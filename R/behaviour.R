#' Translate behaviour choice to daedalus input
#'
#' @param x A single string for the behavioural choice.
#'
#' @param hospital_capacity The country hospital capacity.
#'
#' @return Either `NULL`, or a `<daedalus_behaviour>` with 'new' behavioural
#' parameters, suitable for passing to the `behaviour` argument of
#' [daedalus::daedalus()].
#'
#' @export
process_behaviour_choice <- function(x, hospital_capacity) {
  behaviour <- switch(
    x,
    none = NULL,
    low = daedalus::daedalus_new_behaviour(
      hospital_capacity,
      baseline_optimism = 0.25
    ),
    medium = daedalus::daedalus_new_behaviour(
      hospital_capacity,
      baseline_optimism = 0.5
    ),
    high = daedalus::daedalus_new_behaviour(
      hospital_capacity,
      baseline_optimism = 0.75
    )
  )

  behaviour
}
