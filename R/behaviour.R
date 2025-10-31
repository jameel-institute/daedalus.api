#' Translate behaviour choice to daedalus input
#'
#' @description
#' Note that the mapping from the dashboard options to the optimism parameter
#' is reversed. For a dashboard option of 'low', optimism is high. This allows
#' the dashboard option to present as 'change in public behaviour'.
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
      baseline_optimism = 0.75,
      responsiveness = 0.01,
      behav_effectiveness = 0.2
    ),
    medium = daedalus::daedalus_new_behaviour(
      hospital_capacity,
      baseline_optimism = 0.5,
      responsiveness = 0.01,
      behav_effectiveness = 0.2
    ),
    high = daedalus::daedalus_new_behaviour(
      hospital_capacity,
      baseline_optimism = 0.25,
      responsiveness = 0.01,
      behav_effectiveness = 0.2
    )
  )

  behaviour
}
