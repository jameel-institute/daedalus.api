# Generated by porcelain: do not edit by hand
`__porcelain__` <- function() {
  list(
    "GET /" = function(state, validate) {
      porcelain::porcelain_endpoint$new(
        "GET",
        "/",
        root,
        returning = porcelain::porcelain_returning_json("root"),
        validate = validate)
    },
    "GET /metadata" = function(state, validate) {
      porcelain::porcelain_endpoint$new(
        "GET",
        "/metadata",
        metadata,
        returning = porcelain::porcelain_returning_json("metadata"),
        validate = validate)
    },
    "POST /scenario/run" = function(state, validate) {
      porcelain::porcelain_endpoint$new(
        "POST",
        "/scenario/run",
        scenario_run,
        porcelain::porcelain_input_body_json("data", "scenarioRunRequest"),
        porcelain::porcelain_state(queue = state$queue),
        returning = porcelain::porcelain_returning_json("scenarioRunResponse"),
        validate = validate)
    },
    "GET /scenario/status/<run_id:string>" = function(state, validate) {
      porcelain::porcelain_endpoint$new(
        "GET",
        "/scenario/status/<run_id:string>",
        scenario_status,
        porcelain::porcelain_state(queue = state$queue),
        returning = porcelain::porcelain_returning_json("scenarioStatus"),
        validate = validate)
    },
    "GET /scenario/results/<run_id:string>" = function(state, validate) {
      porcelain::porcelain_endpoint$new(
        "GET",
        "/scenario/results/<run_id:string>",
        scenario_results,
        porcelain::porcelain_state(queue = state$queue),
        returning = porcelain::porcelain_returning_json("scenarioResults"),
        validate = validate)
    })
}
