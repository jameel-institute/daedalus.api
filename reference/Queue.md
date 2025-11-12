# Class for managing running jobs on the redis queue

Class for managing running jobs on the redis queue

Class for managing running jobs on the redis queue

## Public fields

- `controller`:

  RRQ controller

## Methods

### Public methods

- [`Queue$new()`](#method-Queue-new)

- [`Queue$queue_model_run()`](#method-Queue-queue_model_run)

- [`Queue$get_run_status()`](#method-Queue-get_run_status)

- [`Queue$get_run_results()`](#method-Queue-get_run_results)

------------------------------------------------------------------------

### Method `new()`

Initialise redis connection and rrq.

#### Usage

    Queue$new(queue_id = NULL, separate_process = TRUE)

------------------------------------------------------------------------

### Method `queue_model_run()`

Submit a model run job to the queue, and return the run id

#### Usage

    Queue$queue_model_run(parameters, model_version = NULL)

#### Arguments

- `parameters`:

  parameter values for the model run

------------------------------------------------------------------------

### Method `get_run_status()`

Get status information for a model run

#### Usage

    Queue$get_run_status(run_id)

#### Arguments

- `run_id`:

  the run id of the model run

------------------------------------------------------------------------

### Method `get_run_results()`

Get results data for a completed model run. Throws an error if the task
was not successful.

#### Usage

    Queue$get_run_results(run_id)

#### Arguments

- `run_id`:

  the run id of the model run
