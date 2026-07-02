# tests/test_async_guards.R
library(shiny)

# Mock environment similar to reactiveValues
mock_rv <- reactiveValues(
  model_running = FALSE,
  run_token = 0L
)

# Test 1: Re-entrancy guard
simulate_run <- function() {
  if (isTRUE(isolate(mock_rv$model_running))) {
    return("Blocked: Run already in progress")
  }
  isolate(mock_rv$model_running <- TRUE)
  isolate(mock_rv$run_token <- mock_rv$run_token + 1L)
  this_token <- isolate(mock_rv$run_token)
  return(paste("Started run with token", this_token))
}

# First run should start
res1 <- simulate_run()
# Second run should be blocked
res2 <- simulate_run()

cat("Test 1 Result (First Run): ", res1, "\n")
cat("Test 1 Result (Second Run): ", res2, "\n")

if (res2 == "Blocked: Run already in progress") {
  cat("Re-entrancy guard: PASSED\n")
} else {
  cat("Re-entrancy guard: FAILED\n")
}

# Test 2: Token cancellation
# Simulating a cancellation (which increments token or changes state)
cancel_run <- function() {
  isolate(mock_rv$model_running <- FALSE)
  isolate(mock_rv$run_token <- mock_rv$run_token + 1L) # Stale token won't match anymore
}

# Start run 3
isolate(mock_rv$model_running <- FALSE)
dispatch_res <- simulate_run()
captured_token <- isolate(mock_rv$run_token)

# User cancels run 3
cancel_run()

# Simulating late success callback for run 3
callback_run_3 <- function() {
  this_token <- captured_token
  if (this_token != isolate(mock_rv$run_token)) {
    return("Ignored stale results")
  }
  isolate(mock_rv$model_running <- FALSE)
  return("Applied stale results (Incorrect!)")
}

callback_res <- callback_run_3()
cat("Test 2 Callback Result: ", callback_res, "\n")
if (callback_res == "Ignored stale results") {
  cat("Token cancellation guard: PASSED\n")
} else {
  cat("Token cancellation guard: FAILED\n")
}
