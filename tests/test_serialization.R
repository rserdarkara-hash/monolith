# tests/test_serialization.R
library(future)
library(promises)

# Setup multisession future plan
plan(multisession)

# Mock reactive values using actual shiny::reactiveValues
library(shiny)
mock_rv <- reactiveValues(
  idw_factors = list(region1 = list(act = 2.0)),
  tps_lambdas = list(region1 = list(act = 0.05))
)

# Closure over mock_rv (similar to get_regional_param)
get_regional_param_mock <- function(type, loc, target, default = 2.0) {
  field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
  val <- mock_rv[[field]][[loc]][[target]]
  if(is.null(val)) default else val
}

# Test if get_regional_param_mock can be serialized inside future
cat("Testing serialization of closure get_regional_param_mock...\n")
t <- tryCatch({
  f <- future({
    # Sourcing or forcing export of get_regional_param_mock
    force(get_regional_param_mock)
    "Success"
  }, globals = list(get_regional_param_mock = get_regional_param_mock))
  value(f)
}, error = function(e) {
  cat("Caught expected serialization error: ", e$message, "\n")
  "Failed"
})

cat("Result: ", t, "\n")
