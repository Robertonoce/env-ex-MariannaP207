#' which parent environments of <env> contain <name>?
#' inputs:
#'    name: a variable name
#'    env: valid inputs for pryr:::to_env, defaults to the calling frame
#' output: a list of environments containing <name>
anywhere <- function(name, env = parent.frame()) {
  checkmate::assert_string(name)
  #to_env also takes care of checks for <env>
  env <- pryr:::to_env(env)

  result <- list()
  # terminate recursion in emptyenv and return empty
  if (identical(env, emptyenv())) {
    return(list())
  }
  # if <name> is in current <env>, <env> is added to results list ...
  if (exists(name, env, inherits = FALSE)) {
    result <- append(result, env)
  }
  # .... and then we go up one level to the next environment and start to look there
  result <- append(result, anywhere(name, parent.env(env)))
  result
}
