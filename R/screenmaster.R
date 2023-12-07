#' @title Screen important variables
#' @description The \code{screen} function screens statistically significant predictors of an outcome variable from a dataframe which contains multiple variables. 
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame
#' @param sig Significance level, Default: 0.05
#' @param threshold The threshold proportion of missing data to screen out a variable, Default: 0.4
#' @return A character vector of significant variables
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(mtcars)
#’  screen(mpg~., mtcars)
#'  }
#' }
#’ @importFrom caret nearZeroVar nzv
#' @rdname screen
#' @export 

screen <- function(formula, data, sig = 0.05, threshold = 0.4) {
  # Stop messages -----------------------------------------------------------
  
  # Stop message for data frame
  if (!is.data.frame(data)) {
    stop("Please enter a data frame")
  }
  
  # Stop message for formula
  if (!inherits(formula, "formula")) {
    stop("Please enter a valid formula")
  }
  
  # Character vector --------------------------------------------------------
  
  # Extracting variables from formula
  y <- as.character(formula[[2]])
  vars <- all.vars(formula)
  
  # If user wants all variables
  if ("." == vars[2]) {
    vars <- names(data)
  }
  
  # Missing data screen -----------------------------------------------------
  
  vars <- missing.data(data, var.names = vars, threshold = threshold)
  
  # If y variable does not pass missing data screen
  if (!(y %in% vars)) {
    stop(paste0(y, " (dependent variable) contains more than ", threshold * 100, "% missing data"))
  }
  
  # Constant variable screen ------------------------------------------------
  
  vars <- variance_check(vars, data)
  
  # If y variable does not pass constant screen
  if (!(y %in% vars)) {
    stop(paste0(y, " (dependent variable) is either constant or near constant"))
  }

  # Statistical tests -------------------------------------------------------
  
  # Remove y variable from vars
  vars <- vars[vars != y]
  vars <- tests(vars, y, data, sig = sig)

  # Output ------------------------------------------------------------------
  
  return(vars)
  
}
