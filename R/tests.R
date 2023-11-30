tests <- function(vars, y, data, sig = 0.05) {
  significant_vars <- c()
  for (var in vars) {
    p_value <- test(data$var, data$y)
    if (p_value <= sig) append(significant_vars, var)
  }
  return (significant_vars)
}

test <- function(var, y) {
  is_numeric_var <- is.numeric(var)
  is_numeric_y <- is.numeric(y)
  if (is_numeric_var & is_numeric_y) {
    return (test_QQ(var, y))
  } else if (!is_numeric_var & !is_numeric_y) {
    return (test_CC(var, y))
  } else {
    return (test_QC(var, y))
  }
}

# quantitative + quantitative
test_QQ <- function(x, y) {
  p_value <- cor.test(x, y)$p.value
  return(p_value)
}

# categorical + quantitative
test_QC <- function(var, y) {
  if (is.numeric(var)) {
    numeric_var <- var
    cat_var <- y
  } else {
    numeric_var <- y
    cat_var <- var
  }
  test_result <- aov(numeric_var ~ cat_var)
  p_value <- summary(test_result)[[1]]["Pr(>F)"][[1]][1]
  return (p_value)
}

test_CC <- function(var, y) {
  p_value <- suppressWarnings(chisq.test(var, y))$p.value
  return (p_value)
}

# examples for individual tests

# data("starwars")
# test(starwars$height, starwars$mass)
# test(starwars$sex, starwars$height)
# test(starwars$sex, starwars$gender)
