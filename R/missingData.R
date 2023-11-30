# install and/or load package
# pacman::p_load(naniar)

# returns vectors of column names that have less than 40% of the data missing
missing.data <- function(data, var.names, threshold = 0.4) {
<<<<<<< HEAD:missingData.R
    final.vars <- c()
    for (var in var.names) {
      miss <- sum(is.na(data[[var]]))/length(data)
        if (miss < threshold) {
            final.vars <- c(final.vars, var)
        }
    }
    return(final.vars)
=======
    final_vars <- c()
    for (var in var.names) {
        miss <- sum(is.na(data[[var]])) / length(data[[var]])
        # miss <- pct_miss(data[[var]])/100
        if (miss < threshold) {
          final_vars <- c(final_vars, var)
        }
    }
    return(final_vars)
>>>>>>> fec441432e4c5c53e3803aa002740e5e26ff74bf:R/missingData.R
}

#example
pct_miss(airquality[["Ozone"]])
pct_miss(airquality[["Solar.R"]])

missing.data(airquality, c("Ozone", "Solar.R"), threshold = 0.2)
missing.data(airquality, c("Ozone", "Solar.R"), threshold = 0.3)
missing.data(airquality, c("Ozone", "Solar.R"))





#returns p value for cor between two quantitative variables
test.QQ <- function(x, y) {
    cor <- cor.test(x, y)
    return(cor$p.value)
}
test.QQ(mtcars$mpg, mtcars$wt)
InsectSprays
#returns p value for cor between two categorical variables
test.CC <- function(x, y) {

}


head(airquality)
?anova()
