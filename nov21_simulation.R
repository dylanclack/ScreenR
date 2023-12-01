


birthdayclass <- function(size) {
  test <- sample(1:365, size, replace = TRUE)
  
  result <- any(duplicated(test), test == TRUE)
  
  return()
}

df <- data.frame()

prob <- loop(1000, i)
row <- data.frame(size = i, prob = prob)
df <- rbind(df, row)

