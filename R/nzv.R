variance_check <- function(nameVec, data) {
  
  numVec <- character()
  charVec <- character()
  
  for(i in 1:length(nameVec)) {
    if(is.numeric(as.data.frame(data[,nameVec][i])[,1])){
      numVec <- c(numVec, nameVec[i])
    } else {
      charVec <- c(charVec, nameVec[i])
    }
  }
  
  badVars <- character()
  if(length(numVec) != 0) {
    variance_df <- data[numVec]
    badVarsPos <- const_check(variance_df)
    for (i in 1:length(badVarsPos)){
      badVars <- c(badVars, colnames(variance_df)[badVarsPos[i]])
    } 
  }
  
  if(length(charVec) != 0) {
    for(i in 1:length(charVec)) {
      if(length(unique(as.data.frame(data[charVec])[,i])) == 1) {
        badVars <- c(badVars, charVec[i])
      }
    }
  }
  
  good_vars <- colnames(data[!colnames(data) %in% badVars])
  
  if(length(good_vars) == 0){
    return(nameVec)
  }

  return(good_vars)
}

#copy of caret::nearZeroVar
const_check <- function(x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE, 
                        foreach = FALSE, allowParallel = TRUE) {
  if(!foreach) 
    return(nzv(x, freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = saveMetrics, names = names))
  `%op%` <- getOper(foreach && allowParallel && getDoParWorkers() > 1)
  if(saveMetrics) {
    res <- foreach(name = colnames(x), .combine = rbind) %op% 
      {
        r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = TRUE)
        r[, "column"] <- name
        r
      }
    res <- res[, c(5, 1, 2, 3, 4)]
    rownames(res) <- as.character(res$column)
    res$column <- NULL
  }
  else {
    res <- foreach(name = colnames(x), .combine = c) %op% 
      {
        r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, 
                 saveMetrics = FALSE)
        if (length(r) > 0 && r == 1) 
          TRUE
        else FALSE
      }
    res <- which(res)
    if (names) {
      res <- colnames(x)[res]
    }
  }
  res
}

#copy of caret::nzv
nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE) {
  if (is.null(dim(x))) 
    x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data) {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0)
    }
    w <- which.max(t)
    return(max(t, na.rm = TRUE)/max(t[-w], na.rm = TRUE))
  })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique/apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics) {
    out <- data.frame(freqRatio = freqRatio, percentUnique = percentUnique, 
                      zeroVar = zeroVar, nzv = (freqRatio > freqCut & percentUnique <= 
                                                  uniqueCut) | zeroVar)
  }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= 
                    uniqueCut) | zeroVar)
    names(out) <- NULL
    if (names) {
      out <- colnames(x)[out]
    }
  }
  out
}
