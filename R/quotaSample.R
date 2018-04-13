require(fifer)

# quotaSample
# Get a qouta sample from data
#'@param data - The input dataset.
#'@param size - The desired sample size.
#'@param group - The grouping variables. 
quotaSample <- function(data, size, group){
  eachGroupSize <- round(size / prod(sapply(gr, function(x) {
                                                  length(levels(dataset[,x]))
                                                })))
  #iteratively find the most accurate size of output dataset
  repeat {
    outData <- stratified(data, group, size = rep(eachGroupSize))
    if (abs(nrow(outData) / size - 1) > 0.05) {
      newSize <- round(eachGroupSize * size / nrow(outData))
      if (newSize == eachGroupSize)
        break
      else
        eachGroupSize <- newSize
    }
    else
      break
  }
  outData
}