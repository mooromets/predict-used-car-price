
# get a subset from data with only selected brands
# remove incomplete cases
# remove columns not used in training
prepareBrandDataset <- function(data, brand) {
  out <- data[data[, "brand"] %in% brand, ]
  # remove NAs
  out <- out[complete.cases(out), ]
  # convert y to factor
  out$model <- as.factor(as.character(out$model))
  # remove useless columns:
  out[, -which(colnames(out) %in% 
                      c("Longitude", "Latitude", "postalCode", 
                        "State.Abbreviation", "name"))]
}

#fit a model with the specified seed before training
defModelFit <- function(method, seed, ...) {
  print(system.time({
    set.seed(seed)
    modFit <- train(..., method = method)
  }))
  modFit
}