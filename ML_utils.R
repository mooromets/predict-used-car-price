
# get a subset from data with only selected brands
# remove incomplete cases
# remove columns not used in training
prepareBrandDataset <- function(data, brand) {
  out <- data[data[, "brand"] %in% brand, ]
  # remove NAs
  out <- out[complete.cases(out), ]
  # some factor variables might have lost some values
  out <- vars2Factor(out, c("vehicleType", "model", "fuelType", "brand"))
  # remove variables with 0 variance
  out <- out[, apply(out, 2, function (x) length(unique(x))) > 1]  
  # remove useless columns:
  out[, -which(colnames(out) %in% 
                      c("Longitude", "Latitude", "postalCode", 
                        "State.Abbreviation", "State", "name", "X"))]
}


#convert to factors the selected variables in data.frame 
vars2Factor <- function(df, var) {
  df[, var] <- as.data.frame(apply(df[, var], 
                                   2, 
                                   function (x) as.factor(as.character(x))))
  (df)
}


#fit a model with the specified seed before training
defModelFit <- function(method, seed, ...) {
  print(system.time({
    set.seed(seed)
    modFit <- train(..., method = method)
  }))
  modFit
}