source("./R/data-clean.R")
source("./R/quotaSample.R")

# get a subset from data with only selected brands
# remove incomplete cases
# remove not unique variables
# remove columns not used in training
prepareBrandDataset <- function(data, brand) {
  out <- data[data[, "brand"] %in% brand, ]
  # remove NAs
  out <- out[complete.cases(out), ]
  # some factor variables might have lost some values
  out <- vars2Factor(out, c("vehicleType", "model", "fuelType", "brand"))
  # not unique variables
  out <- remove0variance(out)
  #scale
  numericVars <- c("price", "yearOfRegistration", "powerPS", "kilometer")
  out[, numericVars] <- scale(out[, numericVars])
  # remove useless columns:
  out[, -which(colnames(out) %in% 
                      c("Longitude", "Latitude", "postalCode", 
                        "State.Abbreviation", "State", "name", "X"))]
}


#fix missing factor values
fixMissingFactors <- function(df){
  vars2Factor(df = df,
              var = names(df)[sapply(1:ncol(df), 
                                     function(x, data) 
                                       is.factor(data[,x]), 
                                     df)])
}


#convert to factors the selected variables in data.frame 
vars2Factor <- function(df, var) {
  df[, var] <- as.data.frame(apply(df[, var], 
                                   2, 
                                   function (x) as.factor(as.character(x))))
  (df)
}


# remove variables with 0 variance
remove0variance <- function(df) {
  df[, apply(df, 2, function (x) length(unique(x))) > 1]
}


#fit a model with the specified seed before training
defModelFit <- function(method, seed, ...) {
  cat("\n", method, ": \n", system.time({
    set.seed(seed)
    modFit <- train(..., method = method)
  }))
  modFit
}


# Get a cars dataset sample for comparing and tuning models  
VWSample <- function(size) {
  out <- autoData()
  if (nrow(out) > size) {
    out <- quotaSample(prepareBrandDataset(out, "volkswagen"),
                       size,
                       c("model", "fuelType", "gearbox", "notRepairedDamage"))
  }
  out
}