
getBrandDataset <- function(data, brand) {
  out <- data[data[, "brand"] == brand, ]
  # remove NAs
  out <- out[complete.cases(out), ]
  # convert y to factor
  out$model <- as.factor(out$model)
  # remove useless columns:
  out[, -which(colnames(out) %in% 
                      c("Longitude", "Latitude", "postalCode", 
                        "State.Abbreviation", "brand", "name"))]
}