getModels <- function (auto_brands) {
  require (dplyr)
  mod <- clean_auto %>%
    filter(brand %in% auto_brands) %>%
    select(model)
  unique(mod)
}

getRadioChoices <- function (variable) {
  x <- unique(clean_auto[, variable], drop = TRUE)
  x <- x[!is.na(x)]
}