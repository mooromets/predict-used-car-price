getModels <- function (auto_brands) {
  require (dplyr)
  mod <- clean_auto %>%
    filter(brand %in% auto_brands) %>%
    select(model)
  unique(mod)
}

getRadioChoices <- function (variable) {
  x <- unique(clean_auto[variable])
  colnames(x) <- NULL
  x <- x[complete.cases(x), ]
  c("any", unlist(lapply(x, as.character)))
}

filtered_auto <- function(input) {
  require(dplyr)
  (out <- filter(clean_auto, 
                price >= input$price[1], price <= input$price[2],
                yearOfRegistration >= input$year[1], yearOfRegistration <= input$year[2],
                kilometer >= input$km[1], kilometer <= input$km[2],
                powerPS >= input$hp[1], powerPS <= input$hp[2]))
}

fast_lm <- function(auto_data){
  tmp <- auto_data %>% 
    filter (!model %in% SkipValues$models, !brand %in% SkipValues$brands)
  lm(price ~ vehicleType + yearOfRegistration + gearbox + powerPS + model +
       kilometer + brand + notRepairedDamage,
     data = tmp)
}

brandsAndModelsToSkip <- function (threshold = 10^-10) {
  SkipValues <<- list(models = c(), brands = c())
  
  # Get coefficients
  print(system.time(
    fit0 <- fast_lm(clean_auto) # no optimization
    )) 
  cs <- summary(fit0)$coefficients
  rm (fit0) # free up 550 MB
  sorted <- cs[ order(-abs(cs[, 4])), ] # sort factors by "Pr(>|t|)"
  less <- rownames(sorted[sorted[, 4] > threshold, ])  # take names with big probability
  
  # Brands
  brands <- less[grep("brand", less)]
  brands <- gsub("brand", "", brands) # brand names from fit0
  SkipValues$brands <<- brands
  
  # Models
  models_of_brands <- clean_auto %>% # get all models of these brands
    filter(brand %in% brands) %>%
    select(model)
  models_of_brands <- unique(models_of_brands[[1]])
  models <- less[grep("model", less)] 
  models <- gsub("model", "", models) # model names from fi0
  u_models <- union(models, models_of_brands)
  SkipValues$models <<- models

  print(system.time(
    fast_lm(clean_auto) # with optimization
  ))
  (SkipValues)
}

SkipValues <- list(brands = c("saab", "chevrolet", "peugeot", "mazda", "smart", "mitsubishi",  "suzuki", "daihatsu", "hyundai", "rover", "kia", "daewoo", "dacia", "skoda", "subaru", "lada", "honda", "jeep", "fiat", "trabant", "ford", "seat", "chrysler", "nissan", "lancia"),
                   models = c("c4", "croma", "toledo", "leon", "corolla", "kalos", "matiz", "ypsilon", "note", "c5", "c3", "rav", "500", "roadster", "materia", "c2", "jazz", "sprinter", "serie_2", "jetta", "juke", "laguna", "altea", "nubira", "sirion", "meriva", "kappa", "80", "ibiza", "exeo", "fortwo", "panda", "forfour", "charade", "modus", "omega", "touareg", "megane", "b_max", "elefantino", "justy", "samara", "200", "terios", "musa", "verso", "tigra", "espace", "sharan", "caddy", "v70", "cherokee", "legacy", "antara", "agila", "avensis", "range_rover_evoque", "zafira", "s60", "golf", "c_reihe", "serie_3", "6er", "601", "ka", "forester", "stilo", "cordoba", "scenic", "arosa", "v50", "auris", "vectra", "lupo", "aveo", "range_rover_sport", "fusion", "a2", "impreza", "touran", "lybra", "v40", "galant", "cuore", "scirocco", "sandero", "signum", "fabia", "carnival", "s_klasse", "90", "lanos", "roomster", "bravo", "discovery_sport", "polo", "156", "rx_reihe", "primera", "vito", "combo", "kalina", "move", "tt", "clio", "defender", "alhambra", "tucson", "yaris", "picanto", "i3", "logan", "lodgy", "145", "swift", "clubman", "m_klasse", "m_reihe", "crossfire", "beetle", "captiva", "colt", "accord", "astra", "cooper", "duster", "850", "147", "almera", "rio", "passat", "cc", "jimny", "mondeo", "doblo", "twingo", "159", "b_klasse", "c1", "4_reihe", "9000", "one", "2_reihe", "rangerover", "berlingo", "c_max", "v_klasse", "niva", "citigo", "tiguan", "i_reihe", "cl", "x_trail", "spark", "mii", "fiesta", "bora", "kangoo", "a1", "corsa", "a3", "focus", "getz", "e_klasse", "micra", "discovery", "calibra", "range_rover", "ceed", "carisma", "yeti", "civic", "eos", "r19", "cr_reihe", "insignia", "ptcruiser", "lancer", "punto")
                   )