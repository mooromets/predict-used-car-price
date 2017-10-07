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
  out <- filter(clean_auto, 
                price >= input$price[1], price <= input$price[2],
                yearOfRegistration >= input$year[1], yearOfRegistration <= input$year[2],
                kilometer >= input$km[1], kilometer <= input$km[2],
                powerPS >= input$hp[1], powerPS <= input$hp[2])
  out
}

fast_lm <- function(auto_data){
  skipValues <- c("saab", "kappa", "sprinter", "musa", "fortwo", "serie_2", "eos", "sharan", "croma", "aveo", "rx_reihe", "materia", "caddy", "meriva", "c5", "verso", "altea", "6er", "a2", "peugeot", "omega", "200", "beetle", "slk", "c1", "touran", "charade", "note", "yaris", "forfour", "polo", "golf", "sirion", "leon", "nubira", "toledo", "exeo", "samara", "m_klasse", "corolla", "avensis", "s_klasse", "ibiza", "b_max", "range_rover_sport", "kalos", "terios", "mazda", "c4", "b_klasse", "cl", "chevrolet", "elefantino", "niva", "lybra", "hyundai", "cooper", "80", "roadster", "vito", "lupo", "juke", "jetta", "panda", "modus", "mitsubishi", "serie_3", "scirocco", "legacy", "zafira", "carnival", "jazz", "kalina", "601", "matiz", "ypsilon", "justy", "range_rover_evoque", "vectra", "auris", "dacia", "cherokee", "clubman", "tt", "one", "passat", "90", "espace", "s60", "laguna", "i3", "lada", "v50", "citigo", "a1", "rover", "smart", "v70", "antara", "mii", "signum", "impreza", "rav", "combo", "spark", "bora", "skoda", "discovery_sport", "subaru", "daewoo", "kia", "sandero", "cuore", "c3", "lodgy", "156")
  tmp <- auto_data %>% filter (!model %in% skipValues, !brand %in% skipValues)
  lm(price ~ vehicleType + yearOfRegistration + gearbox + powerPS + model +
       kilometer + brand + notRepairedDamage,
     data = tmp)
}
