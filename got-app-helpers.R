# got-app-helpers.R

# Helper functions for the got-app

# GOT_map_data ==============
GOT_map_data <- function(filepath) {
  
  # Read in shapefiles
  continents <- readOGR(dsn = filepath, layer = "continents")
  westeros <- readOGR(dsn = filepath, layer = "political")
  islands <- readOGR(dsn = filepath, layer = "islands")
  regions <- readOGR(dsn = filepath, layer = "regions")
  locations <- readOGR(dsn = filepath, layer = "locations")
  
  # add in casterly rock and the red keep
  locations@data$id <- as.character(locations@data$id)
  casterly <- data.frame(id = 248, name = 'Casterly Rock', 
                         size = '3', confirmed = 1, type = 'Castle',
                         stringsAsFactors = FALSE)
  locations@data <- rbind(locations@data, casterly)
  locations@coords <- rbind(locations@coords, c(7.29, 5.5)) # eyeball casterly rock's coordinates
  redkeep <- data.frame(id = 249, name = 'The Red Keep', 
                        size = '3', confirmed = 1, type = 'Castle',
                        stringsAsFactors = FALSE)
  locations@data <- rbind(locations@data, redkeep)
  locations@coords <- rbind(locations@coords, c(19.34, 4)) # eyeball coordinates
  
  # Keep only important locations from the show
  show.location.names <- c('Astapor', 'Baelish Keep', 'Braavos', 'Casterly Rock',
                           'Castle Black', "Craster's Keep", 'Deepwood Motte', 
                           'Dragonstone', 'Eastwatch-by-the-Sea', 
                           'Fist of the First Men',  'Harrenhal', 'Highgarden', 'Horn Hill',
                           'Karhold', "King's Landing", 'Lannisport', 'Last Hearth', 
                           'Meereen', 'Moat Cailin', "Mole's Town", 'Nightfort', 
                           'Oldtown', 'Pentos','Qarth', 'Queenscrown', 'Riverrun', 
                           'Shadow Tower', "Storm's End", 'Sunspear', 
                           'The Bloody Gate', 'The Dreadfort', 'The Eyrie', 
                           'The Red Keep', 'The Twins', 'Tower of Joy', 'Vaes Dothrak',
                           'Valyria','Volantis','White Harbor', 'Winterfell', 'Yunkai')
  show.locations <- locations[locations@data$name %in% show.location.names, ]
  show.locations@data$name <- droplevels(show.locations@data$name)
  
  # Fix errors
  show.locations@data[show.locations@data$name %in% 'Braavos', 'type'] <- 'City'
  
  map_data <- list(locations = show.locations, 
                   westeros = westeros,
                   regions = regions, 
                   islands = islands,
                   continents = continents)
}



# GOT_bird_data ==============
GOT_bird_data <- function(map_data, sheet_key){

  bird.obs <- gs_key(x = sheet_key, verbose = FALSE) %>% 
    gs_read_csv(verbose = FALSE) %>%
    data.table()
  
  setkey(bird.obs, your_name, species_name, location, season, episode, time)

  # Add bird species to appropriate layer
  all.bird.data <- list(locations = list(), 
                        regions = list(), 
                        islands = list(),
                        westeros = list())
  bird.obs[,name := location]

  for (i in 1:length(all.bird.data)){
    list.layer <- names(all.bird.data)[i]
    all.bird.data[[list.layer]] <- map_data[[list.layer]]
    obs.list <- bird.obs[,c('species_name', 'name')] %>% 
      split(by = 'name') %>% 
      lapply(., '[[', 'species_name') %>%
      lapply(., unique)
    obs.list <- lapply(obs.list, function(x) paste(x, sep = ' ', collapse = ', ')) %>% 
      lapply(., unique)
    dt <- data.table(name = names(obs.list),
                     species.list = obs.list)
    all.bird.data[[list.layer]] <- merge(x = all.bird.data[[list.layer]], 
                                              y = dt, by = 'name', all.x = TRUE)
    all.bird.data[[list.layer]]@data[is.na(all.bird.data[[list.layer]]@data$species.list),
                                     'species.list'] <- 'No species observations for this location yet!'
  }
  return(list(joined.data = all.bird.data, google.form.data = bird.obs))
}

# GOT_map ==============
GOT_map <- function(map_data, bird_data){
  leaflet() %>% 
    addPolygons(data = map_data$continents, color = 'gray40', stroke = FALSE,
                label = ~as.character(name)) %>%
    addPolygons(data = bird_data$westeros, 
                color = 'gray40', stroke = FALSE,
                fillOpacity = 0, 
                popup = ~as.character(species.list), 
                label = ~as.character(name)) %>%
    addPolygons(data = bird_data$regions, stroke = FALSE, fillOpacity = 0, 
                popup = ~as.character(species.list), 
                label = ~as.character(name)) %>%
    addPolygons(data = bird_data$islands, color = 'gray40', stroke = FALSE,
                popup = ~as.character(species.list), 
                label = ~as.character(name)) %>%
    addCircleMarkers(data = bird_data$locations, stroke = FALSE,
                     popup = ~as.character(species.list),
                     radius = 5,
                     color = 'gray22',
                     fillOpacity = 0.75, 
                     label = ~as.character(name)) %>% 
    
    # Centered on Pentos
    setView(lng = 40, lat = 4, zoom = 3.5)
  
}

# Read in data to use in map
map_data <- GOT_map_data(filepath = './data')



