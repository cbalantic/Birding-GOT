# Cathleen Balantic
# Birding Game of Thrones
# 03/08/2018

# Gokhan's static map =========================================================
library(Hmisc)
library(rgdal)
library(tmap)

# Map files from: https://www.cartographersguild.com/showthread.php?t=30472
list.files('Westeros_Essos_shp/GoTRelease')
w <- "./Westeros_Essos_shp/GoTRelease"
# Note: Missing Casterly Rock

# Map code from: https://www.gokhanciflikli.com/post/game-of-thrones/

# Read in shapefiles
westeros <- readOGR(dsn = w, layer = "political")
locations <- readOGR(dsn = w, layer = "locations")
continents <- readOGR(dsn = w, layer = "continents")
islands <- readOGR(dsn = w, layer = "islands")
lakes <- readOGR(dsn = w, layer = "lakes")
landscape <- readOGR(dsn = w, layer = "landscape")
official <- readOGR(dsn = w, layer = "officialMapAreas")
regions <- readOGR(dsn = w, layer = "regions")
rivers <- readOGR(dsn = w, layer = "rivers")
roads <- readOGR(dsn = w, layer = "roads")
wall <- readOGR(dsn = w, layer = "wall")


# add in casterly rock (directly north of Lannisport)
locations@data$id <- as.character(locations@data$id)
casterly <- data.frame(id = 248, name = 'Casterly Rock', 
                       size = '3', confirmed = 1, type = 'Castle',
                       stringsAsFactors = FALSE)
locations@data <- rbind(locations@data, casterly)
locations@coords <- rbind(locations@coords, c(7.34, 5.4)) # eyeball casterly rock's coordinates




# Identify capitals
places <- as.character(locations@data$name)
places <- gsub(" ", "_", places)
capitals <- c("Winterfell", "The Eyrie", "Harrenhal", "Sunspear",
              "King's Landing", "Castle Black", "Pyke",
              "Casterly Rock", "Storm's End", "Highgarden", 
              'Volantis', 'Pentos', 'Quarth', 'Astapor', 'Yunkai', 'Meereen')

holds <- locations[locations@data$name %in% capitals, ]

# Castle icon
castle <- tmap_icons(file = "https://image.ibb.co/kykHfR/castle.png", keep.asp = TRUE)



# Keep only important locations from the show
show.location.names <- c('Astapor', 'Baelish Keep', 'Braavos', 'Casterly Rock',
                         'Castle Black', "Craster's Keep", 'Crossroads Inn',
                         'Deep Lake', 'Deepwood Motte', 'Dragonstone', 'Dusekndale', 
                         'Eastwatch-by-the-Sea', 'Fist of the First Men', 'Greyguard', 
                         'Greywater Watch', 'Gulltown', 'Harrenhal', 'Highgarden', 
                         'Inn (They Ley with Lions)', 'Inn of the Kneeling Man', 'Karhold', 
                         "King's Landing", 'Lannisport', 'Last Hearth', 'Lhazarene Village', 
                         "Lord Harroway's Town", 'Maidenpool', 'Meereen', 'Moat Cailin', 
                         "Mole's Town", "Mummer's Ford", 'Nightfort', 'Old Stone Bridge Inn', 
                         'Oldtown', 'Pentos', 'Planky Town', 'Pyke', 'Qarth', 'Qohor', 
                         'Queenscrown', 'Riverrun', 'Saltpans', 'Shadow Tower', 'Stokeworth',
                         "Storm's End", 'Sunspear', 'The Bloody Gate', 'The Dreadfort', 
                         'The Eyrie', 'The Sorrows (Chroyane)', 'The Twins', 'Tower', 
                         'Tower of Joy', 'Unnamed Village', 'Vaes Dothrak', 'Valyria', 
                         'Village', 'Volantis', 'Volon Therys', 'White Harbor',
                         'Winterfell', 'Yunkai')
show.locations <- locations[locations@data$name %in% show.location.names, ]
show.locations@data$name <- droplevels(show.locations@data$name)

# Keep only important regions from the show
show.region.names <- c('The Neck', 'Whispering Wood', 'The Skirling Pass', 'The Frozen Shore',
                       'The Narrow Sea', 'Bay of Seals', 'The Dothraki Sea', 'Lhazar',
                       'The Red Waste', 'The Gulf of Grief', 'The Smoking Sea', 
                       'The Vale of Arryn')
show.regions <- regions[regions@data$name %in% show.region.names, ]

# Keep only important roads
show.roads <- roads[!(is.na(roads@data$name)), ]

# Keep only important rivers
show.rivers <- rivers[!(is.na(rivers@data$name)), ]

# Keep only important lakes
show.lakes <- lakes[!(is.na(lakes@data$name)), ]


windowsFonts(Times = windowsFont('Times New Roman'))
windowsFonts(GOT = windowsFont('Game of Thrones'))

m <- qtm(continents) +
  
  # Add lake data
  qtm(shp = show.lakes, fill = 'lightblue') +
  
  # Add river data
  qtm(shp = show.rivers, lines.col = 'lightblue', lines.lwd = 0.25) +
  
  # Add road data
  qtm(shp = show.roads, lines.col = 'black', lines.lwd = 0.25, lines.lty = 2) +
  
  # Add Westeros
  tm_shape(shp = westeros) +
  tm_text("name", fontfamily = "Times", size = 1, alpha = .6)  +
  
  # Add region data
  tm_shape(shp = show.regions) +
  tm_text("name", fontfamily = "Times", size = 1, alpha = .6)  +
  
  # Add islands data
  qtm(shp = islands) +
  
  # Plot location names and put a dot above them
  tm_shape(show.locations) +
  #  tm_text("name", size = 0.8, fontfamily = "Times", just = "top") +
  tm_dots("name", size = 0.1, shape = 20, col = "black", ymod = 0.1) +
  
  # Plot capitals and add custom shape
  tm_shape(holds) +
  tm_text("name", size = .25, fontfamily = "Times") +
  tm_dots("name", size = .05, alpha = .5, shape = castle, border.lwd = NA, ymod = .3) + 
  
  # Fluff
  tm_compass(type = "8star", position = c("right", "top"), size = 1.5) +
  tm_layout(bg.color = "lightblue", main.title = "Game of Thrones", frame.lwd = 2,
            fontfamily = "Times") +
  tm_legend(show = FALSE)
m

#Code for hi-res version
#save_tmap(m, "westeros_hires.png", dpi = 300, asp = 0, height = 30, scale = 3)



# Leaflet Zoom Map ====================

library(leaflet)
library(dplyr)
library(rgdal)
library(data.table)

# Map files from: https://www.cartographersguild.com/showthread.php?t=30472
list.files('Westeros_Essos_shp/GoTRelease')
w <- './Westeros_Essos_shp/GoTRelease'

# Read in shapefiles
continents <- readOGR(dsn = w, layer = "continents")
westeros <- readOGR(dsn = w, layer = "political")
islands <- readOGR(dsn = w, layer = "islands")
lakes <- readOGR(dsn = w, layer = "lakes")
rivers <- readOGR(dsn = w, layer = "rivers")
roads <- readOGR(dsn = w, layer = "roads")
wall <- readOGR(dsn = w, layer = "wall")
locations <- readOGR(dsn = w, layer = "locations")

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

# Fix errors
locations@data[locations@data$name %in% 'Braavos', 'type'] <- 'City'

# Keep only important locations from the show
show.location.names <- c('Astapor', 'Baelish Keep', 'Braavos', 'Casterly Rock',
                         'Castle Black', "Craster's Keep", 'Crossroads Inn',
                         'Deep Lake', 'Deepwood Motte', 'Dragonstone', 'Dusekndale', 
                         'Eastwatch-by-the-Sea', 'Fist of the First Men', 'Greyguard', 
                         'Greywater Watch', 'Gulltown', 'Harrenhal', 'Highgarden', 
                         'Inn (They Ley with Lions)', 'Inn of the Kneeling Man', 'Karhold', 
                         "King's Landing", 'Lannisport', 'Last Hearth', 'Lhazarene Village', 
                         "Lord Harroway's Town", 'Maidenpool', 'Meereen', 'Moat Cailin', 
                         "Mole's Town", "Mummer's Ford", 'Nightfort', 'Old Stone Bridge Inn', 
                         'Oldtown', 'Pentos', 'Planky Town', 'Pyke', 'Qarth', 
                         'Queenscrown', 'Riverrun', 'Saltpans', 'Shadow Tower', 'Stokeworth',
                         "Storm's End", 'Sunspear', 'The Bloody Gate', 'The Dreadfort', 
                         'The Eyrie', 'The Sorrows (Chroyane)', 'The Red Keep', 
                         'The Twins', 'Tower', 'Tower of Joy', 'Unnamed Village', 
                         'Vaes Dothrak', 'Valyria', 'Village', 'Volantis', 'Volon Therys', 
                         'White Harbor', 'Winterfell', 'Yunkai')
show.locations <- locations[locations@data$name %in% show.location.names, ]
show.locations@data$name <- droplevels(show.locations@data$name)
round.coords <- as.data.frame(round(show.locations@coords,2))
display.coords <- do.call(paste, round.coords[c('coords.x1', 'coords.x2')])


# "database" to display species observations (I don't think this would ever get that big)
# You can observe at the level of the: 
#     location, landscape, region, island, political area, continent
# how to get these to spatially "drill up" (if using a more specific region?)
# or maybe I am making that too complicated too soon

# Make a data.frame of fake bird observations
# Limiting this to locations for now.
N <- 100
bird.obs <- data.table(obs.id = 1:N, 
                       species = sample(x = c('Northern Cardinal', 'Eastern Screen Owl',
                                              'House Wren', 'Mourning Warbler',
                                              'Praire Warbler','Common Loon', 
                                              'Gull Species', 'Kookaburra',
                                              'Rock Pigeon', 'White-winged Dove',
                                              'Common Loon', 'American Robin'),
                                        size = N, replace = TRUE),
                       name = sample(show.locations@data$name, size = N, replace = TRUE),
                       season = sample(x = 1:7, size = N, replace = TRUE),
                       episode = sample(x = 1:10, size = N, replace = TRUE), 
                       stringsAsFactors = FALSE)
setkey(bird.obs, name, species, season, episode)

# Add bird species to locations table
locations.birds <- show.locations
locations.birds@data <- data.table(locations.birds@data)
locations.birds@data$id <- as.numeric(locations.birds@data$id)
obs.list <- bird.obs[,c('species', 'name')] %>% 
  split(by = 'name') %>% 
  lapply(., '[[', 'species') %>%
  lapply(., unique)
obs.list <- lapply(obs.list, function(x) paste(x, sep = ' ', collapse = ', ')) %>% 
  lapply(., unique)
dt <- data.table(name = names(obs.list),
                 species.list = obs.list)
locations.birds@data <- merge(x = locations.birds@data, y = dt, by = 'name', all.x = TRUE)
locations.birds@data[species.list == '', 
                     species.list := c('No species observations for this location yet!')]
setkey(locations.birds@data, id)

# start with continents and show.locations
linestroke <- FALSE # turn roads/rivers on or off
leaflet() %>% 
  addPolygons(data = continents, color = 'gray50', stroke = FALSE,
              label = ~as.character(name)) %>%
  addPolygons(data = westeros, 
              color = 'gray50', stroke = FALSE,
              fillOpacity = 0, 
              popup = ~as.character(name), 
              label = ~as.character(name)) %>%
  addPolygons(data = regions, stroke = FALSE, fillOpacity = 0, 
              popup = ~as.character(name), 
              label = ~as.character(name)) %>%
  addPolygons(data = islands, color = 'gray50', stroke = FALSE,
              popup = ~as.character(name), label = ~as.character(name)) %>%
  addPolylines(data = wall, color = 'black', stroke = linestroke,
               popup = ~as.character(name), label = ~as.character(name)) %>%
  addPolylines(data = rivers, color = 'blue', stroke = linestroke,
               popup = ~as.character(name), label = ~as.character(name)) %>%
  addPolylines(data = roads, color = 'black', stroke = linestroke,
               popup = ~as.character(name), label = ~as.character(name)) %>%
  
  addCircleMarkers(data = locations.birds, stroke = FALSE,
                   popup = ~as.character(species.list),
                   radius = ~ifelse(type == 'City', 7,
                                    ifelse(type == 'Castle',
                                           5, 3)),
                   color = ~ifelse(type == 'City', 'red',
                                   ifelse(type == 'Castle', 'blue', 'black')),
                   fillOpacity = 0.5, 
                   label = ~as.character(name))


# bird.obs[bird.obs$name == this.location, c('species', 'season', 'episode')]
# 
# popup = paste('<strong>', 'Species:        Season:        Episode:', '</strong>', '<br>',
#               bird.obs$species[1],'    ', bird.obs$season[1], '    ', bird.obs$episode[1], '<br>',
#               bird.obs$species[2],'    ', bird.obs$season[2], '    ', bird.obs$episode[2], '<br>',
#               bird.obs$species[3])

# https://stackoverflow.com/questions/31562383/using-leaflet-library-to-output-multiple-popup-values

