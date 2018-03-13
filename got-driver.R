# Cathleen Balantic
# Birding Game of Thrones
# 03/08/2018

library(googlesheets)
library(data.table)
dt <- data.table(gs_ls())

# Grab sheet key to avoid needing OAuth access:
GOT.sheet.key <- dt[sheet_title == 'GOT_birding_by_ear', sheet_key]
gs_key(x = GOT.sheet.key)

# READ IN MAP DATA:
w <- './Westeros_Essos_shp/GoTRelease'
map.data <- GOT_map_data(filepath = w)

# MERGE BIRD OBSERVATIONS from Google form into spatial data:
GOT.sheet.key <- '1B5_IP0N8AgQ8GscEWm9X7vq_GO7j7N6YQgBJ-D8wPZo'
bird.data <- GOT_bird_data(map_data = map.data, sheet_key = GOT.sheet.key)

# DRAW THE MAP: 
GOT_map(map_data = map.data, bird_data = bird.data)
