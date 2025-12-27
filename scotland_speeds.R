devtools::load_all()
library(sf)
library(dplyr)
library(tmap)


# import crashes and trim to temporal parameters and make spatial
crashes_gb <- get_stats19(year = 2024, type = "collision") |>
  format_sf()

# get local authorities, removing Norther Ireland which is not in stats19
scotland_gpkg <- st_read("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/995533eee7e44848bf4e663498634849/geoPackage?layers=0") |>
  filter(grepl("S", LAD22CD))

# intersect crashes with Scottish geometry
crashes_scot <- crashes_gb[scotland_gpkg,]

# match with TAG costs
crashes_scot_cost <- match_tag(crashes_scot)


# sum up the cost grouped by speed
grouped_by_speed <- crashes_scot_cost |>
  st_set_geometry(NULL) |>
  group_by(speed_limit) |>
  summarise(value_of_prevention = round(sum(cost_per_collision)/1e6))

# create table
gt_1 <- gt(grouped_by_speed)

# save it
gtsave(gt_1, "table1.pdf")

# get link data for Scotland
osm_data = osmactive::get_travel_network(
  place = "Scotland",
  boundary_type = "clipsrc",
  max_file_size = 9e999
)


# translate to driving and cycling
drive_net = osmactive::get_driving_network(osm_data)

# function for matching roads to collisions
roads_cra_match <- function(osm_network_sf, crash_sf) {
  drive_net <- osmactive::get_driving_network(osm_network_sf)

  crs_osm <- sf::st_crs(drive_net)
  crash_sf <- sf::st_transform(crash_sf, crs_osm)

  crash_sf$osm_id <- drive_net$osm_id[sf::st_nearest_feature(crash_sf, drive_net)]

  return(crash_sf)
}

# match roads to collisions
crashes_osm <- roads_cra_match(osm_network_sf = drive_net,crash_sf = crashes_scot_cost) |>
  group_by(osm_id) |>
  summarise(cost_millions = sum(cost_per_collision)/1e6,
            mean_speed = mean(as.numeric(speed_limit), na.rm =TRUE)) |>
  st_set_geometry(NULL)

# pickout columns from the road network and join with crash data
crashes_osm_sf = drive_net |>
  select(osm_id, name, maxspeed) |>
  left_join(crashes_osm,by = "osm_id") |>
  filter(!is.na(cost_millions)) |>
  arrange(desc(cost_millions)) |>
  slice(1:20) |>
  mutate(ranking =rank(-cost_millions,ties.method = "first"))

# some road names from the top20 were missing, manually assign them
namez <- data.frame(osm_id = c("87175788", "728122119", "4005422", "19754157", "33236999", "145038678", "257829037", "258475223", "407766501", "576061661",
                               "608734045", "4088845"),
                    name = c("A941 Lossiemouth", "A76 - Catrine", "M9 - Plean", "B6357 - Annan", "A87 - Breakish",
                             "A96 - Culloden", "A823 - Auchterarder", "A822 - Monzie", "A96 - Auldearn", "A947 - Newmachar", "B734 - Camregan",
                             "A6088 - Wolfehopelee Forest"))

# filter roads with no name and join with manually assigned names
cra_noosm <- filter(crashes_osm_sf, is.na(name)) |>
  select(-name) |>
  left_join(namez, by = "osm_id")
# filter roads with names
cra_osm <- filter(crashes_osm_sf, !is.na(name))

# join two dataframes and create table
crashes_osm_table <- rbind(cra_noosm,cra_osm) |>
  st_set_geometry(NULL) |>
  mutate(cost_millions = round(cost_millions,1)) |>
  select(name,`speed limit` = mean_speed,`value of prevention (millions)` = cost_millions)

# gt table
gt2 <- gt(crashes_osm_table, caption = "The 20 roads with the highest injury (prevention) cost in Scotland in 2024")

# save as pdf for copying and pasting into linkedin
gtsave(gt2, "table2.pdf")
