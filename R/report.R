library(sf)
library(mapview)
devtools::load_all("../stats19_match/")
library(geographr)
library(reshape2)
library(stringr)
library(ggplot2)
library(readODS)
library(cols4all)
library(gt)
library(waffle)
library(stringr)
library(osmactive)
library(osmdata)
library(clock)
library(tmap)
library(tmap.networks)
library(basemaps)
library(eurostat)
library(raster)
library(waffle)
library(geographr)
library(openxlsx)
library(terra)
library(tidyr)
library(openair)
library(dplyr)

source("R/get.R")
source("R/plots.R")
source("R/summary.R")
source("R/tabulate.R")
source("R/utils.R")
source("R/match.R")
source("R/map.R")

select = dplyr::select

# define LA name
la_name = authority

# main output directory
output_dir = paste0("outputs/",gsub(" ","_", la_name))

# create folders for outputs
dir.create(paste0(output_dir, "/data/"),recursive = TRUE)
dir.create(paste0(output_dir,"/plots/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/lsoa/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/msoa/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/osm_links/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/streets/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/costs/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/conditions/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/demog/"),recursive = TRUE)
dir.create(paste0(output_dir, "/plots/imd/"),recursive = TRUE)
dir.create(paste0(output_dir, "/tables/"),recursive = TRUE)
dir.create(paste0(output_dir, "/maps/"),recursive = TRUE)



# define the year range of analysis
base_year <- 2020
upper_year <- 2024

LAs = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/LA_boundaries/LA.gpkg")

n_local_authorities = NROW(LAs)

## use import function
# city_shp = get_la_boundaries(city_name = la_name, source = "ons") |> 
#   st_transform(4326)

# filter city of interest
city_shp = filter(LAs, grepl(la_name, LAD22NM)) |> 
  st_transform(4326)

print(paste0("gathering data for the user defined local authority of ",la_name, " matched with ", city_shp$LAD22NM))

# convert to metres
city_shp_m = st_transform(city_shp, 27700)

# import crashes and trim to temporal parameters and make spatial
crashes_gb <- get_stats19("5 years", type = "collision") |> 
  filter(collision_year >= base_year & collision_year <= upper_year) |> 
  format_sf()

# import vehicles and use c index from crashes
vehicles_gb <- get_stats19("5 years", type = "vehicle")|> 
  mutate(vehicle_type = if_else(escooter_flag == "Vehicle was an e-scooter", "e-scooter", vehicle_type)) # add in escooters

# e scooter collisions from vehicle description
e_scooter_collisions <- filter(vehicles_gb, vehicle_type == "e-scooter")

# for age breaks
dft_breaks = c(0, 11, 15, 19, 24, 29, 39, 49, 59, 69, 100)
dft_labels = c("0-11", "12-15", "16-19", "20-24", "25-29",
               "30-39", "40-49", "50-59", "60-69", "70+")

# import casualties, add fatal column to match serious and slight and include e-scooters from vehicle data, add in dft age bands (different to those included in data)
casualties_gb <- get_stats19("5 years", type = "casualty")|> 
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>  # there is a column for serious and slight, so add one for fatal to make analysis consistent
  mutate(
    casualty_type = ifelse(
      collision_index %in% e_scooter_collisions$collision_index & # add e_scooters
        casualty_type == "Data missing or out of range",
      "E-scooter rider",
      casualty_type
    )) |> 
  dplyr::mutate(dft_age_band = cut(as.numeric(age_of_casualty),
                                   breaks = dft_breaks, labels = dft_labels))


# trim crashes to city boundary
crashes <- crashes_gb[city_shp_m,]

# use collision indexes to filter casualties
casualties <- casualties_gb |> 
  filter(collision_index %in% crashes$collision_index) 

# use collision indexes to filter vehicles
vehicles = vehicles_gb |> 
  filter(collision_index %in% crashes$collision_index) 

# rank casualties by LA for ALL
LA_casualties <- summarise_casualties_per_la(casualties = casualties_gb,la_geo = LAs, per_capita = TRUE, crashes = crashes_gb,casualty_types = "All")

# rank casualties by cycling
LA_casualties_cycling <- summarise_casualties_per_la(casualties = casualties_gb,la_geo = LAs,per_capita = TRUE, crashes = crashes_gb,casualty_types = "Cyclist")

# rank casualties by pedestrians
LA_casualties_pedestrian <- summarise_casualties_per_la(casualties = casualties_gb,la_geo = LAs,per_capita = TRUE, crashes = crashes_gb,casualty_types = "Pedestrian")

# group casualties by IMD class
casualties_imd <- casualties |> 
  #filter(casualty_type == "Pedestrian") |>
  filter(collision_year >= "2020") |> 
  group_by(casualty_imd_decile) |> 
  summarise(all =n()) |> 
  mutate(pc = round(all/sum(all)*100,1))

# summarise by simply most or least
casualties_imd_more_less <- casualties_imd |> 
  mutate(ML = str_sub(casualty_imd_decile, 1,1)) |> 
  group_by(ML) |> 
  summarise(pc = sum(pc))

# IMD data from github
#IMD_2025 = st_read("https://github.com/BlaiseKelly/IMD/releases/download/LSOA_IMD2025/LSOA_IMD2025_WGS84_-4854136717238973930.gpkg")

# import Index of deprivation which also inclues high res LSOA shapes
IMD_2025 = st_read("../IMD/dat/LSOA_IMD2025_WGS84_-4854136717238973930.gpkg")

# get lsoa boundaries
# lsoa_boundaries_21 = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/boundaries-v1.0/lsoa_boundaries.gpkg") |> 
#   st_transform(4326)

lsoa_boundaries_21 = dplyr::select(IMD_2025,LSOA21CD,LSOA21NM,geom = SHAPE) |> 
  st_transform(4326)

st_geometry(lsoa_boundaries_21) = lsoa_boundaries_21$geom

# find centre points to neatly intersect with city shape
lsoa_centroids = st_centroid(lsoa_boundaries_21)

# get lsoas within the city
city_lsoa = lsoa_centroids[city_shp,]

# ONS population
# gb_pop <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
#                     sheet = "Mid-2024 LSOA 2021",startRow = 4)
# city_pop_lsoa <- gb_pop |> 
#   dplyr::select(LSOA.2021.Code, Total) |> 
#   filter(LSOA.2021.Code %in% city_lsoa$LSOA11CD)

git_pop = read.csv("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/lsoa21_pop_tot_2011_2024.csv")

lsoa_pop = git_pop |> 
  dplyr::select(-X) |> 
  pivot_longer(-c(lsoa21cd,lsoa21nm), names_to = "year", values_to = "population") |> 
  mutate(year = gsub("X", "", year))

# reshape for joining below
city_pop_lsoa = git_pop |> 
  dplyr::select("LSOA.2021.Code" = lsoa21cd,
                "Total" = X2024) |> 
  filter(LSOA.2021.Code %in% city_lsoa$LSOA21CD)

city_pop_lsoa_sf = left_join(city_pop_lsoa,lsoa_boundaries_21, by = c("LSOA.2021.Code" = "LSOA21CD"))

st_geometry(city_pop_lsoa_sf) = city_pop_lsoa_sf$geom

tm_lsoa_pop = map_lsoa_pop(city_sf = city_shp,lsoa_geo = lsoa_boundaries_21,base_year = base_year, end_year = upper_year)
tmap_save(tm_lsoa_pop, paste0(output_dir, "/plots/lsoa/lsoa_pop.png"))

# plot the casualty home lsoa
tm_lsoa_cas = map_lsoa_home(casualty_df = casualties, lsoa_geo = lsoa_boundaries_21,palette = "wes.zissou1", city_shp = city_shp, base_year = base_year, end_year = upper_year,info_position = c(0.08,0.4))
tmap_save(tm_lsoa_cas, paste0(output_dir, "/plots/lsoa/lsoa_casualties.png"))
# plot vehicle and driver home lsoa
tm_lsoa_veh = map_lsoa_home(vehicle_df = vehicles,lsoa_geo = lsoa_boundaries_21, city_shp = city_shp, palette = "wes.zissou1", base_year = 2020, end_year = 2024,info_position = c(0.08,0.4))
tmap_save(tm_lsoa_veh, paste0(output_dir, "/plots/lsoa/lsoa_vehicles.png"))
# plot collision home lsoa
tm_lsoa_cra = map_lsoa_crashes(crashes_df = crashes,lsoa_geo = lsoa_boundaries_21,palette = "wes.zissou1", city_shp = city_shp,info_position = c(0.08,0.4), base_year = 2020, end_year = 2024)
tmap_save(tm_lsoa_cra, paste0(output_dir, "/plots/lsoa/lsoa_crashes.png"))

# combine all and plot
all_lsoa = tmap_arrange(tm_lsoa_pop,tm_lsoa_cra, tm_lsoa_cas, tm_lsoa_veh, ncol = 2, nrow = 2)
tmap_save(all_lsoa, paste0(output_dir, "/plots/lsoa/lsoa_all.png"), width = 8000,height = 8000, dpi = 700)

# pick out LA LSOAs
city_imd = filter(IMD_2025,LSOA21CD %in% city_lsoa$LSOA21CD)

# create df to match imd deciles
decile_match = data.frame(imd_decile = c(casualties_imd$casualty_imd_decile[-1]),
                          IMDDecil = rev(seq(1,10,1)))

# join with IMD and calculate totals and pc breakdown for each decile
city_imd_pop = city_imd |> 
  left_join(city_pop_lsoa, by = c("LSOA21CD" = "LSOA.2021.Code")) |> 
  st_set_geometry(NULL) |> 
  group_by(IMDDecil) |> 
  summarise(pop = sum(Total)) |> 
  mutate(imd_pc = pop/sum(pop)*100) |> 
  left_join(decile_match,by = "IMDDecil")

# summarise into M for more deprived or L for least
city_imd_ML = city_imd_pop |> 
  mutate(ML = str_sub(imd_decile, 1,1)) |> 
  group_by(ML) |> 
  summarise(pc = sum(imd_pc),
            pop = sum(pop))

pop_least_imd = round(city_imd_ML$pc[city_imd_ML$ML == "L"],1)

cas_least_imd = round(casualties_imd_more_less$pc[casualties_imd_more_less$ML == "L"],1)

cas_imd_data = 100-round(casualties_imd_more_less$pc[casualties_imd_more_less$ML == "D"],1)

# what year is Year Before Last Year (YBLY)?
YBLY = upper_year-1

# LA_LY totals
LA_LY = filter(LA_casualties, collision_year == upper_year & LAD22NM == city_shp$LAD22NM)

# LA year before last year (YBLY)
LA_YBLY = filter(LA_casualties, collision_year == upper_year-1 & LAD22NM == city_shp$LAD22NM)

LA_5Y = LA_casualties |> 
  filter(collision_year == base_year & LAD22NM == city_shp$LAD22NM)

# LA_LY totals
LA_LY_CYC = filter(LA_casualties_cycling, collision_year == upper_year & LAD22NM == city_shp$LAD22NM)

# LA year before last year (YBLY)
LA_YBLY_CYC = filter(LA_casualties_cycling, collision_year == upper_year-1 & LAD22NM == city_shp$LAD22NM)

LA_5Y_CYC = LA_casualties_cycling |> 
  filter(collision_year == base_year & LAD22NM == city_shp$LAD22NM)

# LA_LY totals
LA_LY_PED = filter(LA_casualties_pedestrian, collision_year == upper_year & LAD22NM == city_shp$LAD22NM)

# LA year before last year (YBLY)
LA_YBLY_PED = filter(LA_casualties_pedestrian, collision_year == upper_year-1 & LAD22NM == city_shp$LAD22NM)

LA_5Y_PED = LA_casualties_pedestrian |> 
  filter(collision_year == base_year & LAD22NM == city_shp$LAD22NM)

cas_age_sex_group = summarise_casualties_by_demog(casualties)

# get clean shape
city_shp_osm = get_la_boundaries(city_name = la_name, source = "ons") |>
  st_buffer(100) |> 
  st_buffer(-100) |> 
  st_transform(4326)

area_bb <- st_bbox(city_shp_osm)

##download landuse from osm
x <- opq(bbox = area_bb,timeout = 999) %>%
  add_osm_feature(key = c('highway')) %>%
  osmdata_sf()

osm_data = x$osm_lines |>
  st_intersection(city_shp_osm)

# # get road network for local area
# osm_data = osmactive::get_travel_network(
#   place = city_shp_osm,
#   boundary = city_shp_osm,
#   boundary_type = "clipsrc",timeout = 9999999,
#   max_file_size = 9e999
# )

# translate to drvining and cycling
drive_net = osmactive::get_driving_network(osm_data) |> 
  filter(!service %in% c("alley", "driveway", "parking_aisle", "garages", "drive-through","lay-by", "private","emergency_access", "yard") &
           !access %in% c( "private", "customers","emergency","delivery","permit") &
           !highway == "service") |> 
  mutate(name = ifelse(is.na(name) & highway == "motorway",ref,name))

# # cycling network
# cycle_net <- osmactive::get_cycling_network(osm_data)
# 
# # determine the cycle network and pick out detailed segregation columns
# cycle_net_d = distance_to_road(cycle_net, drive_net)
# # classify segregation
# cycle_net_c = classify_cycle_infrastructure(cycle_net_d) |>
#   dplyr::select(osm_id,detailed_segregation, geometry)
# 
# # generate a map of the LA cycle network
# map_cycle_network(city = la_name,city_shape = city_shp_osm,osm_data = osm_data,title_position = "right",
#                   stats_position = "right",
#                   legend_position = "left",
#                   city_pop = sum(city_pop_lsoa$Total),
#                   plot_dir = paste0(output_dir, "/plots/"))

# calculate the length of road for each speed limit
speed_limit_length = drive_net |> 
  ungroup() |> 
  mutate(maxspeed = gsub("mph", "", gsub(" mph", "", maxspeed)),
         length = as.numeric(st_length(geometry))) |> 
  st_set_geometry(NULL) |> 
  group_by(maxspeed) |> 
  summarise(tot_length = round(sum(length)/1000,1)) |> 
  mutate(pc_length = round(tot_length/sum(tot_length)*100,1))

# summarise casualties to one row per crash
cas_sum = summarise_casualties_per_collision(casualties)

# summarise by speed limit and join with osm data to calculate stats per length
csl = inner_join(crashes,cas_sum) |> 
  st_set_geometry(NULL) |> 
  group_by(speed_limit) |> 
  summarise(collisions = n(),
            fatal = sum(Fatal),
            serious = round(sum(Serious)),
            ksi = round(sum(Fatal,Serious)),
            slight = round(sum(Slight))) |> 
  left_join(speed_limit_length, by = c("speed_limit" = "maxspeed")) |> 
  mutate(coll_km = round(collisions/tot_length,2),
         fatal_km = round(fatal/tot_length,2),
         serious_km = round(serious/tot_length,2),
         ksi_km = round(ksi/tot_length,2),
         slight_km = round(slight/tot_length,2),
         fatal_col = round(fatal/(collisions),3),
         serious_col = round(serious/(collisions),3),
         ksi_col = round(ksi/(collisions),3),
         slight_col = round(slight/(collisions),3))

colour_by = c("Day","Month","Year", "Hour", "Sex of casualty",
              "Age group","Casualty IMD", "Speed limit")

colour_by = c("Speed limit")

for (c in colour_by){
  
  tm_cas = map_casualties_interactive2(crashes = crashes, casualties = casualties, colour_by = c, extent_geo = city_shp)
  
  tmap_save(tm_cas, filename = paste0(output_dir, "/maps/cas_location_",c,".html"), selfcontained = TRUE)
  
}

colour_by = c("Sex of casualty","Age group","Fatal","KSI","Serious","Slight","Total", "Speed limit")

colour_by = c("Total")

for (c in colour_by){
  
  groupz = c("casualty_type", "year")
  
  for (g in groupz){
    
    tm_rds = map_osm_roads_interactive(crashes = crashes, casualties = casualties,group = g,
                                       colour_by = c,area_name = NULL)
    
    tmap_save(tm_rds, filename = paste0(output_dir, "/maps/osm_",g,"_",c,".html"), selfcontained = TRUE)
  }
  
}


# all options
colour_by = c("number_of_collisions", 
              "age_of_casualty", "sex_of_casualty", "speed_limit",
              "fatal", "serious", "ksi", "slight", "total")

# for report
colour_by = c("number_of_collisions", "ksi")

# all options
casualty_types = c("Cyclist","Motorcyclist","Pedestrian","Car occupant","Goods vehicle occupant","E-scooter rider",
                   "Taxi occupant","Mobility scooter rider","Bus occupant","Agricultural vehicle occupant") 

# for report
casualty_types = c("Cyclist","Pedestrian")

dir.create("osm_links")
c = "age_of_casualty"
errors = list()
for(c in colour_by){
  tryCatch({
    tm_stat = map_osm_roads_static(crashes = crashes, casualties = casualties,osm_data = drive_net,casualty_type = casualty_types,year = NULL,group = "casualty_type", 
                                   colour_by = c, city_shape = city_shp_osm,basemap_bgd_colour = "dark",legend_pos = c(0.03,0.48),area_name = la_name)
    
    tmap_save(tm_stat, paste0(output_dir, "/plots/osm_links/osm_all_cas_",c,".png"))
    
    for (cas_type in casualty_types){
      tryCatch({
        cas_nam = tolower(gsub(" ", "_", cas_type))
        
        tm_stat = map_osm_roads_static(crashes = crashes, casualties = casualties,osm_data = drive_net,casualty_type = cas_type,year = NULL,group = "casualty_type",
                                       colour_by = c, city_shape = city_shp_osm,basemap_bgd_colour = "dark",legend_pos = c(0.03,0.48),area_name = la_name)
        
        tmap_save(tm_stat, paste0(output_dir, "/plots/osm_links/osm_",cas_nam,"_",c,".png"))
      }, error = function(e) {
        errors[[c]] <<- e$message  # <<- to assign to outer scope
      })
    }
    
    for (y in seq(base_year,upper_year)){
      tryCatch({
        tm_stat = map_osm_roads_static(crashes = crashes, casualties = casualties,osm_data = drive_net,casualty_type = NULL,year = y,group = "casualty_type", 
                                       colour_by = c, city_shape = city_shp_osm,basemap_bgd_colour = "dark",legend_pos = c(0.03,0.48),area_name = la_name)
        
        tmap_save(tm_stat, paste0(output_dir, "/plots/osm_links/osm_",y,"_",c,".png"))
      }, error = function(e) {
        errors[[c]] <<- e$message  # <<- to assign to outer scope
      })
    }
  }, error = function(e) {
    errors[[c]] <<- e$message  # <<- to assign to outer scope
  })
}

# generate summary of casualties per road link for the period
cas_osm_period = summarise_osm_link_casualties(crashes = crashes,casualties = casualties,ranking = TRUE,group = "total", osm_data = drive_net) 

# same but dissagragated for each year
cas_osm_year = summarise_osm_link_casualties(crashes = crashes,casualties = casualties,ranking = TRUE,group = "year", osm_data = drive_net) 

# generate summary dissagragated by casualty type
cas_osm_type = summarise_osm_link_casualties(crashes = crashes,casualties = casualties,ranking = TRUE,group = "casualty_type", osm_data = drive_net) 

# plot top 10 KSI streets for entire period for all casualties
cas_osm_plot = cas_osm_period |> 
  arrange(desc(number_of_collisions)) |> 
  slice(1:10)

# loop through each street
for (o in cas_osm_plot$osm_id){
  
  street2analyse = drive_net |> filter(osm_id == o)
  
  map_osm_street_casualties(osm_links = street2analyse,
                            casualties = casualties,
                            crashes = crashes,
                            year_from = base_year,
                            year_to = upper_year,
                            bgd_map_buff = "street",
                            casualties_buffer = 10,
                            plot_buffer = 20,
                            plot_dir = paste0(output_dir, "/plots/streets/"))
  
  map_osm_street_vehicles(osm_links = street2analyse,
                          vehicles = vehicles,
                          casualties = casualties,
                          crashes = crashes,
                          casualty_types = "All",
                          year_from = base_year,
                          year_to = upper_year,
                          bgd_map_buff = "street",
                          casualties_buffer = 10,
                          plot_buffer = 20,
                          plot_dir = paste0(output_dir, "/plots/streets/"))
  
}

msoa_imd = match_msoa_imd(IMD_lsoa_data = IMD_2025)

# simplify the casualty types
casualties_simp = summarise_casualty_types(casualties_gb, summary_type = "short_name") |> 
  mutate(casualty_type = short_name)

cas_df_msoa_all = casualties_per_MSOA(casualties = casualties_simp,per_capita = TRUE,by_year = FALSE, by_casualty = FALSE, casualty_sexes = c("Male", "Female")) |> 
  inner_join(msoa_imd)

cas_df_all = cas_df_msoa_all |> 
  st_set_geometry(NULL) |> 
  ungroup() |> 
  dplyr::select(imd_weighted,msoa21hclnm, fatal_pcap,serious_pcap,slight_pcap,ksi_pcap,total_pcap) |> 
  pivot_longer(-c(imd_weighted,msoa21hclnm),names_to = "severity", values_to = "casualties_pcap") |> 
  mutate(severity = gsub("_pcap","",severity))


ew_slopes <- cas_df_all %>%
  group_by(severity) %>%
  summarise(
    r2    = summary(lm(casualties_pcap ~ imd_weighted))$r.squared,
    slope = coef(lm(casualties_pcap ~ imd_weighted))["imd_weighted"],
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("R² = ", round(r2, 2),
                   "  |  slope = +", round(slope, 3))
  ) 

p0 = scatterPlot(cas_df_all,x="imd_weighted",y="casualties_pcap",type = "severity", linear = TRUE,
                 xlab = "IMD score (population-weighted MSOA)", ylab = "Casualty rate (per 1,000 population of MSOA)",
                 main = paste0("Relationship between pcap casualty rate for all casualty types and home MSOA IMD decile for England and Wales"))

filename <- paste0(output_dir, "/plots/msoa/EngWal_all.png")
png(filename, width=2300, height=1500, units="px", res=190)
print(p0)
dev.off()

msoa_nm = read.csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv") |> 
  distinct(msoa21hclnm, localauthorityname)



LA_cas_all = cas_df_all |> 
  left_join(msoa_nm, by = "msoa21hclnm") |> 
  filter(grepl(la_name, localauthorityname))

# generated to export table for report
cas_df_all_LA = filter(cas_df_msoa_all,grepl(la_name, localauthorityname))

la_slopes <- LA_cas_all %>%
  group_by(severity) %>%
  summarise(
    r2 = summary(lm(casualties_pcap ~ imd_weighted))$r.squared,
    slope = coef(lm(casualties_pcap ~ imd_weighted))["imd_weighted"],
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("R² = ", round(r2, 2),
                   "  |  slope = +", round(slope, 3))
  ) 

p1 = scatterPlot(LA_cas_all,x="imd_weighted",y="casualties_pcap",type = "severity", linear = TRUE,
                 xlab = "IMD score (population-weighted MSOA)", ylab = "Casualty rate (per 1,000 population of MSOA)",
                 main = paste0("Relationship between pcap casualty rate for all casualty types and home MSOA IMD for LA: ",LA_cas_all$localauthorityname[1]))

filename <- paste0(output_dir, "/plots/msoa/la_all.png")
png(filename, width=2300, height=1500, units="px", res=190)
print(p1)
dev.off()

# 
cas_df = casualties_per_MSOA(casualties = casualties_simp,per_capita = TRUE,by_year = FALSE, by_casualty = TRUE, casualty_sexes = c("Male", "Female")) 

# pick out these
casualty_types =  c("Pedestrian","Cyclist","Motorcyclist","Car occupant","Goods vehicle occupant",
                    "Mobility scooter rider","Taxi occupant","E-scooter rider")   

msoa_casualties = cas_df |> 
  filter(casualty_type %in% casualty_types) |> 
  inner_join(msoa_imd) |> 
  st_set_geometry(NULL)

rates = gsub("_rank","", unique(names(msoa_casualties)[grepl("_pcap_rank", names(msoa_casualties))]))


slope_list_ew = list()
slope_list_la = list()
for (r in rates){
  
  ew_slopes <- msoa_casualties %>%
    group_by(casualty_type) %>%
    summarise(
      r2    = summary(lm(!!sym(r) ~ imd_weighted))$r.squared,
      slope = coef(lm(!!sym(r) ~ imd_weighted))["imd_weighted"],
      .groups = "drop"
    ) %>%
    mutate(
      label = paste0("R² = ", round(r2, 2),
                     "  |  slope = +", round(slope, 3))
    ) |> 
    mutate(severity = r)
  
  slope_list_ew[[r]] = ew_slopes
  
  p1 = scatterPlot(msoa_casualties,x="imd_weighted",y=r,type = "casualty_type", linear = TRUE,
                   xlab = "IMD score (population-weighted MSOA)", ylab = "Casualty rate (per 1,000 population of MSOA)",
                   main = paste0("Relationship between ",  gsub("_pcap","",r)," casualty rate for most common casualty types and home MSOA IMD decile for England and Wales"))
  
  filename <- paste0(output_dir, "/plots/msoa/EngWal_",r,".png")
  png(filename, width=2300, height=1500, units="px", res=190)
  print(p1)
  dev.off()
  
  LA_cas_home = filter(msoa_casualties,grepl(la_name, localauthorityname))
  
  la_slopes <- LA_cas_home %>%
    group_by(casualty_type) %>%
    summarise(
      r2 = summary(lm(!!sym(r) ~ imd_weighted))$r.squared,
      slope = coef(lm(!!sym(r) ~ imd_weighted))["imd_weighted"],
      .groups = "drop"
    ) %>%
    mutate(
      label = paste0("R² = ", round(r2, 2),
                     "  |  slope = +", round(slope, 3))
    ) |> 
    mutate(severity = r)
  
  slope_list_la[[r]] = la_slopes
  
  p2 = scatterPlot(LA_cas_home,x="imd_weighted",y=r,type = "casualty_type", linear = TRUE,
                   xlab = "IMD score (population-weighted MSOA)", ylab = "Casualty rate (per 1,000 population of MSOA)",
                   main = paste0("Relationship between ",  gsub("_pcap","",r)," casualty rate for most common casualty types and home MSOA IMD for LA: ",LA_cas_home$localauthorityname[1]))
  
  filename <- paste0(output_dir, "/plots/msoa/la_",r,".png")
  png(filename, width=2300, height=1500, units="px", res=190)
  print(p2)
  dev.off()
  
}

slope_dat_ew = do.call(rbind,slope_list_ew)
slope_dat_la = do.call(rbind,slope_list_la)

# sort la chart slopes
lcs = slope_dat_la |> 
  arrange(desc(r2)) |> 
  slice(1:8) |> 
  mutate(cas_sev = paste(casualty_type,severity))

la_top8 = LA_cas_home |> 
  ungroup() |> 
  filter(casualty_type %in% lcs$casualty_type) |> 
  dplyr::select(casualty_type,imd_weighted, lcs$severity) |> 
  pivot_longer(-c(casualty_type,imd_weighted), names_to = "severity", values_to = "casualties") |> 
  mutate(cas_sev = paste(casualty_type,severity)) |> 
  filter(cas_sev %in% lcs$cas_sev) |> 
  mutate(cas_sev = gsub("_pcap","", cas_sev))

p3 = scatterPlot(la_top8,x="imd_weighted",y="casualties",type = c("cas_sev"), linear = TRUE,
                 xlab = "IMD score (population-weighted MSOA)", ylab = "Casualty rate (per 1,000 population of MSOA)",
                 main = paste0("Relationship between casualty rate and home MSOA IMD for the 8 strongest correlation combinations for LA: ",la_name))

filename <- paste0(output_dir, "/plots/msoa/la_8.png")
png(filename, width=2300, height=1500, units="px", res=190)
print(p2)
dev.off()

casualties_la_simp = casualties |> 
  summarise_casualty_types(summary_type = "short_name") |> 
  mutate(casualty_type = short_name)

imd_casualties = casualties_la_simp |> 
  left_join(decile_match, by = c("casualty_imd_decile" = "imd_decile")) |> 
  group_by(casualty_type, dft_age_band,sex_of_casualty,IMDDecil) |> 
  summarise(fatal = sum(fatal_count),
            serious = sum(casualty_adjusted_severity_serious),
            slight = sum(casualty_adjusted_severity_slight)) |> 
  mutate(ksi = fatal+serious,
         total = fatal+serious+slight) |> 
  mutate(date = lubridate::dmy(paste0("01-", IMDDecil,"-2025"),tz = "GMT")) |> 
  filter(!is.na(date))

write.csv(imd_casualties, "imd.csv")

severities = c("fatal","serious", "ksi", "slight", "total")

demographics = c("age", "sex")

for(s in severities){
  
  for(d in demographics){
    
    plot_casualty_type_demographics(casualties = casualties_la_simp,demog_param = d, la_name = la_name, stat2plot = s,plot_dir = paste0(output_dir,"/plots/imd/"))
    
  }
  
}

age_sex = summarise_casualties_by_demog(casualties = casualties)

cas_type <- data.frame(
  casualty_type = c("Car occupant", "Motorcycle 125cc and under rider or passenger",
                    "Cyclist", "Pedestrian", "Motorcycle over 500cc rider or passenger",
                    "Motorcycle over 125cc and up to 500cc rider or  passenger",
                    "Motorcycle 50cc and under rider or passenger",
                    "Bus or coach occupant (17 or more pass seats)",
                    "Taxi/Private hire car occupant",
                    "Van / Goods vehicle (3.5 tonnes mgw or under) occupant",
                    "Other vehicle occupant", "Data missing or out of range",
                    "Motorcycle - unknown cc rider or passenger",
                    "Goods vehicle (7.5 tonnes mgw and over) occupant",
                    "Electric motorcycle rider or passenger",
                    "Minibus (8 - 16 passenger seats) occupant",
                    "Mobility scooter rider", "Horse rider",
                    "Goods vehicle (over 3.5t. and under 7.5t.) occupant",
                    "Goods vehicle (unknown weight) occupant",
                    "Agricultural vehicle occupant", "E-scooter rider"),
  short_name = c("Car occupant", "Motorcyclist", "Cyclist", "Pedestrian", "Motorcyclist",
                 "Motorcyclist", "Motorcyclist", "Bus occupant", "Taxi occupant",
                 "Goods vehicle occupant", "Other vehicle", "Data missing",
                 "Motorcyclist", "Goods vehicle occupant", "Motorcyclist",
                 "Bus occupant", "Mobility scooter rider", "Horse rider",
                 "Goods vehicle occupant", "Goods vehicle occupant",
                 "Agricultural vehicle occupant", "E-scooter rider"))

sing_veh_pave = summarise_casualties_pavements(crashes = crashes, casualties = casualties,vehicles = vehicles, base_year = base_year, upper_year = upper_year)

sing_veh_pave_gb = summarise_casualties_pavements(crashes = crashes_gb, casualties = casualties_simp,vehicles = vehicles_gb, base_year = base_year, upper_year = upper_year)

plot_ksi_pavement(crashes = crashes, casualties = casualties,vehicles = vehicles, base_year = base_year, upper_year = upper_year,plot_rows = 2, 
                  plot_width = 200,plot_height = 100,
                  title = paste0("Pedestrians KSI whilst on a pavement or verge, coloured by driven\nvehicle that collided with them between ",base_year, " and ", upper_year),
                  plot_dir = paste0(output_dir, "/plots/streets/"))

veh_col = summarise_vehicles_per_collision(vehicles)

# cost of each collision based on severity joined with vehicle data
cc = crashes |> 
  st_set_geometry(NULL) |> 
  match_tag(summarise = "severity") |> 
  dplyr::select(collision_index,cost_per_collision) |> 
  inner_join(veh_col)

# pick out collisions that involved a large vehicle
cc_mv = cc |> 
  filter(Car > 0 |  Motorcycle > 0 | Bus > 0 | `Goods vehicle` > 0 | Taxi > 0)

# split cost of collision by speed
cc_spd = crashes |> 
  st_set_geometry(NULL) |> 
  match_tag(summarise = "severity") |> 
  dplyr::select(collision_index,speed_limit,cost_per_collision, cost_per_casualty) |> 
  mutate(total_cost = cost_per_collision,
         cost_per_collision = cost_per_collision-cost_per_casualty) |> 
  group_by(speed_limit) |> 
  summarise(col_cost = sum(cost_per_collision),
            cas_cost = sum(cost_per_casualty),
            total_cost = sum(total_cost),
            ncols = n()) |> 
  mutate(pc_cost = total_cost/sum(total_cost),
         tot_cost_per_col = total_cost/ncols,
         col_cost_per_col = col_cost/ncols,
         cas_cost_per_col = cas_cost/ncols)

plot_summarise_tag_costs_speed(crashes = crashes,agg_level = "severity",plot_param = "total_cost",city = la_name,plot_dir = output_dir)

plot_summarise_tag_costs_speed(crashes = crashes,agg_level = "severity",plot_param = "cost_per_col",city = la_name,plot_dir = output_dir)


# create a gt table of costs per year
tabulate_summarise_tag_costs(crashes, city = la_name, agg_level = "severity",tab_dir = paste0(output_dir, "/tables/"), file_type = ".html")

tag_costs_col = summarise_tag_costs(crashes_df = crashes,agg_level = "severity")

tag_costs_road = summarise_tag_costs(crashes_df = crashes,agg_level = "severity_road") |> 
  pivot_longer(-c(collision_year,collision_severity),names_to = "road_type",values_to = "cost") |> 
  group_by(collision_year) |> 
  summarise(cost = sum(cost,na.rm = TRUE))

# bar chart of the costs
plot_summarise_tag_costs(crashes, agg_level = "severity_road",plot_dir = paste0(output_dir,"/plots/costs/"), city = la_name)

plot_summarise_tag_costs(crashes, agg_level = "severity",plot_dir = paste0(output_dir,"/plots/costs/"), city = la_name)

# bar charts summarising crash conditions
crash_vars <- c("road_surface_conditions", "junction_detail",
                "speed_limit", "light_conditions", "weather_conditions")

# loop through these
for (v in crash_vars){
  
  plot_crash_conditions(crashes = crashes, casualties = casualties, severities = c("Fatal", "Serious","Slight"), city = la_name,
                        plot_width = 10, parameter = v, plot_dir = paste0(output_dir,"/plots/conditions/"))
  
  plot_crash_conditions(crashes = crashes, casualties = casualties, severities = c("Fatal", "Serious"),plot_width = 10, city = la_name,
                        parameter = v,plot_dir = paste0(output_dir,"/plots/conditions/"))
  
  plot_crash_conditions(crashes = crashes, casualties = casualties, severities = c("Fatal"),plot_width = 9, city = la_name,
                        parameter = v,plot_dir = paste0(output_dir,"/plots/conditions/"))
  print(v)
}

plot_la_ranking(crashes = crashes_gb,casualties = casualties_gb,casualty_types = "Pedestrian", LA = la_name, la_geo = LAs, severities = c("Fatal", "KSI", "Total"),
                plot_dir = paste0(output_dir,"/plots/national"), base_year = base_year,end_year = upper_year)

plot_la_ranking(crashes = crashes_gb,casualties = casualties_gb,casualty_types = "Cyclist", LA = la_name,la_geo = LAs, severities = c("Fatal", "KSI", "Total"),
                plot_dir = paste0(output_dir,"/plots/national"), base_year = base_year,end_year = upper_year)

plot_la_ranking(crashes = crashes_gb,casualties = casualties_gb,casualty_types = "All", LA = la_name, la_geo = LAs, severities = c("Fatal", "KSI", "Total"),
                plot_dir = paste0(output_dir,"/plots/national"), base_year = base_year,end_year = upper_year)

cas_types <- c("All", "Cyclist", "Pedestrian","Car occupant")

variablez = c("ksi_cas", "total_cas", "fatal_cas", "serious_cas")

for (cas in cas_types){
  
  for (v in variablez){
    
    v_nam <- gsub("_cas", "", v)
    
    LA_casualties <- summarise_casualties_per_la(casualties = casualties_gb, crashes = crashes_gb,la_geo = LAs,casualty_types = cas)
    
    #LA_casualties_2024 <- filter(LA_casualties,collision_year == "2024")
    
    if(cas == "All"){
      cas_nam = "All casualties"
    } else {
      cas_nam = gsub("_"," ", cas)
    }
    
    tm1 <- map_la_casualties(region_sf = LA_casualties,
                             variable = v,
                             home_LA = la_name,
                             start_year = upper_year,
                             end_year = upper_year,
                             palette = "wes.zissou1",
                             breaks_style = "kmeans",
                             title = paste(v_nam, cas_nam))
    
    tmap_save(tm1, paste0(output_dir,"/plots/national/la_casualties_",cas, "_",v,"_LY.png"), width = 4500, height = 5000, dpi = 650) 
    
    tm2 <- map_la_casualties(region_sf = LA_casualties,
                             variable = v,
                             home_LA = la_name,
                             start_year = base_year,
                             end_year = upper_year,
                             palette = "wes.zissou1",
                             breaks_style = "kmeans",
                             title = paste(v_nam, cas_nam))
    
    tmap_save(tm2, paste0(output_dir,"/plots/national/la_casualties_",cas, "_",v,"_ALL.png"), width = 4500, height = 5000, dpi = 650) 
    
    # tm3 <- map_la_costs(region_sf = LA_casualties,
    #                          variable = v,
    #                     home_LA = la_name,
    #                     start_year = upper_year,
    #                     end_year = upper_year,
    #                          palette = "wes.zissou1",
    #                          breaks_style = "kmeans",
    #                          title = NULL)
    # 
    # cas_nam = gsub(" ","_", cas_nam)
    # 
    # tmap_save(tm3, paste0(output_dir,"/plots/national/la_costs_",cas_nam,"_",v,"_LY.png"), width = 4500, height = 5000, dpi = 650)
    # 
    # tm4 <- map_la_costs(region_sf = LA_casualties,
    #                     variable = v,
    #                     home_LA = la_name,
    #                     start_year = base_year,
    #                     end_year = upper_year,
    #                     palette = "wes.zissou1",
    #                     breaks_style = "kmeans",
    #                     title = NULL)
    # 
    # cas_nam = gsub(" ","_", cas_nam)
    # 
    # tmap_save(tm4, paste0(output_dir,"/plots/national/la_costs_",cas_nam,"_",v,"_ALL.png"), width = 4500, height = 5000, dpi = 650)
    
  }
  
}

plot_casualty_demographics(casualties = casualties,city = la_name,severity = "ksi", plot_dir = paste0(output_dir))

# save parameters for report
save(LAs,la_name, upper_year, base_year, YBLY,pop_least_imd, cas_least_imd, cas_imd_data,cc,cc_mv,cc_spd,tm_cas,
     LA_LY,LA_LY_CYC, LA_LY_PED,LA_YBLY,LA_YBLY_CYC, LA_YBLY_PED,LA_5Y, LA_5Y_CYC, LA_5Y_PED,slope_dat_ew,slope_dat_la,
     csl, cas_df_all_LA, age_sex,cas_type,imd_casualties,sing_veh_pave,sing_veh_pave_gb,tag_costs_col,tag_costs_road,
     cas_osm_period,cas_osm_year, cas_osm_type,  file = paste0(output_dir, "/data/LA_report_dat.Rdata"))


