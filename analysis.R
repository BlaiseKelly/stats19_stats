# this script downloads the STATS19 data from the DfT and creates some tables and plots

library(sf)
library(mapview)
library(dplyr)
library(stats19)
library(geographr)
library(reshape2)
library(ggplot2)
library(readODS)
library(cols4all)
library(gt)
library(waffle)
library(stringr)
library(osmactive)
library(clock)
library(tmap)
library(basemaps)
library(mapview)
library(h3jsr)

# create directory for the plots
dir.create("plots/")
dir.create("data/")

crashes <- get_stats19("2004", type = "collision")

casualties <- get_stats19("2004", type = "casualty")


lat <- 51.455317
lon <- -2.588910



domain <- st_point(c(lon, lat)) |> 
  st_sfc(crs = 4326) |> 
  st_buffer(6000) |> 
  st_as_sf()

mapview(domain)

# create h3
hex_ids <- h3jsr::polygon_to_cells(domain, res = 8)

# Convert H3 indexes back to sf polygons
hex_sf <- st_as_sf(h3jsr::cell_to_polygon(hex_ids))

# assign corret index
hex_sf$h3_index <- unlist(hex_ids)


crashes_sf <- stats19::format_sf(crashes) |> 
  st_transform(4326)



crashes_in <- crashes_sf[hex_sf,]

cas_in <- casualties |> 
  filter(collision_index %in% crashes_in$collision_index) |> 
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>
  select(collision_index, collision_year, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  group_by(collision_index, collision_year) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))

cra_cas <- left_join(crashes_in, cas_in, by = "collision_index")

cra_cas <- filter(cra_cas,collision_year.x >= 2020)

hex_crash <- st_join(hex_sf,cra_cas)

hc <- hex_crash |> 
  group_by(h3_index) |> 
  summarise(KSI = sum(Fatal+Serious)) |> 
  arrange(desc(KSI))



mapview(hc['KSI'])

bm <- basemaps::basemap_raster(ext=hex_sf, map_service = "carto", map_type = "light")

pal <- cols4all::c4a("tol.rainbow_pu_br")

# plot the boundary
tm1 <- tm_shape(bm)+
  tm_rgb()+
  tm_shape(hc)+
  tm_polygons(fill = "KSI", fill.scale = tm_scale_intervals(values = pal), fill_alpha = 0.4)+
  tm_options(frame = FALSE)+
  tm_legend(frame = FALSE)

dir.create("plots")

tmap_save(tm1, "plots/H3.png")

worst <- hex_crash |> 
  filter(h3_index %in% hc[1,]$h3_index)

worst_casualties <- casualties |> 
  filter(collision_index %in% worst$collision_index) |> 
  left_join(crashes_in, by = "collision_index") |> 
  select(casualty_type, casualty_severity, Serious = collision_adjusted_severity_serious, geometry) |> 
  filter(!casualty_severity == "Slight") |> 
  mutate(New_Serious = if_else(casualty_severity == "Fatal", 1.5, Serious))

st_geometry(worst_casualties) <- worst_casualties$geometry

bm2 <- basemaps::basemap_raster(ext=worst, map_service = "carto", map_type = "light")

#mapview(worst_casualties)
tm2 <- tm_shape(bm2)+
  tm_rgb()+
  tm_shape(worst)+
  tm_polygons(col = "black", fill_alpha = 0)+
  tm_shape(worst_casualties)+
  tm_bubbles(fill = "casualty_type",
             shape = "casualty_type",
             #size = "New_Serious",
             shape.legend = tm_legend_combine("fill")) +
  tm_title("H3 grid with highest KSI between: 2020 and 2024")+
  tm_options(frame = FALSE)+
  tm_legend(frame = FALSE)

tmap_save(tm2, "plots/worst_cell.png", width = 3200, height = 2800)

#mapview(worst_casualties)
tm2 <- tm_shape(bm2)+
  tm_rgb()+
  tm_shape(worst)+
  tm_polygons(col = "black", fill_alpha = 0)+
  tm_shape(worst_casualties)+
  tm_bubbles(fill = "casualty_type",
             shape = "casualty_type",
             #size = "New_Serious",
             shape.legend = tm_legend_combine("fill")) +
  tm_title("H3 grid with highest KSI between: 2020 and 2024")+
  tm_options(frame = FALSE)+
  tm_legend(frame = FALSE)

