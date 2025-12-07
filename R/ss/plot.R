library(tmap)
library(tmap.networks)
library(terra)
library(sf)
library(dplyr)


city_plot <- function(city_shape,
                      map_type = c("osm", "carto_light"),
                      border_col = "#ff7733",
                      plot_dir = "plots/"){
if(map_type == "osm"){
bm <- basemaps::basemap_raster(ext=city_shape, map_service = "osm", map_type = "topographic")
}
if(map_type == "carto_light"){
  bm <- basemaps::basemap_raster(ext=city_shape, map_service = "carto", map_type = "light")
}
# mask the background to the shape
bm_masked <- mask(bm, st_transform(city_shape, 3857))
# plot the boundary
tm1 <- tm_shape(bm_masked)+
  tm_rgb()+
  tm_scalebar()+
  tm_shape(city_shape)+
  tm_polygons(fill_alpha = 0, col = border_col, col_alpha = 1, lwd = 10)+
  tm_layout(frame = FALSE)

# save to folder
tmap_save(tm1, paste0(plot_dir,"/",city, "_map.png"), width = 5000, height = 5000)

return(tm1)

}



cycle_network_plot <- function(city,
                               city_shape,
                               osm_data,
                               city_pop,
                               title_position = "right",
                               stats_position = "right",
                               legend_position = "left",
                               plot_dir = "plots/"){
  
  
  stats <- cycle_network_stats(city = city, city_shape = city_shape, osm_data = osm_data, city_pop = city_pop)
  
  drive_net = osmactive::get_driving_network(osm_data)
  cycle_net <- osmactive::get_cycling_network(osm_data)
  
  cycle_net_d = distance_to_road(cycle_net, drive_net)
  
  cycle_net_c = classify_cycle_infrastructure(cycle_net_d) |>
    select(osm_id,detailed_segregation, geometry) |>
    mutate(segregated = ifelse(detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"),"YES","NO"))
  
  seg_paths <- cycle_net_c[cycle_net_c$detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"),]

#assign(paste0(c,"_net"), cycle_net_c)

bg <- basemaps::basemap_raster(city_shape, map_service = "carto", map_type = "light")
bg_m <- mask(bg,st_transform(city_shape,3857))

if(title_position == "left"){
  tit_pos = c(0,0.99)
} else {
  tit_pos = c(0.74,0.98)
}
if(stats_position == "left"){
  cred_pos = c(0.01,0.93)
} else {
  cred_pos = c(0.75,0.93)
}
if(legend_position == "right"){
  leg_pos = c(0.85,0.15)
} else {
  leg_pos = c(0.15,0.15)
}

#assign(paste0(c,"_map"), cycle_net_c)
tmap_mode("plot")
tm1 <- tm_shape(bg_m)+
  tm_rgb(col_alpha = 1)+
  tm_shape(city_shape)+
  tm_polygons(fill_alpha = 0)+
  tm_shape(cycle_net_c) +
  tm_edges(
    col = "segregated",
    lwd = 3
  )+
  tm_legend(show = TRUE, position = leg_pos)+
  tm_title(text = city, position = tit_pos)+
  tm_credits(paste0("Area population (2024): ", format(round(stats$city_pop), big.mark = ",", scientific = FALSE), "\n",
                    "cycle path total km: ", round(stats$cycle_paths),"\n",
                    "segregated cycle paths total km: ", round(stats$seg_cycle),"\n",
                    "metres of total cycle path per person: ", round(stats$cycle_pp,2),"\n",
                    "metres of segregated cycle path per person: ", round(stats$seg_pp,2),"\n",
                    "km of total cycle path per km\u00B2: ", round(stats$cycle_km2,2),"\n",
                    "km of segregated cycle path per km\u00B2: ", round(stats$seg_km2,2),"\n",
                    "metres of road per person: ", round(stats$drive_pp,2),"\n",
                    "km of road per km\u00B2: ", round(stats$drive_km2,2),"\n"),bg = TRUE, size = 0.5,bg.alpha = 0.3, bg.color = "grey95", position = cred_pos)+
  tm_layout(frame = FALSE)



tmap_save(tm1, paste0(plot_dir,"/", city, "_cycle_paths.png"), dpi = 400)

return(tm1)

}


index_plot <- function(indexes, 
                       base_year = base_year, 
                       end_year = end_year, 
                       pal = c("#ff7733", "#1de9b6","#006853"),
                       city = city,
                       plot_dir = "plots/"){

chart_2 <- melt(indexes, "year")

cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, scale_color_manual(values = pal))

chart_2 %>%
  ggplot(aes(year, value, color = variable)) +
  geom_line(size = 2, alpha = .8) +
  dft_theme+
  theme(panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=100, linetype='dotted', col = 'black')+
  ggtitle(paste0("Index of casualties by severity, ",city,": ", base_year," - ", end_year, " (Index ", base_year,"=100)")) +
  scale_x_continuous(name = NULL,
                     breaks = seq(base_year, end_year, by = 1)  # Add more tick marks
  ) +
  ylab("index")+
  labs(caption = "Source: Stats19")+
  theme(panel.border = element_blank())

ggsave(paste0(plot_dir,"/",city, "_index.png"))

return(chart_2)

}


demog_plot <- function(casualties, 
                      pal = c("#ff7733", "#1de9b6","#006853"),
                      city = city,
                      severity = c("all", "ksi"),
                      stat = c("pc"),
                      plot_dir = "plots/"){
  
  pal = c("#ff7733", "#1de9b6","#006853")
  city = city
  stat = "all"
  plot_dir = "plots/"
  
  if(severity == "ksi"){
  demo_sum <- group_demo(casualties, demographic = c("both"),severities = c("Fatal", "Serious"))
  }
  if(severity == "all"){
    demo_sum <- group_demo(casualties, demographic = c("both"),severities = c("Fatal", "Serious", "Slight"))
  }

# Define colours and theme
cust_theme <- theme(panel.grid.major = element_line(size = 2))
dft_theme <- list(cust_theme, scale_fill_manual(values = pal))  # use fill, not color
if(stat == "ksi"){
  ggplot(demo_sum, aes(x = age_band, y = pc_ksi, fill = sex_of_casualty))+
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
    geom_text(
      aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
      position = position_dodge(width = 0.7),
      vjust = -0.5,
      size = 3
    ) +
    ggtitle(paste0("Percentage of KSI casualties, by sex and age, ", city,": 2010 to 2024")) +
    dft_theme +
    theme(
      panel.background = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    ) +
    ylab(NULL)+
    xlab(NULL)+
    labs(caption = "Source: Stats19")
}
if(stat == "all"){
  ggplot(demo_sum, aes(x = age_band, y = pc_all, fill = sex_of_casualty))+
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
    geom_text(
      aes(label = paste0(round(pc_all),"%")),  # Round values to 1 decimal place
      position = position_dodge(width = 0.7),
      vjust = -0.5,
      size = 3
    ) +
    ggtitle(paste0("Percentage of all casualties, by sex and age, ", city,": 2010 to 2024")) +
    dft_theme +
    theme(
      panel.background = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    ) +
    ylab(NULL)+
    xlab(NULL)+
    labs(caption = "Source: Stats19")
}



ggsave(paste0(plot_dir,"/", city, "_demographic.png"))

}


year_sum_plot <- function(casualties, 
                          pal = c("#ff7733", "#1de9b6","#006853"),
                          city = city,
                          severity = c("Fatal", "Serious", "Slight"),
                          plot_dir = "plots/"){
  
  cas_rates <- casualties_rates(casualties)
  
  # Define colours and theme
  cust_theme <- theme(panel.grid.major = element_line(size = 2))
  dft_theme <- list(cust_theme, scale_fill_manual(values = pal))  # use fill, not color
  
  # year
  year_count <- cas_rates |>
    melt("collision_year") |>
    filter(variable %in% severity) |>
    mutate(collision_year = as.character(collision_year))
  
  ggplot(year_count, aes(x = collision_year, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
    geom_text(
      aes(label = paste0(round(value),"")),  # Round values to 1 decimal place
      position = position_dodge(width = 0.7),
      vjust = -0.5,
      size = 3
    ) +
    ggtitle(paste0("Total casualties by severity (",paste(severity,sep = ","),") and year, ",city,": 2010 to 2024")) +
    dft_theme +
    theme(
      panel.background = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    ) +
    ylab(NULL)+
    xlab(NULL)+
    labs(caption = "Source: Stats19")
  
  ggsave(paste0(plot_dir,"/",city, "_year_totals.png"))
  
}


crash_conditions_plot <- function(crashes, 
                                  casualties,
                            city = "Bristol",
                            severities = c("Fatal", "Serious", "Slight"),
                            parameter = c("road_surface_conditions", "junction_detail", "speed_limit", "light_conditions", "weather_conditions"),
                            plot_width = 10,
                            plot_dir = "plots/"){
  
  cas_summary <- casualties_single_row(casualties)
  
  # get consistency with palette for each severity
  pal_sev <- data.frame(pal = c("#ff7733", "#1de9b6","#006853"),severity = c("Fatal", "Serious", "Slight"))
  
  # pick them out
  pal <- pal_sev$pal[pal_sev$severity %in% severities]

  cra_cas <- crashes |>
    st_set_geometry(NULL) |>
    select(collision_index) |>
    left_join(cas_summary, by = "collision_index") |> 
    melt(c("collision_index", "collision_year")) |>
    filter(value > 0)
  
  crashes_dat <- crashes |>
    st_set_geometry(NULL) |>
    select(collision_index, collision_year, speed_limit, time, day_of_week,first_road_number, junction_detail,
           first_road_class, second_road_number, second_road_class, light_conditions,weather_conditions,datetime,
           road_surface_conditions)
  
    cra_cas_cond <- cra_cas |>
    left_join(crashes_dat, by = "collision_index")
  
  # road surface
  crash_parameter <- cra_cas_cond |>
    filter(variable %in% severities) |> 
    group_by(across(all_of(parameter)), variable) |>
    summarise(casualties = sum(value)) |>
    ungroup() |>
    mutate(pc_ksi = (casualties/sum(casualties))*100)
  
  start_year <- min(crashes_dat$collision_year)
  end_year <- max(crashes_dat$collision_year)
  
  # Define colours and theme
  cust_theme <- theme(panel.grid.major = element_line(size = 2))
  dft_theme <- list(cust_theme, scale_fill_manual(values = pal))  # use fill, not color
  
  ggplot(crash_parameter, aes(x = .data[[parameter]], y = pc_ksi, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
    geom_text(
      aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
      position = position_dodge(width = 0.7),
      vjust = -0.5,
      size = 3
    ) +
    ggtitle(paste0("Percentage of ", paste(severities, sep = ", ")," casualties, by ",gsub("_"," ", parameter), ", ", city,": (",start_year," to ", end_year,")")) +
    dft_theme +
    theme(
      panel.background = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    ) +
    ylab(NULL)+
    xlab(NULL)+
    labs(caption = "Source: Stats19")
  
  ggsave(paste0(plot_dir, "/", city, "_", parameter, ".png"), width = plot_width)
  
}


#MF_peak <- crash_time |> filter(dow == "Monday to Friday") |> arrange(desc(KSI)) |> mutate(hr = str_sub(gsub(" ","", tolower(format(strptime(collision_hr, format = "%H"), "%I %p"))),2))

#SS_peak <- crash_time |> filter(dow %in% c("Saturday", "Sunday")) |> group_by(collision_hr) |> summarise(KSI = sum(KSI)) |> arrange(desc(KSI)) |> mutate(hr = str_sub(gsub(" ","", tolower(format(strptime(collision_hr, format = "%H"), "%I %p"))),2))

time_date_plot <- function(crashes, 
                           casualties,
                           city = "Bristol",
                           severities = c("Fatal", "Serious", "Slight"),
                           parameter = c("road_surface_conditions", "junction_detail", "speed_limit", "light_conditions", "weather_conditions"),
                           plot_width = 10,
                           plot_dir = "plots/"){

# define the colour palette
cols <- rev(c("#ff7733", "#1de9b6","#006853"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, scale_color_manual(values = cols))

crash_time %>%
  ggplot(aes(collision_hr, KSI, color = dow)) +
  geom_line(size = 2, alpha = .8) +
  dft_theme+
  theme(panel.background = element_blank(),
        legend.position = "top", legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle(paste0("Chart 4: Reported ", tolower(report_casualty), " KSIs by hour of day and day of week, GB: ", yr2calc-4, " to ", yr2calc)) +
  ylab(NULL)+
  labs(x = "Hour starting", caption = "Source: Stats19")

}

tag_plot <- function(crashes,
                     casualties,
                     city,
                     pal = c("#ff7733", "#1de9b6"),
                     plot_dir = "plots/"){
  
  city = "Bristol"
  # make sure plot directory exists
  dir.create(plot_dir)
  
  tag_df <- tag_costs(crashes, casualties)
  
  chart_0 <- tag_df |>
    select(-total_cost, -total_casualties, -variable) |> 
    melt(c("collision_year")) |> 
    mutate(value = value/1000000, # convert to millions
           variable = gsub("_", " ", variable))
  
  names(chart_0) <- c("year", "cost category", "cost")
  
  # Define colours and theme
  pal <- c4a("carto.pastel", n = NROW(unique(chart_0$`cost category`)))
  cust_theme <- theme(panel.grid.major = element_line(size = 2))
  dft_theme <- list(cust_theme, scale_color_manual(values = pal))
  
  sy = min(chart_0$year)
  ey = max(chart_0$year)
  
  # plot
  ggplot(chart_0, aes(x = year, y = cost, fill = `cost category`)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    geom_text(
      aes(label = NA),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    ggtitle(paste0(
      "Annual value of prevention of collisions in ",city, " between ",sy, " and ", ey
    ),subtitle = "Calculated using collision data from DfT STATS19 and cost data from TAG") +
    dft_theme +
    theme(
      panel.background = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    ) +
    ylab("Casualty and Collision cost (£ million)") +
    xlab(NULL) +
    labs(caption = "Source: Stats19 and TAG")
  
  ggsave(paste0(plot_dir,"/", city,"_tag_costs.png"))
  
  
}

casualties_plot <- function(crashes, casualties, plot_dir = "plots/", icon_sized_by_severity = TRUE){
  
  casualties_map <- casualties |>
    select(collision_index, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
    group_by(collision_index, casualty_type) |>
    summarise(Fatal = sum(fatal_count),
              Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
              Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |>
    left_join(crashes, by = "collision_index") |>
    select(casualty_type, Fatal, Serious, Slight, geometry) |>
    st_as_sf()
  
  bm <- basemaps::basemap_raster(ext=casualties_map, map_service = "carto", map_type = "light")
  
  sy = min(casualties_map$year)
  ey = max(casualties_map$year)
  
  #pal <- data.frame(name = unique(cas_winsley_map$casualty_type), pal = c4a("brewer.accent", n = NROW(unique(cas_winsley_map$casualty_type))))
  
  if(icon_sized_by_severity == TRUE){
  tm2 <- tm_shape(bm)+
    tm_rgb()+
    tm_shape(casualties_map)+
    tm_bubbles(fill = "casualty_type",
               shape = "casualty_type",
               size = "Serious",
               shape.legend = tm_legend_combine("fill"),
               size.legend = tm_legend(title = "Severity"))+
     tm_title(paste0("Collision location with casualty type represented by shape and colour and severity represented by size. ",city,": ",sy," and ", ey))
  } else {
    tm2 <- tm_shape(bm)+
      tm_rgb()+
      tm_shape(casualties_map)+
      tm_bubbles(fill = "casualty_type",
                 shape = "casualty_type",
                 size = "Serious",
                 shape.legend = tm_legend_combine("fill"),
                 size.legend = tm_legend(title = "Severity"))+
      tm_title(paste0("Collision location with casualty type represented by shape and colour and severity represented by size. ",city,": ",sy," and ", ey))
  }
  tmap_save(tm2, paste0(plot_dir, "/", city,"_cas_type_sev_map.png"), width = 9500, height = 7000, dpi = 650)
  
}


casualty_type_plot <- function(crashes, 
                               casualties,
                               city = "Bristol",
                               severities = c("Fatal", "Serious", "Slight"),
                               plot_width = 20,
                               plot_height = 20,
                               plot_dpi = 50,
                               cas_type = c("casualty_type", "short_name", "in_or_on"),
                               plot_dir = "plots/"){
  
  
  # get consistency with palette for each severity
  pal_sev <- data.frame(pal = c("#ff7733", "#1de9b6","#006853"),severity = c("Fatal", "Serious", "Slight"))
  
  cas_type = "short_name"
  
  # pick them out
  pal <- pal_sev$pal[pal_sev$severity %in% severities]
  
  sy <- min(casualties$collision_year)
  ey <- max(casualties$collision_year)
  

  casualties_map <- summarise_casualty_types(casualties, summary_type = cas_type) |>
  select(collision_index, casualty_type = {{cas_type}}, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  group_by(collision_index, casualty_type) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |>
  left_join(crashes, by = "collision_index") |>
  select(casualty_type, Fatal, Serious, Slight, geometry) |>
  st_as_sf()


casualty_type <- casualties_map |>
  st_set_geometry(NULL) |>
  group_by(casualty_type) |>
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious),
            Slight = sum(Slight)) |>
  melt("casualty_type") |> 
  filter(variable %in% severities) |> 
  ungroup() |>
  mutate(pc_total = (value/sum(value))*100)

# Define colours and theme
cust_theme <- theme(panel.grid.major = element_line(size = 2))
dft_theme <- list(cust_theme, scale_fill_manual(values = pal))  # use fill, not color

ggplot(casualty_type, aes(x = casualty_type, y = pc_total, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_total),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Percentage of casualties, by casualty type, ",city,": ",sy, " to ", ey)) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave(paste0(plot_dir, "/",city, "_", paste(severities,collapse = "_"), "_cas_type.png"),units = "dpi", width = plot_width, height = plot_height, dpi = plot_dpi)

}

waffle_plot <- function(data2plot,
                        plot_rows = 3,
                        legend_position = "bottom",
                        title,
                        pal = "poly.sky24",
                        plot_file = "plots/waffle.png"){
  
  # get enough colours for the variables, and a palette with good contrast
  colz <- c4a(pal, n = NROW(data2plot))
  
  # create waffle plot
  waffle(KSI_pav, rows = plot_rows, colors = colz, legend_pos = legend_position, title = title)
  
  # write out with ggplot2
  ggsave(plot_file)
  
}


osm_street_casualties_plot <- function(osm_links,
                            casualties,
                            crashes,
                            year_from,
                            year_to,
                            casualties_buffer,
                            plot_buffer,
                            legend_pos = c(0.6,1),
                            plot_dir){
  
  cra_cas_osm <- casualty_osm_link(osm_links,casualties,crashes,year_from,year_to, casualties_buffer = 20)

  bm_ps <- basemaps::basemap_raster(ext=st_buffer(cra_cas_osm,plot_buffer*2), map_service = "carto", map_type = "light")
  bm_masked <- mask(bm_ps,st_transform(st_buffer(cra_cas_osm,plot_buffer),3857))
  
  cas_scale <- c("Car occupant","Motorcyclist","Cyclist","Taxi occupant","Goods vehicle occupant","Bus occupant","Other vehicle",
                           "Data missing","Mobility scooter rider","Agricultural vehicle occupant","Horse rider","E-scooter rider","Pedestrian")
  
  cra_cas_osm$casualty_type <- factor(cra_cas_osm$casualty_type,
                                     levels = cas_scale)

  tm1 <- tm_shape(bm_masked)+
    tm_rgb()+
    tm_shape(cra_cas_osm)+
    tm_bubbles(fill = "casualty_type",
               fill.scale = tm_scale_categorical(levels = cas_scale),
               shape = "casualty_type",
               size = "Serious",
               shape.legend = tm_legend_combine("fill"),
               size.legend = tm_legend(title = "Severity")) +
    tm_title(paste0("Collision location with casualty type represented by shape and colour\nand severity represented by size. ",osm_links$name[1],": ", year_from," and ", year_to))+
    tm_layout(frame = FALSE)+
    tm_legend(frame = FALSE, position = legend_pos)

  tmap_save(tm1, paste0(plot_dir,"/",gsub(" ", "_", osm_links$name[1]), "_cas_map.png"), width = 11500, height = 9500, dpi = 1000)

return(tm1)

}

# what vehicles were involved in collisions on a street
osm_street_vehicles_plot <- function(osm_links,
                                       vehicles,
                                       crashes,
                                       year_from,
                                       year_to,
                                       casualties_buffer,
                                       plot_buffer,
                                     severity_point_sizes = c(2,1,0.2),
                                       legend_pos = c(0.6,1),
                                        plot_width = 11500,
                                     plot_height = 9500,
                                     plot_dpi = 1000,
                                       plot_dir){
  
  
  collision_severity <- data.frame(collision_severity = c("Fatal", "Serious", "Slight"),
                                   sev_plot_size = severity_point_sizes)
  
  cra_veh_osm <- vehicle_osm_link(osm_links,casualties,crashes,year_from,year_to, casualties_buffer = 20) |> 
    left_join(collision_severity, by = "collision_severity")
  
  bm_ps <- basemaps::basemap_raster(ext=st_buffer(cra_veh_osm,plot_buffer*2), map_service = "carto", map_type = "light")
  bm_masked <- mask(bm_ps,st_transform(st_buffer(cra_veh_osm,plot_buffer),3857))
  
  veh_scale <- c("Car","Motorcycle","Pedal cycle","Taxi","Goods vehicle","Bus","Other vehicle","Data missing or out of range","Mobility scooter","Agricultural vehicle","Ridden horse","e-scooter")
  
  cra_veh_osm$vehicle_type <- factor(cra_veh_osm$vehicle_type,
                                     levels = veh_scale)
  
  tm1 <- tm_shape(bm_masked)+
    tm_rgb()+
    tm_shape(cra_veh_osm)+
    tm_bubbles(fill = "vehicle_type",
               fill.scale = tm_scale_categorical(levels = veh_scale),
               shape = "vehicle_type",
               size = "sev_plot_size",
               shape.legend = tm_legend_combine("fill"),
               size.legend = tm_legend(title = "sev_plot_size")) +
    tm_title(paste0("Collision location with vehicle driven type represented by shape and colour\nand severity of the collision represented by size. ",osm_links$name[1],": ", year_from," and ", year_to))+
    tm_layout(frame = FALSE)+
    tm_legend(frame = FALSE, position = legend_pos)

  tmap_save(tm1, paste0(plot_dir,"/",gsub(" ", "_", osm_links$name[1]), "_cas_map.png"), width = plot_width, height = plot_height, dpi = plot_dpi)
  
  return(tm1)
  
}

# interactive_plot that can toggle casualty or stats e.g. areas where sex and age are more prevailant