library(h3jsr)


cycle_network_stats <- function(city,city_shape, osm_data, city_pop){

  # translate to drvining and cycling
  drive_net = osmactive::get_driving_network(osm_data)
  cycle_net <- osmactive::get_cycling_network(osm_data)
  
  # determine the cycle network and pick out detailed segregation columns
  cycle_net_d = distance_to_road(cycle_net, drive_net)
  cycle_net_c = classify_cycle_infrastructure(cycle_net_d) |>
    select(osm_id,detailed_segregation, geometry)
  
  # pick out paths likely to be segregated based on 3 common classifications
  seg_paths <- cycle_net_c[cycle_net_c$detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"),]
  
  city_data <- data.frame(city = city,
                           area_city_km2 = as.numeric(st_area(city_shape))/1000000,
                           city_pop = city_pop,
                           driving_routes = sum(as.numeric(st_length(drive_net)))/1000,
                           cycle_paths = sum(as.numeric(st_length(cycle_net_c)))/1000,
                           seg_cycle = sum(as.numeric(st_length(seg_paths)))/1000) |> 
    mutate(pc_seg = seg_cycle/cycle_paths*100,
           cycle_pp = cycle_paths*1000/city_pop,
           seg_pp = seg_cycle*1000/city_pop,
           drive_pp = driving_routes*1000/city_pop,
           cycle_km2 = cycle_paths/area_city_km2,
           seg_km2 = seg_cycle/area_city_km2,
           drive_km2 = driving_routes/area_city_km2)

  return(city_data)
}

roads_cra_match <- function(osm_network_sf, crash_sf){
  
  drive_net = osmactive::get_driving_network(osm_network_sf)
  
  crs_osm <- crs(drive_net)
  
  crash_sf <- st_transform(crash_sf, crs_osm)
  
  crash_sf$osm_id <- drive_net$osm_id[st_nearest_feature(crash_sf,drive_net)]
  
  return(crash_sf)
  
}




# sum up the casualties per collision to form a single row, reduces issues with double counting and makes joining data neater
casualties_single_row <- function(casualties){

      cas_summary <- casualties |>
        mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>
        select(collision_index, collision_year, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
        group_by(collision_index, collision_year) |>
        summarise(Fatal = sum(fatal_count),
                  Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
                  Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))
      
      return(cas_summary)
      
}

casualties_rates <- function(casualties){
  
  cas_summary <- casualties |>
    select(collision_index, collision_year, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
    group_by(collision_index, collision_year) |>
    summarise(Fatal = sum(fatal_count),
              Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
              Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))
  
  cas_rates <- cas_summary |>
    group_by(collision_year) |>
    summarise(Fatal = sum(Fatal),
              Serious = sum(Serious,na.rm = TRUE),
              Slight = sum(Slight,na.rm = TRUE))
  
  return(cas_rates)
  
}

casualties_index <- function(casualties, base_year, end_year){
  
  cas_rates <- casualties_rates(casualties)
  
  # baseline values for index plot
  bm_vals <- cas_rates |> filter(collision_year == base_year)
  
  
  # calaute table of indexes
  indexes <- cas_rates %>%
    transmute(year = collision_year,
              Fatal = Fatal/bm_vals$Fatal*100,
              Serious = Serious/bm_vals$Serious*100,
              Slight = Slight/bm_vals$Slight*100)
  
  return(indexes)
  
}

names(casualties[c()])

# group by demographics age breaks used are those by DfT statistics reports
group_casualties_other <- function(casualties,
                                   keep_index = TRUE,
                       grouping = c("pedestrian_location", "casualty_severity", "casualty_type", "car_passenger", "bus_or_coach_passenger", "casualty_imd_decile", "lsoa_of_casualty"),
                      severities = c("Fatal", "Serious", "Slight")){

  if(keep_index == FALSE){
  cas_group <- casualties |>
    group_by(across(all_of(grouping))) |> 
    summarise(Fatal = sum(fatal_count,na.rm = TRUE),
              Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
              Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))|>
    rowwise() |>
      mutate(All = rowSums(across(all_of(severities))))
  } else {
    cas_group <- casualties |>
      group_by(collision_index, across(all_of(grouping))) |> 
      summarise(Fatal = sum(fatal_count,na.rm = TRUE),
                Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
                Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))|>
      rowwise() |>
      mutate(All = rowSums(across(all_of(severities))))
  }
  
  return(cas_group)
  
  }
  
  
  


# group by demographics age breaks used are those by DfT statistics reports
group_casualties_age_sex <- function(casualties,
                          demographic = c("sex", "age"),
                          breaks=c(0,11,15,19,24,29,39,49,59,69,100),
                          labels=c("0-11","12-15","16-19","20-24","25-29","30-39","40-49","50-59","60-69","70+"),
                          severities = c("Fatal", "Serious", "Slight")){
  
  if (identical(demographic, "age")) {
    cas_demo <- casualties |>
      mutate(age_band = cut(as.numeric(age_of_casualty), breaks=breaks,labels=labels)) |>
      group_by(age_band) %>%
      summarise(Fatal = sum(fatal_count,na.rm = TRUE),
                Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
                Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))|>
      select(age_band,{{ severities }}) |> 
      rowwise() |>
      mutate(All = rowSums(across(all_of(severities))))
  }
  
  if (identical(demographic, "sex")) {
    cas_demo <- casualties |>
      group_by(sex_of_casualty) %>%
      summarise(Fatal = sum(fatal_count,na.rm = TRUE),
                Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
                Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))|>
      select(age_band,{{ severities }}) |> 
      rowwise() |>
      mutate(All = rowSums(across(all_of(severities))))
  }
  
  if (all(c("age", "sex") %in% demographic)) {
    cas_demo <- casualties |>
      mutate(age_band = cut(as.numeric(age_of_casualty), breaks=breaks,labels=labels)) |>
      group_by(sex_of_casualty, age_band) %>%
      summarise(Fatal = sum(fatal_count,na.rm = TRUE),
                Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
                Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |>
      filter(!is.na(age_band)) |>
      select(age_band,{{ severities }}) |> 
      rowwise() |>
      mutate(All = rowSums(across(all_of(severities))))
    
  }
  
  
  
  # add pc_ksi when Fatal and Serious are present
  if(all(c("Fatal", "Serious") %in% severities)){
    cas_demo <- cas_demo |> ungroup() |> mutate(KSI = Fatal+Serious) |> mutate(pc_ksi = (KSI/sum(KSI))*100) |> mutate(pc_all = (All/sum(All))*100) 
  } else {
    cas_demo <- cas_demo |> ungroup() |> mutate(pc_all = (All/sum(All))*100)
  }
  
  return(cas_demo)
  
}

crash_conditions_sum <- function(crashes, 
                                  casualties,
                                  city = "Bristol",
                                  severities = c("Fatal", "Serious", "Slight"),
                                  parameter = c("road_surface_conditions", "junction_detail", "speed_limit", "light_conditions", "weather_conditions")
                                 ){
  
  
  
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
           light_conditions,weather_conditions,datetime,
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
 
  return(crash_parameter)
   
}

time_date_summary <- function(crashes,
                        casualties){
  
  cas_summary <- casualties_single_row(casualties)

crash_time <- cas_summary |>
  left_join(crashes, by = "collision_index") |> # join crashes as number of vehicles is included, quicker than calculating from veh table
  select(datetime, Fatal, Serious) |>
  mutate(#collision_hr = lubridate::hour(datetime),
    dow = clock::date_weekday_factor(datetime, abbreviate = FALSE),
    collision_hr = get_hour(datetime),
    KSI = sum(Fatal, Serious)) |>
  mutate(dow = case_when(dow == "Monday" ~  "Monday to Friday",
                         dow == "Tuesday" ~  "Monday to Friday",
                         dow == "Wednesday" ~  "Monday to Friday",
                         dow == "Thursday" ~  "Monday to Friday",
                         dow == "Friday" ~  "Monday to Friday",
                         dow == "Saturday" ~  "Saturday",
                         dow == "Sunday" ~  "Sunday")) |>
  #mutate(dow = case_when(dow > 1 & dow < 7 ~  "Monday to Friday", dow == 7 ~ "Saturday", dow == 1 ~ "Sunday")) |>
  group_by(collision_hr, dow) |>
  summarise(KSI = sum(KSI)) |>
  mutate(KSI = if_else(dow == "Monday to Friday", KSI/5, KSI))

return(crash_time)

}


tag_costs <- function(crashes,
                         casualties){

  ons_cost_dat <- get_ons_cost_data()
  
  cas_summary <- casualties_single_row(casualties)
  
  cra_cas_down <- crashes |>
    st_set_geometry(NULL) |>
    select(collision_index) |>
    left_join(cas_summary, by = "collision_index") |> 
    melt(c("collision_index", "collision_year")) |>
    filter(value > 0)
  
cwc <- cra_cas_down |>
  mutate(collision_year = as.character(collision_year)) |>
  left_join(ons_cost_dat, by = c("collision_year" = "collision_data_year", "variable" = "severity")) |>
  rowwise() |>
  mutate(casualty_cost = sum(value*as.numeric(gsub(",","", cost_per_casualty))),
         collision_cost = sum(value*as.numeric(gsub(",", "", cost_per_collision)))) |>
  group_by(collision_year, variable) |>
  summarise(total_casualties = round(sum(value),1),
            casualty_cost = round(sum(casualty_cost)),
            collision_cost = round(sum(collision_cost))) |> 
  mutate(total_cost = casualty_cost+collision_cost)

return(cwc)

}



summarise_casualty_types <- function(casualty_df, summary_type = c("short_name", "in_or_on")){
  
  cas_type <- data.frame(casualty_type = c("Car occupant","Motorcycle 125cc and under rider or passenger","Cyclist","Pedestrian","Motorcycle over 500cc rider or passenger","Motorcycle over 125cc and up to 500cc rider or  passenger",
                                           "Motorcycle 50cc and under rider or passenger","Bus or coach occupant (17 or more pass seats)","Taxi/Private hire car occupant","Van / Goods vehicle (3.5 tonnes mgw or under) occupant","Other vehicle occupant","Data missing or out of range",                         
                                            "Motorcycle - unknown cc rider or passenger","Goods vehicle (7.5 tonnes mgw and over) occupant","Electric motorcycle rider or passenger","Minibus (8 - 16 passenger seats) occupant","Mobility scooter rider","Horse rider",  
                                            "Goods vehicle (over 3.5t. and under 7.5t.) occupant", "Goods vehicle (unknown weight) occupant","Agricultural vehicle occupant","E-scooter rider"),
                            short_name = c("Car occupant", "Motorcyclist", "Cyclist", "Pedestrian", "Motorcyclist", "Motorcyclist", "Motorcyclist", 
                                           "Bus occupant", "Taxi occupant", "Goods vehicle occupant", "Other vehicle", "Data missing", "Motorcyclist", 
                                           "Goods vehicle occupant", "Motorcyclist", "Bus occupant", "Mobility scooter rider", "Horse rider", 
                                           "Goods vehicle occupant", "Goods vehicle occupant", "Agricultural vehicle occupant","E-scooter rider"),
                            in_or_on = c("Car", "Motorcyclist", "Bicycle", "Foot", "Motorcyclist", "Motorcyclist", "Motorcyclist", 
                                           "Bus", "Taxi", "Goods vehicle", "Other vehicle", "Data missing", "Motorcyclist", 
                                           "Goods vehicle", "Motorcyclist", "Bus", "Mobility scooterr", "Horse", 
                                           "Goods vehicle", "Goods vehicle", "Agricultural vehicle", "E-scooter")) |> 
    select(casualty_type, {{summary_type}})
  
  
  cas_out <- left_join(casualty_df, cas_type, by = "casualty_type")
  
  return(cas_out)
  
}



# writing function to summarise casualty data by OSM link. Want it to be able to either show all casualties and also crash details like time of day and surface
casualty_osm_link <- function(osm_links,
                              casualties,
                              crashes,
                              year_from,
                              year_to,
                              sum_by_crash = FALSE,
                              casualties_buffer){
  
  # simplify casualty type
  casualties_type_sum <- summarise_casualty_types(casualties) |> 
    mutate(casualty_type = short_name)
  
  osm_buff <- osm_links |>
    st_transform(27700) |>
    st_buffer(buff_dist) |>
    st_union() |>
    st_as_sf()
  
  cra_cas_osm <- crashes[osm_buff,] |> 
    select(collision_index, geometry) |> 
    left_join(casualties_type_sum, by = "collision_index") |> 
    mutate(Fatal = fatal_count,
           Serious = casualty_adjusted_severity_serious,
           Slight = casualty_adjusted_severity_slight)
  
  return(cra_cas_osm)

}

# writing function to summarise casualty data by OSM link. Want it to be able to either show all casualties and also crash details like time of day and surface
vehicle_osm_link <- function(osm_links,
                              casualties,
                              crashes,
                              year_from,
                              year_to,
                              sum_by_crash = FALSE,
                              casualties_buffer){
  

  
  # simplify casualty type
  vehicle_type_sum <- vehicle_groups_simplify(vehicles) 
  
  osm_buff <- osm_links |>
    st_transform(27700) |>
    st_buffer(buff_dist) |>
    st_union() |>
    st_as_sf()
  
  cra_veh_osm <- crashes[osm_buff,] |> 
    select(collision_index,collision_severity, geometry) |> 
    left_join(vehicle_type_sum, by = "collision_index")
  
  return(cra_veh_osm)
  
}


## create some approximate groups
vehicle_groups <- data.frame(vehicle_type = c("Car","Motorcycle 125cc and under","Taxi/Private hire car","Motorcycle over 500cc",               
                                              "Motorcycle over 125cc and up to 500cc","Goods 7.5 tonnes mgw and over","Goods over 3.5t. and under 7.5t","Bus or coach (17 or more pass seats)",
                                              "Van / Goods 3.5 tonnes mgw or under","Motorcycle 50cc and under","Other vehicle","Pedal cycle",                      
                                              "Motorcycle - unknown cc","Electric motorcycle","e-scooter","Minibus (8 - 16 passenger seats)",     
                                              "Mobility scooter","Unknown vehicle type (self rep only)","Agricultural vehicle","Goods vehicle - unknown weight",    
                                              "Data missing or out of range","Ridden horse"),
                             summary_type =  c("Car","Motorcycle","Taxi","Motorcycle", "Motorcycle","Goods vehicle","Goods vehicle","Bus",
                                               "Goods vehicle","Motorcycle","Other vehicle","Pedal cycle","Motorcycle","Motorcycle","e-scooter","Bus",     
                                               "Mobility scooter","Other vehicle","Agricultural vehicle","Goods vehicle","Data missing or out of range","Ridden horse"))

vehicle_groups_simplify <- function(vehicles, summarise_categories = TRUE){
  
  veh_new_types <- vehicles |>
    left_join(vehicle_groups, by = "vehicle_type") |> 
    mutate(vehicle_type = summary_type)
  
  return(veh_new_types)
}


# makes a column for each vehicle and summarises each collision in one row
vehicles_to_single_row <- function(vehicles, summarise_categories = TRUE){
  
# summarise vehicle table so only one row per collision
veh_summary <- vehicles |>
  left_join(vehicle_groups, by = "vehicle_type") |> 
  transmute(collision_index, vehicle_type = summary_type) |>
  group_by(collision_index, vehicle_type) |>
  mutate(number_vehicles = 1) |>
  summarise(number_vehicles = sum(number_vehicles)) |>
  tidyr::pivot_wider(names_from = "vehicle_type", values_from = "number_vehicles")

return(veh_summary)

}

assign_h3 <- function(city_sf,
                            hex_res = 8,
                            crashes,
                            casualties){
  
  city_sf = city_shp
  hex_res = 10
  
  # create h3
  hex_ids <- h3jsr::polygon_to_cells(city_sf, res = hex_res)
  
  # Convert H3 indexes back to sf polygons
  hex_sf <- st_as_sf(h3jsr::cell_to_polygon(hex_ids))
  
  crashes <- st_transform(crashes,4326)
  
  # remove crashes outside hex area (which is slightly different to the city boundary)
  crashes <- crashes[hex_sf,]
  
  # assign corret index
  hex_sf$h3_index <- unlist(hex_ids)
  
  crashes$h3_index <- hex_sf$h3_index[unlist(st_intersects(crashes, hex_sf))]
  
  # crashes_hex <- crashes |> 
  #   st_set_geometry(NULL) |> 
  #   group_by(h3_index) |> 
  #   summarise(tot_cas = sum(as.numeric(number_of_casualties))) |> 
  #   left_join(hex_sf, by = "h3_index")
  # 
  # st_geometry(crashes_hex) <- crashes_hex$x
  # 
  # mapview(crashes_hex['tot_cas'])
  
  return(crashes)

  
  }
