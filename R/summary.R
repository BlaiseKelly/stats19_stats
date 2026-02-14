#' Summarise cycle network statistics for a city
#'
#' Computes summary statistics for driving and cycling networks within a city
#' boundary, including total lengths, segregation, per capita and per km²
#' measures.
#'
#' @param city Character. City name.
#' @param city_shape An `sf` polygon of the city boundary.
#' @param osm_data OSM data object suitable for `osmactive::get_driving_network`
#'   and `osmactive::get_cycling_network`.
#' @param city_pop Numeric. Population of the city.
#' @return A data frame with summary statistics.
#' @export
cycle_network_stats <- function(city, city_shape, osm_data, city_pop) {
  drive_net <- osmactive::get_driving_network(osm_data)
  cycle_net <- osmactive::get_cycling_network(osm_data)
  
  cycle_net_d <- distance_to_road(cycle_net, drive_net)
  cycle_net_c <- classify_cycle_infrastructure(cycle_net_d) %>%
    dplyr::select(osm_id, detailed_segregation, geometry)
  
  seg_paths <- cycle_net_c[cycle_net_c$detailed_segregation %in%
                             c("Level track", "Off Road Path", "Light segregation"), ]
  
  city_data <- data.frame(
    city = city,
    area_city_km2 = as.numeric(sf::st_area(city_shape)) / 1e6,
    city_pop = city_pop,
    driving_routes = sum(as.numeric(sf::st_length(drive_net))) / 1000,
    cycle_paths = sum(as.numeric(sf::st_length(cycle_net_c))) / 1000,
    seg_cycle = sum(as.numeric(sf::st_length(seg_paths))) / 1000
  ) %>%
    dplyr::mutate(
      pc_seg = seg_cycle / cycle_paths * 100,
      cycle_pp = cycle_paths * 1000 / city_pop,
      seg_pp = seg_cycle * 1000 / city_pop,
      drive_pp = driving_routes * 1000 / city_pop,
      cycle_km2 = cycle_paths / area_city_km2,
      seg_km2 = seg_cycle / area_city_km2,
      drive_km2 = driving_routes / area_city_km2
    )
  
  city_data
}

#' Match crashes to nearest OSM road segment
#'
#' Assigns each crash record to the nearest driving network segment from OSM.
#'
#' @param osm_network_sf An `sf` object of OSM network data.
#' @param crash_sf An `sf` object of crash points.
#' @return The crash `sf` object with an added `osm_id` column.
#' @export
roads_cra_match <- function(osm_network_sf, crash_sf) {
  drive_net <- osmactive::get_driving_network(osm_network_sf)
  
  crs_osm <- sf::st_crs(drive_net)
  crash_sf <- sf::st_transform(crash_sf, crs_osm)
  
  crash_sf$osm_id <- drive_net$osm_id[sf::st_nearest_feature(crash_sf, drive_net)]
  
  crash_sf
}

#' Summarise casualties per collision
#'
#' Aggregates casualty records to one row per collision, reducing double
#' counting and simplifying joins.
#'
#' @param casualties A data frame of casualty records.
#' @return A data frame with one row per collision and columns for Fatal,
#'   Serious, and Slight counts.
#' @export
casualties_single_row <- function(casualties) {
  cas_summary <- casualties %>%
    dplyr::mutate(fatal_count = dplyr::if_else(casualty_severity == "Fatal", 1, 0)) %>%
    dplyr::select(collision_index, collision_year, casualty_type,
                  pedestrian_location, fatal_count,
                  casualty_adjusted_severity_serious,
                  casualty_adjusted_severity_slight) %>%
    dplyr::group_by(collision_index, collision_year) %>%
    dplyr::summarise(
      Fatal = sum(fatal_count),
      Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
      Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
      .groups = "drop"
    )
  
  cas_summary
}

#' Summarise casualties per year
#'
#' Aggregates casualty records to yearly totals by severity.
#'
#' @param casualties A data frame of casualty records with columns
#'   `collision_index`, `collision_year`, `fatal_count`,
#'   `casualty_adjusted_severity_serious`, `casualty_adjusted_severity_slight`.
#' @return A data frame with yearly totals for Fatal, Serious, and Slight.
#' @export
casualties_rates <- function(casualties) {
  cas_summary <- casualties %>%
    dplyr::select(collision_index, collision_year, casualty_type,
                  pedestrian_location, fatal_count,
                  casualty_adjusted_severity_serious,
                  casualty_adjusted_severity_slight) %>%
    dplyr::group_by(collision_index, collision_year) %>%
    dplyr::summarise(
      Fatal = sum(fatal_count),
      Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
      Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
      .groups = "drop"
    )
  
  cas_rates <- cas_summary %>%
    dplyr::group_by(collision_year) %>%
    dplyr::summarise(
      Fatal = sum(Fatal),
      Serious = sum(Serious, na.rm = TRUE),
      Slight = sum(Slight, na.rm = TRUE),
      .groups = "drop"
    )
  
  cas_rates
}

#' Create casualty index relative to a base year
#'
#' Computes index values (base year = 100) for Fatal, Serious, and Slight
#' casualties across years.
#'
#' @param casualties Casualty data frame.
#' @param base_year Integer. Year used as baseline.
#' @param end_year Integer. Last year to include.
#' @return A data frame of index values by year.
#' @export
casualties_index <- function(casualties, base_year, end_year) {
  cas_rates <- casualties_rates(casualties)
  
  bm_vals <- dplyr::filter(cas_rates, collision_year == base_year)
  
  indexes <- cas_rates %>%
    dplyr::transmute(
      year = collision_year,
      Fatal = Fatal / bm_vals$Fatal * 100,
      Serious = Serious / bm_vals$Serious * 100,
      Slight = Slight / bm_vals$Slight * 100
    )
  
  indexes
}

#' Group casualties by other demographic variables
#'
#' Aggregates casualties by chosen grouping variables, optionally keeping
#' collision index.
#'
#' @param casualties Casualty data frame.
#' @param keep_index Logical. If `TRUE`, keep `collision_index` in grouping.
#'   Default `TRUE`.
#' @param grouping Character vector of grouping variables. Default includes
#'   `pedestrian_location`, `casualty_severity`, `casualty_type`,
#'   `car_passenger`, `bus_or_coach_passenger`, `casualty_imd_decile`,
#'   `lsoa_of_casualty`.
#' @param severities Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @return A grouped data frame with counts and totals.
#' @export
group_casualties_other <- function(casualties,
                                   keep_index = TRUE,
                                   grouping = c("pedestrian_location",
                                                "casualty_severity",
                                                "casualty_type",
                                                "car_passenger",
                                                "bus_or_coach_passenger",
                                                "casualty_imd_decile",
                                                "lsoa_of_casualty"),
                                   severities = c("Fatal", "Serious", "Slight")) {
  if (!keep_index) {
    cas_group <- casualties %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
      dplyr::summarise(
        Fatal = sum(fatal_count, na.rm = TRUE),
        Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
        Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(All = sum(dplyr::c_across(dplyr::all_of(severities))))
  } else {
    cas_group <- casualties %>%
      dplyr::group_by(collision_index, dplyr::across(dplyr::all_of(grouping))) %>%
      dplyr::summarise(
        Fatal = sum(fatal_count, na.rm = TRUE),
        Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
        Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(All = sum(dplyr::c_across(dplyr::all_of(severities))))
  }
  
  cas_group
}

#' Group casualties by age and/or sex
#'
#' Aggregates casualties by age bands and/or sex, computing percentages and
#' KSI (Killed or Seriously Injured) shares.
#'
#' @param casualties Casualty data frame.
#' @param demographic Character. One of `"age"`, `"sex"`, or both.
#' @param breaks Numeric vector of age breakpoints. Default DfT bands.
#' @param labels Character vector of age band labels.
#' @param severities Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @return A data frame with grouped counts and percentages.
#' @export
group_casualties_age_sex <- function(casualties,
                                     demographic = c("sex", "age"),
                                     breaks = c(0, 11, 15, 19, 24, 29, 39, 49, 59, 69, 100),
                                     labels = c("0-11", "12-15", "16-19", "20-24", "25-29",
                                                "30-39", "40-49", "50-59", "60-69", "70+"),
                                     severities = c("Fatal", "Serious", "Slight")) {
  # age only
  if (identical(demographic, "age")) {
    cas_demo <- casualties %>%
      dplyr::mutate(age_band = cut(as.numeric(age_of_casualty),
                                   breaks = breaks, labels = labels)) %>%
      dplyr::group_by(age_band) %>%
      dplyr::summarise(
        Fatal = sum(fatal_count, na.rm = TRUE),
        Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
        Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(All = sum(dplyr::c_across(dplyr::all_of(severities))))
  }
  
  # sex only
  if (identical(demographic, "sex")) {
    cas_demo <- casualties %>%
      dplyr::group_by(sex_of_casualty) %>%
      dplyr::summarise(
        Fatal = sum(fatal_count, na.rm = TRUE),
        Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
        Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(All = sum(dplyr::c_across(dplyr::all_of(severities))))
  }
  
  # age + sex
  if (all(c("age", "sex") %in% demographic)) {
    cas_demo <- casualties %>%
      dplyr::mutate(age_band = cut(as.numeric(age_of_casualty),
                                   breaks = breaks, labels = labels)) %>%
      dplyr::group_by(sex_of_casualty, age_band) %>%
      dplyr::summarise(
        Fatal = sum(fatal_count, na.rm = TRUE),
        Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
        Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(age_band)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(All = sum(dplyr::c_across(dplyr::all_of(severities))))
  }
  
  # add percentages
  if (all(c("Fatal", "Serious") %in% severities)) {
    cas_demo <- cas_demo %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        KSI = Fatal + Serious,
        pc_ksi = (KSI / sum(KSI)) * 100,
        pc_all = (All / sum(All)) * 100
      )
  } else {
    cas_demo <- cas_demo %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pc_all = (All / sum(All)) * 100)
  }
  
  cas_demo
}

#' Summarise casualties by crash condition
#'
#' Aggregates casualties by severity across a chosen crash condition
#' (e.g. road surface, junction detail, speed limit).
#'
#' @param crashes An `sf` data frame of crash records.
#' @param casualties A data frame of casualty records.
#' @param city Character. City name for reference. Default `"Bristol"`.
#' @param severities Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @param parameter Character. One of `"road_surface_conditions"`,
#'   `"junction_detail"`, `"speed_limit"`, `"light_conditions"`,
#'   `"weather_conditions"`.
#' @return A data frame with counts and percentages by crash condition.
#' @export
crash_conditions_sum <- function(crashes_df,
                                 casualties,
                                 city = "Bristol",
                                 severities = c("Fatal", "Serious", "Slight"),
                                 parameter = c("road_surface_conditions",
                                               "junction_detail",
                                               "speed_limit",
                                               "light_conditions",
                                               "weather_conditions")) {
  parameter <- match.arg(parameter)
  
  cas_summary <- casualties_single_row(casualties)
  
  cra_cas <- crashes %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(collision_index) %>%
    dplyr::left_join(cas_summary, by = "collision_index") %>%
    reshape2::melt(c("collision_index", "collision_year")) %>%
    dplyr::filter(value > 0)
  
  crashes_dat <- crashes %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(collision_index, collision_year, speed_limit, time,
                  day_of_week, first_road_number, junction_detail,
                  light_conditions, weather_conditions, datetime,
                  road_surface_conditions)
  
  cra_cas_cond <- cra_cas %>%
    dplyr::left_join(crashes_dat, by = "collision_index")
  
  crash_parameter <- cra_cas_cond %>%
    dplyr::filter(variable %in% severities) %>%
    dplyr::group_by(dplyr::across(all_of(parameter)), variable) %>%
    dplyr::summarise(casualties = sum(value), .groups = "drop") %>%
    dplyr::mutate(pc_ksi = (casualties/sum(casualties)) * 100)
  
  crash_parameter
}

#' Summarise KSIs by time of day and day of week
#'
#' Aggregates KSI counts by hour and weekday/weekend grouping.
#'
#' @param crashes Crash data frame with `datetime`.
#' @param casualties Casualty data frame.
#' @return A data frame with KSI counts by hour and day group.
#' @export
time_date_summary <- function(crashes, casualties) {
  
  cas_summary <- casualties_single_row(casualties)
  
  crash_time <- cas_summary %>%
    dplyr::left_join(crashes, by = "collision_index") %>%
    dplyr::select(datetime, Fatal, Serious) %>%
    dplyr::mutate(
      dow = clock::date_weekday_factor(datetime, abbreviate = FALSE),
      collision_hr = lubridate::hour(datetime),
      KSI = Fatal + Serious
    ) %>%
    dplyr::mutate(
      dow = dplyr::case_when(
        dow %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Monday to Friday",
        dow == "Saturday" ~ "Saturday",
        dow == "Sunday" ~ "Sunday"
      )
    ) %>%
    dplyr::group_by(collision_hr, dow) %>%
    dplyr::summarise(KSI = sum(KSI), .groups = "drop") %>%
    dplyr::mutate(KSI = dplyr::if_else(dow == "Monday to Friday", KSI / 5, KSI))
  
  crash_time
}

#' Calculate TAG costs for casualties
#'
#' Joins casualty counts with ONS cost data to compute annual prevention costs.
#'
#' @param crashes Crash data frame.
#' @param casualties Casualty data frame.
#' @return A data frame with yearly totals and costs.
#' @export
tag_costs <- function(crashes_df, agg_level) {
  
  tag_df <- match_tag(crashes_df, match_with = agg_level)
  
  if(agg_level == "severity"){
  
  tag_sum <- tag_df %>%
    dplyr::group_by(collision_year, collision_severity) %>%
    dplyr::summarise(
      total_casualties = round(sum(as.numeric(number_of_casualties)), 1),
      casualty_cost = round(sum(cost_per_casualty)),
      collision_cost = round(sum(cost_per_collision-cost_per_casualty)),
      total_cost = round(sum(cost_per_collision)))
  
  }
  
  if(agg_level == "severity_roads"){
    
    tag_sum <- tag_df %>%
      st_set_geometry(NULL) |> 
      dplyr::group_by(collision_year, collision_severity, ons_road) %>%
      dplyr::summarise(total_cost = round(sum(cost))) |> 
      tidyr::pivot_wider(names_from = c(ons_road), values_from = total_cost)
    
  }
  
  return(tag_sum)
  
}

# writing function to summarise casualty data by OSM link. Want it to be able to either show all casualties and also crash details like time of day and surface
casualty_osm_link <- function(osm_links,
                              casualties,
                              crashes,
                              year_from,
                              year_to,
                              sum_by_crash = FALSE,
                              casualties_buffer = 10){
  
  # simplify casualty type
  casualties_type_sum <- summarise_casualty_types(casualties) |> 
    mutate(casualty_type = short_name)
  
  osm_buff <- osm_links |>
    st_transform(27700) |>
    st_buffer(casualties_buffer) |>
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

# writing function to summarise vehicle data by OSM link. Want it to be able to either show all casualties and also crash details like time of day and surface
vehicle_osm_link <- function(osm_links,
                             casualties,
                             casualty_types,
                             crashes,
                             vehicles,
                             year_from,
                             year_to,
                             sum_by_crash = FALSE,
                             casualties_buffer = 10){
  
  # simplify casualty type
  vehicle_type_sum <- vehicle_groups_simplify(vehicles) 
  
  osm_buff <- osm_links |>
    st_transform(27700) |>
    st_buffer(casualties_buffer) |>
    st_union() |>
    st_as_sf()
  
  cra_veh_osm <- crashes[osm_buff,] |> 
    select(collision_index,collision_severity, geometry) |> 
    left_join(vehicle_type_sum, by = "collision_index")
  
  if(!casualty_types == "All"){
    
    cas_in <- casualties |> 
      filter(collision_index %in% cra_veh_osm$collision_index & casualty_type %in% casualty_types)
    
    cra_veh_osm <- cra_veh_osm[cra_veh_osm$collision_index %in% cas_in$collision_index,]
    
  }
  
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
                                               "Mobility scooter","Other vehicle","Agricultural vehicle","Goods vehicle","Data missing or out of range","Ridden horse"),
                             driver_type =  c("Car driver","Motorcycle driver","Taxi driver","Motorcycle driver", "Motorcycle driver","Goods vehicle driver","Goods vehicle driver","Bus driver",
                                               "Goods vehicle driver","Motorcycle driver","Other vehicle","Cyclist","Motorcycle driver","Motorcycle driver","e-scooter driver","Bus driver",     
                                               "Mobility scooter driver","Other vehicle","Agricultural vehicle driver","Goods vehicle driver","Data missing or out of range","Horse rider"))

vehicle_groups_simplify <- function(vehicles, summary_type = c("short_name", "driver_type")){
  
  veh_new_types <- vehicles |>
    left_join(vehicle_groups, by = "vehicle_type") |> 
    mutate(vehicle_type = summary_type)
  
  return(veh_new_types)
}

#' Simplify casualty types
#'
#' Maps detailed casualty types to shorter names or categories.
#'
#' @param casualties Casualty data frame.
#' @param summary_type Character. One of `"short_name"` or `"in_or_on"`.
#' @return Casualty data frame with simplified type column.
#' @export
summarise_casualty_types <- function(casualties,
                                     summary_type = c("short_name", "in_or_on")) {
  summary_type <- match.arg(summary_type)
  
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
                   "Agricultural vehicle occupant", "E-scooter rider"),
    in_or_on = c("Car", "Motorcyclist", "Bicycle", "Foot", "Motorcyclist", "Motorcyclist",
                 "Motorcyclist", "Bus", "Taxi", "Goods vehicle", "Other vehicle",
                 "Data missing", "Motorcyclist", "Goods vehicle", "Motorcyclist",
                 "Bus", "Mobility scooter", "Horse", "Goods vehicle", "Goods vehicle",
                 "Agricultural vehicle", "E-scooter")
  ) %>%
    dplyr::select(casualty_type, !!rlang::sym(summary_type))
  
  cas_out <- dplyr::left_join(casualties, cas_type, by = "casualty_type")
  cas_out
}

#' Summarise vehicles per collision into one row
#'
#' Converts a vehicle-level table into a collision-level summary, with one
#' column per vehicle type and counts as values.
#'
#' @param vehicles A data frame of vehicle records with `collision_index` and
#'   `vehicle_type`.
#' @param summarise_categories Logical. If `TRUE`, vehicle types are simplified
#'   using `vehicle_groups`. Default `TRUE`.
#' @return A wide-format data frame with one row per collision and counts of
#'   vehicles by type.
#' @export
vehicles_to_single_row <- function(vehicles, summarise_categories = TRUE) {
  if (summarise_categories) {
    vehicles <- dplyr::left_join(vehicles, vehicle_groups, by = "vehicle_type") %>%
      dplyr::transmute(collision_index, vehicle_type = summary_type)
  } else {
    vehicles <- dplyr::transmute(vehicles, collision_index, vehicle_type)
  }
  
  veh_summary <- vehicles %>%
    dplyr::group_by(collision_index, vehicle_type) %>%
    dplyr::mutate(number_vehicles = 1) %>%
    dplyr::summarise(number_vehicles = sum(number_vehicles), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "vehicle_type", values_from = "number_vehicles")
  
  veh_summary
}

#' Assign H3 hexagon indexes to crashes
#'
#' Converts a city polygon into H3 hexagons, clips crashes to those hexagons,
#' and assigns each crash an H3 index.
#'
#' @param city_sf An `sf` polygon of the city boundary.
#' @param hex_res Integer. H3 resolution (default `8`).
#' @param crashes An `sf` object of crash points.
#' @param casualties Optional casualty data frame (not used directly here).
#' @return The crash `sf` object with an added `h3_index` column.
#' @export
assign_h3 <- function(city_sf,
                      hex_res = 8,
                      crashes_df,
                      casualties = NULL) {
  # create H3 indexes covering the city polygon
  hex_ids <- h3jsr::polygon_to_cells(city_sf, res = hex_res)
  
  # convert H3 indexes back to sf polygons
  hex_sf <- sf::st_as_sf(h3jsr::cell_to_polygon(hex_ids))
  
  # ensure crashes are in WGS84
  crashes <- sf::st_transform(crashes, 4326)
  
  # keep only crashes inside hex area
  crashes <- crashes[hex_sf, ]
  
  # assign H3 index
  hex_sf$h3_index <- unlist(hex_ids)
  crashes$h3_index <- hex_sf$h3_index[unlist(sf::st_intersects(crashes, hex_sf))]
  
  crashes
}

osm_link_summary = function(osm_data, crash_sf, casualties, match_with = "severity", by_year = TRUE){
  
  osm_data = osm_data
  crash_sf = crashes
  match_with = "severity"
  by_year = TRUE
  
  cas_osm = roads_cra_match(osm_network_sf = osm_data, crash_sf = crash_sf) |> 
    select(collision_index,osm_id) |> 
    st_set_geometry(NULL) |> 
    left_join(casualties_sr, by = "collision_index")
  
  cost_osm <- cas_osm |> 
    select(-collision_index) |> 
    reshape2::melt(c("collision_year", "osm_id"),variable.name = "collision_severity",value.name = "casualties")
  
  if(isTRUE(by_year)){
  
  cost_osm <- match_tag(cost_osm, match_with = "severity") |> 
    transmute(osm_id,
              collision_year,
              casualties,
              casualty_cost = cost_per_casualty*casualties,
              collision_cost = (cost_per_collision-cost_per_casualty)*casualties,
              total_cost = cost_per_collision*casualties) |> 
    group_by(osm_id, collision_year) |> 
    summarise(casualties = sum(casualties),
              casualty_cost = sum(casualty_cost),
              collision_cost = sum(collision_cost),
              total_cost = sum(total_cost))
  
  } else {
    
    cost_osm <- match_tag(cost_osm, match_with = "severity") |> 
      transmute(osm_id,
                collision_year,
                casualties,
                casualty_cost = cost_per_casualty*casualties,
                collision_cost = (cost_per_collision-cost_per_casualty)*casualties,
                total_cost = cost_per_collision*casualties) |> 
      group_by(osm_id) |> 
      summarise(casualties = sum(casualties),
                casualty_cost = sum(casualty_cost),
                collision_cost = sum(collision_cost),
                total_cost = sum(total_cost))
    
  }
  
  return(cost_osm)
  
}


# summarise the 
costs_col_per_LA <- function(crashes_sf, collision_severity = c("Fatal", "Serious", "Slight")){
  
  # crashes_sf <- crashes_gb
  # collision_severity = c("Fatal", "Serious", "Slight")
  
  crashes_tag_simple <- match_tag(crashes_sf,match_with = "severity")
  
  # import LA regions and remove Northern Ireland as there is no data for there
  cl <- st_read("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/995533eee7e44848bf4e663498634849/geoPackage?layers=0") |> 
    filter(!grepl("N", LAD22CD))
  
  # summarise by region
  cts_city <- crashes_tag_simple |> 
    #format_sf() |> 
    st_join(cl) |> 
    filter(collision_severity %in% collision_severity) |> 
    group_by(LAD22NM,collision_year) |> 
    summarise(collisions = n(),
              casualties = sum(as.numeric(number_of_casualties)),
              total_cost_col = sum(cost_per_collision-cost_per_casualty)/1e6,
              total_cost_cas = sum(cost_per_casualty)/1e6,
              total_cost = sum(cost_per_collision)/1e6) |> 
    ungroup() |> 
    group_by(collision_year) |> 
    mutate(annual_cost_rank = rank(-total_cost),
           annual_coll_rank = rank(-collisions),
           annual_cas_rank = rank(-casualties))
  
  # join to shape file
  cts_city_sf <- cts_city |> 
    st_set_geometry(NULL) |> 
    left_join(cl, by = "LAD22NM")
  
  # define geometry
  st_geometry(cts_city_sf) <- cts_city_sf$SHAPE
  
  return(cts_city_sf)
  
}

st_read_trycatch <- function(url, wait = 5) {
  i <- 1
  repeat {
    out <- tryCatch(st_read(url), error = function(e) NULL)
    if (!is.null(out)) {
      message("Success after ", i, " attempts")
      return(out)
    }
    message("Attempt ", i, " failed — waiting...")
    i <- i + 1
    Sys.sleep(wait)
  }
}

# summarise the 
casualties_per_LA <- function(casualties,crashes, casualty_types = "All", casualty_sexes = c("Male", "Female")){
  
  
  
  cl <- st_read_trycatch("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/995533eee7e44848bf4e663498634849/geoPackage?layers=0") |> 
  filter(!grepl("N", LAD22CD))
  

  if(!casualty_types == "All"){
    casualties <- filter(casualties, casualty_type %in% casualty_types)
  }
  
  # summarise by region
  cts_city <- crashes |> 
    st_join(cl) |> 
    inner_join(casualties) |> 
    filter(sex_of_casualty %in% casualty_sexes) |> 
    mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |> 
    st_set_geometry(NULL) |> 
    group_by(LAD22NM, collision_year) |> 
    summarise(collisions = n(),
              fatal_cas = sum(fatal_count),
              serious_cas = sum(casualty_adjusted_severity_serious),
              slight_cas = sum(casualty_adjusted_severity_slight)) |>
    mutate(ksi_cas = fatal_cas+serious_cas) |> 
    mutate(total_cas = fatal_cas+serious_cas+slight_cas) |> 
    ungroup() |> 
    group_by(collision_year) |> 
    mutate(fatal_rank = rank(-fatal_cas),
           serious_rank = rank(-serious_cas),
           slight_rank = rank(-slight_cas),
           ksi_rank = rank(-ksi_cas),
           total_rank = rank(-total_cas))
  
  # join to shape file
  cts_city_sf <- cts_city |> 
    #st_set_geometry(NULL) |> 
    left_join(cl, by = "LAD22NM")
  
  # define geometry
  st_geometry(cts_city_sf) <- cts_city_sf$SHAPE
  
  return(cts_city_sf)
  
}



#function to calculate the gradient of all the roads and group casualties by gradient
gradient_of_roads <- function(){NULL}

lsoa_summaries <- function(casualties = NULL, vehicles = NULL, collisions = NULL, lsoa_geo,
                           city_shp, base_year,end_year){
  
  if(!is.null(casualties)){
    groups_lsoa <- match_2021_lsoa(casualties = casualties) 
  }
  if(!is.null(vehicles)){
    groups_lsoa <- match_2021_lsoa(vehicles = vehicles) 
  }
  if(!is.null(collisions)){
    
    groups_lsoa <- crashes |> 
      st_transform(4326) |> 
      st_join(lsoa_geo) |> 
      st_set_geometry(NULL) |> 
      group_by(lsoa21_name) |> 
      summarise(crashes = n(),
                casualties = sum(as.numeric(number_of_casualties)),
                vehicles = sum(as.numeric(number_of_vehicles))) |> 
      left_join(lsoa_geo, by = "lsoa21_name")
    
    st_geometry(groups_lsoa) <- groups_lsoa$geometry
    
    return(groups_lsoa)
    
  } else {
  

  lsoa21_cent <- st_centroid(lsoa_geo) |> 
  st_transform(27700)

  lsoa21_city = lsoa21_cent[city_shp,]

  lsoa21_outside <- lsoa21_cent |> 
    filter(!lsoa21_code %in% lsoa21_city$lsoa21_code) |> 
    filter(lsoa21_name %in% groups_lsoa$lsoa21_name)

  lsoa21_outside$dist2city_km <- as.numeric(st_distance(city_shp, lsoa21_outside)[1,])/1000

  lsoa21_outside$distances <- cut(lsoa21_outside$dist2city_km, c(0,5,10,20,40,80,1000), c("0 - 5", "6 - 10","11 - 20", "20 - 40", "40 - 80", "81+"))

  st_geometry(lsoa21_outside) <- NULL
  
    groups_lsoa <- groups_lsoa |>  
      group_by(lsoa21_name) |> 
      summarise(persons = n()) |> 
      filter(!is.na(lsoa21_name)) |> 
      left_join(lsoa_geo, by = "lsoa21_name") |> 
      left_join(lsoa21_outside, by = "lsoa21_name")
  
  st_geometry(groups_lsoa) <- groups_lsoa$geometry
  
  return(groups_lsoa)
  
  }
  
}



