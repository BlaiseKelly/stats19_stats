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
#' @examples
#' \dontrun{
#' stats <- summarise_cycle_network("Bristol", bristol_sf, osm_data, city_pop = 472000)
#' }
#' @export
summarise_cycle_network <- function(city, city_shape, osm_data, city_pop) {
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



#' Summarise casualties per collision
#'
#' Aggregates casualty records to one row per collision, reducing double
#' counting and simplifying joins.
#'
#' @param casualties A data frame of casualty records.
#' @return A data frame with one row per collision and columns for Fatal,
#'   Serious, and Slight counts.
#' @examples
#' \dontrun{
#' cas_per_coll <- summarise_casualties_per_collision(casualties)
#' }
#' @export
summarise_casualties_per_collision <- function(casualties) {
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
#' @examples
#' \dontrun{
#' rates <- summarise_casualty_rates(casualties)
#' }
#' @export
summarise_casualty_rates <- function(casualties) {
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
#' @examples
#' \dontrun{
#' idx <- summarise_casualty_index(casualties, base_year = 2010, end_year = 2024)
#' }
#' @export
summarise_casualty_index <- function(casualties, base_year, end_year) {
  cas_rates <- summarise_casualty_rates(casualties)
  
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
#' @examples
#' \dontrun{
#' cas_grouped <- summarise_casualties_by_group(casualties, keep_index = FALSE)
#' }
#' @export
summarise_casualties_by_group <- function(casualties,
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
#' @examples
#' \dontrun{
#' demog <- summarise_casualties_by_demog(casualties,
#'                                        demographic = c("sex", "age"),
#'                                        severities = c("Fatal", "Serious"))
#' }
#' @export
summarise_casualties_by_demog <- function(casualties,
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
#' @examples
#' \dontrun{
#' cond <- summarise_crash_conditions(crashes, casualties,
#'                                    parameter = "speed_limit")
#' }
#' @export
summarise_crash_conditions <- function(crashes_df,
                                 casualties,
                                 city = la_name,
                                 severities = c("Fatal", "Serious", "Slight"),
                                 parameter = c("road_surface_conditions",
                                               "junction_detail",
                                               "speed_limit",
                                               "light_conditions",
                                               "weather_conditions")) {
  parameter <- match.arg(parameter)
  
  cas_summary <- summarise_casualties_per_collision(casualties)
  
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
#' @examples
#' \dontrun{
#' ksi_time <- summarise_ksi_by_time(crashes, casualties)
#' }
#' @export
summarise_ksi_by_time <- function(crashes, casualties) {
  
  cas_summary <- summarise_casualties_per_collision(casualties)
  
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
#' Joins crash records with ONS TAG cost data and aggregates to annual totals
#' by severity, or by severity and road type.
#'
#' @param crashes_df An \code{sf} data frame of crash records.
#' @param agg_level Character. One of \code{"severity"} or
#'   \code{"severity_roads"}.
#' @return A data frame with yearly cost totals.
#' @examples
#' \dontrun{
#' tag <- summarise_tag_costs(crashes, agg_level = "severity")
#' }
#' @export
summarise_tag_costs <- function(crashes_df, agg_level = c("severity","severity_roads")) {
  
  # use stats19 function to calculate cost per collision
  tag_df <- match_tag(crashes_df, match_with = agg_level)
  
  # summarise
  if(agg_level == "severity"){
  
  tag_sum = tag_df %>%
    dplyr::group_by(collision_year, collision_severity) %>%
    dplyr::summarise(
      total_casualties = round(sum(as.numeric(number_of_casualties)), 1),
      casualty_cost = round(sum(cost_per_casualty)),
      collision_cost = round(sum(cost_per_collision-cost_per_casualty)),
      total_cost = round(sum(cost_per_collision)))
  
  }
  
  if(agg_level == "severity_road"){
    
    tag_sum = tag_df %>%
      st_set_geometry(NULL) |> 
      dplyr::group_by(collision_year, collision_severity, ons_road) %>%
      dplyr::summarise(total_cost = round(sum(cost))) |> 
      tidyr::pivot_wider(names_from = c(ons_road), values_from = total_cost)
    
  }
  
  return(tag_sum)
  
}

#' Link casualties to an OSM street segment
#'
#' Buffers the supplied OSM street links, spatially selects crash records
#' within the buffer, and joins casualty records to produce a casualty-level
#' data frame tied to that street.
#'
#' @param osm_links An \code{sf} object of OSM street link(s).
#' @param casualties Casualty data frame.
#' @param crashes An \code{sf} data frame of crash records.
#' @param year_from Integer. Start year for filtering.
#' @param year_to Integer. End year for filtering.
#' @param sum_by_crash Logical. If \code{TRUE}, aggregate to one row per
#'   crash. Default \code{FALSE}.
#' @param casualties_buffer Numeric. Buffer distance in metres around the
#'   street links. Default \code{10}.
#' @return An \code{sf} data frame of casualties linked to the given street.
#' @examples
#' \dontrun{
#' cas_street <- summarise_casualty_osm_link(my_street, casualties, crashes,
#'                                           year_from = 2019, year_to = 2024)
#' }
#' @export
summarise_casualty_osm_link <- function(osm_links,
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

#' Link vehicles to an OSM street segment
#'
#' Buffers the supplied OSM street links, spatially selects crash records,
#' and joins vehicle records to produce a vehicle-level data frame for the
#' street. Optionally filters to collisions involving specific casualty types.
#'
#' @param osm_links An \code{sf} object of OSM street link(s).
#' @param casualties Casualty data frame (used for casualty-type filtering).
#' @param casualty_types Character. Casualty type to filter by, or
#'   \code{"All"}.
#' @param crashes An \code{sf} data frame of crash records.
#' @param vehicles Vehicle data frame.
#' @param year_from Integer. Start year.
#' @param year_to Integer. End year.
#' @param sum_by_crash Logical. If \code{TRUE}, aggregate per crash. Default
#'   \code{FALSE}.
#' @param casualties_buffer Numeric. Buffer distance in metres. Default
#'   \code{10}.
#' @return An \code{sf} data frame of vehicles linked to the street.
#' @examples
#' \dontrun{
#' veh_street <- summarise_vehicle_osm_link(my_street, casualties,
#'                                          casualty_types = "Cyclist",
#'                                          crashes, vehicles,
#'                                          year_from = 2019, year_to = 2024)
#' }
#' @export
summarise_vehicle_osm_link <- function(osm_links,
                             casualties,
                             casualty_types,
                             crashes,
                             vehicles,
                             year_from,
                             year_to,
                             sum_by_crash = FALSE,
                             casualties_buffer = 10){
  
  # simplify casualty type
  vehicle_type_sum <- summarise_vehicle_types(vehicles,summary_type = "short_name") 
  
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


#' Vehicle type grouping lookup table
#'
#' A data frame mapping the detailed STATS19 vehicle types to simplified
#' short names and driver-type labels.
#'
#' @format A data frame with 22 rows and 3 columns:
#'   \describe{
#'     \item{vehicle_type}{Original STATS19 vehicle type label}
#'     \item{short_name}{Simplified vehicle category (e.g. \code{"Car"},
#'       \code{"Motorcycle"})}
#'     \item{driver_type}{Driver/rider label (e.g. \code{"Car driver"},
#'       \code{"Motorcyclist"})}
#'   }
#' @examples
#' head(vehicle_groups)
vehicle_groups <- data.frame(vehicle_type = c("Car","Motorcycle 125cc and under","Taxi/Private hire car","Motorcycle over 500cc",               
                                              "Motorcycle over 125cc and up to 500cc","Goods 7.5 tonnes mgw and over","Goods over 3.5t. and under 7.5t","Bus or coach (17 or more pass seats)",
                                              "Van / Goods 3.5 tonnes mgw or under","Motorcycle 50cc and under","Other vehicle","Pedal cycle",                      
                                              "Motorcycle - unknown cc","Electric motorcycle","e-scooter","Minibus (8 - 16 passenger seats)",     
                                              "Mobility scooter","Unknown vehicle type (self rep only)","Agricultural vehicle","Goods vehicle - unknown weight",    
                                              "Data missing or out of range","Ridden horse"),
                             short_name =  c("Car","Motorcycle","Taxi","Motorcycle", "Motorcycle","Goods vehicle","Goods vehicle","Bus",
                                               "Goods vehicle","Motorcycle","Other vehicle","Pedal cycle","Motorcycle","Motorcycle","e-scooter","Bus",     
                                               "Mobility scooter","Other vehicle","Agricultural vehicle","Goods vehicle","Data missing or out of range","Ridden horse"),
                             driver_type =  c("Car driver","Motorcyclist","Taxi driver","Motorcyclist", "Motorcyclist","Goods vehicle driver","Goods vehicle driver","Bus driver",
                                               "Goods vehicle driver","Motorcyclist","Other vehicle","Cyclist","Motorcyclist","Motorcyclist","E-scooter driver","Bus driver",     
                                               "Mobility scooter rider","Other vehicle","Agricultural vehicle driver","Goods vehicle driver","Data missing","Horse rider"))

#' Simplify vehicle types using grouping lookup
#'
#' Joins vehicle records to the \code{vehicle_groups} lookup table and
#' replaces the detailed \code{vehicle_type} column with a simplified
#' category.
#'
#' @param vehicles A data frame of vehicle records with a
#'   \code{vehicle_type} column.
#' @param summary_type Character. One of \code{"short_name"} or
#'   \code{"driver_type"}. Controls which grouping level to apply.
#' @return The vehicle data frame with \code{vehicle_type} replaced by the
#'   chosen summary category.
#' @examples
#' \dontrun{
#' veh_simple <- summarise_vehicle_types(vehicles, summary_type = "short_name")
#' }
#' @export
summarise_vehicle_types <- function(vehicles, summary_type = c("short_name", "driver_type")){
  
  vehicle_groups <- data.frame(vehicle_type = c("Car","Motorcycle 125cc and under","Taxi/Private hire car","Motorcycle over 500cc",               
                                                "Motorcycle over 125cc and up to 500cc","Goods 7.5 tonnes mgw and over","Goods over 3.5t. and under 7.5t","Bus or coach (17 or more pass seats)",
                                                "Van / Goods 3.5 tonnes mgw or under","Motorcycle 50cc and under","Other vehicle","Pedal cycle",                      
                                                "Motorcycle - unknown cc","Electric motorcycle","e-scooter","Minibus (8 - 16 passenger seats)",     
                                                "Mobility scooter","Unknown vehicle type (self rep only)","Agricultural vehicle","Goods vehicle - unknown weight",    
                                                "Data missing or out of range","Ridden horse"),
                               short_name =  c("Car","Motorcycle","Taxi","Motorcycle", "Motorcycle","Goods vehicle","Goods vehicle","Bus",
                                               "Goods vehicle","Motorcycle","Other vehicle","Pedal cycle","Motorcycle","Motorcycle","e-scooter","Bus",     
                                               "Mobility scooter","Other vehicle","Agricultural vehicle","Goods vehicle","Data missing or out of range","Ridden horse"),
                               driver_type =  c("Car driver","Motorcyclist","Taxi driver","Motorcyclist", "Motorcyclist","Goods vehicle driver","Goods vehicle driver","Bus driver",
                                                "Goods vehicle driver","Motorcyclist","Other vehicle","Cyclist","Motorcyclist","Motorcyclist","E-scooter driver","Bus driver",     
                                                "Mobility scooter rider","Other vehicle","Agricultural vehicle driver","Goods vehicle driver","Data missing","Horse rider"))
  
  veh_new_types <- vehicles |>
    left_join(vehicle_groups, by = "vehicle_type") |> 
    mutate(vehicle_type = !!sym(summary_type))
  
  return(veh_new_types)
  
}

#' Simplify casualty types
#'
#' Maps detailed casualty types to shorter names or categories.
#'
#' @param casualties Casualty data frame.
#' @param summary_type Character. One of `"short_name"` or `"in_or_on"`.
#' @return Casualty data frame with simplified type column.
#' @examples
#' \dontrun{
#' cas_short <- summarise_casualty_types(casualties, summary_type = "short_name")
#' }
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
  return(cas_out)
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
#' @examples
#' \dontrun{
#' veh_wide <- summarise_vehicles_per_collision(vehicles)
#' }
#' @export
summarise_vehicles_per_collision <- function(vehicles, summarise_categories = TRUE) {
  
  vehicle_groups <- data.frame(vehicle_type = c("Car","Motorcycle 125cc and under","Taxi/Private hire car","Motorcycle over 500cc",               
                                                "Motorcycle over 125cc and up to 500cc","Goods 7.5 tonnes mgw and over","Goods over 3.5t. and under 7.5t","Bus or coach (17 or more pass seats)",
                                                "Van / Goods 3.5 tonnes mgw or under","Motorcycle 50cc and under","Other vehicle","Pedal cycle",                      
                                                "Motorcycle - unknown cc","Electric motorcycle","e-scooter","Minibus (8 - 16 passenger seats)",     
                                                "Mobility scooter","Unknown vehicle type (self rep only)","Agricultural vehicle","Goods vehicle - unknown weight",    
                                                "Data missing or out of range","Ridden horse"),
                               short_name =  c("Car","Motorcycle","Taxi","Motorcycle", "Motorcycle","Goods vehicle","Goods vehicle","Bus",
                                               "Goods vehicle","Motorcycle","Other vehicle","Pedal cycle","Motorcycle","Motorcycle","e-scooter","Bus",     
                                               "Mobility scooter","Other vehicle","Agricultural vehicle","Goods vehicle","Data missing or out of range","Ridden horse"),
                               driver_type =  c("Car driver","Motorcyclist","Taxi driver","Motorcyclist", "Motorcyclist","Goods vehicle driver","Goods vehicle driver","Bus driver",
                                                "Goods vehicle driver","Motorcyclist","Other vehicle","Cyclist","Motorcyclist","Motorcyclist","E-scooter driver","Bus driver",     
                                                "Mobility scooter rider","Other vehicle","Agricultural vehicle driver","Goods vehicle driver","Data missing","Horse rider"))
  
  if (summarise_categories) {
    vehicles <- dplyr::left_join(vehicles, vehicle_groups, by = "vehicle_type") |> 
      dplyr::transmute(collision_index, vehicle_type = short_name)
  } else {
    vehicles <- dplyr::transmute(vehicles, collision_index, vehicle_type)
  }
  
  veh_summary <- vehicles %>%
    dplyr::group_by(collision_index, vehicle_type) %>%
    dplyr::mutate(number_vehicles = 1) %>%
    dplyr::summarise(number_vehicles = sum(number_vehicles), .groups = "drop") |> 
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
#' @return The crash \code{sf} object with an added \code{h3_index} column.
#' @examples
#' \dontrun{
#' crashes_hex <- summarise_crashes_to_h3(city_sf = bristol_sf,
#'                                        hex_res = 8,
#'                                        crashes_df = crashes)
#' }
#' @export
summarise_crashes_to_h3 <- function(city_sf,
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

#' Summarise casualty statistics per OSM road link
#'
#' Matches crashes to OSM road segments and aggregates casualty counts,
#' mean demographics, and speed limits per link. Results can be grouped by
#' casualty type, year, or as a grand total, and optionally ranked.
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param casualties Casualty data frame.
#' @param ranking Logical. If \code{TRUE}, add rank columns for each severity.
#'   Default \code{TRUE}.
#' @param osm_data An \code{sf} object of OSM road network segments.
#' @param group Character. One of \code{"casualty_type"}, \code{"year"}, or
#'   \code{"total"}.
#' @return An \code{sf} data frame with one row per OSM link (per group) and
#'   columns for collision counts, severity totals, and optionally rankings.
#' @examples
#' \dontrun{
#' link_cas <- summarise_osm_link_casualties(crashes, casualties,
#'                                           osm_data = drive_net,
#'                                           group = "casualty_type")
#' }
#' @export
summarise_osm_link_casualties = function(crashes,casualties, ranking= TRUE, osm_data,group = c("casualty_type", "year", "total")){

osm_geo = dplyr::select(osm_data,osm_id, name,geometry)

if(group == "casualty_type"){

cas_sum = summarise_casualty_types(casualties,summary_type = "short_name") |> 
  dplyr::mutate(casualty_type = short_name)

group_cols = c("osm_id", "casualty_type")

cas_osm = match_crashes_to_osm(osm_network_sf = osm_data, crash_sf = crashes) |> 
  st_set_geometry(NULL) |> 
  dplyr::left_join(cas_sum, by = "collision_index") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(sex_of_casualty = ifelse(sex_of_casualty == "Male",1,0)) |> 
  dplyr::group_by(across(all_of(group_cols))) %>%
  dplyr::summarise(number_of_collisions = NROW(unique(collision_index)),
            age_of_casualty = mean(age_of_casualty),
            sex_of_casualty = mean(sex_of_casualty),
            speed_limit = mean(as.numeric(speed_limit), na.rm = TRUE),
            fatal = sum(fatal_count),
            serious = sum(casualty_adjusted_severity_serious),
            ksi = sum(fatal_count,casualty_adjusted_severity_serious),
            slight = sum(casualty_adjusted_severity_slight),
            total = sum(fatal,serious,slight)) |> 
  dplyr::left_join(osm_geo, by = "osm_id") |> 
  dplyr::filter(!casualty_type %in% c("Other vehicle", "Data missing"))

}

if(group == "year"){
  
  cas_sum = casualties |> 
    dplyr::mutate(year = collision_year)
  
  group_cols = c("osm_id","year")

cas_osm = match_crashes_to_osm(osm_network_sf = osm_data, crash_sf = crashes) |> 
  st_set_geometry(NULL) |> 
  dplyr::left_join(cas_sum, by = "collision_index") |> 
  ungroup() |> 
  mutate(sex_of_casualty = ifelse(sex_of_casualty == "Male",1,0)) |> 
  group_by(across(all_of(group_cols))) %>%
  summarise(number_of_collisions = NROW(unique(collision_index)),
            age_of_casualty = mean(age_of_casualty),
            sex_of_casualty = mean(sex_of_casualty),
            speed_limit = mean(as.numeric(speed_limit), na.rm = TRUE),
            fatal = sum(fatal_count),
            serious = sum(casualty_adjusted_severity_serious),
            ksi = sum(fatal_count,casualty_adjusted_severity_serious),
            slight = sum(casualty_adjusted_severity_slight),
            total = sum(fatal,serious,slight)) |> 
  left_join(osm_geo, by = "osm_id")

}

if(group == "total"){
  
  cas_osm = match_crashes_to_osm(osm_network_sf = osm_data, crash_sf = crashes) |> 
    st_set_geometry(NULL) |> 
    dplyr::left_join(casualties, by = "collision_index") |> 
    ungroup() |> 
    mutate(sex_of_casualty = ifelse(sex_of_casualty == "Male",1,0)) |> 
    group_by(osm_id) |> 
    summarise(number_of_collisions = NROW(unique(collision_index)),
              age_of_casualty = mean(age_of_casualty),
              sex_of_casualty = mean(sex_of_casualty),
              speed_limit = mean(as.numeric(speed_limit), na.rm = TRUE),
              fatal = sum(fatal_count),
              serious = sum(casualty_adjusted_severity_serious),
              ksi = sum(fatal_count,casualty_adjusted_severity_serious),
              slight = sum(casualty_adjusted_severity_slight),
              total = sum(fatal,serious,slight)) |> 
    left_join(osm_geo, by = "osm_id") 
  
}

if(isTRUE(ranking)){
  
  cas_osm = cas_osm |> 
    mutate(collisions_rank = min_rank(desc(number_of_collisions)),
           fatal_rank = min_rank(desc(fatal)),
              serious_rank = min_rank(desc(serious)),
              ksi_rank = min_rank(desc(ksi)),
              slight_rank = min_rank(desc(slight)),
              total_rank = min_rank(desc(total)))
  
}

st_geometry(cas_osm) = cas_osm$geometry

return(cas_osm)

}

#' Summarise TAG costs per OSM road link
#'
#' Matches crashes to OSM road segments, joins TAG cost data, and aggregates
#' casualty and collision costs per link. Optionally groups by year.
#'
#' @param osm_data An \code{sf} object of OSM road network segments.
#' @param crash_sf An \code{sf} data frame of crash records.
#' @param casualties Casualty data frame.
#' @param match_with Character. TAG matching method passed to
#'   \code{match_tag()}. Default \code{"severity"}.
#' @param by_year Logical. If \code{TRUE}, group results by collision year.
#'   Default \code{TRUE}.
#' @return A data frame with columns \code{osm_id}, \code{casualties},
#'   \code{casualty_cost}, \code{collision_cost}, and \code{total_cost}
#'   (optionally by \code{collision_year}).
#' @examples
#' \dontrun{
#' link_costs <- summarise_osm_link_costs(drive_net, crashes, casualties)
#' }
#' @export
summarise_osm_link_costs = function(osm_data, crash_sf, casualties, match_with = "severity", by_year = TRUE){
  
  osm_data = drive_net
  crash_sf = crashes
  match_with = "severity"
  by_year = TRUE
  
  cas_osm = match_crashes_to_osm(osm_network_sf = osm_data, crash_sf = crash_sf) |> 
    dplyr::select(collision_index,osm_id) |> 
    dplyr::st_set_geometry(NULL) |> 
    left_join(casualties, by = "collision_index") 
  
  cost_osm <- cas_osm |> 
    select(-collision_index) |> 
    tidyr::pivot_longer(-c("collision_year","osm_id"))
  
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


#' Summarise TAG collision costs per Local Authority
#'
#' Matches crashes to TAG cost data, spatially joins to Local Authority
#' boundaries, and aggregates collision counts, casualties, and costs per LA
#' per year. Adds annual cost, collision, and casualty rankings.
#'
#' @param crashes_sf An \code{sf} data frame of crash records for Great
#'   Britain.
#' @param collision_severity Character vector of severity levels to include.
#'   Default \code{c("Fatal", "Serious", "Slight")}.
#' @return An \code{sf} data frame with one row per LA per year and columns
#'   for collisions, casualties, costs, and annual rankings.
#' @examples
#' \dontrun{
#' la_costs <- summarise_costs_per_la(crashes_gb)
#' }
#' @export
summarise_costs_per_la <- function(crashes_sf, collision_severity = c("Fatal", "Serious", "Slight")){
  
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

#' Read spatial data with retry on failure
#'
#' Wraps \code{sf::st_read()} in a retry loop, useful for unreliable remote
#' servers (e.g. the ONS Open Geography Portal). Retries indefinitely with
#' a configurable wait between attempts.
#'
#' @param url Character. URL or file path to read.
#' @param wait Numeric. Seconds to wait between retry attempts. Default
#'   \code{5}.
#' @return An \code{sf} object returned by \code{st_read()}.
#' @examples
#' \dontrun{
#' la_sf <- st_read_retry("https://example.com/boundaries.gpkg")
#' }
#' @export
st_read_retry <- function(url, wait = 5) {
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


#' Summarise casualties per Local Authority
#'
#' Spatially joins crashes to Local Authority boundaries and aggregates
#' casualty counts by severity, year, and LA. Adds national rankings and
#' optionally computes per-capita rates.
#'
#' @param casualties Casualty data frame.
#' @param crashes An \code{sf} data frame of crash records.
#' @param per_capita Logical. If \code{TRUE}, compute per-1000-population
#'   rates using ONS mid-year estimates. Default \code{FALSE}.
#' @param casualty_types Character. Casualty type filter, or \code{"All"} to
#'   include all types. Default \code{"All"}.
#' @param casualty_sexes Character vector of sexes to include. Default
#'   \code{c("Male", "Female")}.
#' @return An \code{sf} data frame with one row per LA per year and columns
#'   for casualty counts, rankings, and optionally per-capita rates.
#' @examples
#' \dontrun{
#' la_cas <- summarise_casualties_per_la(casualties, crashes)
#' la_cyc <- summarise_casualties_per_la(casualties, crashes,
#'                              casualty_types = "Cyclist", per_capita = TRUE)
#' }
#' @export
summarise_casualties_per_la <- function(casualties,crashes,la_geo, per_capita = FALSE, casualty_types = "All", casualty_sexes = c("Male", "Female")){
  
  cl <- la_geo |> 
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
    group_by(LAD22CD, LAD22NM, collision_year) |> 
    summarise(collisions = n_distinct(collision_index),
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
  
  if(isTRUE(per_capita)){
    
    LA_pop = st_read_retry("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/LA_pop_tot_2011_2024.csv") |> 
      dplyr::select(-LADNM) |> 
      dplyr::mutate(across(-LADCD, as.numeric)) |> 
      tidyr::pivot_longer(-LADCD, names_to = "collision_year", values_to = "pop") |> 
      mutate(collision_year = gsub("Total_","", collision_year))
    
    cts_city = cts_city |> 
      mutate(collision_year = as.character(collision_year)) |> 
      left_join(LA_pop, by = c("LAD22CD" = "LADCD","collision_year")) |> 
      mutate(collisions_pcap = (collisions/pop)*1000,
                fatal_pcap = (fatal_cas/pop)*1000,
                serious_pcap = (serious_cas/pop)*1000,
                slight_pcap = (slight_cas/pop)*1000,
                ksi_pcap = (ksi_cas/pop)*1000,
                total_pcap = (total_cas/pop)*1000) |> 
      ungroup() |> 
      group_by(collision_year) |> 
      mutate(fatal_pcap_rank = rank(-fatal_pcap),
             serious_pcap_rank = rank(-serious_pcap),
             slight_pcap_rank = rank(-slight_pcap),
             ksi_pcap_rank = rank(-ksi_pcap),
             total_pcap_rank = rank(-total_pcap))
  }
  
  # join to shape file
  cts_city_sf <- cts_city |> 
    #st_set_geometry(NULL) |> 
    left_join(cl, by = "LAD22NM")
  
  # define geometry
  st_geometry(cts_city_sf) <- cts_city_sf$geom
  
  return(cts_city_sf)
  
}

#' Summarise collisions per MSOA
#'
#' Spatially joins crashes to MSOA boundaries and aggregates casualty counts
#' by severity, year, and MSOA. Adds rankings and optionally computes
#' per-capita rates. Returns an \code{sf} object with MSOA names from the
#' House of Commons Library MSOA Names dataset.
#'
#' @param casualties Casualty data frame.
#' @param crashes An \code{sf} data frame of crash records.
#' @param per_capita Logical. If \code{TRUE}, compute per-1000-population
#'   rates. Default \code{FALSE}.
#' @param by_year Logical. If \code{TRUE}, group by year; otherwise collapse
#'   all years to \code{"All"}. Default \code{TRUE}.
#' @param casualty_types Character. Casualty type filter, or \code{"All"}.
#'   Default \code{"All"}.
#' @param casualty_sexes Character vector of sexes to include. Default
#'   \code{c("Male", "Female")}.
#' @return An \code{sf} data frame with one row per MSOA (per year) and
#'   columns for casualty counts, rankings, and optionally per-capita rates.
#' @examples
#' \dontrun{
#' msoa_coll <- summarise_collisions_per_msoa(casualties, crashes)
#' msoa_cyc  <- summarise_collisions_per_msoa(casualties, crashes,
#'                                            casualty_types = "Cyclist",
#'                                            per_capita = TRUE)
#' }
#' @export
summarise_collisions_per_msoa <- function(casualties,crashes, per_capita = FALSE,by_year = TRUE, casualty_types = "All", casualty_sexes = c("Male", "Female")){
  
  msoa_geo = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/msoa_boundaries-v1.0/msoa.gpkg") |> 
    st_transform(27700) |> 
    mutate(MSOA21CD,geom)
  
  msoa_names = read.csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv") |> 
    select(msoa21cd,msoa21hclnm,localauthorityname)
  
  lsoa_geo = get_lsoa21_boundaries(provider = "geographr") |> 
    st_transform(27700)
  
  lsoa_cent = st_centroid(lsoa_geo)
  
  msoa_lsoa = st_join(msoa_geo,lsoa_cent) |> 
    select(MSOA21CD,lsoa21_code)
  
  if(!casualty_types == "All"){
    casualties <- filter(casualties, casualty_type %in% casualty_types)
  }
  
  # summarise by region
  cts_city <- crashes |> 
    st_join(msoa_geo) |> 
    inner_join(casualties) |> 
    filter(sex_of_casualty %in% casualty_sexes) |> 
    mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |> 
    st_set_geometry(NULL) |> 
    filter(!is.na(MSOA21CD)) |> 
    mutate(collision_year = ifelse(isTRUE(by_year), collision_year,"All")) |> 
    group_by(MSOA21CD,collision_year) |> 
    summarise(collisions = n_distinct(collision_index),
              fatal_cas = sum(fatal_count),
              serious_cas = sum(casualty_adjusted_severity_serious),
              slight_cas = sum(casualty_adjusted_severity_slight)) |>
    mutate(ksi_cas = fatal_cas+serious_cas) |> 
    mutate(total_cas = fatal_cas+serious_cas+slight_cas) |> 
    ungroup() |> 
    group_by(collision_year) |> 
    mutate(collision_rank = min_rank(desc(collisions)),
           fatal_rank = min_rank(desc(fatal_cas)),
           serious_rank = min_rank(desc(serious_cas)),
           slight_rank = min_rank(desc(slight_cas)),
           ksi_rank = min_rank(desc(ksi_cas)),
           total_rank = min_rank(desc(total_cas)))|> 
    select(MSOA21CD,
           collision_year,
           collisions,
           fatal_cas,
           serious_cas,
           slight_cas,
           ksi_cas,
           total_cas,
           collision_rank,
           fatal_rank,
           serious_rank,
           slight_rank,
           ksi_rank,
           total_rank)

  if(isTRUE(per_capita)){
    
    # import lsoa population and match and sum with MSOA
    msoa_pop = read.csv("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/lsoa21_pop_tot_2011_2024.csv") |> 
      dplyr::select(-lsoa21nm,-X) |> 
      dplyr::mutate(across(-lsoa21cd, as.numeric)) |> 
      tidyr::pivot_longer(-lsoa21cd, names_to = "collision_year", values_to = "pop") |> 
      mutate(collision_year = gsub("X","", collision_year)) |> 
      left_join(msoa_lsoa, by = c("lsoa21cd" = "lsoa21_code")) |> 
      select(-geom) |> 
      group_by(MSOA21CD, collision_year) |> 
      summarise(pop = sum(pop))
    
    if(isFALSE(by_year)){
      msoa_pop = msoa_pop |> 
        filter(collision_year >= min(casualties$collision_year) & collision_year <= max(casualties$collision_year)) |> 
        group_by(MSOA21CD) |> 
        summarise(pop = mean(pop)) |> 
        mutate(collision_year = "All")
    }
    
    cts_city = cts_city |> 
      mutate(collision_year = as.character(collision_year)) |> 
      left_join(msoa_pop, by = c("MSOA21CD","collision_year")) |> 
      mutate(collisions_pcap = (collisions/pop)*1000,
             fatal_pcap = (fatal_cas/pop)*1000,
             serious_pcap = (serious_cas/pop)*1000,
             slight_pcap = (slight_cas/pop)*1000,
             ksi_pcap = (ksi_cas/pop)*1000,
             total_pcap = (total_cas/pop)*1000) |> 
      ungroup() |> 
      group_by(collision_year) |> 
      mutate(collisions_pcap_rank = min_rank(desc(collisions_pcap)),
             fatal_pcap_rank = min_rank(desc(fatal_pcap)),
             serious_pcap_rank = min_rank(desc(serious_pcap)),
             slight_pcap_rank = min_rank(desc(slight_pcap)),
             ksi_pcap_rank = min_rank(desc(ksi_pcap)),
             total_pcap_rank = min_rank(desc(total_pcap)))
  }
  
  # join to shape file and meaningful msoa names
  cts_city_sf <- cts_city |> 
    left_join(msoa_names, by = c("MSOA21CD" = "msoa21cd")) |> 
    #st_set_geometry(NULL) |> 
    left_join(msoa_geo, by = "MSOA21CD") 
  
  # define geometry
  st_geometry(cts_city_sf) <- cts_city_sf$geom
  
  return(cts_city_sf)
  
}

#' Summarise casualties per MSOA by home location
#'
#' Matches casualties to MSOAs via their \code{lsoa_of_casualty} home
#' location, aggregates by severity, casualty type, and year, and adds
#' rankings. Optionally computes per-capita rates.
#'
#' @param casualties Casualty data frame with \code{lsoa_of_casualty}.
#' @param per_capita Logical. If \code{TRUE}, compute per-1000-population
#'   rates. Default \code{FALSE}.
#' @param by_year Logical. If \code{TRUE}, group by year; otherwise collapse
#'   all years. Default \code{TRUE}.
#' @param by_casualty Logical. If \code{TRUE}, group by casualty type;
#'   otherwise collapse to \code{"All"}. Default \code{TRUE}.
#' @param casualty_sexes Character vector of sexes to include. Default
#'   \code{c("Male", "Female")}.
#' @return An \code{sf} data frame with one row per MSOA (per year, per
#'   casualty type) and severity counts, rankings, and optionally per-capita
#'   rates.
#' @examples
#' \dontrun{
#' msoa_cas <- casualties_per_MSOA(casualties)
#' msoa_cas_yr <- casualties_per_MSOA(casualties, per_capita = TRUE,
#'                                    by_year = TRUE)
#' }
#' @export
casualties_per_MSOA <- function(casualties, per_capita = FALSE, by_year = TRUE, by_casualty = TRUE, casualty_sexes = c("Male", "Female")){
  
  msoa_geo = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/msoa_boundaries-v1.0/msoa.gpkg") |> 
    st_transform(27700) |> 
    mutate(MSOA21CD,geom)
# also try https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest2.csv static link
#  https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.3.csv link given by right clicking download on website
  msoa_names = readr::read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest2.csv") |> 
    select(msoa21cd,msoa21hclnm,localauthorityname)
  
  lsoa_geo = get_lsoa21_boundaries(provider = "geographr") |> 
    st_transform(27700)
  
  lsoa_cent = st_centroid(lsoa_geo)
  
  msoa_lsoa = st_join(msoa_geo,lsoa_cent) |> 
    select(MSOA21CD,lsoa21_code)

    # summarise by region
    cts_city <- casualties_simp |> 
      left_join(msoa_lsoa, by = c("lsoa_of_casualty" = "lsoa21_code")) |> 
      group_by(MSOA21CD,casualty_type,sex_of_casualty) |> 
      filter(sex_of_casualty %in% casualty_sexes) |> 
      mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |> 
      filter(!is.na(MSOA21CD)) |> 
      mutate(collision_year = ifelse(isTRUE(by_year), collision_year,"All")) |> 
      mutate(casualty_type = ifelse(isTRUE(by_casualty), casualty_type,"All")) |> 
      group_by(MSOA21CD,casualty_type,collision_year) |> 
      summarise(mean_age = mean(age_of_casualty,na.rm = TRUE),
                fatal_cas = sum(fatal_count,na.rm = TRUE),
                serious_cas = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
                slight_cas = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |> 
      mutate(ksi_cas = fatal_cas+serious_cas) |> 
      mutate(total_cas = fatal_cas+serious_cas+slight_cas) |>
        ungroup() |> 
      group_by(collision_year) |> 
      mutate(fatal_rank = min_rank(desc(fatal_cas)),
             serious_rank = min_rank(desc(serious_cas)),
             slight_rank = min_rank(desc(slight_cas)),
             ksi_rank = min_rank(desc(ksi_cas)),
             total_rank = min_rank(desc(total_cas))) |> 
      select(MSOA21CD,
             collision_year,
             casualty_type,
             mean_age,
             fatal_cas,
             serious_cas,
             slight_cas,
             ksi_cas,
             total_cas,
             fatal_rank,
             serious_rank,
             slight_rank,
             ksi_rank,
             total_rank)
    
  
  if(isTRUE(per_capita)){
    
    # import lsoa population and match and sum with MSOA
    msoa_pop = read.csv("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/lsoa21_pop_tot_2011_2024.csv") |> 
      dplyr::select(-lsoa21nm,-X) |> 
      dplyr::mutate(across(-lsoa21cd, as.numeric)) |> 
      tidyr::pivot_longer(-lsoa21cd, names_to = "collision_year", values_to = "pop") |> 
      mutate(collision_year = gsub("X","", collision_year)) |> 
      left_join(msoa_lsoa, by = c("lsoa21cd" = "lsoa21_code")) |> 
      select(-geom) |> 
      group_by(MSOA21CD, collision_year) |> 
      summarise(pop = sum(pop))
    
    if(isFALSE(by_year)){
      msoa_pop = msoa_pop |> 
        filter(collision_year >= min(casualties$collision_year) & collision_year <= max(casualties$collision_year)) |> 
        group_by(MSOA21CD) |> 
        summarise(pop = mean(pop)) |> 
        mutate(collision_year = "All")
    }
    
    cts_city = cts_city |> 
      mutate(collision_year = as.character(collision_year)) |> 
      left_join(msoa_pop, by = c("MSOA21CD","collision_year")) |> 
      mutate(fatal_pcap = (fatal_cas/pop)*1000,
             serious_pcap = (serious_cas/pop)*1000,
             slight_pcap = (slight_cas/pop)*1000,
             ksi_pcap = (ksi_cas/pop)*1000,
             total_pcap = (total_cas/pop)*1000) |> 
      ungroup() |> 
      group_by(collision_year) |> 
      mutate(fatal_pcap_rank = min_rank(desc(fatal_pcap)),
             serious_pcap_rank = min_rank(desc(serious_pcap)),
             slight_pcap_rank = min_rank(desc(slight_pcap)),
             ksi_pcap_rank = min_rank(desc(ksi_pcap)),
             total_pcap_rank = min_rank(desc(total_pcap)))
  }
  
  # join to shape file and meaningful msoa names
  cts_city_sf <- cts_city |> 
    left_join(msoa_names, by = c("MSOA21CD" = "msoa21cd")) |> 
    #st_set_geometry(NULL) |> 
    left_join(msoa_geo, by = "MSOA21CD") 
  
  # define geometry
  st_geometry(cts_city_sf) <- cts_city_sf$geom
  
  return(cts_city_sf)
  
}



#' Calculate road gradients and group casualties by gradient
#'
#' Placeholder function. Will calculate the gradient of all roads in a study
#' area and group casualty statistics by gradient category.
#'
#' @return Currently returns \code{NULL}.
#' @examples
#' \dontrun{
#' grad <- summarise_road_gradients()
#' }
#' @export
summarise_road_gradients <- function(){NULL}

#' Summarise casualty or collision data at LSOA level
#'
#' Matches casualty or vehicle records to their 2021 LSOA (via home location)
#' or spatially joins collisions to LSOA boundaries. Counts persons per LSOA
#' and classifies LSOAs outside the city boundary by distance band.
#'
#' @param casualties Optional casualty data frame. Supply one of
#'   \code{casualties}, \code{vehicles}, or \code{collisions}.
#' @param vehicles Optional vehicle data frame.
#' @param collisions Optional crash \code{sf} data frame.
#' @param lsoa_geo An \code{sf} data frame of LSOA 2021 boundaries.
#' @param city_shp An \code{sf} polygon of the city boundary (BNG CRS).
#' @param base_year Integer. Start year (currently unused filter).
#' @param end_year Integer. End year (currently unused filter).
#' @return An \code{sf} data frame with one row per LSOA and person counts.
#' @examples
#' \dontrun{
#' lsoa_cas <- summarise_lsoa(casualties = cas, lsoa_geo = lsoa_sf,
#'                             city_shp = city_sf, base_year = 2019,
#'                             end_year = 2024)
#' }
#' @export
summarise_lsoa <- function(casualties = NULL, vehicles = NULL, collisions = NULL, lsoa_geo,
                           city_shp, base_year,end_year){
  
  city_shp = st_transform(city_shp,4326)
  
  if ("LSOA21CD" %in% names(lsoa_geo)) {
    lsoa_geo = lsoa_geo |> 
      rename(lsoa21_code = LSOA21CD) |> 
      rename(lsoa21_name = LSOA21NM)
  }
  
  if ("geom" %in% names(lsoa_geo)) {
    lsoa_geo = lsoa_geo %>% rename(geometry = geom)
  }
  
  if(!is.null(casualties)){
    groups_lsoa <- match_lsoa_2021(casualties = casualties) 
  }
  if(!is.null(vehicles)){
    groups_lsoa <- match_lsoa_2021(vehicles = vehicles) 
  }
  if(!is.null(collisions)){
    
    groups_lsoa <- collisions |> 
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
  st_transform(4326)

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
      left_join(lsoa21_outside, by = "lsoa21_name") |>
      filter(!is.na(lsoa21_code.x))
  
  st_geometry(groups_lsoa) <- groups_lsoa$geometry
  
  return(groups_lsoa)
  
  }
  
}

#' Summarise casualty or collision data at MSOA level
#'
#' Matches casualty or vehicle records to MSOAs using LSOA centroids and MSOA
#' boundaries. Retrieves descriptive MSOA names from the House of Commons
#' Library dataset. Counts persons per MSOA and classifies MSOAs outside the
#' city boundary by distance band.
#'
#' @param casualties Optional casualty data frame. Supply one of
#'   \code{casualties}, \code{vehicles}, or \code{collisions}.
#' @param vehicles Optional vehicle data frame.
#' @param collisions Optional crash \code{sf} data frame.
#' @param lsoa_geo An \code{sf} data frame of LSOA 2021 boundaries.
#' @param city_shp An \code{sf} polygon of the city boundary (BNG CRS).
#' @param base_year Integer. Start year (currently unused filter).
#' @param end_year Integer. End year (currently unused filter).
#' @return An \code{sf} data frame with one row per MSOA and person counts.
#' @examples
#' \dontrun{
#' msoa_cas <- summarise_msoa(casualties = cas, lsoa_geo = lsoa_sf,
#'                             city_shp = city_sf, base_year = 2019,
#'                             end_year = 2024)
#' }
#' @export
summarise_msoa <- function(casualties = NULL, vehicles = NULL, collisions = NULL, lsoa_geo,
                           city_shp, base_year,end_year){
  
  msoa_names = read.csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv")
  
  lsoa_city_cent_msoa_nm = st_join(lsoa_city_centroids, msoa_boundaries_21) |> 
    left_join(msoa_names, by = c("msoa21_code" = "msoa21cd")) |> 
    select(lsoa21_code, msoa21hclnm, geometry)
  
  casualties_msoa = casualties |> left_join(lsoa_city_cent_msoa_nm, by = c("lsoa_of_casualty" = "lsoa21_code")) |> 
    mutate(msoa21hclnm = ifelse(is.na(msoa21hclnm),"missing",msoa21hclnm)) |> 
    group_by(msoa21hclnm) |> 
    summarise(Fatal = sum(fatal_count),
              Serious = sum(casualty_adjusted_severity_serious),
              Slight = sum(casualty_adjusted_severity_slight))
  
  if(!is.null(casualties)){
    groups_lsoa <- match_lsoa_2021(casualties = casualties) 
  }
  if(!is.null(vehicles)){
    groups_lsoa <- match_lsoa_2021(vehicles = vehicles) 
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

#' Summarise Casualties on Pavements by Vehicle Type
#'
#' Identifies collisions where a pedestrian was struck on a footway or verge
#' by a single vehicle, then summarises the resulting killed or seriously injured
#' (KSI) casualties by vehicle type. This highlights which vehicle types are
#' most responsible for pavement encroachment collisions causing serious harm.
#'
#' @param crashes A spatial data frame (\code{sf}) of STATS19 collision records,
#'   expected to contain \code{collision_index}, \code{number_of_vehicles}, and
#'   \code{collision_year}.
#' @param casualties A data frame of STATS19 casualty records, expected to
#'   contain \code{collision_index} and \code{pedestrian_location}.
#' @param vehicles A data frame of STATS19 vehicle records, expected to contain
#'   \code{collision_index} and \code{vehicle_type}.
#' @param base_year Integer. The earliest year to include in the analysis
#'   (inclusive).
#' @param upper_year Integer. The latest year to include. Note: not currently
#'   used in the filter — see Details.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{vehicle_type}{The type of vehicle involved in the single-vehicle
#'     pavement collision.}
#'   \item{KSI}{The total number of killed or seriously injured casualties
#'     for that vehicle type.}
#' }
#' Rows with zero KSI are excluded.
#'
#' @details
#' The function filters to single-vehicle collisions (\code{number_of_vehicles
#' == 1}) where the pedestrian location is recorded as \emph{"On footway or
#' verge"}. Casualty severities are aggregated per collision via
#' \code{\link{summarise_casualties_per_collision}} before joining to vehicle
#' records.
#'
#' \strong{Note:} The \code{upper_year} parameter is accepted but not currently
#' applied in the year filter. Consider adding
#' \code{& collision_year <= upper_year} to the filter if an upper bound is
#' intended.
#'
#' @examples
#' \dontrun{
#' ksi_pavement <- summarise_casualties_pavements(
#'   crashes = crashes_la,
#'   casualties = casualties_la,
#'   vehicles = vehicles_la,
#'   base_year = 2018,
#'   upper_year = 2023
#' )
#' ksi_pavement
#' }
#'
#' @seealso \code{\link{summarise_casualties_per_collision}} for the per-collision
#'   severity aggregation step.
#' @importFrom dplyr filter inner_join group_by summarise mutate select rowwise
#' @importFrom sf st_set_geometry
#' @export
summarise_casualties_pavements <- function(crashes_df, casualties_df, vehicles_df,
                                               base_year, upper_year) {
  
  vehicles_df = summarise_vehicle_types(vehicles = vehicles_df, summary_type = "short_name") |> 
    mutate(vehicle_type = short_name)

    cas_summary_pave <- casualties_df |> 
      filter(pedestrian_location == "On footway or verge") |> 
      summarise_casualties_per_collision()
    # pavements
    one_car_pave <- crashes_df |> 
      st_set_geometry(NULL) |> 
      filter(number_of_vehicles == 1 & collision_year >= base_year & collision_year <= upper_year) |> 
      inner_join(vehicles_df, by = "collision_index") |> 
      inner_join(cas_summary_pave, by = "collision_index") |> 
      group_by(vehicle_type) |>
      summarise(Collisions = n(),
                Fatal = sum(Fatal,na.rm=TRUE),
                Serious = sum(round(Serious),na.rm=TRUE),
                Slight = sum(round(Slight),na.rm = TRUE)) |> 
      rowwise() |> 
      mutate(KSI = sum(Fatal,Serious),
             Total = sum(Fatal,Serious,Slight))
    

    return(one_car_pave)
  }

