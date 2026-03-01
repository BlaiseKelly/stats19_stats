
#' Match crashes to TAG collision cost data
#'
#' Downloads the RAS4001 cost tables from the DfT and joins them to a crash
#' data frame by severity (and optionally road type), adding per-casualty and
#' per-collision cost columns. Optionally returns a summary table.
#'
#' @param crashes An \code{sf} data frame of crash records with
#'   \code{collision_year} and \code{collision_severity} columns.
#' @param match_with Character. One of \code{"severity_year"} (costs by
#'   severity and year) or \code{"severity_year_road_type"} (costs also split
#'   by built-up / non-built-up / motorway). Default \code{"severity_year"}.
#' @param summarise Logical. If \code{TRUE}, return an aggregated summary
#'   instead of the full joined data. Default \code{FALSE}.
#' @return A data frame (or \code{sf} object) of crashes enriched with TAG
#'   cost columns, or a summary data frame if \code{summarise = TRUE}.
#' @examples
#' \dontrun{
#' crashes_costed <- match_summarise_tag_costs(crashes, match_with = "severity_year")
#' cost_summary   <- match_summarise_tag_costs(crashes, match_with = "severity_year",
#'                                   summarise = TRUE)
#' }
#' @export
match_summarise_tag_costs <- function(crashes,
                      match_with = "severity_year",
                      summarise = FALSE){
  
  # get ras4001
  url = "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods"
  
  tmpfile <- tempfile(fileext = ".ods")
  
  utils::download.file(url, destfile = tmpfile, mode = "wb")
  
  if(match_with == "severity_year"){
    # get table average_value and tidy up headers
    ras4001 <- readODS::read_ods(tmpfile, sheet = "Average_value", skip = 5, col_names = FALSE) |> 
      transmute("collision_year" = ...1,
                "collision_severity" = ...3,
                "cost_per_casualty" = as.numeric(...4),
                "cost_per_collision" = as.numeric(...5))
    
    # join with collision data
    tag_sev <- cra_2024 |> 
      left_join(ras4001, by = c("collision_year", "collision_severity")) 
    
    if(summarise == TRUE){
      
      tag_sev = tag_sev |>
        st_set_geometry(NULL) |> 
        group_by(collision_severity) |> 
        summarise(casualty_cost_millions = round(sum(cost_per_casualty)/1e6),
                  collision_cost_millions = round(sum(cost_per_collision)/1e6))
      
    }
    
    return(tag_sev)
    
  }
  
  if(match_with == "severity_year_road_type"){
    
    # get table average_value_road_type
    ras4001 = readODS::read_ods(tmpfile, sheet = "Average_value_road_type", skip = 3) |> 
      transmute(collision_year = `Collision data year`,
                collision_severity = Severity,
                built_up = `Built-up roads (£) [note 3]`,
                not_built_up = `Non built-up roads (£) [note 3]`,
                Motorway = `Motorways (£) [note 3]`) |> 
      filter(collision_year == 2024 & collision_severity %in% c("Fatal", "Serious", "Slight")) |> 
      melt(c("collision_year", "collision_severity"), variable.name = "ons_road", value.name = "cost")
    
    # define road category, first by motorway or not, then speed limit and 3 collisions had no speed data but did have urban or rural, so that also used.
    tag_sev_road_type = cra_2024 |> 
      mutate(speed_limit = as.numeric(speed_limit)) |> 
      mutate(ons_road = if_else(first_road_class == "Motorway", "Motorway", if_else(speed_limit <= "40", "built_up", "not_built_up"))) |> 
      mutate(ons_road = if_else(is.na(speed_limit) & urban_or_rural_area == "Urban", "built_up",ons_road)) |> 
      mutate(ons_road = if_else(is.na(speed_limit) & urban_or_rural_area == "Rural", "not_built_up",ons_road)) |> 
      left_join(ras4001, by = c("collision_year", "collision_severity", "ons_road")) 
    
    if(summarise == TRUE){
      
      tag_sev_road_type = tag_sev_road_type |> 
        st_set_geometry(NULL) |> 
        group_by(collision_severity, ons_road) |> 
        summarise(costs_millions = round(sum(cost)/1e6)) |> 
        dcast(collision_severity~ons_road)
      
    }
    
    return(tag_sev_road_type)
    
  }
  
}

#' Match LSOA codes to 2021 equivalents
#'
#' Matches casualty or vehicle LSOA codes to 2021 LSOA codes using official
#' lookup tables from the `geographr` package.
#'
#' @param casualties Optional casualty data frame with \code{lsoa_of_casualty}.
#' @param vehicles Optional vehicle data frame with \code{lsoa_of_driver}.
#' @return The input data frame with added 2021 LSOA codes and names.
#' @examples
#' \dontrun{
#' cas_lsoa21 <- match_lsoa_2021(casualties = my_casualties)
#' veh_lsoa21 <- match_lsoa_2021(vehicles = my_vehicles)
#' }
#' @export
match_lsoa_2021 <- function(casualties = NULL,
                            vehicles = NULL) {
  if (!is.null(casualties)) {
    df2match <- casualties
    col_nam <- "lsoa_of_casualty"
  } else {
    df2match <- vehicles
    col_nam <- "lsoa_of_driver"
  }
  
  # lookup tables
  lsoa_lookup_01 <- geographr::lookup_lsoa01_lsoa11 %>%
    dplyr::select(lsoa01_code, lsoa11_name, lsoa11_code) %>%
    dplyr::distinct(lsoa11_code, .keep_all = TRUE)
  
  lsoa_lookup_21 <- geographr::lookup_lsoa11_lsoa21_ltla22 %>%
    dplyr::select(lsoa11_code, lsoa21_name, lsoa21_code)
  
  # stage 1: 01 -> 11 -> 21
  lsoas_1 <- df2match %>%
    dplyr::select(dplyr::all_of(col_nam)) %>%
    dplyr::left_join(lsoa_lookup_01,
                     by = setNames("lsoa01_code", col_nam)) %>%
    dplyr::filter(!is.na(lsoa11_code)) %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa11_code) %>%
    dplyr::left_join(lsoa_lookup_21, by = "lsoa11_code") %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa21_code, lsoa21_name)
  
  # stage 2: 11 -> 21
  lsoas_2 <- df2match %>%
    dplyr::select(dplyr::all_of(col_nam)) %>%
    dplyr::left_join(lsoa_lookup_21,
                     by = setNames("lsoa11_code", col_nam)) %>%
    dplyr::filter(!is.na(lsoa21_code)) %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa21_code, lsoa21_name)
  
  # stage 3: already 21
  lsoas_3 <- df2match %>%
    dplyr::select(dplyr::all_of(col_nam)) %>%
    dplyr::left_join(lsoa_lookup_21,
                     by = setNames("lsoa21_code", col_nam)) %>%
    dplyr::filter(!is.na(lsoa21_name)) %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa21_name) %>%
    dplyr::mutate(lsoa21_code = !!rlang::sym(col_nam))
  
  # combine
  lsoas <- dplyr::bind_rows(lsoas_1, lsoas_2, lsoas_3) %>%
    dplyr::distinct(!!rlang::sym(col_nam), .keep_all = TRUE)
  
  df_lsoa <- df2match %>%
    dplyr::left_join(lsoas, by = col_nam)
  
  df_lsoa
}

#' Compute population-weighted IMD decile for MSOAs
#'
#' Aggregates LSOA-level Index of Multiple Deprivation (IMD 2025) data to MSOA
#' level using a population-weighted mean of IMD deciles. Downloads the IMD
#' GeoPackage from GitHub if not supplied.
#'
#' @param IMD_lsoa_data Optional \code{sf} data frame of LSOA-level IMD data.
#'   If \code{NULL} (the default), the function downloads it from GitHub.
#' @return A data frame with columns \code{MSOA21CD},
#'   \code{imd_weighted} (population-weighted mean IMD decile), and
#'   \code{n_lsoa} (number of LSOAs in each MSOA).
#' @examples
#' \dontrun{
#' msoa_imd <- match_msoa_imd()
#' msoa_imd <- match_msoa_imd(IMD_lsoa_data = my_imd_sf)
#' }
#' @export
match_msoa_imd = function(IMD_lsoa_data = NULL){
  
  if(is.null(IMD_lsoa_data)){
    
    IMD_lsoa_data = st_read("https://github.com/BlaiseKelly/IMD/releases/download/LSOA_IMD2025/LSOA_IMD2025_WGS84_-4854136717238973930.gpkg")
    
  }
  
  lsoa_geo = get_lsoa21_boundaries(provider = "geographr") |> 
    st_transform(27700)
  
  lsoa_cent = st_centroid(lsoa_geo)
  
  lsoa_pop = read.csv("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/lsoa21_pop_tot_2011_2024.csv") |> 
    select(lsoa21cd,pop = X2024)
  
  msoa_geo = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/msoa_boundaries-v1.0/msoa.gpkg") |> 
    st_transform(27700) |> 
    select(MSOA21CD,geom)
  
  msoa_lsoa = st_join(msoa_geo,lsoa_cent) |> 
    select(MSOA21CD,lsoa21_code) |> 
    st_set_geometry(NULL)
  
  msoa_imd <- IMD_2025 |> 
    st_set_geometry(NULL) |> 
    left_join(lsoa_pop, by = c("LSOA21CD" = "lsoa21cd")) |> 
    left_join(msoa_lsoa, by = c("LSOA21CD" = "lsoa21_code")) |> 
    group_by(MSOA21CD) %>%
    summarise(
      imd_weighted = weighted.mean(IMDDecil, w = pop, na.rm = TRUE),
      n_lsoa = n()
    )
  
  return(msoa_imd)
  
}

#' Match crashes to nearest OSM road segment
#'
#' Assigns each crash record to the nearest driving network segment from OSM.
#'
#' @param osm_network_sf An \code{sf} object of OSM network data.
#' @param crash_sf An \code{sf} object of crash points.
#' @return The crash \code{sf} object with an added \code{osm_id} column.
#' @examples
#' \dontrun{
#' crashes_matched <- match_crashes_to_osm(osm_network_sf = osm_data,
#'                                         crash_sf = crashes)
#' }
#' @export
match_crashes_to_osm <- function(osm_network_sf, crash_sf) {
  drive_net <- osmactive::get_driving_network(osm_network_sf)
  
  crs_osm <- sf::st_crs(drive_net)
  crash_sf <- sf::st_transform(crash_sf, crs_osm)
  
  crash_sf$osm_id <- drive_net$osm_id[sf::st_nearest_feature(crash_sf, drive_net)]
  
  crash_sf
}
