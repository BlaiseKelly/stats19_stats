

#' Download and read ONS collision cost data
#'
#' Downloads the official ONS collision cost dataset (ODS format), reads the
#' "Average_value" sheet, and returns a cleaned data frame.
#'
#' @param url Character. URL of the ONS ODS file. Default is the published
#'   government link.
#' @return A data frame with columns:
#'   \describe{
#'     \item{collision_data_year}{Year of collision data}
#'     \item{price_year}{Price year}
#'     \item{severity}{Severity category}
#'     \item{cost_per_casualty}{Cost per casualty (string with commas)}
#'     \item{cost_per_collision}{Cost per collision (string with commas)}
#'   }
#' @export
get_ons_cost_data <- function(
    url = "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods"
) {
  tmpfile <- tempfile(fileext = ".ods")
  
  utils::download.file(url, destfile = tmpfile, mode = "wb")
  
  ons_cost <- readODS::read_ods(tmpfile, sheet = "Average_value", skip = 3)
  
  # drop first row and keep first 5 columns
  ons_cost_form <- ons_cost[-1, 1:5]
  
  names(ons_cost_form) <- c(
    "collision_data_year", "price_year", "severity",
    "cost_per_casualty", "cost_per_collision"
  )
  
  ons_cost_form
}

#' Get electric scooter data
#'
#' Placeholder function for retrieving electric scooter data.
#'
#' @return Currently returns `NULL`. Implement data retrieval logic here.
#' @export
get_electric_scooter_data <- function() {
  NULL
}

#' Match LSOA codes to 2021 equivalents
#'
#' Matches casualty or vehicle LSOA codes to 2021 LSOA codes using official
#' lookup tables from the `geographr` package.
#'
#' @param casualties Optional casualty data frame with `lsoa_of_casualty`.
#' @param vehicles Optional vehicle data frame with `lsoa_of_driver`.
#' @return The input data frame with added 2021 LSOA codes and names.
#' @export
match_2021_lsoa <- function(casualties = NULL,
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

# with the package a 1km population raster for GB is included from landscan https://www.nature.com/articles/s41597-025-04817-z
get_population <- function(){
  
  
  
}

get_lsoa21_geo <- function(provider = "geographr", lsoa_code, lsoa_name){
  
  if(provider == "geographr"){
    lsoa_geo = geographr::boundaries_lsoa21 |> 
      select(lsoa21_code,lsoa21_name,geometry)
  } else {
    # download LSOA gpkg from https://communitiesopendata-communities.hub.arcgis.com/datasets/4da63019f25546aa92a922a5ea682950_0/explore?location=52.533125%2C-2.489482%2C7.17
    lsoa_geo = st_read(provider) |> 
      select(lsoa21_code = {{lsoa_code}},lsoa21_name = {{lsoa_name}},geometry = SHAPE)
  }
  
  return(lsoa_geo)
  
}

get_lsoa_pop = function(lsoa_codes){
  
  sr_pop = nomisr::nomis_search(name = "*TS001*")
  
  # Download TS017 data for all LSOAs in England & Wales
  population <- nomisr::nomis_get_data(
    id = sr_pop$id,  # TS001 dataset
    geography = lsoa_codes,  # LSOA geography code
    measures = 20100  # Observation measure 20100 is value 20301 is percentage
  ) |> 
    dplyr::filter(C2021_RESTYPE_3_NAME == "Total: All usual residents") |> 
    dplyr::select(lsoa21_code = GEOGRAPHY_CODE, population = OBS_VALUE)
  
  return(population)
  
}
