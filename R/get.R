

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
#' @examples
#' \dontrun{
#' ons_costs <- get_ons_cost_data()
#' head(ons_costs)
#' }
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
#' @return Currently returns \code{NULL}. Implement data retrieval logic here.
#' @examples
#' \dontrun{
#' escooter <- get_escooter_data()
#' }
#' @export
get_escooter_data <- function() {
  NULL
}


#' Download LSOA 2021 boundary geometries
#'
#' Retrieves Lower Layer Super Output Area (LSOA) 2021 boundary polygons from
#' either the \code{geographr} package or a GeoPackage hosted on GitHub
#' (originally sourced from the ONS Open Geography Portal).
#'
#' @param provider Character. One of \code{"geographr"} (default) or
#'   \code{"ons"}. Controls where boundaries are loaded from.
#' @param lsoa_code Unquoted column name for the LSOA code field when
#'   \code{provider = "ons"} (passed via tidy evaluation).
#' @param lsoa_name Unquoted column name for the LSOA name field when
#'   \code{provider = "ons"}.
#' @return An \code{sf} data frame with columns \code{lsoa21_code},
#'   \code{lsoa21_name}, and \code{geometry}.
#' @examples
#' \dontrun{
#' lsoa_sf <- get_lsoa21_boundaries(provider = "geographr")
#' lsoa_sf <- get_lsoa21_boundaries(provider = "ons",
#'                                   lsoa_code = LSOA21CD,
#'                                   lsoa_name = LSOA21NM)
#' }
#' @export
get_lsoa21_boundaries <- function(provider = c("geographr","ons"), lsoa_code, lsoa_name){
  
  if(provider == "geographr"){
    lsoa_geo = geographr::boundaries_lsoa21 |> 
      dplyr::select(lsoa21_code,lsoa21_name,geometry)
  }
  
  if(provider == "ons"){
    
    # unreliable ONS server switched to github release
    lsoa_url = "https://github.com/BlaiseKelly/stats19_stats/releases/download/boundaries-v1.0/lsoa_boundaries.gpkg"
   
    # import and normalise the names
    lsoa_geo <- sf::st_read(lsoa_url) |> 
        dplyr::select(lsoa21_code = {{lsoa_code}},lsoa21_name = {{lsoa_name}},geometry = geom)

    # make sure it knows the geometry column
    st_geometry(lsoa_geo) = lsoa_geo$geometry
    
  }
  
  return(lsoa_geo)
  
}

#' Download LSOA population data from NOMIS
#'
#' Queries the NOMIS API for Census 2021 usual resident population counts
#' (table TS001) at LSOA level for the supplied geography codes.
#'
#' @param lsoa_codes Character. NOMIS geography type code identifying the
#'   LSOAs to retrieve (e.g. \code{"TYPE298"}).
#' @return A data frame with columns \code{lsoa21_code} and \code{population}.
#' @examples
#' \dontrun{
#' pop <- get_nomis_populationulation(lsoa_codes = "TYPE298")
#' }
#' @export
get_nomis_populationulation = function(lsoa_codes){
  
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

#' Download Local Authority boundary polygon
#'
#' Retrieves the boundary polygon for a named Local Authority from either the
#' ONS Open Geography Portal (via a GitHub mirror) or the Eurostat NUTS Level 3
#' boundaries. The ONS path selects the largest polygon fragment and applies a
#' buffer–union–debuffer to clean messy coastal edges.
#'
#' @param city_name Character. Name (or partial regex match) of the Local
#'   Authority to retrieve, e.g. \code{"Bristol"}.
#' @param source Character. One of \code{"ons"} or \code{"eurostat"}.
#' @return An \code{sf} polygon for the matched Local Authority.
#' @examples
#' \dontrun{
#' bristol_sf <- get_la_boundaries("Bristol", source = "ons")
#' leeds_sf   <- get_la_boundaries("Leeds", source = "eurostat")
#' }
#' @export
get_la_boundaries = function(city_name, source = c("ons", "eurostat")){
  
  if(source == "ons"){
  
    # get local authorities, removing Norther Ireland which is not in stats19
    cl <- 
      #st_read("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/995533eee7e44848bf4e663498634849/geoPackage?layers=0") |> 
      st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/LA_boundaries/LA.gpkg") |> 
      filter(grepl(city_name, LAD22NM)) |>
      mutate(name = city_name) |> 
    st_cast("POLYGON") |> 
    mutate(area = st_area(geom)) |> 
    arrange(desc(area)) |> 
    slice(1:1) |> 
    st_buffer(100) |> # get rid of messy edge geometry for some cities
    st_union() |>
    st_buffer(-100) |>
    st_as_sf()
    
    return(cl)
  
  }
  
  if(source == "eurostat"){
    
    ## import nuts geo lvel 3 (cities) from eurostat and filter for UK
    uk_cities <- eurostat::get_eurostat_geospatial("sf", resolution = "01", nuts_level = "3", "2021", crs = "4326") |>
      filter(CNTR_CODE == "UK" & grepl(city_name, NUTS_NAME)) |>
      transmute(name = city_name,
             full_name = NUTS_NAME,
             geometry)
    
    return(uk_cities)
    
  }
  
}
