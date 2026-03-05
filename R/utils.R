


#' Read spatial data with retry on failure
#'
#' Wraps \code{sf::st_read()} in a retry loop, useful for unreliable remote
#' servers (e.g. the ONS Open Geography Portal). Retries indefinitely with
#' a configurable wait between attempts.
#'
#' @param url Character. URL or file path to read.
#' @param wait Numeric. Seconds to wait between retry attempts. Default
#'   \code{5}.
#' @return An \code{sf} object returned by \code{sf::st_read()}.
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


#' Vehicle type grouping lookup table
#'
#' A data frame mapping detailed STATS19 vehicle types to simplified short
#' names and driver-type labels. Used by \code{summarise_vehicle_types()}.
#'
#' @format A data frame with 22 rows and 3 columns:
#'   \describe{
#'     \item{vehicle_type}{Original STATS19 vehicle type label}
#'     \item{short_name}{Simplified category (e.g. \code{"Car"},
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
  
  #summary_type = "driver_type"
  
  veh_new_types <- vehicles |>
    left_join(vehicle_groups, by = "vehicle_type") |> 
    mutate(vehicle_type = !!sym(summary_type))
  
  return(veh_new_types)
  
}

#' Available basemap tile providers
#'
#' Character vector of basemap tile names suitable for \code{tmap::tm_basemap()}.
#' @examples
#' basemap_options
basemap_options = c("CartoDB.DarkMatter","Stadia.AlidadeSmoothDark", "CartoDB.Positron")

# run the report R script to generate the output files for the 
run_report_inputs <- function(authority) {
  params <- list(authority = authority)
  
  source("R/report.R", local = list2env(params, envir = globalenv()))
}


# render documents 
run_site <- function(authority) {
  # Read the QMD
  qmd_lines <- readLines("Rmd/LA_report_default.qmd")
  
  # Replace the param default
  qmd_lines <- gsub(
    "AUTHORITY",
    authority,
    qmd_lines
  )
  writeLines(qmd_lines, "LA_report.qmd")
  
  # Read the yaml
  yaml_lines <- readLines("yml/_quarto_default.yml")
  
  # Replace the param default
  yaml_lines <- gsub(
    "AUTHORITY",
    authority,
    yaml_lines
  )
  writeLines(yaml_lines, "_quarto.yml")
  
  # Render the whole site
  quarto::quarto_render()
}
