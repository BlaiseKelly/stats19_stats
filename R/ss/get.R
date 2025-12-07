library(readODS)

get_ons_cost_data <- function(url = "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods"){
  
  url = "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods"
  
  # Define the URL and a temporary file path
  tmpfile <- tempfile(fileext = ".ods")
  
  # Download the file
  download.file(url, destfile = tmpfile, mode = "wb")
  
  # Now read the ODS file from the local path
  ons_cost <- read_ods(tmpfile, sheet = "Average_value", skip = 3)
  
  # adjust the names which are badly formatted
  ons_cost_form <- ons_cost[-1,1:5]
  
  # replace with manual names
  names(ons_cost_form) <- c("collision_data_year","price_year","severity","cost_per_casualty","cost_per_collision")
  
  return(ons_cost_form)
  
  
}

get_electric_scooter_data <- function(){
  
  
  
}

# the lsoa provided at the time of the crash was relevant for that time of year. Match to 2021
match_2021_lsoa <- function(casualties = NULL,
                            vehicles = NULL){
  
  
  if(!is.null(casualties)){
    df2match <- casualties
  col_nam <- "lsoa_of_casualty"
  } else {
    df2match <- vehicles
    col_nam <- "lsoa_of_driver"
  }
  
  # import 01 to 11 lookup
  lsoa_lookup_01 <- geographr::lookup_lsoa01_lsoa11 |> 
    select(lsoa01_code, lsoa11_name, lsoa11_code) |> 
    distinct(lsoa11_code, .keep_all = TRUE)
  
  # import 11 to 21 lookup
  lsoa_lookup_21 <- geographr::lookup_lsoa11_lsoa21_ltla22 |> 
    select(lsoa11_code, lsoa21_name, lsoa21_code) 
  
  # match in stages, with 01 data also filling in 21 conversion
  lsoas_1 <- df2match |> 
    select({{col_nam}}) |> 
    left_join(lsoa_lookup_01, join_by(!!sym(col_nam) == "lsoa01_code"), relationship = "many-to-many") |> 
    filter(!is.na(lsoa11_code)) |> 
    select({{col_nam}}, lsoa11_code) |> 
    left_join(lsoa_lookup_21, by = "lsoa11_code",relationship = "many-to-many") |>  
    select({{col_nam}}, lsoa21_code, lsoa21_name)
  
  # with 11 data, also filling in 21 conversion
  lsoas_2 <- df2match |> 
    select({{col_nam}}) |> 
    left_join(lsoa_lookup_21, join_by(!!sym(col_nam) == "lsoa11_code"), relationship = "many-to-many") |> 
    filter(!is.na(lsoa21_code)) |> 
    select({{col_nam}}, lsoa21_code, lsoa21_name)
  
  # with 21 data
  lsoas_3 <- df2match |> 
    select({{col_nam}}) |> 
    left_join(lsoa_lookup_21, join_by(!!sym(col_nam) == "lsoa21_code"), relationship = "many-to-many") |> 
    filter(!is.na(lsoa21_name)) |> 
    select({{col_nam}},lsoa21_name) |> 
    mutate(lsoa21_code = col_nam)
  
  # bind, and drop duplicates
  lsoas <- rbind(lsoas_1, lsoas_2, lsoas_3) |> 
    distinct(!!sym(col_nam), .keep_all = TRUE)
  
  #join with casualties
  df_lsoa <- df2match |> 
    left_join(lsoas, by = c(col_nam),relationship = "many-to-many") 
  
  return(df_lsoa)
  
}
