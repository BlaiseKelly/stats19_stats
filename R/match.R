

library(dplyr)
library(readODS)
library(reshape2)

match_TAG <- function(crashes,
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
