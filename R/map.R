
#' Available basemap tile providers
#'
#' Character vector of basemap tile names suitable for \code{tmap::tm_basemap()}.
#' @examples
#' basemap_options
basemap_options = c("CartoDB.DarkMatter","Stadia.AlidadeSmoothDark", "CartoDB.Positron")

#' Interactive casualty bubble map by casualty type
#'
#' Creates an interactive \code{tmap} bubble map of casualty locations, with
#' radio buttons to toggle between casualty types. Bubble colour represents
#' IMD decile and size represents severity.
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param casualties Casualty data frame.
#' @param extent_geo An \code{sf} polygon used to set the initial map extent.
#' @param fill_palette Character. Colour palette name for the fill scale.
#'   Default \code{"tol.incandescent"}.
#' @param backgroup_map Character. Basemap tile provider. Default
#'   \code{"CartoDB.DarkMatter"}.
#' @return A \code{tmap} object (interactive view mode).
#' @examples
#' \dontrun{
#' tm <- map_casualties_interactive(crashes, casualties, extent_geo = city_sf)
#' tm
#' }
#' @export
map_casualties_interactive = function(crashes,
                                       casualties,
                                       extent_geo,
                                       fill_palette = "tol.incandescent",
                                       backgroup_map = "CartoDB.DarkMatter"){
  
  # summarise the casualty types into more general groups
  cas_sum = summarise_casualty_types(casualties)
  
  # createdf and make names neater for plot
  cra_cas = inner_join(crashes, cas_sum) |> 
    transmute(`Collision index` = collision_index,
              `Collision year` = as.character(collision_year),
              `Casualty severity` = casualty_severity,
              `Date and time` = datetime,
              `Sex of casualty` = sex_of_casualty,
              `Age of casualty` = as.character(age_of_casualty),
              `Casualty type` = short_name,
              `Casualty IMD` = casualty_imd_decile,
              sev_plot_size = if_else(fatal_count == 1, 1.5, casualty_adjusted_severity_serious),
              `Speed limit` = as.numeric(speed_limit)) |> 
    filter(!`Casualty type` %in% c("Other vehicle", "Data missing"))

  # initial geometry extent to start map off
tm1 <- tm_shape(extent_geo) +
  tm_polygons(fill_alpha = 0,
              col_alpha = 0,
              popup.vars = FALSE,
              group = "LSOA", 
              group.control = "hide")

# define the casualty types
casualty_types <- sort(unique(cra_cas$`Casualty type`))

# create function to loop through
tm1 <- reduce(casualty_types, function(map_obj, ct) {
  
  cas_df <- cra_cas[cra_cas$`Casualty type` == ct,]
  
  if(NROW(cas_df) > 1) {
    map_obj <- map_obj +
      tm_shape(cas_df) +  # cas_df is captured in this iteration's closure
      tm_bubbles(
        fill = "Casualty IMD",
        fill.scale = tm_scale_categorical(values = fill_palette),
        size = "sev_plot_size",
        size.legend = tm_legend(title = "Casualty Severity"),
        size.scale = tm_scale_intervals(
          breaks = c(0, 0.6, 1.1, Inf),         # Category boundaries
          labels = c("Slight", "Serious", "Fatal"),
          values.range = c(0.4, 1.5)),
        popup.vars = c("Collision year", "Casualty severity", "Date and time", 
                       "Sex of casualty", "Age of casualty", "Casualty type", 
                       "Casualty IMD", "Speed limit"),
        hover = "Collision year",
        id = "Collision index",
        group = ct,
        group.control = "radio")
  }
  
  map_obj
  
}, .init = tm1)

tm1 <- tm1 + 
  tm_basemap(backgroup_map, group.control = "none")+ 
  tm_view(control.collapse = FALSE,overlay.groups = "Cyclist")

return(tm1)

}



#' Interactive casualty bubble map coloured by a chosen variable
#'
#' Creates an interactive \code{tmap} bubble map of casualty locations with
#' radio buttons to toggle casualty type. Unlike
#' \code{map_casualties_interactive()}, the fill colour can be mapped to
#' day of week, month, year, hour, sex, age group, IMD decile, or speed
#' limit.
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param casualties Casualty data frame.
#' @param extent_geo An \code{sf} polygon for the initial extent.
#' @param colour_by Character. Variable to colour bubbles by. One of
#'   \code{"Day"}, \code{"Month"}, \code{"Year"}, \code{"Hour"},
#'   \code{"Sex of casualty"}, \code{"Age group"}, \code{"Casualty IMD"},
#'   \code{"Speed limit"}.
#' @return A \code{tmap} object (interactive view mode).
#' @examples
#' \dontrun{
#' tm <- map_casualties_interactive2(crashes, casualties,
#'                                   colour_by = "Hour")
#' tm
#' }
#' @export
map_casualties_interactive2 = function(crashes,casualties, extent_geo = city_shp,
                                       colour_by = c("Day","Month","Year", "Hour", "Sex of casualty",
                                                     "Age group","Casualty IMD", "Speed limit")){
  
  colour_by = c
  extent_geo = city_shp
  
  cas_sum = summarise_casualty_types(casualties,summary_type = "short_name")
  
  cra_cas = inner_join(crashes, cas_sum) |> 
    transmute(`Collision index` = collision_index,  # Added opening parenthesis
              `Collision year` = as.character(collision_year),
              `Casualty severity` = casualty_severity,
              `Date and time` = datetime,
              `Sex of casualty` = sex_of_casualty,
              `Age of casualty` = as.character(age_of_casualty),
              `Age group` = as.character(dft_age_band),
              `Casualty type` = short_name,
              `Casualty IMD` = casualty_imd_decile,
              sev_plot_size = if_else(fatal_count == 1, 1.5, casualty_adjusted_severity_serious),
              `Speed limit` = as.numeric(speed_limit)) |> 
    filter(!`Casualty type` %in% c("Other vehicle", "Data missing"))
  
  if(colour_by == "Day"){
    cra_cas$Day = lubridate::wday(cra_cas$`Date and time`, label = TRUE,abbr = FALSE)
    
  }
  if(colour_by == "Month"){
    cra_cas$Month = lubridate::month(cra_cas$`Date and time`, label = TRUE,abbr = FALSE)
  }
  if(colour_by == "Year"){
    cra_cas$Year = lubridate::year(cra_cas$`Date and time`)
  }
  if(colour_by == "Hour"){
    cra_cas$Hour = sprintf("%02d", lubridate::hour(cra_cas$`Date and time`))
  }
  
  casualty_types <- sort(unique(cra_cas$`Casualty type`))
  
  all_colour_levels <- sort(unique(cra_cas[[colour_by]]))
  
  tm1 = tm_shape(extent_geo) +
    tm_polygons(fill_alpha = 0,
                group = "LSOA", 
                group.control = "hide")

  #tm1 <- purrr::reduce(casualty_types, function(map_obj, ct) {
    
    for (ct in casualty_types){
    
    cas_df <- dplyr::filter(cra_cas, `Casualty type` == ct)
    
    if(NROW(cas_df) > 1) {
      
     if(length(unique(cas_df$sev_plot_size))==1){
      
      cas_df$sev_plot_size <- cas_df$sev_plot_size + runif(nrow(cas_df), 0, 0.001)
     }
      
      #map_obj <- map_obj
       tm1 = tm1 + tm_shape(cas_df) +  # cas_df is captured in this iteration's closure
        tm_bubbles(
          fill = colour_by,
          fill.scale = tm_scale_categorical(
            values = "-tol.incandescent",
            levels = all_colour_levels),
          fill.legend = tm_legend(title = colour_by),
          size = "sev_plot_size",
          size.legend = tm_legend(title = "Casualty Severity"),
          size.scale = tm_scale(values.range = c(0.3, 1.5),
                                labels = c("Slight", "Serious", "Fatal")),
          popup.vars = c("Collision year", "Casualty severity", "Date and time", 
                         "Sex of casualty", "Age of casualty", "Casualty type", 
                         "Casualty IMD", "Speed limit"),
          hover = "Collision year",
          id = "Collision index",
          group = ct,
          group.control = "radio") +
        tm_basemap("CartoDB.DarkMatter", group.control = "none")
      
    }
    
    }
    
 # }, .init = tm1)
  
  tm1 = tm1+
    tm_title(paste0("Casualty location disagregated by casualty type, sized by severity and coloured by ",colour_by))+
    tm_view(control.collapse = FALSE)
  
  return(tm1)
  
}


#' Interactive OSM road network casualty map
#'
#' Creates an interactive \code{tmap} line map showing casualty statistics
#' per OSM road link. Lines are coloured by a chosen variable and grouped
#' by casualty type or year with radio button toggles.
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param casualties Casualty data frame.
#' @param osm_data An \code{sf} object of OSM driving network segments.
#' @param group Character. One of \code{"casualty_type"} or \code{"year"}.
#' @param area_name Character. Area name for the map title. If \code{NULL},
#'   taken from the first crash record.
#' @param colour_by Character. Variable to colour lines by. One of
#'   \code{"Sex of casualty"}, \code{"Age group"}, \code{"Fatal"},
#'   \code{"KSI"}, \code{"Serious"}, \code{"Slight"}, \code{"Total"},
#'   \code{"Speed limit"}.
#' @return A \code{tmap} object (interactive view mode).
#' @examples
#' \dontrun{
#' tm <- map_osm_roads_interactive(crashes, casualties,
#'                                 osm_data = drive_net,
#'                                 colour_by = "KSI")
#' tm
#' }
#' @export
map_osm_roads_interactive = function(crashes,casualties, osm_data = drive_net,group = c("casualty_type"), area_name = NULL,
                                     colour_by = c("Sex of casualty","Age group","Fatal","KSI","Serious","Slight","Total", "Speed limit")){
  
  
  min_year = min(crashes$collision_year)
  max_year = max(crashes$collision_year)
  
  if(is.null(area_name)){
    area_name = crashes$local_authority_district[1]
  }
  
  
  rd_tot = summarise_osm_link_casualties(crashes = crashes,casualties, osm_data = osm_data,ranking = TRUE, group = "total") |> 
    ungroup() |> 
    transmute(`OSM ID` = osm_id,
              `Road name` = name,
              `Mean sex of casualties` = round(sex_of_casualty,1),
              `Mean age of casualties` = round(age_of_casualty),
              `Speed limit` = round(speed_limit/10)*10,
              `Collisions` = number_of_collisions,
              `Fatal` = round(fatal),
              `Serious` = round(serious),
              `KSI` = round(ksi),
              `Slight` = round(slight),
              `Total` = round(total),
              `Collisions rank` = collisions_rank,
              `Fatal rank` = fatal_rank,
              `Serious rank` = serious_rank,
              `KSI rank` = ksi_rank,
              `Slight rank` = slight_rank,
              `Total rank` = total_rank,
              geometry)
  
  if(colour_by %in% c("Fatal","KSI","Serious","Slight","Total")){
    tot_colour_levels = seq(min(rd_tot[[colour_by]]),max(rd_tot[[colour_by]]),1)
    pal = "tol.incandescent"
  }
  if(colour_by %in% c("Speed limit")){
    tot_colour_levels = c(20,30,40,50,60,70)
    pal = "tol.incandescent"
  }
  if(colour_by == c("Age group")){
    tot_colour_levels = c(0, 11, 15, 19, 24, 29, 39, 49, 59, 69, 100)
    labels = c("0-11", "12-15", "16-19", "20-24", "25-29",
               "30-39", "40-49", "50-59", "60-69", "70+")
    pal = "tol.incandescent"
  }
  if(colour_by == c("Sex of casualty")){
    tot_colour_levels = seq(0,1,0.2)
    pal = c("#de50a6","#4fadcf")
  }
  
  if(group == "casualty_type"){
    rd_cas = summarise_osm_link_casualties(crashes = crashes,casualties, osm_data = osm_data,ranking = TRUE, group = group) |> 
      ungroup() |> 
      transmute(`OSM ID` = osm_id,
                `Road name` = name,
                `Casualty type` = casualty_type,
                `Mean sex of casualties` = round(sex_of_casualty,1),
                `Mean age of casualties` = round(age_of_casualty),
                `Speed limit` = round(speed_limit/10)*10,
                `Collisions` = number_of_collisions,
                `Fatal` = round(fatal),
                `Serious` = round(serious),
                `KSI` = round(ksi),
                `Slight` = round(slight),
                `Total` = round(total),
                geometry)
    
    group_vars = unique(rd_cas$`Casualty type`)
  }
  if(group == "year"){
    rd_cas = summarise_osm_link_casualties(crashes = crashes,casualties, osm_data = osm_data,ranking = TRUE, group = group) |> 
      ungroup() |> 
      transmute(`OSM ID` = osm_id,
                `Road name` = name,
                `Year` = year,
                `Mean sex of casualties` = round(sex_of_casualty,1),
                `Mean age of casualties` = round(age_of_casualty),
                `Speed limit` = round(speed_limit/10)*10,
                `Collisions` = number_of_collisions,
                `Fatal` = round(fatal),
                `Serious` = round(serious),
                `KSI` = round(ksi),
                `Slight` = round(slight),
                `Total` = round(total),
                geometry)
    
    group_vars = unique(rd_cas$Year)
  }
  
  
  if(colour_by %in% c("Fatal","KSI","Serious","Slight","Total")){
    cas_colour_levels = seq(min(rd_tot[[colour_by]]),max(rd_tot[[colour_by]]),1)
    pal = "tol.incandescent"
    map_title = paste0(colour_by," casualties by OSM link. ",area_name,": ",min_year, " to ", max_year)
  }
  if(colour_by %in% c("Speed limit")){
    cas_colour_levels = c(20,30,40,50,60,70)
    pal = "tol.incandescent"
    map_title = paste0(colour_by," by OSM link. ",area_name,": ",min_year, " to ", max_year)
  }
  if(colour_by == c("Age group")){
    cas_colour_levels = c(0, 11, 15, 19, 24, 29, 39, 49, 59, 69, 100)
    labels = c("0-11", "12-15", "16-19", "20-24", "25-29",
               "30-39", "40-49", "50-59", "60-69", "70+")
    pal = "tol.incandescent"
    colour_by = "Mean age of casualties"
    map_title = paste0(colour_by," by OSM link. ",area_name,": ",min_year, " to ", max_year)
  }
  if(colour_by == c("Sex of casualty")){
    cas_colour_levels = seq(0,1,0.2)
    pal = c("#de50a6","#4fadcf")
    colour_by = "Mean sex of casualties"
    map_title = paste0(colour_by," by OSM link. ",area_name,": ",min_year, " to ", max_year)
  }
  
  
  tm1 = tm_shape(rd_tot) +  # cas_df is captured in this iteration's closure
    tm_lines(
      col = colour_by,lwd = 3,
      col.scale = tm_scale_continuous(
        values = pal),
      col.legend = tm_legend(title = colour_by),
      popup.vars = c("OSM ID","Road name","Mean sex of casualties","Mean age of casualties","Speed limit",
                     "Collisions","Fatal","Serious","KSI","Slight","Total","Collisions rank","Fatal rank","KSI rank","Slight rank","Total rank"),
      hover = "Road name",
      id = "Road name",
      group = "All",
      group.control = "radio")
  
  tm1 = tm1 + purrr::reduce(group_vars, function(map_obj, ct) {
    
    if(group == "casualty_type"){
      cas_df <- dplyr::filter(rd_cas, `Casualty type` == ct)
    } 
    if(group == "year"){
      cas_df <- dplyr::filter(rd_cas, `Year` == ct)
    }
    
    if(NROW(cas_df) > 1) {
      map_obj = map_obj +
        tm_shape(cas_df) +  # cas_df is captured in this iteration's closure
        tm_lines(
          col = colour_by,lwd = 3,
          col.scale = tm_scale_continuous(
            values = pal),
          col.legend = tm_legend(title = colour_by),
          popup.vars = c("OSM ID", "Road name", "Mean sex of casualties", 
                         "Mean age of casualties", "Speed limit", "Collisions", "Fatal","Serious", 
                         "Slight", "Total"),
          hover = "Road name",
          id = "Road name",
          group = ct,
          group.control = "radio") 
    }
    map_obj
    
  }, .init = tm1)
  
  tm1 = tm1+
    tm_basemap("CartoDB.DarkMatter", group.control = "none")+
    tm_title(map_title)+
    tm_view(control.collapse = FALSE)

  return(tm1)
  
}

#' Static OSM road network casualty map
#'
#' Creates a static \code{tmap} map of casualty statistics per OSM road link
#' over a raster basemap. Lines are coloured by a chosen variable and the
#' result can be filtered by casualty type and/or year.
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param casualties Casualty data frame.
#' @param osm_data An \code{sf} object of OSM driving network segments.
#' @param casualty_type Character vector of casualty types to include, or
#'   \code{NULL} for all.
#' @param year Integer. Single year to filter by, or \code{NULL} for all.
#' @param area_name Character. Area name for the map title. If \code{NULL},
#'   taken from the first crash record.
#' @param city_shape An \code{sf} polygon for basemap extent.
#' @param map_title Character. Custom title, or \code{NULL} for auto-generated.
#' @param basemap_bgd_colour Character. Basemap style: \code{"dark"} or
#'   \code{"light"}.
#' @param group Character. Grouping variable passed to
#'   \code{summarise_osm_link_casualties()}. Default \code{"total"}.
#' @param legend_pos Numeric vector of length 2 for legend position.
#' @param top_x Integer. If not \code{NULL}, show only the top x roads.
#' @param colour_by Character. Variable to colour lines by.
#' @return A \code{tmap} object (plot mode).
#' @examples
#' \dontrun{
#' tm <- map_osm_roads_static(crashes, casualties, osm_data = drive_net,
#'                            colour_by = "ksi", city_shape = city_sf)
#' }
#' @export
map_osm_roads_static = function(
    crashes,
    casualties,
    osm_data = drive_net,
    casualty_type = NULL,
    year = NULL,
    area_name = NULL,
    city_shape = city_shp,
    map_title = NULL,
    basemap_bgd_colour = "dark",
    group = "total",
    legend_pos = c(0.03,0.48),
    top_x = NULL,
    colour_by = c("number_of_collisions", "osm_id", "casualty_type",
                  "age_of_casualty", "sex_of_casualty", "speed_limit",
                  "fatal", "serious", "ksi", "slight", "total")
) {
  
  colour_by = match.arg(colour_by)
  
  if(is.null(year)){
    min_year = min(crashes$collision_year)
    max_year = max(crashes$collision_year)
  } else {
    min_year = year
    max_year = year
  }
  
  if (is.null(area_name)) area_name = crashes$local_authority_district[1]
  
  # Palette and title
  pal = "tol.incandescent"
  
  label = switch(colour_by,
                 "sex_of_casualty" = "Mean sex of casualties",
                 "age_of_casualty" = "Mean age of casualties",
                 colour_by
  )
  
  if (colour_by == "sex_of_casualty") pal = c("#de50a6", "#4fadcf")
  if (colour_by == "sex_of_casualty") legend_pos = c(0.02,0.4)
  
  if (is.null(map_title)) {
    label = gsub("_", " ", label)
    suffix = paste0(area_name, ": ", min_year, " to ", max_year)
    map_title = if (colour_by == "speed_limit") {
      paste0(label, " by OSM link. ", suffix)
    } else {
      if(NROW(casualty_type) < 2){
        paste0(label, " ",casualty_type, " by OSM link. ", suffix)
      } else{
        paste0(label, " for all casualties by OSM link. ", suffix)
      }
    }
  }
  
  # Build road summary
  rd_cas = summarise_osm_link_casualties(
    crashes = crashes,
    casualties = casualties,
    osm_data = osm_data,
    ranking = FALSE,
    group = group
  ) |> 
    dplyr::ungroup()
  
  # Apply filters using .env to avoid column name ambiguity
  if (!is.null(casualty_type)) {
    rd_cas = dplyr::filter(rd_cas, casualty_type %in% .env$casualty_type)
  }
  if (!is.null(year)) {
    rd_cas = dplyr::filter(rd_cas, year == .env$year)
  }
  
  if(NROW(casualty_type) < 2 | !is.null(year)){
    
    # Aggregate to one row per osm_id
    rd_cas = rd_cas |>
      dplyr::group_by(osm_id) |>
      dplyr::summarise(
        plot_value = sum(!!sym(colour_by), na.rm = TRUE),
        geometry = dplyr::first(geometry)
      ) |>
      # dplyr::mutate(
      #   line_width = scales::rescale(plot_value, to = c(5, 10))) |> 
      sf::st_as_sf()
  } else {
    
    # Aggregate to one row per osm_id
    rd_cas = rd_cas |>
      dplyr::group_by(osm_id) |>
      dplyr::summarise(
        plot_value = mean(!!sym(colour_by), na.rm = TRUE),
        geometry = dplyr::first(geometry)
      ) |>
      # dplyr::mutate(
      #   line_width = scales::rescale(plot_value, to = c(5, 10))) |> 
      sf::st_as_sf()
    
  }
  
  if(!is.null(top_x)){
    rd_cas = rd_cas |> 
      arrange(desc(colour_by)) |> 
      slice(1:top_x)
  }
  
  # Build map
  bm = basemaps::basemap_raster(city_shape, map_service = "carto", map_type = basemap_bgd_colour)
  
  tmap::tm_shape(bm) +
    tmap::tm_rgb() +
    tmap::tm_shape(rd_cas) +
    tmap::tm_lines(
      col = "plot_value",
      lwd = 2.5,
      col.scale = tmap::tm_scale_continuous(values = pal),
      col.legend = tmap::tm_legend(title = label)
    ) +
    tmap::tm_title(map_title)+
    tmap::tm_layout(frame = FALSE) +
    tmap::tm_legend(frame = FALSE, position = legend_pos)
}

#' Create a city boundary map with basemap background
#'
#' `map_city_boundary()` renders a basemap clipped to a city polygon and overlays the
#' city boundary. The function saves a high-resolution PNG to `plot_dir` and
#' returns the `tmap` object invisibly.
#'
#' @param city_shape An `sf` polygon (or multipolygon) describing the city
#'   boundary. Must have a valid CRS.
#' @param city_name Character. Short name used for the output filename and map
#'   title.
#' @param map_type Character. One of `"osm"` or `"carto_light"`. Controls which
#'   basemap service and style to request. Default `c("osm", "carto_light")`.
#' @param border_col Character. Colour used for the city boundary line.
#'   Default `"#ff7733"`.
#' @param plot_dir Character. Directory where the PNG will be saved. Created if
#'   it does not exist. Default `"plots/"`.
#' @param width,height Integer. Output image dimensions in pixels. Defaults are
#'   5000 x 5000 to match the original behaviour.
#' @return A `tmap` object (invisibly). The PNG is written to disk as a side
#'   effect.
#' @examples
#' \dontrun{
#' map_city_boundary(city_shape = my_city_sf, city_name = "MyCity")
#' }
#' @export
map_city_boundary <- function(city_shape,
                      city_name,
                      map_type = c("osm", "carto_light"),
                      border_col = "#ff7733",
                      plot_dir = "plots/",
                      border_width = 9,
                      width = 5000,
                      height = 5000) {
  map_type <- match.arg(map_type)
  
  stopifnot(inherits(city_shape, "sf"))
  if (missing(city_name) || !nzchar(city_name)) {
    stop("`city_name` must be provided and non-empty.")
  }
  
  # ensure output directory exists
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # choose basemap
  bm <- switch(
    map_type,
    osm = basemaps::basemap_raster(ext = city_shape, map_service = "osm", map_type = "topographic"),
    carto_light = basemaps::basemap_raster(ext = city_shape, map_service = "carto", map_type = "light")
  )
  
  # mask the background to the shape (transform to Web Mercator for raster mask)
  bm_masked <- terra::mask(bm, sf::st_transform(city_shape, 3857))
  
  # build tmap
  tm1 <- tmap::tm_shape(bm_masked) +
    tmap::tm_rgb() +
    tmap::tm_scalebar() +
    tmap::tm_shape(city_shape) +
    tmap::tm_polygons(fill_alpha = 0, col = border_col, col_alpha = 1, lwd = border_width) +
    tmap::tm_layout(frame = FALSE)
  
  out_file <- file.path(plot_dir, paste0(city_name, "_map.png"))
  tmap::tmap_save(tm1, out_file, width = width, height = height)
  
  invisible(tm1)
}


#' Default severity colour palette
#'
#' Three-colour vector for Fatal (red), Serious (blue), Slight (green) used
#' across plotting functions.
#' @keywords internal
cols <- rev(c("#00ab3d", "#005bb2","#c81329"))

#' Custom ggplot2 theme element
#' @keywords internal
cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))

#' Combined DfT-style ggplot2 theme
#' @keywords internal
dft_theme <- list(cust_theme, ggplot2::scale_color_manual(values = cols))


#' Plot cycling network with segregation classification and summary stats
#'
#' `map_cycle_network()` creates a map of cycling infrastructure for a city,
#' classifies segregated vs non-segregated paths, overlays them on a basemap,
#' annotates the map with summary statistics (from `summarise_cycle_network`) and
#' saves the result as a PNG. The function returns the `tmap` object invisibly.
#'
#' **Note:** This function relies on helper functions that are not included
#' here: `summarise_cycle_network()`, `distance_to_road()`, and
#' `classify_cycle_infrastructure()`. Ensure these are available in the package
#' or namespace where you use this function.
#'
#' @param city Character. City name used for title and filename.
#' @param city_shape An `sf` polygon (or multipolygon) describing the city
#'   boundary.
#' @param osm_data OSM data object expected by `osmactive::get_driving_network`
#'   and `osmactive::get_cycling_network`.
#' @param city_pop Numeric. Population used in the summary statistics.
#' @param title_position Character. `"left"` or `"right"` placement for the map
#'   title. Default `"right"`.
#' @param stats_position Character. `"left"` or `"right"` placement for the
#'   credits/stats box. Default `"right"`.
#' @param legend_position Character. `"left"` or `"right"` placement for the
#'   legend. Default `"left"`.
#' @param plot_dir Character. Directory where the PNG will be saved. Created if
#'   it does not exist. Default `"plots/"`.
#' @param dpi Integer. Output resolution for saved PNG. Default `400`.
#' @return A `tmap` object (invisibly). The PNG is written to disk as a side
#'   effect.
#' @examples
#' \dontrun{
#' map_cycle_network("MyCity", my_city_sf, my_osm_data, city_pop = 200000)
#' }
#' @export
map_cycle_network <- function(city,
                               city_shape,
                               osm_data,
                               city_pop,
                               title_position = c("right", "left"),
                               stats_position = c("right", "left"),
                               legend_position = c("left", "right"),
                               plot_dir = "plots/",
                               dpi = 400) {
  title_position <- match.arg(title_position)
  stats_position <- match.arg(stats_position)
  legend_position <- match.arg(legend_position)
  
  stopifnot(is.character(city), length(city) == 1)
  stopifnot(inherits(city_shape, "sf"))
  if (missing(osm_data)) stop("`osm_data` is required.")
  if (missing(city_pop) || !is.numeric(city_pop)) stop("`city_pop` must be numeric.")
  
  # ensure output directory exists
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # compute stats (external helper)
  stats <- summarise_cycle_network(city = city, city_shape = city_shape, osm_data = osm_data, city_pop = city_pop)
  
  # extract networks (osmactive functions)
  drive_net <- osmactive::get_driving_network(osm_data)
  cycle_net <- osmactive::get_cycling_network(osm_data)
  
  # compute distance to road and classify infrastructure (external helpers)
  cycle_net_d <- distance_to_road(cycle_net, drive_net)
  cycle_net_c <- classify_cycle_infrastructure(cycle_net_d) %>%
    dplyr::select(osm_id, detailed_segregation, geometry) %>%
    dplyr::mutate(segregated = ifelse(detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"), "YES", "NO"))
  
  # subset segregated paths (kept in case needed)
  seg_paths <- cycle_net_c[cycle_net_c$detailed_segregation %in% c("Level track", "Off Road Path", "Light segregation"), ]
  
  # basemap and mask
  bg <- basemaps::basemap_raster(city_shape, map_service = "carto", map_type = "light")
  bg_m <- terra::mask(bg, sf::st_transform(city_shape, 3857))
  
  # positions
  tit_pos <- if (title_position == "left") c(0, 0.99) else c(0.70, 0.98)
  cred_pos <- if (stats_position == "left") c(0.01, 0.93) else c(0.71, 0.93)
  leg_pos <- if (legend_position == "right") c(0.85, 0.15) else c(0.15, 0.15)
  
  # build tmap
  tmap::tmap_mode("plot")
  tm1 <- tmap::tm_shape(bg_m) +
    tmap::tm_rgb(col_alpha = 1) +
    tmap::tm_shape(city_shape) +
    tmap::tm_polygons(fill_alpha = 0) +
    tmap::tm_shape(cycle_net_c) +
    tmap.networks::tm_edges(col = "segregated", lwd = 3) +
    tmap::tm_legend(show = TRUE, position = leg_pos) +
    tmap::tm_title(text = city, position = tit_pos) +
    tmap::tm_credits(
      paste0(
        "Area population (2024): ", format(round(stats$city_pop), big.mark = ",", scientific = FALSE), "\n",
        "cycle path total km: ", round(stats$cycle_paths), "\n",
        "segregated cycle paths total km: ", round(stats$seg_cycle), "\n",
        "metres of total cycle path per person: ", round(stats$cycle_pp, 2), "\n",
        "metres of segregated cycle path per person: ", round(stats$seg_pp, 2), "\n",
        "km of total cycle path per km\u00B2: ", round(stats$cycle_km2, 2), "\n",
        "km of segregated cycle path per km\u00B2: ", round(stats$seg_km2, 2), "\n",
        "metres of road per person: ", round(stats$drive_pp, 2), "\n",
        "km of road per km\u00B2: ", round(stats$drive_km2, 2), "\n"
      ),
      bg = TRUE, size = 0.5, bg.alpha = 0.3, bg.color = "grey95", position = cred_pos
    ) +
    tmap::tm_layout(frame = FALSE)+
    tmap::tm_credits("derived using osmactive R package", size = 0.4)
  
  out_file <- file.path(plot_dir, paste0("/la_cycle_paths.png"))
  tmap::tmap_save(tm1, out_file, dpi = dpi)
  
  invisible(tm1)
}

#' Plot casualty locations with type and severity
#'
#' Creates a bubble map of casualty locations, with bubble colour/shape
#' representing casualty type and bubble size representing severity.
#'
#' @param crashes An `sf` data frame of crash records.
#' @param casualties A data frame of casualty records.
#' @param city Character. City name used in the plot title and filename.
#' @param plot_dir Directory to save PNG. Default `"plots/"`.
#' @param icon_sized_by_severity Logical. If `TRUE`, bubble size reflects
#'   severity. Default `TRUE`.
#' @return Invisibly returns the `tmap` object.
#' @examples
#' \dontrun{
#' map_casualties(crashes, casualties, city = "Bristol")
#' }
#' @export
map_casualties <- function(crashes,
                            casualties,
                            city,
                            plot_dir = "plots/",
                            plot_type = c("static", "interactive"),
                            icon_sized_by_severity = TRUE) {
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  casualties_map <- casualties %>%
    dplyr::select(collision_index, casualty_type, pedestrian_location,
                  fatal_count, casualty_adjusted_severity_serious,
                  casualty_adjusted_severity_slight) %>%
    dplyr::group_by(collision_index, casualty_type) %>%
    dplyr::summarise(Fatal = sum(fatal_count),
                     Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
                     Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::left_join(crashes, by = "collision_index") %>%
    dplyr::select(casualty_type, Fatal, Serious, Slight, geometry) %>%
    sf::st_as_sf()
  
  bm <- basemaps::basemap_raster(ext = casualties_map,
                                 map_service = "carto", map_type = "light")
  
  sy <- min(casualties$collision_year, na.rm = TRUE)
  ey <- max(casualties$collision_year, na.rm = TRUE)
  
  if(plot_type == "static"){
    tmap_mode("plot")
  } else {
    tmap_mode("view")
  }
  
  tm2 <- tmap::tm_shape(bm) +
    tmap::tm_rgb() +
    tmap::tm_shape(casualties_map) +
    tmap::tm_bubbles(fill = "casualty_type",
                     shape = "casualty_type",
                     size = "Serious",
                     shape.legend = tmap::tm_legend_combine("fill"),
                     size.legend = tmap::tm_legend(title = "Severity")) +
    tmap::tm_title(
      paste0("Collision location with casualty type represented by shape and colour ",
             "and severity represented by size. ", city, ": ", sy, " and ", ey)
    )
  
  
  if(plot_type == "static"){
    out_file <- file.path(plot_dir, paste0(city, "_cas_type_sev_map.png"))
    tmap::tmap_save(tm2, out_file, width = 9500, height = 7000, dpi = 650)
  } else {
    out_file <- file.path(plot_dir, paste0(city, "_cas_type_sev_map.html"))
    tmap::tmap_save(tm2, out_file)
  }
  
  
  invisible(tm2)
}

#' Plot casualties on a specific street (OSM link)
#'
#' Creates a bubble map of casualties linked to a given OSM street segment,
#' with bubble colour/shape representing casualty type and bubble size
#' representing severity.
#'
#' @param osm_links An `sf` object of OSM street links.
#' @param casualties Casualty data frame.
#' @param crashes Crash data frame.
#' @param year_from Integer. Start year for filtering.
#' @param year_to Integer. End year for filtering.
#' @param casualties_buffer Numeric. Buffer distance for linking casualties.
#' @param plot_buffer Numeric. Buffer distance for plotting extent.
#' @param legend_pos Numeric vector of length 2. Legend position. Default `c(0.6, 1)`.
#' @param plot_dir Directory to save PNG.
#' @return Invisibly returns the `tmap` object.
#' @examples
#' \dontrun{
#' map_osm_street_casualties(my_street, casualties, crashes,
#'                           year_from = 2019, year_to = 2024,
#'                           casualties_buffer = 15, plot_buffer = 200,
#'                           plot_dir = "plots/")
#' }
#' @export
map_osm_street_casualties <- function(osm_links,
                                      casualties,
                                      crashes,
                                      year_from,
                                      year_to,
                                      bgd_map_buff = c("street", "crashes"),
                                      casualties_buffer,
                                      plot_buffer,
                                      legend_pos = NULL,
                                      plot_dir) {
  
  cra_cas_osm <- summarise_casualty_osm_link(osm_links, casualties, crashes,
                                   year_from, year_to,
                                   casualties_buffer = casualties_buffer) |> 
    mutate(sev_plot_size = if_else(Fatal == 1, 1.5, Serious)) |> 
    mutate(sev_plot_size = ifelse(Slight < 0.1,0.1,Slight))
  
  # if no legend position has been defined, calculate angle of osm link to determine where the legend should go
  angle <- stplanr::line_bearing(osm_links)
  if(is.null(legend_pos)){
    legend_pos <- ifelse(angle >= 90 & angle <= 180, list(c(0.75, 1)),
                         ifelse(angle >= -90 & angle < 0, list(c(0.75, 1)), list(c(0,1))))
    
    legend_pos <- unlist(legend_pos)
  }
  
  bm_ps <- basemaps::basemap_raster(
    ext = sf::st_buffer(osm_links, plot_buffer*2),
    map_service = "carto", map_type = "light"
  )
  if(bgd_map_buff == "crashes"){
    
    bm_masked <- terra::mask(bm_ps,
                             sf::st_transform(sf::st_buffer(cra_cas_osm, plot_buffer), 3857))
  } else {
    bm_masked <- terra::mask(bm_ps,
                             sf::st_transform(sf::st_buffer(osm_links, plot_buffer), 3857))
  }
  
  cas_scale <- c("Car occupant", "Motorcyclist", "Cyclist", "Taxi occupant",
                 "Goods vehicle occupant", "Bus occupant", "Other vehicle",
                 "Data missing", "Mobility scooter rider",
                 "Agricultural vehicle occupant", "Horse rider",
                 "E-scooter rider", "Pedestrian")
  
  cra_cas_osm$casualty_type <- factor(cra_cas_osm$casualty_type, levels = cas_scale)
  
  tm1 <- tmap::tm_shape(bm_masked) +
    tmap::tm_rgb() +
    tmap::tm_shape(cra_cas_osm) +
    tmap::tm_bubbles(fill = "casualty_type",
                     fill.scale = tmap::tm_scale_categorical(levels = cas_scale),
                     fill_alpha = 0.7,
                     shape = "casualty_type",
                     size = "sev_plot_size",
                     shape.legend = tmap::tm_legend_combine("fill"),
                     size.legend = tmap::tm_legend(title = "Severity")) +
    tmap::tm_title(
      paste0("Collision location with casualty type represented by shape and colour ",
             "and severity represented by size.\n",
             osm_links$name[1], ": ", year_from, " and ", year_to)
    ) +
    tmap::tm_layout(frame = FALSE) +
    tmap::tm_legend(frame = FALSE, position = legend_pos)
  
  out_file <- file.path(plot_dir,
                        paste0(gsub(" ", "_", osm_links$name[1]), "_cas_map.png"))
  #tmap::tmap_save(tm1, out_file, width = 11500, height = 9500, dpi = 1000)
  tmap::tmap_save(tm1, out_file)
  
  return(tm1)
}

#' Plot vehicles involved in collisions on a street (OSM link)
#'
#' Creates a bubble map of vehicles linked to a given OSM street segment,
#' with bubble colour/shape representing vehicle type and bubble size
#' representing collision severity.
#'
#' @param osm_links An `sf` object of OSM street links.
#' @param vehicles Vehicle data frame.
#' @param crashes Crash data frame.
#' @param year_from Integer. Start year for filtering.
#' @param year_to Integer. End year for filtering.
#' @param casualties_buffer Numeric. Buffer distance for linking casualties.
#' @param plot_buffer Numeric. Buffer distance for plotting extent.
#' @param severity_point_sizes Numeric vector of length 3. Point sizes for
#'   severities `"Fatal"`, `"Serious"`, `"Slight"`. Default `c(2, 1, 0.2)`.
#' @param legend_pos Numeric vector of length 2. Legend position. Default `c(0.6, 1)`.
#' @param plot_width Numeric. Width of saved PNG. Default `11500`.
#' @param plot_height Numeric. Height of saved PNG. Default `9500`.
#' @param plot_dpi Numeric. Resolution of saved PNG. Default `1000`.
#' @param plot_dir Directory to save PNG.
#' @return Invisibly returns the `tmap` object.
#' @examples
#' \dontrun{
#' map_osm_street_vehicles(my_street, vehicles, crashes, casualties,
#'                         year_from = 2019, year_to = 2024,
#'                         casualties_buffer = 15, plot_buffer = 200,
#'                         plot_dir = "plots/")
#' }
#' @export
map_osm_street_vehicles <- function(osm_links,
                                    vehicles,
                                    crashes,
                                    casualties,
                                    casualty_types = "All",
                                    year_from,
                                    year_to,
                                    bgd_map_buff = c("street", "crashes"),
                                    casualties_buffer,
                                    plot_buffer,
                                    legend_pos = NULL,
                                    plot_width = 11500,
                                    plot_height = 9500,
                                    plot_dpi = 1000,
                                    plot_dir) {
  

  cra_veh_osm <- summarise_vehicle_osm_link(osm_links = osm_links, 
                                  vehicles = vehicles, 
                                  crashes = crashes, 
                                  casualties = casualties,
                                  year_from = year_from, 
                                  year_to = year_to, 
                                  casualty_types = casualty_types,
                                  casualties_buffer = casualties_buffer) |> 
    mutate(sev_plot_size = if_else(collision_severity == "Fatal", 1.5,
                                   if_else(collision_severity == "Serious",1,0.4)))
  
  # if no legend position has been defined, calculate angle of osm link to determine where the legend should go
  angle <- stplanr::line_bearing(osm_links)
  if(is.null(legend_pos)){
    legend_pos <- ifelse(angle >= 90 & angle <= 180, list(c(0.8, 1)),
                         ifelse(angle >= -90 & angle < 0, list(c(0.8, 1)), list(c(0,1))))
    
    legend_pos <- unlist(legend_pos)
  }
  
  
  bm_ps <- basemaps::basemap_raster(
    ext = sf::st_buffer(osm_links, plot_buffer*2),
    map_service = "carto", map_type = "light"
  )
  
  # trim background map to the street or the crashes, makes the plot look a little neater
  if(bgd_map_buff == "crashes"){
    
    bm_masked <- terra::mask(bm_ps,
                             sf::st_transform(sf::st_buffer(cra_cas_osm, plot_buffer), 3857))
  } else {
    bm_masked <- terra::mask(bm_ps,
                             sf::st_transform(sf::st_buffer(osm_links, plot_buffer), 3857))
  }
  
  # summarised vehicle categories
  veh_scale <- c("Car", "Motorcycle", "Pedal cycle", "Taxi", "Goods vehicle",
                 "Bus", "Other vehicle", "Data missing or out of range",
                 "Mobility scooter", "Agricultural vehicle", "Ridden horse",
                 "e-scooter")
  
  # match
  cra_veh_osm <- cra_veh_osm |> 
    dplyr::mutate(vehicle_type = factor(vehicle_type, levels = veh_scale)) 
  
  tm1 <- tmap::tm_shape(bm_masked) +
    tmap::tm_rgb() +
    tmap::tm_shape(cra_veh_osm) +
    tmap::tm_bubbles(fill = "vehicle_type",
                     fill.scale = tmap::tm_scale_categorical(levels = veh_scale),
                     fill_alpha = 0.7,
                     shape = "vehicle_type",
                     size = "sev_plot_size",
                     size.scale = tmap::tm_scale_continuous(),
                     shape.legend = tmap::tm_legend_combine("fill"),
                     size.legend = tmap::tm_legend(title = "Severity")) +
    tmap::tm_title(
      paste0("Collision location with driven vehicle type represented by shape and colour",
             "and severity of the collision represented by\nsize, for ",casualty_types, " casualties ",
             osm_links$name[1], ": ", year_from, " and ", year_to)
    ) +
    tmap::tm_layout(frame = FALSE) +
    tmap::tm_legend(frame = FALSE, position = legend_pos)
  
  out_file <- file.path(plot_dir,
                        paste0(gsub(" ", "_", osm_links$name[1]), "_", casualty_types, "_veh_map.png"))
  
  #tmap::tmap_save(tm1, out_file,width = plot_width, height = plot_height, dpi = plot_dpi)
  tmap::tmap_save(tm1, out_file)
  
  return(tm1)
}



#' Map TAG collision costs by region
#'
#' Creates a choropleth map of total TAG collision costs by Local Authority,
#' with an inset table showing the 10 LAs with the greatest reduction
#' potential.
#'
#' @param region_sf An \code{sf} data frame of LA boundaries with cost data.
#' @param variable Character. Column name to plot.
#' @param palette Character. Colour palette name.
#' @param custom_breaks Numeric vector of class break values.
#' @param title Character. Map title.
#' @param legend_title Character. Legend title.
#' @return A \code{tmap} object.
#' @examples
#' \dontrun{
#' tm <- map_region_summarise_tag_costs(la_costs_sf, variable = "total_cost",
#'                            palette = "wes.zissou1",
#'                            title = "TAG costs 2024")
#' }
#' @export
map_region_summarise_tag_costs <- function(region_sf, variable,palette,custom_breaks,title,legend_title){
  
  bks <- c(0,2,6,10,15,30,60,100,200,400)
  
  cts_plot <- cts_city_sf |> 
    filter(collision_year == y) |> 
    arrange(desc(total_cost))
  
  tm1 <- tm_shape(cts_plot) +
    tm_polygons(fill = "total_cost",
                fill.scale = tm_scale_intervals(values = "wes.zissou1", breaks = bks),
                fill.legend = tm_legend("Value (£million)", frame = FALSE,legend.border.col = NA),
                lwd = 0.1
    )+
    #tm_text("name",size = 1)
    tm_title(y,size = 2)+
    tm_layout(frame = FALSE)+
    tm_credits(
      paste0("10 Local Authorities with greatest\nreduction potential:\n", 
             cts_plot$LAD22NM[1],": ", round(cts_plot$total_cost[1],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[2],": ", round(cts_plot$total_cost[2],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[3],": ", round(cts_plot$total_cost[3],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[4],": ", round(cts_plot$total_cost[4],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[5],": ", round(cts_plot$total_cost[5],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[6],": ", round(cts_plot$total_cost[6],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[7],": ", round(cts_plot$total_cost[7],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[8],": ", round(cts_plot$total_cost[8],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[9],": ", round(cts_plot$total_cost[9],1), " (\u00A3m)\n",
             cts_plot$LAD22NM[10],": ", round(cts_plot$total_cost[10],1), " (\u00A3m)\n"),
      bg = TRUE, size = 0.7, bg.alpha = 0.3, bg.color = "grey95", position = c(0.9,0.6)
    )
  
  
}

#' Choropleth map of collision costs by Local Authority
#'
#' Creates a choropleth of collision or casualty costs by Local Authority,
#' with an inset table listing the top 10 LAs with greatest reduction
#' potential.
#'
#' @param region_sf An \code{sf} data frame of LA boundaries with cost columns.
#' @param variable Character. Column to plot. One of \code{"total_cost_col"},
#'   \code{"total_cost_cas"}, or \code{"total_cost"}.
#' @param title Character. Map title, or \code{NULL} to auto-generate from
#'   \code{variable}.
#' @param legend_title Character. Legend title, or \code{NULL} for
#'   \code{"Value (GBP million)"}.
#' @param palette Character. Colour palette name. Default
#'   \code{"wes.zissou1"}.
#' @param breaks_style Character. Break style for \code{tm_scale_intervals()}.
#'   Default \code{"cat"}.
#' @return A \code{tmap} object.
#' @examples
#' \dontrun{
#' tm <- map_la_costs(la_costs_sf, variable = "total_cost",
#'                    breaks_style = "log10_pretty")
#' }
#' @export
map_la_costs = function(region_sf, variable = c("total_cost_col", "total_cost_cas", "total_cost"),title = NULL, legend_title = NULL,
                        home_LA = la_name,start_year = base_year,end_year = upper_year,palette =  "wes.zissou1", breaks_style = c("cat", "fixed","log10_pretty")){

  
  if(is.null(title)){
    legend_title = "Value (\u00A3million)"
  }
  
  if(start_year == end_year){
    
    region_sf <- region_sf |> 
      filter(collision_year == year) |> 
      select(LAD22NM, {{variable}}) |> 
      arrange(desc(.data[[variable]])) |> 
      mutate(rank = as.numeric(min_rank(desc(.data[[variable]]))))
    
   title = paste0(v," ", cas_nam, " costs in ", end_year)
    
  } else {
    
    region_sf <- region_sf |> 
      select(LAD22NM, {{variable}}) |> 
      group_by(LAD22NM) |> 
      summarise(!!variable := sum(.data[[variable]])) |> 
      arrange(desc(.data[[variable]])) |> 
      mutate(rank = as.numeric(min_rank(desc(.data[[variable]]))))
    
    title = paste(v, cas_nam, "costs between", start_year, "and", end_year)
    
  }
  
  la_df = region_sf |> filter(grepl(home_LA,LAD22NM))
  lr = sprintf("%03d",la_df$rank)
  
  if(la_df$rank > 10){
    ranking_table = paste0("10 Local Authorities with greatest\nreduction potential:\n", 
                                  "001: ", region_sf$LAD22NM[1],": ", round(region_sf[[variable]][1],1), " \n",
                                  "002: ", region_sf$LAD22NM[2],": ", round(region_sf[[variable]][2],1), " \n",
                                  "003: ", region_sf$LAD22NM[3],": ", round(region_sf[[variable]][3],1), " \n",
                                  "004: ", region_sf$LAD22NM[4],": ", round(region_sf[[variable]][4],1), " \n",
                           "005: ", region_sf$LAD22NM[5],": ", round(region_sf[[variable]][5],1), " \n",
                           "006: ", region_sf$LAD22NM[6],": ", round(region_sf[[variable]][6],1), " \n",
                           "007: ", region_sf$LAD22NM[7],": ", round(region_sf[[variable]][7],1), " \n",
                           "008: ", region_sf$LAD22NM[8],": ", round(region_sf[[variable]][8],1), " \n",
                           "009: ", region_sf$LAD22NM[9],": ", round(region_sf[[variable]][9],1), " \n",
                           "010: ", region_sf$LAD22NM[10],": ", round(region_sf[[variable]][10],1), " \n",
                           lr," ", la_df$LAD22NM,": ", round(la_df[[variable]],1), " \n")
  } else {
    ranking_table = paste0("10 Local Authorities with greatest\nreduction potential:\n",
                                  "01: ", region_sf$LAD22NM[1],": ", round(region_sf[[variable]][1],1), " \n",
                           "02: ", region_sf$LAD22NM[2],": ", round(region_sf[[variable]][2],1), " \n",
                           "03: ", region_sf$LAD22NM[3],": ", round(region_sf[[variable]][3],1), " \n",
                           "04: ", region_sf$LAD22NM[4],": ", round(region_sf[[variable]][4],1), " \n",
                           "05: ", region_sf$LAD22NM[5],": ", round(region_sf[[variable]][5],1), " \n",
                           "06: ", region_sf$LAD22NM[6],": ", round(region_sf[[variable]][6],1), " \n",
                           "07: ", region_sf$LAD22NM[7],": ", round(region_sf[[variable]][7],1), " \n",
                           "08: ", region_sf$LAD22NM[8],": ", round(region_sf[[variable]][8],1), " \n",
                           "09: ", region_sf$LAD22NM[9],": ", round(region_sf[[variable]][9],1), " \n",
                           "10: ", region_sf$LAD22NM[10],": ", round(region_sf[[variable]][10],1), " \n")
  }
  
  
  tm1 <- tm_shape(region_sf) +
    tm_polygons(fill = variable,
                fill.scale = tm_scale_intervals(values = palette, style = breaks_style),
                fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                lwd = 0.1
    )+
    #tm_text("name",size = 1)
    tm_title(title,size = 2)+
    tm_layout(frame = FALSE)+
    tm_credits(ranking_table,
      bg = TRUE, size = 0.7, bg.alpha = 0.3, bg.color = "grey95", position = c(0.9,0.6)
    )
  
  
  return(tm1)
  
}

#' Choropleth map of casualty/vehicle home locations by LSOA
#'
#' Creates a choropleth of person counts by LSOA, showing where casualties
#' or vehicle drivers live. Includes an inset summary table of LSOAs outside
#' the city boundary grouped by distance band, and optionally overlays a
#' raster basemap.
#'
#' @param casualty_df Optional casualty data frame with
#'   \code{lsoa_of_casualty}.
#' @param vehicle_df Optional vehicle data frame with \code{lsoa_of_driver}.
#' @param variable Character. Variable name in \code{summarise_lsoa()} output
#'   to plot.
#' @param lsoa_geo An \code{sf} data frame of LSOA 2021 boundaries.
#' @param city_shp An \code{sf} polygon of the city boundary (BNG CRS).
#' @param credit_title Character. Title for the inset credit text.
#' @param table_header Character. Header for the distance-band table.
#' @param palette Character. Colour palette name. Default
#'   \code{"wes.zissou1"}.
#' @param base_year Integer. Start year for filtering.
#' @param end_year Integer. End year for filtering.
#' @param info_position Numeric vector of length 2. Position for inset text.
#' @param bgd_map_buff Numeric. Buffer distance for basemap extent, or
#'   \code{NULL} for no basemap.
#' @param title Character. Map title.
#' @param legend_title Character. Legend title.
#' @return A \code{tmap} object.
#' @examples
#' \dontrun{
#' tm <- map_lsoa_home(casualty_df = casualties, lsoa_geo = lsoa_sf,
#'                     city_shp = city_sf, variable = "persons",
#'                     credit_title = "Casualty home LSOA",
#'                     table_header = "Distance (km)  Persons",
#'                     base_year = 2019, end_year = 2024,
#'                     title = "Where do casualties live?")
#' }
#' @export
map_lsoa_home <- function(casualty_df = NULL, vehicle_df = NULL, 
                          variable,
                          lsoa_geo,
                          city_shp,
                          bgd_map_buff = 0,
                          bgd_map = FALSE, 
                          palette =  "wes.zissou1",
                          city_only = TRUE,
                          base_year = 2020, 
                          end_year = 2024,
                          info_position = c(0,0.2)){
  
  if(!is.null(casualty_df)){
    lsoa_all = summarise_lsoa(casualties = casualty_df,lsoa_geo = lsoa_geo,city_shp = city_shp,base_year = base_year, end_year = end_year)
    legend_title = "casualties"
    title = paste0("Home LSOA area for all casualties between ", base_year, " and ", end_year)
    credit_title = "Distance of home LSOA\nof casualties outside LA:"
    table_header = "distance (km)   casualties"
    total_persons <- NROW(casualty_df)
    lsoa_missing <-  NROW(filter(casualty_df, lsoa_of_casualty == "-1"))
  }
  if(!is.null(vehicle_df)){
    lsoa_all = summarise_lsoa(vehicles = vehicle_df,lsoa_geo = lsoa_geo,city_shp = city_shp,base_year = base_year, end_year = end_year)
    legend_title = "drivers"
    title = paste0("Home LSOA area for all drivers involved in collisions between ", base_year, " and ", end_year)
    credit_title = "Distance of home LSOA\nof drivers outside LA:"
    table_header = "distance (km)   drivers"
    total_persons <- NROW(vehicle_df)
    lsoa_missing <-  NROW(filter(vehicle_df, lsoa_of_driver == "-1"))
  }
  
  lsoa_city <- filter(lsoa_all, is.na(dist2city_km))
  
  lsoa_outside_city <- lsoa_all |>
    filter(!is.na(dist2city_km)) |>
    st_set_geometry(NULL) |>
    group_by(distances) |>
    summarise(persons = sum(persons))
  
  if(isTRUE(city_only)){
    
    lsoa_plot = lsoa_city
    
  } else {
    lsoa_plot = lsoa_all
  }
  
  tmap_mode("plot")
  
  if(isTRUE(bgd_map)){
    
    city_buff <- st_buffer(city_shp,bgd_map_buff)
    
    bm_ps <- basemaps::basemap_raster(ext = city_buff,map_service = "carto", map_type = "light")
    
    tm1 <- tm_shape(bm_ps)+
      tm_rgb()
    
    alp = 0.7
    
  } else {
    tm1 = tm_shape(city_shp) +
      tm_polygons(fill_alpha = 0, col_alpha = 0)
    
    alp = 1}
  
  if(max(lsoa_plot$persons)>1){
  
  tm1 = tm1+
    tm_shape(lsoa_plot) +
    tm_polygons(fill = "persons",fill_alpha = alp,
                fill.scale = tm_scale_intervals(values = palette),
                fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                lwd = 0.1)
  
  } else {
    
    col4one = c4a(palette)[1]
  
  tm1 = tm1+
    tm_shape(lsoa_plot) +
    tm_polygons(fill = col4one,
                fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                lwd = 0.1)
  
  }
  
    tm1 = tm1 + 
      tm_credits(
      credit_title,
      position = info_position,
      fontface = "bold") +
    tm_credits(
      paste0(sprintf("%-15s %s", table_header, ""),"\n",
             sprintf("%-19s %6d", lsoa_outside_city$distances[1], lsoa_outside_city$persons[1]),"\n",
             sprintf("%-17s %6d", lsoa_outside_city$distances[2], lsoa_outside_city$persons[2]),"\n",
             sprintf("%-16s %6d", lsoa_outside_city$distances[3], lsoa_outside_city$persons[3]),"\n",
             sprintf("%-16s %6d", lsoa_outside_city$distances[4], lsoa_outside_city$persons[4]),"\n",
             sprintf("%-16s %6d", lsoa_outside_city$distances[5], lsoa_outside_city$persons[5]),"\n",
             sprintf("%-16s %6d", "total (inc LA)", total_persons),"\n",
             sprintf("%-16s %6d", "no data", lsoa_missing)),
      position = info_position,
      fontfamily = "mono")+
    tm_title(title,size = 2)+
    tm_layout(frame = FALSE)
  
  return(tm1)
}

#' Choropleth map of crash counts by LSOA
#'
#' Creates a choropleth of collision counts by LSOA, showing where crashes
#' occurred. Uses \code{summarise_lsoa()} to aggregate and joins to LSOA
#' boundaries.
#'
#' @param crashes_df An \code{sf} data frame of crash records.
#' @param lsoa_geo An \code{sf} data frame of LSOA 2021 boundaries.
#' @param city_shp An \code{sf} polygon of the city boundary.
#' @param palette Character. Colour palette name. Default
#'   \code{"wes.zissou1"}.
#' @param base_year Integer. Start year for filtering. Default \code{2020}.
#' @param end_year Integer. End year for filtering. Default \code{2024}.
#' @param info_position Numeric vector of length 2. Position for credits.
#' @return A \code{tmap} object.
#' @examples
#' \dontrun{
#' tm <- map_lsoa_crashes(crashes, lsoa_sf, city_sf,
#'                        base_year = 2019, end_year = 2024)
#' }
#' @export
map_lsoa_crashes <- function(crashes_df, 
                              lsoa_geo,
                              city_shp,
                              palette =  "wes.zissou1",
                              base_year = 2020, 
                              end_year = 2024,
                              info_position = c(0,0.2)){
  
  
  lsoa_all = summarise_lsoa(collisions = crashes_df,lsoa_geo = lsoa_geo,city_shp = city_shp,base_year = base_year, end_year = end_year)
  legend_title = "crashes"
  title = paste0("collisions by LSOA area between ", base_year, " and ", end_year)
  
  tm1 <- tm_shape(lsoa_all) +
    tm_polygons(fill = "crashes",fill_alpha = 1,
                fill.scale = tm_scale_intervals(values = palette),
                fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                lwd = 0.1)+
    tm_credits(
      "In which LSOA did\ncollisions take place?",
      position = info_position,
      fontface = "bold") +
    tm_title(title,size = 2)+
    tm_layout(frame = FALSE)
  
  return(tm1)
  
}

#' Choropleth map of population by LSOA for a given shape file
#'
#' Creates a choropleth of population by LSOA, showing where crashes
#' occurred. Uses \code{summarise_lsoa()} to aggregate and joins to LSOA
#' boundaries.
#'
#' @param city_shp An \code{sf} polygon of the city boundary.
#' @param lsoa_geo An \code{sf} data frame of LSOA 2021 boundaries.
#' @param palette Character. Colour palette name. Default
#'   \code{"wes.zissou1"}.
#' @param base_year Integer. Start year for filtering. Default \code{2020}.
#' @param end_year Integer. End year for filtering. Default \code{2024}.
#' @param info_position Numeric vector of length 2. Position for credits.
#' @return A \code{tmap} object.
#' @examples
#' \dontrun{
#' tm <- map_lsoa_crashes(city_shp,lsoa_sf, 
#'                        base_year = 2019, end_year = 2024)
#' }
#' @export
map_lsoa_pop <- function(city_shp = city_shp,
                             lsoa_geo = lsoa_boundaries_21,
                             city_sf = city_shp,
                             palette =  "wes.zissou1",
                             base_year = 2020, 
                             end_year = 2024,
                             info_position = c(0,0.2)){

  # read in LSOA population data
  git_pop = read.csv("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/lsoa21_pop_tot_2011_2024.csv")
  
  lsoa_pop = git_pop |> 
    select(-X) |> 
    pivot_longer(-c(lsoa21cd,lsoa21nm), names_to = "year", values_to = "population") |> 
    mutate(year = gsub("X", "", year))
  
  # find centre points to neatly intersect with city shape
  lsoa_centroids = st_centroid(lsoa_geo)
  
  # get lsoas within the city
  city_lsoa = lsoa_centroids[city_sf,]
  
  # reshape for joining below
  city_pop_lsoa = lsoa_pop |> 
    filter(lsoa21cd %in% city_lsoa$LSOA21CD) |> 
    dplyr::filter(year >= base_year & year <= end_year) |> 
    group_by(lsoa21cd) |> 
    summarise(mean_pop = round(mean(population)))

  
  city_pop_lsoa_sf = left_join(city_pop_lsoa,lsoa_boundaries_21, by = c("lsoa21cd" = "LSOA21CD"))
  
  st_geometry(city_pop_lsoa_sf) = city_pop_lsoa_sf$geom
  
  
tm1 <- tm_shape(city_pop_lsoa_sf) +
  tm_polygons(fill = "mean_pop",fill_alpha = 1,
              fill.scale = tm_scale_intervals(values = "wes.zissou1", style = "pretty"),
              fill.legend = tm_legend("population", frame = FALSE,legend.border.col = NA),
              lwd = 0.1)+
  tm_title(paste0("Estimated LSOA population between ",base_year, " and ",end_year),size = 2)+
  tm_layout(frame = FALSE)+
  tm_credits("Source: ONS")

return(tm1)

}


#' Choropleth map of casualty counts by Local Authority
#'
#' Creates a choropleth of casualty totals by Local Authority for a given
#' severity and year, with an inset listing the top 10 LAs.
#'
#' @param region_sf An \code{sf} data frame of LA boundaries with casualty
#'   columns (e.g. from \code{summarise_casualties_per_la()}).
#' @param variable Character. Column to plot. One of \code{"fatal_cas"},
#'   \code{"serious_cas"}, \code{"slight_cas"}, \code{"total_cas"},
#'   \code{"ksi_cas"}.
#' @param year Character. Year to filter by. Default \code{"2024"}.
#' @param title Character. Map title, or \code{NULL} to auto-generate.
#' @param legend_title Character. Legend title, or \code{NULL}.
#' @param palette Character. Colour palette name. Default
#'   \code{"wes.zissou1"}.
#' @param breaks_style Character. Break style for \code{tm_scale_intervals()}.
#' @return A \code{tmap} object.
#' @examples
#' \dontrun{
#' tm <- map_la_casualties(la_cas_sf, variable = "ksi_cas", year = "2024")
#' }
#' @export
map_la_casualties <- function(region_sf, variable = c("fatal_cas", "serious_cas", "slight_cas", "total_cas", "ksi_cas"),home_LA = la_name,
                              start_year = base_year, end_year = upper_year, title = NULL, legend_title = NULL,palette = "wes.zissou1", 
                              breaks_style = c("cat", "fixed","log10_pretty")){
  
  # region_sf = LA_casualties
  # variable = v
  # home_LA = la_name
  # start_year = 2024
  # end_year = 2024
  # palette = "wes.zissou1"
  # breaks_style = "kmeans"
  # title = paste0(v_nam," ", cas_nam, " casualties")
  
  if(is.null(title)){
    title = gsub("_cas", "",variable)
  }
  
  if(start_year == end_year){
    
    region_sf <- region_sf |> 
      filter(collision_year == end_year) |> 
      select(LAD22NM, {{variable}}) |> 
      arrange(desc(.data[[variable]])) |> 
      mutate(rank = as.numeric(min_rank(desc(.data[[variable]]))))

    top_title = paste(title, "in", end_year)
    
  } else {
    
    region_sf <- region_sf |> 
      select(LAD22NM, {{variable}}) |> 
      group_by(LAD22NM) |> 
      summarise(!!variable := sum(.data[[variable]])) |> 
      arrange(desc(.data[[variable]])) |> 
      mutate(rank = as.numeric(min_rank(desc(.data[[variable]]))))

    top_title = paste(title, "between", start_year,"and",end_year)
  
  }
  
    la_df = region_sf |> filter(grepl(home_LA,LAD22NM))
    lr = sprintf("%03d",la_df$rank)
    
    if(la_df$rank > 10){
    ranking_table = paste0("001: ", region_sf$LAD22NM[1],": ", round(region_sf[[variable]][1],1), " \n",
           "002: ", region_sf$LAD22NM[2],": ", round(region_sf[[variable]][2],1), " \n",
           "003: ", region_sf$LAD22NM[3],": ", round(region_sf[[variable]][3],1), " \n",
           "004: ", region_sf$LAD22NM[4],": ", round(region_sf[[variable]][4],1), " \n",
           "005: ", region_sf$LAD22NM[5],": ", round(region_sf[[variable]][5],1), " \n",
           "006: ", region_sf$LAD22NM[6],": ", round(region_sf[[variable]][6],1), " \n",
           "007: ", region_sf$LAD22NM[7],": ", round(region_sf[[variable]][7],1), " \n",
           "008: ", region_sf$LAD22NM[8],": ", round(region_sf[[variable]][8],1), " \n",
           "009: ", region_sf$LAD22NM[9],": ", round(region_sf[[variable]][9],1), " \n",
           "010: ", region_sf$LAD22NM[10],": ", round(region_sf[[variable]][10],1), " \n",
           lr," ", la_df$LAD22NM,": ", round(la_df[[variable]],1), " \n")
    } else {
    ranking_table = paste0("01: ", region_sf$LAD22NM[1],": ", round(region_sf[[variable]][1],1), " \n",
                                      "02: ", region_sf$LAD22NM[2],": ", round(region_sf[[variable]][2],1), " \n",
                                      "03: ", region_sf$LAD22NM[3],": ", round(region_sf[[variable]][3],1), " \n",
                                      "04: ", region_sf$LAD22NM[4],": ", round(region_sf[[variable]][4],1), " \n",
                                      "05: ", region_sf$LAD22NM[5],": ", round(region_sf[[variable]][5],1), " \n",
                                      "06: ", region_sf$LAD22NM[6],": ", round(region_sf[[variable]][6],1), " \n",
                                      "07: ", region_sf$LAD22NM[7],": ", round(region_sf[[variable]][7],1), " \n",
                                      "08: ", region_sf$LAD22NM[8],": ", round(region_sf[[variable]][8],1), " \n",
                                      "09: ", region_sf$LAD22NM[9],": ", round(region_sf[[variable]][9],1), " \n",
                                      "10: ", region_sf$LAD22NM[10],": ", round(region_sf[[variable]][10],1), " \n")
    }
  
  tm1 <- tm_shape(region_sf) +
    tm_polygons(fill = variable,
                fill.scale = tm_scale_intervals(values = palette, style = breaks_style),
                fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                lwd = 0.1
    )+
    #tm_text("name",size = 1)
    tm_title(top_title,size = 2)+
    tm_layout(frame = FALSE)+
    tm_credits(
      paste0("10 Local Authorities with\nmost ", title,":\n"),
      position = c(0.9,0.6),
      fontface = "bold")+
    tm_credits(ranking_table,
               bg = TRUE, size = 0.7, bg.alpha = 0.3, bg.color = "grey95", position = c(0.9,0.6)
    )

  return(tm1)
  
}



#' Clockboard-style collision map
#'
#' Placeholder function. Will create a clockboard visualisation of collision
#' patterns at SOA level, faceted by variable (cost, casualties, weather,
#' light, etc.).
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param casualties Casualty data frame.
#' @param soa_size Character. One of \code{"lsoa"} or \code{"msoa"}.
#' @param var2plot Character. Variable to visualise.
#' @return Currently returns \code{NULL}.
#' @examples
#' \dontrun{
#' map_clockboard_collisions(crashes, casualties, soa_size = "lsoa",
#'                           var2plot = "casualties")
#' }
#' @export
map_clockboard_collisions <- function(crashes, casualties, soa_size = c("lsoa", "msoa"), 
                                           var2plot = c("cost", "casualties", "vehicles", "weather", "light", "road_surface", "day_of_week", "hour", "month")){NULL}

# soa_collision_summaries <- function(crashes, casualties, soa_size = c("lsoa", "msoa"), casualty_type = "All",
#                                     var2plot = c("cost_per_collision", "number_of_casualties", "number_of_vehicles"), 
#                                     condition = c("weather_conditions", "light_conditions", "road_surface_conditions", "day_of_week", "hour", "month", "All"),
#                                     plot_dir = "plots/"){
#   
#                                       if(soa_size == "lsoa"){
#                                       soa_boundaries_21 <- geographr::boundaries_lsoa21
#                                       }
#                                       if(soa_size == "msoa"){
#                                         soa_boundaries_21 <- geographr::boundaries_msoa21
#                                       }
#   
#   cra_cost <- match_tag(crashes = crashes, match_with = "severity")
#   
#   if(condition == "hour"){
#     
#     cra_cost[[condition]] <- lubridate::hour(cra_cost$datetime)
#     
#   }
#   
#   if(condition == "month"){
#     
#     cra_cost[[condition]] <- lubridate::month(cra_cost$datetime)
#     
#   }
#   
#   if(condition == "day_of_week"){
#     
#     cra_cost[[condition]] <- lubridate::wday(cra_cost$datetime, label = TRUE)
#     
#   }
#   
#   
#   var_df <- select(cra_cost,{{var2plot}},{{condition}}, geometry) |> 
#     st_transform(4326) |> 
#     st_join(soa_boundaries_21) |> 
#     st_set_geometry(NULL) |> 
#     group_by(lsoa21_name,!!sym(condition)) |> 
#     summarise(tot_cost = sum(cost_per_collision)/1e6)
#   
#   bks <- seq(min(var_df$tot_cost),max(var_df$tot_cost), by = c(max(var_df$tot_cost)-min(var_df$tot_cost))/10)
#   
#   varz <- unique(var_df[[condition]])
#   plot_list <- list()
#   for (c in varz){
#     
#     var_df_c <- var_df |> 
#       filter(!!sym(condition) == c) |> 
#       left_join(soa_boundaries_21, by = "lsoa21_name") 
#     
#     bks <- seq(min(var_df_c$tot_cost),max(var_df_c$tot_cost), by = c(max(var_df_c$tot_cost)-min(var_df_c$tot_cost))/10)
#     
#     st_geometry(var_df_c) <- var_df_c$geometry
#     
#     tm <- tm_shape(var_df_c) +
#       tm_polygons(fill = "tot_cost",
#                   fill.scale = tm_scale_intervals(values = "wes.zissou1", breaks = bks),
#                   fill.legend = tm_legend("Value (£million)", frame = FALSE,legend.border.col = NA),
#                   lwd = 0.1
#       )+
#       #tm_text("name",size = 1)
#       tm_title(paste0(c))+
#       tm_layout(frame = FALSE)
#     
#     tmap_save(tm, paste0(plot_dir, soa_size,"_", c, ".png"))
#   
#     #assign(paste0("tm_", which(c == varz)), tm)
#     plot_list[[as.character(c)]] <- tm
#   }
#   
#   map_out <- do.call(tmap_arrange,plot_list)
#   
#   tmap_save(map_out, paste0(plot_dir, soa_size,"_", condition, ".png"))
#   
# }
