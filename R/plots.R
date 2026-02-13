#' Create a city boundary map with basemap background
#'
#' `city_plot()` renders a basemap clipped to a city polygon and overlays the
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
#' city_plot(city_shape = my_city_sf, city_name = "MyCity")
#' }
#' @export
city_plot <- function(city_shape,
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

# define the colours for the plot
cols <- rev(c("#00ab3d", "#005bb2","#c81329"))
cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, ggplot2::scale_color_manual(values = cols))


#' Plot cycling network with segregation classification and summary stats
#'
#' `cycle_network_plot()` creates a map of cycling infrastructure for a city,
#' classifies segregated vs non-segregated paths, overlays them on a basemap,
#' annotates the map with summary statistics (from `cycle_network_stats`) and
#' saves the result as a PNG. The function returns the `tmap` object invisibly.
#'
#' **Note:** This function relies on helper functions that are not included
#' here: `cycle_network_stats()`, `distance_to_road()`, and
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
#' cycle_network_plot("MyCity", my_city_sf, my_osm_data, city_pop = 200000)
#' }
#' @export
cycle_network_plot <- function(city,
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
  stats <- cycle_network_stats(city = city, city_shape = city_shape, osm_data = osm_data, city_pop = city_pop)
  
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
  tit_pos <- if (title_position == "left") c(0, 0.99) else c(0.74, 0.98)
  cred_pos <- if (stats_position == "left") c(0.01, 0.93) else c(0.75, 0.93)
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
    tmap::tm_layout(frame = FALSE)
  
  out_file <- file.path(plot_dir, paste0(city, "_cycle_paths.png"))
  tmap::tmap_save(tm1, out_file, dpi = dpi)
  
  invisible(tm1)
}

#' Plot casualty indexes over time
#'
#' `index_plot()` creates a line chart of casualty indexes by severity,
#' normalised to a base year. It saves the chart as a PNG and returns the
#' reshaped data invisibly.
#'
#' @param indexes A data frame with a `year` column and one or more index
#'   columns (numeric).
#' @param base_year Integer. The first year shown and the reference year for
#'   index normalisation.
#' @param end_year Integer. The last year shown on the x-axis.
#' @param pal Character vector of colours for the severity categories.
#'   Default `c("#ff7733", "#1de9b6","#006853")`.
#' @param city Character. City name used in the plot title and filename.
#' @param plot_dir Character. Directory where the PNG will be saved. Created if
#'   it does not exist. Default `"plots/"`.
#' @return Invisibly returns the reshaped data frame used for plotting.
#' @examples
#' \dontrun{
#' index_plot(my_indexes, base_year = 2010, end_year = 2024, city = "London")
#' }
#' @export
index_plot <- function(indexes,
                       base_year,
                       end_year,
                       pal = c("#ff7733", "#1de9b6", "#006853"),
                       city,
                       plot_dir = "plots/") {
  stopifnot(is.data.frame(indexes))
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  chart_data <- reshape2::melt(indexes, "year")
  
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_color_manual(values = pal))
  
  p <- chart_data %>%
    ggplot2::ggplot(ggplot2::aes(year, value, color = variable)) +
    ggplot2::geom_line(size = 2, alpha = .8) +
    dft_theme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = seq(base_year, end_year, by = 1),
                                name = NULL) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dotted", col = "black") +
    ggplot2::ggtitle(
      paste0("Index of casualties by severity, ",
             city, ": ", base_year, " - ", end_year,
             " (Index ", base_year, " = 100)")
    ) +
    ggplot2::ylab("index") +
    ggplot2::labs(caption = "Source: Stats19")
  
  out_file <- file.path(plot_dir, paste0(city, "_index.png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(chart_data)
}


#' Plot casualty demographics by age and sex
#'
#' `demog_plot()` creates a bar chart showing casualty percentages by age band
#' and sex, either for all severities or for KSI (Killed or Seriously Injured).
#' It saves the chart as a PNG.
#'
#' @param casualties A data frame of casualty records suitable for
#'   `group_demo()`.
#' @param pal Character vector of fill colours. Default
#'   `c("#ff7733", "#1de9b6","#006853")`.
#' @param city Character. City name used in the plot title and filename.
#' @param severity Character. One of `"all"` or `"ksi"`. Controls which
#'   severities are aggregated. Default `"all"`.
#' @param stat Character. One of `"pc"` (percentage). Default `"pc"`.
#' @param plot_dir Character. Directory where the PNG will be saved. Created if
#'   it does not exist. Default `"plots/"`.
#' @return Invisibly returns the aggregated demographic data used for plotting.
#' @examples
#' \dontrun{
#' demog_plot(my_casualties, city = "London", severity = "ksi")
#' }
#' @export
demog_plot <- function(casualties,
                       pal = c("#ff7733", "#1de9b6", "#006853"),
                       city,
                       severity = c("all", "ksi"),
                       stat = c("pc"),
                       plot_dir = "plots/") {
  severity <- match.arg(severity)
  stat <- match.arg(stat)
  
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  demo_sum <- if (severity == "ksi") {
    group_demo(casualties, demographic = "both", severities = c("Fatal", "Serious"))
  } else {
    group_demo(casualties, demographic = "both", severities = c("Fatal", "Serious", "Slight"))
  }
  
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
  
  if (stat == "ksi") {
    p <- ggplot2::ggplot(demo_sum, ggplot2::aes(x = age_band, y = pc_ksi, fill = sex_of_casualty)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.7), width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(pc_ksi), "%")),
        position = ggplot2::position_dodge(width = 0.7),
        vjust = -0.5, size = 3
      ) +
      ggplot2::ggtitle(paste0("Percentage of KSI casualties, by sex and age, ", city, ": 2010 to 2024")) +
      dft_theme +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "top",
                     legend.title = ggplot2::element_blank()) +
      ggplot2::ylab(NULL) + ggplot2::xlab(NULL) +
      ggplot2::labs(caption = "Source: Stats19")
  } else {
    p <- ggplot2::ggplot(demo_sum, ggplot2::aes(x = age_band, y = pc_all, fill = sex_of_casualty)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.7), width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(pc_all), "%")),
        position = ggplot2::position_dodge(width = 0.7),
        vjust = -0.5, size = 3
      ) +
      ggplot2::ggtitle(paste0("Percentage of all casualties, by sex and age, ", city, ": 2010 to 2024")) +
      dft_theme +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "top",
                     legend.title = ggplot2::element_blank()) +
      ggplot2::ylab(NULL) + ggplot2::xlab(NULL) +
      ggplot2::labs(caption = "Source: Stats19")
  }
  
  out_file <- file.path(plot_dir, paste0(city, "_demographic.png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(demo_sum)
}

#' Plot yearly casualty totals by severity
#'
#' `year_sum_plot()` produces a bar chart of casualty counts by severity and
#' year, saving the result as a PNG.
#'
#' @param casualties A data frame of casualty records suitable for
#'   `casualties_rates()`.
#' @param pal Character vector of fill colours. Default
#'   `c("#ff7733", "#1de9b6","#006853")`.
#' @param city Character. City name used in the plot title and filename.
#' @param severity Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @param plot_dir Character. Directory where the PNG will be saved. Created if
#'   it does not exist. Default `"plots/"`.
#' @return Invisibly returns the reshaped data frame used for plotting.
#' @examples
#' \dontrun{
#' year_sum_plot(my_casualties, city = "London")
#' }
#' @export
year_sum_plot <- function(casualties,
                          pal = c("#ff7733", "#1de9b6", "#006853"),
                          city,
                          severity = c("Fatal", "Serious", "Slight"),
                          plot_dir = "plots/") {
  stopifnot(is.data.frame(casualties))
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  cas_rates <- casualties_rates(casualties)
  
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
  
  year_count <- cas_rates %>%
    reshape2::melt("collision_year") %>%
    dplyr::filter(variable %in% severity) %>%
    dplyr::mutate(collision_year = as.character(collision_year))
  
  p <- ggplot2::ggplot(year_count, ggplot2::aes(x = collision_year, y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.7), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(value))),
      position = ggplot2::position_dodge(width = 0.7),
      vjust = -0.5, size = 3
    ) +
    ggplot2::ggtitle(
      paste0("Total casualties by severity (", paste(severity, collapse = ", "),
             ") and year, ", city, ": 2010 to 2024")
    ) +
    dft_theme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.title = ggplot2::element_blank()) +
    ggplot2::ylab(NULL) + ggplot2::xlab(NULL) +
    ggplot2::labs(caption = "Source: Stats19")
  
  out_file <- file.path(plot_dir, paste0(city, "_year_totals.png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(year_count)
}

#' Plot casualty percentages by crash condition
#'
#' Produces a bar chart showing the percentage of casualties by severity across
#' a chosen crash condition (e.g. road surface, junction detail, speed limit).
#'
#' @param crashes An `sf` data frame of crash records.
#' @param casualties A data frame of casualty records suitable for
#'   `casualties_single_row()`.
#' @param city Character. City name used in the plot title and filename.
#'   Default `"Bristol"`.
#' @param severities Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @param parameter Character. One of `"road_surface_conditions"`,
#'   `"junction_detail"`, `"speed_limit"`, `"light_conditions"`,
#'   `"weather_conditions"`. Default `"road_surface_conditions"`.
#' @param plot_width Numeric. Width of the saved PNG in inches. Default `10`.
#' @param plot_dir Character. Directory where the PNG will be saved. Created if
#'   it does not exist. Default `"plots/"`.
#' @return Invisibly returns the aggregated data frame used for plotting.
#' @export
crash_conditions_plot <- function(crashes,
                                  casualties,
                                  city = "Bristol",
                                  severities = c("Fatal", "Serious", "Slight"),
                                  parameter = c("road_surface_conditions", "junction_detail",
                                                "speed_limit", "light_conditions", "weather_conditions"),
                                  plot_width = 10,
                                  plot_dpi = 400,
                                  plot_dir = "plots/") {
  parameter <- match.arg(parameter)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  cas_summary <- casualties_single_row(casualties)
  
  pal_sev <- data.frame(
    pal = c("#ff7733", "#1de9b6", "#006853"),
    severity = c("Fatal", "Serious", "Slight")
  )
  pal <- pal_sev$pal[pal_sev$severity %in% severities]
  
  cra_cas <- crashes %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(collision_index) %>%
    dplyr::left_join(cas_summary, by = "collision_index") %>%
    reshape2::melt(c("collision_index", "collision_year")) %>%
    dplyr::filter(value > 0)
  
  crashes_dat <- crashes %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(collision_index, collision_year, speed_limit, time, day_of_week,
                  first_road_number, junction_detail, first_road_class,
                  second_road_number, second_road_class, light_conditions,
                  weather_conditions, datetime, road_surface_conditions)
  
  cra_cas_cond <- cra_cas %>%
    dplyr::left_join(crashes_dat, by = "collision_index")
  
  crash_parameter <- cra_cas_cond %>%
    dplyr::filter(variable %in% severities) %>%
    dplyr::group_by(dplyr::across(all_of(parameter)), variable) %>%
    dplyr::summarise(casualties = sum(value), .groups = "drop") %>%
    dplyr::mutate(pc_ksi = (casualties / sum(casualties)) * 100)
  
  start_year <- min(crashes_dat$collision_year, na.rm = TRUE)
  end_year <- max(crashes_dat$collision_year, na.rm = TRUE)
  
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
  
  p <- ggplot2::ggplot(crash_parameter,
                       ggplot2::aes(x = .data[[parameter]], y = pc_ksi, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.7), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(pc_ksi), "%")),
      position = ggplot2::position_dodge(width = 0.7),
      vjust = -0.5, size = 3
    ) +
    ggplot2::ggtitle(
      paste0("Percentage of ", paste(severities, collapse = ", "),
             " casualties, by ", gsub("_", " ", parameter),
             ", ", city, ": (", start_year, " to ", end_year, ")")
    ) +
    dft_theme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::ylab(NULL) + 
    ggplot2::xlab(NULL) +
    ggplot2::labs(caption = "Source: Stats19")
  
  out_file <- file.path(plot_dir, paste0(city, "_", parameter,"_", paste(gsub(",","_", severities),collapse = "_"), ".png"))
  ggplot2::ggsave(out_file, plot = p, width = plot_width, height = plot_width*0.9, dpi = plot_dpi)
  
  invisible(crash_parameter)
}

#' Plot KSIs by hour of day and day of week
#'
#' @param crash_time Data frame with columns `collision_hr`, `KSI`, and `dow`.
#' @param report_casualty Character string used in the title (e.g. "casualties").
#' @param yr2calc Integer. End year of the period.
#' @param city Character. City name for title. Default `"Bristol"`.
#' @param plot_dir Directory to save PNG. Default `"plots/"`.
#' @return Invisibly returns the plot object.
#' @export
time_date_plot <- function(crash_time,
                           report_casualty,
                           yr2calc,
                           city = "Bristol",
                           plot_dir = "plots/") {
  cols <- rev(c("#ff7733", "#1de9b6", "#006853"))
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_color_manual(values = cols))
  
  p <- crash_time %>%
    ggplot2::ggplot(ggplot2::aes(collision_hr, KSI, color = dow)) +
    ggplot2::geom_line(size = 2, alpha = .8) +
    dft_theme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "top", legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle(
      paste0("Chart 4: Reported ", tolower(report_casualty),
             " KSIs by hour of day and day of week, GB: ",
             yr2calc - 4, " to ", yr2calc)
    ) +
    ggplot2::ylab(NULL) +
    ggplot2::labs(x = "Hour starting", caption = "Source: Stats19")
  
  out_file <- file.path(plot_dir, paste0(city, "_time_date.png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(p)
}

#' Plot annual TAG costs
#'
#' Creates a stacked bar chart of annual prevention costs of collisions
#' (TAG values), saving the result as a PNG.
#'
#' @param crashes Crash data frame.
#' @param casualties Casualty data frame.
#' @param city Character. City name for title and filename.
#' @param plot_dir Directory to save PNG. Default `"plots/"`.
#' @return Invisibly returns the reshaped data frame used for plotting.
#' @export
tag_plot <- function(crashes,
                     agg_level,
                     city,
                     plot_dir = "plots/") {
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  tag_df <- tag_costs(crashes, agg_level)
  
  if(agg_level == "severity"){
  
  chart_0 <- tag_df |> 
    st_set_geometry(NULL) |> 
    dplyr::select(-total_cost, -total_casualties, -collision_severity) |> 
    reshape2::melt(c("collision_year")) |> 
    dplyr::mutate(value = value / 1e6,   # convert to millions
                  variable = gsub("_", " ", variable))
  
  names(chart_0) <- c("year", "cost category", "cost")
  
  pal <- c4a("carto.pastel", n = length(unique(chart_0$`cost category`)))
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
  
  sy <- min(chart_0$year, na.rm = TRUE)
  ey <- max(chart_0$year, na.rm = TRUE)
  
  p <- ggplot2::ggplot(chart_0, ggplot2::aes(x = year, y = cost, fill = `cost category`)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = NA),   # placeholder, no labels
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3
    ) +
    ggplot2::ggtitle(
      paste0("Annual value of prevention of collisions split by collision and casualty cost in ", city,
             " between ", sy, " and ", ey),
      subtitle = "Calculated using collision data from DfT STATS19 and cost data from TAG"
    ) +
    dft_theme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "top",
                   plot.title    = element_text(size = 12),
                   plot.subtitle = element_text(size = 8),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::ylab("(£ million)") +
    ggplot2::xlab(NULL) +
    ggplot2::labs(caption = "Source: Stats19 and TAG")
  
  out_file <- file.path(plot_dir, paste0(city, "_tag_costs.png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(chart_0)
  }
  
  if(agg_level == "severity_road"){
      
      chart_0 <- tag_df |> 
        ungroup() |> 
        dplyr::select(-collision_severity) |> 
        reshape2::melt(c("collision_year")) |> 
        dplyr::mutate(value = value / 1e6,   # convert to millions
                      variable = gsub("_", " ", variable))
      
      names(chart_0) <- c("year", "road type", "cost")
      
      pal <- c4a("carto.pastel", n = length(unique(chart_0$`road type`)))
      cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
      dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
      
      sy <- min(chart_0$year, na.rm = TRUE)
      ey <- max(chart_0$year, na.rm = TRUE)
      
      p <- ggplot2::ggplot(chart_0, ggplot2::aes(x = year, y = cost, fill = `road type`)) +
        ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(label = NA),   # placeholder, no labels
          position = ggplot2::position_stack(vjust = 0.5),
          size = 3
        ) +
        ggplot2::ggtitle(
          paste0("Annual value of prevention of collisions by road type in ", city,
                 " between ", sy, " and ", ey),
          subtitle = "Calculated using collision data from DfT STATS19 and cost data from TAG"
        ) +
        dft_theme +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       legend.position = "top",
                       legend.title = ggplot2::element_blank()) +
        ggplot2::ylab("(£ million)") +
        ggplot2::xlab(NULL) +
        ggplot2::labs(caption = "Source: Stats19 and TAG")
      
      out_file <- file.path(plot_dir, paste0(city, "_tag_costs_road.png"))
      ggplot2::ggsave(out_file, plot = p)
      
      invisible(chart_0)
    }
    
    
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
#' @export
casualties_plot <- function(crashes,
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

#' Plot casualty percentages by type
#'
#' Creates a bar chart showing casualty percentages by casualty type and
#' severity.
#'
#' @param crashes Crash data frame.
#' @param casualties Casualty data frame.
#' @param city Character. City name for title and filename. Default `"Bristol"`.
#' @param severities Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @param plot_width Numeric. Width of saved PNG. Default `20`.
#' @param plot_height Numeric. Height of saved PNG. Default `20`.
#' @param plot_dpi Numeric. Resolution of saved PNG. Default `50`.
#' @param cas_type Character. One of `"casualty_type"`, `"short_name"`,
#'   `"in_or_on"`. Default `"short_name"`.
#' @param plot_dir Directory to save PNG. Default `"plots/"`.
#' @return Invisibly returns the aggregated data frame used for plotting.
#' @export
casualty_type_plot <- function(crashes,
                               casualties,
                               city = "Bristol",
                               severities = c("Fatal", "Serious", "Slight"),
                               plot_width = 11,
                               plot_height = 11,
                               plot_dpi = 200,
                               cas_type = c("casualty_type", "short_name", "in_or_on"),
                               plot_dir = "plots/") {
  cas_type <- match.arg(cas_type)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  pal_sev <- data.frame(pal = c("#ff7733", "#1de9b6", "#006853"),
                        severity = c("Fatal", "Serious", "Slight"))
  pal <- pal_sev$pal[pal_sev$severity %in% severities]
  
  sy <- min(casualties$collision_year, na.rm = TRUE)
  ey <- max(casualties$collision_year, na.rm = TRUE)
  
  casualties_map <- summarise_casualty_types(casualties, summary_type = cas_type) %>%
    dplyr::select(collision_index,
                  casualty_type = !!rlang::sym(cas_type),
                  pedestrian_location, fatal_count,
                  casualty_adjusted_severity_serious,
                  casualty_adjusted_severity_slight) %>%
    dplyr::group_by(collision_index, casualty_type) %>%
    dplyr::summarise(Fatal = sum(fatal_count),
                     Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
                     Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::left_join(crashes, by = "collision_index") %>%
    dplyr::select(casualty_type, Fatal, Serious, Slight, geometry) %>%
    sf::st_as_sf()
  
  casualty_type_df <- casualties_map %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(casualty_type) %>%
    dplyr::summarise(Fatal = sum(Fatal),
                     Serious = sum(Serious),
                     Slight = sum(Slight),
                     .groups = "drop") %>%
    reshape2::melt("casualty_type") %>%
    dplyr::filter(variable %in% severities) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pc_total = (value / sum(value)) * 100)
  
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
  
  p <- ggplot2::ggplot(casualty_type_df,
                       ggplot2::aes(x = casualty_type, y = pc_total, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.7), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(pc_total), "%")),
      position = ggplot2::position_dodge(width = 0.7),
      vjust = -0.5, size = 3
    ) +
    ggplot2::ggtitle(
      paste0("Percentage of casualties, by casualty type, ",
             city, ": ", sy, " to ", ey)
    ) +
    dft_theme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,vjust = 1)) +
    ggplot2::ylab(NULL) + ggplot2::xlab(NULL) +
    ggplot2::labs(caption = "Source: Stats19")
  
  out_file <- file.path(plot_dir,
                        paste0(city, "_", paste(severities, collapse = "_"), "_cas_type.png"))
  ggplot2::ggsave(out_file, plot = p,
                  width = plot_width, height = plot_height, dpi = plot_dpi)
  
  invisible(casualty_type_df)
}

#' Create a waffle plot
#'
#' Produces a waffle chart from a vector or data frame of values, saves the
#' result as a PNG, and returns the plot invisibly.
#'
#' @param data2plot Named numeric vector or data frame of values to plot.
#'   Each element/category will be represented in the waffle chart.
#' @param plot_rows Integer. Number of rows in the waffle chart. Default `3`.
#' @param legend_position Character. Position of legend. Default `"bottom"`.
#' @param title Character. Plot title.
#' @param pal Character. Palette name for `c4a()`. Default `"poly.sky24"`.
#' @param plot_file File path for output PNG. Default `"plots/waffle.png"`.
#' @return Invisibly returns the waffle plot object.
#' @examples
#' \dontrun{
#' waffle_plot(c(Fatal = 50, Serious = 100, Slight = 200),
#'             plot_rows = 5,
#'             title = "Casualties by severity")
#' }
#' @export
waffle_plot <- function(data2plot,
                        plot_rows = 3,
                        legend_position = "bottom",
                        title,
                        pal = "poly.sky24",
                        plot_file = "plots/waffle.png") {
  # get enough colours for the variables
  colz <- c4a(pal, n = NROW(data2plot))
  
  # create waffle plot
  p <- waffle::waffle(data2plot,
                      rows = plot_rows,
                      colors = colz,
                      legend_pos = legend_position,
                      title = title)
  
  # save to file
  ggplot2::ggsave(plot_file, plot = p,dpi = 400)
  
  invisible(p)
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
#' @export
osm_street_casualties_plot <- function(osm_links,
                                       casualties,
                                       crashes,
                                       year_from,
                                       year_to,
                                       bgd_map_buff = c("street", "crashes"),
                                       casualties_buffer,
                                       plot_buffer,
                                       legend_pos = NULL,
                                       plot_dir) {
  
  cra_cas_osm <- casualty_osm_link(osm_links, casualties, crashes,
                                   year_from, year_to,
                                   casualties_buffer = casualties_buffer) |> 
    mutate(sev_plot_size = if_else(Fatal == 1, 1.5, Serious))
  

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
#' @export
osm_street_vehicles_plot <- function(osm_links,
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
  
  
  cra_veh_osm <- vehicle_osm_link(osm_links = osm_links, 
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
                     shape = "vehicle_type",
                     size = "sev_plot_size",
                     size.scale = tmap::tm_scale_continuous(),
                     shape.legend = tmap::tm_legend_combine("fill"),
                     size.legend = tmap::tm_legend(title = "Severity")) +
    tmap::tm_title(
      paste0("Collision location with vehicle type represented by shape and colour",
             "and severity of the collision represented by\nsize, for ",casualty_types, " casualties",
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

# same as for collisions but focused on casualties, i.e. cyclists, pedestrians
soa_casualty_summaries <- function(){}

clockboard_collision_summaries <- function(crashes, casualties, soa_size = c("lsoa", "msoa"), 
                                    var2plot = c("cost", "casualties", "vehicles", "weather", "light", "road_surface", "day_of_week", "hour", "month")){NULL}

soa_collision_summaries <- function(crashes, casualties, soa_size = c("lsoa", "msoa"), casualty_type = "All",
                                    var2measure = c("cost_per_collision", "number_of_casualties", "number_of_vehicles"), 
                                    condition = c("weather_conditions", "light_conditions", "road_surface_conditions", "day_of_week", "hour", "month", "All"),
                                    plot_dir = "plots/"){
  
                                      if(soa_size == "lsoa"){
                                      soa_boundaries_21 <- geographr::boundaries_lsoa21
                                      }
                                      if(soa_size == "msoa"){
                                        soa_boundaries_21 <- geographr::boundaries_msoa21
                                      }
  
  cra_cost <- match_tag(crashes = crashes, match_with = "severity")
  
  if(condition == "hour"){
    
    cra_cost[[condition]] <- lubridate::hour(cra_cost$datetime)
    
  }
  
  if(condition == "month"){
    
    cra_cost[[condition]] <- lubridate::month(cra_cost$datetime)
    
  }
  
  if(condition == "day_of_week"){
    
    cra_cost[[condition]] <- lubridate::wday(cra_cost$datetime, label = TRUE)
    
  }
  
  
  var_df <- select(cra_cost,{{var2plot}},{{condition}}, geometry) |> 
    st_transform(4326) |> 
    st_join(soa_boundaries_21) |> 
    st_set_geometry(NULL) |> 
    group_by(lsoa21_name,!!sym(condition)) |> 
    summarise(tot_cost = sum(cost_per_collision)/1e6)
  
  bks <- seq(min(var_df$tot_cost),max(var_df$tot_cost), by = c(max(var_df$tot_cost)-min(var_df$tot_cost))/10)
  
  varz <- unique(var_df[[condition]])
  plot_list <- list()
  for (c in varz){
    
    var_df_c <- var_df |> 
      filter(!!sym(condition) == c) |> 
      left_join(soa_boundaries_21, by = "lsoa21_name") 
    
    bks <- seq(min(var_df_c$tot_cost),max(var_df_c$tot_cost), by = c(max(var_df_c$tot_cost)-min(var_df_c$tot_cost))/10)
    
    st_geometry(var_df_c) <- var_df_c$geometry
    
    tm <- tm_shape(var_df_c) +
      tm_polygons(fill = "tot_cost",
                  fill.scale = tm_scale_intervals(values = "tol.rainbow_wh_br", breaks = bks),
                  fill.legend = tm_legend("Value (£million)", frame = FALSE,legend.border.col = NA),
                  lwd = 0.1
      )+
      #tm_text("name",size = 1)
      tm_title(paste0(c))+
      tm_layout(frame = FALSE)
    
    tmap_save(tm, paste0(plot_dir, soa_size,"_", c, ".png"))
  
    #assign(paste0("tm_", which(c == varz)), tm)
    plot_list[[as.character(c)]] <- tm
  }
  
  map_out <- do.call(tmap_arrange,plot_list)
  
  tmap_save(map_out, paste0(plot_dir, soa_size,"_", condition, ".png"))
  
}
  

# function to plot any super output area/local authority
region_tag_plot <- function(region_sf, variable,palette,custom_breaks,title,legend_title){
  
  bks <- c(0,2,6,10,15,30,60,100,200,400)
  
  cts_plot <- cts_city_sf |> 
    filter(collision_year == y) |> 
    arrange(desc(total_cost))
  
  tm1 <- tm_shape(cts_plot) +
    tm_polygons(fill = "total_cost",
                fill.scale = tm_scale_intervals(values = "tol.rainbow_wh_br", breaks = bks),
                fill.legend = tm_legend("Value (£million)", frame = FALSE,legend.border.col = NA),
                lwd = 0.1
    )+
    #tm_text("name",size = 1)
    tm_title(y,size = 2)+
    tm_layout(frame = FALSE)+
    tm_credits(
      paste0("10 Local Authorities with greatest\nreduction potential:\n", 
             cts_plot$LAD22NM[1],": ", round(cts_plot$total_cost[1],1), " (£m)\n",
             cts_plot$LAD22NM[2],": ", round(cts_plot$total_cost[2],1), " (£m)\n",
             cts_plot$LAD22NM[3],": ", round(cts_plot$total_cost[3],1), " (£m)\n",
             cts_plot$LAD22NM[4],": ", round(cts_plot$total_cost[4],1), " (£m)\n",
             cts_plot$LAD22NM[5],": ", round(cts_plot$total_cost[5],1), " (£m)\n",
             cts_plot$LAD22NM[6],": ", round(cts_plot$total_cost[6],1), " (£m)\n",
             cts_plot$LAD22NM[7],": ", round(cts_plot$total_cost[7],1), " (£m)\n",
             cts_plot$LAD22NM[8],": ", round(cts_plot$total_cost[8],1), " (£m)\n",
             cts_plot$LAD22NM[9],": ", round(cts_plot$total_cost[9],1), " (£m)\n",
             cts_plot$LAD22NM[10],": ", round(cts_plot$total_cost[10],1), " (£m)\n"),
      bg = TRUE, size = 0.7, bg.alpha = 0.3, bg.color = "grey95", position = c(0.9,0.6)
    )
  
  
}

# all break options c("cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust","bclust", "fisher", "jenks", "dpih", "headtails", "log10_pretty")

# function to plot any super output area/local authority
LA_plot <- function(region_sf, variable = c("total_cost_col", "total_cost_cas", "total_cost"),title = NULL, legend_title = NULL,
                    palette = "tol.rainbow_wh_br", breaks_style = c("cat", "fixed","log10_pretty")){
  
  if(is.null(title)){
    title = gsub("_", " ", variable)
  }
  
  if(is.null(title)){
  legend_title = "Value (£million)"
  }
  
  region_sf <- region_sf |> 
    select(LAD22NM, {{variable}}, SHAPE) |> 
    arrange(desc(.data[[variable]]))
  
  
  tm1 <- tm_shape(region_sf) +
    tm_polygons(fill = variable,
                fill.scale = tm_scale_intervals(values = palette, style = breaks_style),
                fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                lwd = 0.1
    )+
    #tm_text("name",size = 1)
    tm_title(title,size = 2)+
    tm_layout(frame = FALSE)+
    tm_credits(
      paste0("10 Local Authorities with greatest\nreduction potential:\n", 
             region_sf$LAD22NM[1],": ", round(region_sf[[variable]][1],1), " (£m)\n",
             region_sf$LAD22NM[2],": ", round(region_sf[[variable]][2],1), " (£m)\n",
             region_sf$LAD22NM[3],": ", round(region_sf[[variable]][3],1), " (£m)\n",
             region_sf$LAD22NM[4],": ", round(region_sf[[variable]][4],1), " (£m)\n",
             region_sf$LAD22NM[5],": ", round(region_sf[[variable]][5],1), " (£m)\n",
             region_sf$LAD22NM[6],": ", round(region_sf[[variable]][6],1), " (£m)\n",
             region_sf$LAD22NM[7],": ", round(region_sf[[variable]][7],1), " (£m)\n",
             region_sf$LAD22NM[8],": ", round(region_sf[[variable]][8],1), " (£m)\n",
             region_sf$LAD22NM[9],": ", round(region_sf[[variable]][9],1), " (£m)\n",
             region_sf$LAD22NM[10],": ", round(region_sf[[variable]][10],1), " (£m)\n"),
      bg = TRUE, size = 0.7, bg.alpha = 0.3, bg.color = "grey95", position = c(0.9,0.6)
    )
  

  return(tm1)
  
}
  

# function to plot any super output area/local authority
LA_plot_cas <- function(region_sf, variable = c("fatal_cas", "serious_cas", "slight_cas", "total_cas", "ksi_cas"),year = "2024",
                        title = NULL, legend_title = NULL,palette = "tol.rainbow_wh_br", breaks_style = c("cat", "fixed","log10_pretty")){
  
  if(is.null(title)){
  title = gsub("_cas", "",variable)
  }
  
  region_sf <- region_sf |> 
    filter(collision_year == year) |> 
    select(LAD22NM, {{variable}}, SHAPE) |> 
    arrange(desc(.data[[variable]]))
  
  top_title = paste(title, "in", year)
  
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
      paste0("10 Local Authorities with\nhighest ", title,":\n", 
             region_sf$LAD22NM[1],": ", round(region_sf[[variable]][1],1), " \n",
             region_sf$LAD22NM[2],": ", round(region_sf[[variable]][2],1), " \n",
             region_sf$LAD22NM[3],": ", round(region_sf[[variable]][3],1), " \n",
             region_sf$LAD22NM[4],": ", round(region_sf[[variable]][4],1), " \n",
             region_sf$LAD22NM[5],": ", round(region_sf[[variable]][5],1), " \n",
             region_sf$LAD22NM[6],": ", round(region_sf[[variable]][6],1), " \n",
             region_sf$LAD22NM[7],": ", round(region_sf[[variable]][7],1), " \n",
             region_sf$LAD22NM[8],": ", round(region_sf[[variable]][8],1), " \n",
             region_sf$LAD22NM[9],": ", round(region_sf[[variable]][9],1), " \n",
             region_sf$LAD22NM[10],": ", round(region_sf[[variable]][10],1), " \n"),
      bg = TRUE, size = 0.7, bg.alpha = 0.3, bg.color = "grey95", position = c(0.9,0.6)
    )
  
  
  return(tm1)
  
}


ranking_plot <- function(crashes,
                         casualties,
                         LA = "Bristol",
                         severities = "KSI",
                         casualty_types = "Cyclist",
                       base_year,
                       end_year,
                       plot_dir = "plots/") {

  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # crashes = crashes_gb
  # casualties = casualties_gb
  # LA = "Bristol"
  # severities = "KSI"
  # casualty_types = "Pedestrian"
  # severities = c("Serious", "Slight")
  # base_year = 2010
  # end_year = 2024
  # pal = c("#ff7733", "#1de9b6", "#006853")
  
  param2get = tolower(paste0(severities, "_rank"))
  
  pal_sev <- data.frame(pal = c("#ff7733", "#1de9b6", "#006853", "red"),
                        severity = c("Fatal", "Serious", "Slight","KSI"))
  
  ranking_df <- casualties_per_LA(casualties = casualties, crashes = crashes, casualty_types = casualty_types) |> 
    filter(grepl(LA, LAD22NM)) |> 
    st_set_geometry(NULL) |> 
    select(year = collision_year, {{param2get}})
  
  if(NROW(param2get)>1){
  chart_data <- reshape2::melt(ranking_df, "year") |> 
    dplyr::mutate(variable = gsub("_rank", "", variable))
  
  pal <- pal_sev$pal[pal_sev$severity %in% severities]
  } else {
    chart_data <- ranking_df |> 
      select(year, value = {{param2get}}) |> 
      mutate(variable = gsub("_rank","", param2get))
    
    pal <- pal_sev$pal[pal_sev$severity %in% severities]
  }
  
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_color_manual(values = pal))
  
  p <- chart_data %>%
    ggplot2::ggplot(ggplot2::aes(year, value, color = variable)) +
    ggplot2::geom_line(size = 2, alpha = .8) +
    dft_theme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.title = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = seq(base_year, end_year, by = 1),
                                name = NULL) +
    #scale_y_continuous(breaks = seq(0, max(value), by = 5))+
    ggplot2::ggtitle(
      paste0("Ranking ",casualty_types, " casualties for ",paste(severities,collapse = " "),  " compared to other LA, ",
             LA, ": ", base_year, " - ", end_year)) +
    ggplot2::ylab("ranking") +
    ggplot2::labs(caption = "Source: Stats19")
  
  out_file <- file.path(plot_dir, paste0(LA, "_rank_", casualty_types,".png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(chart_data)
}


# function to plot any super output area/local authority
lsoa_home_plot <- function(casualty_df = NULL, vehicle_df = NULL, 
                           variable,
                           lsoa_geo,
                           city_shp,
                           bgd_map_buff = 0,
                           bgd_map = FALSE, 
                           palette = "tol.rainbow_wh_br",
                           city_only = TRUE,
                           base_year = 2020, 
                           end_year = 2024,
                           info_position = c(0,0.2)){

  
  if(!is.null(casualty_df)){
    lsoa_all = lsoa_summaries(casualties = casualty_df,lsoa_geo = lsoa_geo,city_shp = city_shp,base_year = base_year, end_year = end_year)
    legend_title = "casualties"
    title = paste0("Home LSOA area for all casualties between ", base_year, " and ", end_year)
    credit_title = "distance (km)   casualties"
    total_persons <- NROW(casualty_df)
    lsoa_missing <-  NROW(filter(casualty_df, lsoa_of_casualty == "-1"))
  }
  if(!is.null(vehicle_df)){
    lsoa_all = lsoa_summaries(vehicles = vehicle_df,lsoa_geo = lsoa_geo,city_shp = city_shp,base_year = base_year, end_year = end_year)
    legend_title = "drivers"
    title = paste0("Home LSOA area for all drivers involved in collisions between ", base_year, " and ", end_year)
    credit_title = "distance (km)   drivers"
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
    tm1 = NULL
  alp = 1}
  
  
  tm1 <- tm1+
    tm_shape(lsoa_plot) +
    tm_polygons(fill = "persons",fill_alpha = alp,
                fill.scale = tm_scale_intervals(values = palette),
                fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                lwd = 0.1)+
    tm_credits(
      paste0("Distance of home LSOA\nfrom border of area:\n",
             credit_title, "\n",
             lsoa_outside_city$distances[1],":       ", lsoa_outside_city$persons[1],"\n",
             lsoa_outside_city$distances[2],":       ", lsoa_outside_city$persons[2],"\n",
             lsoa_outside_city$distances[3],":       ", lsoa_outside_city$persons[3],"\n",
             lsoa_outside_city$distances[4],":       ", lsoa_outside_city$persons[4],"\n",
             lsoa_outside_city$distances[5],":       ", lsoa_outside_city$persons[5],"\n",
             "total (inc area):      ", total_persons,"\n",
             "no data:    ",  lsoa_missing),
      position = info_position)+
    tm_title(title,size = 2)+
    tm_layout(frame = FALSE)
  
  return(tm1)
}


# function to plot any super output area/local authority
lsoa_crashes_plot <- function(crashes_df, 
                              lsoa_geo,
                              city_shp,
                           palette = "tol.rainbow_wh_br",
                           base_year = 2020, 
                           end_year = 2024,
                           info_position = c(0,0.2)){
  

    lsoa_all = lsoa_summaries(collisions = crashes_df,lsoa_geo = lsoa_geo,city_shp = city_shp,base_year = base_year, end_year = end_year)
    legend_title = "crashes"
    title = paste0("collisions by LSOA area between ", base_year, " and ", end_year)
    
    tm1 <- tm_shape(lsoa_all) +
      tm_polygons(fill = "crashes",fill_alpha = 1,
                  fill.scale = tm_scale_intervals(values = palette),
                  fill.legend = tm_legend(legend_title, frame = FALSE,legend.border.col = NA),
                  lwd = 0.1)+
      tm_title(title,size = 2)+
      tm_layout(frame = FALSE)
    
    return(tm1)
    
}
  
