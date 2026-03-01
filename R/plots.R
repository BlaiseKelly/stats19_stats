


#' Plot casualty indexes over time
#'
#' `plot_casualty_index()` creates a line chart of casualty indexes by severity,
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
#' plot_casualty_index(my_indexes, base_year = 2010, end_year = 2024, city = "London")
#' }
#' @export
plot_casualty_index <- function(indexes,
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
  
  out_file <- file.path(plot_dir, paste0("plots/index.png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(chart_data)
}


#' Plot casualty demographics by age and sex
#'
#' `plot_casualty_demographics()` creates a bar chart showing casualty percentages by age band
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
#' plot_casualty_demographics(my_casualties, city = "London", severity = "ksi")
#' }
#' @export
plot_casualty_demographics <- function(casualties,
                       pal = c("#ff7733", "#1de9b6", "#006853"),
                       city,
                       severity = c("all", "ksi"),
                       stat = c("pc"),
                       plot_dir = "plots/") {
  severity <- match.arg(severity)
  stat <- match.arg(stat)
  
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  demo_sum <- if (severity == "ksi") {
    #group_demo(casualties, demographic = "both", severities = c("Fatal", "Serious"))
    summarise_casualties_by_demog(casualties,severities = c("Fatal", "Serious"))
  } else {
    #group_demo(casualties, demographic = "both", severities = c("Fatal", "Serious", "Slight"))
    summarise_casualties_by_demog(casualties,severities = c("Fatal", "Serious", "Slight"))
  }
  
  cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
  dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
  
  if (severity == "ksi") {
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
  
  out_file <- file.path(plot_dir, paste0("plots/demog/demographics.png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(demo_sum)
}

#' Plot yearly casualty totals by severity
#'
#' `plot_casualties_by_year()` produces a bar chart of casualty counts by severity and
#' year, saving the result as a PNG.
#'
#' @param casualties A data frame of casualty records suitable for
#'   `summarise_casualty_rates()`.
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
#' plot_casualties_by_year(my_casualties, city = "London")
#' }
#' @export
plot_casualties_by_year <- function(casualties,
                          pal = c("#ff7733", "#1de9b6", "#006853"),
                          city,
                          severity = c("Fatal", "Serious", "Slight"),
                          plot_dir = "plots/") {
  stopifnot(is.data.frame(casualties))
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  cas_rates <- summarise_casualty_rates(casualties)
  
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
  
  out_file <- file.path(plot_dir, paste0("plots/year_totals.png"))
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
#'   `summarise_casualties_per_collision()`.
#' @param city Character. City name used in the plot title and filename.
#'   Default `la_name`.
#' @param severities Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @param parameter Character. One of `"road_surface_conditions"`,
#'   `"junction_detail"`, `"speed_limit"`, `"light_conditions"`,
#'   `"weather_conditions"`. Default `"road_surface_conditions"`.
#' @param plot_width Numeric. Width of the saved PNG in inches. Default `10`.
#' @param plot_dir Character. Directory where the PNG will be saved. Created if
#'   it does not exist. Default `"plots/"`.
#' @return Invisibly returns the aggregated data frame used for plotting.
#' @examples
#' \dontrun{
#' plot_crash_conditions(crashes, casualties, city = "Bristol",
#'                       parameter = "speed_limit")
#' }
#' @export
plot_crash_conditions <- function(crashes,
                                  casualties,
                                  city = la_name,
                                  severities = c("Fatal", "Serious", "Slight"),
                                  parameter = c("road_surface_conditions", "junction_detail",
                                                "speed_limit", "light_conditions", "weather_conditions"),
                                  plot_width = 10,
                                  plot_dpi = 400,
                                  plot_dir = "plots/") {
  parameter <- match.arg(parameter)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  cas_summary <- summarise_casualties_per_collision(casualties)
  
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
  
  out_file <- file.path(plot_dir, paste0(parameter,"_", paste(gsub(",","_", severities),collapse = "_"), ".png"))
  ggplot2::ggsave(out_file, plot = p, width = plot_width, height = plot_width*0.9, dpi = plot_dpi)
  
  invisible(crash_parameter)
}

#' Plot KSIs by hour of day and day of week
#'
#' @param crash_time Data frame with columns `collision_hr`, `KSI`, and `dow`.
#' @param report_casualty Character string used in the title (e.g. "casualties").
#' @param yr2calc Integer. End year of the period.
#' @param city Character. City name for title. Default `la_name`.
#' @param plot_dir Directory to save PNG. Default `"plots/"`.
#' @return Invisibly returns the plot object.
#' @examples
#' \dontrun{
#' plot_ksi_by_time(crash_time_df, report_casualty = "pedestrian",
#'                  yr2calc = 2024, city = "Bristol")
#' }
#' @export
plot_ksi_by_time <- function(crash_time,
                           report_casualty,
                           yr2calc,
                           city = la_name,
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
#' @examples
#' \dontrun{
#' plot_summarise_tag_costs(crashes, agg_level = "severity", city = "Bristol")
#' }
#' @export
plot_summarise_tag_costs <- function(crashes,
                     agg_level,
                     city,
                     plot_dir = "plots/") {
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  tag_df <- summarise_tag_costs(crashes, agg_level)
  
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
  
  out_file <- file.path(plot_dir, paste0(city, "_summarise_tag_costs.png"))
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
      
      out_file <- file.path(plot_dir, paste0(city, "_summarise_tag_costs_road.png"))
      ggplot2::ggsave(out_file, plot = p)
      
      invisible(chart_0)
    }
    
    
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
#' @examples
#' \dontrun{
#' plot_summarise_tag_costs(crashes, agg_level = "severity", city = "Bristol")
#' }
#' @export
plot_summarise_tag_costs_speed <- function(crashes,
                                     agg_level,
                                     plot_param = c("total_cost", "cost_per_col"),
                                     city,
                                     plot_dir = "plots/") {
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  sy <- min(crashes$collision_year, na.rm = TRUE)
  ey <- max(crashes$collision_year, na.rm = TRUE)
  
  if(agg_level == "severity"){
  
  if(plot_param == "total_cost"){
  
  tag_df = crashes |> 
    st_set_geometry(NULL) |> 
    match_tag(summarise = "severity") |> 
    select(collision_index,speed_limit,cost_per_collision, cost_per_casualty) |> 
    mutate(total_cost = cost_per_collision,
           cost_per_collision = cost_per_collision-cost_per_casualty) |> 
    group_by(speed_limit) |> 
    summarise(`collision cost` = sum(cost_per_collision/1e6),
              `casualty cost` = sum(cost_per_casualty/1e6)) |> 
    pivot_longer(-speed_limit, names_to = "cost category", values_to = "cost")
  
  title = paste0("Value of prevention of collisions by speed limit and collision and casualty in ", city,
         " between ", sy, " and ", ey)
  
  unitz = "million"
  
  }
  
  if(plot_param == "cost_per_col"){
    
    tag_df = crashes |> 
      st_set_geometry(NULL) |> 
      match_tag(summarise = agg_level) |> 
      select(collision_index,speed_limit,cost_per_collision, cost_per_casualty) |> 
      mutate(total_cost = cost_per_collision,
             cost_per_collision = cost_per_collision-cost_per_casualty) |> 
      group_by(speed_limit) |> 
      summarise(`collision cost` = sum(cost_per_collision/1e3),
                `casualty cost` = sum(cost_per_casualty/1e3),
                ncols = n()) |>
      transmute(speed_limit,
                `mean collision cost` = `collision cost`/ncols,
                `mean casualty cost` = `casualty cost`/ncols) |> 
      pivot_longer(-speed_limit, names_to = "cost category", values_to = "cost")
    
    title = paste0("Mean cost per collision by speed limit and collision and casualty in ", city,
                   " between ", sy, " and ", ey)
    
    unitz = "thousand"
    
  }
  
    pal <- c4a("carto.pastel", n = length(unique(tag_df$`cost category`)))
    cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
    dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
    
    p <- ggplot2::ggplot(tag_df, ggplot2::aes(x = speed_limit, y = cost, fill = `cost category`)) +
      ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = NA),   # placeholder, no labels
        position = ggplot2::position_stack(vjust = 0.5),
        size = 3
      ) +
      ggplot2::ggtitle(
        title,
        subtitle = "Calculated using collision data from DfT STATS19 and cost data from TAG"
      ) +
      dft_theme +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "top",
                     plot.title    = element_text(size = 12),
                     plot.subtitle = element_text(size = 8),
                     legend.title = ggplot2::element_blank()) +
      ggplot2::ylab(paste0("\u00A3 ",unitz)) +
      ggplot2::xlab(NULL) +
      ggplot2::labs(caption = "Source: Stats19 and TAG")
    p
    out_file <- file.path(paste0(plot_dir,"/plots/costs/tag_", plot_param, "_costs_speed_limit.png"))
    ggplot2::ggsave(out_file, plot = p, width = 250,height = 220, units = "mm")
    
  }
  
  ## TO DO implement road type instead of casualty - at the moment severity_road is returning the same df
  
  if(agg_level == "severity_road"){
    
    if(plot_param == "total_cost"){
      
      tag_df = crashes |> 
        st_set_geometry(NULL) |> 
        match_tag(summarise = agg_level)
        select(collision_index,speed_limit,cost_per_collision, cost_per_casualty) |> 
        mutate(total_cost = cost_per_collision,
               cost_per_collision = cost_per_collision-cost_per_casualty) |> 
        group_by(speed_limit) |> 
        summarise(`collision cost` = sum(cost_per_collision/1e6),
                  `casualty cost` = sum(cost_per_casualty/1e6)) |> 
        pivot_longer(-speed_limit, names_to = "cost category", values_to = "cost")
      
      title = paste0("Value of prevention of collisions by speed limit and collision and casualty in ", city,
                     " between ", sy, " and ", ey)
      
      unitz = "million"
      
    }
    
    if(plot_param == "cost_per_col"){
      
      tag_df = crashes |> 
        st_set_geometry(NULL) |> 
        match_tag(summarise = "severity") |> 
        select(collision_index,speed_limit,cost_per_collision, cost_per_casualty) |> 
        mutate(total_cost = cost_per_collision,
               cost_per_collision = cost_per_collision-cost_per_casualty) |> 
        group_by(speed_limit) |> 
        summarise(`collision cost` = sum(cost_per_collision/1e3),
                  `casualty cost` = sum(cost_per_casualty/1e3),
                  ncols = n()) |>
        transmute(speed_limit,
                  `mean collision cost` = `collision cost`/ncols,
                  `mean casualty cost` = `casualty cost`/ncols) |> 
        pivot_longer(-speed_limit, names_to = "cost category", values_to = "cost")
      
      title = paste0("Mean cost per collision by speed limit and collision and casualty in ", city,
                     " between ", sy, " and ", ey)
      
      unitz = "thousand"
      
    }
    
    pal <- c4a("carto.pastel", n = length(unique(tag_df$`cost category`)))
    cust_theme <- ggplot2::theme(panel.grid.major = ggplot2::element_line(size = 2))
    dft_theme <- list(cust_theme, ggplot2::scale_fill_manual(values = pal))
    
    p <- ggplot2::ggplot(tag_df, ggplot2::aes(x = speed_limit, y = cost, fill = `cost category`)) +
      ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = NA),   # placeholder, no labels
        position = ggplot2::position_stack(vjust = 0.5),
        size = 3
      ) +
      ggplot2::ggtitle(
        title,
        subtitle = "Calculated using collision data from DfT STATS19 and cost data from TAG"
      ) +
      dft_theme +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "top",
                     plot.title    = element_text(size = 12),
                     plot.subtitle = element_text(size = 8),
                     legend.title = ggplot2::element_blank()) +
      ggplot2::ylab(paste0("\u00A3 ",unitz)) +
      ggplot2::xlab(NULL) +
      ggplot2::labs(caption = "Source: Stats19 and TAG")
    
    out_file <- file.path(plot_dir, paste0(city, "_total_tag_costs_speed_limit.png"))
    ggplot2::ggsave(out_file, plot = p, width = 300,height = 150, units = "mm")
    
  }
  
  
}


#' Plot casualty percentages by type
#'
#' Creates a bar chart showing casualty percentages by casualty type and
#' severity.
#'
#' @param crashes Crash data frame.
#' @param casualties Casualty data frame.
#' @param city Character. City name for title and filename. Default `la_name`.
#' @param severities Character vector of severities to include. Default
#'   `c("Fatal", "Serious", "Slight")`.
#' @param plot_width Numeric. Width of saved PNG. Default `20`.
#' @param plot_height Numeric. Height of saved PNG. Default `20`.
#' @param plot_dpi Numeric. Resolution of saved PNG. Default `50`.
#' @param cas_type Character. One of `"casualty_type"`, `"short_name"`,
#'   `"in_or_on"`. Default `"short_name"`.
#' @param plot_dir Directory to save PNG. Default `"plots/"`.
#' @return Invisibly returns the aggregated data frame used for plotting.
#' @examples
#' \dontrun{
#' plot_casualty_types(crashes, casualties, city = "Bristol",
#'                     cas_type = "short_name")
#' }
#' @export
plot_casualty_types <- function(crashes,
                               casualties,
                               city = la_name,
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
                        paste0(paste(severities, collapse = "_"), "_cas_type.png"))
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
#' plot_waffle(c(Fatal = 50, Serious = 100, Slight = 200),
#'             plot_rows = 5,
#'             title = "Casualties by severity")
#' }
#' @export
plot_waffle <- function() {
  
  
  invisible(p)
}

plot_ksi_pavement = function(crashes,
                             casualties,
                             vehicles,
                             base_year,
                             upper_year,
                             plot_rows = 3,
                             legend_position = "bottom",
                             title = paste0("Pedestrians KSI whilst on a pavement or verge, coloured by driven vehicle that collided with them between ",base_year, " and ", upper_year),
                             pal = "poly.sky24",
                             plot_width = 350,
                             plot_height = 200,
                             plot_dir = "plots/"){
  
  
  one_car_pave = summarise_casualties_pavements(crashes,casualties,vehicles, base_year, upper_year)
  
  KSI_pav <- one_car_pave |> 
    rowwise() |>
    mutate(KSI =round(sum(Fatal, Serious))) |> 
    filter(KSI > 0) |> 
    select(vehicle_type, KSI)
  
  # get enough colours for the variables
  colz <- c4a(pal, n = NROW(KSI_pav))
  
  # create waffle plot
  p <- waffle::waffle(KSI_pav,
                      rows = plot_rows,
                      colors = colz,
                      legend_pos = legend_position,
                      title = title)
  
  # save to file
  ggplot2::ggsave(paste0(plot_dir,"KSI_pavements.png"), plot = p, width = plot_width, height = plot_height, units = "mm", dpi = 400)
  
}



# all break options c("cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust","bclust", "fisher", "jenks", "dpih", "headtails", "log10_pretty")



#' Plot Local Authority casualty ranking over time
#'
#' Creates a line chart showing a Local Authority's national casualty ranking
#' across years for specified severity levels and casualty types.
#'
#' @param crashes An \code{sf} data frame of GB crash records.
#' @param casualties Casualty data frame.
#' @param LA Character. Name of the Local Authority to plot. Default
#'   \code{"Bristol"}.
#' @param severities Character vector of severity levels or \code{"KSI"}.
#'   Default \code{"KSI"}.
#' @param casualty_types Character. Casualty type to filter by. Default
#'   \code{"Cyclist"}.
#' @param base_year Integer. First year shown on x-axis.
#' @param end_year Integer. Last year shown on x-axis.
#' @param plot_dir Character. Directory where the PNG will be saved. Default
#'   \code{"plots/"}.
#' @return Invisibly returns the reshaped data frame used for plotting.
#' @examples
#' \dontrun{
#' plot_la_ranking(crashes_gb, casualties_gb, LA = "Bristol",
#'                 severities = "KSI", casualty_types = "Cyclist",
#'                 base_year = 2010, end_year = 2024)
#' }
#' @export
plot_la_ranking <- function(crashes,
                         casualties,
                         LA = la_name,
                         la_geo,
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

  pal_sev <- data.frame(pal = c("#ff7733", "#1de9b6", "#006853", "#2b1a8a","#de50a6"),
                        severity = c("Fatal", "Serious", "Slight","KSI", "Total"))
  
  ranking_df <- summarise_casualties_per_la(casualties = casualties, crashes = crashes,la_geo = la_geo, casualty_types = casualty_types) |> 
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
  
  out_file <- file.path(plot_dir, paste0("rank_", casualty_types,".png"))
  ggplot2::ggsave(out_file, plot = p)
  
  invisible(chart_data)
}


#' Plot Casualty Type Demographics by IMD Decile
#'
#' Summarises casualty IMD (Index of Multiple Deprivation) data by casualty type
#' and a demographic parameter (age or sex), then produces a time-proportion plot
#' using \code{\link[openair]{timeProp}}. IMD deciles are mapped to monthly dates
#' to leverage openair's time-series plotting, with decile 1 (most deprived) to
#' 10 (least deprived) displayed along the x-axis. Casualty types contributing
#' less than 5\% of the total are filtered out to avoid sparse plots.
#'
#' @param casualties A data frame of casualty records for the local authority,
#'   expected to contain columns including \code{casualty_type},
#'   \code{casualty_imd_decile}, \code{fatal_count},
#'   \code{casualty_adjusted_severity_serious},
#'   \code{casualty_adjusted_severity_slight}, and the relevant demographic
#'   column (\code{dft_age_band} or \code{sex_of_casualty}).
#' @param demog_param Character string specifying the demographic breakdown.
#'   One of \code{"age"} (maps to \code{dft_age_band}) or \code{"sex"}
#'   (maps to \code{sex_of_casualty}). Default is \code{"age"}.
#' @param stat2plot Character string specifying the severity statistic to plot.
#'   One of \code{"fatal"}, \code{"serious"}, \code{"ksi"}, \code{"slight"},
#'   or \code{"total"}. Default is \code{"fatal"}.
#' @param plot_dir Character string specifying the output directory for the
#'   saved PNG file. Default is \code{"plots/lsoa/"}.
#'
#' @return Called for its side effect of saving a PNG plot to \code{plot_dir}.
#'   Returns \code{NULL} invisibly.
#'
#' @details
#' The function uses a date-encoding trick to repurpose
#' \code{\link[openair]{timeProp}} for non-temporal categorical data: each IMD
#' decile is assigned a synthetic monthly date so that openair renders the
#' deciles as an ordered x-axis. The plot width is adjusted based on the number
#' of casualty type panels (wider for 4 or fewer types).
#'
#' @examples
#' \dontrun{
#' # Plot KSI casualties by age band and IMD decile
#' plot_casualty_type_demographics(
#'   casualties = casualties_la_simp,
#'   demog_param = "age",
#'   stat2plot = "ksi",
#'   plot_dir = "plots/lsoa/"
#' )
#'
#' # Plot slight casualties by sex and IMD decile
#' plot_casualty_type_demographics(
#'   casualties = casualties_la_simp,
#'   demog_param = "sex",
#'   stat2plot = "slight"
#' )
#' }
#'
#' @seealso \code{\link[openair]{timeProp}} for the underlying plotting function.
#' @importFrom dplyr left_join group_by summarise mutate filter
#' @importFrom rlang sym
#' @importFrom lubridate dmy
#' @importFrom openair timeProp
#' @importFrom grDevices png dev.off
#' @export
plot_casualty_type_demographics <- function(casualties,
                                            demog_param = c("age", "sex"),
                                            la_name = la_name,
                                            stat2plot = c("fatal", "serious", "ksi", "slight", "total"),
                                            plot_dir = "plots/lsoa/") {
  
  if(demog_param == "age"){
    demog_opt = "dft_age_band"
  }
  if(demog_param == "sex"){
    demog_opt = "sex_of_casualty"
  }
  
  lsoa_casualties = casualties_la_simp |> 
    left_join(decile_match, by = c("casualty_imd_decile" = "imd_decile")) |> 
    mutate(is_serious = ifelse(casualty_adjusted_severity_serious >= 0.5,casualty_adjusted_severity_serious,0)) |> 
    group_by(casualty_type, !!sym(demog_opt),IMDDecil) |> 
    summarise(fatal = sum(fatal_count),
              serious = sum(casualty_adjusted_severity_serious),
              ksi = sum(is_serious,fatal),
              #ksi = ifelse(serious >= 0.5, sum(fatal,serious),sum(fatal)),
              slight = sum(casualty_adjusted_severity_slight)) |> 
    mutate(total = fatal+serious+slight)
  
  # if a casualty type has less than 5% of total then there is not enough data for a visible plot so remove the casualty type
  lsoa_check = lsoa_casualties |> 
    group_by(casualty_type) |> 
    summarise(total = sum(!!sym(stat2plot))) |> 
    mutate(pc = total/sum(total)) |> 
    filter(pc > 0.05)
  
  # remove any less than 5% to keep the plot cleaner
  lsoa_cas = filter(lsoa_casualties, casualty_type %in% lsoa_check$casualty_type)
  
  # time proportion plot
  # p = timeProp(lsoa_cas,pollutant = stat2plot, avg.time = "month",type = "casualty_type", proportion = demog_opt,cols = pal,
  #              date.format = "%m",xlab = "IMD Decile (1 = most deprived and 10 = least deprived)",key.title = demog_param,statistic = "sum",
  #              main = paste0("IMD Decile of ", stat2plot," casualties home address in ", la_name,", dissagregated by casualty type and ",demog_param))
  
  ggplot2::ggplot(lsoa_cas, aes(x = factor(IMDDecil), y = .data[[stat2plot]], fill = .data[[demog_opt]])) +
    geom_col(position = position_stack(reverse = TRUE), 
             colour = "white", linewidth = 0.2) +
    facet_wrap(~casualty_type, scales = "free_y") +
    scale_fill_viridis_d(option = "inferno", direction = 1) +
    labs(
      title = paste0("IMD Decile of ", stat2plot," casualties home address in ", la_name,", dissagregated by casualty type and ",demog_param),
      x = "IMD Decile (1 = most deprived and 10 = least deprived)",
      y = stat2plot,
      fill = demog_param
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      strip.background = element_rect(fill = "#f0f0f0", colour = NA),
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(face = "bold", size = 13),
      axis.ticks = element_blank()
    )
  
  ggplot2::ggsave(paste0(plot_dir,stat2plot,"_imd_",demog_param,".png"))
  
}
