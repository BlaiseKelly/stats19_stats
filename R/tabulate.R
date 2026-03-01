
#' Create a gt table of TAG collision costs
#'
#' Computes TAG collision and casualty costs from crash data, formats values
#' with thousand separators, and produces a styled \code{gt} table. The table
#' is saved to disk as a PNG (or other format) and returned as a \code{gt}
#' object.
#'
#' @param crashes An \code{sf} or regular data frame of crash records with
#'   \code{collision_year} and \code{collision_severity} columns.
#' @param city Character. City name used in the table title and filename.
#' @param agg_level Character. One of \code{"severity"} (costs by severity)
#'   or \code{"severity_road"} (costs by severity and road type).
#' @param tab_dir Character. Directory where the output file will be saved.
#'   Default \code{"tables/"}.
#' @param file_type Character. File extension for the saved table (e.g.
#'   \code{".png"}, \code{".html"}). Default \code{".png"}.
#' @return A \code{gt} table object.
#' @examples
#' \dontrun{
#' t1 <- tabulate_summarise_tag_costs(crashes, city = "Bristol",
#'                          agg_level = "severity")
#' t1 <- tabulate_summarise_tag_costs(crashes, city = "Bristol",
#'                          agg_level = "severity_road")
#' }
#' @export
tabulate_summarise_tag_costs <- function(crashes, city, agg_level, tab_dir = "tables/", file_type = ".png"){

  
  crash_geo = inherits(crashes,"sf")
  if(crash_geo){
    st_geometry(crashes) = NULL
  }
  
  cwc <- summarise_tag_costs(crashes,agg_level)
  
  if(agg_level == "severity"){
  
# format values with commas
cwc_tot <- cwc |>
  ungroup() |>
  rowwise() |>
  mutate(casualty_cost = prettyNum(casualty_cost, big.mark = ",", scientific = FALSE),
         collision_cost = prettyNum(collision_cost, big.mark = ",", scientific = FALSE),
         total = prettyNum(total_cost, big.mark = ",", scientific = FALSE),
         total_casualties = round(total_casualties)) |>
  select(-total_cost)

cc_tot_all <- sum(as.numeric(gsub(",","", cwc_tot$total)))

start_year <- min(cwc$collision_year)
end_year <- max(cwc$collision_year)

# country table
t1 <- gt(cwc_tot,auto_align = TRUE) |>
  cols_width(collision_year ~px(60)) |>
  cols_label(collision_year = md("**Year**"),
             collision_severity = md("**Severity**"),
             total_casualties = md("**Casualties**"),
             casualty_cost = md("**Casualty cost (£)**"),
             collision_cost = md("**Collision cost (£)**"),
             total = md("**Total (£)**")) |>
  tab_footnote(md("**Source: DfT STATS19 and TAG**")) |>
  tab_header(
    title = md(paste0("**Number of reported road casualties and value of prevention by year, ",city,": ",start_year, " to ", end_year,"**"))) |>
  tab_options(heading.align = "left",
              column_labels.border.top.style = "none",
              table.border.top.style = "none",
              column_labels.border.bottom.style = "none",
              column_labels.border.bottom.width = 1,
              column_labels.border.bottom.color = "black",
              table_body.border.top.style = "none",
              table_body.border.bottom.color = "white",
              heading.border.bottom.style = "none",
              table.border.bottom.style = "none") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(collision_year)),
      cells_body(columns = c(collision_year))
    )) |>
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(columns = everything())
  )

gtsave(t1, paste0(tab_dir, "/", city,"_costs", file_type))

return(t1)

  }
  
  if(agg_level == "severity_road"){
    
    # format values with commas
    cwc_tot <- cwc |>
      ungroup() |>
      rowwise() |>
      mutate(total = prettyNum(round(sum(built_up,not_built_up,Motorway,na.rm = TRUE)), big.mark = ",", scientific = FALSE)) |> 
      mutate(built_up = prettyNum(round(built_up), big.mark = ",", scientific = FALSE),
             Motorway = prettyNum(round(Motorway), big.mark = ",", scientific = FALSE),
             not_built_up = prettyNum(round(not_built_up), big.mark = ",", scientific = FALSE)) 
    
   # cc_tot_all <- sum(as.numeric(gsub(",","", cwc_tot$total)))
    
    start_year <- min(cwc$collision_year)
    end_year <- max(cwc$collision_year)
    
    # country table
    t1 <- gt(cwc_tot,auto_align = TRUE) |>
      cols_width(collision_year ~px(60)) |>
      cols_label(collision_year = md("**Year**"),
                 collision_severity = md("**Severity**"),
                 built_up = md("**Built up (£)**"),
                 not_built_up = md("**Not built up (£)**"),
                 Motorway = md("**Motorway (£)**"),
                 total = md("**Total (£)**")) |>
      tab_footnote(md("**Source: DfT STATS19 and TAG**")) |>
      tab_header(
        title = md(paste0("**Number of reported road casualties and value of prevention by year, ",city,": ",start_year, " to ", end_year,"**"))) |>
      tab_options(heading.align = "left",
                  column_labels.border.top.style = "none",
                  table.border.top.style = "none",
                  column_labels.border.bottom.style = "none",
                  column_labels.border.bottom.width = 1,
                  column_labels.border.bottom.color = "black",
                  table_body.border.top.style = "none",
                  table_body.border.bottom.color = "white",
                  heading.border.bottom.style = "none",
                  table.border.bottom.style = "none") |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = list(
          cells_column_labels(columns = c(collision_year)),
          cells_body(columns = c(collision_year))
        )) |>
      tab_style(
        style = cell_fill(color = "white"),
        locations = cells_body(columns = everything())
      )
    
    gtsave(t1, paste0(tab_dir, "/", city,"_costs", file_type))
    
    return(t1)
    
    
  }
 
}

#' Create a gt table of Local Authority cost rankings
#'
#' Computes collision costs per Local Authority, sorts by the chosen metric,
#' and produces a styled \code{gt} table of the top \code{n} LAs. The table
#' is saved to disk.
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param severities Character vector of severity levels to include. Default
#'   \code{c("Fatal", "Serious", "Slight")}.
#' @param sort_by Character. Column to sort LAs by. One of
#'   \code{"casualties"}, \code{"cost"}, or \code{"collisions"}.
#' @param table_year Integer. Year to filter to. Default \code{2024}.
#' @param rows Integer. Number of rows (top LAs) to show. Default \code{10}.
#' @return A \code{gt} table object.
#' @examples
#' \dontrun{
#' t1 <- tabulate_la_cost_ranking(crashes, sort_by = "cost",
#'                                table_year = 2024, rows = 10)
#' }
#' @export
tabulate_la_cost_ranking <- function(crashes, severities = c("Fatal", "Serious", "Slight"), sort_by = c("casualties", "cost", "collisions"), table_year = 2024, rows = 10){
  
  LA_sum <- costs_cas_col_per_LA(crashes,severities) |> 
    st_set_geometry(NULL) |> 
    filter(collision_year = table_year) |> 
    arrange(desc(sort_by)) |> 
    slice(1:rows)
  
  # country table
  t1 <- gt(LA_sum,auto_align = TRUE) |>
    cols_width(LAD22NM ~px(60)) |>
    cols_label(LAD22NM = md("**Local Authority**"),
               total_collisions = md("**Total collisions**"),
               total_casualties = md("**Total casualties**"),
               total_cost = md("**Total cost (£mn)**"),
               annual_coll_rank = md("**Collision ranking**"),
               annual_cas_rank = md("**Casualty ranking**"),
               annual_cost_rank = md("**Cost ranking**")) |>
    tab_footnote(md("**Source: DfT STATS19 and TAG**")) |>
    tab_header(
      title = md(paste0("**Number of collisions, casualties and the cost for top 10 Local Authorities in ", table_year,"**"))) |>
    tab_options(heading.align = "left",
                column_labels.border.top.style = "none",
                table.border.top.style = "none",
                column_labels.border.bottom.style = "none",
                column_labels.border.bottom.width = 1,
                column_labels.border.bottom.color = "black",
                table_body.border.top.style = "none",
                table_body.border.bottom.color = "white",
                heading.border.bottom.style = "none",
                table.border.bottom.style = "none") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(columns = c(collision_year)),
        cells_body(columns = c(collision_year))
      )) |>
    tab_style(
      style = cell_fill(color = "white"),
      locations = cells_body(columns = everything())
    )
  
  gtsave(t1, paste0(tab_dir, "/", city,"_costs.png"))
  
  
}

