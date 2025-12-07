
tag_table <- function(crashes, city, agg_level, tab_dir = "tables/"){
  tab_dir = "tables/"
  dir.create(tab_dir)

  cwc <- tag_costs(crashes,agg_level)
  
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

gtsave(t1, paste0(tab_dir, "/", city,"_costs.png"))

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
    
    gtsave(t1, paste0(tab_dir, "/", city,"_costs.png"))
    
    return(t1)
    
    
  }
 
}

costs_col_ranking_table <- function(crashes, severities = c("Fatal", "Serious", "Slight"), sort_by = c("casualties", "cost", "collisions"), rows = 10){
  
  costs_cas_col_per_LA(crashes,severities) |> 
    st_set_geometry(NULL) |> 
    arrange(desc(sort_by))
  
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
  
  gtsave(t1, paste0(tab_dir, "/", city,"_costs.png"))
  
  
}

