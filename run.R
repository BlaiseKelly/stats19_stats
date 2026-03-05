
source("R/utils.R")

# 
run_report_inputs("Leeds")

# prepare site files
run_site("Leeds")

# render files
quarto::quarto_render()
