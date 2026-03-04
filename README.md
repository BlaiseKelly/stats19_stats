This repository is a collection of functions to analyse stats19 data and produce reports for local areas. Currently it is possible to generate reports for Local Authorities and Combined Authorities.

To quickly generate a pre defined report for a Local Authority region, clone the repository and run the following function:

```
# to generate the plots, and dataframes for the report
run_report_inputs(authority = "Bristol")

# generates a html
run_report_quarto(authority = "Bristol")

```

and it will output plots in the outputs/authority/ folder some of which are used to create the report: LA_report.html

The output report and interactive maps and tables are shown here: https://blaisekelly.github.io/stats19_stats/

In the coming weeks some customisation options will be added for the report and the functions better explained.
