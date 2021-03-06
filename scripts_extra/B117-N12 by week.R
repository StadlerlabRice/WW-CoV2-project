# Ad-hoc script for comparing B117 with N1N2 across multiple weeks
# And weekly change analysis
# PK ; 5/March/2021

# Prelims ----

# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file

# sheets to read from "qPCR Complete data" google sheet
read_these_sheets <- c('') # sheet name(s) : separate by comma (,)

title_name <- 'B117-N12 0201-0301'

# Input ----


# Processing ----

# Notes for camille
# For comparing N12 and B117 data across weeks. You can use this command - https://tidyr.tidyverse.org/reference/pivot_longer.html
# to merge the xx_All and xx_Variant under 1 colum called xx and create another column for variant_status that would be All or Variant. this xx can then be renamed to be the same as the weekly N1N2 data and then they can be merged with bind_rows()

# Plotting ----

# use existing functions plot_scatter() for comparisons
# use inspiration from weekly_comparisons.Rmd for weekly plots : "### Weekly timeseries - N1/N2/BcoV"

# Output ----

# saving plots and temporary saving of csv for compiled data

