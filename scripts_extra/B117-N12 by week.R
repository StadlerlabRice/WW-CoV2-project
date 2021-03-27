# Ad-hoc script for comparing B117 with N1N2 across multiple weeks
# And weekly change analysis
# PK ; 5/March/2021

# Prelims ----

# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file

# sheets to read from "qPCR Complete data" google sheet
read_sheets_N <- c('0201 Rice', '020821 Rice (redo)', '022321 Rice', '0301 Rice') # sheet name(s) for N1N2 data: separate by comma (,)
read_sheets_S <- c('0201 Redo B117', '0209 WWTP Redo B117', '0223 WWTP B117', '0301 WWTP B117') # sheet name(s) for B117 data: separate by comma (,)

title_name <- 'B117-N12 0201-0301'

# Input ----

raw_N <- map_dfr(read_sheets_N, 
                 ~ read_sheet(ss = sheeturls$complete_data, sheet = .x)) # read all the sheets, bind them by rows to data frame (dfr)

raw_S <- map_dfr(read_sheets_S, 
                 ~ read_sheet(ss = sheeturls$complete_data, sheet = .x))


# Processing ----

# Notes for camille
# For comparing N12 and B117 data across weeks. You can use this command - https://tidyr.tidyverse.org/reference/pivot_longer.html
# to merge the xx_All and xx_Variant under 1 colum called xx and create another column for variant_status that would be All or Variant. this xx can then be renamed to be the same as the weekly N1N2 data and then they can be merged with bind_rows()

proc_N <- raw_N %>% 
  mutate(across(where(is.list), as.character), # change erroneous list columns to character : Happens when the data was copy pasted from excel
         across(Date, as.character)) 
# %>%  # change date to char
#   rename('Copies/L WW-N1N2' = 'Copies/l WW',
#          'Target Name-N1N2' = 'Target Name') # rename the columns to reflect N1N2 in it
  
proc_S <- raw_S %>% 
  pivot_longer(cols = matches('Copies|Droplets'), names_to = c(".value", 'variant_status'), names_sep = "_") 
# %>% 
#   rename('Copies/L WW-S1S2' = 'Copies/l WW',
#          'Target Name-S1S2' = 'Target Name') # rename the columns to reflect S1S2 in it

proc_NS <- map2(list(proc_N, proc_S),
                c('N1N2', 'S1S2'),
                ~ rename(.x, str_c('Copies/L WW-', .y) := 'Copies/l WW', # trying to use a variable column name 
                         # Error: The LHS of `:=` must be a string or a symbol
                       str_c('Target Name-', .y) := 'Target Name')
                ) # rename the columns to reflect S1S2 in it)

proc_long_varstatus <- full_join(proc_N, proc_S, 
                                 by = c('Tube_ID', 'Facility', 'WWTP', 'Date', 'Lab')) %>% 
  select(-matches('Vol|vol|Sample_Type|recovery|comments')) %>% 
  separate(Tube_ID, into = c('Week_ID', NA), remove = FALSE) # separate the week number into a different column; for colouring plots

# Plotting ----

# use existing functions plot_scatter() for comparisons
# use inspiration from weekly_comparisons.Rmd for weekly plots : "### Weekly timeseries - N1/N2/BcoV"

# Output ----

# saving plots and temporary saving of csv for compiled data

