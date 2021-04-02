# FUnctions to load qPCR data and manipulate it. The functions can be called from another R file

# read in excel file (.xls) of qPCR exported from Quantstudio 3 
  # Make sure to include raw data as well

# Library calling  ----
# calling libraries ; make sure they are installed (install.packages)
library(readxl); library(magrittr); library(tidyverse); library(ggrepel); library(googlesheets4); library(rlang); library(lubridate); library(plotly) 

# google sheets ----
sheeturls <- list(templates = 'https://docs.google.com/spreadsheets/d/19oRiRcRVS23W3HqRKjhMutJKC2lFOpNK8aNUkC-No-s/edit#gid=478762118',
                  biobot_id = 'https://docs.google.com/spreadsheets/d/1ghb_GjTS4yMFbzb65NskAlm-2Gb5M4SNYi4FHE4YVyI/edit#gid=233791008',
                  sample_registry = 'https://docs.google.com/spreadsheets/d/1mJcCt1wMiOuBic6sRlBZJf8KSNu2y-B5PjzCUu7jPM8/edit#gid=521099478',
                  raw_ddpcr = 'https://docs.google.com/spreadsheets/d/1jdO_P9SZGezSTLiIARtSmA7qaUuX3wA-jCe7YiQ1sCI/edit#gid=0',
                  user_inputs = 'https://docs.google.com/spreadsheets/d/1SAINnazqMrjTBSuhYiIBbx8B7_reHzaEuwGTdkNA6wk/edit#gid=492585552',
                  
                  # Outputs
                  data_dump = 'https://docs.google.com/spreadsheets/d/1ouk-kCJHERRhOMNP07lXfiC3aGB4wtWXpnYf5-b2CI4/edit#gid=0',
                  complete_data = 'https://docs.google.com/spreadsheets/d/1ltvW7xZf2aUPoBchD4NFGuV0gGWPkdsOW3a0Hxalm-Y/edit#gid=1363292517',
                  wwtp_only_data = 'https://docs.google.com/spreadsheets/d/1dBESjgWSFsOBodFFpYNhWIOAQ2a3wvoLkrn12V_rFck/edit#gid=0' 
)


# dummy data, fns  ---- 
# test data for testing simple functions and other functions for testing 

# dummy test tibble
# test tibble with numeric, char and a factor
a <- tibble(a1 = 1:6, a2 = 6:1, a3 = rep(c('a', 'b'),3), a4 = as_factor(5:10))


# expression to test on plotting
y_namr_test <- list( 'a2' = expression(paste('this is a ', mu, 'L')),
                     'a4' = expression(paste('super large ', sigma, 'L')))


# test ggplot
a_plt <- ggplot(a, aes(a1, a2, colour = a3)) + 
  geom_point() + 
  geom_line() + 
  ylab(y_namr_test[['a4']])


# this is a dummy function to provide an environment to test quosures, unquoting and things like that
dumfun <- function(a, var_a = a2, var_b = a3, c_var = 'a1', d_var = NULL)
{   
  d_var %<>% {if(is.null(.)) expr(c(!!var_a, !!var_b)) else .}
  
  a %>% select(!!d_var)
}

# calling more funs ----

list_of_general_functions <- c("g.1-reading_files_funs.R",
                               "g.2-tibble_columns_funs.R",
                               "g.3-obsolete_arcane_funs.R",
                               "g.4-qPCR_specific_funs.R",
                               "g.5-mathematical_fitting_funs.R",
                               "g.6-formatting_plot_funs.R",
                               "g.7-COVID specific_writing_funs.R",
                               "g.8-plot_mean_sd_jitter.R",
                               "g.9-plot_scatter.R")

# handy command to print a tibble in this format
# cat(paste(shQuote('Tibble/vector here', type="cmd"), collapse=",\n ")) : Source: https://stackoverflow.com/a/6347520/9049673

# Source all the functions listed above
map(str_c('./scripts_general functions/', list_of_general_functions),
    source)







