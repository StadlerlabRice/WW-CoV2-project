# OBSOLETE: Parameters for qPCR analysis
# Author: Prashant Kalvapalle;  June 13 2020


# WWTP filters ----

# WWTP names etc for filtering data for plots of 2-calcs.. and 3-weekly comparisons

# get all bayou, manhole and WWTP sample names (remain same every week)
biobot_lookup <- map_df(c('All Bayou', 'All manhole', 'All wastewater'), 
                        ~ read_sheet(sheeturls$biobot_id , sheet = .x, range = 'A:C', col_types = 'ccc') %>% 
                          rename('WWTP' = contains('SYMBOL', ignore.case = T), 
                                 'Facility' = matches('FACILITY NAME', ignore.case = T),
                                 'Type' = 'Facility Type') %>% 
                          mutate(WWTP = as.character(WWTP) %>% str_remove(' '), # convert to char and removed spaces
                                 'assay_variable' = WWTP))
# TODO : streamline getting all three kinds of samples into 1 sheet read call..


# List of all WWTPs
WWTP_symbols <- biobot_lookup %>%
  filter(Type == 'Wastewater') %>% 
  pull(WWTP) # this vector of all WWTP abbreviations (symbols) is good for exact matching 

# combine the list of WWTPs into 1 string, separated by the OR symbol "|"
WWTP_symbols_regex <- WWTP_symbols %>% 
  paste(collapse = "|") # this single char with all WWTP names is for regex string matching (approx) : Use with caution

# list all manhole names (for regex matching)
manhole_symbols_regex <- biobot_lookup %>%
  filter(!str_detect(Type, 'Wastewater|Bayou')) %>% 
  pull(WWTP) %>% 
  paste(collapse = "|")


# Make a string to filter out all regular WWTP and manhole samples to plot separately
wwtp_manhole_names <- str_c(WWTP_symbols_regex, manhole_symbols_regex, sep = '|')


# User inputs ----
# deleted obsolete ones from here.. 

# Other parameters ----

# Additional parameters (changed rarely)
# plot_assay_variable <- 'Sample' # printed on the x axis of the graph

# inclusion exclusion variables
# plot_select_facet <- '' # Options ('' or 'something') ; filters a particular template name to plot 

# plot_exclude_facet <- '^none' # Regex pattern: 'Controls2', '^MHT*', '^none; exclude categories for plotting; ex: Controls etc.: filters based on `Sample Name`: works only in assay mode
# plot_exclude_assay_variable <- '^none' # Regex pattern: '^N', '^none' or ''; exclude assay_variables for plotting; ex: no template control etc.: filters based on assay_variable: works only in assay mode


