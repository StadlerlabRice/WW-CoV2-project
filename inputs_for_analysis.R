# Read in the qPCR file and do manual analysis and plotting
# Author: Prashant Kalvapalle;  June 13 2020


# User inputs ----
# choose file name, title for plots and experiment mode (file name starts in the same directory as Rproject) 
# Sample naming guide for plate template: in google sheet 'enter cell address of experiment name' (check 'plate_template_raw' for link)
# BCoV-608_A.2; ignore-facet_(x axis variable).biological replicate (BCoV is ignored, only for pipetting reference, actual target is taken from the qPCR results)
# If code fails, first thing: check the number of lines to skip before the data begins and tally with the code (including the headings)

flnm.here <- 'WW41_727_BCoV_Std24'  # set the filename (if sourcing analysis.R to run)

std_par <- tibble(                       # Input the slope and intercept from standard curve of various primer pairs/targets here - Target should match Target field (provided in excel sheet - Sample input reference.csv) 
  Target = c('BRSV_N', 'BCoV_M', 'N1_CoV2', 'N2_CoV2', 'N1_multiplex',  'N2_multiplex', 'pMMoV_Vgp1'),
  Slope =  c(-3.61,    -3.33,    -2.98,     -3.12,     -3.08,           -3.06,           -3.13),
  y_intercept = c(38,  34.55,       39,        40,        38,              37,              35.87) # values for various targets
)
template_volume_qpcr <- 4 # ul template volume in qPCR reaction


# Other parameters ----

# Additional parameters (changed rarely)
plot_assay_variable <- 'Sample' # printed on the x axis of the graph

# inclusion exclusion variables
plot_select_facet <- '' # Options ('' or 'something') ; filters a particular template name to plot 
plot_exclude_facet <- '^none' # Regex pattern: 'Controls2', '^MHT*', '^none; exclude categories for plotting; ex: Controls etc.: filters based on `Sample Name`: works only in assay mode
plot_exclude_assay_variable <- '^none' # Regex pattern: '^N', '^none' or ''; exclude assay_variables for plotting; ex: no template control etc.: filters based on assay_variable: works only in assay mode