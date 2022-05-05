# Plotting school samples by grade and enrollment from collated data sheet
# Prashant
# 11 April 2022

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file


title_name <- 'Lift stations and their WWTP_7-April-22'


# Load files ----


# Metadata 

LS_metadata <- read_csv('excel files/boxplots_for_LS_7-4-22/LS_to WWTP_connection.csv') %>% 
  
  # select only the 4 relevant columns out of 7 -- the rest are redundant with current cols in the data
  select(Short_name, Primary_WWTP, Primary_Percent,  HHD_WWTP_Name) 

WWTP_metadata <- filter(biobot_lookup, Type == 'Wastewater') %>% 
  
  # cleanup : remove the dots in M.U.D.#203 ; and make all CAPS
  mutate(across(Facility,
                ~ str_replace_all(.x, c('\\.' = '', 
                                        ' # | #' = '#') ) %>% # M.U.D. # 203 -> MUD#203
                  toupper %>%   # uppercase
                  
                  str_replace('WEST LAKE HOUSTON', 'WEST LAKE') )) %>% # change to match data
  
  rename('abbr_WWTP' = WWTP) %>% 
  select(Facility, abbr_WWTP)


# Data 

WWTP_data <- 
  read_csv('excel files/boxplots_for_LS_7-4-22/Rice_WWTP_All_Results_20220404.csv') %>% 
  
  # add a new column to mark WW samples and order by increasing population
  mutate('Facility_type' = 'WWTP',
         WWTP = fct_reorder(WWTP, Population)) %>% # order by population
  
  # filter dates after March/21 only
  mutate(across(Date,
                ~ lubridate::mdy(.x))) %>%  # convert dates into good format
  
  filter(Date > lubridate::my('3/21')) %>%  # filter samples later than march 2021
  
  # Attach abbreviated names
  left_join(WWTP_metadata, by = 'Facility') %>% 
  mutate(abbr_WWTP = fct_reorder(abbr_WWTP, Population)) # set order


LS_data <- 
  read_csv('excel files/boxplots_for_LS_7-4-22/Rice_LS_All_Results_20220404.csv') %>% 
  mutate('Facility_type' = 'Lift station',
         WWTP = fct_reorder(WWTP, Population)) %>% # order by population size
  
  # change date format
  mutate(across(Date,
                ~ lubridate::mdy(.x))) %>% 
  
  # Make column for abbreviated facility names
  mutate(abbr_LS = str_match(Sample_ID, '.([:alnum:]*)-')[ , 2]) %>%  # capture the ".(stuff)-"
  
  # Make corrections to old abbreviations
  mutate(across(abbr_LS,
                ~ rename_old_LS_abbrs(.x)) ) %>% 
  
  # set order
  mutate(abbr_LS = fct_reorder(abbr_LS, Population)) # set order


# polishing ----

# merge only the relevant rows of WWTP and LS data 
# merge by rows for simple boxplot : WWTP vs LS

merged_ww.ls_selected <- 
  
  list(WWTP_data, LS_data) %>%
  
  # extract the result col and facility type only
  map(~ select(.x, Copies_Per_Liter_WW, Facility_type)) %>% 
  
  # bind the data row by row
  bind_rows()


# append the metadata to LS
LS_comprehensive <- LS_data %>% 
  
  # join with the lookup list
  left_join(LS_metadata, by = c('WWTP' = 'Short_name')) %>% 
  
  # change column names
  rename('abbr_name' = 'abbr_LS') 

primary_ww_present <- pull(LS_comprehensive, Primary_WWTP) %>% unique()


# append the massage the WW data
WW_for.merge <- rename(WWTP_data, 'abbr_name' = 'abbr_WWTP') %>% 
  
  # add the column for Primary_WWTP, same as WWTP
  mutate(Primary_WWTP = str_replace(WWTP, 'SIMS BAYOU.*', 'SIMS BAYOU')) %>% 
  # except Sims Bayou North and South are shown as SIMS BAYOU (since their LS mix into both)
  
  # Retain only the ones named as Primary_WWTP in any lift station samples
  filter(Primary_WWTP %in% primary_ww_present)


# join LS with Wastewater data ----
# merging by rows - to plot the respective primary_WWTP in promixity
LS_and_WW <- bind_rows(LS_comprehensive, WW_for.merge) %>% 
  
  # add an abbreviated column for Primary_WWTP
  left_join(WWTP_metadata, by = c('Primary_WWTP' = 'Facility')) %>% 
  
  # manage the abbreviation column for primary wwtps
  rename('abbr_primary_wwtp' = abbr_WWTP) %>% 
  mutate(across(abbr_primary_wwtp, 
                ~ if_else(str_detect(Primary_WWTP, 'SIMS BAYOU'), # remake abbreviation for the combined SIMS Bayou
                          'SB-merg',
                          .x) %>% 
                  replace_na('OCSL'))) %>% # Name abbreviation for OCSL = Outside City Sewage limits?
  
  # Set order by population
  mutate(across(c(abbr_primary_wwtp, Primary_WWTP, abbr_name),
         ~ fct_reorder(.x, Population)) ) # set order by population


# N1-N2 Averaged data for both replicates

LS_WW_avg_nest <- LS_and_WW %>% 
  
  # group by everything except N1-N2 target, replicate containing ID and value columns
  group_by(across(c(-Target_Name, -Sample_ID, -Copies_Per_Liter_WW) )) %>% 
  
  # make average
  summarise(across(Copies_Per_Liter_WW, ~ mean(.x, na.rm = T))) %>% 
  
  # Mark samples above detection limit
  mutate(positivity = as_factor(Copies_Per_Liter_WW > Detection_Limit)) %>% 
  
  # re-group by primary_WWTP
  group_by(Primary_WWTP) %>% 
  
  # nest into a list-column
  nest()

# Documentation for correcting old/redundant abbreviations
# Get the samples that are not in the Biobot Sample IDs google sheet (which I assume is current)

# pull(LS_and_WW, abbr_name) %>% unique() %>% as.character() %>% {.[!(. %in% biobot_lookup$WWTP)]}



# Plots ----

# function to label the number of data points on the plot
# Copied from stack overflow : https://stackoverflow.com/a/31140454/9049673
give.n <- function(x){ 
  return(c(y = mean(x), label = length(x)))
}



# WW and Liftstation ----
# make box and dot plot
plt.WW_vs_LS <- 
  {ggplot(merged_ww.ls_selected,
          aes(x = Facility_type, y = Copies_Per_Liter_WW,
              # label = Facility
              )) + 
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.1,
                 position = position_jitter(width = .3, height = 0)) +
      
      # Violin plot
      geom_violin(alpha = 0.5) + 
      
      # show median
      geom_boxplot(aes(y = median(Copies_Per_Liter_WW)), colour = 'red') +
      
      
      
      # text labels for number of points
      stat_summary(fun.data = give.n, geom = 'text',
                   position = position_dodge2(width = .75),
                   colour = 'white', size = 3) +

      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      ggtitle('WWTP vs Lift stations : Mar-2021 to Apr-2022',
              subtitle = 'Number of data points shown inside the violin. Red line shows median') +
      
      theme(legend.position = 'top') #+ # legend position
      
      # theme_gray() # theme of the plot, gray background
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()


# Lift station ----

# Each Lift station - ordered by population 
plt.LS <- 
  {ggplot(LS_data,
          aes(x = as.numeric(WWTP), y = Copies_Per_Liter_WW,
              # colour = Target_Name,
              label = Facility)) + 
      
      # boxplot
      # geom_boxplot(mapping = aes(colour = NA),
      #              outlier.shape = NA, alpha = .4) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.2) + 
      
      # showing median
      stat_summary(geom = 'point', fun = 'median', 
                 shape = '-', size = 7,
                 colour = 'red') +
      
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      ggtitle('Lift stations : till 7th April, 2022',
              subtitle = 'Lift stations on x axis arranged by increasing population. Medians shown in red') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()



# Each Lift station - labelled with abbreviation (ordered by population)
plt.LS_named <- 
  {ggplot(LS_data,
          aes(x = Copies_Per_Liter_WW, y = abbr_LS,
              # colour = Target_Name,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(outlier.shape = NA, alpha = .4,
                   xmin = NA, xmax = NA) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.2, size = 0.4) + 
      
      # showing median
      stat_summary(geom = 'point', fun = 'median', 
                   shape = '|', size = 4,
                   colour = 'red') +
      
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'b') +  # tick marks to indicate log y axis
      
      ggtitle('Lift stations : till 7th April, 2022',
              subtitle = 'Lift stations on y axis arranged by increasing population upwards. Medians shown in red') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_x() %>%
  
  print()



# WWTP ----

# plot each WWTP, ordered by population
plt.WWTP <- 
  {ggplot(WWTP_data,
          aes(x = as.numeric(WWTP), y = Copies_Per_Liter_WW,
              # colour = Target_Name,
              label = Facility)) + 
      
      # boxplot
      # geom_boxplot(mapping = aes(colour = NA),
      #              outlier.shape = NA, alpha = .4) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.2) + 
      
      # showing median
      stat_summary(geom = 'point', fun = 'median', 
                   shape = '-', size = 7,
                   colour = 'red') +
      
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      ggtitle('WWTP : March 2021 - 7th April, 2022',
              subtitle = 'WWTP on x axis arranged by increasing population. Medians shown in red') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()


# Plot WWTP with abbreviated names on Y axis (ordered by population size - like above)

plt.WWTP_named <- 
  {ggplot(WWTP_data,
          aes(x = Copies_Per_Liter_WW, y = abbr_WWTP,
              # colour = Target_Name,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(aes(colour = NULL),
                   outlier.shape = NA, alpha = .4,
                   xmin = NA, xmax = NA,
                   show.legend = F) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.2, size = 0.3) + 
      
      # showing median
      stat_summary(geom = 'point', fun = 'median', 
                   shape = '|', size = 4,
                   colour = 'red') +
      
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'b') +  # tick marks to indicate log y axis
      
      ggtitle('Wastewater : March 2021 - 7th April, 2022',
              subtitle = 'WWTPs on y axis arranged by increasing population upwards. Medians shown in red') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_x() %>%
  
  print()


# LS by primary WWTP ----

# Plot LS alongside their primary WW data : boxplots

plt.LS_primary_WW <- 
  {ggplot(LS_and_WW,
          aes(x = Copies_Per_Liter_WW, y = abbr_name,
              colour = Facility_type,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(aes(colour = NULL),
                   outlier.shape = NA, alpha = .4,
                   xmin = NA, xmax = NA,
                   show.legend = F) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.2) + 
      
      # showing median
      stat_summary(geom = 'point', fun = 'median', 
                   shape = '|', size = 4,
                   colour = 'red') +
      
      # Facet by Primary_WWTP
      
      facet_wrap(~ Primary_WWTP,   # Use for making compact plot
                 scales = 'free_y') +
      
      # facet_grid(rows = vars(abbr_primary_wwtp), # Use for making aligned facets
      #            scales = 'free_y', space = 'free_y') +

      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'b') +  # tick marks to indicate log y axis
      
      ggtitle('Lift stations and respective primary WWTP : March/2021 - 7/April/2022',
              subtitle = 'Lift stations/WWTPs on y axis arranged by increasing population upwards. Medians shown in red') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_x() %>%
  
  print()


# Time series ----

# Plotting function (for each primary WWTP) : y = Copies_Per_Liter_WW, x = Date, colour by LS/WW..
plot_LS_time.series <- function(.df)
{
  {ggplot(.df, 
          aes(x = Date, y = Copies_Per_Liter_WW,
              colour = Facility)) + 
      
      # plot points
      geom_point(aes(alpha = positivity)) + 
      
      geom_line(aes(size = if_else(Facility_type == 'WWTP', 1, 0.5))) +
      scale_size_identity() # ensures line thickness follows weights above
    
    
  } %>% 
    
    print() # show plot
  
}



# Save plots ----

ggsave(str_c('qPCR analysis/', 'Lift stations by name_7-April-22', '.png'),
       width = 8, height = 9)


# save plots into html file
# calling r markdown file -- Abandoned for now
rmarkdown::render('scripts_extra/e8_html_plots_timeseries.Rmd', 
                  output_file = str_c('../qPCR analysis/', 'Timeseries:Liftstations_7-April-22', '.html'))

