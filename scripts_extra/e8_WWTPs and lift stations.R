# Plotting school samples by grade and enrollment from collated data sheet
# Prashant
# 11 April 2022

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file


title_name <- 'Lift stations and their WWTP_7-April-22'

# Load files ----
WWTP_data <- 
  read_csv('excel files/boxplots_for_LS_7-4-22/Rice_WWTP_All_Results_20220404.csv') %>% 
  
  # add a new column to mark WW samples and order by increasing population
  mutate('Facility_type' = 'WWTP',
         WWTP = fct_reorder(WWTP, Population)) %>% 
  
  # filter dates after March/21 only
  mutate(across(Date,
                ~ lubridate::mdy(.x))) %>%  # convert dates into good format
  
  filter(Date > lubridate::my('3/21')) # filter samples later than march 2021


LS_data <- 
  read_csv('excel files/boxplots_for_LS_7-4-22/Rice_LS_All_Results_20220404.csv') %>% 
  mutate('Facility_type' = 'Lift station',
         WWTP = fct_reorder(WWTP, Population)) %>% 
  
  # change date format
  mutate(across(Date,
                ~ lubridate::mdy(.x)))


LS_metadata <- read_csv('excel files/boxplots_for_LS_7-4-22/LS_to WWTP_connection.csv')

# polishing ----

# merge only the relevant rows of WWTP and LS data
merged_ww.ls_selected <- 
  
  list(WWTP_data, LS_data) %>%
  
  # extract the result col and facility type only
  map(~ select(.x, Copies_Per_Liter_WW, Facility_type)) %>% 
  
  # bind the data row by row
  bind_rows()


# Merge lift station to respective WWTP
LS_comprehensive <- LS_data %>% 
  
  # join with the lookup list
  left_join(LS_metadata, by = c('WWTP' = 'Short_name')) %>% 
  # rename the primary wwtp to aid joining
  rename(LS = 'WWTP') %>% 
  
  # join with WWTP data of the corresponding LS
  left_join(WWTP_data, by = c('Primary_WWTP' = 'WWTP', 'Date', 'Target_Name', 'Lab'))
# Need to create a custom function that regex matches strings, difference matches data within a week
# and pass it to fuzzy_left_join(match_fun = ..)


# Troubleshooting the fuzzyjoin with dummy data containing dates

# check dates match of LS and WWTP
LSd <- LS_data %>% 
  filter(str_detect(LS, 'ALIEF')) %>% 
  pull(Date) %>% unique()

WWd <- WWTP_data %>% 
  filter(str_detect(WWTP, 'UPPER BRAYS')) %>% 
  pull(Date) %>% unique()

# need to match within 2-3 days.. or within the same week
# divide a into two parts a1, a2 and join the first 4 dates to each of them..

LSd[1] - WWd[1] == 1

fuzzyjoin::difference_left_join(a1, a3, by = c('d', 'a1' = 'a2', 'a3') , max_dist = 2)
a3 <- mutate(a2, a3 = 'a')








# function to label the number of data points on the plot
# Copied from stack overflow : https://stackoverflow.com/a/31140454/9049673
give.n <- function(x){ 
  return(c(y = mean(x), label = length(x)))
}


# Plots ----

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


# Each school - by enrollment 
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
          aes(x = Copies_Per_Liter_WW, y = WWTP,
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



# Save plots ----

ggsave(str_c('qPCR analysis/', 'WWTP vs Lift stations_7-April-22', '.png'),
       width = 6, height = 4)


# save plots into html file
# calling r markdown file -- Abandoned for now
rmarkdown::render('scripts_extra/e6_all school overview.Rmd', 
                  output_file = str_c('./qPCR analysis/', title_name, '.html'))

