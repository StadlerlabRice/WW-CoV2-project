# Plotting school samples by grade and enrollment from collated data sheet
# Prashant
# 11 April 2022

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file


title_name <- 'Schools by enrollment_7-April-22'

# Load file
all_data <- read_xlsx('excel files/boxplots_for_LS_7-4-22/Wastewater Surveillance manhole Report 03282022.xlsx')

school_metadata <- read_xlsx('excel files/boxplots_for_LS_7-4-22/HISD_school_list_clean.xlsx') %>% 
  
  # join to get shortened name of the schools as listed in the data file
  right_join(read_xlsx('excel files/boxplots_for_LS_7-4-22/Wastewater Schools in PCR testing program - SCHOOL ID - 02142022.xlsx'),
             by = 'school_id')

# View data
# view(school_data)

# filter rice only data that is positive or inconclusive
school_rice.data <- 
  
  all_data %>%
  
  # Filter relevant data
  filter(Lab == 'Rice',             # select 'Rice' data
         `Current Result` != 'Negative',      # non negative
         `Facility Type` == 'School') %>%   # only from Schools
  
  # Attach school metadata
  left_join(school_metadata, by = 'Facility') %>% 
  
  # Order by enrollment
  arrange(Enrollment_as_Oct2019) %>% 

  pivot_longer(cols = starts_with('Rep'),
               names_to = 'Replicate',
               values_to = 'Copies_per_litre')

# function to label the number of data points on the plot
# Copied from stackoverflow : https://stackoverflow.com/a/31140454/9049673
give.n <- function(x){ 
  return(c(y = mean(x), label = length(x)))
}


# make plot
plt.general <- 
  {ggplot(school_rice.data,
          aes(x = Replicate, y = Copies_per_litre,
              fill = Grade_Level,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(outlier.shape = NA, alpha = .6) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(shape = 21,
                 alpha = 0.2, 
                 position = position_jitterdodge(dodge.width = .75, 
                                                 jitter.width = .12, jitter.height = 0)) + 
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks() +  # tick marks to indicate log y axis
      
      ggtitle('School data by grade : till 7th April, 2022',
              subtitle = 'Number of data points shown above the median line. Whiskers show 25% and 75% quartiles') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()


# Each school - ordered by enrollment 
plt.eachschool <- 
  {ggplot(school_rice.data,
          aes(x = Enrollment_as_Oct2019, y = Copies_per_litre,
              fill = Replicate,
              label = Facility)) + 
      
      # boxplot
      # geom_boxplot(outlier.shape = NA, alpha = .6) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(shape = 21,
                 alpha = 0.2, 
                 position = position_jitterdodge(dodge.width = .75, 
                                                 jitter.width = .12, jitter.height = 0)) + 
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      ggtitle('School data by enrollment : till 7th April, 2022',
              subtitle = 'Number of data points shown above the median line. Whiskers show 25% and 75% quartiles') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()

# Save plots ----

ggsave(str_c('qPCR analysis/', title_name, '.png'),
       width = 10, height = 8)


# save plots into html file
# Abandoned
