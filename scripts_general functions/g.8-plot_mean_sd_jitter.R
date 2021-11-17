
# Plotting mean, sd and individual replicates in a jitter form
# Yet to be fully generalized function. But should work for any data frame by adding dummy columns. 
# Default parameters correspond to COVID-qPCR files

plot_mean_sd_jitter <- function(.data = presentable_data, 
                                long_format = FALSE,
                                
                                measure_var = 'Copy #', 
                                colour_var = Target_Name, x_var = assay_variable, y_var = Copies_Per_Liter_WW, 
                                facet_var = NULL, 
                                
                                unique_columns_to_incl = c('Facility'), # columns to determine uniqueness for calc mean ...
                                # ... will automatically include the x_var and colour_var
                                
                                # Filtering variables
                                sample_filtering_var = '.*', exclude_sample = F, 
                                WWTP_filtering_var = wwtp_manhole_names, exclude_WWTP = F, 
                                target_filter_var = '.*', 
                                
                                ascending_order = FALSE, 
                                title_text = title_name, ylabel = 'Genome copies/ul RNA', xlabel = plot_assay_variable, 
                                facet_style = 'grid')

{ # Convenient handle for repetitive plotting of individual replicates as points, and mean as horizontal bars
  # Specify data format: typically wide vs long which is a special case (specify in long_format = FALSE or TRUE)
  
  
  # filtering data to be plotted by user inputs
  .dat_filtered <- 
    .data %>% 
    filter(., if('Sample_name' %in% colnames(.)) str_detect(`Sample_name`, sample_filtering_var, negate = exclude_sample) else TRUE, 
           if('WWTP' %in% colnames(.)) str_detect(WWTP, WWTP_filtering_var, negate = exclude_WWTP) else TRUE, 
           str_detect(Target_Name, target_filter_var))
  
  mean_y_var_str <- expr(!!str_c('mean_', deparse(enexpr(y_var)) ) ) # make a string to use mean_{{y_var}} in regular functions
  
  
  # Long format : filtering data to be plotted by user inputs : ---- OBSOLETE ; do not use
  if(long_format) # use long format if not plotting Copy #s - ex. Recovery, % recovery etc.
  { # If data is in long_format, multiple columns to be plotted simultaneously will be put into the same column..
    # .. using pivot_longer() before feeding data into this function. This is a special case. Useful when copies/L is ..
    # .. plotted along with Spiked-in copies/L (with open circles)
    
    .data_to_plot <- filter(.dat_filtered, Measurement == measure_var)
    
    if(ascending_order) .data_to_plot %<>% mutate_at('WWTP', as.character) %>% 
      arrange(`mean`) %>% # this will not work, there is not column named 'mean' anymore
      mutate_at('WWTP', as_factor)
    
    y_var <- sym('value') # default y variable is value
    
    # summ_actual_spike_in <- .dat_filtered %>% filter(str_detect(Measurement,'Actual'))
    
  } else
    
  {
    .data_to_plot <- .dat_filtered %>% 
      group_by({{x_var}}, {{colour_var}}, across(any_of(unique_columns_to_incl))) %>%  # group
      mutate( "mean_{{y_var}}" := mean({{y_var}}) ) %>% 
      
    {if(ascending_order) 
      { ungroup(.) %>% # temporarily ungroup so that x_var can be changed
        mutate(., across('WWTP', as.character)) %>%  # make x_var a character so that it can be factorized again in order
        arrange(across( any_of(mean_y_var_str) )) %>% 
        mutate_at('WWTP', as_factor) %>% # make WWTP factor again, this time it will be in ascending order of mean_y_var
        group_by({{x_var}}, {{colour_var}}, across(any_of(unique_columns_to_incl))) 
      } else .  # group again
    } 
    
  }
  
  # Exit with a useful message if data is empty
  if(plyr::empty(.data_to_plot)) return('Data is empty, nothing to plot')  
  
  # plotting
  plt1 <- .data_to_plot %>% ggplot(aes(x = {{x_var}}, y = {{y_var}}, colour = {{colour_var}})) +
    geom_point(aes_string(y = mean_y_var_str), size = 2) + 
    # geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
    
    # Individual data points
    geom_jitter(mapping = aes(alpha = map_chr({{y_var}}, ~. == 0), 
                              size = map_chr({{y_var}}, ~. == 0)), 
                width = .2, show.legend = F ) +
    scale_alpha_manual(values = c(.3, 1)) + scale_size_manual(values = c(1, 2)) + # manual scale for emphasizing unamplified samples
    
    # # Plotting actual spike ins (only for Recovery plot'; only with long format data )
    # { if(measure_var == 'Recovered') list(geom_point(data = summ_actual_spike_in, colour = 'black', shape = 21), 
    #                                       geom_line(data = summ_actual_spike_in, aes(group = {{colour_var}})))
    # } +
    
    # Facetting
    facet_grid(cols = vars({{facet_var}}), scales = 'free_x', space = 'free_x') +
    
    # experimental - conditional facetting (doesn't work for unknown reasons) : Just facet_grid() the output to remove facets or add new!
    # { if (facet_style == 'grid') list(facet_grid(cols = vars({{facet_var}}), scales = 'free_x', space = 'free_x'))
    #   if (facet_style == 'wrap free') list(facet_wrap(facets =  vars({{facet_var}}), scales = 'free')) 
    #   else NULL
    # } +
    
    # Labelling
    ggtitle(title_text) + ylab(ylabel) #+ xlab(xlabel)
  
  # plt1.formatted <- plt1 %>% format_classic() # clean formatting, removes grey background, change colourscheme
  print(plt1)
  
}