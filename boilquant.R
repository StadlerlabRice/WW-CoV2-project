# Ad-hoc file for boilquant file processing

# Name editing file
nmchanger <- c('BCoV_M' = 'BCoV-qPCR')

# Select the data of boilquants : vacboil
boilqr <- map(long_processed_minimal, 
             ~ filter(., str_detect(Sample_name, 'vac')) %>% 
               mutate(Target = str_replace_all(Target, nmchanger)) %>% 
               mutate(WWTP = as.character(WWTP) %>%  as.numeric()) %>% 
             mutate_cond(Measurement == 'Copy #' & Target == 'BCoV-ddPCR' , across(any_of(c('value','mean', 'sd')), ~ . * 100)) 
             )

plt2 <- plot_mean_sd_jitter(.data_list = boilqr, 
                     sample_var = extra_categories, exclude_sample = T, x_var = WWTP, ylabel = 'Genome copies/ul RNA') + 
    xlab('Lysis time (min)')

plt2

plt2 %>% 
  ggplotly(dynamicTicks = T)



