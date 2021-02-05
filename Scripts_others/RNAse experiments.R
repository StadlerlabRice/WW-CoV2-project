# Ad-hoc file for Pavan's RNAse files processing

# Name editing file
nmchanger <- c('BCoV_M' = 'BCoV-qPCR')

# Select the data of boilquants : vacboil
RNase <- map(long_processed_minimal, 
              ~ filter(., str_detect(Sample_name, '922')) %>% 
                mutate(Target = str_replace_all(Target, nmchanger)) #%>%
                # mutate(WWTP = as.character(WWTP) %>%  as.numeric()) %>% 
                # mutate_cond(Measurement == 'Copy #' & Target == 'BCoV-ddPCR' , across(any_of(c('value','mean')), ~ . * 100)) 
)

# Plot
plt1 <- plot_mean_sd_jitter(.data_list = RNase, 
                     sample_var = extra_categories, exclude_sample = T, x_var = WWTP, ylabel = 'Genome copies/ul RNA') + 
    xlab('Expt conditions')

plt1 %>% 
  ggplotly(dynamicTicks = T)

plt1 %>% print()
