# Ad-hoc script to plot Rice and Baylor data

read_these_sheets <- c('713 Rice', '713 Baylor') # sheet name(s) in the raw data 

dat.all <- map_dfr(read_these_sheets, ~ read_sheet(sheeturls$wwtp_only_data , sheet = .x) %>%  
                      mutate('Week' = str_match(.x, '[:digit:]*(?= Rice)')) %>%
                      rename('Target' = matches('Target'), 'Original Sample Volume' = matches('WW_vol|Original Sample Volume'), 'Ct' = matches('^CT', ignore.case = T)) %>% 
                      mutate_at('Target', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2')))
                    
) 

plt1 <- dat.all %>% {ggplot(., aes(x = WWTP, y = Recovery_Rate, colour = Lab)) + 
    geom_point() + ggtitle('713 Rice vs Baylor')} %>% 
  format_classic() 

plt1 %>% 
  print()

ggsave('qPCR analysis/713 rice vs baylor.png', height = 4, width = 12)

plt1 + ylim(c(0,100))
