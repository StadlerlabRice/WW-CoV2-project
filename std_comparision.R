# Ad hoc script
# comparing standard curves

s17 <- bring_results %>% 
  mutate(Target = str_c(Target, '_17')) %>% 
  filter(Task == 'STANDARD')

s16 <- bring_results %>% 
  mutate(Target = str_c(Target, '_16')) %>% 
  filter(Task == 'STANDARD')

s15 <- bring_results %>% 
  mutate(Target = str_c(Target, '_15')) %>% 
  filter(Task == 'STANDARD')

s_comb <- bind_rows(s15, s16, s17)

s_comb %>% filter(str_detect(Category, 'NTC|Std')) %>%  plotstdcurve(title_name, 'log(Copy #)') %>% print() # plot standard curve
