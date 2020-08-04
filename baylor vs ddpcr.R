# adhoc 713-715 baylor ddPCR vs baylor's own qPCR N1/N2

# load ddPCR data
dd <- read_sheet(sheeturls$wwtp_only_data, sheet = '713-715 baylor ddPCR') %>% 
  filter(Lab == 'B') %>% 
  mutate('Method' = 'R-ddPCR')

# load Baylor's excel files
q713 <- '../../../Covid Tracking Project/Rice and Baylor Combined Data/Baylor Data/7_13/' %>% 
  str_c('WWTP_713_Baylor_Results', '.csv') %>% 
  read_csv() %>% 
  group_by(WWTP, Target_Name) %>% 
  mutate('Method' = 'B-qPCR', WWTP_ID = str_c('713.', WWTP, '-', row_number()), .before = 1) %>% 
  filter(WWTP_ID %in% dd$WWTP_ID) %>% 
  ungroup()

q716 <- 'C:/Users/new/Box Sync/Covid Tracking Project/Rice and Baylor Combined Data/Baylor Data/7_16/' %>% 
  str_c('WWTP_716_Baylor_Results', '.csv') %>% 
  read_csv() %>% 
  group_by(WWTP, Target_Name) %>% 
  mutate('Method' = 'B-qPCR', WWTP_ID = str_c('715.', WWTP, '-', row_number()), .before = 1) %>% 
  filter(WWTP_ID %in% dd$WWTP_ID) %>% 
  ungroup()

# q merge
q.merge <- bind_rows(q713, q716) %>% 
  rename('Copies_Per_Litre_WW' = 'Copies_Per_Liter_WW')
  
# head to head fight!
merge.data <- bind_rows(dd, q.merge) %>% 
  mutate(Week = str_extract(WWTP_ID, '[:digit:]*(?=.)'))

# plotting
plt1 <- merge.data %>% ggplot(aes(WWTP, Copies_Per_Litre_WW, colour = Method)) + geom_point() + facet_grid(rows = vars(Target_Name), cols = vars(Week), scales = 'free_y') + ggtitle('713-715 Baylor ddPCR vs qPCR')

plt1 %>% format_logscale_y() %>% print()


long_target <- merge.data %>% select(-Copies_per_uL_RNA) %>% 
  pivot_wider(names_from = 'Target_Name', values_from = Copies_Per_Litre_WW)
  
plt2 <- long_target %>% ggplot(aes(`SARS CoV-2 N1`, `SARS CoV-2 N2`, colour = Method, shape = Week)) + geom_point() + scale_shape_manual(values = c(19, 2)) + ggtitle('713-715 Baylor ddPCR vs qPCR', subtitle = 'Copies_Per_Litre_WW')

plt2 %>% format_logscale_x() %>% format_logscale_y() %>% print()

long_method <- merge.data %>% select(WWTP_ID, Copies_Per_Litre_WW, Target_Name, Method) %>% 
  pivot_wider(names_from = Method, values_from = Copies_Per_Litre_WW)

plt3 <- long_method %>% ggplot(aes(`R-ddPCR`, `B-qPCR`, colour = Target_Name)) + geom_point() + ggtitle('713-715 Baylor ddPCR vs qPCR', subtitle = 'Copies_Per_Litre_WW') + geom_smooth(method = lm) + geom_abline(slope = 1, intercept = 0)

plt3 %>% format_logscale_x() %>% format_logscale_y() %>% print()
