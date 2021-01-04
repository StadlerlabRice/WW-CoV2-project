read_these_sheets <- c('dd.WW94_1221_N1N2', 'dd.WW95_1222_Bayou_DZ ')

list_raw_quant_data <- map(read_these_sheets, ~ read_sheet(sheeturls$data_dump, sheet = ., range = 'A:J'))

spike_conc <- 200000 #copies/mL
spike_vol <- 0.005 # mL
elution_volume <- 50 # uL
spike_copies <- spike_conc * spike_vol

# bind multiple reads and clean up names
direct_extract_data <- bind_rows(list_raw_quant_data) %>% 
  rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it 
  # unite('Biobot_id', c(Sample_name, assay_variable), sep = '', remove = F) %>%
  select(-`Well Position`) %>% 
  mutate_at('assay_variable', as.character) %>% 
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% 
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) %>% # make a unique column for matching volumes
  filter(Sample_name == 'd1222') %>%
  mutate(Spiked = case_when(grepl('U', assay_variable) ~ 'Unspiked',
                            grepl('S', assay_variable) ~ 'Spiked')) %>%
  mutate(assay_variable = sub('.', '', assay_variable))
  # mutate(vol_extracted = 0.3) %>%
  # mutate(Recovered = `Copy #` * 1e3 * elution_volume/vol_extracted)

spikein_data <- bind_rows(list_raw_quant_data) %>% 
  rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it 
  # unite('Biobot_id', c(Sample_name, assay_variable), sep = '', remove = F) %>%
  select(-`Well Position`) %>% 
  mutate_at('assay_variable', as.character) %>% 
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% 
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) %>% # make a unique column for matching volumes
  filter(grepl('^L$|^M$|^A$|^O$|ZL|ZM|SA|SO', assay_variable)) %>%
  filter(Sample_name == 1221) %>%
  drop_na() %>%
  mutate(Spiked = case_when(grepl('Z.|S.', assay_variable) ~ 'Spiked',
                            grepl('.', assay_variable) ~ 'Unspiked')) %>%
  mutate('Spike Content' = case_when(grepl('L|M', assay_variable) ~ 'Zeptometrix Surrogate',
                            grepl('A|O', assay_variable) ~ 'Synthetic RNA')) %>%
  mutate(assay_variable = sub('Z', '', assay_variable)) %>%
  mutate(assay_variable = sub('S', '', assay_variable))


ggplot(direct_extract_data) +
  aes(x = assay_variable, y = `Copy #`, colour = Spiked) +
  geom_boxplot() +
  geom_jitter(aes(shape = Target)) +
  scale_color_hue() +
  ylab('Copies per uL Template') +
  xlab('WWTP') +
  scale_x_discrete(labels=c("Forest Cove", "Homestead", "DI Water")) +
  geom_hline(yintercept=20, linetype="dashed", size=1, alpha=0.5) +
  geom_hline(yintercept=2.535165, color="red", linetype="dashed", size=1, alpha=0.5) +
  theme_classic()

ggplot(spikein_data) +
  aes(x = assay_variable, y = `Copy #`, colour = Spiked) +
  geom_jitter(aes(shape = Target)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(vars(`Spike Content`), scales="free") +
  scale_x_discrete(labels=c("West District", "Northeast", "FWSD23", "Greenridge")) +
  ylab('Copies per uL Template') +
  xlab('WWTP') +
  geom_hline(yintercept=2.102333, color="red", linetype="dashed", size=1, alpha=0.5) +
  expand_limits(y = 0)

L_mean <- spikein_data %>% filter(assay_variable == 'L') %>% group_by(Spiked) %>% summarize(mean = mean(`Copy #`)) %>% select(mean)
M_mean <- spikein_data %>% filter(assay_variable == 'M') %>% group_by(Spiked) %>% summarize(mean = mean(`Copy #`))
