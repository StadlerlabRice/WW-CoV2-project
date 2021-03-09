# B117 short circuit : Short circuit : Saves data for HHD in csv and wwtp output sheet

# Action begins ----

# streamlined output for the HHD

present_only_WW <- presentable_data %>% 
  filter(str_detect(WWTP, WWTP_names)) # retain only WWTP data

# control_cols_for_deselection <- str_c('.', c('DI', 'NTC', 'Blank', 'WHC', 'Mix', 'b117'), '-', collapse = '|')

  # Write data if not empty
  if(present_only_WW %>% plyr::empty() %>% !.){
    check_ok_and_write(present_only_WW, sheeturls$wwtp_only_data, title_name) # save results to a google sheet, ask for overwrite
    write_csv(present_only_WW, path = str_c('excel files/Weekly data to HHD/', title_name, '.csv'), na = '') # output csv file
  }

present_manhole_samples <- presentable_data %>% filter(str_detect(WWTP, manhole_sample_symbols))

# Write data if not empty
if(present_manhole_samples %>% plyr::empty() %>% !.){
  check_ok_and_write(present_manhole_samples, sheeturls$wwtp_only_data, str_c(title_name, ' manhole samples')) # save results to a google sheet, ask for overwrite
  write_csv(present_manhole_samples, path = str_c('excel files/Weekly data to HHD/', title_name, ' manhole samples.csv'), na = '') # output CSV file
}