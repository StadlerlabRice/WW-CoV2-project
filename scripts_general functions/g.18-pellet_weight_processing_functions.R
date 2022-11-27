# g.18-pellet_weight_processing_functions.R


get_pellet_metadata <- function()
{
  # Get pellet weight related data (monkeypox or future targets, for copies/g calculation)
  week_name <- str_extract(title_name, '[:digit:]{6}')
  
  # get the Tube Label, pellet_mass and dry_mass_fraction
  pellet_weight_data <- read_sheet(sheeturls$pellet_weights, sheet = str_c(week_name, ' Pellets')) %>% 
    mutate(across(Label_tube, ~str_remove(., ' ') )) %>% # remove spaces from the Sample_name
    select(Label_tube, pellet_wet_mass, dry_mass_fraction) %>% 
    mutate(across(c(pellet_wet_mass, dry_mass_fraction), as.numeric)) %>% 
    
    # Since wells are merged for MPX analyses, the avg of replicate (2 and 1) will be merged into 1
    # TODO : ideally all MPX volumes also need this but they are all usually 50 ml so we are not bothering as of now
    
    mutate(merged_label = str_replace(Label_tube, '2$', '1')) %>% # make replicate 2 into 1 in a new temp column
    group_by(merged_label) %>% mutate(across(pellet_wet_mass, mean, na.rm = TRUE)) %>% # take avg of the 2 replicates
    filter(!str_detect(Label_tube, '2$')) %>% select(-merged_label) # remove the duplicate entry (old replicate 2)
  
}