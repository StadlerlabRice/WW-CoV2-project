# g.19-get_volumes_from_sample_registry.R

get_volumes_from_sample_registry <- function()
{
  # Get volumes data from google sheet : "Sample registry"
  volumes.data_registry <- 
    range_speedread(ss = sheeturls$sample_registry,  # this is 20x faster than read_sheet!
                    sheet = 'Concentrated samples',
                    range = 'A:H', # specify range to read
                    col_types = 'ccncccnn')  %>% # pre-specify column types within the range
    
    # rename col names
    rename('Label_tube' = `Label on tube`, 
           'Biobot_id' = `Biobot/other ID`,
           Filtered_WW_vol = `WW volume filtered (ml)`,
           
           Sample_Type = `Grab vs Composite`,
           No_of_Hours_Missed = `HHD Notes (# of samples missed/hours)`, 
           
           # spiking relevant data
           Vaccine_ID = `Stock ID of Spike`,
           'Received_WW_vol' = `Total WW volume received (ml)`, 
           WW_weight = `Total WW weight measured (kg)`) %>% 
    
    select(Received_WW_vol, Label_tube, Filtered_WW_vol, Vaccine_ID, 
           `Biobot_id`,
           WW_weight, Sample_Type, No_of_Hours_Missed) %>% # select only the useful columns
    
    distinct() %>% # for removing repeated data in early stuff, before 608 (interferes with the merging of volumes for same bottles)
    mutate_at('Label_tube', ~str_remove_all(., " ")) %>% 
    mutate_at('Biobot_id', str_remove,  ' ') %>% 
    
    # Extrapolating volumes for same bottle - If weights are written out, we assume different bottles with different volumes
    mutate(., unique_labels = str_remove_all(Label_tube,'[:digit:]$'))  %>% # Remove the trailing single digit that tells the replicate # 
    # this will be changed to the 'Bottle' column? Too much data entry
    
    group_by(unique_labels) %>% 
    mutate(across(Received_WW_vol, 
                  ~ifelse(is.na(WW_weight), # If any of the replicates doesn't have weight
                          if(all(is.na(.))) NA else max(., na.rm = T), # the make it's volume the max
                          .) # Unless the volume is all NA in which case NA is used insteado of the max
    ) ) %>% # This will not allw MAX to create -Inf :: to prevent errors in write_sheet 
    ungroup() %>% 
    select(-unique_labels, -WW_weight)
  
}