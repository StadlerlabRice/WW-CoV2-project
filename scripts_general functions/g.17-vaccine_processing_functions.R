# g.17-vaccine_processing_functions

# Obsolete stuff: Making small functions that process vaccine spike-in data (to keep the main code cleaner)

calculations_for_vaccine_data <- function(.df)
{
  .df %>% 
    mutate(Surrogate_virus_input_per.L.WW = spiking_virus_vaccine_stock_conc * spike_virus_volume / (Received_WW_vol * 1e-3), 
           Percentage_recovery_BCoV = 100 * Copies_Per_Liter_WW/Surrogate_virus_input_per.L.WW)
  
  
  # (source for chemagic conc. factor calculations : Google sheet below
  # "concentration factor calc" : https://docs.google.com/spreadsheets/d/19oRiRcRVS23W3HqRKjhMutJKC2lFOpNK8aNUkC-No-s/edit#gid=2134801800)
  
  # Remove the columns not relevant targets that are not surrogate viruses (BCoV or BRSV) or for Vaccine stocks
  mutate_cond(!str_detect(Target_Name, 'BCoV|BRSV') |
                str_detect(Sample_name, 'Vaccine'),  # And vaccines
              Surrogate_virus_input_per.L.WW = NA, 
              Percentage_recovery_BCoV = NA) %>% 
    
    # And for vaccine (ie. Vaccine stock quants), remove Copies_Per_Liter_WW
    mutate_cond(str_detect(Sample_name, 'Vaccine'),
                Copies_Per_Liter_WW = NA) %>%  # not relevant for vaccine stock
    
    
    select(-spiking_virus_vaccine_stock_conc)
    
}


# checking for duplicates in the vaccine IDs

check_vaccine_ID_duplicates <- function()
{
  # Vaccine ID duplication value check - Brings user attention to duplicate values in the Vaccine_summary in data dump
  processed_quant_data$Vaccine_ID %>% 
    unique() %>%  # find all the Vaccine IDs in use for the current week data
    paste(collapse = '|') %>% # make a regular expression (regex) combining them
    
    {filter(spike_list, str_detect(Vaccine_ID, .))} %>% 
    unique %>%  # filter the list of vaccine data with these IDs (from data dump) that are unique
    
    {if(nrow (.) > 1) {
      duplicate_vaccine_values <- . 
      view(duplicate_vaccine_values)
      stop("Duplicate vaccine IDs found in the data dump, 
         please check the table: *duplicate_vaccine_values* for more information")
    }
      else if(nrow(.) > 0 &&  .$spiking_virus_vaccine_stock_conc == 0){
        zero_vaccine_values <- . 
        view(zero_vaccine_values)
        stop("Zeros found in vaccine quants in the data dump, please fix") 
      }
    }
  
}


# Write and save vaccine data from 1-processing functions
process_vaccine_data_to_googlesheet <- function()
  # calling only for the side effect, no input or outputs to this function
  
{
  
  # Saving vaccine data into Vaccines sheet in data dump: For easy book keeping
  vaccine_data <- final_data_with_LODs %>% filter(str_detect(Sample_name, 'Vaccine|Vaccineb|Vacboil|stdVac') & !str_detect(Target, '^N.')) %>%
    mutate('Prepared on' = '',
           Week = str_extract(flnm, '[:digit:]{6}') %>% unlist() %>% str_c(collapse = ', '), # get 6 digit date :ddmmyy
           Vaccine_ID = assay_variable, 
           .before = 1) %>% 
    mutate(Run_ID = str_extract(flnm, 'dd.WW[:digit:]*'), CT = NA) %>% 
    select(`Prepared on`,	Week,	Vaccine_ID,	`Well Position`,	
           CT,	Target,	Sample_name,	assay_variable,	`Tube ID`,	
           biological_replicates,	Copies_per_uL_RNA,	Run_ID)
  
  # Add to existing sheet
  if(vaccine_data %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data, 'Vaccines')
  
  # Mean of vaccine data
  vaccine_data.mean <- vaccine_data %>% ungroup() %>% 
    select(1:3, Target, Copies_per_uL_RNA, Run_ID) %>% group_by(across(-Copies_per_uL_RNA)) %>% 
    summarise(across(Copies_per_uL_RNA, list(Mean_qPCR = mean, SD_qPCR = sd), na.rm = T), .groups = 'keep') %>% 
    
    # adding a RNA extraction conc. factor only if not boiled (Sbxx naming)
    mutate('[Stock conc.] copies/ul' = `Copies_per_uL_RNA_Mean_qPCR` * if_else(str_detect(Vaccine_ID, 'S[:digit:]+'), 50/20, 1), 
           'Estimated factor' = '',
           Comments = '',
           'Conc normalized to estimated factor' = '') %>% 
    relocate(Run_ID, .after = last_col()) %>% 
    mutate('x' = '', .before = 1)
  
  # Add to existing sheet in Vaccine_summary
  if(vaccine_data.mean %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data.mean, 'Vaccine_summary')
  
  
}

