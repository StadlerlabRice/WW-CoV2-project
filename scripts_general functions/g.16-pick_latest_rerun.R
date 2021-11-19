
# pick the latest rerun if multiple data appears with the same Sample_ID across different Run_weeks
# for use within 3-Run_weekly_comparisons.r when joining data across runs, across Run_weeks..

pick_latest_rerun <- function(.data)
{

  # find the samples that have reruns
  find_reruns <- .data %>% 
    select(WWTP, Run_week, Target_Name, Copies_Per_Liter_WW, Sample_ID) %>% 
    group_by(Target_Name, WWTP, Sample_ID) %>% # this is what the replicates are determined on
    mutate(id = row_number()) %>% # the id is used to check for repeats in the data within each group
    
    {filter(., id > 1) %>% # first filter the rerun samples
        pull(Sample_ID) -> subset_sample_IDs # pull them into a temporary variable
      filter(., Sample_ID %in% subset_sample_IDs) # identify all runs of the same sample_IDs
    }
  
  # decide on the old samples to be thrown out
  find_reruns %>% summarise(Run_week = fct_drop(Run_week) %>% levels %>% min) -> subset_data_to_discard
  
  # remove these samples from the orginal dataset (with more columns :( )
  reduced_dat <- anti_join(.data, subset_data_to_discard)  
  
  
}
