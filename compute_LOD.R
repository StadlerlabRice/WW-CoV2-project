# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file
source('./inputs_for_analysis.R') # Source the file with user inputs

complete_LOD_table <- function(fl) {
  targs <- c('N1_multiplex', 'N2_multiplex', 'BCoV')
  
  n1sheet <- append_LOD_info(fl, targs[1])
  n2sheet <- append_LOD_info(fl, targs[2])
  bcovsheet <- append_LOD_info(fl, targs[3])
  
  totalsheet <- rbind(n1sheet, n2sheet, bcovsheet)
  
  return(totalsheet)
  
}

append_LOD_info <- function(fl, targ) {
  fl <- fl %>% filter(Target == targ)
  
  # Pull negative controls out
  negative_controls <- fl %>% filter(Sample_name == 'NTC'| assay_variable == 'DI'| assay_variable == 'BLANK')
  
  # Pull any rows with 3 droplets a.k.a the LOQ
  threes <- fl %>% filter(Positives == 3)
  # If no rows has 3 droplets then the concentration is hard coded to 6
  # Otherwise, take the mean of 3 droplet concentrations
  if(dim(threes)[1] == 0) {
    three_copies <- 6.5
  } else {
    three_copies <- mean(threes$CopiesPer20uLWell)
  }
  
  # Calculate the LOB
  limit_blank <- mean(negative_controls$CopiesPer20uLWell) + (1.6 * sd(negative_controls$CopiesPer20uLWell))
  # Calculate the LOD
  LOD <- three_copies + limit_blank
  
  # Put everything into the table
  new_table <- fl %>% mutate(Positivity = case_when(CopiesPer20uLWell < LOD ~ "Negative",
                                       TRUE ~ "Positive",)) %>%
    mutate(LimitOfDet = LOD)
  
  return(new_table)
}
