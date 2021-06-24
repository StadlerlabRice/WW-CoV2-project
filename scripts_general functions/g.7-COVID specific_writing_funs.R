# writing data and other specific COVID wastewater related functions

# Data writing output ----

# This function writes to the specified google sheet if the current sheet does
# not exist. If the sheet does exist it will ask the user before writing.
check_ok_and_write <- function(data, sheet_URL, title_name)
{
  write_ok <- TRUE
  sheet_dne <- FALSE
  
  # this block sets sheet_dne to true if the sheet does not exist
  sheet_dne <- tryCatch(
    expr = {
      read_sheet(sheet_URL, sheet = title_name)
      message("Sheet already exists")
      FALSE
    },
    error = function(e){
      message('Sheet does not exist')
      return(TRUE)
      print(e)
    }
  )
  
  # if the sheet exists (sheet_dne is false), then ask the user if
  # they want to overwrite. If the user selects cancel, then abort
  if (!sheet_dne) {
    write_ok <- menu(c('Yes', 'No'), title = paste("A sheet with the name", title_name, "already exists. Do you want to overwrite?", sep=" "))
    if (write_ok == 2){
      # stop("Cancel selected, script aborted.")
      print(str_c("Not overwriting: ", title_name))
    }
  }
  
  if (write_ok == 1) {
    tryCatch({ R.utils::withTimeout({write_sheet(data, sheet_URL, sheet=title_name)}, 
                                          substitute = FALSE,
                                          timeout = 20, # 10 second timeout
                                          onTimeout = "warning")},
             warning = function(e) 
             {cat('Sheet was not written due to timeout. Manually copy the excel file from excel files/Archive/Data dump files')
               message(e)
               write_csv(data, str_c('excel files/Archive/To google sheets/', title_name, '.csv'), na = '')},
             
             error = function(e) 
             {cat('Sheet was not written due to error. Manually copy the csv file from excel files/Archive/To google sheets/ 
                  to google sheets for record')
               message(e)
               write_csv(data, str_c('excel files/Archive/To google sheets/', title_name, '.csv'), na = '')}
    )
  }

return() # Dummy output return, since this function is used only for side effects
  
}


# Manhole/Bayou Naming Utilities ----
get_manhole_names <- function() {
  lookup_table <- read_sheet(sheeturls$biobot_id, 'All manhole')
  manhole_codes <- lookup_table %>% pull('Facility SYMBOL') %>% str_replace_all(" ", "")
  scrub_spaces <- manhole_codes %>% str_replace_all(" ", "") %>% paste(collapse = "|")
  return(scrub_spaces)
}


get_bayou_names <- function() {
  lookup_table <- read_sheet(sheeturls$biobot_id, 'All Bayou')
  manhole_codes <- lookup_table %>% pull('Facility SYMBOL') %>% str_replace_all(" ", "")
  scrub_spaces <- manhole_codes %>% str_replace_all(" ", "") %>% paste(collapse = "|")
  return(scrub_spaces)
}



# LOD Computation Utilities ----

#' This function runs append_LOD_info() as many time as there are unique targets
#' in the input dataframe under the Target column. Then sticks the data back
#' together in a dataframe. The result should have the same length as the input
#' 
#' fl: a dataframe from qPCR data dump
complete_LOD_table <- function(fl) {
  targs <- unique(fl$Target)
  totalsheet <- fl[0,]
  
  for(i in 1:length(targs)) {
    nextsheet <- append_LOD_info(fl, targs[i])
    totalsheet <- rbind(totalsheet, nextsheet)
  }
  
  return(totalsheet)
  
}



#' This function appends info related to the LOD/LOB/LOQ to the sheet. This
#' function can only do this for one PCR target at a time.
#' 
#' fl: a dataframe from qPCR data dump
#' 
#' targ: the name of a target from the Target column of the dataframe. Example
#' "N1_multiplex", "N2_multiplex", "BCoV" etc.

append_LOD_info <- function(fl, targ) {
  fl <- fl %>% filter(Target == targ)
  
  # Pull negative controls out
  negative_controls <- fl %>% filter(assay_variable == 'NTC'| 
                                       assay_variable == 'DI'| 
                                       str_detect(assay_variable, regex('BLANK', ignore.case = TRUE)) )
  
  
  # Pull any rows with 3 droplets a.k.a the LOQ
  threes <- fl %>% filter(PositiveDroplets == 3)
  # If no rows has 3 droplets then the concentration is hard coded to 0.7
    # Estimate for 3 droplets is 0.7674 copies/ul RNA (Wonder if DZ rounded down to 0.7)
    # LOD Details - https://docs.google.com/document/d/1V1Dun0vMMb4XpyJELzMI5ClO3wWziE56Mblrm1kDyMo/edit
  # Otherwise, take the mean of 3 droplet concentrations
  if(dim(threes)[1] == 0) {
    three_copies <- 0.7
  } else {
    three_copies <- mean(threes$Copies_per_uL_RNA)
  }
  
  # Calculate the LOB
  limit_blank <- mean(negative_controls$Copies_per_uL_RNA, na.rm = TRUE) + 
    (1.6 * sd(negative_controls$Copies_per_uL_RNA, na.rm = TRUE))
  
  # Calculate the LOD
  LOD <- three_copies + limit_blank
  
  # Put everything into the table
  new_table <- fl %>% mutate(Positivity = case_when(Copies_per_uL_RNA < LOD ~ "Negative",
                                                    Copies_per_uL_RNA >= LOD ~ "Positive",)) %>%
    mutate(LimitOfDet = LOD)
  
  return(new_table)
}


# Plotting functions ----

# plotting individual replicates
plot_biological_replicates <- function(results_abs, title_text = title_name, xlabel = plot_assay_variable)
{ # Convenient handle for repetitive plotting 'Copy #' vs biological replicate
  
  plt <- results_abs %>% ggplot(aes(x = `Tube ID`, y = Copies_per_uL_RNA, color = Target)) + ylab('Copies/ul RNA extract') +    # Specify the plotting variables 
    geom_point(size = 2) + facet_grid(~`Sample_name`, scales = 'free_x', space = 'free_x') + # plot points and facetting
    ggtitle(title_text) + xlab(xlabel)
  plt.formatted <- plt %>% format_classic(.) %>% format_logscale_y() # formatting plot, axes labels, title and logcale plotting
}
