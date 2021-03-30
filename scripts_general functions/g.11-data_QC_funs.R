# data frame or tibble quality check functions
# Prashant; 29/March/2021

# find the number of times each element repeats in a dataframe column
count_multiplicates <- function(.data, .colm, outliers_only = TRUE)
{ 
  # Inputs
  # .data = the data where you need to find duplicates
  # .colm = the column where replicates need to be counted
  
  
  # group by the desired column and count n() number of elements in the group
  out_multi <- .data %>% group_by({{.colm}}) %>% summarize(repeated_instances = n())
  
  if(outliers_only) {
    the_mode <- stat_mode_vector(out_multi$repeated_instances) # find the mode of the data using the handwritten function
    
    out_multi %>% 
      filter(! repeated_instances %in% the_mode) %>%  # remove most frequently repeated instances
      
      {if(is.character(pull(., {{.colm}})) ) 
        add_row(., {{.colm}} := 'most repeated', repeated_instances = the_mode) 
        else add_row(., {{.colm}} := 999, repeated_instances = the_mode)}  # add a row to refer to the mode that was removed
      
    # y <- .data  %>% pull({{.colm}}) %>% table # find the frequency table of the vector : named vector with frequency
    # return(y[which(y != stat_mode_vector(y))])  
    
  } else return(out_multi)
  
}


# find mode of a vector

stat_mode_vector <- function(x)
{
  y <- table(x) # find the frequency table of the vector : named vector with frequency
  names(y)[which(y == max(y))] %>% as.numeric() # report elements which repeat the maximum number of times
}
