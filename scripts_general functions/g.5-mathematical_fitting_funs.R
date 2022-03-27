# Mathematically related functions, curve fits etc.

# Linear regressions (~plots) ----


# getting regression line and R2 values to put into the standard curve plot 

# least square fitting - linear regression (typical use: for qPCR standard curve)
lm_std_curve <- function(df, trig = 0)
{
  x = df %>% pull(Quantity) %>% log10 ; y = df %>% pull(CT)
  m <- lm(y ~ x, df)
  lm_eqn(m, trig)
}


# Extract linear regression equation and parameters from the fit by lm
# Source: https://stackoverflow.com/a/7549819/9049673

lm_eqn <- function(m, trig = 0){
  # use trig = 'coeff' to output data frame of the fitting coefficients 
  # trig = 0 for output of equation (for pasting on the plot)
  
  # format the equation "y = mx + c, R^2 = Rsquare value" as text to put on plot
  eq <- substitute(italic(y) == b %.% italic(x)+ a*","~~italic(r)^2~":"~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 4), 
                        b = format(unname(coef(m)[2]), digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  
  # output a table with slope, y_intercept and r_square rounded off
  if(trig == 'coeff') tibble(slope = round(coef(m)[2], 2), y_intercept = round(coef(m)[1], 2), r_square = round(summary(m)$r.squared, 3))
  
  else as.character(as.expression(eq)); 
}




