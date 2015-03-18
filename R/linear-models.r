#================================================================================
# @linear-models.r, A R Script for model fitting
#
# This script was developed as part of PhD thesis
# 
# @author Antonio Prestes Garcia
# @date Feb/2015
#
# Changelog:
# 
#================================================================================

# ------------------------------------------------------------
# polynom computes linear polynomial models for data
#
# @param d data frame containing x and y series
# @param x series
# @param y series
# @param o the model degree
# @return the model fitted to data
# ------------------------------------------------------------
bs.polynom<- function(d,x,y,o) {
  model<- with(d,lm(y~poly(x,o,raw=TRUE)))
  return(model)
}

#--------------------------------------------------------------------------------
# bs.polynom.string Converts a polinomyal model output to a equation string
#
# @param m  The polynomial model
# @return 	The equation string as y= b + b x + b x^2 + ... + b x^n
#--------------------------------------------------------------------------------
bs.polynom.string<- function(m) {
  s<- paste0("y= ", format(coef(m)[1], digits = 2))
  
  for(i in 2:length(m$coefficients)) { 
    if(i <= 2) { 
      s<- paste0(s," + ", format(m$coefficients[i], digits = 2)," x") 
    } else {	
      s<- paste0(s," + ", format(m$coefficients[i], digits = 2)," x^",i-1) 
    }
  }
  return(s)
}


#--------------------------------------------------------------------------------
# bs.regression.string 
#
# @param m The regression model
# @return A string representation of regression equation and R squared. 
#--------------------------------------------------------------------------------
bs.regression.string<- function(m) {
  e<- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(coef(model)[1], digits = 2), 
                      b = format(coef(model)[2], digits = 2), 
                      r2 = format(summary(model)$r.squared, digits = 3)))
  return(as.character(as.expression(e)))                  
}