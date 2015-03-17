# ------------------------------------------------------------
# Linear models 
#
# ------------------------------------------------------------

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