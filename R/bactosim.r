#================================================================================
# @bactosim.r, The library entry point
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
# .onLoad shows the library disclaimer
#
# ------------------------------------------------------------
.onLoad<- function(libname, pkgname) {
  packageStartupMessage("BactoSim(haldane) simulation and data analysis tools version 1.1.0\n")
  
  # ----- Libraries ---------------------------------------------------------------
  library(ggplot2)
  library(gdata)
  library(grid)
  
  # ----- Global defines ----------------------------------------------------------
  
}

# ------------------------------------------------------------
# Compute the polynomial equations for plasmid dataset
# filtering the first 8 hours of experimental observations
# ------------------------------------------------------------
bs.plasmid.eqns<- function() {
  d<- bs.filter(bs.plasmids(),expression(Time <= 8))
  d$Time<- d$Time * 60
  l1<- levels(factor(d$Plasmid))
  l2<- levels(factor(d$Density))
  for(p in l1) {
    for(k in l2) {
      d1<- bs.filter(d,bquote(Plasmid == .(p) & Density == .(k)))
      m<- bs.polynom(d1,d1$Time,d1$Rate,3)
      s<- bs.polynom.string(m)
      print(paste(p,"(",k,") ::",s))
    }
  }
}


# ------------------------------------------------------------
# plot rates for plasmid dataset
# 
# ------------------------------------------------------------
bs.plasmid.plots<- function() {
  all.d<- bs.plasmids()
  max.y<- c(0,ceiling(max(d$Rate,na.rm=TRUE)))
  d<- bs.filter(all.d,expression(Time <= 12))
  d$Time<- d$Time * 60
  max.x<- c(0,ceiling(max(d$Time,na.rm=TRUE)))
  scale.x <- seq(0,max.x[2],length.out=100)
  l1<- levels(factor(d$Plasmid))
  l2<- levels(factor(d$Density))
  for(p in l1) {
    for(k in l2) {
      d1<- bs.filter(d,bquote(Plasmid == .(p) & Density == .(k)))
      m<- bs.polynom(d1,d1$Time,d1$Rate,3)
      v<- data.frame(Time = scale.x, Rate = predict(m, data.frame(x=scale.x)))
      v$PLasmid<- rep(p,length(scale.x))
      #print(paste(p,"(",k,")"))
      g<- bs.beginPlot("Conjugation Rate","Time(minutes)","T/(T+R)",max.x,max.y)
      g<- bs.addPoints(g, d1, "Time", "Rate","Plasmid", "blue", paste0(p,"(",k,"%)"))
      g<- bs.addLine(g, v, "Time", "Rate","Plasmid", "red", "Polynom")
      g<- bs.endPlot(g)
      print(g)
    }
  }
  return(g)
}



