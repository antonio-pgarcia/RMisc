# ------------------------------------------------------------
# BactoSim library entry point
#
# ------------------------------------------------------------

# ------------------------------------------------------------
# .onLoad shows the library disclaimer
#
# ------------------------------------------------------------
.onLoad<- function(libname, pkgname) {
  packageStartupMessage("BactoSim(haldane) data analysis tools v1.0\n")
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