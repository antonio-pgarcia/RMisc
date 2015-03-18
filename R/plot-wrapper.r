#================================================================================
# @plot-wrapper.r, A simple wrapper for plot functions
#
# This script was developed as part of PhD thesis
# 
# @author Antonio Prestes Garcia
# @date Feb/2015
#
# Changelog:
# 
#================================================================================

#--------------------------------------------------------------------------------
# saveAsPDF
# 
# @param f The file name 
#--------------------------------------------------------------------------------
bs.saveAsPDF<- function(f) {
  pdf(file=f)
}

#--------------------------------------------------------------------------------
# saveAsEPS
# 
# @param f The file name 
#--------------------------------------------------------------------------------
bs.saveAsEPS<- function(f) {
  setEPS()
  postscript(f)
}

#--------------------------------------------------------------------------------
# saveAsEPS
# 
# @param F The file name 
#--------------------------------------------------------------------------------
bs.saveAsTIFF<- function(f) {
  tiff(f, compression = "lzw")
}

#--------------------------------------------------------------------------------
# closeGraphic
# 
#--------------------------------------------------------------------------------
bs.closeGraphic<- function() {
  suppress<- dev.off()
}

#--------------------------------------------------------------------------------
# beginPlot
# 
# @param title
# @param label.x
# @param label.y
# @return g A ggplot with no layers
#--------------------------------------------------------------------------------
bs.beginPlot<- function(title,label.x,label.y,lim.x, lim.y) {
  g<- ggplot()
  g<- g + theme_bw()
  g<- g + labs(title=title)
  g<- g + xlab(label.x)
  g<- g + ylab(label.y)
  g<- g + xlim(lim.x)
  g<- g + ylim(lim.y)
  g<- g + theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"), legend.position=bs.legendPos(), panel.grid.minor = element_blank(), legend.title = element_blank(), legend.background = element_blank())
  g$index<- 0
  return(g)
}

#--------------------------------------------------------------------------------
# endPlot
#
# @param g The graphic object to be closed 
#--------------------------------------------------------------------------------
bs.endPlot<- function(g) {
  g<- g + scale_colour_manual(values=g$mycolors)
  g<- g + scale_fill_manual(values=g$mycolors)
  return(g)
}

#--------------------------------------------------------------------------------
# legendPos
#
#--------------------------------------------------------------------------------
bs.legendPos<- function() {
  return(c(0.15,0.90))
}

#--------------------------------------------------------------------------------
# addPoints
# 
#--------------------------------------------------------------------------------
bs.addPoints<- function(g,d,x,y,l,color,name) {
  d<- local(d)
  d[l]<- factor(as.character(name))
  g<- g + geom_point(data=d,aes_string(x=x, y=y,color=l,fill=l)) 
  g$mycolors<- c(g$mycolors,c(color))  
  return(g)
}

#--------------------------------------------------------------------------------
# addLine
# 
#--------------------------------------------------------------------------------
bs.addLine<- function(g,d,x,y,l,color, name) {
  d<- local(d)
  d[l]<- factor(as.character(name))
  g<- g + geom_line(data=d,aes_string(x=x, y=y,color=l,fill=l)) 
  g$mycolors<- c(g$mycolors,c(color))  
  return(g)
}
