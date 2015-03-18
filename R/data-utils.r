#================================================================================
# @data-utils.r, A R Script with utilities for data processing
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
# Rename column name
#
# @param d data frame containing the column to be renamed
# @param old column name
# @param nnew the new column name
# @return the data frame with renamed column
# ------------------------------------------------------------
bs.rename<- function(d,old,nnew) {
  names(d)[names(d) == old]<- nnew
  return(d)
}

#--------------------------------------------------------------------------------
# read
#
# @param f The file name 
# @return The loaded data frame
#--------------------------------------------------------------------------------
bs.read<- function(f) {
  # Reading data with headers
  d<- read.table(f,header=TRUE)
  return(d)
}

#--------------------------------------------------------------------------------
# filter
#
# @param d data frame
# @param e filter expression
# @return Filtered data frame
#--------------------------------------------------------------------------------
bs.filter<- function(d,e) {
  dd<- with(d, subset(d, eval(e), drop=TRUE)) 
  return(dd)
}

#--------------------------------------------------------------------------------
# plasmids
#
# @param 
# @return The experimental plasmid data frame
#--------------------------------------------------------------------------------
bs.plasmids<- function() {
  return(bs.read("raw-data/plasmid-dataset.dat"))
}

