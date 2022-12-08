#Joshua G. Smith
#December 7, 2022

rm(list=ls())

require(dplyr)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data/RC_EM_2010to2019/raw"
dataout <- "/Users/Joshua/Box Sync/Data/RC_EM_2010to2019/processed"
figdir 


################################################################################
#import data

mlpa_swath <- read.csv(file.path(datadir, "MLPA_kelpforest_swath.4.csv"))
rcca_ab <- read.csv(file.path(datadir, "RCCA_abalone_size_data.csv"))
rcca_invert_swath <- read.csv(file.path(datadir, "RCCA_invertebrate_swath_data.csv"))
rcca_urchin_size <- read.csv(file.path(datadir, "RCCA_urchin_size_data.csv"))








  
  