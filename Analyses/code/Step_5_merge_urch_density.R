#Joshua G. Smith
#January 12, 2023

rm(list=ls())

require(tidyverse)
require(patchwork)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
gisdir <- file.path(datadir, "gis_data/processed")
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

#read spatial data
#sites <- readRDS(file.path(gisdir, "site_locations.Rds"))
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

#read survey data
mlpa_dat <- read.csv(file.path(datadir, "monitoring_processed/mlpa_urchin_demographics.csv")) %>%
                  rename("site_new" = site) %>%
                  select(survey, region, year, site_new, species, Lat, Long, everything())
rcca_dat <- read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_demographics.csv"))%>%
  select(survey, region, year, site_new, species, Lat, Long, everything())

################################################################################
#merge datasets

View(mlpa_dat)
View(rcca_dat)

urch_join <- rbind(rcca_dat, mlpa_dat)%>%
              mutate(species = tolower(species))


write.csv(urch_join, file.path(datadir, "/monitoring_processed/mlpa_rcca_urchin_density_combined.csv"),row.names = FALSE)






