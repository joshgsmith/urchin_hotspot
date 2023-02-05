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

#read monitoring dat
rcca_urchin_sizefq <- read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_sizefq.csv"))%>%
  mutate(site_new = sub("(.)", "\\U\\1", site_new, perl=TRUE))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


################################################################################
##process rcca

# step 1 - determine size frequency
DatU <- vcdExtra::expand.dft(rcca_urchin_sizefq, freq="count") %>%
  mutate(
    region = ifelse(latitude >= 39.5,"Humboldt",
                    ifelse(latitude < 39.5 &
                             latitude > 39.2, "Mendocino",
                           ifelse(latitude < 39 &
                                    latitude > 38.8, "Point Arena",
                                  ifelse(latitude < 38.8 &
                                           latitude > 38.5, "Sonoma",latitude)))))

# step 2 - calculate size level means for 2016-2021

site_mean <-  rcca_urchin_sizefq %>%
  filter(year >= 2016) %>%
  group_by(site_new, species)%>%
  summarize(site_mean = mean(size))

grand_mean <-  rcca_urchin_sizefq %>%
  filter(year >= 2016) %>%
  group_by(species)%>%
  summarize(pop_mean = mean(size))

#step 3 - join means

mean_dat <- left_join(site_mean, grand_mean, by=c('species'))%>%
              mutate(u_diff = site_mean-pop_mean)


#step 4 - join lat long and region

datU1 <- DatU %>% select(Lat = latitude, Long = longitude, site_new, 
                         region) %>%
          group_by(region, site_new)%>%
          summarize(Lat = mean(Lat),
                    Long = mean(Long))

#step 5 - join spatial dat 

size_dat <- left_join(mean_dat, datU1, by="site_new")


saveRDS(size_dat, file.path(datadir, "/monitoring_processed/rcca_mean_density_2016-22.Rda"))




