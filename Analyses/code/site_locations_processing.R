#Joshua G. Smith
#January 2, 2022

rm(list=ls())

require(dplyr)

datadir <- "/Users/Joshua/Box Sync/Data"
dataout <- "/Users/Joshua/Box Sync/Data/gis_data/processed"
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

################################################################################
#read data

mlpa_swath <- read.csv(file.path(datadir, "monitoring_processed/mlpa_swath.csv"))
rcca_invert_swath <- read.csv(file.path(datadir, "monitoring_processed/rcca_invert_swath.csv"))
rcca_kelp_swath <- read.csv(file.path(datadir, "monitoring_processed/rcca_kelp_swath.csv"))
rcca_urchin_sizefq <- read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_sizefq.csv"))

################################################################################
#select sites

mlpa_swath1 <- mlpa_swath %>%
                mutate(program = "mlpa",
                       method = "invert swath",
                       restoration_site = "no",
                       survey_type = "annual")%>%
                dplyr::distinct(program, method, survey_type,
                              year, site = affiliated_mpa,
                             restoration_site, latitude, longitude) 



site_locations <- rbind(mlpa_swath1, rcca_invert_swath1, rcca_kelp_swath1,
                       rcca_urchin_sizefq1)


saveRDS(site_locations, file.path(dataout, "site_locations.Rds"))


################################################################################
#rcca unmatched site names


rcca_invert_swath2 <- rcca_invert_swath %>%
  mutate(program = "rcca",
         method = "invert swath")%>%
  dplyr::distinct(program, method, survey_type,
                  site_orig,
                  site_new) 

rcca_kelp_swath2 <- rcca_kelp_swath %>%
  mutate(program = "rcca",
         method = "kelp swath")%>%
  dplyr::distinct(program, method, survey_type,
                  site_orig,
                  site_new) 

rcca_urchin_sizefq2 <- rcca_urchin_sizefq %>%
  mutate(program = "rcca",
         method = "size fq")%>%
  dplyr::distinct(program, method, survey_type,
                  site_orig,
                  site_new) 


rcca_sites <- rbind(rcca_invert_swath2,
                    rcca_kelp_swath2,
                    rcca_urchin_sizefq2)

#examine mismatched sites
rcca_unmatched <- rcca_sites %>% filter(site_orig != site_new)





