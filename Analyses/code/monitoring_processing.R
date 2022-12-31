#Joshua G. Smith
#December 7, 2022

rm(list=ls())

require(dplyr)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
dataout <- "/Users/Joshua/Box Sync/Data/RC_EM_2010to2019/processed"
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"


################################################################################
#import data

#------mlpa data
mlpa_swath <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/MLPA_kelpforest_swath.4.csv"))%>%
                janitor::clean_names()
kelp_taxon <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/MLPA_kelpforest_taxon_table.4.csv"))%>%
  janitor::clean_names()
kelp_site_table <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/MLPA_kelpforest_site_table.4.csv"))%>%
  janitor::clean_names()

#------rcca annual surveys
rcca_ab <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_abalone_size_data.csv"))%>%
  janitor::clean_names()
rcca_invert_swath <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_invertebrate_swath_data.csv"))%>%
  janitor::clean_names()
rcca_urchin_size <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_urchin_size_data.csv"))%>%
  janitor::clean_names()
rcca_inverts_2020 <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_Inverts_2020-21.csv"))%>%
  janitor::clean_names()
rcca_urchin_size_data_2020 <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_urchin_size_data_2020-21.csv"))%>%
  janitor::clean_names()
rcca_kelp_swath <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_algae_swath_data.csv"))%>%
  janitor::clean_names()

#-----rcca seasonal surveys
#inverts
rcca_seasonal_invert_fall22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Inverts/Inverts_TNC_fall22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "fall")
rcca_seasonal_invert_spring22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Inverts/Inverts_TNC_spring22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "spring")
rcca_seasonal_invert_summer22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Inverts/Inverts_TNC_summer22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "summer")
rcca_seasonal_invert_winter22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Inverts/Inverts_TNC_winter22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "winter")

#kelp
rcca_seasonal_kelp_fall22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Kelp/Kelp_TNC_fall22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "fall")
rcca_seasonal_kelp_spring22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Kelp/Kelp_TNC_spring22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "spring")
rcca_seasonal_kelp_summer22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Kelp/Kelp_TNC_summer22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "summer")
rcca_seasonal_kelp_winter22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Kelp/Kelp_TNC_winter22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "winter")

#urchin size fq
rcca_seasonal_urchinfq_fall22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Urchin Size-Frequency/Urchin_TNC_fall22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "fall") %>%
  data.frame()%>%
  mutate(date_start = as.factor(date_start),
         date_end = as.factor(date_end),
         start_at = as.factor(start_time)) %>%
  mutate_if(sapply(., is.character), as.factor)

rcca_seasonal_urchinfq_spring22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Urchin Size-Frequency/Urchin_TNC_spring22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "spring")%>%
  data.frame()%>%
  mutate(date_start = as.factor(date_start),
         date_end = as.factor(date_end),
         start_at = as.factor(start_time)) %>%
  mutate_if(sapply(., is.character), as.factor)

rcca_seasonal_urchinfq_summer22 <- read.csv(file.path(datadir, "RC_EM_2022/raw/Urchin Size-Frequency/Urchin_TNC_summer22.csv"))%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "summer")%>%
  data.frame()%>%
  mutate(date_start = as.factor(date_start),
         date_end = as.factor(date_end),
         start_at = as.factor(start_time)) %>%
  mutate_if(sapply(., is.character), as.factor)%>%
  mutate(date_end = as.factor(format(as.Date(date_end, format="%m/%d/%Y"))))

rcca_seasonal_urchinfq_winter22 <- readxl::read_xlsx(file.path(datadir, "RC_EM_2022/raw/Urchin Size-Frequency/Urchin_TNC_winter22.xlsx"), sheet=1)%>%
  janitor::clean_names()%>%
  mutate(survey_type = "quarterly",
         season = "winter")%>%
  data.frame()%>%
  mutate(date_start = as.factor(date_start),
         date_end = as.factor(date_end),
         start_at = as.factor(start_time)) %>%
  mutate_if(sapply(., is.character), as.factor)


#-----mpa traits
mpa_traits <- read.csv(file.path(datadir, "mpa_traits/mpa_attributes_clean.csv"))%>%
  janitor::clean_names()




################################################################################
#process mlpa kelp forest

#process sites
kelp_sites <- kelp_site_table %>%
  janitor::clean_names() %>%
  dplyr::select(site, ca_mpa_name_short, mpa_class=site_designation, mpa_designation=site_status,
                latitude, longitude)%>%
  distinct() #remove duplicates

#select kelp dat
kelp_swath <- mlpa_swath %>%
  dplyr::select(year, site, zone, transect, classcode, count, size)

#join species names by class code 
kelp_taxon1 <- kelp_taxon %>%
  janitor::clean_names() %>%
  dplyr::select(classcode, species_definition) %>%
  distinct() #remove duplicates

kelp_swath_counts <-left_join(kelp_swath, kelp_taxon, by="classcode")

kelp_swath_counts1 <- kelp_swath_counts %>%
  dplyr::select(year, site,  zone, transect, species=species_definition, count, size)


#join with affiliated mpa
kelp_swath_counts2 <- left_join(kelp_swath_counts1, kelp_sites, by="site")

kelp_swath_counts3 <- kelp_swath_counts2 %>%
  ungroup() %>%
  dplyr::select(year, affiliated_mpa=ca_mpa_name_short,  latitude, longitude,mpa_class, mpa_designation, zone, transect, species, count, size) %>%
  mutate(mpa_designation = ifelse(mpa_designation=="reference","ref",mpa_class))

#add regions

regions <- mpa_traits %>%
  dplyr::select(name, region3=bioregion, region4 = four_region_north_ci)

kelp_swath_counts3$affiliated_mpa <- tolower(kelp_swath_counts3$affiliated_mpa)

kelp_swath_counts4 <- left_join(kelp_swath_counts3, regions, by=c("affiliated_mpa"="name"))%>%
                      dplyr::select(year, region3, region4, everything())%>%
                      filter(region3 == "north")

View(kelp_swath_counts4)


#export
#write.csv(kelp_swath_counts4, file.path(dataout, "kelp_forest_swath.csv"), row.names = FALSE)

################################################################################
#process Reef Check

#--------------invert swath
View(rcca_invert_swath)

#calculate transect density
rcca_swath <- rcca_invert_swath %>%
              mutate(transect_den = round(ifelse(distance < 30,
                                           amount*(30/distance),
                                           amount),2),
                     region = ifelse(latitude >= 37.7749, "North",""))%>%
              filter(region == "North")%>%
              dplyr::select(year, month, day, site,latitude, longitude,
                            transect, species=classcode,
                            counts = transect_den, depth_ft, temp10m)%>%
                mutate(species = tolower(species))

rcca_swath_2020 <- rcca_inverts_2020 %>%
                    dplyr::select(date_end, site=site_name, latitude, longitude,
                                  transect, species, amount, distance)%>%
                    mutate(year = format(as.Date(date_end, format="%m/%d/%Y"),"%Y"),
                           month = format(as.Date(date_end, format="%m/%d/%Y"),"%m"),
                           day = format(as.Date(date_end, format="%m/%d/%Y"),"%d"),
                           transect_den = round(ifelse(distance < 30,
                                                           amount*(30/distance),
                                                           amount),2),
                           depth_ft = NA,
                           temp10m = NA)%>%
                   dplyr::select(year, month, day, site,latitude, longitude,
                     transect, species,
                      counts = transect_den, depth_ft, temp10m) %>%
                  mutate(species = tolower(species))

rcca_swath2 <- rbind(rcca_swath, rcca_swath_2020)

unique(rcca_swath2$species)

#write.csv(rcca_swath2, file.path(dataout, "rcca_invert_swath.csv"), row.names = FALSE)

#--------------kelp swath
rcca_kelp_swath1 <- rcca_kelp_swath %>%
  mutate(counts_den = round(ifelse(distance < 30,
                                     amount*(30/distance),
                                     amount),2),
         stipe_den = round(ifelse(distance < 30,
                                     stipes*(30/distance),
                                     stipes),2),
         region = ifelse(latitude >= 37.7749, "North",""),
         survey_type = "annual")%>%
  filter(region == "North")%>%
  dplyr::select(survey_type, year, month, day, site,latitude, longitude,
                transect,depth_ft, temp10m, species=classcode,
                counts_den, stipe_den)%>%
  mutate(species = factor(tolower(species)),
         site = factor(tolower(site)),
         counts_den = ifelse(is.na(counts_den),0,counts_den),
         stipe_den = ifelse(is.na(stipe_den),0,stipe_den))
         

#--------------Urchin size frequency

View(rcca_urchin_size)
View(rcca_urchin_size_data_2020)

rcca_urchin_size1 <- rcca_urchin_size %>%
                      dplyr::select(year, month, day, site, latitude=lat,
                                    longitude=lon, species=classcode,
                                    size, count=amount, depth_ft, temp10m)%>%
                      mutate(region = ifelse(latitude >= 37.7749, "North",""))%>%
                      filter(region == "North")%>%
                      select(!region)
  

rcca_urchin_size2 <- rcca_urchin_size_data_2020 %>%
                      mutate(year = format(as.Date(date_end, format="%m/%d/%Y"),"%Y"),
                             month = format(as.Date(date_end, format="%m/%d/%Y"),"%m"),
                             day = format(as.Date(date_end, format="%m/%d/%Y"),"%d"),
                             depth_ft = NA,
                             temp10m = NA)%>%
                      dplyr::select(year, month, day, site=site_name, latitude, longitude,
                                    species, size=test_diameter, count=amount, depth_ft,
                                    temp10m)

rcca_urchin_size_fq <- rbind(rcca_urchin_size1, rcca_urchin_size2) %>%
                        mutate(survey_type = "annual",
                               season = "summer")
  
#--------------kelp swath



#write.csv(rcca_urchin_size_fq, file.path(dataout, "rcca_urchin_size_fq.csv"), row.names = FALSE)


################################################################################
#process Reef Check 2022 quarterly surveys 

#--------------invert swath processing
#merge datasets
rcca_invert22 <- rbind(rcca_seasonal_invert_fall22,rcca_seasonal_invert_spring22,
                       rcca_seasonal_invert_winter22, rcca_seasonal_invert_summer22)

#write.csv(rcca_invert22, file.path(datadir, "RC_EM_2022/processed/rcca_invert_swath22.csv"), row.names = FALSE)

#--------------kelp processing

rcca_kelp22 <- rbind(rcca_seasonal_kelp_fall22,rcca_seasonal_kelp_spring22,
                       rcca_seasonal_kelp_winter22, rcca_seasonal_kelp_summer22)

#write.csv(rcca_kelp22, file.path(datadir, "RC_EM_2022/processed/rcca_kelp22.csv"), row.names = FALSE)

#--------------urchin size frequency

rcca_urchinfq22 <- rbind(rcca_seasonal_urchinfq_fall22, rcca_seasonal_urchinfq_spring22,
                         rcca_seasonal_urchinfq_summer22, rcca_seasonal_urchinfq_winter22)%>%
                   mutate(date_end = as.character(date_end),
                          year = format(as.Date(date_end, format="%Y-%m-%d"),"%Y"),
                                 month = format(as.Date(date_end, format="%Y-%m-%d"),"%m"),
                                 day = format(as.Date(date_end, format="%Y-%m-%d"),"%d"),
                          depth_ft = NA,
                          temp10m = NA) %>%
                  dplyr::select(year, month, day, site = site_name, latitude, longitude,
                                species, size = test_diameter, count = amount, depth_ft,
                                temp10m, survey_type, season)

#write.csv(rcca_urchinfq22, file.path(datadir, "RC_EM_2022/processed/rcca_urchinsizefq22.csv"), row.names = FALSE)



################################################################################
#join RCCA quarterly and annual surveys

#--------------invert swath processing
annual_swath1 <- rcca_swath2 %>%
                  mutate(survey_type = "annual")%>%
                  dplyr::select(survey_type, year, month, day, site, latitude, longitude,
                                transect, depth_ft, temp10m,  species, counts)%>%
                  mutate(site = factor(tolower(site)),
                         species = factor(tolower(species)),
                         transect = factor(transect))
qtr_swath1 <- rcca_invert22 %>%
            mutate(date_end = as.character(date_end),
                year = format(as.Date(date_end, format="%Y-%m-%d"),"%Y"),
                month = format(as.Date(date_end, format="%Y-%m-%d"),"%m"),
                day = format(as.Date(date_end, format="%Y-%m-%d"),"%d"),
                depth_ft = NA,
                temp10m = NA)%>%
            dplyr::select(survey_type, year, month, day, site = site_name, latitude, longitude,
                transect, depth_ft, temp10m,  species, counts = amount)%>%
            mutate(site = factor(tolower(site)),
                species = factor(tolower(species)),
                transect = factor(transect))

rcca_swath_join <- rbind(annual_swath1, qtr_swath1) %>%
                    mutate(survey_type = factor(survey_type),
                           site_orig = factor(site),
                           year = factor(year),
                           month = factor(stringr::str_remove(month, "^0+")),
                           day = factor(stringr::str_remove(day, "^0+")),
                           restoration_site = factor(ifelse(site == "caspar south restoration" |
                                                             site == "albion restoration"|
                                                             site == "ocean cove kelper","yes","no")),
                           site_new = site_orig,
                           site_new = recode(site, "albion restoration" = "albion cove",
                                             "caspar south restoration" = "caspar south",
                                             "ft ross" = "fort ross",
                                             "ocean cove kelper" = "ocean cove",
                                             "point arena mpa (m2)" = "point arena mpa",
                                             "stillwater cove sonoma" = "stillwater sonoma"))%>%
  dplyr::select(!(site))%>%
  dplyr::select(survey_type, year, month, day, restoration_site,site_orig, site_new, everything())

                           


#write.csv(rcca_swath_join, file.path(datadir, "monitoring_processed/rcca_invert_swath.csv"), row.names = FALSE)


#--------------urchin size frequency


annual_urch1 <- rcca_urchin_size_fq %>%
                mutate(site = factor(tolower(site)),
                       species = factor(species),
                       survey_type = factor(survey_type),
                       season = factor(season))%>%
                dplyr::select(survey_type, season, year, month, day, 
                               depth_ft, temp10m,
                              everything())

qtr_urch1 <- rcca_urchinfq22 %>%
             mutate(site = factor(tolower(site)),
                    species = factor(species),
                    survey_type = factor(survey_type),
                    season = factor(season))%>%
                dplyr::select(survey_type, season, year, month, day, 
                depth_ft, temp10m,
                everything())

rcca_urchin_sizefq <- rbind(annual_urch1, qtr_urch1) %>%
                        mutate(site_orig = factor(tolower(site)),
                               species = factor(tolower(species)),
                               year = factor(year),
                               month = factor(stringr::str_remove(month, "^0+")),
                               day = factor(stringr::str_remove(day, "^0+")),
                               restoration_site = factor(ifelse(site == "caspar south restoration" |
                                                                  site == "albion restoration"|
                                                                  site == "ocean cove kelper","yes","no")),
                               site_new = site_orig,
                               site_new = recode(site, "albion restoration" = "albion cove",
                                                 "caspar south restoration" = "caspar south",
                                                 "ft ross" = "fort ross",
                                                 "ocean cove kelper" = "ocean cove",
                                                 "point arena mpa (m2)" = "point arena mpa",
                                                 "stillwater cove sonoma" = "stillwater sonoma"))%>%
  dplyr::select(!(site))%>%
  dplyr::select(survey_type, season, year, month, day, restoration_site,site_orig, site_new, everything())


#write.csv(rcca_urchin_sizefq, file.path(datadir, "monitoring_processed/rcca_urchin_sizefq.csv"), row.names = FALSE)


#--------------kelp swath

annual_kelp <- rcca_kelp_swath1 

qtr_kelp <- rcca_kelp22 %>%
            mutate(date_end = as.character(date_end),
                   year = format(as.Date(date_end, format="%Y-%m-%d"),"%Y"),
                   month = format(as.Date(date_end, format="%Y-%m-%d"),"%m"),
                   day = format(as.Date(date_end, format="%Y-%m-%d"),"%d"),
                   depth_ft = NA,
                   temp10m = NA)%>%
            dplyr::select(survey_type, year, month, day, 
                          site = site_name, latitude, longitude, transect,
                          depth_ft, temp10m, species, counts_den = amount,
                          stipe_den = stipes
                          )%>%
            filter(!(is.na(species)))%>%
            #note: distance is assumed to be 30m
            mutate(survey_type = factor(survey_type),
                   site = factor(tolower(site)),
                   transect = factor(transect),
                   species = factor(species),
                   counts_den = ifelse(is.na(counts_den),0,counts_den),
                   stipe_den = ifelse(is.na(stipe_den),0,stipe_den)
                   )

rcca_kelp_swath <- rbind(annual_kelp, qtr_kelp) %>%
  mutate(site_orig = factor(tolower(site)),
         species = factor(tolower(species)),
         year = factor(year),
         month = factor(stringr::str_remove(month, "^0+")),
         day = factor(stringr::str_remove(day, "^0+")),
         restoration_site = factor(ifelse(site == "caspar south restoration" |
                                            site == "albion restoration"|
                                            site == "ocean cove kelper","yes","no")),
         site_new = site_orig,
         site_new = recode(site, "albion restoration" = "albion cove",
                       "caspar south restoration" = "caspar south",
                       "ft ross" = "fort ross",
                       "ocean cove kelper" = "ocean cove",
                       "point arena mpa (m2)" = "point arena mpa",
                       "stillwater cove sonoma" = "stillwater sonoma"))%>%
  dplyr::select(!(site))%>%
  dplyr::select(survey_type, year, month, day, restoration_site,site_orig, site_new, everything())


#write.csv(rcca_kelp_swath, file.path(datadir, "monitoring_processed/rcca_kelp_swath.csv"), row.names = FALSE)










  