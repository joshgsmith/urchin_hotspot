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

#read mlpa data
mlpa_swath <- read.csv(file.path(datadir, "monitoring_processed/mlpa_swath.csv"))%>%
  rename("Lat" = latitude,
         "Long"=longitude) %>%
  mutate(site = paste(affiliated_mpa, mpa_designation),
         region = ifelse(Lat >= 39.5,"Humboldt",
                         ifelse(Lat < 39.5 &
                                  Lat > 39.2, "Mendocino",
                                ifelse(Lat < 39 &
                                         Lat > 38.8, "Point Arena",
                                       ifelse(Lat < 38.8 &
                                                Lat > 38.5, "Sonoma",Lat))))) %>%
  select(year, site, region, Lat, Long, transect, species, count, size)


rcca_urchin_sizefq <- read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_sizefq.csv"))%>%
  mutate(site_new = sub("(.)", "\\U\\1", site_new, perl=TRUE))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


################################################################################
#process mlpa urchin size data

#calculate annual mean site density
site_density <- mlpa_swath %>%
  #select years of interest
  filter(
    species == "Strongylocentrotus purpuratus"|
      species == "Mesocentrotus franciscanus") %>%
  mutate(species = recode(species, "Strongylocentrotus purpuratus" = "Purple urchin",
                          "Mesocentrotus franciscanus" = "Red urchin")) 
  
DatU <- vcdExtra::expand.dft(site_density, freq="count")


#plot
DatU_site <- DatU %>%
  filter(year >= 2016)

g1 <- ggplot(DatU_site %>% filter(species=="Purple urchin"), aes(x=size)) +
      geom_histogram(binwidth = 1)+
      facet_wrap(~site)



################################################################################
#process rcca

#calculate annual mean site density
DatU <- vcdExtra::expand.dft(rcca_urchin_sizefq, freq="count") %>%
  mutate(
         region = ifelse(latitude >= 39.5,"Humboldt",
                         ifelse(latitude < 39.5 &
                                  latitude > 39.2, "Mendocino",
                                ifelse(latitude < 39 &
                                         latitude > 38.8, "Point Arena",
                                       ifelse(latitude < 38.8 &
                                                latitude > 38.5, "Sonoma",latitude)))))


#plot
DatU_site <- DatU %>%
  filter(year >= 2016)

ymax <-  rcca_urchin_sizefq %>%
  filter(year >= 2016,
         species=="purple urchin") %>%
  group_by(site_new, size)%>%
  summarize(total = sum(count)) %>%
  group_by(site_new)%>%
  summarize(y_max = max(total))

mean <-  rcca_urchin_sizefq %>%
  filter(year >= 2016,
         species=="purple urchin") %>%
  group_by(site_new)%>%
  summarize(u_size = mean(size))

grand_mean <-  rcca_urchin_sizefq %>%
  filter(year >= 2016,
         species=="purple urchin") %>%
  summarize(u_size = mean(size))

plot_dat1 <- left_join(DatU_site, ymax)
plot_dat <- left_join(plot_dat1, mean)



my_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=11),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=4),
                     plot.title = element_text(size=6),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)),
                    #facets
                   strip.text.x = element_text(size = 7))
                   





g1 <- ggplot(plot_dat %>% filter(species=="purple urchin"), aes(x=size)) +
  geom_histogram(binwidth = 1)+
  #ggstance::geom_boxploth(aes(y = y_max), #width=100, 
   #                       size=2,
    #                      color = "blue", alpha = .5,
     #                     lwd = .5,
      #                    width=4) +
  facet_wrap(~site_new, scales="free_y")+
  geom_vline(aes(xintercept = u_size),col='purple',size=1, dat=mean)+
  geom_vline(aes(xintercept = u_size),col='black',size=1, dat=grand_mean)+
  labs(y="Counts",
       x="Size (cm)")+
  my_theme

ggsave(g1, filename=file.path(figdir, "Rcca_purp_hist.png"), 
       width=8, height=7, units="in", dpi=600)





