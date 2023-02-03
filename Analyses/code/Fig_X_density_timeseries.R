#Joshua G. Smith
#February 2, 2023

rm(list=ls())

require(tidyverse)
require(patchwork)
require(sf)
require(maptools)
require(mapdata)
require(Metrics)
require(terra)
require(tidyterra)
require(gstat)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
gisdir <- file.path(datadir, "gis_data/processed")
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

#read combined density data
urch_den <- read.csv(file.path(datadir, "monitoring_processed/mlpa_rcca_urchin_density_combined.csv"))%>%
              mutate(site_new = sub("(.)", "\\U\\1", site_new, perl=TRUE)) %>%
                     mutate(region = ifelse(site_new == "Saunders reef smca ref","Point Arena",region))

################################################################################
#Plot density

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=8, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)))


g1 <- urch_den %>%
      filter(survey == "rcca")%>%
      ggplot(aes(x=year, y=m2_den, color=species))+
      geom_point()+
      geom_line()+
      scale_color_manual(values=c("purple","red"))+
      scale_y_continuous(limits=c(0,40))+
      scale_x_continuous(breaks = c(2008, 2012, 2016, 2020))+
      facet_wrap(region~site_new)+
      my_theme+
      ggtitle("RCCA surveys")


g2 <- urch_den %>%
  filter(survey == "mlpa")%>%
  ggplot(aes(x=year, y=m2_den, color=species))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("purple","red"))+
  scale_y_continuous(limits=c(0,40))+
  facet_wrap(region~site_new)+
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020))+
  my_theme+
  ggtitle("MLPA surveys")

g <- gridExtra::grid.arrange(g1, g2, nrow=1)



################################################################################
#drop sites with less than 4 years in time series

urch_drop_dat <- urch_den %>%
                  filter(species == "purple urchin")%>%
                  group_by(site_new, species)%>%
                  summarize(n_year = n()) %>%
                  select(site_new, n_year)

urch_dat_new <- left_join(urch_den, urch_drop_dat)%>%
                filter(n_year >= 4)


my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=8, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   #facet text size
                   strip.text.x = element_text(size = 6)
                   )



g1 <- urch_dat_new %>%
  filter(survey == "rcca")%>%
  ggplot(aes(x=year, y=m2_den, color=species))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("purple","red"))+
  scale_y_continuous(limits=c(0,40))+
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020))+
  facet_wrap(region~site_new)+
  my_theme+
  labs(y="Density (no. per m²)")+
  ggtitle("RCCA surveys")


# Export figure
ggsave(g1, filename=file.path(figdir, "Rcca_sitedensity_timerseries.png"), 
       width=10, height=7, units="in", dpi=600)



g2 <- urch_dat_new %>%
  filter(survey == "mlpa")%>%
  ggplot(aes(x=year, y=m2_den, color=species))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("purple","red"))+
  scale_y_continuous(limits=c(0,40))+
  facet_wrap(region~site_new)+
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020))+
  my_theme+
  labs(y="Density (no. per m²)")+
  ggtitle("MLPA surveys")

ggsave(g2, filename=file.path(figdir, "Mlpa_sitedensity_timerseries.png"), 
       width=10, height=7, units="in", dpi=600)
















