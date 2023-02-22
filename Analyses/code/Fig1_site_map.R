#Joshua G. Smith
#January 2, 2023

rm(list=ls())

require(tidyverse)
require(patchwork)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
gisdir <- file.path(datadir, "gis_data/processed")
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

#read data
sites <- readRDS(file.path(gisdir, "site_locations.Rds"))
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

################################################################################
#format data

# Habitat stats
hab_stats <- sites %>% 
  count(site, program, method)

# Build habitat key
program <- sort(unique(sites$habitat))

#offset points to ease comparison
sites_off <- sites %>%
           mutate(lon_adj = ifelse(
                method == "kelp swath", longitude-0.04, 
                ifelse(method == "size fq", longitude-0.08, longitude))) %>%
          group_by(program, site, method) %>%
          dplyr::summarize(mean_lat = mean(latitude),
                   mean_lon = mean(lon_adj)) 

#make site label
sites_label <- sites_off %>% filter(program=="rcca") %>%
                    mutate(long_adj = ifelse(site == "mackerricher north"|
                                               site=="glass beach"|
                                               site=="noyo north"|
                                               site == "noyo south",
                                             -124.1,
                                             ifelse(site == "caspar north"|
                                                      site == "caspar south"|
                                                      site == "frolic cove"|
                                                      site == "russian gulch"|
                                                      site == "mendocino headlands" |
                                                      site == "portuguese beach"|
                                                      site == "van damme" |
                                                      site == "albion cove",-124,mean_lon)))

################################################################################
#plot data



restoration_site <- tibble(site=c("Albion", "Caspar"),
                 long_dd=c(-123.770694, -123.818565),
                 lat_dd=c(39.228264, 39.361313),
                 #hjust=c(-0.2, 0.5, 0.5),
                 #vjust=c(0.5, -0.3, -0.3)
                 )

site_avg_locations <- sites %>%
                      group_by(program, site)%>%
                      dplyr::summarize(mean_lon = mean(longitude),
                                       mean_lat = mean(latitude))%>%
                      mutate(long_adj = mean_lon - 0.15)

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)
region_lats_long <- c(42, 39.0, 37.18, 34.5, 32)
region_labels <- tibble(region=paste0(c("North", "North\nCentral", "Central", "South"), "\nCoast"),
                        lat_dd=zoo::rollmean(region_lats_long, k = 2))



# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))


#Build inset
g1_inset <-  ggplotGrob(
  ggplot() +
    # Plot regions
    #geom_hline(mapping=aes(yintercept=region_lats), lwd=0.4) +
    # Plot land
    geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
    geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
    # Plot box
    annotate("rect", xmin=-123, xmax=-124, ymin=38.2, ymax=39.5, color="indianred1", fill=NA, lwd=0.8) +
    # Label regions
    #geom_text(data=region_labels, mapping=aes(y=lat_dd, label=region), x= -124.4, hjust=0, size=2) +
    # Labels
    labs(x="", y="") +
    # Crop
    coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
    # Theme
    theme_bw() + base_theme +
    theme( plot.margin = unit(rep(0, 4), "null"),
           panel.margin = unit(rep(0, 4), "null"),
           panel.background = element_rect(fill='transparent'), #transparent panel bg
           # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
           axis.ticks = element_blank(),
           axis.ticks.length = unit(0, "null"),
           axis.ticks.margin = unit(0, "null"),
           axis.text = element_blank(),
           axis.title=element_blank())
)
g1_inset





g1 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot regions
  #geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, fill="indianred1", color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  #plot sites
  geom_point(data=sites_off # %>% filter(program=="rcca")
             , aes(x=mean_lon, y=mean_lat, color=method, shape=program), size = 2) +
  # Plot restoration sites
  #geom_point(data=restoration_site, mapping=aes(x=long_dd+0.025, y=lat_dd), size=2.2) +
  geom_text(data=restoration_site, mapping=aes(x=long_dd, y=lat_dd, label=site, hjust=-0.5, vjust=0
                                               ),
            size=3) +
  #add county line
  geom_hline(mapping=aes(yintercept=38.768802), linetype = "dashed")+
  annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  #ggrepel::geom_text_repel(sites_off %>% filter(method == "size fq"),
   #                        mapping=aes(x=mean_lon, y=mean_lat, label=site), 
    #                       inherit.aes = F, size=2, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
   #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_color_manual(name="Survey type", values=c( "purple", "limegreen", "dodgerblue")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-124.2, -123), ylim = c(38.51, 39.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.15, 0.15), 
        legend.key.size = unit(0.4, "cm"))+
  # Add inset
  annotation_custom(grob = g1_inset, 
                    xmin = -123.4, 
                    xmax = -122.8,
                    ymin = 39.2) 

g1




# Export figure
ggsave(g1, filename=file.path(figdir, "Fig1_site_locations.png"), 
       width=5.8, height=6.5, units="in", dpi=600)






################################################################################
#plot by 'region' 







