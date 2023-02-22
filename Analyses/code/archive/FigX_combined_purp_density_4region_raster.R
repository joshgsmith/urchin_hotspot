#Joshua G. Smith
#January 23, 2023

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
require(ggsn)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
gisdir <- file.path(datadir, "gis_data/processed")
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

#read spatial data
sites <- readRDS(file.path(gisdir, "site_locations.Rds"))
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds")) %>%
  mutate(mpa_simple = ifelse(type == "SMCA","SMCA","SMR"))

coastn83 <- st_read(file.path(datadir, "/gis_data/raw/coastn83/coastn83.shp"))
CA_state <- st_read(file.path(datadir, "/gis_data/raw/CA_state/ca_boundary_wgs84.shp"))

#read density data
urch_den <- read.csv(file.path(datadir, "monitoring_processed/mlpa_rcca_urchin_density_combined.csv"))

# Get land
states <- map_data("state") %>%
  filter(region %in% c("california","oregon"))%>%
  #WGS84
  sf::st_as_sf(coords = c("long","lat"), crs=4326) %>%
  group_by(region) %>% 
  summarize(do_union=FALSE) %>%
 st_cast("POLYGON") %>% 
  ungroup()

usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
counties <- map_data("county") %>%
  filter(region == "california")%>%
  #WGS84
  sf::st_as_sf(coords = c("long","lat"),  crs = 4236) %>%
  group_by(region, subregion) %>% 
  summarize(do_union=FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

################################################################################
#aggregate data
density_mean <- urch_den %>%
  #calculate mean after MHW
  filter(year > 2016)%>%
  select(survey, region, site_new, species, Lat, Long, m2_den)%>%
  mutate(Long = ifelse(region == "Humbodlt" | region == "Mendocino" | region == "Sonoma", Long-0.005, Long))%>%
  group_by(survey, region, site_new, species) %>%
  summarise_at(vars("Lat","Long","m2_den"), mean)

# create site number

urch_den1 <- urch_den %>%
  filter(species == "purple urchin")%>%
  arrange(desc(Lat))%>%
  mutate(site_number = seq(1:nrow(.))) %>%
  select(site_new, site_number)

urch_den_spatial <- left_join(density_mean, urch_den1, by="site_new") %>%
  mutate(site_name = paste(site_number, site_new))


density_dat_all <- st_as_sf(urch_den_spatial, coords = c("Long","Lat"),
                            crs=4326) 
density_dat_pur <- st_as_sf(urch_den_spatial, coords = c("Long","Lat"),
                            crs=4326) %>%
  filter(species == "purple urchin")

density_dat_red <- st_as_sf(urch_den_spatial, coords = c("Long","Lat"),
                            crs=4326) %>%
  filter(species == "red urchin")

####transoform to Teale Albers
states_T <- st_transform(states, crs = 3310)
counties_T <- st_transform(counties, crs=3310)
density_pur_T <- st_transform(density_dat_pur, crs=3310)
density_red_T <- st_transform(density_dat_red, crs=3310)
coastn_T <- st_transform(coastn83, crs=3310)
CA_state_T <- st_transform(CA_state, crs=3310)

#create clipping buffer
CA_buff_1000 <- CA_state_T %>%
  st_buffer(5000)


#check features
ggplot() +
  geom_sf(data = CA_buff_1000)+
  geom_sf(data = usa)+
  #geom_sf(data = CA_state_T)+
  #geom_sf(data = counties_T)+
  geom_sf(data = coastn_T)+
  geom_sf(data = density_pur_T, aes(col = m2_den))+
  coord_sf(xlim = c(-124.8, -123), ylim = c(38, 42.1), crs=4236) 


#CA_buff_inner <- st_crop(st_union(CA_buff_1000), st_union(states))



################################################################################
#create null model of original data
#https://bookdown.org/igisc/EnvDataSci/spatial-interpolation.html

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
null <- RMSE(mean(density_pur_T$m2_den), density_pur_T$m2_den)
null

sd(density_dat_pur$m2_den)

n <- length(density_dat_pur$m2_den)
null*sqrt(n/(n-1))

################################################################################
#Veronoi polygons

# proximity (Voronoi/Thiessen) polygons
denV <- vect(density_pur_T)
v <- terra::voronoi(denV)
plot(v)
points(denV)

#crop to buffer
outer_crop <- vect(st_union(CA_buff_1000))
vDen <- terra::crop(v, outer_crop)

plot(vDen,"m2_den")

#crop to land
vDen1 <- crop(vDen, states_T)

plot(vDen1, "m2_den")

#rasterize the polygons
r <- rast(vDen1, res=100)  # Builds a blank raster of given dimensions and resolution  
# We're in Teale metres, so the res is 100 m
vr <- rasterize(vDen1, r, "m2_den")
plot(vr)


#### plot
#https://dieghernan.github.io/tidyterra/
V <- ggplot(states_T) + 
  geom_spatraster(data=vr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density"
    )
  )+
  #add states
  geom_sf(data = states_T)+
  #add counties
  geom_sf(data = counties_T)+
  #add survey sites
  geom_sf(data = density_pur_T)+
  #crop
  coord_sf(xlim = c(-124.8, -123), ylim = c(38, 42.1), crs=4326) +
  theme_minimal()+
  labs(fill = "Sea urchin \ndensity \n(no. per m²)")



################################################################################
#IDW purple

d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))

gs <- gstat(formula=m2_den~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr <- mask(idw, vr)
plot(idwr, 1)


## Landmarks
landmarks <- tibble(site=c("Del Norte","Humboldt","Albion", "Caspar"),
                           Longitude=c(-124.2,-124.1,-123.770694, -123.818565),
                           Latitude=c(41.8, 41.2, 39.228264, 39.361313),
                           #hjust=c(-0.2, 0.5, 0.5),
                           #vjust=c(0.5, -0.3, -0.3)
)




# Theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=6),
                   #legend
                   legend.text = element_text(size=4),
                   legend.key.size = unit(0.3,'cm'),
                   legend.title=element_text(size=5),
                   plot.tag=element_text(size=4),
                   plot.title = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(color=NA))


#### plot
g1 <- ggplot(usa) + 
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  #add states
  geom_sf(data = usa)+
  #geom_sf(data = CA_state_T)+
  #add counties
  #geom_sf(data = counties_T)+
  #geom_sf(data=coastn_T)+
  #add survey sites
  geom_sf(data = density_pur_T, aes(shape=survey), size=0.5)+
  # label landmarks
  geom_text(data=landmarks, mapping=aes(x=Longitude, y=Latitude, label=site, hjust=-0.5, vjust=0
  ),
  size=2) +
  # Crop
  coord_sf(xlim = c(-124.8, -123.3), ylim = c(41, 42.1), crs=4326) +
  theme_minimal()+
  labs(fill = "Density \n(no. per m²)")+
  ggtitle("Purple sea urchin")+
  scale_x_continuous(breaks = c(-124.5, -124.0, -123.5))+
  #scale_x_continuous(n.breaks = 4)+
  my_theme+
  theme(legend.key = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(1,1,-0.5,1), "cm")
        )+
  #add scale bar
  ggsn::scalebar(x.min = -124.8, x.max = -123.3, 
                 y.min = 41, y.max = 42.1,
                 #anchor=c(x=-124.7,y=41),
                 location="bottomleft",
                dist = 5, dist_unit = "km",
                transform=TRUE, 
                 model = "WGS84",
                st.dist=0.02,
                st.size=1,
                border.size=.5,
                height=.01
                 )+
  #add northing arrow
  north(x.min = -124.8, x.max = -123.3, 
        y.min = 41, y.max = 42.1,
        location = "topright", 
        scale = 0.05, symbol = 10)+
  ggtitle("Humboldt")

g1


g2 <- ggplot(CA_state_T) + 
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  #add states
  #geom_sf(data = usa)+
  #add counties
  #geom_sf(data = counties_T)+
  #geom_sf(data=coastn_T)+
  geom_sf(data=CA_state_T)+
  #add survey sites
  geom_sf(data = density_pur_T, aes(shape=survey), size=0.5)+
  # label landmarks
  geom_text(data=landmarks, mapping=aes(x=Longitude, y=Latitude, label=site, hjust=-0.5, vjust=0
  ),
  size=2) +
  # Crop
  coord_sf(xlim = c(-124, -123.6), ylim = c(39.2, 39.5), crs=4326) +
  theme_minimal()+
  labs(fill = "Density \n(no. per m²)")+
  ggtitle("Purple sea urchin")+
  #scale_x_continuous(breaks = c(-124, -123.5))+
  my_theme+
  theme(legend.key = element_blank(),
        legend.position = "none",
       plot.margin=unit(c(1,1,-0.5,1), "cm")
        )+
  #add scale bar
  ggsn::scalebar(x.min = -124, x.max = -123.6, 
                 y.min = 39.2, y.max = 39.5,
                 #anchor=c(x=-124,y=39.2),
                 location="bottomleft",
                 dist = 2.5, dist_unit = "km",
                 transform=TRUE, 
                 model = "WGS84",
                 st.dist=0.02,
                 st.size=1,
                 border.size=.5,
                 height=.01
  )+
  #add northing arrow
  north(x.min = -124, x.max = -123.6, 
        y.min = 39.2, y.max = 39.5,
        location = "topright", 
        scale = 0.05, symbol = 10)+
  ggtitle("Mendocino")

g2


g3 <- ggplot(CA_state_T) + 
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  #add states
  #geom_sf(data = states_T)+
  geom_sf(data=CA_state_T)+
  #add counties
  #geom_sf(data = counties_T)+
  #geom_sf(data=coastn_T)+
  #add survey sites
  geom_sf(data = density_pur_T, aes(shape=survey), size=0.5)+
  # label landmarks
  geom_text(data=landmarks, mapping=aes(x=Longitude, y=Latitude, label=site, hjust=-0.5, vjust=0
  ),
  size=2) +
  # Crop
  coord_sf(xlim = c(-123.8, -123.55), ylim = c(38.80, 38.96), crs=4326) +
  theme_minimal()+
  labs(fill = "Density \n(no. per m²)")+
  ggtitle("Purple sea urchin")+
  #scale_x_continuous(breaks = c(-124, -123.5))+
  my_theme+
  theme(legend.key = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(-0.5,1,1,1), "cm")
        )+
  #add scale bar
  ggsn::scalebar(x.min = -123.8, x.max = -123.55, 
                 y.min = 38.80, y.max = 38.96,
                # anchor=c(x=-123.8,y=38.80),
                location="bottomleft",
                 dist = 2.5, dist_unit = "km",
                 transform=TRUE, 
                 model = "WGS84",
                 st.dist=0.02,
                 st.size=1,
                 border.size=.5,
                 height=.01
  )+
  north(x.min = -123.8, x.max = -123.55, 
        y.min = 38.80, y.max = 38.96,
        location = "topright", 
        scale = 0.05, symbol = 10)+
  ggtitle("Point Arena")

g3


g4 <- ggplot(CA_state_T) + 
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  #add states
  geom_sf(data=CA_state_T)+
  #add counties
  #geom_sf(data = counties_T)+
  #geom_sf(data=coastn_T)+
  #add survey sites
  geom_sf(data = density_pur_T, aes(shape=survey), size=0.5)+
  # label landmarks
  geom_text(data=landmarks, mapping=aes(x=Longitude, y=Latitude, label=site, hjust=-0.5, vjust=0
  ),
  size=2) +
  # Crop
  coord_sf(xlim = c(-123.65, -123.2), ylim = c(38.45, 38.75), crs=4326) +
  theme_minimal()+
  labs(fill = "Density \n(no. per m²)")+
  ggtitle("Purple sea urchin")+
  #scale_x_continuous(breaks = c(-124, -123.5))+
  my_theme+
  theme(legend.key = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(-0.5,1,1,1), "cm")
        )+
  #add scale bar
  ggsn::scalebar(x.min = -123.65, x.max = -123.2, 
                 y.min = 38.45, y.max = 38.75,
                 #anchor=c(x=-123.5,y=38.45),
                 location="bottomleft",
                 dist = 2.5, dist_unit = "km",
                 transform=TRUE, 
                 model = "WGS84",
                 st.dist=0.02,
                 st.size=1,
                 border.size=.5,
                 height=.01
  )+
  north(x.min = -123.65, x.max = -123.2, 
        y.min = 38.45, y.max = 38.75,
        location = "topright", 
        scale = 0.05, symbol = 10)+
  ggtitle("Sonoma")

g4



combined_plot <- gridExtra::grid.arrange(g1, g2, g3, g4
                                         )


get_only_legend <- function(plot) {
  
  # get tabular interpretation of plot
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  #  Mark only legend in plot
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend
  legend <- plot_table$grobs[[legend_plot]]
  
  # return legend
  return(legend) 
}

g_legend1 <- ggplot(states_T) + 
  geom_sf(data = states_T)+
  #add counties
  geom_sf(data = counties_T)+
  #add survey sites
  geom_sf(data = density_pur_T %>% rename("Program" = survey), aes(shape=Program), size=1.5)+
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  # Crop
  coord_sf(xlim = c(-124.8, -123.5), ylim = c(41, 42), crs=4326) +
  theme_minimal()+
  labs(fill = "Density \n(no. per m²)")+
  ggtitle("Purple sea urchin")+
  scale_x_continuous(breaks = c(-124, -123.5))+
  theme(legend.key = element_blank(),
        legend.text = element_text(size=7),
        legend.title=element_text(size=8))


legend1 <- get_only_legend(g_legend1)

g <- gridExtra::grid.arrange(gridExtra::arrangeGrob(g1, g2, g3, g4,
                               nrow=2,
                               ncol=2),
                   legend1, ncol=2, heights=c(10, 1),
                   widths=c(10, 1))
g


# Export figure
ggsave(g, filename=file.path(figdir, "combined_pur_density_4region_raster.png"), 
       width=7, height=6.5, units="in", dpi=600, bg="white")




