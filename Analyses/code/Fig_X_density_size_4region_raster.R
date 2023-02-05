#Joshua G. Smith
#February 4, 2023

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

#read monitoring data
urch_den <- read.csv(file.path(datadir, "monitoring_processed/mlpa_rcca_urchin_density_combined.csv"))
urch_size <- readRDS(file.path(datadir, "/monitoring_processed/rcca_mean_density_2016-22.Rda"))


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
#step 1 - process density data 
#aggregate data

density_mean <- urch_den %>%
  #calculate mean after MHW
  filter(year > 2016)%>%
  select(survey, region, site_new, species, Lat, Long, m2_den)%>%
  mutate(Long = ifelse(region == "Humbodlt" | region == "Mendocino" | region == "Sonoma", Long-0.005, Long))%>%
  group_by(survey, region, site_new, species) %>%
  summarise_at(vars("Lat","Long","m2_den"), mean)


################################################################################
#step 2 - project data
density_dat_pur <- st_as_sf(density_mean, coords = c("Long","Lat"),
                            crs=4326) %>%
                            filter(species == "purple urchin")

density_dat_red <- st_as_sf(density_mean, coords = c("Long","Lat"),
                            crs=4326) %>%
                            filter(species == "red urchin")

size_dat_pur <- st_as_sf(urch_size, coords = c("Long","Lat"),
                         crs=4326) %>%
                          filter(species == "purple urchin")

size_dat_red <- st_as_sf(urch_size, coords = c("Long","Lat"),
                         crs=4326) %>%
                          filter(species == "red urchin")


####transoform to Teale Albers
states_T <- st_transform(states, crs = 3310)
counties_T <- st_transform(counties, crs=3310)
density_pur_T <- st_transform(density_dat_pur, crs=3310)
density_red_T <- st_transform(density_dat_red, crs=3310)
size_pur_T <- st_transform(size_dat_pur, crs=3310)
size_red_T <- st_transform(size_dat_red, crs=3310)
coastn_T <- st_transform(coastn83, crs=3310)
CA_state_T <- st_transform(CA_state, crs=3310)

#create clipping buffer
CA_buff_1000 <- CA_state_T %>%
  st_buffer(5000)





################################################################################
####Interpolate data for red density
# proximity (Voronoi/Thiessen) polygons
denV <- vect(density_pur_T)
v <- terra::voronoi(denV)
points(denV)

#crop to buffer
outer_crop <- vect(st_union(CA_buff_1000))
vDen <- terra::crop(v, outer_crop)

#crop to land
vDen1 <- crop(vDen, states_T)

#rasterize the polygons
r <- rast(vDen1, res=100)  # Builds a blank raster of given dimensions and resolution  
# We're in Teale metres, so the res is 100 m
vr <- rasterize(vDen1, r,"m2_den")

#IDW 
d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))
gs <- gstat(formula=m2_den~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr_den_pur <- mask(idw, vr)




####Interpolate data for red density
# proximity (Voronoi/Thiessen) polygons
denV <- vect(density_red_T)
v <- terra::voronoi(denV)
points(denV)

#crop to buffer
outer_crop <- vect(st_union(CA_buff_1000))
vDen <- terra::crop(v, outer_crop)

#crop to land
vDen1 <- crop(vDen, states_T)

#rasterize the polygons
r <- rast(vDen1, res=100)  # Builds a blank raster of given dimensions and resolution  
# We're in Teale metres, so the res is 100 m
vr <- rasterize(vDen1, r,"m2_den")

#IDW 
d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))
gs <- gstat(formula=m2_den~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr_den_red <- mask(idw, vr)





####Interpolate data for pur size
# proximity (Voronoi/Thiessen) polygons
denV <- vect(size_pur_T)
v <- terra::voronoi(denV)
points(denV)

#crop to buffer
outer_crop <- vect(st_union(CA_buff_1000))
vDen <- terra::crop(v, outer_crop)

#crop to land
vDen1 <- crop(vDen, states_T)

#rasterize the polygons
r <- rast(vDen1, res=100)  # Builds a blank raster of given dimensions and resolution  
# We're in Teale metres, so the res is 100 m
vr <- rasterize(vDen1, r,"u_diff")

#IDW 
d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))
gs <- gstat(formula=u_diff~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr_size_pur <- mask(idw, vr)






####Interpolate data for pur size
# proximity (Voronoi/Thiessen) polygons
denV <- vect(size_red_T)
v <- terra::voronoi(denV)
points(denV)

#crop to buffer
outer_crop <- vect(st_union(CA_buff_1000))
vDen <- terra::crop(v, outer_crop)

#crop to land
vDen1 <- crop(vDen, states_T)

#rasterize the polygons
r <- rast(vDen1, res=100)  # Builds a blank raster of given dimensions and resolution  
# We're in Teale metres, so the res is 100 m
vr <- rasterize(vDen1, r,"u_diff")

#IDW 
d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))
gs <- gstat(formula=u_diff~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr_size_red <- mask(idw, vr)




################################################################################
####plot

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


#### Humboldt
d1 <- ggplot(usa) + 
  geom_spatraster(data=idwr_den_pur) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  #add states
  geom_sf(data = usa)+
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
        legend.position = "right",
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
  ggtitle("Purple sea urchin density")

d1

#### plot
s1 <- ggplot(usa) + 
  geom_spatraster(data=idwr_size_pur) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  #add states
  geom_sf(data = usa)+
  #add survey sites
  geom_sf(data = size_pur_T, size=0.5)+
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
        legend.position = "right",
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
  ggtitle("Purple sea urchin size (cm from mean)")

s1










