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

#read RCCA data
urch_den <- read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_demographics.csv"))

# Get land
states <- map_data("state") %>%
          filter(region %in% c("california","oregon"))%>%
          sf::st_as_sf(coords = c("long","lat"), crs=3310) %>%
          group_by(region) %>% 
          summarize(do_union=FALSE) %>%
          st_cast("POLYGON") %>% 
          ungroup()



usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
counties <- map_data("county") %>%
            filter(region == "california")%>%
            sf::st_as_sf(coords = c("long","lat"),  crs = 3310) %>%
            group_by(region, subregion) %>% 
            summarize(do_union=FALSE) %>%
            st_cast("POLYGON") %>% 
            ungroup()


# create site number

urch_den1 <- urch_den %>%
  filter(species == "purple urchin")%>%
  arrange(desc(Lat))%>%
  mutate(site_number = seq(1:nrow(.))) %>%
  select(site_new, site_number)

urch_den_spatial <- left_join(urch_den, urch_den1, by="site_new") %>%
  mutate(site_name = paste(site_number, site_new))


density_dat_all <- st_as_sf(urch_den_spatial, coords = c("Long","Lat")) 
density_dat_pur <- st_as_sf(urch_den_spatial, coords = c("Long","Lat"),
                            crs=3310) %>%
                      filter(species == "purple urchin")


#check features

ggplot() +
  #geom_sf(data = states)+
  geom_sf(data = CA_buff_1000)+
  geom_sf(data = counties)+
  geom_sf(data = density_dat_pur, aes(col = m2_den))+
  coord_sf(xlim = c(-124.8, -123), ylim = c(39, 42.1)) 
    

#create clipping buffer
CA_buff_1000 <- states %>%
  st_buffer(0.1)

#CA_buff_inner <- st_crop(st_union(CA_buff_1000), st_union(states))



################################################################################
#create null model of original data
#https://bookdown.org/igisc/EnvDataSci/spatial-interpolation.html

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
null <- RMSE(mean(density_dat_pur$m2_den), density_dat_pur$m2_den)
null

sd(density_dat_pur$m2_den)

n <- length(density_dat_pur$m2_den)
null*sqrt(n/(n-1))

################################################################################
#Veronoi polygons

# proximity (Voronoi/Thiessen) polygons
denV <- vect(density_dat_pur)
v <- terra::voronoi(denV)
plot(v)
points(denV)

#crop to buffer
outer_crop <- vect(st_union(CA_buff_1000))
vDen <- terra::crop(v, outer_crop)

plot(vDen,"m2_den")

#crop to land
vDen1 <- crop(vDen, states)

plot(vDen1, "m2_den")

#rasterize the polygons
r <- rast(vDen1, res=0.02)  # Builds a blank raster of given dimensions and resolution  
# We're in Teale metres, so the res is 1000 m
vr <- rasterize(vDen1, r, "m2_den")
plot(vr)


#### plot
#https://dieghernan.github.io/tidyterra/
V <- ggplot(states) + 
  geom_spatraster(data=vr) +  
  scale_fill_whitebox_c(
  palette = "muted",
  labels = scales::label_number(#suffix = "Density"
                                )
)+
  #add states
geom_sf(data = states)+
  #add counties
  geom_sf(data = counties)+
  #add survey sites
  geom_sf(data = density_dat_pur)+
  #crop
  coord_sf(xlim = c(-124.8, -123), ylim = c(38, 42.1), datum=st_crs(3310)) +
  theme_minimal()+
  labs(fill = "Sea urchin \ndensity \n(no. per m²)")



################################################################################
#IDW
require(gstat)

d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))

gs <- gstat(formula=m2_den~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr <- mask(idw, vr)
plot(idwr, 1)



#### plot
idw <- ggplot(states) + 
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density"
    )
  )+
  #add states
  geom_sf(data = states)+
  #add counties
  geom_sf(data = counties)+
  #add survey sites
  geom_sf(data = density_dat_pur)+
  #crop
  coord_sf(xlim = c(-124.8, -123), ylim = c(38, 42.1), datum=st_crs(3310)) +
  theme_minimal()+
  labs(fill = "Sea urchin \ndensity \n(no. per m²)")

idw



###fix projections and zoom
#https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/



#####maybe need to convert terra to sf?
#https://stackoverflow.com/questions/73825468/converting-spatvector-objects-to-data-frames-for-use-in-ggplot2


################################################################################
#create grid

#create grid area

grid_area <- raster::erase(CA_buff, land)



# Create grid 
#
# Define parameters:
width_in_pixels = 100 # 300 is better but slower 
# dx is the width of a grid cell in meters
dx <- ceiling( (st_bbox(CA_buff)["xmax"] - 
                  st_bbox(CA_buff)["xmin"]) / width_in_pixels)
# dy is the height of a grid cell in meters
# because we use quadratic grid cells, dx == dy
dy = dx
# calculate the height in pixels of the resulting grid
height_in_pixels <- floor( (st_bbox(CA_buff)["ymax"] - 
                              st_bbox(CA_buff)["ymin"]) / dy)
# Make the grid   
grid <- st_make_grid(CA_buff, 
                     cellsize = 500,
                     n = c(width_in_pixels, height_in_pixels),
                     what = "centers")








