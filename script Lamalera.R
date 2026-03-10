##############################################
##### basic run through of online sampling in 
##### relation to a destination
##### 10 Mar 2026
##### let's use Lembata  Island first
############################################


library(wikipediatrend)
library(stringr)
library(dplyr)
library(zoo)

library(sf)
library(ggplot2)
library(osmdata)
install.packages("marmap")
library(marmap)
library(rnaturalearth)
library(rnaturalearthhires)

#### key starting points
village<-"Lamarela"
island<-"Lembata"

setwd(here::here())

tourism.terms<-as.data.frame(available_tags("tourism"))
accom<-tourism.terms$Value[2,7,8,9,11,12,13,15]


#get our basic large scale map

indonesia_bbox <- c(lon1 = 90, lon2 = 145, lat1 = -15, lat2 = 10)



hires_coast <- ne_download(scale = "large", type = "coastline", category = "physical", returnclass="sf")
indonesia_hires <- st_crop(hires_coast, st_as_sfc(st_bbox(c(xmin=90, xmax=145, ymin=-15, ymax=10), crs=4326)))


bathy <- getNOAA.bathy(
  lon1 = indonesia_bbox["lon1"],
  lon2 = indonesia_bbox["lon2"],
  lat1 = indonesia_bbox["lat1"],
  lat2 = indonesia_bbox["lat2"],
  resolution = 2 
)

bathy_df <- marmap::fortify.bathy(bathy) #so it integrates with ggplot options


indo_plot<-ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient2(
    name = "Depth (m)",
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 0
  ) +
  #geom_contour(data = bathy_df, aes(x, y, z = z), breaks = c(-50,-100, -500, -1000, -2000), color = "blue") +
  geom_sf(data = indonesia_hires, fill = "grey60", color = "black", linewidth = 0.2) +
  coord_sf(xlim = c(90, 145), ylim = c(-15, 10), expand = FALSE) +
  labs(title = "Indonesia — High Resolution Coastline & Bathymetry",x="longitude",y="latitude") +
  theme_minimal()

ggsave("indonesia.png",plot=indo_plot,device="png",height=10,width=20,units="cm",dpi=200,bg="white")









