##############################################
##### basic run through of online sampling in 
##### relation to a destination
##### 10 Mar 2026
##### let's use Lembata  Island first
############################################

#remotes::install_github("petermeissner/wikipediatrend")
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
accom<-tourism.terms$Value[c(2,7,8,9,11,12,13,15)]


#get our basic large scale map

indonesia_bbox <- c(lon1 = 90, lon2 = 145, lat1 = -15, lat2 = 10)
lembata_bbox<-c(lon1 =123, lon2=124, lat1=-9,  lat2=-8)


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


bathy_lembata <- getNOAA.bathy(
  lon1 = lembata_bbox["lon1"],
  lon2 = lembata_bbox["lon2"],
  lat1 = lembata_bbox["lat1"],
  lat2 = lembata_bbox["lat2"],
  resolution = 2 
)

bathy_lembata_df <- marmap::fortify.bathy(bathy_lembata) #so it integrates with ggplot options



lembata_plot<-ggplot() +
  geom_raster(data = bathy_lembata_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient2(
    name = "Depth (m)",
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = 0
  ) +
#  geom_contour(data = bathy_lembata_df, aes(x, y, z = z), breaks = c(-30,-50,-100), color = "blue") +
  geom_sf(data = indonesia_hires, fill = "grey60", color = "black", linewidth = 0.2) +
  coord_sf(xlim = c(123, 124), ylim = c(-9, -8), expand = FALSE) +
  labs(title = "Lembata Island",x="longitude",y="latitude") +
  theme_minimal()

lembata_plot

ggsave("lembata.png",plot=lembata_plot,device="png",height=10,width=15,units="cm",dpi=200,bg="white")



#indonesia ADM
#let's plot nuts3 for show

indo_NUTS3<-st_read("indonesia_adm/idn_admbnda_adm3_bps_20200401.shp")
st_crs(indo_NUTS3)
#same CRS

indo_adm<-ggplot() +
  geom_sf(data = indonesia_hires, fill = "grey60", color = "black", linewidth = 0.2) +
  geom_sf(data = indo_NUTS3, fill = NA, color = "purple", linewidth = 0.1) +
  
  coord_sf(xlim = c(90, 145), ylim = c(-15, 10), expand = FALSE) +
  
  labs(title = "Indonesia — NUTS3",x="longitude",y="latitude") +
  theme_minimal()

ggsave("indo_NUTS3.png",plot=indo_adm,device="png",height=10,width=20,units="cm",dpi=200,bg="white")


indo3<-indo_NUTS3[,c("ADM0_EN","ADM3_EN","ADM3_PCODE","geometry")]
colnames(indo3)<-c("country","name","NUTS","geometry")

geometries<-c("osm_polygons","osm_multipolygons","osm_points","osm_lines","osm_multilines")


###########################################################################
#### OSM

for (i in 1:nrow(indo3)) {
  query_poly<-st_make_valid(indo3$geometry[i])
  
  osmbbox<-matrix(NA,2,2,dimnames=list(c("x","y"),c("min","max")))
  
  sfbbox<-st_bbox(queries[i,])
  osmbbox[1,]<-c(sfbbox$xmin,sfbbox$xmax)
  osmbbox[2,]<-c(sfbbox$ymin,sfbbox$ymax)
  
  
  #here we keep all attraction even the unnamed ones
  tourism<- osmbbox %>%
    opq() %>%
    add_osm_feature(key = "tourism", value = "attraction") %>%
    osmdata_sf() #%>%
 
  #########
  
  tourism_accom<- osmbbox %>%
    opq() %>%
    add_osm_feature(key = "tourism", value = accom) %>%
    osmdata_sf() %>%
    trim_osmdata(indo3$geometry[i])
}



#######################################################################
#### wikipedia



attraction_wiki.df<-data.frame(wiki_url=rep(NA,nattraction),dist=NA,attractiveness=NA,trend=NA,trendSE=NA)

m<-1
for (j in 1:length(geompres)) {
  
  ntemp<-nrow(tourism[[geompres[j]]])
  if(!is.null(tourism[[geompres[j]]]$wikipedia)) {
    attraction_wiki.df$wiki_url[m:(m+ntemp-1)]<-tourism[[geompres[j]]]$wikipedia
  }
  attraction_wiki.df$dist[m:(m+ntemp-1)]<-as.numeric(st_distance(st_make_valid(tourism[[geompres[j]]]),coast.std))
  m<-m+ntemp
}

for (k in 1:nrow(attraction_wiki.df)) {
  if((!is.na(attraction_wiki.df$wiki_url[k]))&(nchar(attraction_wiki.df$wiki_url[k])>3)) {
    
    try<-tryCatch(wp_trend(unlist(str_split(attraction_wiki.df$wiki_url[k],":"))[2],from = "2016-01-01",to = "2025-05-31", lang=c("nb","nn","sv","de","fo","gl","kl","da","no","se","is","en","fi","fr","es","pt","zh"),warn=FALSE),error=function(e) NA)
    if (!is.null(dim(try))) {
      attraction_wiki.df$attractiveness[k]<-sum(try$views,na.rm=TRUE)
      try.recent<-try[try$date>"2021-12-31",]
      if(nrow(try.recent)>30) {
        allviews<-aggregate(try.recent$views,by=list(try.recent$date),sum)
        
        zootry<-as.ts(zoo(allviews$x,allviews$Group.1),start=c(2021,365),end=c(2025,151),frequency=1)
        zootry[is.na(zootry)]<-0
        try.decomp<-decompose(ts((as.data.frame(zootry)$x),start=c(2022,1),end=c(2025,151),frequency=365))
        lm0<-summary(lm(try.decomp$trend~seq(1:length(try.decomp$x))))
        attraction_wiki.df$trend[k]<-lm0$coef[2]
        attraction_wiki.df$trendSE[k]<-lm0$coef[4]
      }
    }
  }
}
