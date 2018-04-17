library(ggmap)
library(tidyverse)
library(stringr)
library(stringi)

load("./output/eb_refined.Rdata")

world <- map_data("world")

zz <- address_lat_long(data=eb_refined)

ggplot()+
  geom_polygon(data=world, aes(y=lat, x=long, group=group), fill=NA, color="black")+
  geom_point(data=zz, aes(x=lon,y=lat), color="purple")
