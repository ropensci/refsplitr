library(ggmap)
library(tidyverse)
library(stringr)
library(stringi)
library(refnet)
load("./output/eb_refined.Rdata")

world <- map_data("world")

zz <- address_lat_long(data=eb_refined)

plot_addresses_points(data=zz)
plot_addresses_country(data=zz)

net_plot_coauthor(addresses=zz)
net_plot_coauthor_address()
net_plot_coauthor_country()
