#hexsticker
library(refnet)
library(hexSticker)
load(file="~/refnet2/output/hex_input.Rdata")

polys <- dd$data_polygon
path <- dd$data_path
pois <- dd$data_points

a <- ggplot() + 
  geom_polygon(data=polys, 
               aes(long,lat,group=group), 
               fill=gray(8/10),
               color="black") +
  geom_path(data=polys, 
            aes(long,lat,group=group), 
            color=gray(6/10)) +
  coord_equal() + 
  geom_path(data=path,
            aes(x = x, y = y, 
                group = Group), 
            alpha=0.1,
            color="#542788",
            size=1.5
  ) +  # Customize taper
  geom_point(data=pois, 
               aes(LON,LAT))+
  theme_bw()+
  theme(line = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        strip.text = element_blank(),
        plot.title = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")


p.1 <- sticker(a,
               package="refnet2", 
               s_x = 1, # horizontal position of subplot
               s_y = 1.1, # vertical position of subplot
               s_width = 1.5, # width of subplot
               s_height = 1.5, # height of subplot
               p_x = 1, # horizontal position of font
               p_y = .53, # vertical position of font
               p_size = 10, # font size
               p_color = "#000000", # font colour
               h_size = 7, # hexagon border size
               h_fill = "white", # hexagon fill colour
               h_color = "#542788") # hexagon border colour


