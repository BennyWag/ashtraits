# figures redo for paper --------------------------------------------------

library(broom)
library(ggrepel)
library(ggalt)
library(ggpubr)
library(RColorBrewer)
library(terra)
library(raster)
library(sf)
library(vegan)
library(fasterize)
library(geodata)
library(ggspatial)
library(viridis)
library(corrplot)
library(tidyverse)
library(factoextra)
library(scales)
library(patchwork)

data<-read.csv('data/all_avg.csv')%>%
  mutate(prov = as.factor(prov))

prov_origin<-data%>%select(1:2)

gmin_raw<-read.csv('data/gmin.csv')%>%
  rename(rep = day, gmin = gminvalue)%>%
  filter(time_min>200,
         gmin < 6)%>%
  mutate(prov = as.factor(prov))


sd_raw<-read.csv('data/sd.csv')%>%
  mutate(prov = as.factor(prov))%>%
  filter(density < 400)

op_raw<-read.csv('data/op.csv')%>%rename(op = 3)%>%mutate(op = op*-1, prov = as.factor(prov))

sla_raw<-read.csv('data/sla.csv')%>%rename(prov = 1, avg_area = 3, avg_drwt = 4, sla = 5)%>%
  mutate(prov = as.factor(prov))

#add origin

gmin_raw_state<-left_join(gmin_raw, prov_origin, by = 'prov')
sd_raw_state<-left_join(sd_raw, prov_origin, by = 'prov')
op_raw_state<-left_join(op_raw, prov_origin, by = 'prov')
sla_raw_state<-left_join(sla_raw, prov_origin, by = 'prov')

#dir.create('figures/')

display.brewer.all()

# elevation x ahmi graph --------------------------------------------------

ggplot(data, aes(AHMI, dem, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 4)+
  geom_text_repel(show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  scale_y_continuous(breaks = scales::breaks_width(200)) +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  labs(y = 'Elevation (m)', color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")


ggsave('1.ele_ahmi.svg',path = 'figures/', width = 20, height = 20, units = 'cm', dpi = 600)


ele_1<-ggplot(data, aes(mean_temp, dem, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 4)+
  geom_text_repel(show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  scale_y_continuous(breaks = scales::breaks_width(200)) +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  labs(y = 'Elevation (m)', x = 'Mean annual temperature (°C)', color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

ele_2<-ggplot(data, aes(mean_precip, dem, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 4)+
  geom_text_repel(show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  scale_y_continuous(breaks = scales::breaks_width(200)) +
  scale_x_continuous(breaks = scales::breaks_width(100)) +
  labs(y = 'Elevation (m)', x = 'Annual precipitation (mm)', color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

ele_1/ele_2 +  
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')


ggsave('1.1.ele_ahmi.svg',path = 'figures/', width = 20, height = 20, units = 'cm', dpi = 600)

# tratis x Prov -----------------------------------------------------------

traits_1<-ggplot(gmin_raw_state, aes(prov, gmin, color = state))+
  geom_boxplot(size = 0.7)+
  scale_color_brewer(palette = 'Dark2')+
  labs(y = expression(g[min] ~ (mmol ~ m^{-2} ~ s^{-1})), x = NULL, color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 11, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 11, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")


traits_2<-ggplot(op_raw_state, aes(prov, op, color = state))+
  geom_boxplot(size = 0.7)+
  scale_color_brewer(palette = 'Dark2')+
  labs(y = expression(pi[100] ~ (MPa)), x = NULL, color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 11, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 11, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

traits_3<-ggplot(sd_raw_state, aes(prov, density, color = state))+
  geom_boxplot(size = 0.7)+
  scale_color_brewer(palette = 'Dark2')+
  scale_y_continuous(breaks = scales::breaks_width(50)) +
  labs(y = expression(SD ~ (mm^{-2})), x = 'Provenance', color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 11, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 11, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")


traits_4<-ggplot(sla_raw_state, aes(prov, sla, color = state))+
  geom_boxplot(size = 0.7)+
  scale_color_brewer(palette = 'Dark2')+
  labs(y = expression(SLA ~ (cm^{-2} ~ g^{-1})), x = 'Provenance', color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 11, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 11, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")


(traits_1 + traits_2) /
  (traits_3 + traits_4) +
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')


ggsave('2.traits.svg',path = 'figures/', width = 25, height = 20, units = 'cm', dpi = 600)

# inter-trait relationships -----------------------------------------------

#gmin x sd

inter_1<-ggplot(data, aes(avg_stomata, avg_gmin, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 2)+
  geom_text_repel(show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  geom_smooth(aes(color = NULL), method = 'lm', color = 'black')+
  stat_cor(aes(x = avg_stomata, y = avg_gmin, 
               label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE)+
  labs(y = expression(g[min] ~ (mmol ~ m^{-2} ~ s^{-1})), x = expression(SD ~ (mm^{-2})), color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")
  

#gmin x SLA

inter_2<-ggplot(data, aes(avg_SLA, avg_gmin, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 2)+
  geom_text_repel(show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  geom_smooth(aes(color = NULL), method = 'lm', color = 'black')+
  stat_cor(aes(x = avg_SLA, y = avg_gmin, 
               label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE)+
  labs(y = expression(g[min] ~ (mmol ~ m^{-2} ~ s^{-1})), x = expression(SLA ~ (cm^{-2} ~ g^{-1})), color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

#OP x SD

inter_3<-ggplot(data, aes(avg_stomata, avg_osmotic*-1, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 2)+
  geom_text_repel(show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  geom_smooth(aes(color = NULL), method = 'lm', color = 'black')+
  stat_cor(aes(x = avg_stomata, y = avg_osmotic*-1, 
               label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE)+
  labs(y = expression(pi[100] ~ (MPa)), x = expression(SD ~ (mm^{-2})), color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

#OP x SLA

inter_4<-ggplot(data, aes(avg_SLA, avg_osmotic*-1, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 2)+
  geom_text_repel(show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  geom_smooth(aes(color = NULL), method = 'lm', color = 'black')+
  stat_cor(aes(x = avg_SLA, y = avg_osmotic*-1, 
               label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE)+
  labs(y = expression(pi[100] ~ (MPa)), x = expression(SLA ~ (cm^{-2} ~ g^{-1})), color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

#fix spacing at 20 x 20 cm output

inter_1 <- inter_1 + theme(plot.margin = margin(5.5, 10, 5.5, 5.5))  # more space on the right
inter_3 <- inter_3 + theme(plot.margin = margin(5.5, 10, 5.5, 5.5))

(inter_1 + inter_2) /
  (inter_3 + inter_4) +
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')


ggsave('3.inter_traits.svg',path = 'figures/', width = 20, height = 20, units = 'cm', dpi = 600)

# trait x climate relationships -------------------------------------------

data_long<-data%>%
  select(-c('sd_gmin', 'sd_stomata', 'sd_osmotic', 'sd_SLA'))%>%
  mutate(avg_osmotic = avg_osmotic*-1)%>%
  pivot_longer(cols = 3:6, names_to = 'var')%>%
  mutate(label = case_when(var == "avg_gmin" ~ 'g[min] ~ mmol ~ m^{-2} ~ s^{-1}',
                           var == "avg_osmotic" ~ 'pi[100] ~ MPa',
                           var == 'avg_SLA' ~ 'SLA ~ cm^{-2} ~ g^{-1}',
                           var == 'avg_stomata' ~ 'SD ~ mm^{-2}'))



#traits x AHMI

ggplot(data_long, aes(AHMI, value, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 2)+
  geom_text_repel(show.legend = F)+
  facet_wrap(~label, scales = 'free', labeller =  label_parsed)+
  scale_color_brewer(palette = 'Dark2')+
  geom_smooth(aes(color = NULL), method = 'lm', color = 'black')+
  stat_cor(aes(x = AHMI, y = value, 
               label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE)+
  labs(x = 'AHMI', y = NULL, color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        strip.text = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

#All labels

data_long_long<-data_long%>%
  rename('"MAT (" * degree * "C)"' = 4,
         'MAP~(mm)' = 5,
         'Elevation' = 6)%>%
  pivot_longer(cols = 3:6, names_to = 'clim', values_to = 'climvars')%>%
  mutate(clim = factor(clim, levels = c('AHMI', '"MAT (" * degree * "C)"', 'MAP~(mm)', 'Elevation')))

ggplot(data_long_long, aes(climvars, value, color = as.factor(state), label = as.character(prov)))+
  geom_point(size = 2)+
  geom_text_repel(show.legend = F)+
  facet_grid(label~clim, scales = 'free', labeller =  label_parsed)+
  scale_color_brewer(palette = 'Dark2')+
  geom_smooth(aes(color = NULL), method = 'lm', color = 'black')+
  stat_cor(aes(x = climvars, y = value, 
               label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           inherit.aes = FALSE)+
  labs(x = NULL, y = NULL, color = 'State of origin')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        strip.text = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

ggsave('4.climate_traits.svg',path = 'figures/', width = 30, height = 30, units = 'cm', dpi = 600)


# study area map ----------------------------------------------------------

#get base data

#dir.create('data/map')

#borders

aus<-gadm(country = 'AU', level = 1, resolution = 2, path = 'data/map')%>%
  st_as_sf()%>%
  dplyr::select(state = NAME_1, country = COUNTRY)%>%
  st_make_valid()

aus_simp<-st_read('data/map/aus/Australia_proj.shp')%>%st_transform(crs = 4326)

#places

aus_places<-osm(country = 'AU', var = 'places', path = 'data/map')%>%st_as_sf()%>%
  filter(place %in% c('town', 'city'))

#burnley

burnley_point <- st_sfc(st_point(c(145.02442530956603, -37.82995167117297)), crs = 4326)%>%
  st_sf()%>%
  mutate(name = 'Burnley')

#AA extent - get VIC and rasterise

rasterOptions(progress = 'text')

AA_vic<-st_read('data/map/AA/AA_EVC_Vic.gpkg')

raster_template <- raster(extent(AA_vic), 
                          resolution = 0.001, 
                          crs = crs(AA_vic))

AA_vic_raster <- fasterize(AA_vic, raster_template, fun = 'sum', background = 0)

# NSW nd combine extents 

AA_nsw<-raster('data/map/AA/vegmap_AA.tif')

extent_vic <- extent(AA_vic_raster)
extent_nsw <- extent(AA_nsw)

combined_extent <- extent(
  min(extent_vic@xmin, extent_nsw@xmin),  # xmin
  max(extent_vic@xmax, extent_nsw@xmax),  # xmax
  min(extent_vic@ymin, extent_nsw@ymin),  # ymin
  max(extent_vic@ymax, extent_nsw@ymax)   # ymax
)

ext_combined <- raster(combined_extent, res = 0.001, crs = crs(AA_vic))

#resample

vic_resampled <- resample(AA_vic_raster, ext_combined, method = "ngb")
nsw_resampled <- resample(AA_nsw, ext_combined, method = "ngb")

#fill no data with 0

vic_resampled[is.na(vic_resampled)] <- 0
nsw_resampled[is.na(nsw_resampled)] <- 0


names(nsw_resampled)<-'layer'

#combine

AA_mainland<-nsw_resampled+vic_resampled

AA_mainland[AA_mainland == 0] <- NA_real_

writeRaster(AA_mainland, "data/map/AA/combined_AA_mainland.tiff", 
            options = c("COMPRESS=LZW"),
            overwrite = T)

#without Otways:

e<-extent(AA_mainland)

east_extent <- extent(145, e@xmax, e@ymin, e@ymax)

AA_main_crop<-crop(AA_mainland, east_extent)

plot(AA_main_crop)

writeRaster(AA_main_crop, "data/map/AA/combined_AA_mainland_crop.tiff", 
            options = c("COMPRESS=LZW"),
            overwrite = T)

#get origin

climate_data_sf<-read.csv('data/climate_origin.csv')%>%
  rename(prov = Provenance)%>%
  mutate(prov = as.factor(prov))%>%
  filter(!row_number() %in% c(14))%>%
  st_as_sf(coords = c('x', 'y'), crs = 4326)

#make map

aus_base<-aus_simp%>%dplyr::filter(STATENAME %in% c('Victoria', 'Tasmania', 'New South Wales', 'Australian Capital Territory'))

st_bbox(aus_base)

aus_base_crop<-st_crop(aus_base, xmin = 140.963, ymin = -43.637, xmax = 153.635, ymax = -33)

#get DEM

dem_30s<-elevation_30s(country="AU", path = 'data/map', mask = T, res = 0.5)%>%
  crop(aus_base_crop)%>%
  mask(aus_base_crop)

dem_sample <- spatSample(dem_30s, size = 5e5, method = "regular", as.raster = TRUE) %>%
  as.data.frame(xy = TRUE, na.rm = TRUE) %>% 
  setNames(c('x', 'y', 'ele'))

aa_df<-AA_mainland%>%
  as.data.frame(xy = T, na.rm = T)%>%
  setNames(c('x', 'y', 'AA'))%>%
  mutate(region = case_when(x<145 ~ 'Otways',
                            TRUE ~ 'Alps'))%>%
  group_split(region)

names(aa_df)<-c('Alps', 'Otways')

map<-ggplot() +
  geom_raster(data = dem_sample, aes(x = x, y = y, fill = ele)) +
  #geom_raster(data = aa_df, aes(x = x, y = y, fill = AA), fill = 'dodgerblue') +
  geom_encircle(data = aa_df$Alps,
                aes(x = x, y = y),
                s_shape = 1,
                expand = 0.02, 
                alpha = 0.5, 
                size = 4, 
                show.legend = F,
                color = 'dodgerblue',
                fill = 'darkgrey')+
  # geom_encircle(data = aa_df$Otways,
  #               aes(x = x, y = y),
  #               s_shape = 1,
  #               expand = 0.02, 
  #               alpha = 0.5, 
  #               size = 4, 
  #               show.legend = F,
  #               color = 'dodgerblue',
  #               fill = 'darkgrey')+
  #scale_fill_viridis(option = "E")+ 
  scale_fill_gradient(low = "white", high = "darkgrey") +
  geom_sf(data = aus_base_crop, color = "darkgrey", fill = NA, linewidth = 0.5)+
  geom_sf(data = climate_data_sf, aes(color = State), shape = 3,  size = 3, stroke = 1.1)+
  scale_color_brewer(palette = 'Dark2')+
  geom_sf(data = burnley_point, color = 'orange', size = 4)+
  labs(fill = 'Elevation (m)',
       color = 'State of origin',
       x = NULL,
       y = NULL) +
  coord_sf(expand = F)+
  theme_bw()+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black')+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

map

inset<-ggplot(data = aus_simp, show.legend = "point")+
  geom_sf(fill = 'white', color = 'black')+
  geom_rect(xmin = extent(aus_base_crop)[1], xmax = extent(aus_base_crop)[2],
            ymin = extent(aus_base_crop)[3], ymax = extent(aus_base_crop)[4],
            color = 'red', fill = NA, linewidth = 0.5)+
  labs(x = NULL, y = NULL, title = NULL)+
  coord_sf(expand = F)+
  theme_void()

map+annotation_custom(ggplotGrob(inset), xmin = 148.5, xmax = 152.1, ymin = -33.1, ymax = -34.8)


ggsave('0.map.svg',path = 'figures/', width = 20, height = 25, units = 'cm', dpi = 600)

# AHMI appendix map -------------------------------------------------------

ahmi<-rast('data/map/ahmi_81-20.tif')
plot(ahmi)

ahmi_plot<-spatSample(ahmi, size = 5e5, method = "regular", as.raster = TRUE) %>%
  as.data.frame(xy = TRUE, na.rm = TRUE) %>% 
  setNames(c('x', 'y', 'ahmi'))%>%
  mutate(ahmi_cat = cut(
    ahmi,
    breaks = c(-Inf, 10, 30, 50, 80, Inf),
    labels = c("<10", "10-30", "30-50", "50-80", ">80"),
    right = FALSE))

ahmi_colors <- c(
  "<10" = "blue",
  "10-30" = "green",
  "30-50" = "yellow",
  "50-80" = "orange",
  ">80" = "red"
)

labels_df <- data.frame(
  x = c(142, 144, 143, 146),
  y = c(-35, -36.5, -37.5, -38),
  label = c("Mallee: 80-130", "Dry sclerophyll: 50-80", "Lowland/Coastal: 30-50", "Montane: 10-30 (AA range)")
)

ggplot() +
  geom_raster(data = ahmi_plot, aes(x = x, y = y, fill = ahmi_cat)) +
  scale_fill_manual(values = ahmi_colors) +
  geom_sf(data = climate_data_sf%>%filter(State == 'VIC'), 
          fill = 'dodgerblue', color = 'black', shape = 23,  size = 3)+
  geom_sf(data = burnley_point, fill = 'orange', color = 'black', size = 4, shape = 21)+
  geom_label(data = labels_df, aes(x = x, y = y, label = label),
            color = "black", 
            fill = 'white',
            size = 4, 
            fontface = "bold") +
  labs(fill = 'AHMI',
       x = NULL,
       y = NULL) +
  coord_sf(expand = F)+
  theme_bw()+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'br', style = 'ticks', text_col = 'black')+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = c(0.98, 0.98),  # Top-right inside plot (x, y in NPC coordinates)
        legend.justification = c("right", "top"),  # Anchor the position
        legend.background = element_rect(fill = "white", color = "black", size = 0.8),  
        legend.box.background = element_rect(color = "black", size = 0.8))

ggsave('A3.ahmi_map.svg',path = 'figures/', width = 30, height = 20, units = 'cm', dpi = 600)


# ALA x AHMI --------------------------------------------------------------

ahmi_eucs<-read.csv('data/eucs_ahmi.csv')

ggplot(ahmi_eucs, aes(reorder(species, ahmi), ahmi, fill = source))+
  geom_boxplot(size = 1, outlier.shape = NA)+
  scale_y_continuous(breaks = seq(0, 140, by = 10), limits = c(0,140))+
  scale_fill_manual(values = c("ALA" = "dodgerblue", "This study" = "orange")) +
  labs(y = 'AHMI', fill = 'Data source')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', angle = 45,  hjust = 1),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = 'bottom')

ggsave('5.ahmi_comp.svg',path = 'figures/', width = 30, height = 20, units = 'cm', dpi = 600)

# PCA for review ----------------------------------------------------------

#on averages

data_pca<-data%>%select(3, 5, 7, 9, 11:14)%>%
  mutate(across(where(is.numeric), scale))


pca_result <- prcomp(data_pca, scale. = TRUE)
summary(pca_result)
print(pca_result)

biplot(pca_result)

#using ggplot

#Build a data frame
pcaData <- as.data.frame(pca_result$x[, 1:2]) # extract first two PCs
pcaData <- cbind(pcaData, data$prov, data$state) # add species to df
colnames(pcaData) <- c("PC1", "PC2", "Provenance", 'State')

all_PCplot<-ggplot(pcaData) +
  aes(PC1, PC2, color = State, label = Provenance) + 
  geom_point(size = 2) + 
  geom_text_repel(vjust = -1, size = 3, show.legend = F) +  
  labs(title = 'Traits and climate origin averages')+
  coord_fixed()+
  theme_bw()

#using factoextra

fviz_pca_var(pca_result,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)+
  theme_bw()

fviz_pca_biplot(pca_result, repel = T)

all_varplot<-fviz_pca_biplot(pca_result, 
                repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969" )+
  coord_fixed()+
  labs(title = 'Traits and climate origin averages - variable contribution')+
  theme_bw()
             
all_PCplot / all_varplot

ggsave('X.PCA_avg.svg',path = 'figures/', width = 15, height = 20, units = 'cm', dpi = 600)


#detailed data - traits

# gmin #

gmin_raw_state_av<-gmin_raw_state%>%
  group_by(prov, state, rep)%>%
  summarise(mean_gmin = mean(gmin))

gmin_raw_state_climate<-left_join(gmin_raw_state_av, 
                                  data%>%select(prov, AHMI, mean_temp, mean_precip, dem), by = 'prov')

gmin_pca<-gmin_raw_state_climate%>%
  ungroup()%>%
  dplyr::select(4:8)

gmin_pca_result <- prcomp(gmin_pca, scale. = TRUE)

print(gmin_pca_result)

biplot(gmin_pca_result)

gmin_pcaData <- as.data.frame(gmin_pca_result$x[, 1:2]) 
gmin_pcaData <- cbind(gmin_pcaData, gmin_raw_state_climate$prov, gmin_raw_state_climate$state) 
colnames(gmin_pcaData) <- c("PC1", "PC2", "Provenance", 'State')

set.seed(222)
gmin_pca<-ggplot(gmin_pcaData) +
  aes(PC1, PC2, color = Provenance) + 
  geom_point(size = 2) + 
  labs(title = 'gmin + climate of origin')+
  scale_color_manual(values = sample(hue_pal()(21)))+
  coord_fixed()+
  theme_bw()+
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

# sd #

sd_raw_state_av<-sd_raw_state%>%
  group_by(prov, state, tree)%>%
  summarise(mean_sd = mean(density))

sd_raw_state_climate<-left_join(sd_raw_state_av, 
                                  data%>%select(prov, AHMI, mean_temp, mean_precip, dem), by = 'prov')

sd_pca<-sd_raw_state_climate%>%
  ungroup()%>%
  dplyr::select(4:8)%>%
  mutate(across(where(is.numeric), scale))

sd_pca_result <- prcomp(sd_pca, scale. = TRUE)

print(sd_pca_result)

biplot(sd_pca_result)

sd_pcaData <- as.data.frame(sd_pca_result$x[, 1:2]) 
sd_pcaData <- cbind(sd_pcaData, sd_raw_state_av$prov, sd_raw_state_av$state) 
colnames(sd_pcaData) <- c("PC1", "PC2", "Provenance", 'State')

set.seed(222)
sd_pca<-ggplot(sd_pcaData) +
  aes(PC1, PC2, color = Provenance) + 
  geom_point(size = 2) + 
  labs(title = 'SD + climate of origin')+
  scale_color_manual(values = sample(hue_pal()(21)))+
  coord_fixed()+
  theme_bw()+
  guides(color = guide_legend(nrow = 3, byrow = TRUE))


# op #

op_raw_state_av<-op_raw_state%>%
  group_by(prov, state, tree)%>%
  summarise(mean_op = mean(op))

op_raw_state_climate<-left_join(op_raw_state_av, 
                                data%>%select(prov, AHMI, mean_temp, mean_precip, dem), by = 'prov')

op_pca<-op_raw_state_climate%>%
  ungroup()%>%
  dplyr::select(4:8)%>%
  mutate(across(where(is.numeric), scale))

op_pca_result <- prcomp(op_pca, scale. = TRUE)

biplot(op_pca_result)

op_pcaData <- as.data.frame(op_pca_result$x[, 1:2]) 
op_pcaData <- cbind(op_pcaData, op_raw_state_av$prov, op_raw_state_av$state) 
colnames(op_pcaData) <- c("PC1", "PC2", "Provenance", 'State')

set.seed(222)
op_pca<-ggplot(op_pcaData) +
  aes(PC1, PC2, color = Provenance) + 
  geom_point(size = 2) + 
  labs(title = 'OP + climate of origin')+
  scale_color_manual(values = sample(hue_pal()(21)))+
  coord_fixed()+
  theme_bw()+
  guides(color = guide_legend(nrow = 3, byrow = TRUE))


# sla #

sla_raw_state_av<-sla_raw_state%>%
  group_by(prov, state, trees)%>%
  summarise(mean_sla = mean(sla))

sla_raw_state_climate<-left_join(sla_raw_state_av, 
                                data%>%select(prov, AHMI, mean_temp, mean_precip, dem), by = 'prov')

sla_pca<-sla_raw_state_climate%>%
  ungroup()%>%
  dplyr::select(4:8)%>%
  mutate(across(where(is.numeric), scale))

sla_pca_result <- prcomp(sla_pca, scale. = TRUE)

biplot(sla_pca_result)

sla_pcaData <- as.data.frame(sla_pca_result$x[, 1:2]) 
sla_pcaData <- cbind(sla_pcaData, sla_raw_state_av$prov, sla_raw_state_av$state) 
colnames(sla_pcaData) <- c("PC1", "PC2", "Provenance", 'State')

set.seed(222)
sla_pca<-ggplot(sla_pcaData) +
  aes(PC1, PC2, color = Provenance) + 
  geom_point(size = 2) + 
  labs(title = 'SLA + climate of origin')+
  scale_color_manual(values = sample(hue_pal()(21)))+
  coord_fixed()+
  theme_bw()+
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

(gmin_pca + sd_pca) /
  (sla_pca + op_pca) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom') 

ggsave('X.PCA_indiv.svg',path = 'figures/', width = 20, height = 20, units = 'cm', dpi = 600)

# combined PCA per tree ---------------------------------------------------

trait_all<-left_join(gmin_raw_state_av%>%rename(tree = rep), sd_raw_state_av, by = c('prov', 'state', 'tree'))%>%
  left_join(., op_raw_state_av, by = c('prov', 'state', 'tree'))%>%
  left_join(., sla_raw_state_av%>%rename(tree = trees), by = c('prov', 'state', 'tree'))

trait_all_pca<-trait_all%>%
  ungroup()%>%
  dplyr::select(4:7)%>%
  mutate(across(where(is.numeric), scale))

trait_all_pca_pca_result <- prcomp(trait_all_pca, scale. = T)

print(trait_all_pca_pca_result)
#PC1 driven by sd and op
#PC2 driven by gmin + sla

summary(trait_all_pca_pca_result)

biplot(trait_all_pca_pca_result)

all_pcaData <- as.data.frame(trait_all_pca_pca_result$x[, 1:2]) 
all_pcaData <- cbind(all_pcaData, trait_all$prov, trait_all$state) 
colnames(all_pcaData) <- c("PC1", "PC2", "Provenance", 'State')

set.seed(222)
all_trait_plot<-ggplot(all_pcaData) +
  aes(PC1, PC2, color = Provenance) + 
  geom_point(size = 2) + 
  labs(title = 'All traits')+
  scale_color_manual(values = sample(hue_pal()(21)))+
  coord_fixed()+
  theme_bw()+
  guides(color = guide_legend(nrow = 3, byrow = TRUE))+
  theme(legend.position = 'bottom') 

alltraits_varplot<-fviz_pca_var(trait_all_pca_pca_result,
                                col.var = "contrib", # Color by contributions to the PC
                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE,
                                title = 'All traits - variable contribution')+
  theme_bw()

all_trait_plot / alltraits_varplot

biplot_1<-fviz_pca_biplot(trait_all_pca_pca_result, 
                habillage = trait_all$state,
                palette = brewer.pal(5, 'Dark2'),
                col.var = 'red',
                label = 'var',
                addEllipses = T,
                ellipse.level = 0.9,
                title = 'Traits - PCA biplot',
                legend.title = 'State')+
  theme_bw()

ggsave('X.PCA_alltraits.svg',path = 'figures/', width = 15, height = 20, units = 'cm', dpi = 600)

# add climate variables - plot together

# combined PCA with climate data ------------------------------------------

trait_all_climate<-left_join(trait_all,
                             data%>%select(1,11:14),
                             by = 'prov')%>%
  ungroup()

trait_all_climate_scaled<-trait_all_climate%>%
  mutate(across(4:11, ~ as.numeric(scale(.))))

corrplot(cor(select(trait_all_climate_scaled, 8:11)),
         method = "number", type="lower", order="hclust", tl.cex = 0.75, tl.col="black", tl.srt = 45)

trait_cliamte_pca <- prcomp(trait_all_climate_scaled%>%select(4:9, 11), scale. = T)

print(trait_cliamte_pca)
#PC1 driven by elev (pos), mean_temp(pos) and AHMI
#PC2 driven by mean_sd(neg), mean_op(pos)

summary(trait_cliamte_pca)

biplot(trait_cliamte_pca)
summary(trait_cliamte_pca)
scores(trait_cliamte_pca, display = 'species')

biplot_2<-fviz_pca_biplot(trait_cliamte_pca, 
                habillage = trait_all_climate_scaled$state,
                palette = brewer.pal(5, 'Dark2'),
                col.var = 'red',
                label = 'var',
                addEllipses = T,
                ellipse.level = 0.9,
                title = 'Traits & Climate - PCA biplot',
                legend.title = 'State')+
  labs(color = 'State')+
  theme_bw()

biplot_1 / biplot_2 + plot_layout(guides = 'collect') 

ggsave('X.PCA_final.svg',path = 'figures/', width = 20, height = 20, units = 'cm', dpi = 600)

#save results

print(trait_all_pca_pca_result)
print(trait_cliamte_pca)

trait_pca_contrib<-trait_all_pca_pca_result$rotation%>%as.data.frame()%>%rownames_to_column(var = 'variable')
climate_pca_contrib<-trait_cliamte_pca$rotation%>%as.data.frame()%>%rownames_to_column(var = 'variable')

write.csv(trait_pca_contrib, 'outputs/pca_results-traits.csv', row.names = F)
write.csv(climate_pca_contrib, 'outputs/pca_results-traits+climate.csv', row.names = F)

# RDA ---------------------------------------------------------------------

traits<-trait_all_climate_scaled%>%select(4:7)
climate<-trait_all_climate_scaled%>%select(8,9,11)

trait_climate_rda<-rda(traits ~ ., data = climate)

summary(trait_climate_rda)

#6% of total variance explained by model
#RDA1: 4.95% explained
#RDA2: 1%

scores(trait_climate_rda, display = 'species', choices = 1:7) #'species' scores
scores(trait_climate_rda, display = 'bp', choices = 1:7) #biplot scores

#RD1 driven by DEM (neg.) and AHMI (pos)
#RD2 driven by AHMI and mean temp

#scores(trait_climate_rda, display = "bp", choices = 1:7, scaling = 2, type = "residual")

coef(trait_climate_rda) #variable contribution

vif.cca(trait_climate_rda) #variane inflation factor

summary(trait_climate_rda)$cont$importance #variance explained by axes

RsquareAdj(trait_climate_rda)
anova.cca(trait_climate_rda, step = 1000)
#model is signficiant ~ 0.01

anova.cca(trait_climate_rda, step = 1000, by = "axis")
#only RDA1 signficiant at 0.004, RDA2 not significant

anova.cca(trait_climate_rda, step = 1000, by = "term")
#only DEM significant at 0.007

ordiplot(trait_climate_rda, scaling = 2, type = "points")


