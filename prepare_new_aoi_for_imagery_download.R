library(sf)
library(stats)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/data/AOI')

list.files()

sf_use_s2(FALSE)
cfri_ra = st_read('new_AOI_CFRI_raplots.gpkg') # pts
cfri_activity = st_read('new_AOI_CFRIactivities.gpkg') # poly
gmug_activity = st_read('new_AOI_fire_timber_GMUGactivities.gpkg') # poly
sbeadmr = st_read('new_AOI_sbeadmr.gpkg') %>% # pts
  st_transform(crs=4326)

shps = list(cfri_ra, cfri_activity, gmug_activity, sbeadmr)

# all same crs?
for (i in shps){
  print(st_crs(i)==st_crs(4326))
}

# which are not yet polygons?
# cfri_ra, sbeadmr
for (i in shps){
  plot(st_geometry(i))
}

# convert cfri_ra, sbeadmr to bounding polygons

# cfri gets one big polygon
cfri_ra_bounds = st_convex_hull(st_union(cfri_ra)) %>%
  st_as_sf() %>%
  st_buffer(dist=200) %>%
  st_convex_hull()
  
ggplot() +
  geom_sf(data=cfri_ra_bounds) +
  geom_sf(data=cfri_ra, color='red') +
  theme_bw()

# sbeadmr gets 4 clustered polygons?
plot(st_geometry(sbeadmr))
coords <- st_coordinates(sbeadmr)
kmeans_result <- kmeans(coords, centers = 4)
sbeadmr$cluster = kmeans_result$cluster

ggplot() +
  geom_sf(data=sbeadmr, aes(color=as.factor(cluster)))

sbeadmr_bounds = list()
for (i in unique(sbeadmr$cluster)){
  d = sbeadmr %>%
    filter(cluster==i)
  p = st_convex_hull(st_union(d)) %>%
    st_as_sf() %>%
    st_buffer(dist=200) %>%
    st_convex_hull()
  sbeadmr_bounds[[i]] = p
}
sbeadmr_bounds = bind_rows(sbeadmr_bounds)

ggplot() +
  geom_sf(data=sbeadmr_bounds) +
  geom_sf(data=sbeadmr, color='red') +
  theme_bw()

# simplify cfri_activty
cfri_activity_bounds = cfri_activity %>%
  st_cast('POLYGON') %>%
  select(geom) %>%
  rename(x=geom)
ggplot() +
  geom_sf(data=cfri_activity_bounds)

# simplify gmug activities
gmug_activity_bounds = gmug_activity %>%
  st_cast('POLYGON') %>%
  select(geom) %>%
  rename(x=geom)
ggplot() +
  geom_sf(data=gmug_activity_bounds)

#############
# final step - merge all the polys

all_bounds = bind_rows(cfri_ra_bounds, sbeadmr_bounds, cfri_activity_bounds, gmug_activity_bounds)
all_bounds = all_bounds %>%
  st_make_valid() %>%
  st_union() %>%
  st_as_sf() %>%
  st_cast('POLYGON')

ggplot() +
  geom_sf(data=all_bounds)

st_write(all_bounds, 'all_new_AOI.shp', append=F)
