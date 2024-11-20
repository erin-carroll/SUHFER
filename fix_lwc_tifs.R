library(terra)

fps = list.files('data/predictions', full.names=T, pattern='lwc')
fps

r = rast(fps[1])
r
plot(r)
r[r > 100] = NA
plot(r)

for (fp in fps){
  r = rast(fp)
  r[r>100] = NA
  writeRaster(r, fp, overwrite=T)
}

# also fix the weird values from tile boundary in 2021-2023 lwc
## to do later