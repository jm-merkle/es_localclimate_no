j=4

test<-terra::rast(file.path(here::here(),"ancillary_data",paste0("heatdays_",j,".tif")))

plot(test)
test[test>0]<-1
test[test==0]<-NA

plot(test)

plot(citytownb)
plot(test,add=T)


# see if we can mask the citytownfile
mask_poly <- as.polygons(!is.na(test), dissolve = TRUE)
tv<-intersect(citytownb,mask_poly)
plot(mask_poly)
plot(tv)
# this doesn't work properly yet, but we'll get there.
# think its related to the shape forming an overall frame around everything.