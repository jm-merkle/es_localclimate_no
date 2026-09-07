#Download OSM data for visualisation
gc()

fp<-file.path(here::here(),"ancillary_data")

# download OSM data
# A function to download roads data for specific shapes within Norway
osm_get_streets_for_shp<-function(kshp){
  # reproject our shape to something that works with degrees
  kshp_w84 <- project(kshp, "EPSG:4326")
  # download roads
  roads <- kshp_w84 %>%
    sf::st_as_sf() %>%
    sf::st_bbox() %>%
    osmdata::opq() %>%
    osmdata::add_osm_feature(key="highway",value = c("motorway","trunk","primary","secondary")) %>%
    osmdata_sf()
  # reproject
  roads_t <- roads$osm_lines %>%
    terra::vect() %>%
    terra::project(kshp)
  # return result
  return(roads_t)
}

# then download
road_shp_list<-list()
for(i in 1:nrow(cityb_buf)){
  print(i)
  road_shp_list[[i]]<- osm_get_streets_for_shp(cityb_buf[i,])
}

# then save
for(i in 1:nrow(cityb_buf)){
  writeVector(road_shp_list[[i]],file.path(fp,paste0("roads_",cityb_buf$LAU_ID[i],".shp")))
}

#tidy up
rm(fp,road_shp_list)




