


kable(firstdf %>% 
        filter((month==8 & city_code %in% cityb_l[[3]]$LAU_ID) | (month == 7 & city_code %in% cityb_l[[2]]$LAU_ID)) %>%
        select(-c(p_pixels_rm,p_pixels_rm_due_na_lcm,p_pixels_rm_due_na_data)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))







plot(cityb_l[[4]])


















# check CRS not matching

j=1




inputurbanshape=cityb
lcm=lcm
heatrast=terra::rast(file.path(here::here(),"ancillary_data",paste0("heatdays_",j,".tif")))



# Step 1: rasterise the inputshape
cityr<-terra::rasterize(inputurbanshape,lcm,field="LAU_ID")
# Step 2: align heatraster with lcm and make binary
heatrast<-resample(heatrast,lcm,method="near")
heatrast[heatrast>0]<-1
heatrast[heatrast==0]<-NA
# Step 3: Mask the urban raster by what falls under the heat days category
cityrm<-mask(cityr,heatrast)



crs(cityr) == crs(heatrast)
res(cityr) == res(heatrast)
ext(cityr) == ext(heatrast)
# same resolution, same extent, but different CRS. 


plot(crop(lcm,cityb_buf[1,],mask=T),alpha=0.5,legend=F)
#plot(cityr,alpha=0.9,add=T)
plot(crop(heatrast,cityb_buf[1,],mask=T),alpha=0.8,add=T)
plot(crop(cityrm,cityb_buf[1,]),alpha=0.8,add=T)

# Step 4: Identify city codes that fall into the category
cities <- unique(na.omit(values(cityrm)))
citycodes <- levels(cityr)[[1]]$LAU_ID[levels(cityr)[[1]]$ID %in% cities]
# Step 5: filter city shape accordingly
outputurbanshape <- inputurbanshape[inputurbanshape$LAU_ID %in% citycodes,]
# Step 6: return result and tidy up
rm(cityr,cityrm,cities,citycodes)

























citytownb$LAU_NAME[!citytownb$LAU_NAME %in% unique(c(citytownb_l[[1]]$LAU_NAME,citytownb_l[[2]]$LAU_NAME,citytownb_l[[3]]$LAU_NAME,citytownb_l[[4]]$LAU_NAME))]



length(unique(c(citytownb_l[[1]]$LAU_NAME,citytownb_l[[2]]$LAU_NAME,citytownb_l[[3]]$LAU_NAME,citytownb_l[[4]]$LAU_NAME)))

























# we turn this into a function that returns a modified spatial vector that
# only includes the applicable shapes.




test<-mask_heat_fun(inputurbanshape=cityb,
                    lcm=lcm,
                    heatrast=terra::rast(file.path(here::here(),"ancillary_data",paste0("heatdays_",3,".tif")))
                    )













plot(test)
plot(cityb)
test$LAU_ID
# great, it works

# chetest# check for citytowns

gc()


length(cityb_l[[1]]$LAU_ID)


j=4

hr<-terra::rast(file.path(here::here(),"ancillary_data",paste0("heatdays_",j,".tif")))



plot(cityr,plg = list(ncol = 3))
plot(cityrm,plg = list(ncol = 3))
# i clearly see that some cities are gone. Now we just need the list.




length(levels(cityr)[[1]]$LAU_ID[levels(cityr)[[1]]$ID %in% cities])

# 73 in June
# 71 in July
# 46 in August
# 65 in September


# check for cities
cityr<-terra::rasterize(cityb,lcm,field="LAU_ID")


j=4

test<-terra::rast(file.path(here::here(),"ancillary_data",paste0("heatdays_",j,".tif")))
test<-resample(test,lcm,method="near")
test[test>0]<-1
test[test==0]<-NA

cityrm<-mask(cityr,test)

plot(cityr,plg = list(ncol = 3))
plot(cityrm,plg = list(ncol = 3))
# i clearly see that some cities are gone. Now we just need the list.


cities <- unique(na.omit(values(cityrm)))
levels(cityr)[[1]]$LAU_ID[levels(cityr)[[1]]$ID %in% cities]
length(levels(cityr)[[1]]$LAU_ID[levels(cityr)[[1]]$ID %in% cities])

# 8 in June
# 8 in July
# 5 in August
# 8 in September

cityb_l[[1]]$LAU_ID






