


################################################################################
##### try recreate INCA bug ####################################################
################################################################################

# right, so copilot has identified that there might be a massive bug in the INCA script.
# the creation of the cooling rasters might be based on wrong inputs, because the code
# draws from column indeces rather than column names. The column indices indicate that
# the coefficients imposed to generate the cooling rasters are completely wrong. 
# it uses mean tree cover as alpha1h, cooling_mean as beta1h and cooling median as gamma1h.
# then it uses alpha1h as alpha 2h, beta1h as beta2h, and gamma1h as gamma2h.
# see python script line 491 ff.

# We start at 6.2, the creation of cooling rasters

gc()
### We run this as a loop across months and cities. Following INCA's python implementation as close as possible
cooling_list_m <- vector("list", length(mdr))

for(j in 1:length(mdr)){
# make a list for the city-wise cooling rasters
cooling_list <- vector("list", nrow(cityb_buf))
for(i in 1:nrow(cityb_buf)){
  # get coefficients (wrongly)
  alpha1h_ <- firstdf$avg_tcd[firstdf$month==mdr[j] & firstdf$city==cityb_buf$LAU_NAME[i]]
  beta1h_ <- cooldf$average_cooling[cooldf$month==mdr[j] & cooldf$city==cityb_buf$LAU_NAME[i]]
  gamma1h_ <- cooldf$median_cooling[cooldf$month==mdr[j] & cooldf$city==cityb_buf$LAU_NAME[i]]
  # create simulated lst rasters
  lst_sim_green_ <- alpha1h_ + beta1h_*crop(tcdr_no,cityb_buf[i,],mask=T) + gamma1h_*crop(evapr_no,cityb_buf[i,],mask=T)
  lst_sim_gray_ <- alpha1h_ + 0*crop(tcdr_no,cityb_buf[i,],mask=T) + 0*crop(evapr_no,cityb_buf[i,],mask=T)
  # create wrong second regression coefficients
  alpha2h_ <- firstdf$est_intercept[firstdf$month==mdr[j] & firstdf$city==cityb_buf$LAU_NAME[i]]
  beta2h_ <- firstdf$est_tcd[firstdf$month==mdr[j] & firstdf$city==cityb_buf$LAU_NAME[i]]
  gamma2h_ <- firstdf$est_evap[firstdf$month==mdr[j] & firstdf$city==cityb_buf$LAU_NAME[i]]
  # create simulated air temp rasters wrongly
  t_air_green_ <- alpha2h_ + beta2h_*lst_sim_green_ + gamma2h_*crop(lat_r,cityb_buf[i,],mask=T)
  t_air_gray_ <- alpha2h_ + beta2h_*lst_sim_gray_ + gamma2h_*crop(lat_r,cityb_buf[i,],mask=T)
  # cooling
  cooling_ <- t_air_gray_ - t_air_green_
  # as in INCA we force negative values to be zero
  cooling_[cooling_<0] <- 0
  # save in stack
  cooling_list[[i]]<-cooling_
  # tidy up
  rm(alpha1h_,beta1h_,gamma1h_,lst_sim_gray_,lst_sim_green_,t_air_gray_,t_air_green_,cooling_,alpha2h_,beta2h_,gamma2h_)
}
# in this function now we put the cities together and impose the mean cooling for the overlapping boundaries
cooling_list_m[[j]] <- do.call(terra::mosaic,c(cooling_list,fun=mean))
# save
writeRaster(cooling_list_m[[j]],file.path(rfp,paste0("cooling_wrong_",j,".tif")),overwrite=T)
}
cooling_stack_wrong<-terra::rast(cooling_list_m)
names(cooling_stack_wrong)<-mdr
rm(cooling_list_m)


# plot for july
j=2
for(i in 1:nrow(cityb_buf)){
  plot(crop(cooling_stack[[j]], cityb_buf[i,],mask=T),main=paste(cityb_buf$LAU_NAME[i],"cooling","month",mdr[j]), legend=T)
  plot(roads_list[[i]],add=T)
}



# this is INCA cooling for OSLO
plot(crop(inca_cooling,cityb_buf[1,],mask=T))
# here is ours
plot(crop(cooling_stack[[2]], cityb_buf[1,],mask=T), legend=T)
# damn we clearly have found the problem here.... Crazy.
##########################################################
### now make a table with the wrong data
gc()
# now cooling by nuts and ecosystem
# Import Norway NUTs2 shape
n2shp<-terra::vect(file.path(d_fp,"Norway_files","NUTS2021_NO_LVL2_wosvalbard","NUTS2021_NO_LVL2_wos.shp"))
n2shp <- project(n2shp,crs(lcm))
# rasterize the shape
n2r_no <- terra::rasterize(n2shp,mask_stack,field="NUTS_ID")
# then we need lcm, make sure it is aligned with the rest of the raster data
lcm2 <- resample(lcm,mask_stack,method='near')
# now make a loop across months
stl_wrong<-list()
for(j in 1:length(mdr)){
  # Step 1: Make a stack of the relevant data and move it into a dataframe
  astack <- c(
    cooling_stack_wrong[[j]],
    n2r_no,
    lcm2)
  names(astack)<-c("cooling","region","lcm")
  adf<-as.data.frame(astack)
  clean_adf <- adf %>% drop_na()
  # Step 2: Retrieve all the different summary statistics we want
  # means by region and ecosystem type
  mean_by_region_es <- clean_adf %>% 
    group_by(region,lcm) %>% 
    summarise(cooling = mean(cooling))
  mean_by_es <- clean_adf %>% 
    select(-region) %>% 
    group_by(lcm) %>% 
    summarise(cooling = mean(cooling)) %>%
    mutate(region = "NO") %>%
    relocate(region,.before=lcm)
  # total means by NUTS2
  mean_by_region <- clean_adf %>% 
    group_by(region) %>% 
    summarise(cooling = mean(cooling)) %>% 
    mutate(lcm="All") %>%
    relocate(lcm,.after=region)
  # total mean for the whole country NUTS0
  mean_overall <- clean_adf %>% select(cooling) %>% 
    summarise(cooling=mean(cooling)) %>%
    mutate(region = "NO",lcm = "All") %>%
    relocate(cooling,.after =lcm)
  # put together
  mean_vals <- rbind(mean_by_es,mean_overall,mean_by_region_es,mean_by_region) %>% 
    filter(region !="NO09") %>% 
    pivot_wider(names_from=lcm,values_from = cooling) %>%
    left_join(n2shp %>% as.data.frame() %>% select(NUTS_ID,NUTS_NAME),
              by = join_by(region == NUTS_ID)) %>%
    rename(Name = NUTS_NAME) %>%
    relocate(Name,.after=region) %>%
    rename(Region = region)
  mean_vals$Name[mean_vals$Region=="NO"]<-"Norge"
  # put into our list
  stl_wrong[[j]]<-mean_vals
  # Step 3: clean up
  rm(astack,adf,clean_adf,mean_overall,mean_by_region,mean_by_es,mean_by_region_es,mean_vals)
}
names(stl_wrong)<-mdr


test<-stl_wrong[[j]]
### ok so I still do not get the same SUT as INCA, but the cooling raster is now
# more similar. I suspect the next bug with the aggregation across lcm categories

# check totals
sum(values(cooling_stack_wrong[[j]]),na.rm=T)
sum(values(inca_cooling,na.rm=T))
# ok INCA sum is still three times larger than our sum. Look at this by city
j=2

i=1

plot(mask())


plot(crop(cooling_stack_wrong[[j]],cityb_buf[i,],mask=T))
plot(crop(inca_cooling,cityb_buf[i,],mask=T))

ext(inca_cooling)
ext(cooling_stack_wrong)
inca_cooling <- terra::resample(inca_cooling,cooling_stack_wrong)

i=8
plot(crop(inca_cooling,cityb[i,],mask=T) - crop(cooling_stack_wrong[[j]],cityb[i,],mask=T))

## main observation: The cooling rasters are pretty close to each other. 
# So I wonder where the value sum difference comes from. Let's have a look at the overall map

plot(inca_cooling-cooling_stack_wrong[[j]])
# change colour scheme so we can see differences better
sum(values(inca_cooling),na.rm=T)
sum(values(cooling_stack_wrong[[j]]),na.rm=T)
# no actually the sum is quite close now. So the large sum before resampling
# needs to be because some other regions get included?


inca_cooling<-rast(file.path(here::here(),"tmp_inca_cooling.tif"))
plot(inca_cooling)
# right so here we need to dig more. Where do the value differences come from 
# between the resampled cooling map and the original one.


################################################################################

# now cooling by nuts and ecosystem

gc()
# Import Norway NUTs2 shape
n2shp<-terra::vect(file.path(d_fp,"Norway_files","NUTS2021_NO_LVL2_wosvalbard","NUTS2021_NO_LVL2_wos.shp"))
n2shp <- project(n2shp,crs(lcm))

n2shp$NUTS_ID


plot(n2shp)

# rasterize the shape
n2r_no <- terra::rasterize(n2shp,mask_stack,field="NUTS_ID")
plot(n2r_no)

# then we need lcm
lcm2 <- resample(lcm,mask_stack,method='near')
res(lcm2)

# now we put this into a dataframe with lcm

plot(crop(lcm,mask_stack,mask=T))

res(mask_stack)


plot(mask_stack)

j=2

astack <- c(
  cooling_stack[[j]],
  n2r_no,
  lcm2)
names(astack)<-c("cooling","region","lcm")
adf<-as.data.frame(astack)
clean_adf <- adf %>% drop_na()

# all means we want

# means by region and ecosystem type
mean_by_region_es <- clean_adf %>% 
  group_by(region,lcm) %>% 
  summarise(cooling = mean(cooling))
mean_by_es <- clean_adf %>% 
  select(-region) %>% 
  group_by(lcm) %>% 
  summarise(cooling = mean(cooling)) %>%
  mutate(region = "NO") %>%
  relocate(region,.before=lcm)
# total means by NUTS2
mean_by_region <- clean_adf %>% 
  group_by(region) %>% 
  summarise(cooling = mean(cooling)) %>% 
  mutate(lcm="All") %>%
  relocate(lcm,.after=region)
# total mean for the whole country NUTS0
mean_overall <- clean_adf %>% select(cooling) %>% 
  summarise(cooling=mean(cooling)) %>%
  mutate(region = "NO",lcm = "All") %>%
  relocate(cooling,.after =lcm)
# put together
mean_vals <- rbind(mean_by_es,mean_overall,mean_by_region_es,mean_by_region) %>% 
  filter(region !="NO09") %>% 
  pivot_wider(names_from=lcm,values_from = cooling) %>%
  left_join(n2shp %>% as.data.frame() %>% select(NUTS_ID,NUTS_NAME),
            by = join_by(region == NUTS_ID)) %>%
  rename(Name = NUTS_NAME) %>%
  relocate(Name,.after=region) %>%
  rename(Region = region)
mean_vals$Name[mean_vals$Region=="NO"]<-"Norge"
# tidy up
rm(mean_overall,mean_by_region,mean_by_es,mean_by_region_es)



################################################################################
# now a dataframe

supplydf<-as.data.frame()








################################################################################

plot(n2r_no)
plot(cooling_stack[[j]],add=T,legend=F)

# This happens due to the buffer around sandnes! Need to check what INCA does here. 

# And then we need to check whether the mean calculations are based on the whole 
# nuts area, or only pixels we have cooling for, or masked to available LST data.

gc()



# also check pixel count 

# Trondheim etc (NO06)
test<-clean_adf %>% filter(city=="NO06",lcm=="Settlements and other artificial areas")
# Settlements: I get 3168 pixels, while INCA reports 2755
test<-clean_adf %>% filter(city=="NO06",lcm=="Cropland")
# Cropland: I get 2909 pixels, INCA gets 3403
test<-clean_adf %>% filter(city=="NO06",lcm=="Grassland")
# I get 293 pixels, INCA reports 196
test<-clean_adf %>% filter(city=="NO06",lcm=="Forest and woodlands")
# I get 13045 pixels, INCA reports 13691
test<-clean_adf %>% filter(city=="NO06",lcm=="Heathlands and shrub")
# I get 669 pixels, INCA reports 744
test<-clean_adf %>% filter(city=="NO06",lcm=="Sparsely vegetated ecosystems")
# I get 406 pixels, INCA reports 248

# My take is that these pixel counts are rather far off


# and our averages are very far off from INCA results, both its reported supply
# tables and its reported intermediated calculations. So let's start with checking 
# proportions

clean_adf %>% filter(city=="NO06") %>% count(lcm) %>% mutate(prop = n/sum(n))
# right. So these are not entirely the same, but also not too far off. 

# Let's try compute the sum of cooling by es type

clean_adf %>% filter(city=="NO06") %>% select(lcm,cooling) %>% group_by(lcm) %>% summarise(sum_cool = sum(cooling))
# ok here we see the problem. Our sums are like five times smaller than what INCA has. This is what we need to dig further in to.


# what does this look like in oslo? NO08
clean_adf %>% filter(city=="NO08") %>% select(lcm,cooling) %>% group_by(lcm) %>% summarise(sum_cool = sum(cooling))
# even larger difference 

# what does it look like in vestlandet (NO0A)
clean_adf %>% filter(city=="NO0A") %>% select(lcm,cooling) %>% group_by(lcm) %>% summarise(sum_cool = sum(cooling))
# very large difference as well. I wonder what they have done to get those sums.



# check overall sum
sum(clean_adf$cooling)
sum(values(cooling_stack[[j]]),na.rm=T)

global(cooling_stack[[j]], "sum", na.rm = TRUE)


global(evapr_no, "mean", na.rm = TRUE)

global(evapr_no, "mean", na.rm = TRUE)
global(tcdr_no, "mean", na.rm = TRUE)
global(cooling_stack[[j]], "sum", na.rm = TRUE)
global(!is.na(cooling_stack[[j]]), "sum")

inca_cooling<-rast(file.path(here::here(),"tmp_inca_cooling.tif"))

plot(inca_cooling)
res(inca_cooling)
inca_cool_a <- resample(inca_cooling,cooling_stack)

plot(inca_cool_a)
sum(values(inca_cool_a),na.rm=T)

# this is INCA cooling for OSLO
plot(crop(inca_cooling,cityb_buf[1,],mask=T))

# this is the difference to ours¨
plot(crop(cooling_stack[[j]],cityb_buf[1,],mask=T))

# ok this is really crazy... Cooling goes up to 50 degrees in Oslo! What is wrong with these guys.
summary(values(inca_cool_a))

summary(values(cooling_stack[[j]]))

# right, so copilot has identified that there might be a massive bug in the INCA script.
# the creation of the cooling rasters might be based on wrong inputs, because the code
# draws from column indeces rather than column names. The column indices indicate that
# the coefficients imposed to generate the cooling rasters are completely wrong. 
# it uses mean tree cover as alpha1h, cooling_mean as beta1h and cooling median as gamma1h.
# then it uses alpha1h as alpha 2h, beta1h as beta2h, and gamma1h as gamma2h.
# see python script line 491 ff.

# I should try to recreate this mistake, and see if it brings me closer to the INCA reported results.






