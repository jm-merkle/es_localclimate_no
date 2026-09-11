

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




























#################################################################################

alpha1hr_7 <- terra::rasterize(cityb_buf_7,mask_stack[[j]],field="est_intercept",fun=mean)
beta1hr_7 <- terra::rasterize(cityb_buf_7,mask_stack[[j]],field="est_tcd",fun=mean)
gamma1hr_7 <- terra::rasterize(cityb_buf_7,mask_stack[[j]],field="est_evap",fun=mean)

lst_sim_green_7 <- alpha1hr_7 + beta1hr_7*tcdr_no + gamma1hr_7*evapr_no
lst_sim_gray_7 <- alpha1hr_7 

t_air_green_7 <- alpha2h + beta2h*lst_sim_green_7 + gamma2h*lat_r
t_air_gray_7 <- alpha2h + beta2h*lst_sim_gray_7 + gamma2h*lat_r

cooling_7 <- t_air_gray_7 - t_air_green_7
plot(cooling_7)
cooling_nn_7 <- cooling_7
cooling_nn_7[cooling_nn_7<0]<-0

plot(cooling_nn_7)

plot(crop(cooling_nn_7,cityb[8,],mask=T))

################################################################################
j=2

#first we create a latitude raster
# Project raster to geographic coordinates
tmp_ll <- project(lstr_stack[[1]], "EPSG:4326")
# Create latitude raster
lat_ll <- init(tmp_ll, "y")
# Back to the original grid
lat_r <- project(lat_ll, lstr_stack[[1]])
rm(tmp_ll,lat_ll)

# estimate lst with vegetation
lstvr <- intercept_stack_m[[j]] + beta1_stack_m[[j]]*tcdr_no + gamma1_stack_m[[j]]*evapr_no
plot(lstvr)

# then air temperature with vegetation
tairmv <- alpha2h + beta2h*lstvr + gamma2h*lat_r
# and without vegetation
tairuv <- alpha2h + beta2h*intercept_stack_m[[j]] + gamma2h*lat_r
plot(tairmv)
plot(tairuv)

i=7
#air temp
plot(crop(tairmv,cityb_buf[i,],mask=T))
plot(crop(tairuv,cityb_buf[i,],mask=T))

# look at inputs to tairuv
plot(crop(intercept_stack_m[[j]],cityb_buf[i,],mask=T))
plot(crop(lat_r,cityb_buf[i,],mask=T))


mean(values(crop(tairmv,cityb_buf[i,],mask=T),na.rm=T))
mean(values(crop(tairuv,cityb_buf[i,],mask=T),na.rm=T))

mean(values(crop(tairuv,cityb_buf[i,],mask=T),na.rm=T)) - mean(values(crop(tairmv,cityb_buf[i,],mask=T),na.rm=T))

#lst
plot(crop(lstvr,cityb_buf[i,],mask=T))
mean(values(crop(lstvr,cityb_buf[i,],mask=T),na.rm=T))









plot(cooling_stack[[2]])


i=1
plot(crop(cooling_stack[[2]],cityb_buf[i,],mask=T))
mean(values(crop(cooling_stack[[2]],cityb_buf[i,],mask=T),na.rm=T))
median(values(crop(cooling_stack[[2]],cityb_buf[i,],mask=T),na.rm=T))

plot(crop(cooling_stack_um[[2]],cityb_buf[i,],mask=T))
mean(values(crop(cooling_stack_um[[2]],cityb_buf[i,],mask=T),na.rm=T))
median(values(crop(cooling_stack_um[[2]],cityb_buf[i,],mask=T),na.rm=T))

plot(crop(tcdr_no,cityb_buf[i,],mask=T))
plot(crop(evapr_no,cityb_buf[i,],mask=T))




j=2 # means july


df_sut<-as.data.frame(matrix(ncol=14,nrow=nrow(cityb_buf)))
colnames(df_sut)<-c("LAU",estv,"w_avg_cool")


i=8 # means first city


# the LAU name

r <- mask(
  crop(cooling_stack[[j]], cityb_buf[i, ]),
  cityb_buf[i, ]
)
plot(r)


lcm_city <- crop(lcm2, r)
r <- mask(r, lcm_city)
plot(r)

global(r, median, na.rm = TRUE)
global(r, mean, na.rm = TRUE)

plot(crop(lstr_stack[[i]],cityb_buf[i,]))

# we are not exactly alike INCA, but close. I wonder whether this is related to 
# that they have masked out some of the missing inputs, but not all?

# test whether it makes a difference to apply the non-simplified calculation.
# done. it does not make a difference.



# check that averages are based on the masked data


















df_sut$LAU[i]<-as.data.frame(cityb_buf[i,])$LAU_NAME
# compute cooling averages
# using terra zonal
z <- terra::zonal(mask(crop(cooling_stack[[j]], cityb_buf[i,]),cityb_buf[i,]), # values
                  mask(crop(lcm2, cityb_buf[i,]),cityb_buf[i,]),# categories
                  fun="mean",
                  na.rm=T)

r <- mask(
  crop(cooling_stack[[j]], cityb_buf[i, ]),
  cityb_buf[i, ]
)
lcm_city <- crop(lcm2, r)

r <- mask(r, lcm_city)

global(r, median, na.rm = TRUE)
global(r, mean, na.rm = TRUE)
plot(r)

# right so here I get a value of -2.109 as a mean, 
# while I got -2.16 as a weighted mean in the table. (which is what we see as INCA result, too)
# how is that possible? Need to do more digging, to find out how our code differs from INCA here.

## adjusted version
cool <- mask(crop(cooling_stack[[j]], cityb_buf[i,]), cityb_buf[i,])
lcm_city <- mask(crop(lcm2, cityb_buf[i,]), cityb_buf[i,])
cool_lcm <- mask(lcm_city, cool)
# set column names
colnames(z) <- c("es_id","cooling")
# add ecosystem names
z$es_name <- lcm_idf$es_type[
  match(z$es_id, lcm_idf$es_id)]
# here is the weighted cooling across all ecosystems
dfp<-freq(cool_lcm)
dfp['es_weight']<-dfp$count / dfp %>% select(count) %>% sum()
z <- z %>% left_join(dfp,by=join_by("es_id" == "value"))
z['weighted_average_cooling']<-z$cooling*z$es_weight
sum(z$weighted_average_cooling)
# Ok, what I notice here is that the weighted average that we get out here IS NOT the same as what we get out from our direct raster operations using the global function...


cool <- mask(crop(cooling_stack[[j]], cityb_buf[i,]), cityb_buf[i,])
lcm_city <- mask(crop(lcm2, cityb_buf[i,]), cityb_buf[i,])

table(
  is.na(values(cool)),
  is.na(values(lcm_city))
)




rm(dfp)
# paste result into our table, loop again, just to ensure we keep empty values where ecosystems do not exist
for(k in 1:length(estv)){
  df_sut[i,k+1]<- {
    x <- z %>% filter(es_name==estv[k]) %>% pull(cooling)
    if(length(x) == 0) NA else x
  }
  rm(x)
}
# finally the weighted average for the LAU as a whole
df_sut$w_avg_cool[i]<-sum(z$weighted_average_cooling)
rm(z)



# now loop across LAU
for(i in 1:nrow(cityb_buf)){
  # the LAU name
  df_sut$LAU[i]<-as.data.frame(cityb_buf[i,])$LAU_NAME
  # compute cooling averages
  # using terra zonal
  z <- terra::zonal(mask(crop(cooling_stack[[j]], cityb_buf[i,]),cityb_buf[i,]), # values
                    mask(crop(lcm2, cityb_buf[i,]),cityb_buf[i,]),# categories
                    fun="mean",
                    na.rm=T)
  # set column names
  colnames(z) <- c("es_id","cooling")
  # add ecosystem names
  z$es_name <- lcm_idf$es_type[
    match(z$es_id, lcm_idf$es_id)]
  # here is the weighted cooling across all ecosystems
  dfp<-freq(mask(crop(lcm2, cityb_buf[i,]),cityb_buf[i,]))
  dfp['es_weight']<-dfp$count / freq(mask(crop(lcm2, cityb_buf[i,]),cityb_buf[i,])) %>% select(count) %>% sum()
  z <- z %>% left_join(dfp,by=join_by("es_id" == "value"))
  z['weighted_average_cooling']<-z$cooling*z$es_weight
  rm(dfp)
  # paste result into our table, loop again, just to ensure we keep empty values where ecosystems do not exist
  for(k in 1:length(estv)){
    df_sut[i,k+1]<- {
      x <- z %>% filter(es_name==estv[k]) %>% pull(cooling)
      if(length(x) == 0) NA else x
    }
    rm(x)
  }
  # finally the weighted average for the LAU as a whole
  df_sut$w_avg_cool[i]<-sum(z$weighted_average_cooling)
  rm(z)
}