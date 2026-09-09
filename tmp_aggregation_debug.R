

# build a new function to compute what we want

gc()

# make a dataframe from firstdf
cooldf<-firstdf %>% select(month,city,city_code,pixels_in_shp,p_pixels_used)
cooldf['T_air_sim']<-NA
cooldf['T_air_zero_sim']<-NA
cooldf['average_cooling']<-NA
cooldf['median_cooling']<-NA

# Project raster to geographic coordinates
tmp_ll <- project(lstr_stack[[1]], "EPSG:4326")
# Create latitude raster
lat_ll <- init(tmp_ll, "y")
# Back to the original grid
lat_r <- project(lat_ll, lstr_stack[[1]])
rm(tmp_ll,lat_ll)


for(j in 1:length(mdr)){
  for(i in 1:nrow(cityb_buf)){
    # Step 1: Move spatial data into dataframe
    estack <- c(
      crop(lstr_stack[[j]], cityb_buf[i,], mask = TRUE),
      crop(tcdr_no, cityb_buf[i,], mask = TRUE),
      crop(evapr_no, cityb_buf[i,], mask = TRUE),
      crop(lat_r, cityb_buf[i,], mask = TRUE))
    names(estack)<-c("lst","tcd","evap","lat")
    cmdf<-as.data.frame(estack) %>% drop_na()
    # Step 2: Fill
    cmdf['alpha1h'] <- firstdf %>% filter(month==mdr[j] & city==cityb_buf$LAU_NAME[i]) %>% select(est_intercept)
    cmdf['beta1h'] <- firstdf %>% filter(month==mdr[j] & city==cityb_buf$LAU_NAME[i]) %>% select(est_tcd)
    cmdf['gamma1h'] <- firstdf %>% filter(month==mdr[j] & city==cityb_buf$LAU_NAME[i]) %>% select(est_evap)
    #cmdf['lst_h'] <-  cmdf$alpha1h + (cmdf$tcd*cmdf$beta1h) + (cmdf$evap*cmdf$gamma1h)
    cmdf['t_air_h'] <- alpha2h + (cmdf$lst*beta2h) + (cmdf$lat*gamma2h)
    cmdf['t_air_zero_h'] <- alpha2h + (cmdf$alpha1h*beta2h) + (cmdf$lat*gamma2h)
    cmdf['cool'] <- cmdf$t_air_zero_h - cmdf$t_air_h
    # Step 3: Move into cooldf
    cooldf$T_air_sim[cooldf$month==mdr[j] & cooldf$city==cityb_buf$LAU_NAME[i]] <- mean(cmdf$t_air_h)
    cooldf$T_air_zero_sim[cooldf$month==mdr[j] & cooldf$city==cityb_buf$LAU_NAME[i]] <- mean(cmdf$t_air_zero_h)
    cooldf$average_cooling[cooldf$month==mdr[j] & cooldf$city==cityb_buf$LAU_NAME[i]] <- mean(cmdf$cool)
    cooldf$median_cooling[cooldf$month==mdr[j] & cooldf$city==cityb_buf$LAU_NAME[i]] <- median(cmdf$cool)
    # Step 4: Clean up
    rm(estack,cmdf)
  }
}













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