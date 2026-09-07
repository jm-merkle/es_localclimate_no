j=2 # means july


df_sut<-as.data.frame(matrix(ncol=14,nrow=nrow(cityb_buf)))
colnames(df_sut)<-c("LAU",estv,"w_avg_cool")


i=2 # means first city


# the LAU name

r <- mask(
  crop(cooling_stack[[j]], cityb_buf[i, ]),
  cityb_buf[i, ]
)
plot(r)

r <- mask(r, crop(lstr_stack[[i]],cityb_buf[i,]))
plot(r)


lcm_city <- crop(lcm2, r)
r <- mask(r, lcm_city)
plot(r)

global(r, median, na.rm = TRUE)
global(r, mean, na.rm = TRUE)

plot(crop(lstr_stack[[i]],cityb_buf[i,]))






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