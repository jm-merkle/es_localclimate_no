# test what happens if we use norwegian national weather data

tfp<-file.path(here::here(),"ancillary_data","norway_training_data_2021")

test<-read.csv(file.path(tfp,"flateogluft CSV.csv"))

test <- test # %>% filter(max.air_temperature.PT1H. > 0 )

# right now check the american weather station data again
length(unique(wstd$StationFID))
# so that means it is one observation per station

# impose the same for our norwegian data
length(unique(test$sourceId))
# we only have 29 observations - that's not a lot. 

test <- test %>% 
  group_by(sourceId) %>% 
  slice_max(`max.air_temperature.PT1H.`, n = 1, with_ties = FALSE) %>%
  ungroup()

alpha2hn<-lm(`max.air_temperature.PT1H.` ~ surface_temperature + lat, data = test %>% filter(`max.air_temperature.PT1H.` > 0))$coefficients['(Intercept)']
beta2hn<-lm(`max.air_temperature.PT1H.` ~ surface_temperature + lat, data = test %>% filter(`max.air_temperature.PT1H.` > 0))$coefficients['surface_temperature']
gamma2hn<-lm(`max.air_temperature.PT1H.` ~ surface_temperature + lat, data = test %>% filter(`max.air_temperature.PT1H.` > 0))$coefficients['lat']

# if we used these estimated coefficients (the only relevant one is beta2hn), it would roughly double the magnitudes of our cooling results.

### alternative with a fixed effect for time

tfp<-file.path(here::here(),"ancillary_data","norway_training_data_2021")

test<-read.csv(file.path(tfp,"flateogluft CSV.csv"))

library(fixest)

colnames(test)
fixest::feols(`max.air_temperature.PT1H.` ~ surface_temperature + lat | referenceTime, 
              cluster= ~sourceId,data=test %>% filter(`max.air_temperature.PT1H.` >0))

# so from this kind of estimation, we would get an even larger beta2hn scale factor... Leave this for now. It is not quality checked.


colnames(test)




