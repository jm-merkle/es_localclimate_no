# process nordic gridded maximum temperature data from copernicus

dates <- format(
  seq(as.Date("2024-06-01"), as.Date("2024-09-30"), by = "day"),
  "%Y%m%d"
)

# loop across days
for(q in 1:length(dates)){
  # to trace
  print(dates[q])
  # read file
  tmpr<-terra::rast(file.path(d_fp,"..","COPERNICUS","nordic_max_temp_2024","raw",paste0("NGCD_TX_type1_version_26.03_",dates[q],".nc")))-273.15
  # reformat
  tmpr<-terra::resample(tmpr,lcm)
  tmpr<-terra::crop(tmpr,lcm,mask=T)
  tmpr<-terra::crop(tmpr,citytownb_buf,mask=T)
  tmpr[tmpr<25]<-NA
  # save if hot
  if(max(values(tmpr),na.rm=T)>=25){
    terra::writeRaster(tmpr,file.path(here::here(),"ancillary_data","maxairtemp",paste0("maxairtemp_citytown_no_",dates[q],".tif")),overwrite=T)
  } else{}
  # clean
  rm(tmpr)
  gc()
}

# now we create one aggregated map for each month
# each pixel shows the number of days on which temperatures exceeded 25 deg.
alldaysfiles<-list.files(file.path(here::here(),"ancillary_data","maxairtemp"))
mdr<-c(6,7,8,9)

hl<-list()
for(j in 1:length(mdr)){
  print(mdr[j])
  mapindx<-grep(paste0("20240",mdr[j]),alldaysfiles)
  ml<-list()
  for(i in 1:length(mapindx)){
    # here we force all pixels exceeding 25 degrees to 1, all others to 0
    ml[[i]]<-terra::rast(file.path(here::here(),"ancillary_data","maxairtemp",alldaysfiles[mapindx[i]]))
    ml[[i]][ml[[i]]>0]<-1
    ml[[i]][is.na(ml[[i]])]<-0
  }
  # make a stack from list
  hl[[j]]<-terra::rast(ml)
  # sum across layers to create a single raster
  hl[[j]]<-sum(hl[[j]])
  rm(ml,mapindx)
}
names(hl)<-mdr

# then remove the daily files and only save the monthly ones
for(j in 1:length(mdr)){
  hl[[j]]<-sum(hl[[j]])
}

# write result
for(j in 1:length(mdr)){
  writeRaster(hl[[j]],file.path(here::here(),"ancillary_data",paste0("heatdays_",j,".tif")),overwrite=T)
}

# remove the individual day data
unlink(file.path(here::here(),"ancillary_data","maxairtemp"),recursive=T)


