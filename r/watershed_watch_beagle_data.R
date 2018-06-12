library(tidyverse)
library(here)
options(stringsAsFactors = FALSE)

#get cmc ww data
cmc<-read.csv(here('data/ww_cmc2016.csv')) %>%
  separate(station_id, c('station_id', 'site'), '_') %>%
  select(station_id, site, date=sample_date, time=sample_time, dilution, chla_ugl)
    
  cmc$date<-as.Date(cmc$date)
  
  #multiply the chla conc by the dilution factor.  Is this correct?  
    cmc$chla_cmc<-cmc$chla_ugl * as.numeric(substr(cmc$dilution, 4, 4))
    
  #get mean chla by station_id, site, and date
    a<-group_by(cmc, station_id, site, date)
    cmc_mean<-summarise(a, chla_cmc = mean(chla_cmc))
      
#get ww data
ww<-read.csv(here('data/ww_all.csv'))

#filter- paramter=='chla'; sample.type=='Grab'; depth>=3m
  ww<-filter(ww, Parameter == "Chlorophyll a, water, fluorometric method, corrected for pheophytin - 32209" & 
           Sample.Type == 'Grab' & 
           Depth <=3) 
  
  # Note: some ww$chla units in mg/l but the values look more like ug/l; keep them as is for now.
  table(ww$Unit)
  filter(ww, Unit=='mg/l')  
  
  # select fields to keep
  ww<-select(ww, station_id=Station.Name, date=Date, chla=Concentration)
  
  #format date
  ww$date<-as.Date(ww$date)
  
#get mean chla by station_id and date
  a<-group_by(ww, station_id, date)
  ww_mean<-summarise(a, chla_ww = mean(chla))
  
    #join ww & cmc
all<-left_join(cmc_mean, ww_mean)

  #remove missing values
    all<-filter(all, !is.na(date) & !is.na(chla_ww))
    
plot(all$chla_ww, all$chla_cmc)

jpeg(here::here('output/temp.jpg'))
ggplot(all, aes(chla_ww, chla_cmc)) +
  geom_point(colour='blue') + coord_fixed() +
  xlab("Lab") + ylab("Beagle") +
  ggtitle('2016 Chla ug/l')+ 
  annotate(geom="text", x = 52, y = 3, label = 'watershed_watch_beagle_data.r')
dev.off()
