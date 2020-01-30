library(tidyverse)
library(mapview)
library(sf)
library(ggmap)
library(lubridate)
#Set your API Key
#ggmap::register_google(key = "YOUR_KEY", write = TRUE)
solar_a <- read.csv("SolarAttributes.csv", stringsAsFactors = F)
non_solar_a <- read.csv("NonSolarAttributes.csv", stringsAsFactors = F)
#plot on map

region <- solar_a$LGA
reg <- as.data.frame(region)
reg$region <- as.character(reg$region)
summary(reg)
#reg.a
reg.a <- unique(reg$region)
reg.a <- as.data.frame(reg.a)
reg.a$reg.a <- as.character(reg.a$reg.a)
summary(reg.a)
enc <- mutate_geocode(reg.a, reg.a)
#manually set wrong conflicting co-ordinates
enc$lon[29] = 151.1132887
enc$lat[29] = -32.602355
enc$lon[10] = 151.1170149
enc$lat[10] = -33.9130231
enc$lon[14] = 151.7812534
enc$lat[14] = -32.9272881
enc$lon[35] = 151.136028
enc$lat[35] = -33.8298693
enc$lon[25] = 151.1046184
enc$lat[25] = -33.8015751
enc$lon[1] = 151.0993
enc$lat[1] = -33.7024
enc$lon[32] = 151.0255673
enc$lat[32] = -33.8545702
enc$lon[24] = 151.5268437
enc$lat[24] = -32.708177
enc$lon[19] = 151.1024811
enc$lat[19] = -33.8858088
enc$lon[38] = 151.1836516
enc$lat[38] = -33.8015957
enc$lon[7] = 151.0492718
enc$lat[7] = -34.03333
enc$lon[33] = 151.1274125
enc$lat[33] = -33.8894781
enc$lon[8] = 151.0512
enc$lat[8] = -31.9235
enc$lon[9] = 151.1403655
enc$lat[9] = -33.9542619


colnames(enc)
enc
write.csv(enc, "encrypt.csv")
#enc.t <- as_tibble(enc)
#enc <- read.csv("encrypt.csv")
enc_c <- st_as_sf(enc, coords = c("lon", "lat"), crs=4326)
mapview(enc_c)

b<-geocode("Wisemans Ferry")

bw_map <- get_googlemap(center = c(151.18500,  -33.02657), zoom = 8)

p <- ggmap(bw_map)+geom_point(data =enc, aes(x=lon, y=lat, color=reg.a), size=3)+labs(color="LGA")+
  theme(legend.title = element_text(size = 9), legend.text = element_text(size = 5.5), 
        legend.key = element_rect(colour = "transparent", fill = NULL), legend.position = c(0.95, 0.45))
#geom_point(aes(x=as.numeric(l$lon) , y= as.numeric(l$lat) ))

#p <- ggmap(bw_map)+geom_point(data =enc, aes(x=lon, y=lat, color=reg.a), size=3)+theme(legend.position = "none")

p




solar_a$LGA <- as.factor(solar_a$LGA)
non_solar_a$LGA <- as.factor(non_solar_a$LGA)
str(solar_a)
summary(solar_a)

View(head(solar_a))


a <- unique(solar_a$LGA)

#sydney
s<- which(solar_a$LGA == "CANADA BAY" | solar_a$LGA == "CANTERBURY" 
          | solar_a$LGA == "HORNSBY" | solar_a$LGA == "BURWOOD"
          | solar_a$LGA == "HUNTERS HILL" | solar_a$LGA == "NORTH SYDNEY" 
          | solar_a$LGA == "MOSMAN" | solar_a$LGA == "STRATHFIELD"
          | solar_a$LGA == "WILLOUGHBY" | solar_a$LGA == "WOOLLAHRA" 
          | solar_a$LGA == "SYDNEY" | solar_a$LGA == "RYDE"
          | solar_a$LGA == "MANLY"
          | solar_a$LGA == "RANDWICK" | solar_a$LGA == "LANE COVE" 
          | solar_a$LGA == "WARRINGAH" | solar_a$LGA == "PITTWATER" 
          | solar_a$LGA == "KU-RING-GAI" | solar_a$LGA == "RYDE")

#hunter region
h <- which(solar_a$LGA == "LAKE MACQUARIE" | solar_a$LGA == "NEWCASTLE" 
          | solar_a$LGA == "MAITLAND" | solar_a$LGA == "CESSNOCK"
          | solar_a$LGA == "PORT STEPHENS" | solar_a$LGA == "SINGLETON" | solar_a$LGA == "WAVERLEY"
          | solar_a$LGA == "MUSWELLBROOK" | solar_a$LGA == "UPPER HUNTER")


#central coast region
k <- which(solar_a$LGA == "GOSFORD" | solar_a$LGA == "WYONG")
#southern regions
o <- -c(s,h,k)

solar_a$LGA <- as.character(solar_a$LGA)
z<-count(solar_a[s,], LGA)

y<-count(solar_a[h,], LGA)

x<-count(solar_a[k,], LGA)

w<-count(solar_a[o,], LGA)

write.csv(z, "z.csv" )
write.csv(y, "y.csv" )
write.csv(x, "x.csv" )
write.csv(w, "w.csv" )


ns<- which(non_solar_a$LGA == "CANADA BAY" | non_solar_a$LGA == "CANTERBURY" 
          | non_solar_a$LGA == "HORNSBY" | non_solar_a$LGA == "BURWOOD"
          | non_solar_a$LGA == "HUNTERS HILL" | non_solar_a$LGA == "NORTH SYDNEY" 
          | non_solar_a$LGA == "MOSMAN" | non_solar_a$LGA == "STRATHFIELD"
          | non_solar_a$LGA == "WILLOUGHBY" | non_solar_a$LGA == "WOOLLAHRA" 
          | non_solar_a$LGA == "SYDNEY" | non_solar_a$LGA == "RYDE"
          | non_solar_a$LGA == "MANLY"
          | non_solar_a$LGA == "RANDWICK" | non_solar_a$LGA == "LANE COVE" 
          | non_solar_a$LGA == "WARRINGAH" | non_solar_a$LGA == "PITTWATER" 
          | non_solar_a$LGA == "KU-RING-GAI" | non_solar_a$LGA == "RYDE")

#hunter region
nh <- which(non_solar_a$LGA == "LAKE MACQUARIE" | non_solar_a$LGA == "NEWCASTLE" 
           | non_solar_a$LGA == "MAITLAND" | non_solar_a$LGA == "CESSNOCK"
           | non_solar_a$LGA == "PORT STEPHENS" | non_solar_a$LGA == "SINGLETON" | non_solar_a$LGA == "WAVERLEY"
           | non_solar_a$LGA == "MUSWELLBROOK" | non_solar_a$LGA == "UPPER HUNTER")


#central coast region
nk <- which(non_solar_a$LGA == "GOSFORD" | non_solar_a$LGA == "WYONG")
#southern regions
no <- -c(ns,nh,nk)

non_solar_a$LGA <- as.character(non_solar_a$LGA)
nz<-count(non_solar_a[ns,], LGA)

ny<-count(non_solar_a[nh,], LGA)

nx<-count(non_solar_a[nk,], LGA)

nw<-count(non_solar_a[no,], LGA)

write.csv(nz, "nz.csv" )
write.csv(ny, "ny.csv" )
write.csv(nx, "nx.csv" )
write.csv(nw, "nw.csv" )
summary(solar_a)



solar_a$a <- rep("", nrow(solar_a))

solar_a$a <- ifelse(solar_a$Generator.Capacity <= 1, "0.1-1.0kWp", 
                    ifelse(solar_a$Generator.Capacity <= 2, "1.1-2.0kWp",
                           ifelse(solar_a$Generator.Capacity <= 3, "2.1-3.0kWp", 
                                  ifelse(solar_a$Generator.Capacity <= 4, "3.1-4.0kWp", 
                                         ifelse(solar_a$Generator.Capacity <= 5, "4.1-5.0kWp", 
                                                ifelse(solar_a$Generator.Capacity <= 6, "5.1-6.0kWp", 
                                                       ifelse(solar_a$Generator.Capacity <= 7, "6.1-7.0kWp", 
                                                              ifelse(solar_a$Generator.Capacity <= 8, "7.1-8.0kWp", 
                                                                     ifelse(solar_a$Generator.Capacity <= 9, "8.1-9.0kWp", 
                                                                            ifelse(solar_a$Generator.Capacity <= 10, "9.1-10.0kWp" ))))))))))

solar_a$a <- as.factor(solar_a$a)
#levels(solar_a$a)  <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#solar_a$a[which(solar_a$Generator.Capacity <= 1)] = 1 


ggplot(solar_a)+geom_bar(aes(x=a, y = (..count..)/sum(..count..), fill=a)) + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Solar PV Generator Capacity") +
  ylab("Frequency") + labs(fill = "Generator Capacity (kWp)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

