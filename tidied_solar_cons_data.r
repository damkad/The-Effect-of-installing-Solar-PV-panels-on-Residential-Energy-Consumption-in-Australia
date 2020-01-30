library("tidyverse")
library("lubridate")

list.files()
solar_data <- read.csv("SolarMonthlyData_2657Custs.csv", stringsAsFactors = T)
summary(solar_data)
View(head(solar_data))
#unique(solar_data$Netwk.Bill.Rate.Type)

#m <- solar_data %>% subset(solar_data$Customer.ID == 2 & solar_data$Consumption.Month==11.2009)  
#unique(m$Netwk.Bill.Rate.Type)

x.sc <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "SC")  

x.sc.day <- x.sc %>% subset(x.sc$Unit.of.me == "DAY")
x.sc.kwh <- x.sc %>% subset(x.sc$Unit.of.me == "KWH")
x.sc.join <- left_join(x.sc.day, x.sc.kwh, by = c("Customer.ID"="Customer.ID", "Netwk.Bill.Rate.Type"="Netwk.Bill.Rate.Type", "Consumption.Month"="Consumption.Month"))
x.sc.join <- x.sc.join[,-c(5,7)] %>% rename(Day=Sum.x, KWH= Sum.y) 
#x.sc.join <- x.sc.join %>% subset(Day > 27 & Day < 31)
summary(x.sc.join)
unique(x.sc.join$Customer.ID)
length(unique(x.sc.join$Customer.ID)) 

x.pk <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "PK")  
x.pk.day <- x.pk %>% subset(x.pk$Unit.of.me == "DAY")
x.pk.kwh <- x.pk %>% subset(x.pk$Unit.of.me == "KWH")
x.pk.join <- left_join(x.pk.day, x.pk.kwh, by = c("Customer.ID"="Customer.ID", "Netwk.Bill.Rate.Type"="Netwk.Bill.Rate.Type", "Consumption.Month"="Consumption.Month"))
x.pk.join <- x.pk.join[,-c(5,7)] %>% rename(Day=Sum.x, KWH= Sum.y)
#x.pk.join <- x.pk.join %>% subset(Day > 27 & Day <31)

summary(x.pk.join)
length(unique(x.pk.join$Customer.ID)) 

x.op2 <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "OP2")  
x.op2 <- x.op2[,-5] %>% rename(KWH=Sum)
summary(x.op2)
length(unique(x.op2$Customer.ID)) 

x.op1 <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "OP1")  
x.op1 <- x.op1[,-5] %>% rename(KWH=Sum)
summary(x.op1)
length(unique(x.op1$Customer.ID)) 

x.op <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "OP")  
x.op <- x.op[,-5] %>% rename(KWH=Sum)
summary(x.op)
length(unique(x.op$Customer.ID)) 

#SC  PK  OP1 OGR SH  SGR OP  PGR OP2 LVP LVS PGG OGG SGG

x.ogr <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "OGR")  
x.ogr <- x.ogr[,-5] %>% rename(KWH=Sum)
summary(x.ogr)
length(unique(x.ogr$Customer.ID)) 

x.sh <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "SH")  
x.sh <- x.sh[,-5] %>% rename(KWH=Sum)
summary(x.sh)
length(unique(x.sh$Customer.ID)) 

x.sgr <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "SGR")  
x.sgr <- x.sgr[,-5] %>% rename(KWH=Sum)
summary(x.sgr)
length(unique(x.sgr$Customer.ID)) 


x.pgr <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "PGR")  
x.pgr <- x.pgr[,-5] %>% rename(KWH=Sum)
summary(x.pgr)
length(unique(x.pgr$Customer.ID)) 


x.lvp <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "LVP")  
x.lvp.day <- x.lvp %>% subset(x.lvp$Unit.of.me == "DAY")
x.lvp.kwh <- x.lvp %>% subset(x.lvp$Unit.of.me == "KWH")
x.lvp.join <- left_join(x.lvp.day, x.lvp.kwh, by = c("Customer.ID"="Customer.ID", "Netwk.Bill.Rate.Type"="Netwk.Bill.Rate.Type", "Consumption.Month"="Consumption.Month"))
x.lvp.join <- x.lvp.join[,-c(5,7)] %>% rename(Day=Sum.x, KWH= Sum.y)
#x.lvp.join <- x.lvp.join %>% subset(Day > 27 & Day <31)

summary(x.lvp.join)
length(unique(x.lvp.join$Customer.ID)) 


x.lvs <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "LVS")  
x.lvs <- x.lvs[,-5] %>% rename(KWH=Sum)
summary(x.lvs)
length(unique(x.lvs$Customer.ID)) 

x.pgg <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "PGG")  
x.pgg <- x.pgg[,-5] %>% rename(KWH=Sum)
summary(x.pgg)
length(unique(x.pgg$Customer.ID)) 

x.ogg <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "OGG")  
x.ogg <- x.ogg[,-5] %>% rename(KWH=Sum)
summary(x.ogg)
length(unique(x.ogg$Customer.ID)) 

x.sgg <- solar_data %>% subset(solar_data$Netwk.Bill.Rate.Type == "SGG")  
x.sgg <- x.sgg[,-5] %>% rename(KWH=Sum)
summary(x.sgg)
length(unique(x.sgg$Customer.ID)) 




#for blocked tariff
block <- left_join(x.sc.join, x.op2, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
blocka <- left_join(block, x.op1, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))

colnames(blocka)

i <- which(is.na(blocka$KWH.y))
blocka$KWH.y[i]=0

j <- which(is.na(blocka$KWH))
blocka$KWH[j]=0

blocka$Cl <- blocka$KWH.y + blocka$KWH


blocka$ClType <- paste(blocka$Netwk.Bill.Rate.Type.y, blocka$Netwk.Bill.Rate.Type)
blocka$ClType <- str_replace_all(blocka$ClType, "NA", "")
blocka$ClType <- as.factor(blocka$ClType)
blocka <- blocka %>% rename(cons = KWH.x, ntwk = Netwk.Bill.Rate.Type.x)
blocka <- blocka[, -c(6:9)]


#using the time of the year tariff - Peak (PK) + Shoulder (SH) + 
#Off peak (OP) + Control load (OP1/ OP2/ OP1 OP2/ --)

tou <- left_join(x.pk.join, x.sh, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
tou <- rename(tou,  PK = KWH.x, SHD = KWH.y)
tou.a <- left_join(tou, x.op, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
#length(unique(tou.a$Customer.ID))
tou.a <- rename(tou.a,  OP = KWH)
tou.b <- left_join(tou.a, x.op1, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
tou.b <- rename(tou.b,  Cl1 = KWH)
tou.c <- left_join(tou.b, x.op2, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
tou.c <- rename(tou.c,  Cl2 = KWH)
colnames(tou.c)
i <- which(is.na(tou.c$PK))
tou.c$PK[i]=0

ia <- which(is.na(tou.c$SHD))
tou.c$SHD[ia]=0

ib <- which(is.na(tou.c$OP))
tou.c$OP[ib]=0

ic <- which(is.na(tou.c$Cl1))
tou.c$Cl1[ic]=0

id <- which(is.na(tou.c$Cl2))
tou.c$Cl2[id]=0

#tou.c <- tou.c %>% rename( KWH.x =cons )

tou.c$cons <- tou.c$PK + tou.c$SHD + tou.c$OP

tou.c$Cl <- tou.c$Cl1 + tou.c$Cl2


tou.c$ClType <- paste(tou.c$Netwk.Bill.Rate.Type.y.y, tou.c$Netwk.Bill.Rate.Type)
tou.c$ClType <- str_replace_all(tou.c$ClType, "NA", "")
tou.c$ClType<- as.factor(tou.c$ClType)

tou.c$ntwk <- paste(tou.c$Netwk.Bill.Rate.Type.x, tou.c$Netwk.Bill.Rate.Type.y, tou.c$Netwk.Bill.Rate.Type.x.x)
tou.c$ntwk <- str_replace_all(tou.c$ntwk, "NA", "")
tou.c$ntwk<- as.factor(tou.c$ntwk)
tou.c <- tou.c[, c(1, 17, 3, 4, 14, 15, 16)]
summary(tou.c)





#using the time of the year tariff-2 - Peak (LVP) + Shoulder (LVS) + 
#Off peak (OP) + Control load (OP1/ OP2/ OP1 OP2/ --)

tou2 <- left_join(x.lvp.join, x.lvs, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
tou2 <- rename(tou2,  PK = KWH.x, SHD = KWH.y)
tou.a2 <- left_join(tou2, x.op, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
#length(unique(tou.a$Customer.ID))
tou.a2 <- rename(tou.a2,  OP = KWH)
tou.b2 <- left_join(tou.a2, x.op1, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
tou.b2 <- rename(tou.b2,  Cl1 = KWH)
tou.c2 <- left_join(tou.b2, x.op2, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
tou.c2 <- rename(tou.c2,  Cl2 = KWH)
colnames(tou.c2)

i2 <- which(is.na(tou.c2$PK))
tou.c2$PK[i2]=0

ia2 <- which(is.na(tou.c2$SHD))
tou.c2$SHD[ia2]=0

ib2 <- which(is.na(tou.c2$OP))
tou.c2$OP[ib2]=0

ic2 <- which(is.na(tou.c2$Cl1))
tou.c2$Cl1[ic2]=0

id2 <- which(is.na(tou.c2$Cl2))
tou.c2$Cl2[id2]=0

#tou.c <- tou.c %>% rename( KWH.x =cons )

tou.c2$cons <- tou.c2$PK + tou.c2$SHD + tou.c2$OP

tou.c2$Cl <- tou.c2$Cl1 + tou.c2$Cl2


tou.c2$ClType <- paste(tou.c2$Netwk.Bill.Rate.Type.y.y, tou.c2$Netwk.Bill.Rate.Type)
tou.c2$ClType <- str_replace_all(tou.c2$ClType, "NA", "")
tou.c2$ClType<- as.factor(tou.c2$ClType)

tou.c2$ntwk <- paste(tou.c2$Netwk.Bill.Rate.Type.x, tou.c2$Netwk.Bill.Rate.Type.y, tou.c2$Netwk.Bill.Rate.Type.x.x)
tou.c2$ntwk <- str_replace_all(tou.c2$ntwk, "NA", "")
tou.c2$ntwk<- as.factor(tou.c2$ntwk)
#rock fortress deliver shield in whom i take refuge
tou.c2 <- tou.c2[, c(1, 17, 3, 4, 14, 15, 16)]
summary(tou.c2)

#vertically combine the dataset
consumption_data_sv <- rbind(blocka, tou.c, tou.c2)
summary(consumption_data_sv)

#include the gross energy consumption
fab <- left_join(consumption_data_sv, x.ogg, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
fab <- fab %>% rename(ogg = KWH, ntwk_ogg = Netwk.Bill.Rate.Type)
fab <- left_join(fab, x.pgr, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
fab<- fab %>% rename(pgr = KWH, ntwk_pgr = Netwk.Bill.Rate.Type)
fab <- left_join(fab, x.sgr, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
fab<- fab %>% rename(sgr = KWH, ntwk_sgr = Netwk.Bill.Rate.Type)
fab <- left_join(fab, x.ogr, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
fab<- fab %>% rename(ogr = KWH, ntwk_ogr = Netwk.Bill.Rate.Type)
fab <- left_join(fab, x.pgg, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
fab<- fab %>% rename(pgg = KWH, ntwk_pgg = Netwk.Bill.Rate.Type)
fab <- left_join(fab, x.sgg, by=c("Customer.ID"="Customer.ID", "Consumption.Month"="Consumption.Month"))
fab<- fab %>% rename(sgg = KWH, ntwk_sgg = Netwk.Bill.Rate.Type)


colnames(fab)
f1 <- which(is.na(fab$ogg))
fab$ogg[f1]=0

f2 <- which(is.na(fab$pgr))
fab$pgr[f2]=0

f3 <- which(is.na(fab$sgr))
fab$sgr[f3]=0

f4 <- which(is.na(fab$ogr))
fab$ogr[f4]=0

f5 <- which(is.na(fab$pgg))
fab$pgg[f5]=0

f6 <- which(is.na(fab$sgg))
fab$sgg[f6]=0


fab$gross <- fab$sgg + fab$pgg +
  fab$ogr + fab$sgr + fab$pgr + fab$ogg



fab$GrossType <- paste(fab$ntwk_sgg, fab$ntwk_pgg, fab$ntwk_ogr, fab$ntwk_sgr, fab$ntwk_pgr)
fab$GrossType <- str_replace_all(fab$GrossType, "NA", "")
fab$GrossType <- as.factor(fab$GrossType)
fab <- fab[, -c(8:19)]

summary(fab)

#x.sc.join <- x.sc.join[-index,]
fab$date <- paste(fab$Day, fab$Consumption.Month, sep=".") %>% str_replace_all("201$", "2010")
fab$date <- dmy(fab$date) 
fab <- fab[,-c(3,4)]

#combine solar attribute with tidied solar dataset
solar_attribute <- read.csv("SolarAttributes.csv", stringsAsFactors = T)
fab <- left_join(solar_attribute, fab, by=c("Customer.ID"= "Customer.ID"))


#for solar data, add total energy consumption
colnames(fab)
fab$cons_total <- fab$cons + fab$Cl
#sum up monthly data to get yearly data
summary(fab)
#for monthly less than 300KWh, take it off
#length(which(solar_data$cons < 300))
fab$year <-year(fab$date)  
fab$month <-month(fab$date) 

fab <- drop_na(fab)

#write_csv(fab, "tidied_solar_pv.csv")



id <- unique(fab$Customer.ID)
yr <- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
#length(id)
mat <- matrix(0, nrow = length(id), ncol = length(yr))

for (h in 1:length(yr)){
  for (i in 1:length(id)){
    #get unique id in the dataset
    index <-  which(fab$Customer.ID== id[i] &  fab$year == yr[h])
    #for each index, check for
    if (length(index)==12){
      mat[i,h] <-  sum(fab$cons_total[index])
    }
    
  }
}

View(mat)

total_cons <- cbind(cust_id = id, year = mat)
class(total_cons)
total_cons <- as.data.frame(total_cons)
total_cons <- total_cons %>% rename("2007"="V2", "2008"="V3", "2009"="V4",
                                    "2010"="V5", "2011"="V6", "2012"="V7",
                                    "2013"="V8", "2014"="V9", "2015"="V10")

total_solar_con <- total_cons %>% gather(key = year, value = cons, 2:10)
total_solar_con <- subset(total_solar_con, total_solar_con$cons != 0)

View(total_solar_con)


write_csv(total_solar_con, "solarpv_con.csv")

ggplot(total_solar_con)+geom_bar(stat = "identity", aes(x=year, y=cons))

