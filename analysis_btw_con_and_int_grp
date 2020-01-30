library(mapview)
library(sf)
library(tidyverse)
library(ggmap)
library(lubridate)

list.files()
solar_data <- read.csv("solarpv_con.csv", stringsAsFactors = T)
non_solar_data <- read.csv("nsolarpv_con.csv", stringsAsFactors = T)

solar_data <- solar_data %>% subset(solar_data$cons > 3000 & solar_data$cons < 31000) 
#summary of households energy consumption with solar pv
summary(solar_data)
#create a column that indicates yr in which intervention took place
solar_data$group <- ifelse(solar_data$year >= 2010, 1,0)
solar_data$group <- as.factor(solar_data$group)
solar_data$year <- as.factor(solar_data$year)
ggplot(solar_data) + 
  geom_bar(aes(x=year , y = (..count..)/sum(..count..), fill=group)) + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Year of energy consumption in solar pv households") +
  ylab("Frequency") 

#from the bar graph, year 2012 and 2013 had the highest number of households with energy consumption readings,

#Before solar pv installation, yr 2009 had the highest number of households with energy consumption readings,
#after installation, #year 2010 had the least number of households with energy consumption readings

 
#to view the energy consumption in households with solar pv, a box plot is used
solar_data$group <- as.factor(solar_data$group)
solar_data$year <- as.factor(solar_data$year)
ggplot(data = solar_data) + geom_boxplot(aes(y = cons, x = group, fill = year))
#as seen in the box-plot, after installation of solar pv, which occurred in 2010,
#there has been a decrease in energy consumption progressively until yr 2014 which was slightly higher than yr 2013. 
#as such, yr 2013 recorded the lowest energy consumption.
#prior to installation, energy consumption in the yr 2007 was close to yr 2008. However, there was 
#decrease in energy consumption in the subsequent yr - 2009. 

#Effect of solar pv installation
#using the period with the highest no of households
#before solar installation- 2009
#and period with the highest no of households after solar installation-2013

solar_data_0913 <- solar_data %>% subset(solar_data$year == 2009 | solar_data$year == 2013)

a <- solar_data[,-4] %>% subset(solar_data$year == 2009)
b <- solar_data[,-4] %>% subset(solar_data$year == 2013)
c <- left_join(a, b, by=c("cust_id"="cust_id"))
c<- drop_na(c)
c <- c[, -c(2,4)] %>% rename( "2009" = cons.x, "2013"=cons.y)

c.gather <- c %>% gather(key=year, value = cons, 2:3)
c.gather$group <- ifelse(c.gather$year== 2009, 0, 1)
c.gather$group <- as.factor(c.gather$group)
#using boxplot to visualize the data
ggplot(data = c.gather) + geom_boxplot(aes(y = cons, x = group, fill = year)) +
  ylab("Energy Consumption (KWh)")


#there is a reduction in energy consumption from 2009 to 2013
#to quantify the effect, t-test is used 
#however, to use t-test, the dataset before and after intervention 
#should be normally distributed.
#before intervention
#Performing the normality test on the dataset 


qqnorm(c$`2009`)
qqline(c$`2009`)
#the null hypothesis states that the sample comes from a normally distributed population
shapiro.test(c$`2009`)
?shapiro.test
#histogram plot for a better visualization
hist(c$`2009`, main="Histogram of 2009 energy consumption data",xlab="Slightly skewed to the right data for 2009")


qqnorm(c$`2013`)
qqline(c$`2013`)
shapiro.test(c$`2013`)
#histogram plot for a better visualization
hist(c$`2013`, main="Histogram of 2013 energy consumption data",xlab="Slightly skewed to the right data for 2013")
#skewer to the right than 2009 consumption data

#The Shapiro test tells me the data are not normally distributed.
#From the output, the p-value < 0.05 implying that the distribution of the data are 
#significantly different from normal distribution. In other words, we cannot assume normality.
#But the deviation from normality is not really apparent as seen from the qqplot.
#Moreover, the dataset has a large no of observation, as such, the parametric t-test can be used for inference on the dataset

#Using Paired T-test to see whether the change in consumption between 2009 and 2013 was significantly different
#null hypothesis: there is no significance difference between pre and post intervention
result <- t.test(c$`2009`, c$`2013`, paired = T)
result
format(result)
View(format(result))
#The p-value of the test is< 2.2e-16, which is less than the significance level alpha = 0.05. 
#We can then reject null hypothesis and conclude that 
#the average consumption of the participants before the installation of solar pv is significantly 
#different from the average consumption after installation of solar pv.
#the result also provides confidence intervals of the before-after difference: here, 1207.257 - 1440.870 kwh less consumption in the After period


#to further 
#to see if there is a significant difference in the change in energy cons
#between the two groups we need to perform an unpaired samples t test to the differences (if normality)
#mean consumption for 2 groups, before and after.
#But before we do that, we need to know that we are comparing similar households weighted by postcode.


#filter dataset
non_solar_data <- non_solar_data %>% subset(non_solar_data$cons > 3000 & non_solar_data$cons < 31000) 

#select for yr 2009 & 2013
#summary of households energy consumption with solar pv
summary(non_solar_data)



non_solar_data_0913 <- non_solar_data %>% subset(non_solar_data$year == 2009 | non_solar_data$year == 2013)

a1 <- non_solar_data %>% subset(non_solar_data$year == 2009)
b1 <- non_solar_data %>% subset(non_solar_data$year == 2013)
c1 <- left_join(a1, b1, by=c("cust_id"="cust_id"))
c1<- drop_na(c1)
c1 <- c1[, -c(2,4)] %>% rename( "2009" = cons.x, "2013"=cons.y)

#combine data solar energy consumption dataset with solar attribute
bb <- read.csv("SolarAttributes.csv", stringsAsFactors = T)

cc <- read.csv("NonSolarAttributes.csv", stringsAsFactors = T)

aa <- left_join(bb, cc, by="Postcode")

ca <-  unique(aa$Postcode)
#create a bin, and initialize variables
fam <- ""
box <- ""
i=0
aa$LGA.y <- as.character(aa$LGA.y)

for (i in 1:length(ca)){
  cai <- which(aa$Postcode == ca[i])
  #aa$Customer.ID.x[cai]
  #aa$Customer.ID.y[cai]
  int <- unique(aa$Customer.ID.x[cai])
  cont <- unique(aa$Customer.ID.y[cai])
  lga <- unique(aa$LGA.y[cai]) 

  m <- min(length(int), length(cont))
  #cut to ensure equal length
  cont_grp <- cont[1:m]
  int_grp <- int[1:m]
 
  fam <- cbind(int_grp, cont_grp, lga,  postcode = ca[i])
  
  box <- rbind(box, fam)
    
}
box <- as.data.frame(box)
box <- drop_na(box)
#first row is empty
box <- box[-1,]

#the value stored in the dataframe is a factor,
#however, integer type is need, if changed directly
#from factor to integer, it distorts the data
#as such, covert 1st to character, then convert to integer
box$cont_grp <- as.character(box$cont_grp)
box$int_grp <- as.character(box$int_grp)

box$cont_grp <- as.integer(box$cont_grp)
box$int_grp <- as.integer(box$int_grp)

summary(box)


#match postcode attributes box with non-solar group
cprime_ns <- left_join(box, c1, by=c("cont_grp"="cust_id"))
cprime_ns <- gather(cprime_ns, key = year, value = cons, 5:6)
cprime_ns$group <- 0

#cprime_ns <- cprime_ns %>% rename("C2009"="2009", "C2013"="2013")
#match postcode attributes box-int_group with solar group
cprime_ns1 <- left_join(box, c, by=c("int_grp"="cust_id"))
cprime_ns1 <- gather(cprime_ns1, key = year, value = cons, 5:6)
cprime_ns1$group <- 1


#cprime_ns1 <- cprime_ns1 %>% rename("I2009"="2009", "I2013"="2013")
#vertically bind both dataset
cprime_com <- rbind(cprime_ns, cprime_ns1)
cprime_com <- drop_na(cprime_com)
summary(cprime_com)

cprime_com$group <- as.factor(cprime_com$group)
cprime_com$year <- as.factor(cprime_com$year)
cprime_com$postcode <- as.character(cprime_com$postcode)
cprime_com$postcode <- as.integer(cprime_com$postcode)


north_region <- subset(cprime_com, lga== "CANADA BAY" | lga== "CANTERBURY" 
                       | lga== "HORNSBY" | lga== "BURWOOD"
                       | lga== "HUNTERS HILL" | lga== "NORTH SYDNEY" 
                       | lga== "MOSMAN" | lga== "STRATHFIELD"
                       | lga== "WILLOUGHBY" | lga== "WOOLLAHRA" 
                       | lga== "SYDNEY" | lga== "RYDE"
                       | lga== "MANLY"
                       | lga== "RANDWICK" | lga== "LANE COVE" 
                       | lga== "WARRINGAH" | lga== "PITTWATER" 
                       | lga== "KU-RING-GAI" | lga== "RYDE")

coastal_reg <- subset(cprime_com, lga == "GOSFORD" | lga == "WYONG")
hunter_reg <- subset(cprime_com, lga== "LAKE MACQUARIE" | lga== "NEWCASTLE" |
                       lga== "MAITLAND" | lga== "CESSNOCK"| lga== "PORT STEPHENS" |
                       lga== "SINGLETON" | lga== "WAVERLEY"| lga== "MUSWELLBROOK" | 
                       lga== "UPPER HUNTER")
southern_reg <- subset(cprime_com, lga== "ASHFIELD" | lga== "AUBURN" 
                       | lga== "BANKSTOWN" | lga== "BOTANY BAY"
                       | lga== "HURSTVILLE" | lga== "KOGARAH" 
                       | lga== "LEICHHARDT" | lga== "MARRICKVILLE"
                       | lga== "SUTHERLAND" | lga == "ROCKDALE")




ggplot(north_region)+ geom_boxplot(aes(x=year, y=cons, fill=group))

ggplot(coastal_reg)+ geom_boxplot(aes(x=year, y=cons, fill=group))

ggplot(hunter_reg)+ geom_boxplot(aes(x=year, y=cons, fill=group))

ggplot(southern_reg)+ geom_boxplot(aes(x=year, y=cons, fill=group)) 


#COMPARISM BY REGION
#1. 
#for the northern sydney region,
#in yr 2009

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population

#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
north_con_int_09 <-north_region$cons[north_region$group == 1 & north_region$year == 2009]

qqnorm(north_con_int_09)
qqline (north_con_int_09)#how does this look? not too bad

hist(north_con_int_09, main="Histogram of intervention group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(north_con_int_09)#The Shapiro test tells me the data are not normally distributed.

#for control group
north_con_cont_09 <-north_region$cons[north_region$group == 0 & north_region$year == 2009]

qqnorm(north_con_cont_09)
qqline (north_con_cont_09)#how does this look? not too bad

hist(north_con_cont_09, main="Histogram of control group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(north_con_cont_09)#The Shapiro test tells me the data are not normally distributed.

ncbind <- cbind(north_con_cont_09, "cont_grp")
icbind <- cbind(north_con_int_09, "inter_grp")
nibind <- rbind(ncbind, icbind)
nibind <- as.data.frame(nibind)
summary(nibind)
nibind <-  rename(nibind, cons = north_con_cont_09, type = V2 )
nibind$cons <- as.character(nibind$cons)
nibind$cons <- as.numeric(nibind$cons)
#check for variance

#The null hypothesis is that the gas consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.ftest <- var.test(cons ~ type, data = nibind)
res.ftest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res <- t.test(cons ~ type, data = nibind, var.equal = TRUE)
res

#As the p-value turns out to be 0.7233 and is greater than the 0.05 significance level, we accept the null hypothesis.
#in yr 2009, there was no significance difference in energy consumption between intervention and control grp

#in yr 2013

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population

#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
north_con_int_13 <-north_region$cons[north_region$group == 1 & north_region$year == 2013]

qqnorm(north_con_int_13)
qqline (north_con_int_13)#how does this look? not too bad

hist(north_con_int_13, main="Histogram of intervention group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(north_con_int_13)#The Shapiro test tells me the data are not normally distributed.

#for control group
north_con_cont_13 <-north_region$cons[north_region$group == 0 & north_region$year == 2013]

qqnorm(north_con_cont_13)
qqline (north_con_cont_13)#how does this look? not too bad

hist(north_con_cont_13, main="Histogram of control group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(north_con_cont_13)#The Shapiro test tells me the data are not normally distributed.

ncbind <- cbind(north_con_cont_13, "cont_grp")
icbind <- cbind(north_con_int_13, "inter_grp")
nibind <- rbind(ncbind, icbind)
nibind <- as.data.frame(nibind)
summary(nibind)
nibind <-  rename(nibind, cons = north_con_cont_13, type = V2 )
nibind$cons <- as.character(nibind$cons)
nibind$cons <- as.numeric(nibind$cons)
#check for variance

#The null hypothesis is that the energy consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.ftest <- var.test(cons ~ type, data = nibind)
res.ftest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res <- t.test(cons ~ type, data = nibind, var.equal = TRUE)
res
format(res)
#As the p-value turns out to be 0.2144, and is greater than the 0.05 significance level, we accept the null hypothesis.
#in yr 2013, there was no significance difference in energy consumption between intervention and control grp


#use similar approach for other region

#2. 
#for the southern sydney region,
#in yr 2009

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population

#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
south_con_int_09 <-southern_reg$cons[southern_reg$group == 1 & southern_reg$year == 2009]

qqnorm(south_con_int_09)
qqline (south_con_int_09)#how does this look? not too bad

hist(south_con_int_09, main="Histogram of intervention group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(south_con_int_09)#The Shapiro test tells me the data are not normally distributed.

#for control group
south_con_cont_09 <-southern_reg$cons[southern_reg$group == 0 & southern_reg$year == 2009]

qqnorm(south_con_cont_09)
qqline (south_con_cont_09)#how does this look? not too bad

hist(south_con_cont_09, main="Histogram of control group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(south_con_cont_09)#The Shapiro test tells me the data are not normally distributed.

sncbind <- cbind(south_con_cont_09, "cont_grp")
sicbind <- cbind(south_con_int_09, "inter_grp")
snibind <- rbind(sncbind, sicbind)
snibind <- as.data.frame(snibind)
summary(snibind)
snibind <-  rename(snibind, cons = south_con_cont_09, type = V2 )
snibind$cons <- as.character(snibind$cons)
snibind$cons <- as.numeric(snibind$cons)
#check for variance

#The null hypothesis is that the energy consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.stest <- var.test(cons ~ type, data = snibind)
res.stest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res_s <- t.test(cons ~ type, data = snibind, var.equal = TRUE)
res_s

#As the p-value turns out to be 0.1176, and is greater than the 0.05 significance level, we accept the null hypothesis.
#in yr 2009, there was no significance difference in energy consumption between intervention and control grp

#in yr 2013

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population

#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
south_con_int_13 <-southern_reg$cons[southern_reg$group == 1 & southern_reg$year == 2013]

qqnorm(south_con_int_13)
qqline (south_con_int_13)#how does this look? not too bad

hist(south_con_int_13, main="Histogram of intervention group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(south_con_int_13)#The Shapiro test tells me the data are not normally distributed.

#for control group
south_con_cont_13 <-southern_reg$cons[southern_reg$group == 0 & southern_reg$year == 2013]

qqnorm(south_con_cont_13)
qqline (south_con_cont_13)#how does this look? not too bad

hist(south_con_cont_13, main="Histogram of control group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(south_con_cont_13)#The Shapiro test tells me the data are not normally distributed.

sncbind <- cbind(south_con_cont_13, "cont_grp")
sicbind <- cbind(south_con_int_13, "inter_grp")
snibind <- rbind(sncbind, sicbind)
snibind <- as.data.frame(snibind)
summary(snibind)
snibind <-  rename(snibind, cons = south_con_cont_13, type = V2 )
snibind$cons <- as.character(snibind$cons)
snibind$cons <- as.numeric(snibind$cons)
#check for variance

#The null hypothesis is that the energy consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.ftest <- var.test(cons ~ type, data = snibind)
res.ftest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res <- t.test(cons ~ type, data = snibind, var.equal = TRUE)
res
format(res)
#As the p-value turns out to be 0.78, and is greater than the 0.05 significance level, we accept the null hypothesis.
#in yr 2013, there was no significance difference in energy consumption between intervention and control grp


#use similar approach for other region



#3. 
#for the hunter region,
#in yr 2009

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population

#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
hunter_con_int_09 <-hunter_reg$cons[hunter_reg$group == 1 & hunter_reg$year == 2009]

qqnorm(hunter_con_int_09)
qqline (hunter_con_int_09)#how does this look? not too bad

hist(hunter_con_int_09, main="Histogram of intervention group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(hunter_con_int_09)#The Shapiro test tells me the data are not normally distributed.

#for control group
hunter_con_cont_09 <-hunter_reg$cons[hunter_reg$group == 0 & hunter_reg$year == 2009]

qqnorm(hunter_con_cont_09)
qqline (hunter_con_cont_09)#how does this look? not too bad

hist(hunter_con_cont_09, main="Histogram of control group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(hunter_con_cont_09)#The Shapiro test tells me the data are not normally distributed.

hncbind <- cbind(hunter_con_cont_09, "cont_grp")
hicbind <- cbind(hunter_con_int_09, "inter_grp")
xhnibind <- rbind(hncbind, hicbind)
xhnibind <- as.data.frame(xhnibind)
summary(xhnibind)
xhnibind <-  rename(xhnibind, cons = hunter_con_cont_09, type = V2 )
xhnibind$cons <- as.character(xhnibind$cons)
xhnibind$cons <- as.numeric(xhnibind$cons)
#check for variance

#The null hypothesis is that the energy consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.htest <- var.test(cons ~ type, data = xhnibind)
res.htest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res_h <- t.test(cons ~ type, data = xhnibind, var.equal = TRUE)
res_h

#As the p-value turns out to be 0.086, and is greater than the 0.05 significance level, we accept the null hypothesis.
#in yr 2009, there was no significance difference in energy consumption between intervention and control grp

#in yr 2013

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population
 
#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
hunter_con_int_13 <-hunter_reg$cons[hunter_reg$group == 1 & hunter_reg$year == 2013]

qqnorm(hunter_con_int_13)
qqline (hunter_con_int_13)#how does this look? not too bad

hist(hunter_con_int_13, main="Histogram of intervention group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(hunter_con_int_13)#The Shapiro test tells me the data are not normally distributed.

#for control group
hunter_con_cont_13 <-hunter_reg$cons[hunter_reg$group == 0 & hunter_reg$year == 2013]

qqnorm(hunter_con_cont_13)
qqline (hunter_con_cont_13)#how does this look? not too bad

hist(hunter_con_cont_13, main="Histogram of control group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(hunter_con_cont_13)#The Shapiro test tells me the data are not normally distributed.

hncbind <- cbind(hunter_con_cont_13, "cont_grp")
hicbind <- cbind(hunter_con_int_13, "inter_grp")
hnibind <- rbind(hncbind, hicbind)
hnibind <- as.data.frame(hnibind)
summary(hnibind)
hnibind <-  rename(hnibind, cons = hunter_con_cont_13, "type" = V2 )
hnibind$cons <- as.character(hnibind$cons)
hnibind$cons <- as.numeric(hnibind$cons)
#check for variance

#The null hypothesis is that the energy consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.ftest <- var.test(cons ~ type, data = hnibind)
res.ftest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res <- t.test(cons ~ type, data =hnibind, var.equal = TRUE)
res
format(res)
#As the p-value turns out to be 0.3562, and is greater than the 0.05 significance level, we reject the null hypothesis.
#in yr 2013, there was a significance difference in energy consumption between intervention and control grp
#energy consumption reduced from cont grp to interv grp

#use similar approach for other region

#4. 
#for the coastal region,
#in yr 2009

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population

#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
coastal_con_int_09 <-coastal_reg$cons[coastal_reg$group == 1 & coastal_reg$year == 2009]

qqnorm(coastal_con_int_09)
qqline (coastal_con_int_09)#how does this look? not too bad

hist(coastal_con_int_09, main="Histogram of intervention group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(coastal_con_int_09)#The Shapiro test tells me the data are not normally distributed.

#for control group
coastal_con_cont_09 <-coastal_reg$cons[coastal_reg$group == 0 & coastal_reg$year == 2009]

qqnorm(coastal_con_cont_09)
qqline (coastal_con_cont_09)#how does this look? not too bad

hist(coastal_con_cont_09, main="Histogram of control group consumption in yr 2009",xlab="skewed data for the control group")#this does not look good!

shapiro.test(coastal_con_cont_09)#The Shapiro test tells me the data are not normally distributed.

cncbind <- cbind(coastal_con_cont_09, "cont_grp")
cicbind <- cbind(coastal_con_int_09, "inter_grp")
cnibind <- rbind(cncbind, cicbind)
cnibind <- as.data.frame(cnibind)
summary(cnibind)
cnibind <-  rename(cnibind, cons = coastal_con_cont_09, type = V2 )
cnibind$cons <- as.character(cnibind$cons)
cnibind$cons <- as.numeric(cnibind$cons)
#check for variance

#The null hypothesis is that the energy consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.htest <- var.test(cons ~ type, data = cnibind)
res.htest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res_h <- t.test(cons ~ type, data = cnibind, var.equal = TRUE)
res_h

#As the p-value turns out to be 0.3, and is greater than the 0.05 significance level, we accept the null hypothesis.
#in yr 2009, there was no significance difference in energy consumption between intervention and control grp

#in yr 2013

#to compare pre and post intervention and control group, the variance test is 
#carried out to know if both samples are from the same population

#we need to check for normality in BOTH groups (0 and 1)
#for intervention group
coastal_con_int_13 <-coastal_reg$cons[coastal_reg$group == 1 & coastal_reg$year == 2013]

qqnorm(coastal_con_int_13)
qqline (coastal_con_int_13)#how does this look? not too bad

hist(coastal_con_int_13, main="Histogram of intervention group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(coastal_con_int_13)#The Shapiro test tells me the data are not normally distributed.

#for control group
coastal_con_cont_13 <-coastal_reg$cons[coastal_reg$group == 0 & coastal_reg$year == 2013]

qqnorm(coastal_con_cont_13)
qqline (coastal_con_cont_13)#how does this look? not too bad

hist(coastal_con_cont_13, main="Histogram of control group consumption in yr 2013",xlab="skewed data for the control group")#this does not look good!

shapiro.test(coastal_con_cont_13)#The Shapiro test tells me the data are not normally distributed.

cncbind <- cbind(coastal_con_cont_13, "cont_grp")
cicbind <- cbind(coastal_con_int_13, "inter_grp")
cnibind <- rbind(cncbind, cicbind)
cnibind <- as.data.frame(cnibind)
summary(cnibind)
cnibind <-  rename(cnibind, cons = coastal_con_cont_13, type = V2 )
cnibind$cons <- as.character(cnibind$cons)
cnibind$cons <- as.numeric(cnibind$cons)
#check for variance

#The null hypothesis is that the energy consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for me to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.ftest <- var.test(cons ~ type, data = cnibind)
res.ftest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:
res <- t.test(cons ~ type, data =cnibind, var.equal = TRUE)
res
format(res)
#As the p-value turns out to be 0.268, and is greater than the 0.05 significance level, we accept the null hypothesis.
#in yr 2013, there was no significance difference in energy consumption between intervention and control grp
#energy consumptio n reduced from cont grp to interv grp
