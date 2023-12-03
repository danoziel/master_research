# names(at_btselem) ----
"date"                                "total_killed"                       

y <- at_btselem
y [is.na(y)] <- 0 #replace NA to 0

y <- y %>% group_by(yearweek,week_range) %>%
  summarise(total_attacks=sum(total_attacks),
            attacks_located_IL=sum(total_events_located_IL),
            attacks_located_yesha=sum(total_events_located_yesha),
            attacks_target_civilian=sum(total_events_target_civilian),
            attacks_target_security_forces=sum(total_events_target_security_forces),
            sum(total_killed)) %>%filter(yearweek>="2001-W30")

x <- bind_peace10_Q3
xy <- left_join(x,y) %>% select(1,3,4,7:11,2,5,6)
 
xil <- btselem10_il %>% select(1,2)
names(xil)[2] <- "killed_located_IL"

xye <- btselem10_ye %>% select(1,2)
names(xye)[2] <- "killed_located_yesha"

xciv <- btselem10_civ %>% select(1,2) 
names(xciv)[2] <-"killed_target_civilian" 
  
xsec <- btselem10_sec %>% select(1,2)
names(xsec)[2] <- "killed_target_security_forces"
 
x4 <- left_join(xil,xye,by = "yearweek")
x4 <- left_join(x4,xciv)
x4 <- left_join(x4,xsec)
x4 <- x4 %>%  filter(yearweek!="2012-W32",yearweek!="2013-W00",yearweek!="2015-W52")

zzz <- left_join(xy,x4,by="yearweek")

zq <- left_join(zzz,peace_Q1Q2[,1:3],by="yearweek")
zq <- zq[-587,]

zq$q2[zq$q2==0] <- NA
z <- left_join(zq,mete)

names(zqm)
z$total_attacks [is.na(z$total_attacks)] <- 0 #replace NA to 0
z$attacks_located_IL [is.na(z$attacks_located_IL)] <- 0 #replace NA to 0
z$attacks_located_yesha [is.na(z$attacks_located_yesha)] <- 0 #replace NA to 0
z$attacks_target_civilian [is.na(z$attacks_target_civilian)] <- 0 #replace NA to 0
z$attacks_target_security_forces [is.na(z$attacks_target_security_forces)] <- 0 #replace NA to 0

z$total_killed [is.na(z$total_killed)] <- 0 #replace NA to 0
z$killed_located_IL [is.na(z$killed_located_IL)] <- 0 #replace NA to 0
z$killed_located_yesha [is.na(z$killed_located_yesha)] <- 0 #replace NA to 0
z$killed_target_civilian [is.na(z$killed_target_civilian)] <- 0 #replace NA to 0
z$killed_target_security_forces [is.na(z$killed_target_security_forces)] <- 0 #replace NA to 0

z <- z %>%  select(1,2,16,17,3,4:9,12:15,10,18,19,11)

peace_btselm <- z


# meterologic----
ims_data_1 <- ims_data_1_ %>% filter(!is.na(humidity))
ims_data_3 <- ims_data_3_ %>% filter(!is.na(humidity))

class(ims_data_1$date)

ims_data_3$date <- as.IDate(ims_data_3$date)
ims_data_1$temperature <- as.numeric(ims_data_1_$temperature)
ims_data_1$humidity <- as.numeric(ims_data_1_$humidity)

mete <- meter %>% filter(temperature != "-",humidity!= "-"   )
mete$yearweek <-  strftime(mete$date, format = "%Y-W%U") 



meter <- rbind(ims_data_1,ims_data_3)
mete <- mete %>% group_by(yearweek) %>% summarise(mean(temperature),mean(humidity))


ims_data_1 <- ims_data_1[1:28274,]


# ------

library(foreign)
library(haven)		

write.dta(peace_and_attackes, "C:/Users/Dan/Documents/R/heat and peace/peace_and_attackes.dta")

write.table(peace_and_attackes, file="peace_and_attackes.csv",sep=",",row.names=F)

save(peace_btselm, file ='peace_btselm.RData')

# ----


