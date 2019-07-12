#from may24 redo estimates

setwd("~/Desktop/Dir/github")

#setwd("~/Desktop/Dir/lisa")


rm(list=ls())

#------------------------------------------------------------------------------

load("~/Desktop/Dir/genus/sod_7yrs.Rda")

sod$start_year <- as.numeric(as.character(sod$start_year))
indx <- sapply(sod, is.factor)
sod[indx]<- lapply(sod[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, sod, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2) 
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, sod, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


##View(blah)

blah <- left_join(blah, sod, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
sod <- dplyr::rename(sod, year=start_year)
blah <- left_join(blah, sod, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
#View(a3)
wow <- a3
#View(wow)
wow <- wow[complete.cases(wow),]
sod2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "sod"
sod_vesknorm <- vesknorm


#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/ribo_7yrs.Rda")

ribo$start_year <- as.numeric(as.character(ribo$start_year))
indx <- sapply(ribo, is.factor)
ribo[indx]<- lapply(ribo[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, ribo, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, ribo, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, ribo, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
ribo <- dplyr::rename(ribo, year=start_year)
blah <- left_join(blah, ribo, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
ribo2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "ribo"
ribo_vesknorm <- vesknorm 


#------------------------------------------------------------------------------

#save
load("~/Desktop/Dir/genus/thiamin_7yrs.Rda")

thiamin$start_year <- as.numeric(as.character(thiamin$start_year))
indx <- sapply(thiamin, is.factor)
thiamin[indx]<- lapply(thiamin[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, thiamin, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, thiamin, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, thiamin, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
thiamin <- dplyr::rename(thiamin, year=start_year)
blah <- left_join(blah, thiamin, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
thiamin2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "thiamin"
thiamin_vesknorm <- vesknorm

#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/vita_7yrs.Rda")

vita$start_year <- as.numeric(as.character(vita$start_year))
indx <- sapply(vita, is.factor)
vita[indx]<- lapply(vita[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, vita, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, vita, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, vita, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
vita <- dplyr::rename(vita, year=start_year)
blah <- left_join(blah, vita, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
vita2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "vita"
vita_vesknorm <- vesknorm 


#------------------------------------------------------------------------------

#save
load("~/Desktop/Dir/genus/vitc_7yrs.Rda")

vitc$start_year <- as.numeric(as.character(vitc$start_year))
indx <- sapply(vitc, is.factor)
vitc[indx]<- lapply(vitc[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, vitc, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, vitc, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, vitc, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
vitc <- dplyr::rename(vitc, year=start_year)
blah <- left_join(blah, vitc, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
vitc2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "vitc"
vitc_vesknorm <- vesknorm


#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/zinc_7yrs.Rda")

zinc$start_year <- as.numeric(as.character(zinc$start_year))
indx <- sapply(zinc, is.factor)
zinc[indx]<- lapply(zinc[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, zinc, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, zinc, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, zinc, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
zinc <- dplyr::rename(zinc, year=start_year)
blah <- left_join(blah, zinc, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
zinc2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "zinc"
zinc_vesknorm <- vesknorm


#------------------------------------------------------------------------------

#save
load("~/Desktop/Dir/genus/pot_7yrs.Rda")

pot$start_year <- as.numeric(as.character(pot$start_year))
indx <- sapply(pot, is.factor)
pot[indx]<- lapply(pot[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, pot, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, pot, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, pot, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
pot <- dplyr::rename(pot, year=start_year)
blah <- left_join(blah, pot, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
pot2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "pot"
pot_vesknorm <- vesknorm 

#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/phos_7yrs.Rda")

phos$start_year <- as.numeric(as.character(phos$start_year))
indx <- sapply(phos, is.factor)
phos[indx]<- lapply(phos[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, phos, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, phos, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, phos, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
phos <- dplyr::rename(phos, year=start_year)
blah <- left_join(blah, phos, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
phos2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "phos"
phos_vesknorm <- vesknorm 

#------------------------------------------------------------------------------

#save
load("~/Desktop/Dir/genus/bsix_7yrs.Rda")

bsix$start_year <- as.numeric(as.character(bsix$start_year))
indx <- sapply(bsix, is.factor)
bsix[indx]<- lapply(bsix[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, bsix, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, bsix, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, bsix, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
bsix <- dplyr::rename(bsix, year=start_year)
blah <- left_join(blah, bsix, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
bsix2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "bsix"
bsix_vesknorm <- vesknorm 

#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/cop_7yrs.Rda")

cop$start_year <- as.numeric(as.character(cop$start_year))
indx <- sapply(cop, is.factor)
cop[indx]<- lapply(cop[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, cop, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, cop, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, cop, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
cop <- dplyr::rename(cop, year=start_year)
blah <- left_join(blah, cop, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
cop2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "cop"

cop_vesknorm <- vesknorm 


#------------------------------------------------------------------------------

#save
load("~/Desktop/Dir/genus/fiber_7yrs.Rda")

fiber$start_year <- as.numeric(as.character(fiber$start_year))
indx <- sapply(fiber, is.factor)
fiber[indx]<- lapply(fiber[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, fiber, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, fiber, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, fiber, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
fiber <- dplyr::rename(fiber, year=start_year)
blah <- left_join(blah, fiber, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
fiber2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "fiber"

fiber_vesknorm <- vesknorm 

#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/niac_7yrs.Rda")

niac$start_year <- as.numeric(as.character(niac$start_year))
indx <- sapply(niac, is.factor)
niac[indx]<- lapply(niac[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, niac, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, niac, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, niac, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
niac <- dplyr::rename(niac, year=start_year)
blah <- left_join(blah, niac, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
niac2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "niac"

niac_vesknorm <- vesknorm 

#------------------------------------------------------------------------------

#save
load("~/Desktop/Dir/genus/calcium_7yrs.Rda")

calcium$start_year <- as.numeric(as.character(calcium$start_year))
indx <- sapply(calcium, is.factor)
calcium[indx]<- lapply(calcium[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, calcium, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, calcium, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, calcium, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
calcium <- dplyr::rename(calcium, year=start_year)
blah <- left_join(blah, calcium, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
calcium2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "calcium"
calcium_vesknorm <- vesknorm 

#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/folate_7yrs.Rda")

folate$start_year <- as.numeric(as.character(folate$start_year))
indx <- sapply(folate, is.factor)
folate[indx]<- lapply(folate[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, folate, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, folate, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, folate, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
folate <- dplyr::rename(folate, year=start_year)
blah <- left_join(blah, folate, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
folate2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "folate"
folate_vesknorm <- vesknorm 


#------------------------------------------------------------------------------

#save
load("~/Desktop/Dir/genus/iron_7yrs.Rda")

iron$start_year <- as.numeric(as.character(iron$start_year))
indx <- sapply(iron, is.factor)
iron[indx]<- lapply(iron[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, iron, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, iron, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, iron, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
iron <- dplyr::rename(iron, year=start_year)
blah <- left_join(blah, iron, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
iron2 <- wow$start


#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "iron"
iron_vesknorm <- vesknorm

#------------------------------------------------------------------------------


#save
load("~/Desktop/Dir/genus/mag_7yrs.Rda")

mag$start_year <- as.numeric(as.character(mag$start_year))
indx <- sapply(mag, is.factor)
mag[indx]<- lapply(mag[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
#a1 <- a1[,c(1:4)]
a2 <- filter(popdamyikes, !(start_year==end_year))
#a2 <- a2[,c(1:4)]

blah2 <- left_join(a1, mag, by=c("iso", "start_year"))
###View(blah2)

#multiple years totaldam
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2)
temp <- NULL
temp <- data.frame()
###View(temp)

for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

###View(temp)

temp2 <- melt(temp, id=c("V1", "V2"))
##View(temp2)
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, mag, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)
#temp2[is.na(temp2)] <- 0

temp2 <- temp2[complete.cases(temp2),]

temp2$start <- as.numeric(as.character(temp2$start))
rmp <- NULL
rmp <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 
##View(rmp)
blah <- NULL
blah <- left_join(rmp, a2, by=c("names", "iso"))
blah <- filter(blah, !is.na(value))


#View(blah)

blah <- left_join(blah, mag, by=c("iso", "start_year"))
blah <- blah[,c(1,3:8)]
blah <- dplyr::rename(blah, year=end_year)
mag <- dplyr::rename(mag, year=start_year)
blah <- left_join(blah, mag, by=c("iso", "year"))


blah <- blah[,c(1:7, 12:14)]
blah <- blah[,-c(5,10)]
blah <- dplyr::rename(blah, min2=min2.x,  min1=min1.x, end_year=year, start=value)

blah2$start <- as.numeric(as.character(blah2$start))

#View(blah2)
blah2 <- blah2[,-c(4,10)]
a3 <- bind_rows(blah, blah2)
###View(a3)
wow <- a3
##View(wow)
wow <- wow[complete.cases(wow),]
mag2 <- wow$start

#normalizinged based on average and then creating swhitearate table 

wow$avg = rowMeans(wow[,c(5:8)], na.rm=TRUE)
wow$sd <- apply(wow[,5:8], 1, sd)

vesknorm <- wow[,c(1,2,5:10)]

vesknorm$code <- "mag"
mag_vesknorm <- vesknorm 

#------------------------------------------------------------------------------------------------------------------------

abs_estimates_new <- data.frame(rbind(bsix_vesknorm, calcium_vesknorm, thiamin_vesknorm, cop_vesknorm, fiber_vesknorm, folate_vesknorm, iron_vesknorm, mag_vesknorm, niac_vesknorm, phos_vesknorm, pot_vesknorm, ribo_vesknorm, sod_vesknorm, vita_vesknorm, vitc_vesknorm, zinc_vesknorm))


save(abs_estimates_new, file="abs_estimates_new.Rda")

# abs_estimates_new$code <- c("bsix", "calcium", "thiamin", "cop", "fiber", "folate", "iron", "mag", "niac", "phos", "pot", "ribo", "sod", "vita", "vitc", "zinc")

abs_estimates_new<- arrange(abs_estimates_new, code)


bah <- filter(abs_estimates_new, code=="iron")

warView(ah)
ah$rel_dip= ah$rel_dip*100

colnames(ah) <- c("code", "0", "-2", "-1", "+1", "+2", "avg", "sd", "abs_dip", "perc_diff")

save(ah, file="make_chart_wthis.Rda") 





