
#from may1 recreating popdam not averag


rm(list=ls())


#-----------------------------------------------------------------------------------------------------------------------------------------------

load("~/Desktop/Dir3/e'z2/emdat_combined.Rda")
emdat_orig <- emdat
load("~/Desktop/Dir/resubmission/emdat_newpopdams.Rda")


colnames(emdat)

colnames(emdat_orig)

emdat_wna <- emdat_orig[,-c(4:5)]
colnames(emdat_wna) <- c("start_year", "distype", "iso", "totalaff", "totaldam")

save(emdat_wna, file="emdat_w_nas.Rda")


#------------------------------------------------------------------

load("~/Desktop/Dir/resubmission/emdat_w_nas.Rda")
emdat <- emdat_wna

#load("~/Desktop/Dir/resubmission/new_popdam.Rda")

find_consecutive_years = function(years) {
  groups = split(years, cumsum(c(1, diff(years) != 1)))
  temp = sapply(groups, function(x) {
    if (length(x) == 1) {
      return (data.frame(start_year = x, end_year = x))
    } else {
      return (data.frame(start_year = min(x), end_year = max(x)))
    }
    return(data.frame(t(temp)))
  })
}

# group by country and extract consecutive years for each country
a3_new = ddply(.data = emdat,
               .variables = c("iso", "distype"),
               .fun = function(x) {
                 # print(x$iso)
                 years = x$start_year
                 cons_years = t(find_consecutive_years(years))
                 df = data.frame(cons_years)
                 return(df)
               })

View(a3_new)

a3_new$start_year <- as.numeric(a3_new$start_year)
a3_new$end_year <- as.numeric(a3_new$end_year)

a32b <- a3_new %>% mutate(duration= end_year-start_year)
#a32b <- filter(a32b, duration<=5)
#View(a32b)

View(a1)
a1<- filter(a32b, duration==0)
a1$start_year <- as.numeric(as.character(a1$start_year))
blah2 <- left_join(a1, emdat, by=c("iso", "start_year", "distype"))
View(blah2)
blah2$duration=NULL


#------------------------------------------------------------------------------------


anti_join(a3, a3_new, by=c("iso", "distype", "start_year"))



multiyear <- filter(a32b, duration>0)

#View(multiyear)
multiyear <- multiyear[,1:4]

temp <- NULL
temp <- data.frame()
# #View(temp)
# 
for(i in 1:nrow(multiyear)){
  x = as.data.frame(rbind(c(multiyear$iso[i], multiyear$distype[i], (multiyear$start_year[i]:multiyear$end_year[i]))))
  temp <- rbind.fill(temp,x)
}
View(temp)

temp <- melt(temp, id=c("V1", "V2"))
temp <- arrange(temp, V1, V2, value)

temp$variable <- NULL

colnames(temp) <- c("iso", "distype", "start_year")
temp <- filter(temp, !is.na(start_year))

#View(temp)
temp$start_year <- as.numeric(as.character(temp$start_year))
temp <- arrange(temp, iso, distype, start_year)

temp2<- left_join(temp, emdat, by=c("iso", "distype", "start_year"))

View(temp2)              
temp2 <- arrange(temp2, iso, distype, start_year)

rmp <- temp2 %>% 
  group_by(iso, distype, cumsum(c(1, diff(temp2$start_year) != 1))) %>%
  summarize(sumaff = sum(totalaff, na.rm=TRUE),
            sumdam = sum(totaldam, na.rm=TRUE),
            start_year1 = min(start_year),
            end_year1 = max(start_year)
  )

colnames(rmp) <- c("iso", "distype", "cumsum", "sumaff", "sumdam", "start_year", "end_year")
View(rmp)

View(multiyear)
#------------------------------------------------------

load("~/Desktop/Dir/resubmission/pop2.Rda")
load("~/Desktop/Dir/resubmission/gdp2.Rda")

temptemp <- left_join(temp, pop2,  by=c("iso", "start_year")) %>% left_join(., gdp2,  by=c("iso", "start_year"))
View(temptemp)
temptemp <- arrange(temptemp, iso, distype, start_year)

temptemp <- temptemp %>% group_by(iso, distype, cumsum(c(1, diff(temptemp$start_year) != 1))) %>%
  summarize(pop = mean(pop, na.rm=TRUE),
            gdp = mean(gdp, na.rm=TRUE),
            start_year1 = min(start_year),
            end_year1 = max(start_year))

temptemp$`cumsum(c(1, diff(temptemp$start_year) != 1))`<- NULL
colnames(temptemp) <- c("iso", "distype", "pop", "gdp", "start_year", "end_year")

#------------------------------------------------------

blah <- left_join(rmp, multiyear, by=c("iso", "distype", "start_year", "end_year")) %>% left_join(., temptemp,  by=c("iso", "distype", "start_year", "end_year"))
View(blah)

#THIS IS WHERE YOU CAN CHOOSE TO NOT AVERAGE!

blah$duration = (blah$end_year-blah$start_year)+1
blah$totalaff = blah$sumaff/blah$duration
blah$totaldam = blah$sumdam/blah$duration

blah <- blah[, -c(3,10)]
#blah <- dplyr::rename(blah, distype=distype.x)
#blah <- dplyr::rename(blah, totaldam = meantotaldam, totalaff= meantotalaff)


blah2 <- blah2 %>% group_by(iso, start_year, end_year, distype) %>% summarise(totaldam = sum(totaldam), totalaff=sum(totalaff))

blah2 <- left_join(blah2, pop2, by=c("iso", "start_year")) %>% left_join(., gdp2, by=c("iso", "start_year"))


b3 <- bind_rows(blah, blah2)
View(b3)

filta <- b3
View(filta)

filta$ppop <- (filta$totalaff/filta$pop)*100

#turn off scientific notation
options(scipen=999)

filta$pgdp <- ((filta$totaldam*1000)/filta$gdp)*100
save(filta, file="alldis_alldata.Rda")

