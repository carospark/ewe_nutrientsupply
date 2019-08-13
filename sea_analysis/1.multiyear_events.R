#load in EWE data
load("~/Desktop/Dir/resubmission/emdat_w_nas.Rda")

# function to find EWEs that occurred in the same country over a consecutive period of years
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

a32b <- a3_new %>% mutate(duration= end_year-start_year)

#separate EWEs by single events or multiyear events 
single <- filter(a32b, duration==0)
single2 <- left_join(single, emdat, by=c("iso", "start_year", "distype"))

multiyear <- filter(a32b, duration>0)
multiyear <- multiyear[,1:4]

#for multiyear events, extract each year in order to aggregate total "persons affected" and total "financial damage" sums
temp <- NULL
temp <- data.frame()
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
temp <- arrange(temp, iso, distype, start_year)

temp2<- left_join(temp, emdat, by=c("iso", "distype", "start_year"))
temp2 <- arrange(temp2, iso, distype, start_year)

multiyear2 <- temp2 %>% 
  group_by(iso, distype, cumsum(c(1, diff(temp2$start_year) != 1))) %>%
  summarize(sumaff = sum(totalaff, na.rm=TRUE),
            sumdam = sum(totaldam, na.rm=TRUE),
            start_year1 = min(start_year),
            end_year1 = max(start_year)
  )

colnames(multiyear2) <- c("iso", "distype", "cumsum", "sumaff", "sumdam", "start_year", "end_year")

#load in population and GDP data (from World Bank)
load("~/Desktop/Dir/resubmission/pop2.Rda")
load("~/Desktop/Dir/resubmission/gdp2.Rda")

#find and append population and GDP for the country and year of each multiyear EWE
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
multiyear_total <- left_join(multiyear2, multiyear, by=c("iso", "distype", "start_year", "end_year")) %>% left_join(., temptemp,  by=c("iso", "distype", "start_year", "end_year"))

#append population and GDP for single-year EWEs
single2 <- single2 %>% group_by(iso, start_year, end_year, distype) %>% summarise(totaldam = sum(totaldam), totalaff=sum(totalaff))
single2 <- left_join(single2, pop2, by=c("iso", "start_year")) %>% left_join(., gdp2, by=c("iso", "start_year"))


#non-averaged total sums for "persons affected" and "financial damage"
multiyear_total$duration = (multiyear_total$end_year-multiyear_total$start_year)+1
multiyear_total$totalaff = multiyear_total$sumaff/multiyear_total$duration
multiyear_total$totaldam = multiyear_total$sumdam/multiyear_total$duration

multiyear_total <- multiyear_total[, -c(3,10)]


#commbine all EWEs and find the proportion of affected per population, or financial damage per GDP
combined <- bind_rows(multiyear_total, single2)
combined$ppop <- (combined$totalaff/combined$pop)*100
combined$pgdp <- ((combined$totaldam*1000)/combined$gdp)*100

save(combined, file="alldis_alldata.Rda")

