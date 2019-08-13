#using sodium supply as an example

load("~/Desktop/Dir/genus/sod_5yrs.Rda")

sod$start_year <- as.numeric(as.character(sod$start_year))
indx <- sapply(sod, is.factor)
sod[indx]<- lapply(sod[indx], function(x) as.numeric(as.character(x)))

load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")

popdamyikes <- b32b[,c(1,5,6)]
a1 <- filter(popdamyikes, start_year==end_year)
a2 <- filter(popdamyikes, !(start_year==end_year))

single <- left_join(a1, sod, by=c("iso", "start_year"))

#multiple years 
a2 <- arrange(a2, iso, start_year)
a2$names <- rownames(a2) 
temp <- NULL
temp <- data.frame()
for(i in 1:nrow(a2)){
  x = as.data.frame(rbind(c(a2$iso[i], a2$names[i], (a2$start_year[i]:a2$end_year[i]))))
  temp <- rbind.fill(temp,x)
}

temp2 <- melt(temp, id=c("V1", "V2"))
temp2 <- arrange(temp2, V1, V2, value)
temp2$variable <- NULL
colnames(temp2) <- c("iso","names", "start_year")
temp2 <- filter(temp2, !is.na(start_year))

temp2$start_year <- as.numeric(as.character(temp2$start_year))
temp2 <- left_join(temp2, sod, by=c("iso", "start_year"))
temp2 <- arrange(temp2, iso,  start_year)


multi <- NULL
multi <- temp2 %>% 
  group_by(iso, names) %>%
  summarize(value = mean(start, na.rm=TRUE)) 

                   
multiyear <- left_join(multi, a2, by=c("names", "iso"))

single <- left_join(single, sod, by=c("iso", "start_year"))
single <- single[,c(1,3:8)]
single <- dplyr::rename(single, year=end_year)
sod <- dplyr::rename(sod, year=start_year)
single <- left_join(single, sod, by=c("iso", "year"))


single <- single[,c(1:7, 12:14)]
single <- single[,-c(5,10)]
single <- dplyr::rename(single, min2=min2.x,  min1=min1.x, end_year=year, start=value)

multiyear$start <- as.numeric(as.character(multiyear$start))


multiyear <- multiyear[,-c(4,10)]

#combine single and multiyear EWEs, and find each observation's non-EWE average and standard deviation
a3 <- bind_rows(multiyear, single)
a3$avg = rowMeans(a3[,c(5:8)], na.rm=TRUE)
a3$sd <- apply(a3[,5:8], 1, sd)

a3 <- a3[,c(1,2,5:10)]
a3$code <- "Sodium"
sod_vesknorm <- a3

                   
#-----------------------
#repeat above protocol for all nutrient supplies, then combine all into one dataframe for future use
                   
abs_estimates_new <- data.frame(rbind(bsix_vesknorm, calcium_vesknorm, thiamin_vesknorm, cop_vesknorm, fiber_vesknorm, folate_vesknorm, iron_vesknorm, mag_vesknorm, niac_vesknorm, phos_vesknorm, pot_vesknorm, ribo_vesknorm, sod_vesknorm, vita_vesknorm, vitc_vesknorm, zinc_vesknorm))

save(abs_estimates_new, file="abs_estimates_new.Rda")





