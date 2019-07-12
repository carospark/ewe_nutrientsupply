#from fuckme cntrls again

rm(list=ls())

setwd("~/Desktop/Dir/lisa/controls")
load("~/Desktop/Dir/lisa/controls/results.Rda")
load("~/Desktop/Dir/lisa/controls/lisa_absdef.Rda")
load("~/Desktop/Dir/resubmission/controls/thousand_controls.Rda")
load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")
load("~/Desktop/Dir/genus/sfa_7yrs.Rda")

indx <- sapply(sfa, is.factor)
sfa[indx]<- lapply(sfa[indx], function(x) as.numeric(as.character(x)))


isonames <- (b32b$iso)
n= 400
thousand <- thousand$V1

falserand <- setNames(data.frame(matrix(ncol = 2, nrow = n)), c("iso", "start_year"))
falserand$iso <- sample(isonames, size=n, rep=TRUE)
falserand$start_year <- sample(thousand, size=n, rep=TRUE)

#remove duplicates, but keep one!!!!
falserand <- distinct(falserand)

####remove events within 2 years of each other
falserand <- arrange(falserand, iso, start_year)

gah <- falserand %>% group_by(iso) %>% mutate(next_diff = lead(start_year)-start_year,
                                              next_diff_lag = lag(next_diff))

gahb <-filter(gah, (is.na(next_diff) | next_diff>2) & (next_diff_lag>2 | is.na(next_diff_lag)))

gahb <- gahb[,c(1:2)]
wow <- left_join(gahb, sfa, by=c("iso", "start_year"))
wow<- wow[complete.cases(wow),]
wow <- wow[,4:8]
wow$start <- as.numeric(as.character(wow$start))

wow$avg = rowMeans(wow, na.rm=TRUE)

raw<- wow
save(raw, file="raw_sfa.Rda")

wow2 <- (wow[,1:5]-wow$avg)/wow$avg

absdef2 <- wow2 %>% summarise_all(mean)

save(absdef2, file="lisa_absdef.Rda")

rm(list=ls()) 


wrapper <- function(){
  
  setwd("~/Desktop/Dir/lisa/controls")
  load("~/Desktop/Dir/lisa/controls/results.Rda")
  load("~/Desktop/Dir/lisa/controls/raw_sfa.Rda")
  load("~/Desktop/Dir/lisa/controls/lisa_absdef.Rda")
  load("~/Desktop/Dir/resubmission/controls/thousand_controls.Rda")
  load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")
  load("~/Desktop/Dir/genus/sfa_7yrs.Rda")
  
  indx <- sapply(sfa, is.factor)
  sfa[indx]<- lapply(sfa[indx], function(x) as.numeric(as.character(x)))
  
  
  
  isonames <- (b32b$iso)
  n= 400
  thousand <- thousand$V1
  
  falserand <- setNames(data.frame(matrix(ncol = 2, nrow = n)), c("iso", "start_year"))
  falserand$iso <- sample(isonames, size=n, rep=TRUE)
  falserand$start_year <- sample(thousand, size=n, rep=TRUE)
  
  #remove duplicates, but keep one!!!!
  falserand <- distinct(falserand)
  
  ####remove events within 2 years of each other
  falserand <- arrange(falserand, iso, start_year)
  
  gah <- falserand %>% group_by(iso) %>% mutate(next_diff = lead(start_year)-start_year,
                                                next_diff_lag = lag(next_diff))
  
  gahb <-filter(gah, (is.na(next_diff) | next_diff>2) & (next_diff_lag>2 | is.na(next_diff_lag)))
  
  
  gahb <- gahb[,c(1:2)]
  wow <- left_join(gahb, sfa, by=c("iso", "start_year"))
  wow<- wow[complete.cases(wow),]
  wow <- wow[,4:8]
  wow$start <- as.numeric(as.character(wow$start))
  
  wow$avg = rowMeans(wow, na.rm=TRUE)
  
  raw <- rbind(raw, wow)
  save(raw, file="raw_sfa.Rda")
  
  wow2 <- (wow[,1:5]-wow$avg)/wow$avg
  
  boop <- wow2 %>% summarise_all(mean)
  absdef2 <- rbind(absdef2, boop)
  
  save(absdef2, file="lisa_absdef.Rda")
  
  rm(list=ls()) 
  
}

replicate(1000, wrapper())


rm(list=ls())

load("~/Desktop/Dir/lisa/controls/lisa_absdef.Rda")

sfa_abscntrls <- as.data.frame(absdef2)
colnames(sfa_abscntrls) <- c("-2", "-1", "0", "+1", "+2")

setwd("~/Desktop/Dir/lisa/controls")
save(sfa_abscntrls, file="sfa_abscntrls.Rda")

rm(list=ls())
load("~/Desktop/Dir/lisa/controls/sfa_abscntrls.Rda")

sfa_abscntrls <- reshape::melt(sfa_abscntrls)
colnames(sfa_abscntrls) <- c("year", "normcomp")

sfa_abscntrls$year = factor(sfa_abscntrls$year,
                            levels = c("-2", "-1", "0", "+1", "+2"))


sfa_abscntrls <- sfa_abscntrls[complete.cases(sfa_abscntrls),]
hr <- sfa_abscntrls %>% group_by(year) %>%
  summarise(twenty = quantile(normcomp,0.25),
            seventy = quantile(normcomp, 0.75),
            median = median(normcomp))
hr$code = "sfa"
sfa_finalbox <- hr

setwd("~/Desktop/Dir/lisa/controls")
save(sfa_finalbox, file="sfa_finbox_abscntrls.Rda")


rm(list=ls())
load("~/Desktop/Dir/lisa/controls/sfa_abscntrls.Rda")

sfa_abscntrls <- reshape::melt(sfa_abscntrls)
colnames(sfa_abscntrls) <- c("year", "normcomp")

sfa_abscntrls$year = factor(sfa_abscntrls$year,
                            levels = c("-2", "-1", "0", "+1", "+2"))


sfa_abscntrls <- sfa_abscntrls[complete.cases(sfa_abscntrls),]
hr <- sfa_abscntrls %>% group_by(year) %>%
  summarise(five= quantile(normcomp, 0.05),
            twenty = quantile(normcomp,0.25),
            seventy = quantile(normcomp, 0.75),
            ninetyfive= quantile(normcomp,0.95),
            median = median(normcomp))
hr$code = "sfa"
sfa_finalbox <- hr

setwd("~/Desktop/Dir/lisa/controls")
save(sfa_finalbox, file="sfa_finbox_abscntrls2.Rda")


#beep(2)




rm(list=ls())



