#using saturated fatty acids as an example ("sfa")

wrapper <- function(){
   
  load("~/Desktop/Dir/lisa/controls/results.Rda")  #empty dataframe
  load("~/Desktop/Dir/lisa/controls/raw_sfa.Rda")  #empty dataframe
  load("~/Desktop/Dir/lisa/controls/lisa_absdef.Rda")  #empty dataframe
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
  
  #remove duplicates, but keep one!
  falserand <- distinct(falserand)
  
#remove events within 2 years of each other
  falserand <- arrange(falserand, iso, start_year)
  
  new_false <- falserand %>% group_by(iso) %>% mutate(next_diff = lead(start_year)-start_year,
                                                next_diff_lag = lag(next_diff))
  
  new_false <-filter(new_false, (is.na(next_diff) | next_diff>2) & (next_diff_lag>2 | is.na(next_diff_lag)))
  new_false <- new_false[,c(1:2)]
                     
sfa_final <- left_join(new_false, sfa, by=c("iso", "start_year"))
  sfa_final$avg = rowMeans(sfa_final, na.rm=TRUE)
  
  raw <- rbind(raw, sfa_final)
  save(raw, file="raw_sfa.Rda")
  
  perc_change <- (sfa_final[,1:5]-wow$avg)/wow$avg
  
 perc_change <-   perc_change %>% summarise_all(mean)
  absdef2 <- rbind(absdef2, perc_change)
  
  save(absdef2, file="lisa_absdef.Rda")
  
  rm(list=ls()) 
  
}

replicate(1000, wrapper())


rm(list=ls())

load("~/Desktop/Dir/lisa/controls/lisa_absdef.Rda")

sfa_abscntrls <- as.data.frame(absdef2)
colnames(sfa_abscntrls) <- c("-2", "-1", "0", "+1", "+2")
sfa_abscntrls <- reshape::melt(sfa_abscntrls)
colnames(sfa_abscntrls) <- c("year", "normcomp")

sfa_abscntrls$year = factor(sfa_abscntrls$year,
                            levels = c("-2", "-1", "0", "+1", "+2"))


sfa_abscntrls <- sfa_abscntrls[complete.cases(sfa_abscntrls),]
sfa_finalbox <- sfa_abscntrls %>% group_by(year) %>%
  summarise(five= quantile(normcomp, 0.05),
            twenty = quantile(normcomp,0.25),
            seventy = quantile(normcomp, 0.75),
            ninetyfive= quantile(normcomp,0.95),
            median = median(normcomp))
sfa_finalbox$code = "sfa"

save(sfa_finalbox, file="sfa_finbox_abscntrls2.Rda")



