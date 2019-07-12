#from adding africa to popdam_final



rm(list=ls())
load("~/Desktop/Dir/resubmission/alldis_alldata.Rda")




popdam_ex5 <- filter(filta, 
                     (ppop >= quantile(filta$ppop, 0.9, na.rm=TRUE)) |
                       (pgdp >= quantile(filta$pgdp, 0.9, na.rm=TRUE)) |
                       (sumaff >= quantile(filta$sumaff, 0.9, na.rm=TRUE)) | 
                       (sumdam >= quantile(filta$sumdam, 0.9, na.rm=TRUE))|
                       (totalaff >= quantile(filta$totalaff, 0.9, na.rm=TRUE))|
                       (totaldam >= quantile(filta$totaldam, 0.9, na.rm=TRUE)) )




# popdam_ex6 <- filter(filta, 
#                      (ppop >= quantile(filta$ppop, 0.85, na.rm=TRUE)) |
#                        (pgdp >= quantile(filta$pgdp, 0.85, na.rm=TRUE)) |
#                        (sumaff >= quantile(filta$sumaff, 0.85, na.rm=TRUE)) | 
#                        (sumdam >= quantile(filta$sumdam, 0.85, na.rm=TRUE))|
#                        (totalaff >= quantile(filta$totalaff, 0.85, na.rm=TRUE))|
#                        (totaldam >= quantile(filta$totaldam, 0.85, na.rm=TRUE)) )



popdam <- arrange(popdam_ex5, iso, start_year)
#popdam <- arrange(popdam_ex, iso, start_year)
#popdam <- arrange(popdam_orig, iso, start_year)


b32 <- popdam %>% group_by(iso) %>% mutate(next_diff = lead(start_year)-end_year,
                                           next_diff_lag = lag(next_diff))
b32 <- arrange(b32, iso, start_year)

b32b <-filter(b32, (is.na(next_diff) | next_diff>2) & (next_diff_lag>2 | is.na(next_diff_lag)))
View(b32b)



# b32 <- b32[,c(1,5,6,13,14)]
# b32b <- b32b[,c(1,5,6,13,14)]


save(b32b, file="popdam_final_relaxedthresh.Rda")


