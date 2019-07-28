
load("~/Desktop/Dir/resubmission/alldis_alldata.Rda")

#find 90th percentile for total damage, total persons affected, and the proportion of people affected per population or financial damage per GDP 
popdam <- filter(combined, 
                     (ppop >= quantile(filta$ppop, 0.9, na.rm=TRUE)) |
                       (pgdp >= quantile(filta$pgdp, 0.9, na.rm=TRUE)) |
                       (sumaff >= quantile(filta$sumaff, 0.9, na.rm=TRUE)) | 
                       (sumdam >= quantile(filta$sumdam, 0.9, na.rm=TRUE))|
                       (totalaff >= quantile(filta$totalaff, 0.9, na.rm=TRUE))|
                       (totaldam >= quantile(filta$totaldam, 0.9, na.rm=TRUE)) )


popdam <- arrange(popdam, iso, start_year)

#find EWEs that overlap with other EWEs within the 5 year window and remove all
b32 <- popdam %>% group_by(iso) %>% mutate(next_diff = lead(start_year)-end_year,
                                           next_diff_lag = lag(next_diff))
b32 <- arrange(b32, iso, start_year)
b32b <-filter(b32, (is.na(next_diff) | next_diff>2) & (next_diff_lag>2 | is.na(next_diff_lag)))

save(b32b, file="popdam_final_relaxedthresh.Rda")


