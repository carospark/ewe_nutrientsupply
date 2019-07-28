#example taken from polyunsaturated fatty acids supply data

#load in raw GENuS data and then clean (i.e. remove NA values)

pufa <- read_csv("~/Desktop/genus/nutrient_sum_25polyunsatFA.csv")
pufa<- select(pufa, -contains("_"))
pufa$YEAR <- NULL
pufa$X2<- NULL 
pufa <- pufa[4:178,]
pufa <- rename(pufa, iso=X1)
pufa[pufa== "*"] <- 0
nodata <- pufa[rowSums(pufa == "0") >=51, ]
pufa <- pufa[rowSums(pufa==0) <51,]
pufa[pufa==0] <- NA

save(pufa, file="pufa.Rda")
pufa <- melt(pufa, id="iso")
colnames(pufa) <- c("iso", "start_year", "start")

#remove data entries that cannot have 5 year windows (i.e. because they're no data before 1964 or after 2010)
marker <- NULL
View(pufa)
marker = ddply(.data = pufa,
               .variables = c("iso"),
               .fun = function(x) {
                 rbind((head(x, n=3)),(tail(x, n=3)))
               }
)
marker$mark <- 1

pufa <- left_join(pufa, marker, by=c("iso", "start_year"))
pufa$start.y <- NULL
colnames(pufa) <- c("iso", "start_year", "start", "mark")
pufa <- arrange(pufa, iso, start_year)
pufa <- subset(pufa, is.na(pufa$mark))


#create 5 year windows for the remaining data entries
pufa <- cbind(min2 = lag(pufa$start, n=2, default= NA),
              min1 = lag(pufa$start, n=1, default = NA),
              pufa,
              plus1 = lead(pufa$start, n=1, default= NA),
              plus2 = lead(pufa$start, n=2, default=NA),
)

save(pufa, file= "pufa_5yrs.Rda")


