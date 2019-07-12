#from aug 23 .R



# setwd("~/Desktop/Dir/genus")
# 
# library(tidyr)
# library(plyr)
# library(dplyr)
# library(ggplot2)
# library(RColorBrewer)
# library(reshape2)
# library("ggthemes")
# library(tm)
# library(scales)
# library(readr)

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




load("~/Desktop/Dir/genus/pufa.Rda")

pufa <- melt(pufa, id="iso")
colnames(pufa) <- c("iso", "start_year", "start")


marker <- NULL
View(pufa)
marker = ddply(.data = pufa,
               .variables = c("iso"),
               .fun = function(x) {
                 rbind((head(x, n=3)),(tail(x, n=3)))
               }
)
marker$mark <- 1
View(marker)

pufa <- left_join(pufa, marker, by=c("iso", "start_year"))
View(pufa)
pufa$start.y <- NULL
colnames(pufa) <- c("iso", "start_year", "start", "mark")

pufa <- arrange(pufa, iso, start_year)

##################################################now analysis

pufa <- cbind(min3 = lag(pufa$start, n=3, default=NA),
              min2 = lag(pufa$start, n=2, default= NA),
              min1 = lag(pufa$start, n=1, default = NA),
              pufa,
              plus1 = lead(pufa$start, n=1, default= NA),
              plus2 = lead(pufa$start, n=2, default=NA),
              plus3 = lead(pufa$start, n=3, default=NA)
)
View(pufa)                   

#take out things that don't have 5 year windows
pufa <- subset(pufa, is.na(pufa$mark))
pufa$mark <- NULL


pufa[rowSums(pufa == "0") >=51, ]

pufa<- pufa[rowSums(is.na(pufa))<6,]

pufa <- pufa[complete.cases(pufa),]

save(pufa, file= "pufa_7yrs.Rda")

rm(list = ls())


