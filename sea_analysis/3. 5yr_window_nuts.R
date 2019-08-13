#example taken from sodium data

#load in raw GENuS data and then clean (i.e. remove NA values)

sodium <- read_csv("~/Desktop/genus/nutrient_sum_sodium.csv")
sodium <- select(sodium, -contains("_"))
sodium<- rename(sodium, iso=X1)
sodium[sodium== "*"] <- 0
sodium[sodium==0] <- NA

save(sodium, file="sodium.Rda")
sodium <- melt(sodium, id="iso")
colnames(sodium) <- c("iso", "start_year", "start")

#remove data entries that cannot have 5 year windows (i.e. because they're no data before 1964 or after 2010)
marker <- NULL
View(sodium)
marker = ddply(.data = sodium,
               .variables = c("iso"),
               .fun = function(x) {
                 rbind((head(x, n=3)),(tail(x, n=3)))
               }
)
marker$mark <- 1

sodium <- left_join(sodium, marker, by=c("iso", "start_year"))
sodium$start.y <- NULL
colnames(sodium) <- c("iso", "start_year", "start", "mark")
sodium <- arrange(sodium, iso, start_year)
sodium <- subset(sodium, is.na(sodium$mark))


#create 5 year windows for the remaining data entries
sodium <- cbind(min2 = lag(sodium$start, n=2, default= NA),
              min1 = lag(sodium$start, n=1, default = NA),
              sodium,
              plus1 = lead(sodium$start, n=1, default= NA),
              plus2 = lead(sodium$start, n=2, default=NA),
)

save(sod, file= "sod_5yrs.Rda")


