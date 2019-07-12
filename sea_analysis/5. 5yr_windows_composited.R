#from may 27 redo regional analysis

rm(list=ls())
setwd("~/Desktop/Dir/github")

load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new.Rda")
load("/Users/awesomesauce/Desktop/Dir/lisa/country_groups.Rda")


wanted <- c("5801", "5802", "5815", "5817", "5706")

ay <- filter(country_groups, code %in% wanted)
ay$code <- as.character(ay$code)
abs <- left_join(abs_estimates_new, ay, by="iso")


abs <- abs[complete.cases(abs),]

View(abs)

abs[,2:6]= (abs[,2:6]-abs$avg)/abs$sd
abs$iso <- NULL
res<- abs %>% group_by(code.x, code.y) %>% summarise_each(mean)

boop <- res[,1:7]
View(boop)

colnames(boop)<- c("code", "group", "0", "-2", "-1", "+1", "+2")
boop[boop == "5801"] <- "Least Developed Countries"
boop[boop == "5802"] <- "Landlocked Developing"
boop[boop == "5815"] <- "Low Income Food Deficit"
boop[boop == "5817"] <- "Net Food-Importing Developing"
boop[boop == "5706"] <- "European Union"

boop <- as.data.frame(boop)
boop2 <- reshape::melt(boop, by=c("code", "group"))
View(boop2)

boop2$variable <- factor(boop2$variable, levels = c("-2", "-1", "0", "+1", "+2"))


# boop2$grpee <- paste(boop2[,1],boop2[,2])
boop2<- arrange(boop2, code)



head(global)

global <- abs_estimates_new

global[,2:6]= (global[,2:6]-global$avg)/global$sd

global$iso <- NULL
global2<- global %>% group_by(code) %>% summarise_each(mean)

global2








