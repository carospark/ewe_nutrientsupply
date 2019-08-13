
load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new.Rda")
load("/Users/awesomesauce/Desktop/Dir/lisa/country_groups.Rda")


wanted <- c("5801", "5802", "5815", "5817", "5706")

ay <- filter(country_groups, code %in% wanted)
ay$code <- as.character(ay$code)
abs <- left_join(abs_estimates_new, ay, by="iso")

abs <- abs[complete.cases(abs),]
abs[,2:6]= (abs[,2:6]-abs$avg)/abs$sd
abs$iso <- NULL
res<- abs %>% group_by(code.x, code.y) %>% summarise_each(mean)

res2 <- res[,1:7]
colnames(res2)<- c("code", "group", "0", "-2", "-1", "+1", "+2")
res2[res2 == "5801"] <- "Least Developed Countries"
res2[res2 == "5802"] <- "Landlocked Developing"
res2[res2 == "5815"] <- "Low Income Food Deficit"
res2[res2 == "5817"] <- "Net Food-Importing Developing"
res2[res2 == "5706"] <- "European Union"

res2 <- as.data.frame(res2)
res2 <- reshape::melt(res2, by=c("code", "group"))

res2$variable <- factor(res2$variable, levels = c("-2", "-1", "0", "+1", "+2"))
res2<- arrange(res2, code)


global <- abs_estimates_new
global[,2:6]= (global[,2:6]-global$avg)/global$sd
global$iso <- NULL
global2<- global %>% group_by(code) %>% summarise_each(mean)







