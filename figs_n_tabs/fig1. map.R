
#from may27 subgroups map

setwd("~/Desktop/Dir/github")

rm(list=ls())


## POSITIVE IS HUMAN AFFECTED, NEGATIVE IS FINANCIAL DAMAGE


setwd("~/Desktop/Dir/lisa")
load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")
load("~/Desktop/Dir/resubmission/vesknorm.Rda")

vesk <- vesknorm[,c(1,3:4)]
vesk <- left_join(vesk, b32b, by=c("iso", "start_year", "end_year"))

vesk <- vesk[,c(1,11,12)]
vesk[vesk==0]<- NA
vesk$pgdp <- vesk$pgdp*-1

View(vesk)

vesk$combo <- vesk$ppop+vesk$pgdp
vesk <- vesk %>% mutate( combo = ifelse(is.na(pgdp),95,combo),
                         combo= ifelse(is.na(ppop), -95, combo))


vesk <- vesk[,c(1,4)]
View(vesk)
world <- map_data("world") 
base <- ggplot(data=world, mapping=aes(x=long, y=lat, group=group))+coord_fixed(1.3)+geom_polygon(fill="#dfdfdf")
world$region <- iso.alpha(world$region, n=3)

world <- dplyr::rename(world, iso=region)




b32b <- unique(b32b$iso)
pops <- as.data.frame(b32b)
exp <- unique(vesk$iso)
heyhey<- data.frame(iso =setdiff(b32b, exp), "combo"= NA)
vesk2 <- full_join(vesk, heyhey, by=c("iso"))
vesk2<- vesk2[,1:2]
colnames(vesk2) <-c("iso", "combo") 

View(vesk3)
vesk3 <- arrange(vesk2, combo)

vesk3[36:79,2] <- vesk3[36:79,2]-10
vesk3[80:106,2] <- vesk3[80:106,2]+10

rawr <- inner_join(world, vesk3, by="iso")

colnames(rawr)


my.labels <- c("High $ damage, \n N/A ppl affected", "High $ damage, \n high ppl affected",
               "High ppl affected, \n N/A $ damage")


my.labels <- c("High financial damage, \n N/A people affected", "High financial damage, \n high people affected",
               "High people affected, \n N/A financial damage")


base <- ggplot(data=world, mapping=aes(x=long, y=lat, group=group))+coord_fixed(1.3)+geom_polygon(fill="#eaeaea")+ coord_cartesian(ylim = c(-50, 90))

View(vesk3)

base + geom_polygon(data = rawr, aes(fill= combo)) + 
  
  scale_fill_gradient(low = "orange", high = "purple", na.value="#bababa",
                      breaks=c(-90,0,80),labels= my.labels)+
  guides(fill = guide_colorbar(ticks = TRUE, barwidth=27, barheight=1.2, direction="horizontal"))+
  theme_classic()+
  theme(axis.title=element_blank(), axis.ticks=element_blank(), axis.line = element_blank())+ theme(axis.text = element_blank())+
  theme(legend.position="bottom", legend.box="horizontal", legend.title= element_blank(), legend.text = element_text(size=10),
        legend.margin = margin(unit(c(-10,0,0,0),"cm")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())





#ggsave("newmap.png", width=8.6, height=5)





#---------------------------------------------------------------



rm(list=ls())
