
load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new.Rda")
load("/Users/awesomesauce/Desktop/Dir/lisa/country_groups.Rda")

wanted <- c("5801", "5802", "5815", "5817", "5706")

ay <- filter(country_groups, code %in% wanted)
ay$code <- as.character(ay$code)
abs <- left_join(abs_estimates_new, ay, by="iso")


abs <- abs[complete.cases(abs),]

#View(abs)

abs[,2:6]= (abs[,2:6]-abs$avg)/abs$avg
abs$iso <- NULL
res<- abs %>% group_by(code.x, code.y) %>% summarise_each(mean)

bo <- res[,1:7]

colnames(bo)<- c("code", "group", "0", "-2", "-1", "+1", "+2")
bo[bo == "5801"] <- "Least Developed Countries"
bo[bo == "5802"] <- "Landlocked Developing"
bo[bo == "5815"] <- "Low Income Food Deficit"
bo[bo == "5817"] <- "Net Food-Importing Developing"
bo[bo == "5706"] <- "European Union"

bo2 <- reshape::melt(bo, by=c("code", "group"))

bo2$variable <- factor(bo2$variable, levels = c("-2", "-1", "0", "+1", "+2"))

bo2$code <- c(rep("Vitamin B6", 25), rep("Calcium", 25), rep("Copper", 25), rep("Fiber", 25), rep("Folate", 25), rep("Iron", 25), rep("Magnesium", 25), rep("Niacin", 25), rep("Phosphorus", 25), rep("Potassium",25), rep("Riboflavin", 25), rep("Sodium", 25), rep("Thiamin", 25), rep("Vitamin A",25), rep("Vitamin C",25), rep("Zinc",25))


#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------

load("~/Desktop/Dir/lisa/controls/zinc_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/vitc_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/vita_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/bsix_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/thiamin_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/sod_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/ribo_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/niac_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/mag_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/pot_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/phos_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/iron_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/folate_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/fiber_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/cop_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/calcium_finbox_abscntrls2.Rda")

#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------
load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new.Rda")

avg_est <- abs_estimates_new[,-c(1,8)]
avg_est[,1:5] <- (avg_est[,1:5]-avg_est$avg)/avg_est$avg
avg_est$avg <- NULL

avgest <- avg_est %>% group_by(code) %>% summarise_each(mean)
#View(avgest)

colnames(avgest)<- c("code", "0", "-2", "-1", "+1", "+2")

avgest2 <- as.data.frame(avgest)
avgest2 <- reshape::melt(avgest2, by=c("code"))

avgest2$variable <- factor(avgest2$variable, levels = c("-2", "-1", "0", "+1", "+2"))
hi <- avgest2

load("~/Desktop/Dir/lisa/controls/zinc_finbox_abscntrls2.Rda")
a1<- zinc_finalbox
load("~/Desktop/Dir/lisa/controls/vitc_finbox_abscntrls2.Rda")
a2<-  vitc_finalbox
load("~/Desktop/Dir/lisa/controls/vita_finbox_abscntrls2.Rda")
a3<- vita_finalbox 
load("~/Desktop/Dir/lisa/controls/bsix_finbox_abscntrls2.Rda")
a4<- bsix_finalbox 
load("~/Desktop/Dir/lisa/controls/thiamin_finbox_abscntrls2.Rda")
a5<- thiamin_finalbox 
load("~/Desktop/Dir/lisa/controls/sod_finbox_abscntrls2.Rda")
a6<- sod_finalbox
load("~/Desktop/Dir/lisa/controls/ribo_finbox_abscntrls2.Rda")
a7<-ribo_finalbox
load("~/Desktop/Dir/lisa/controls/niac_finbox_abscntrls2.Rda")
a8<-niac_finalbox
load("~/Desktop/Dir/lisa/controls/mag_finbox_abscntrls2.Rda")
a9<-mag_finalbox 
load("~/Desktop/Dir/lisa/controls/pot_finbox_abscntrls2.Rda")
a10<-pot_finalbox 
load("~/Desktop/Dir/lisa/controls/phos_finbox_abscntrls2.Rda")
a11<- phos_finalbox
load("~/Desktop/Dir/lisa/controls/iron_finbox_abscntrls2.Rda")
a12<- iron_finalbox
load("~/Desktop/Dir/lisa/controls/folate_finbox_abscntrls2.Rda")
a13<- folate_finalbox 
load("~/Desktop/Dir/lisa/controls/fiber_finbox_abscntrls2.Rda")
a14<- fiber_finalbox 
load("~/Desktop/Dir/lisa/controls/cop_finbox_abscntrls2.Rda")
a15<- cop_finalbox
load("~/Desktop/Dir/lisa/controls/calcium_finbox_abscntrls2.Rda")
a16<- calcium_finalbox

hi_box <- rbind(a1, a2, a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16)

gr <- c(rep("Zinc", 5), rep("Vitamin C", 5), rep("Vitamin A", 5), rep("Vitamin B6", 5), rep("Thiamin", 5), rep("Sodium", 5), rep("Riboflavin", 5), rep("Niacin", 5), rep("Magnesium", 5), rep("Potassium",5), rep("Phosphorus", 5), rep("Iron", 5), rep("Folate", 5), rep("Fiber",5), rep("Copper",5), rep("Calcium",5))

hi_box$code <- gr
hi$code <- c("Vitamin B6", "Calcium", "Copper", "Fiber", "Folate", "Iron", "Magnesium", "Niacin", "Phosphorus", "Potassium", "Riboflavin", "Sodium", "Thiamin", "Vitamin A", "Vitamin C", "Zinc")

hi$group <- "All w/ 90th percentile EWEs"
colnames(hi)


hi_box2 <- rbind(zinc_finalbox, vitc_finalbox, vita_finalbox, bsix_finalbox, thiamin_finalbox, sod_finalbox, ribo_finalbox, niac_finalbox, mag_finalbox, pot_finalbox, phos_finalbox, iron_finalbox, folate_finalbox, fiber_finalbox, cop_finalbox, calcium_finalbox)

hi_box2$code <- gr



library("ggnewscale")


# 
ribs <- ggplot()+ theme_classic()+ geom_ribbon(data=hi_box, aes(year, ymin=five, ymax=ninetyfive, group=code, fill=code))+ scale_fill_hue(c=50, l=90) +new_scale_fill()+ geom_ribbon(data=hi_box, aes(year, ymin=twenty, ymax=seventy, group=code, fill=code))

dr<- ribs + geom_line(data=hi_box, aes(year, median, color=code, group=code))+scale_color_hue(l=50)+geom_line(data=hi, aes(variable, value, group= code), size=1.3)+facet_wrap(~code)

dr + theme_bw() + theme(panel.border=element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank()) + scale_x_discrete(name="Year since event")+
  theme(axis.line=element_line(colour="black", size=0.5, linetype="solid"))+ theme(legend.position="none") +
  theme(strip.text.x = element_text(face="bold"),
        strip.background = element_blank())+ scale_y_continuous(name="Normalized composite")+theme(panel.spacing = unit(-0.8, "lines"))




#---------------------------------------------------

cr <- ggplot()+geom_ribbon(data=hi_box, aes(year, ymin=five, ymax=ninetyfive, group=code), fill="#a9a8a8", alpha=0.8)+ geom_ribbon(data=hi_box, aes(year, ymin=twenty, ymax=seventy, group=code),fill="#696969" ,alpha=0.5)+ geom_line(data=hi_box, aes(year, median, group=code), color="#696969")+ geom_line(data=hi, aes(variable, value, group= code), size=1.3)+facet_wrap(~code)


cr + theme_bw() + theme(panel.border=element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank()) + scale_x_discrete(name="Year since event")+
  theme(axis.line=element_line(colour="black", size=0.5, linetype="solid"))+ theme(legend.position="none") +
  theme(strip.text.x = element_text(face="bold"),
        strip.background = element_blank())+ scale_y_continuous(name="Normalized composite")+theme(panel.spacing = unit(-0.8, "lines"))

