#from plot.R

rm(list=ls())
setwd("~/Desktop/Dir/lisa")

load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new.Rda")
load("/Users/awesomesauce/Desktop/Dir/lisa/country_groups.Rda")

wanted <- c("5801", "5802", "5815", "5817", "5706")

ay <- filter(country_groups, code %in% wanted)
ay$code <- as.character(ay$code)
abs <- left_join(abs_estimates_new, ay, by="iso")


abs <- abs[complete.cases(abs),]

##View(abs)

abs[,2:6]= (abs[,2:6]-abs$avg)/abs$avg
abs$iso <- NULL
res<- abs %>% group_by(code.x, code.y) %>% summarise_each(mean)

boop <- res[,1:7]
##View(boop)

colnames(boop)<- c("code", "group", "0", "-2", "-1", "+1", "+2")
boop[boop == "5801"] <- "Least Developed Countries"
boop[boop == "5802"] <- "Landlocked Developing"
boop[boop == "5815"] <- "Low Income Food Deficit"
boop[boop == "5817"] <- "Net Food-Importing Developing"
boop[boop == "5706"] <- "European Union"

boop <- as.data.frame(boop)
boop2 <- reshape::melt(boop, by=c("code", "group"))
##View(boop2)

boop2$variable <- factor(boop2$variable, levels = c("-2", "-1", "0", "+1", "+2"))


# boop2$grpee <- paste(boop2[,1],boop2[,2])
boop2<- arrange(boop2, code)

boop2$code <- c(rep("Vitamin B6", 25), rep("Calcium", 25), rep("Copper", 25), rep("Fiber", 25), rep("Folate", 25), rep("Iron", 25), rep("Magnesium", 25), rep("Niacin", 25), rep("Phosphorus", 25), rep("Potassium",25), rep("Riboflavin", 25), rep("Sodium", 25), rep("Thiamin", 25), rep("Vitamin A",25), rep("Vitamin C",25), rep("Zinc",25))


#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------

load("~/Desktop/Dir/lisa/controls/zinc_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/vitc_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/vita_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/bsix_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/thiamin_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/sod_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/ribo_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/niac_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/mag_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/pot_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/phos_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/iron_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/folate_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/fiber_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/cop_finbox_abscntrls.Rda")
load("~/Desktop/Dir/lisa/controls/calcium_finbox_abscntrls.Rda")


#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------
#------------------#------------------#------------------#------------------
boop2
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

hi_box <- rbind(zinc_finalbox, vitc_finalbox, vita_finalbox, bsix_finalbox, thiamin_finalbox, sod_finalbox, ribo_finalbox, niac_finalbox, mag_finalbox, pot_finalbox, phos_finalbox, iron_finalbox, folate_finalbox, fiber_finalbox, cop_finalbox, calcium_finalbox)

gr <- c(rep("Zinc", 5), rep("Vitamin C", 5), rep("Vitamin A", 5), rep("Vitamin B6", 5), rep("Thiamin", 5), rep("Sodium", 5), rep("Riboflavin", 5), rep("Niacin", 5), rep("Magnesium", 5), rep("Potassium",5), rep("Phosphorus", 5), rep("Iron", 5), rep("Folate", 5), rep("Fiber",5), rep("Copper",5), rep("Calcium",5))

hi_box$code <- gr
hi$code <- c("Vitamin B6", "Calcium", "Copper", "Fiber", "Folate", "Iron", "Magnesium", "Niacin", "Phosphorus", "Potassium", "Riboflavin", "Sodium", "Thiamin", "Vitamin A", "Vitamin C", "Zinc")

hi$group <- "All w/ 90th percentile EWEs"
colnames(hi)


cr<- ggplot()+geom_line(data=hi, aes(variable, value, group= code, color=group), size=1.3)+ geom_line(data=boop2, aes(variable, value, group=interaction(code,group), color= group), size=0.6)+scale_color_manual(values=(c("black", "#ec9032", "#a8dd76", "#66d4c6", "#f6a0d0", "#ffcc5c")))+facet_wrap(~code)+theme_classic()+theme(legend.position="top")


cr<- ggplot()+ geom_line(data=boop2, aes(variable, value, group=interaction(code,group), color= group), size=0.8)+geom_line(data=hi, aes(variable, value, group= code, color=group))+scale_color_manual(values=(c("black", "#ec9032", "#a8dd76", "#66d4c6", "#f6a0d0", "#ffcc5c")))+facet_wrap(~code)+theme_classic()+theme(legend.position="top")

# orange, green, blue, pink, yellow

cr + theme_bw() + theme(panel.border=element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank()) + scale_x_discrete(name="Year since event")+ theme(axis.line=element_line(colour="black", size=0.5, linetype="solid"))+ theme(legend.position="top", legend.title=element_blank(), legend.text = element_text(margin = margin(r = 10, unit = "pt"))) +guides(color=guide_legend(nrow=2))+
  theme(strip.text.x = element_text(face="bold"),
        strip.background = element_blank())+ scale_y_continuous(name="Normalized composite")+
  theme(panel.spacing = unit(-0.8, "lines"))+theme(text = element_text(size = 12))


#redo_means
