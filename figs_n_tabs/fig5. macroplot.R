
load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new_macro.Rda")
load("/Users/awesomesauce/Desktop/Dir/lisa/country_groups.Rda")
abs_estimates_new <- abs_estimates_new_macro

wanted <- c("5802", "5815")

ay <- filter(country_groups, code %in% wanted)
ay$code <- as.character(ay$code)
abs <- left_join(abs_estimates_new, ay, by="iso")
abs <- abs[complete.cases(abs),]

abs[,2:6]= (abs[,2:6]-abs$avg)/abs$avg
abs$iso <- NULL
res<- abs %>% group_by(code.x, code.y) %>% summarise_each(mean)

bo <- res[,1:7]

colnames(bo)<- c("code", "group", "0", "-2", "-1", "+1", "+2")
bo[bo == "5802"] <- "Landlocked Developing"
bo[bo == "5815"] <- "Low Income Food Deficit"

bo2 <- reshape::melt(bo, by=c("code", "group"))

bo2$variable <- factor(bo2$variable, levels = c("-2", "-1", "0", "+1", "+2"))
bo2$code <- c(rep("Calories", 10), rep("Carbohydrates", 10),  rep("Monounsaturated FA",10), rep("Protein", 10), rep("Polyunsaturated FA", 10), rep("Saturated FA", 10))



load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new_macro.Rda")

avg_est <- abs_estimates_new_macro[,-c(1,8)]
avg_est[,1:5] <- (avg_est[,1:5]-avg_est$avg)/avg_est$avg
avg_est$avg <- NULL

avgest <- avg_est %>% group_by(code) %>% summarise_each(mean)
##View(avgest)

colnames(avgest)<- c("code", "0", "-2", "-1", "+1", "+2")

avgest2 <- as.data.frame(avgest)
avgest2 <- reshape::melt(avgest2, by=c("code"))

avgest2$variable <- factor(avgest2$variable, levels = c("-2", "-1", "0", "+1", "+2"))
hi <- avgest2

hi$code <- c("Calories", "Carbohydrates", "Monounsaturated FA", "Protein", "Polyunsaturated FA", "Saturated FA")

hi$group <- "All w/ 90th percentile EWEs"



load("~/Desktop/Dir/lisa/controls/cal_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/carbs_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/mufa_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/pufa_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/prot_finbox_abscntrls2.Rda")
load("~/Desktop/Dir/lisa/controls/sfa_finbox_abscntrls2.Rda")


hi_box <- rbind(cal_finalbox, carbs_finalbox, mufa_finalbox, pufa_finalbox, prot_finalbox, sfa_finalbox)

hi_box$code <- c(rep("Calories", 5), rep("Carbohydrates", 5),  rep("Monounsaturated FA",5), rep("Protein", 5), rep("Polyunsaturated FA", 5), rep("Saturated FA", 5))


cr <- ggplot()+geom_ribbon(data=hi_box, aes(year, ymin=five, ymax=ninetyfive, group=code), fill="#9c9b9b", alpha=0.8)+ geom_ribbon(data=hi_box, aes(year, ymin=twenty, ymax=seventy, group=code),fill="#565656" ,alpha=0.5)+ geom_line(data=hi_box, aes(year, median, group=code), color="#565656")+ geom_line(data=hi, aes(variable, value, group= code, color=group), size=1.3)+geom_line(data=bo2, aes(variable, value, group=interaction(code,group), color= group), size=1)+scale_color_manual(values=(c("black", "#a8dd76", "#f6a0d0")))+facet_wrap(~code)+theme_classic()+theme(legend.position="top")

cr + theme_bw() + theme(panel.border=element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank()) + scale_x_discrete(name="Year since event")+ theme(axis.line=element_line(colour="black", size=0.5, linetype="solid"))+ theme(legend.position="top", legend.title=element_blank(), legend.text = element_text(margin = margin(r = 10, unit = "pt"))) +
  theme(strip.text.x = element_text(face="bold"),
        strip.background = element_blank())+ scale_y_continuous(name="Normalized composite")+
  theme(panel.spacing = unit(-0.8, "lines"))+theme(text = element_text(size = 12))
