
load("/Users/awesomesauce/Desktop/Dir/lisa/combined_lolli.Rda")

z <- combine[,1:4]
z$group = "all"
y<- combine[,c(1,5:7)]
y$group= "landlock"
colnames(z) <- c("Nutrient", "low", "high", "diff", "group")
colnames(y) <- c("Nutrient", "diff", "low", "high", "group")

recomb <- rbind(z,y)

recomb <- arrange(recomb, rev(group), -diff)
recomb <- recomb[c(1,2,5,8,9,12 ,  3,4,6,7,10,11,13:44),]

recomb[c(1,2,8,9,5,12),]
rownames(recomb) <- NULL
recomb<- recomb[c(1:6, 21, 7:44, 43),]
rownames(recomb) <- NULL
recomb[7,1]<- "blank"
recomb[46,1]<- "blank"


recomb$Nutrient<- factor(recomb$Nutrient, levels=recomb$Nutrient)



recomb$grp_1 <- c(rep(2,23), rep(3,23))
recomb$grp_3 <- c(rep(1,23), rep(1.5,23))
recomb$grp_2 <- c("#F8766D", "#EC8239", "#DB8E00" ,"#C79800", "#AEA200", "#8FAA00" , "white", "#64B200", "#00B81B", "#00BD5C","#00C085", "#00C1A7", "#00BFC4" ,"#00BADE", "#00B2F3" ,"#00A6FF", "#7C96FF" ,"#B385FF", "#D874FD", "#EF67EB", "#FD61D3", "#FF63B6", "#FF6B94", rep("darkblue", 22), "white")



recomb <- transform(recomb, z0 = as.numeric(Nutrient))

ggplot(recomb, aes(x=diff, xmin=low, xmax=high, y=Nutrient, group=group))+ 
  geom_vline(xintercept=0, color="gray")+
  geom_segment(aes(x=low, xend=high, y=Nutrient, yend=Nutrient,color=I(grp_2), group=group, size=I(grp_3)))+
  geom_point(aes(x=diff, y=Nutrient, color=I(grp_2), group=group, size=I(grp_1)))+
  geom_hline(aes(yintercept=7), color="gray", linetype=2)+scale_y_discrete()+
  
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+ theme(legend.position = "none")+
  xlab("Estimated percentage change in supply")+
  theme(text=element_text(size=16))+theme(axis.title=element_text(size=13))+
  theme(axis.title.y=element_blank())+scale_x_continuous(limits=c(-20,5))


#ggsave("allnutrient_cis_perc.jpg", width=7, height=8.5)



