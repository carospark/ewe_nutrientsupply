
load("/Users/awesomesauce/Desktop/Dir/lisa/abs_dip_boot_ci.Rda")
a <- as.data.frame(t(absdiff_boot))
#View(a)
setDT(a, keep.rownames=TRUE)
a <- dplyr::rename(a, code= rn)
colnames(a)<- c("code","AVG_lowci", "AVG_highci", "AVG_mean")

load("/Users/awesomesauce/Desktop/Dir/lisa/percentage_dip_boot.Rda")
b <- as.data.frame(t(percentage_boot))
setDT(b, keep.rownames=TRUE)
b <- dplyr::rename(b, code= rn)
colnames(b)<- c("code","AVGrel_lowci", "AVGrel_highci", "AVGrel_mean")

c <- left_join(a,b, by="code")
code <- c("Vitamin B6", "Calcium", "Copper", "Fiber", "Folate", "Iron", "Magnesium", "Niacin", "Phosphorus", "Potassium", "Riboflavin", "Sodium", "Thiamin", "Vitamin A", "Vitamin C", "Zinc")
c$code <- code
#View(c)



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


load("/Users/awesomesauce/Desktop/Dir/lisa/rel_dip_specgrp.Rda")
load("/Users/awesomesauce/Desktop/Dir/lisa/abs_dip_specgrp.Rda")


rownames(dip)<- c("mean", "lowci", "highci")
setDT(dip, keep.rownames=TRUE)
dip <- melt(dip, by=rn)
dip2 <- cbind(dip, colsplit(gah$variable, "_", c("code", "group")))

absdip <- dcast(dip2, code~ rn+group, value.var="value")

abs_spec <- absdip[,c(1,
                    12,7,2, 
                    13,8,3,
                    14,9,4,
                    15,10,5,
                    16,6,11)]


rownames(reldiff)<- c("mean", "lowci", "highci")
setDT(reldiff, keep.rownames=TRUE)

reldiff2 <- melt(reldiff, by=rn)
reldiff2 <- cbind(reldiff2, colsplit(reldiff2$variable, "_", c("code", "group")))

reldiff3 <- dcast(reldiff2, code~ rn+group, value.var="value")
rel_spec <- reldiff3[,c(1,
                     12,7,2, 
                     13,8,3,
                     14,9,4,
                     15,10,5,
                     16,6,11)]

rel_spec[,2:16]<-rel_spec[,2:16]*100


abs_spec_table <- abs_spec[,-1]
rel_spec_table <- rel_spec[,-1]

abs_spec_table$code <- code
rel_spec_table$code <- code


abs_spec_table <- abs_spec_table[,c(16,1:15)]
abs_spec_table<- arrange(abs_spec_table, code) 
abs_preci <- abs_spec_table

rel_spec_table <- rel_spec_table[,c(16,1:15)]
rel_spec_table<- arrange(rel_spec_table, code) 
rel_preci <- rel_spec_table




#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c2<-c
c2[,5:7]<- c[,5:7]*100

landlock1 <- rel_preci[,c(1,8:10)]
landlock2 <- abs_preci[,c(1,8:10)]

fancy <- left_join(c2, landlock1, by="code") %>% left_join(., landlock2, by="code")

fancy[] <- lapply(fancy, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
#sapply(fancy, class)

adequate_indx <- c(0.2, 230, 210, NA, 72.5, 5.635, 52.5, 3, 187.5, 550, 0.35, 245, 0.25, 450, 45, 2.5)

rda <- c(0.5, 700, 340, 19, 150,    7,     80, 6, 460,  300,   0.5, 120, 0.5,  300, 15, 3)

fancy$adequate<- rda

fancy2<- fancy
##View(fancy2)

fancy3<- fancy2 %>% mutate(avg_inf = (AVG_mean/adequate),
                           lowci_inf = AVG_lowci/adequate,
                           highci_inf= AVG_highci/adequate,
                           inf_5802 = mean_5802.y/adequate,
                           low_inf5802 = lowci_5802.y/adequate,
                           high_inf5802 = highci_5802.y/adequate)


##View(fancy3)
fancy3[,15:20] = fancy3[,15:20]*100

fancygr <- round(fancy3[,-1], digits = 5)
##View(fancygr)

fancy4 <- round(fancy3[,-1], digits = 2)
##View(fancy4)

#fancy4<- as.data.frame(apply(fancy4,2, function(x){ifelse(abs(x)>100, -100, x) }) )
fancy4$code <- code
fancy4$adequate<- rda
#View(fancy4)

child_table <- fancy4[,c(20, 13,1:3,14:16, 10:12, 17:19)]
View(child_table)


child<- child_table
##View(child)
child$AVG_mean <- paste( "\\makecell{\\num{",child$AVG_mean,"}\\\\(\\num{",child$AVG_lowci,"}, \\num{",child$AVG_highci,"})}"," &",sep="")
child$avg_inf <- paste( "\\makecell{\\num{",child$avg_inf,"}\\\\(\\num{",child$lowci_inf,"}, \\num{",child$highci_inf,"})}"," &",sep="")
child$landlock_abs_dip <- paste( "\\makecell{\\num{",child$mean_5802.y,"}\\\\(\\num{",child$lowci_5802.y,"}, \\num{",child$highci_5802.y,"})}"," &",sep="")
child$landlock_inf <- paste( "\\makecell{\\num{",child$inf_5802,"}\\\\(\\num{",child$low_inf5802,"}, \\num{",child$high_inf5802,"})}"," \\\\",sep="")
child$code <- paste(child$code," &",sep="")
child$adequate <- paste(child$adequate," &",sep="")
View(child)
child2 <- child[,c(1,2,5,6,15,16)]
##View(child2)
colnames(child2)<- c("Nutrient", "Adequate Intake (AI) for infants", "Absolute difference from mean, all EWEs", "as percentage of AI", "Absolute difference from mean, Landlocked Developing Countries", "as percentage of AI")

write.csv(child2, file="child_rda.csv")
