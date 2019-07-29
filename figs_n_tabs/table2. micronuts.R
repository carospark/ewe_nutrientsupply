
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


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


load("/Users/awesomesauce/Desktop/Dir/lisa/rel_dip_specgrp.Rda")
load("/Users/awesomesauce/Desktop/Dir/lisa/abs_dip_specgrp.Rda")

rownames(dip)<- c("mean", "lowci", "highci")
setDT(dip, keep.rownames=TRUE)


dip2 <- melt(dip, by=rn)
dip2 <- cbind(dip, colsplit(dip$variable, "_", c("code", "group")))

dip3 <- dcast(dip2, code~ rn+group, value.var="value")

abs_spec <- dip3[,c(1,
                    12,7,2, 
                    13,8,3,
                    14,9,4,
                    15,10,5,
                    16,6,11)]


rownames(reldiff)<- c("mean", "lowci", "highci")
setDT(reldiff, keep.rownames=TRUE)

reldiff2 <- melt(reldiff, by=rn)
reldiff3 <- cbind(reldiff2, colsplit(reldiff2$variable, "_", c("code", "group")))

reldiff3 <- dcast(reldiff3, code~ rn+group, value.var="value")
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
#View(abs_preci)

rel_spec_table <- rel_spec_table[,c(16,1:15)]
rel_spec_table<- arrange(rel_spec_table, code) 
rel_preci <- rel_spec_table




#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


rel1 <- left_join(c, rel_preci, by="code")
rel1 <- rel1[,-c(2:4)]
#View(rel1)

rel1[,2:4]<- rel1[,2:4]*100
rel1[,2:19] <-round(rel1[,2:19], digits=2)


rel1$AVGrel_mean <- paste("\\makecell{\\num{",rel1$AVGrel_mean,"}\\\\(\\num{",rel1$AVGrel_lowci,"}, \\num{",rel1$AVGrel_highci,"})}"," &",sep="")
rel1$mean_5706 <- paste("\\makecell{\\num{",rel1$mean_5706,"}\\\\(\\num{",rel1$lowci_5706,"}, \\num{",rel1$highci_5706,"})}"," &",sep="")

rel1$mean_5801 <- paste("\\makecell{\\num{",rel1$mean_5801,"}\\\\(\\num{",rel1$lowci_5801,"}, \\num{",rel1$highci_5801,"})}"," &",sep="")
rel1$mean_5802 <- paste("\\makecell{\\num{",rel1$mean_5802,"}\\\\(\\num{",rel1$lowci_5802,"}, \\num{",rel1$highci_5802,"})}"," &",sep="")
rel1$mean_5815 <- paste("\\makecell{\\num{",rel1$mean_5815,"}\\\\(\\num{",rel1$lowci_5815,"}, \\num{",rel1$highci_5815,"})}"," &",sep="")
rel1$mean_5817 <- paste("\\makecell{\\num{",rel1$mean_5817,"}\\\\(\\num{",rel1$lowci_5817,"}, \\num{",rel1$highci_5817,"})}"," &",sep="")
rel1$code <- paste(rel1$code," &",sep="")


rel2 <- rel1[,c(1,4,5,11,8,14,17)]

rel2<- arrange(rel2, code)

colnames(rel2)<- c("Nutrient", "All countries incl. in study", "European Union", "Landlocked Developing Countries", "Least Developed Countries", "Low Income Food Deficit Countries", "Net Food-Importing Developing Countries")

#write.csv(rel2, file="rel_dips_specialgrps.csv")







