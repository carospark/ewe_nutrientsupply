#from sept 6 multidisaster


setwd("~/Desktop/Dir/august katja")
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)



load("~/Desktop/Dir/NEW regional special/popdam_7yrsgrp.Rda")
guh <- filter(popdam_7yrsgrp, code=="5802")
landlock<- unique(guh$iso)
landlock <- as.data.frame(landlock)
colnames(landlock) <- c("iso")

load("/Users/awesomesauce/Desktop/Dir/june/emdat_combined_final.Rda")

landlock <- left_join(landlock, emdat, by="iso")

#half half split between n<5 and n>=5

fruitprod1000box[fruitprod1000box=="min2"] <- "-2"

landlock[landlock=="Wildfire"] <- "Extreme heat"
landlock[landlock=="Heat wave"] <- "Extreme heat"
landlock[landlock=="Cold wave"] <- "Extreme cold"
landlock[landlock=="Severe winter conditions"] <- "Extreme cold"

types <- landlock %>% count(iso, distype)

#17 countries so...

distype = unique(landlock$distype)
distype = rep(distype, 17)
iso = unique(landlock$iso)
iso <- rep(iso,5)
df=data.frame(iso, distype)  

df<- arrange(df, iso)
df_yas <- NULL
df_yas <- left_join(df, types, by=c("iso", "distype"))

df_yas[df_yas<5] <- 1
df_yas[is.na(df_yas)] = 0
df_yas$n[df_yas$n>=5] <- 2


df_yas$distype <- factor(df_yas$distype,
                         levels=c("Storm", "Flood", "Extreme heat", "Extreme cold", "Drought"))


#for #1
df_yas$distype <- factor(df_yas$distype,
                         levels=c("Flood", "Storm", "Drought", "Extreme cold", "Extreme heat"))

#for image #2
df_yas$distype <- factor(df_yas$distype,
                         levels=c("Flood", "Extreme cold", "Drought", "Storm", "Extreme heat"))


ggplot()+geom_bar(data=df_yas, aes(x=iso, y=1, fill=(distype), alpha=as.factor(n)),  stat="identity")+
  coord_polar(theta="x")+
  scale_fill_manual(values= (c("#f96464", "#00f6d7", "#f6a200", "#80ba00","#8500f6")))+
  scale_alpha_discrete(range = c(0, 1), guide="none")+theme_classic()+
  theme(axis.line=element_blank(),axis.title=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(legend.title=element_blank())+ guides(fill = guide_legend(reverse = TRUE))