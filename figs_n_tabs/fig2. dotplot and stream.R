#from may28 redo dot plot events 


rm(list=ls())
setwd("~/Desktop/Dir/lisa")




# Library
#devtools::install_github("hrbrmstr/streamgraph")

library(streamgraph)

# Create data:
year=rep(seq(1990,2016) , each=10)
name=rep(letters[1:10] , 27)
value=sample( seq(0,1,0.0001) , length(year))
data=data.frame(year, name, value)

#View(data)

# Basic stream graph: just give the 3 arguments
streamgraph(data, key="name", value="value", date="year", interactive=FALSE)






#------------------------------------------------------------------------
rm(list=ls())
load("~/Desktop/Dir/resubmission/popdam_final_relaxedthresh.Rda")
load("~/Desktop/Dir/resubmission/vesknorm.Rda")

#View(b32b)
#View(vesknorm)

vesk <- vesknorm[,c(1,3:4)]
vesk <- left_join(vesk, b32b, by=c("iso", "start_year", "end_year"))


#View(vesk)
vesk <- vesk[,c(1:4)]

vesk <- arrange(vesk, iso, start_year)
vesk$names <- rownames(vesk)
temp <- NULL
temp <- data.frame()
for(i in 1:nrow(vesk)){
  x = as.data.frame(rbind(c(vesk$iso[i], vesk$distype[i], vesk$names[i], (vesk$start_year[i]:vesk$end_year[i]))))
  temp <- rbind.fill(temp,x)
}


#View(temp2)
temp <- temp[!duplicated(temp),]

temp$V3<- NULL
temp2 <- melt(temp, id=c("V1", "V2"))
temp2$value <- as.numeric(as.character(temp2$value))
temp2 <- arrange(temp2, V1, V2, value)
#View(temp2)

temp2 <- temp2[complete.cases(temp2),]
temp2<- temp2[,-c(1,3)]
colnames(temp2) <- c("name", "year")
temp2$value <- 1

temp2$name<- as.character(temp2$name)
temp2[temp2=="Severe winter conditions"]<- "Extreme cold"
temp2[temp2=="Cold wave"]<- "Extreme cold"
temp2[temp2=="Heat wave"]<- "Extreme heat"
temp2[temp2=="Wildfire"]<- "Extreme heat"

unique(temp2$name)
temp3 <- temp2
temp2 <- temp2 %>% group_by(name, year) %>% summarise(sum(value))
temp2 <- dplyr::rename(temp2, value="sum(value)")

# Basic stream graph: just give the 3 arguments

#View(temp2)

streamgraph(temp2, key="name", value="value", date="year", interactive=TRUE)%>%sg_fill_manual(values=c("#D7191C", "darkorange", "#ffe700", "#ABDDA4", "#2B83BA"))
#streamgraph(temp2, key="name", value="value", date="year", interactive=TRUE, offset="zero") 


#webshot::install_phantomjs()
library(webshot)


# Make a webshot in pdf : high quality but can not choose printed zone
webshot("untitled.html", "output.pdf", delay = 0.2)



temp %>% group_by(V2) %>% count()




#------------------------------------------------------------------------

View(temp3)

brewer.pal(n = 5, name = 'Spectral')



ggplot(temp3, aes(x = year, fill = name)) +
  geom_dotplot(stackgroups = TRUE, binwidth = 1, binpositions = "all", color="white")+ 
  scale_x_continuous(name="Year of Event", breaks=c(1964, 1970, 1980, 1990, 2000, 2010))+scale_fill_manual(values= c("#D7191C", "darkorange", "#ffe700", "#ABDDA4", "#2B83BA"))+
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())+ theme(axis.title.y=element_blank(),
                                                                axis.text.y=element_blank(),
                                                                axis.ticks.y=element_blank())+
  theme(legend.position= c(.5,.47), legend.direction = "horizontal")+ 
  theme(legend.title=element_blank(), legend.background = element_blank())+theme(text=element_text(size=12))


ggsave("dots.png")
