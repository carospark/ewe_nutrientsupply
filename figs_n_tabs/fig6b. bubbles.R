# from june2 fooling around bubbles




#taken from jan28
#install.packages("packcircles")

rm(list=ls())
setwd("~/Desktop/Dir/lisa/controls")

# libraries
library(packcircles)


load("/Users/awesomesauce/Desktop/Dir/lisa/combined_lolli.Rda")
landlock <- combine[,c(1,5)]
landlock <- filter(landlock, code== "Folate" | code== "Magnesium"  | code== "Phosphorus"  | code== "Potassium"  | code== "Thiamin"  | code== "Vitamin B6"  | code== "Vitamin C" | code== "Zinc"  | code== "Calories"  )
landlock$mean_5802 <-landlock$mean_5802*-1 
colnames(landlock) <- c("Nutrient", "perc")
data = landlock

packing <- circleProgressiveLayout(data$perc, sizetype='radius')


#--------------------------------------------------------------------------

data2 = combine[,c(1,4)]
data2 <- filter(data2, code== "Folate" | code== "Magnesium"  | code== "Phosphorus"  | code== "Potassium"  | code== "Thiamin"  | code== "Vitamin B6"  | code== "Vitamin C" | code== "Zinc"  | code== "Calories"  )
data2$AVGrel_mean <-data2$AVGrel_mean*-1 
colnames(data2) <- c("Nutrient", "perc")
packing2 <- circleProgressiveLayout(data2$perc, sizetype='radius')

packing2[,1:2] <- packing[,1:2]


# We can add these packing information to the initial data2 frame
data2 = cbind(data2, packing2)

View(data2)
View(packing2)

packing2[1,1:2] = packing2[1,1:2]+0.4
packing2[2,1:2] = packing2[2,1:2]-1
packing2[3,1:2] = packing2[3,1:2]+2
packing2[4,1:2] = packing2[4,1:2]+1.5
packing2[5,1:2]= packing2[5,1:2]-0.6
packing2[6,1:2]= packing2[6,1:2]-2
packing2[7,1:2] = packing2[7,1:2]-0.8
packing2[8,1:2]= packing2[8,1:2]+2
#packing2[9,1:2]= packing2[9,1:2]


dat.gg2 <- circleLayoutVertices(packing2, npoints=100)


#--------------------------------------------------------------------------

# We can add these packing information to the initial data frame
data = cbind(data, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=100)



arg <- c("#F8766D", "#EC8239", "#DB8E00" ,"#C79800", "#AEA200", "#8FAA00" , "white", "#64B200", "#00B81B", "#00BD5C","#00C085", "#00C1A7", "#00BFC4" ,"#00BADE", "#00B2F3" ,"#00A6FF", "#7C96FF" ,"#B385FF", "#D874FD", "#EF67EB", "#FD61D3", "#FF63B6", "#FF6B94")
arg

data$Nutrient

# Make the plot
bubblz <- ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "white", alpha = 0.6) +
  geom_polygon(data = dat.gg2, aes(x, y, group = id, fill=as.factor(id)))+ scale_fill_manual(values= c("#00B81B", "#FD61D3", "#FF63B6", "#B385FF", "#00BADE", "#D874FD", "#00BFC4", "#FF6B94", "#00BD5C")) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=perc, label = Nutrient), color="black") +
  scale_size_continuous(range = c(3.5,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") 


bubblz

ggsave("bubbles2.png", height=4.5, width=3.1)

#rm(list=ls())


