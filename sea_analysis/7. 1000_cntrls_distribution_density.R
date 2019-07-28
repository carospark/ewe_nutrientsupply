
# from https://stats.stackexchange.com/questions/321542/how-can-i-draw-a-value-randomly-from-a-kernel-density-estimate


rdens <- function(n, density=z, data=x, kernel="gaussian") {
  width <- z$bw                              # Kernel width
  rkernel <- function(n) rnorm(n, sd=width)  # Kernel sampler
  sample(x, n, replace=TRUE) + rkernel(n)    # Here's the entire algorithm
}

n <- 160
load("~/Desktop/Dir/resubmission/vesknorm.Rda")
x <- vesknorm$start_year

# Compute a kernel density estimate.
# It returns a kernel width in $bw as well as $x and $y vectors for plotting.
#
z <- density(x, kernel="gaussian")
#
# Sample from the KDE.
#

density(y, kernel="gaussian")

hey<- function() {
  y <- rdens(n, z, x)
  y2 <- round(y[y<2008.5 & y>=1963.5])
}

thousand<- as.data.frame(do.call(rbind, replicate(1000, as.matrix(hey()), simplify=FALSE)))

save(thousand, file="thousand_controls.Rda")


#for plotting:
ay <- as.data.frame(thousand[1:1200,])
ay$group <- c(rep("a", 150), rep("b", 150), rep("c", 150), rep("d", 150), rep("e", 150), rep("f", 150), rep("g", 150), rep("h", 150))
colnames(ay) <- c("V1", "group")

ggplot(data=ay)+ geom_histogram(aes(x=V1, group=group, fill=group), binwidth=2, position="identity", alpha=.4)+theme_classic()+xlab("Control year of event")+ylab("Count")+theme(legend.position="top", legend.title=element_blank())+ guides(fill = guide_legend(nrow = 1))+theme(text=element_text(size=17))

#ggsave("8controls.png")


ba<- ggplot(data=thousand) + 
  geom_histogram(aes(x = V1, y=..density..),
                 binwidth=2,
                 colour="orange", fill="orange", alpha=.5) +
  geom_density(aes(x=V1), alpha=.5, color="#e36d5f", fill="#e36d5f")+ ylim(0,0.05)+theme_classic()+ylab("Density")+xlab("Control year of event")

ba+theme(text=element_text(size=17))


ca<- ggplot() + 
  geom_histogram(aes(x = x, y=..density..),
                 binwidth=2,
                 colour="orange", fill="orange", alpha=.5) +
  geom_density(aes(x=x), alpha=.5, color="#e36d5f", fill="#e36d5f")+theme_classic()+ylab("Density")+xlab("Actual year of event")


ca+theme(text=element_text(size=17))
grid.arrange(ca, ba, ncol=2)
#ggsave("controlyears.png")

ggsave("foo.png", arrangeGrob(ca, ba, ncol=2))



