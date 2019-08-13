load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new.Rda")

dat <- abs_estimates_new[,c(2,7,9)]
dat <- dat %>% mutate(abs_diff = start-avg,
                            rel_diff = abs_diff/avg)

boots <- dat[,c(3,5)]
boots$id <- rep(1:151, 16)

boots_percent <- dcast(boots, formula=id~code, value.var="rel_diff")
boots_percent$id <- NULL

data <- boots_percent

meanfun <- function(data, i){
  d <- data[i]
  return(mean(d))
}


myfun <- function(x){
  bo <- boot(x, statistic=meanfun, R=1000)
  merg<- boot.ci(bo, conf=0.95, type="bca")
  #  c(merg$bca[4:5], bo$t0)
  c(merg$bca[4:5], bo$t0)
}

percentage_boot <- sapply(data, myfun)

save(percentage_boot, file="percentage_dip_boot.Rda")

#---------------------------------------------------------------------------------------------------------------



rm(list=ls())

load("/Users/awesomesauce/Desktop/Dir/lisa/abs_estimates_new.Rda")

dat <- abs_estimates_new[,c(2,7,9)]
dat <- dat %>% mutate(abs_diff = start-avg,
                            rel_diff = abs_diff/avg)


boots <- dat[,c(3,4)]
boots$id <- rep(1:151, 16)

boots_absdiff <- dcast(boots, formula=id~code, value.var="abs_diff")
boots_absdiff$id <- NULL

data <- boots_absdiff

meanfun <- function(data, i){
  d <- data[i]
  return(mean(d))   
}


myfun <- function(x){
  bo <- boot(x, statistic=meanfun, R=1000)
  merg<- boot.ci(bo, conf=0.95, type="bca")
  #  c(merg$bca[4:5], bo$t0)
  c(merg$bca[4:5], bo$t0)
}

absdiff_boot <- sapply(data, myfun)


save(absdiff_boot, file="abs_dip_boot_ci.Rda")



