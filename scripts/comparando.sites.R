library(vegan)
library(broom)
library(usedist)
library(tidyr)
data("BCI")
data1 <- BCI[1:5,]
data2 <- BCI[1:5,]
data3 <- BCI[1:5,]


dissi_com <- function(data1, data2, data3) {
  resu = list()
  sitios <- rownames(data1)
  data1[is.na(data1)] = 0
  data2[is.na(data2)] = 0
  data3[is.na(data3)] = 0
  
  for (i in 1: length(sitios)) {
    comm_times <- rbind (data1, data2, data3)
    data <- subset(comm_times, sitios == sitios[i])
    dissim <- vegdist(data, method = "bray")
    sitios.novo.t1 <- paste(sitios[i], "t1", sep = "_")
    sitios.novo.t2 <- paste(sitios[i], "t2", sep = "_")
    sitios.novo.t3 <- paste(sitios[i], "t3", sep = "_")
    sitios.novo <- c(sitios.novo.t1,sitios.novo.t2,sitios.novo.t3)
    dissim <-dist_setNames(dissim, sitios.novo) 
    dissi <- tidy(dissim)
    resu [[i]] <- dissi
    
  }
  print(resu)
  return(resu)
}

teste <- dissi_com(data1, data2, data3)
resultados <- do.call(rbind, teste)
