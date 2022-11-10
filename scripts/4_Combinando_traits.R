#importacao de dados de atributos
#matriz para substituicao deve ter a coluna de Species e Genero 
#matriz de trait com os valores das especies

trait_total<-read.csv2(here::here("pasta_de_dados", "trait_foliar.csv"))
traitgen<-read.csv2(here::here("pasta_de_dados", "trait_foliargenero.csv"))

#matriz para substituir os dados
traitmatriz<-read.csv2(here::here("pasta_de_dados", "trait_preencher.csv"))

#dados de wd
wdtotal<-read.csv2(here::here("pasta_de_dados", "wd_total.csv"))
wdtotalgen<- read.csv2(here::here("pasta_de_dados", "wd_totalgenero.csv"))

#dados de N e P foliar
np<-read.csv2(here::here("pasta_de_dados", "n_p.csv"))
npgen<-read.csv2(here::here("pasta_de_dados", "n_pgenero.csv"))


#----
combine.trait <- function (matrix.to.fill, traits.species, traits.genus){
  print(matrix.to.fill)
  print(traits.species)
  print(traits.genus)
  combine.species <- merge(matrix.to.fill,traits.species, by.x="Species", by.y="Species")
    missing.species <- as.data.frame(setdiff(matrix.to.fill$Species, combine.species$Species))
    colnames(missing.species)<-"Species"
    yet.missing <- matrix.to.fill[which(matrix.to.fill$Species %in% missing.species$Species),] 
  combine.genus <- merge(yet.missing,traits.genus, by.x="Genero", by.y="Genero")
  all.species <- rbind(combine.species, combine.genus)

  print(setdiff(matrix.to.fill$Species,all.species$Species))
  print(setdiff(all.species$Species,matrix.to.fill$Species))
  
  return (all.species)
}

teste <- combine.trait(traitmatriz,trait_total,traitgen)
teste
teste <- combine.trait(teste,wdtotal,wdtotalgen)
teste
teste <- combine.trait(teste, np,npgen)
teste

library(dplyr)
teste <- teste %>%
  mutate(LA = as.numeric(LA)) %>%
  mutate(SLA = as.numeric(SLA)) %>%
  mutate(LDMC = as.numeric(LDMC)) %>%
  mutate(WD = as.numeric(WD)) %>%
  mutate(LNC = as.numeric(LNC)) %>%
  mutate(LPC = as.numeric(LPC)) 
  
  


##### salvando a matriz ####
#write.csv(teste, "traitok.csv")
