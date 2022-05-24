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

#combinacao entre as especies das duas matrizes
combinacaofoliar<-merge(traitmatriz,trait_total, by.x="Species", by.y="Species");(combinacaofoliar)
faltantes <- as.data.frame(setdiff(traitmatriz$Species, combinacaofoliar$Species))
colnames(faltantes)<-"Species"; faltantes

tablefalt <- traitmatriz[which(traitmatriz$Species %in% faltantes$Species),] #combina todos os individuos da matriz total e da matriz de WD a partir dos treeID;table
tablefalt
tablegenero<-merge(tablefalt,traitgen, by.x="Genero", by.y="Genero");(tablegenero)
foliarok<-rbind(tablegenero, combinacaofoliar);foliarok

setdiff((traitmatriz$Species), (foliarok$Species))

combinacaowd<-merge(foliarok,wdtotal, by.x="Species", by.y="Species");head(combinacaowd)
faltantes <- as.data.frame(setdiff(traitmatriz$Species, combinacaowd$Species))
colnames(faltantes)<-"Species"
faltantes

tablefalt <- traitmatriz[which(traitmatriz$Species %in% faltantes$Species),] #combina todos os individuos da matriz total e da matriz de WD a partir dos treeID;table
tablefalt

tablegenero<-merge(tablefalt,wdtotalgen, by.x="Genero", by.y="Genero");(tablegenero)
comblinhas<-rbind(wdtotal[,1:2], tablegenero[,2:3]);comblinhas

wdok<-merge(foliarok,comblinhas, by.x="Species", by.y="Species");head(wdok)
setdiff(traitmatriz$Species, wdok$Species)

combinacaonp<-merge(wdok, np, by.x="Species", by.y="Species");(combinacaonp)
faltantes <- as.data.frame(setdiff(traitmatriz$Species, combinacaonp$Species))
colnames(faltantes)<-"Species"; faltantes
tablefalt <- traitmatriz[which(traitmatriz$Species %in% faltantes$Species),] #combina todos os individuos da matriz total e da matriz de WD a partir dos treeID;table
tablefalt

tablegenero<-merge(tablefalt,npgen, by.x="Genero", by.y="Genero");head(tablegenero)
comblinhas2<-rbind(np[,1:3], tablegenero[,c(2:4)]);comblinhas2

npok<-merge(wdok,comblinhas2, by.x="Species", by.y="Species");npok

sppfalt<-setdiff(traitmatriz$Species, npok$Species);sppfalt
spnv<-matrix(data=NA, nrow=length(sppfalt), ncol=9); colnames(spnv)<-colnames(npok); spnv
spnv[1:3,1]<-sppfalt; spnv
traitfalt<-rbind(npok, spnv); traitfalt

matrixtrait<-traitfalt[order((traitfalt$Species)),]; matrixtrait
setdiff(matrixtrait$Species, traitmatriz$Species)
setdiff(traitmatriz$Species,matrixtrait$Species)
duplicated(matrixtrait$Species)

View(matrixtrait)


#### convertendo de fator para numerico
f2n <- function(f) {  if(!is.factor(f)) stop("the input must be a factor") 
  as.numeric(levels(f))[as.integer(f)]} #funcao para converter Factor to Numeric (f2n)
matrixtrait$LA<-f2n(matrixtrait$LA)
matrixtrait$SLA<-f2n(matrixtrait$SLA)
matrixtrait$LDMC<-f2n(matrixtrait$LDMC)
matrixtrait$LNC<-f2n(matrixtrait$LNC)
matrixtrait$LPC<-f2n(matrixtrait$LPC)
matrixtrait$WD<-f2n(matrixtrait$WD)

#preenchendo com a media da coluna
matrixtrait$SLA[is.na(matrixtrait$SLA)]<-mean(matrixtrait$SLA, na.rm = T);is.na(matrixtrait$SLA)
matrixtrait$LA[is.na(matrixtrait$LA)]<-mean(matrixtrait$LA, na.rm = T);is.na(matrixtrait$LA)
matrixtrait$LDMC[is.na(matrixtrait$LDMC)]<-mean(matrixtrait$LDMC, na.rm = T);is.na(matrixtrait$LDMC)
matrixtrait$LNC[is.na(matrixtrait$LNC)]<-mean(matrixtrait$LNC, na.rm = T);is.na(matrixtrait$LNC)
matrixtrait$LPC[is.na(matrixtrait$LPC)]<-mean(matrixtrait$LPC, na.rm = T);is.na(matrixtrait$LPC)
matrixtrait$WD[is.na(matrixtrait$WD)]<-mean(matrixtrait$WD, na.rm = T);is.na(matrixtrait$WD)
View(matrixtrait)


##### salvando a matriz ####
#write.csv(matrixtrait, "trait_molz_sinosok.csv")
