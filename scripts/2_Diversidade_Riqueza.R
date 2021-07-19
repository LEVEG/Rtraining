## Diversidade de espécies e Riqueza ##

## Como carregar pacotes
library(reshape);library(reshape2)
library(here);library(dplyr);library(tibble);library(vegan)

## carregar dados de comunidade

data <- read.csv2(here::here("pasta_de_dados", "nome_da_tabela.csv"))
head(data)

#como filtrar dados dentro do R - funcao filter
#precisa ser uma "tidy table" 
data<-filter(data, D1 != "0")

#funcao dcast do  pacote reshape2
comunidade<-reshape2::dcast(data,  PlotCode~Species, value.var="D1",fun.agg = length)
head(comunidade)

#removendo a primeira linha e segunda coluna
#comunidade<-comunidade[-1,-2]
#head(comunidade)

#sitio precisa estar como nome de linha, e nao como variavel
comunidade2<-comunidade %>% remove_rownames %>% column_to_rownames(var="PlotCode")
head(comunidade2) #coloca 1 coluna como rowname

## Calcular indices de diversidade taxonomica 

library(vegan) # pacote bem importante! 
#vegan: vegetation analises :)

## Calcular diversidade de Shannon
comunidade_div<- diversity(comunidade2, "shannon");comunidade_div

## Calcular diversidade de Simpson
comunidade_div2<- diversity(comunidade2, "simpson");comunidade_div2

## Calcular Riqueza rarefeita
comunidade2 <- round(comunidade2)
raremax <- min(rowSums(comunidade2))
comunidade2_rare <- rarefy(comunidade2, raremax); comunidade2_rare

## Beta diversidade ## VER AINDA:
comm_beta <- betadiver(comunidade2); comm_beta
plot(comm_beta)
scores(comm_beta)

# Hill numbers:  Chao et al 2014
#inverso de simpson, equivalente ao q=2 de Chao et al 2014
comunidade_div2<- diversity(comunidade2, "invsimpson");comunidade_div2

library(hillR)
dummy <- FD::dummy

# same as: vegan::specnumber(dummy$abun)
hill_taxa(comm = dummy$abun, q = 0)

# same as: exp(vegan::diversity(x = dummy$abun, index = 'shannon'))
hill_taxa(comm = dummy$abun, q = 1)

# same as: vegan::diversity(x = dummy$abun, index = 'invsimpson')
hill_taxa(comm = dummy$abun, q = 2)


####### baselga

library(betapart)

comm <-decostand (comunidade2, method="pa")
#comm[comm>0] <-1 

## calculando o multiple site dissimilarity
beta_nat_2<-beta.multi(comm, index.family="sorensen")
beta_nat_2 #isso para saber a beta total, e o que ? tuenover e nestedness

###extrair matriz de dist usando simpson #isso se turnover for maior
beta_nat<- beta.pair(comm, index.family="sorensen") # par a par, matriz de distancia 
class(beta_nat)
beta_sim<-beta_nat[["beta.sim"]] ###objeto apenas com a matriz de simpson
beta_sim # turnover
class(beta_sim)
length(beta_sim)
