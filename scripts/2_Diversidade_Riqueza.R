## Diversidade de especies e Riqueza ##

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
rarecurve(comunidade2)

# Hill numbers:  Chao et al 2014

library(hillR)
dummy <- FD::dummy

#q=0 corresponde a riqueza de especies
# same as: vegan::specnumber(dummy$abun)
hill_taxa(comm = dummy$abun, q = 0)

#q=1 corresponde ao expoente da diversidade de shannon
# same as: exp(vegan::diversity(x = dummy$abun, index = 'shannon'))
hill_taxa(comm = dummy$abun, q = 1)

#q=2 corresponde ao inverso de simpson
# same as: vegan::diversity(x = dummy$abun, index = 'invsimpson')
hill_taxa(comm = dummy$abun, q = 2)
#inverso de simpson, equivalente ao q=2 de Chao et al 2014
comunidade_div2<- diversity(comunidade2, "invsimpson");comunidade_div2

