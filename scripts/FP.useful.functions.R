
# ForestPlots useful functions ----


# Version 0.0.1 
# Kauane Bordin

# PACOTES ---
library(dplyr)
library(reshape)
library(tidyverse)

# AREA BASAL -----
basal.area <- function(x) { (pi*(x/2)^2)/10000} # calculate basal area in m^2 ha ### BA = (sum(n (PI*(Di/2)^2 )))/Ha |||| #(pi*(1/2)^2)/10000


# MATRIZ DE COMUNIDADES -------
# Funcao para gerar matriz de comunidade, ou termos de area basal ou
# em termos de densidade de stems
# !!! Caso queira densidade de individuos, usar a funcao "stem2abund" primeiro!!!!

# Para gerar a matriz de comunidade, sao necessarios dois parametros: o conjunto de
# dados no formato da planilha do ForestPlots E value = ba ou value = dens
# value = ba gera uma matriz de comunidades por area basal
# value = dens gera uma matriz de comunidades com densidade

# SE FOR USAR o SÍTIO como comunidade: mudar para column_to_rownames(var="Plot.Code")


community.matrix <- function (data2, value){ 
  if(value == "ba"){ # value = ba gera uma matriz por area basal
    
    data2 <- reshape2::dcast(data2, Sub.Plot.T1~Species, value.var="ba",fun.agg = sum)#area basal
    data2 <- data2 %>% remove_rownames %>% column_to_rownames(var="Sub.Plot.T1")#coloca 1 coluna como rowname
    data2 <- data2[order(rownames(data2)),] #ordena as parcelas alfabeticamente
    
  } else {
    data2 <- reshape2::dcast(data2, Sub.Plot.T1~Species, value.var="ba",fun.agg = length)#contagem
    data2 <- data2 %>% remove_rownames %>% column_to_rownames(var="Sub.Plot.T1") #coloca 1 coluna como rowname
    data2 <- data2[order(rownames(data2)),] #ordena as parcelas alfabeticamente
  }
}


# LIMPEZA DE DADOS: DE STEMS PARA INDIVIDUOS -------
# dados no formato da planilha do ForestPlots
# a funcao cria uma nova coluna com o tagnumber que possui mais de um stem, o qual e representado
# por letras, e depois disso separa a letra dos numeros, e por fim filtra apenas uma das letras 
# por tag = ou seja, um registro = um individuo = um stem.
stem2abund <- function (x){
  separate.tag <-strsplit(x$Tag.No, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)
  x$new.tag<- sapply(separate.tag, paste, collapse=" ")
  x <-separate(x, new.tag, into = c("tag.number", "stem.letter"), sep = " (?=[^ ]+$)")
  x[is.na(x)] = "singlestem"
  x2 <- dplyr::filter(x,stem.letter != "b" & stem.letter != "c" & 
                        stem.letter != "d" & stem.letter != "e" & stem.letter != "E" & 
                        stem.letter != "f" & stem.letter != "g" & stem.letter != "h" &
                        stem.letter != "i" & stem.letter != "j" & stem.letter != "k" &
                        stem.letter != "l") 
}


# EXEMPLOS ------------

matriz.exemplo.FP <- read.csv2(here::here("pasta_de_dados", "matriz.exemplo.forestplots.csv"))
names(matriz.exemplo.FP) #nomes das colunas

 #siga o passo a passo nessa ordem :)

# Primeiro carregue os dados
dados.analise <- read.csv2(here::here("pasta_de_dados", "dados_originais.csv"))

# Calcule a area basal para cada um dos stems
dados.analise$ba <- basal.area(dados.analise$D1)
names(dados.analise)

#matriz de comunidade usando area basal
comunidade.BA <- community.matrix(data2 = dados.analise, value = "ba")

#matriz de comunidades usando densidade de stems
comunidade.stem.density <- community.matrix(data2 = dados.analise, value = "dens")

#matriz de comunidades usando densidade de individuos
dados.individual.density <- stem2abund(dados.analise)
comunidade.individual.density <- community.matrix(data2 = dados.individual.density, value = "dens")
