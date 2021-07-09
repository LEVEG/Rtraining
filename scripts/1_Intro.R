
### Aula 1 ### 
### Introdução ###

## Diretorio
# ao abrir o Rproj. os scripts podem ser abertos através da guia "Files"

## Como carregar pacotes
library(reshape);library(reshape2)
library(here);library(dplyr);library(tibble)

## Como carregar uma tabela

# 1- Quando a tabela estiver em formato .txt no seu diretorio
data <- read.table("nome_da_tabela.txt", h=T, row.names = 1)

# 2- Quando a tabela estiver em formato .csv no seu diretorio
data <- read.csv("nome_da_tabela.csv", h=T)
data <- read.csv2(here::here("pasta_de_dados", "nome_da_tabela.csv"))
#nunca use file_choose() 


## Como visualizar a tabela carregada

head(data)
View(data)

## Como ver nomes das colunas
names(data)

## nome das linhas
rownames(data)

## Como saber a dimensão da tabela
dim(data)

## Saber a "classe" da tabela
class(data)

## saber a classe dos dados
str(data)

## Como excluir uma (ou +) coluna(s): Varias maneiras... aqui vai algumas
data <- data[,c(-1,-2,-5)] 

# ou

data <- data[,-c(1:3)]

# ou

data <- data[,-c(1,5)]

# preciso manter a tabela original, entao vou criar uma nova tabela
data_novo <- data

## Como excluir uma linha da tabela
data <- data[-1,]

# Lembrar que antes da vírgula é linha e depois da vírgula se refere a colunas.

## Minha tabela é gingantesca, mas quero apenas 3 colunas dela

data <- data[,c("coluna1","coluna2","coluna3")]

#ou

data<-data[,c(1,5,8)]

## Minha tabela é gigantesca, mas tem linhas com NA e quero tirar essas linhas
row.has.na <- apply(data, 1, function(x){any(is.na(x))}) #tirar NA
sum(row.has.na)
data <- data[!row.has.na,]

# ou eu sei o nome da linha que devo tirar

data<-data[!(data$Nome_coluna=="Nome_linha"),]

## Minha matriz tem espécies nas linhas e sites nas colunas, mas quero que seja ao contrário:
data <- cast(data, Site~species, fun.aggregate = mean)

## Tenho duas tabelas e quero juntá-las

# concatena nas linhas:
data_v2 <- rbind(data, data1)

# concatena nas colunas:
data_v3 <- cbind(data, data1)

## Mudar o nome de uma coluna
data <- data.frame(site1=1:3, site2=rnorm(3))
head(data)
colnames(data) <- c("site_a", "site_b")

## Mudar nome de uma linha
rownames(data)[rownames(data) == "1"] <- "Esp a"

## Quero saber a media dos valores de uma coluna
mean(data$site_b)

## Quero somar os valores de uma coluna
sum(data$site_a) ## esse $ indica qual coluna da tabela que queremos fazer a ação

## valor máximo da coluna
max(data$site_a)

## Valor mínimo da coluna
min(data$site_a)


# matriz de comunidades ---------------
#criacao da matriz de comunidades
#nessa matriz, temos os sitios (comunidades) nas linhas
#especies nas colunas
#densidade da especie no valor da celula

data <- read.csv2(here::here("pasta_de_dados", "nome_da_tabela.csv"))
head(data)

#funcao dcast do  pacote reshape2
comunidade<-reshape2::dcast(data,  PlotCode~Species, value.var="D1",fun.agg = length)
head(comunidade)

#removendo a primeira linha e segunda coluna
comunidade<-comunidade[-1,-2]
head(comunidade)

#sitio precisa estar como nome de linha, e nao como variavel
comunidade2<-comunidade %>% remove_rownames %>% column_to_rownames(var="PlotCode")
head(comunidade2) #coloca 1 coluna como rowname

## Fazer a matriz binária

comunidade2[comunidade2>0] <-1 ; comunidade2


## O que for 0 colocar NA

comunidade2[comunidade2==0] <-NA ; comunidade2

## Transpor a matriz

t2<- t(comunidade2)
t2


