# carregar pacotes ------
library(here);library(stats);library(dplyr);library(tibble);library(vegan);library(ape); library(ecodist);library(ggplot2)
data(iris)

## PCA ----------


## calcular PCA
pca_co <- prcomp(iris[,1:4], center = T, scale. = T) # importante para comunidades com medidas distintas, mesma amplitude
#center e scale: fazm com que os dados sejam ajustados para a mesma escala, removendo altas variações
#scores da pca: como cada variavel esta associada aos eixos
pca_co
#como cada UA esta associada aos eixos
pca_co$x # os eixos da PCA, cada um dos indíviduos com um score

summary(pca_co)

# Figura
biplot(pca_co)

# Figura bonitinha
library(ggfortify)
ordpca_co<-autoplot(pca_co, 
                    label=FALSE, 
                    data=iris, #dados originais inseridos na pca
                    colour="Species", #coluna que indica os grupos de cores
                    label.size=4, #tamanho dos rotulos
                    shape=19, #omite o simbolo
                    size=3,
                    loadings=TRUE, #inclui as setas com as variaveis
                    loadings.colour="black" #edita a cor das setas
) +
  theme_bw() +
  scale_color_manual(values = c("#666666", "#996600","#000000")) +
  geom_vline(xintercept=0, color="black", linetype="dotted") +
  geom_hline(yintercept=0, color="black", linetype="dotted") +
  annotate("text", x=0.52, y=-0.37, label= "Sepal.Lenght") +
  annotate("text", x=-0.26, y=-0.92, label= "Sepal.Width") +
  annotate("text", x=0.58, y=-0.02, label= "Petal.Lenght") +
  annotate("text", x=-0.56, y=-0.06, label= "Petal.Width") +
  theme(legend.background=element_rect(color="grey"),
        legend.position=c(0.12,0.90),
        legend.text=element_text(size=rel(1.1)),
        legend.title = element_text(size=rel(1.1))) +
  labs(color="Strata")
ordpca_co


## pca

###### esp?cies que coocorrem
ord.var_co <- read.table("PCA_coocorrem.txt", h=T)
head(ord.var_co)
dados.pca_co<-ord.var_co[,(-1)]
pca_co <- prcomp(dados.pca_co, center = T, scale. = T)
pca_co
biplot(pca_co)
my.theme<-theme(axis.title=element_text(size=rel(1.3)), axis.text=element_text(size=rel(1.3)), 
                axis.line = element_line(colour = "black"), 
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.border=element_rect(fill=NA, size=1, color="black"),
                panel.background = element_blank())
ordpca_co<-autoplot(pca_co, 
                    label=FALSE, 
                    data=ord.var_co, #dados originais inseridos na pca
                    colour="trat", #coluna que indica os grupos de cores
                    label.size=4, #tamanho dos rotulos
                    shape=19, #omite o simbolo
                    size=3,
                    loadings=TRUE, #inclui as setas com as variaveis
                    loadings.colour="black" #edita a cor das setas
) +
  theme_bw() +my.theme +
  scale_color_manual(values = c("#666666", "#996600")) +
  geom_vline(xintercept=0, color="black", linetype="dotted") +
  geom_hline(yintercept=0, color="black", linetype="dotted") +
  annotate("text", x=-0.15, y=-0.3, label= "LA") +
  annotate("text", x=0.4, y=0.15, label= "LDMC") +
  annotate("text", x=-0.10, y=0.38, label= "SM") +
  annotate("text", x=-0.25, y=0.30, label= "SN") +
  annotate("text", x=-0.33, y=-0.03, label= "SLA") +
  theme(legend.background=element_rect(color="grey"),
        legend.position=c(0.12,0.90),
        legend.text=element_text(size=rel(1.1)),
        legend.title = element_text(size=rel(1.1))) +
  labs(color="Strata")
ordpca_co


## PCoA ------------
d <- vegdist(iris[,1:4]) # aqui euclidiana, porque estão na mesma unidade de medida.
pcoa <- pcoa(d)
pcoa$vectors
pcoa$values$Cum_corr_eig

summary(pcoa)

biplot(pcoa)


iris2<-as.data.frame(iris);iris2$Species <-as.factor(iris2$Species)
df.pcoa<-as.data.frame(cbind(pcoa$vectors,iris2[,4:5]))
ggplot(df.pcoa, aes(x = Axis.1, y = Axis.2, color=Species)) + geom_point()+
  geom_vline(xintercept=0, color="black", linetype="dotted") +
  geom_hline(yintercept=0, color="black", linetype="dotted")+
  theme_light()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())


## NMDS -------
data(iris)
iris.d <- dist(iris[,1:4])

### nmds() is timeconsuming, so this was generated
### in advance and saved.
### set.seed(1234)
library(ecodist) 
iris.nmds <- nmds(iris.d, nits=20, mindim=1, maxdim=4) # aqui com pacote ecodist
### save(iris.nmds, file="ecodist/data/iris.nmds.rda")
data(iris.nmds)

# examine fit by number of dimensions
plot(iris.nmds)

#outra função muito usada
library(vegan)
nmds2 <- metaMDS(iris[,1:4])# aqui vegan, mais usada
nmds2$species

plot(nmds2, type="t")
iris2<-as.data.frame(iris);iris2$Species <-as.factor(iris2$Species)
df<-as.data.frame(cbind(nmds2$points,iris2[,4:5]))
ggplot(df, aes(x = MDS1, y = MDS2, color=Species,size=I(2))) + geom_point()+
  geom_vline(xintercept=0, color="black", linetype="dotted") +
  geom_hline(yintercept=0, color="black", linetype="dotted")+
  theme_light()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())
summary(nmds2)


## Beta diversidade ------------
#matriz de comunidade
data <- read.csv2(here::here("pasta_de_dados", "nome_da_tabela.csv"))
head(data)
data<-filter(data, D1 != "0")
comunidade<-reshape2::dcast(data,  PlotCode~Species, value.var="D1",fun.agg = length)
#sitio precisa estar como nome de linha, e nao como variavel
comunidade2<-comunidade %>% remove_rownames %>% column_to_rownames(var="PlotCode")
#matriz de distancia
dist <- vegdist(comunidade2)
comm_beta <- betadisper(dist); comm_beta #colocar na ordenacao

#sados de exemplo fornecidos pela funcao
data(varespec)
## Bray-Curtis distances between samples
dis <- vegdist(varespec)
## First 16 sites grazed, remaining 8 sites ungrazed
groups <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))
## Calculate multivariate dispersions
mod <- betadisper(dis, groups)
mod
plot(mod)
plot(mod, ellipse = TRUE, hull = F)
## Perform test
anova(mod)
## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 99)
## Tukey's Honest Significant Differences
mod.HSD <- TukeyHSD(mod)


## Particao da diversidade beta - aninhamento e turnover-------
#Proposta do Baselga

library(betapart)
comm <-decostand (comunidade2, method="pa")#converte para presenca e ausencia
#comm[comm>0] <-1 

## calculando o multiple site dissimilarity
beta_nat_2<-beta.multi(comm, index.family="sorensen")
beta_nat_2 #isso para saber a beta total, e o que ? turnover e aninhamento

## extrair matriz de distancia usando simpson #isso se turnover for maior
beta_nat<- beta.pair(comm, index.family="sorensen") # par a par, matriz de distancia 
class(beta_nat)
beta_sim<-beta_nat[["beta.sim"]] ###objeto apenas com a matriz de simpson
beta_sim # turnover
class(beta_sim)
length(beta_sim)


