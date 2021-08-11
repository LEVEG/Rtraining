## PCA ###

# pacotes
library(here);library(stats)

## carregar tabela
#primeiro abrir diretório da pasta de dados

ord.var_co <- read.table("dados_PCA.txt", h=T)
head(ord.var_co)
dados.pca_co<-ord.var_co[,(-1)]
class(dados.pca_co$SM)



## calcular PCA

pca_co <- prcomp(dados.pca_co, center = T, scale. = T)
pca_co

# Figura
biplot(pca_co)

# Figura bonitinha

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


## Beta diversidade 
#matriz de comunidade
#matriz de distancia

comm_beta <- betadisper(comunidade2); comm_beta #colocar na ordenacao
plot(comm_beta)
scores(comm_beta)

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
