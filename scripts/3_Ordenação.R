## PCA ###

# pacotes
library(here);library(stats)

## carregar tabela

ord.var_co <- read.csv2(here::here("pasta_de_dados", "PCA.csv"))
head(ord.var_co)
dados.pca_co<-ord.var_co[,c(-1,-2)]

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
