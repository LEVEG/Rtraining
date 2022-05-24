## Diversidade funcional e CWM ###
## função dbfd ##

require(FD)
ex1 <- dbFD(dummy$trait, dummy$abun)
ex1

# importante: nome das especies devem estar iguais e na mesma ordem!

comm <- read.table("comm_leveg.txt", h=T, row.names = 1); head(comm)
traits <- read.table("atributos.txt", h=T, row.names = 1); head(traits)

teste <- dbFD(traits, comm)
teste
