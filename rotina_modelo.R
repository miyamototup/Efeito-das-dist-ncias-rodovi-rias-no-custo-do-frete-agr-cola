library(gmapsdistance)
library(XLConnect)

frete=readWorksheetFromFile('/home/bmiyamoto/Documentos/Pesquisa/Jamile/frete.xlsx',sheet=1)
frete$dist=NA

for (i in 1:dim(frete)[1]){
munor=frete[i,]$ORIGEM
munor=gsub(" ","+",munor)
origem=paste(munor,'+',frete[i,]$UFORIGEM,sep='')

mundest=frete[i,]$DESTINO
mundest=gsub(" ","+",mundest)
destino=paste(mundest,'+',frete[i,]$UFDESTINO,sep='')

distancia=gmapsdistance(origin = origem,destination = destino,mode = "driving",key="AIzaSyB1lM6_b69uvrznVKGC-rS-Q1--cOzBp6k")
frete[i,]$dist=distancia$Distance
}
save.image('/home/bmiyamoto/Documentos/Pesquisa/Jamile/frete.RData')
#########################################################################
###########################################################################
library(XLConnect)
library(tidyr)

load('/home/bmiyamoto/Documentos/Pesquisa/Jamile/frete.RData')
frete$logfrete=log(frete$R..t)
frete$distkm=frete$dist/1000
frete$Rotauf=paste(frete$UFORIGEM,frete$UFDESTINO,sep='')

#Binárias
#frete=cbind(frete,model.matrix(~frete$PRODUTO-1),model.matrix(~frete$MES-1),
 #           model.matrix(~frete$Rotauf-1))

#Ver o impacto do frete de retorno no frete da ida. As rotas que tem frete
# de retorno tem frete mais barato do que as que não tem? Usa fertilizante só para isso

#como frete de retorno é so com fertilizantes, vou separar só os fertilizantes
#para ver se tem rotas de grãos associadas
fretefert=frete[frete$PRODUTO=='Fertilizantes',c(6,4)]
fretefert=unique(fretefert)
fretefert$retorno=1
frete=merge(frete,fretefert,.x=c('ORIGEM','DESTINO'),by.y=c('DESTINO','ORIGEM'),all.x=T)
frete=frete[frete$PRODUTO!='Fertilizantes',]
frete$retorno[is.na(frete$retorno)]=0

#Os fretes rodoviários tem comportamento de preço por faixas por quilometros
#frete ate 500 é mais caro do que frete para distancia superiores a 500
#criar binária para isso.
frete$fretedist500=0
frete$fretedist500[frete$distkm<=500]=1
#Talvez eu tenha que cirar uma variável de interação entre essa binária
#e a distância para mostrar que o custo por quilómetro até 500 km é
#maior do que o custo por quilômetro para distâncias superiores a 500 km
frete$interacao500km=frete$distkm*frete$fretedist500

#Não estou conseguindo captar esse efeito quadrático da distância, ous seja
# o preço do frete aumenta até certo ponto mas depois tende a ser
#decrescente. Vou tentar criar distancia ao quadrado
frete$distquadrado=frete$distkm*frete$distkm

####
###Período safra e entresafra das culturas por UF (eu pegar isso)
#Vou usar definição do Ministério da Agricultura. 

#Soja
#centro oeste de janeiro a abril
frete$SAFRA=0
frete$SAFRA[(frete$PRODUTO=='Soja'&(frete$MES=='Janeiro'|frete$MES=='Fevereiro'|
                                      frete$MES=='Março'|frete$MES=='Abril')&(
                                        frete$UFORIGEM=='GO'|frete$UFORIGEM=='MS'|frete$UFORIGEM=='MT'))]=1
#sudeste de fevereiro a maio
frete$SAFRA[(frete$PRODUTO=='Soja'&(frete$MES=='Fevereiro'|frete$MES=='Março'|
                                      frete$MES=='Abril'|frete$MES=='Maio')&(
                                        frete$UFORIGEM=='SP'))]=1

#sul de janeiro a maio
frete$SAFRA[(frete$PRODUTO=='Soja'&(frete$MES=='Janeiro'|frete$MES=='Fevereiro'|frete$MES=='Março'|
                                      frete$MES=='Abril'|frete$MES=='Maio')&(
                                        frete$UFORIGEM=='PR'))]=1


#Milho
#centro-oesste de junho a setembro
frete$SAFRA[(frete$PRODUTO=='Milho'&(frete$MES=='Junho'|frete$MES=='Julho'|
                                      frete$MES=='Agosto'|frete$MES=='Setembro')&(
                                        frete$UFORIGEM=='GO'|frete$UFORIGEM=='MS'|frete$UFORIGEM=='MT'))]=1

#sudeste de maio a setembro
frete$SAFRA[(frete$PRODUTO=='Milho'&(frete$MES=='Maio'|frete$MES=='Junho'|frete$MES=='Julho'|
                                       frete$MES=='Agosto'|frete$MES=='Setembro')&(
                                         frete$UFORIGEM=='SP'))]=1


#sul de maio a setembro
frete$SAFRA[(frete$PRODUTO=='Milho'&(frete$MES=='Maio'|frete$MES=='Junho'|frete$MES=='Julho'|
                                       frete$MES=='Agosto'|frete$MES=='Setembro')&(
                                         frete$UFORIGEM=='PR'))]=1

#preços das cotações internacionais ou preço pago ao produtor
#para ver  impacto das cotações no preço do frete
#Cotação semanal de chicago
#Preciso aprender a deflacionar preços internacionais para o mercado brasileiro
#ou por ser dolar posso considerar moeda constante? perguntar par ao Marco.
#Na verdade inserir essas cotações só faria sentido se eu estivesse trabalhando com painel
#porque só tenho dados para o ano de 2014. Como inserir uma vairiável relacionada
# a essa cotação que reflita os preços de 2014? ou mesmo a variação
#2013 2014? o que seria mais interessante?
cotmilho=readWorksheetFromFile("/home/bmiyamoto/Documentos/Pesquisa/Jamile/dados/Cotação Chicago_milho.xlsx",
                               sheet=1)
cotmilho=cotmilho[,c(1,2)]
colnames(cotmilho)=c('Semana','Milho')
cotmilho=separate(cotmilho,Semana,c('Ano','Mes','Dia'),sep='-')
cotmilho=cotmilho[cotmilho$Ano>=2013,]
cotmilho$Milho=as.numeric(cotmilho$Milho)
cotmilho=aggregate(cotmilho[,c(4)],by=list(Ano=cotmilho$Ano),mean,na.rm=T)

cotsoja=readWorksheetFromFile("/home/bmiyamoto/Documentos/Pesquisa/Jamile/dados/Cotação Chicago_milho.xlsx",
                              sheet=1)
cotsoja=cotsoja[,c(1,3)]
colnames(cotsoja)=c('Semana','Soja')
cotsoja=separate(cotsoja,Semana,c('Ano','Mes','Dia'),sep='-')
cotsoja=cotsoja[cotsoja$Ano>=2013,]
cotsoja$Soja=as.numeric(cotsoja$Soja)
cotsoja=aggregate(cotsoja[,c(4)],by=list(Ano=cotsoja$Ano),mean,na.rm=T)

#Discutir como inserir a cotação


#Qualidade das vias segundo índice criado pela Jamile
#multiplicar os indices dos estados para chegarmos (origemxdestino)
#para chegarmos aos índices da rota
#Vou calcular a média do trajeto, ou seja média do índice entre a origem e o destino
indice=readWorksheetFromFile("/home/bmiyamoto/Documentos/Pesquisa/Jamile/dados/Indicador.xlsx",
                             sheet=1)
colnames(indice)=c('UF','indice')
indice=indice[indice$UF=='GO'|indice$UF=='MS'|indice$UF=='MT'|
              indice$UF=='PR'|indice$UF=='SP',]
iorigem=indice
colnames(iorigem)[2]='iorigem'
idestino=indice
colnames(idestino)[2]='idestino'

frete=merge(frete,iorigem,by.x=c('UFORIGEM'),by.y=c('UF'),all.x=T,all.y=F)
frete=merge(frete,idestino,by.x=c('UFDESTINO'),by.y=c('UF'),all.x=T,all.y=F)
frete$iqualirota=(frete$iorigem+frete$idestino)/2

#Diesel na origem
#mais uma vez preciso calcular a média da rota, ou seja média entre
# o preço do diesel na origem e no destino
diesel=readWorksheetFromFile("/home/bmiyamoto/Documentos/Pesquisa/Jamile/dados/DIESEL - LEVANTAMENTO DE PREÇOS_ESTADO_2014.xlsx",
                             sheet=1)

diesel=diesel[c(2:6),c(1,14)]
diesel$UF=c('MT','MS','GO','SP','PR')
diesel=diesel[,c(2,3)]
colnames(diesel)[1]='diesel'

dieselorigem=diesel
colnames(dieselorigem)[1]='dieselorigem'

dieseldestino=diesel
colnames(dieseldestino)[1]='dieseldestino'

frete=merge(frete,dieselorigem,by.x=c('UFORIGEM'),by.y=c('UF'))
frete=merge(frete,dieseldestino,by.x=c('UFDESTINO'),by.y=c('UF'))
frete$diesel=(frete$dieselorigem+frete$dieseldestino)/2


##A interação e binária para 500km não funcionou. vou criar
#tres binárias com base nos quartis das distancias
#As binárias não deram significativas
frete$distbin1=0
frete$distbin1[frete$distkm>=411&frete$distkm<600]=1
frete$distbin2=0
frete$distbin2[frete$distkm>=600&frete$distkm<1050]=1
frete$distbin3=0
frete$distbin3[frete$distkm>=1050]=1


#Ajuste
#Vou deixar tudo log log para facilitar a interpretação
frete$logdistkm=log(frete$distkm)
frete$logdistquadrado=log(frete$distquadrado)
frete$logiqualirota=log(frete$iqualirota)
frete$logdiesel=log(frete$diesel)
frete$interacaolog500km=frete$fretedist500*frete$logdistkm

frete=frete[frete$distkm>0,]
y=cbind(frete$logfrete)
#x=cbind(as.matrix(frete[,c(13,16,17)]))
x=cbind(as.matrix(frete[,c(15,19,29,31,32)]))
lmfrete=lm(y ~ x, data=frete)
summary(lmfrete)



#Além disso vou ajustar uma regressão quantílica sem as binárias das
# distancias
library(quantreg)

f1=rq(y ~ x,tau=.25)
summary(f1)

f2=rq(y ~ x,tau=.5)
summary(f2)

f3=rq(y ~ x,tau=.75)
summary(f3)




#Tentar agrupar os municípios por micro. para o caso de por exemplo
#considerar todoas rotas para cubatão como rota para santos
#### Bem pensando melhor acho que não vale fazer isso para não
#perder variabilidade da amostra





#Talvez testar IGPM(FGV) e diesel(ANP). relação 70 diesel 30 igp para
#correção dos preços dos fretes. essa relação ainda existe?

#Agrupar milho e soja na catagoria grãos no modelo para todos os produtos

#Estrturura do artigo
#Artigo sobre transporte rodicário de cargas agrícolas e do comportamento dos fretes
#introdução
#importancia da logistica e do modal rodoviário para moviemntação
# e como alimentador das rotas intermodais. as rotas intermordais
# não funcionam sem as rotas rodoviárias.
#Os principais produtos agrícolas brasileiros (soja, milho, etc)

#subtopico
#caracterizar os sistema rodoviário. falando um pouco como é o transporte
# de carga ou por região  que estamos trabalhando no modelo. As realdiades
#regionais são diferentes.

#material e método 
#construção das variáveis e dos índices. 

#Resultados


#Jamile pediu mapa identificando as UFS
library(maptools)
uf=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Jamile/estados_2010/estados_2010.shp')
uf@data$cor[uf@data$sigla=='MT'|uf@data$sigla=='MS'|uf@data$sigla=='GO'|
              uf@data$sigla=='SP'|uf@data$sigla=='PR']=adjustcolor('forestgreen',1)
plot(uf,col=uf@data$cor)
#legenda
legenda=as.character(c("Data Source"))
cores=as.character(adjustcolor('forestgreen',1))
legend(x=-71.8,y=-17, legenda, fill=cores, bty="n", cex=0.8 )
text(getSpPPolygonsLabptSlots(uf), labels=uf@data$sigla, cex=0.7)
library(maps)
map.scale(x=-48.6, y=-31.5,relwidth=0.08,metric=T,ratio=T,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/Jamile/estados_2010')
source(compassRose(-43.4,-26.5))

