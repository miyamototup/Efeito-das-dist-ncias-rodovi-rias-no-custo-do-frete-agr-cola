library(maptools)
rodo=readShapeSpatial('/home/bmiyamoto/Documentos/Pesquisa/Artigo Jamile/Shapefiles/ST_DNIT_Rodovias_SNV2015_03.shp')

str(rodo)
head(rodo@data)
View(rodo@data)

inforodo=rodo@data
setwd('/home/bmiyamoto/Documentos/Pesquisa/Artigo Jamile/')
write.csv(inforodo,file="Distância rodoviária",row.names=F)

plot(rodo)
