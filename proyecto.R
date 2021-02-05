#Analisis de datos para comparación entre los indices de los distintos años y metodos de admisión


#Vamos a analizar primero la tabla relacionada con los datos de Sartenejas desde 2005 al 2017
#cargamos los datos desde data_informe_ciu_sartenejas_para_decagene.csv en el DataFrame df_sj
df_sj = read.table("E:/USB/Resolución de problemas relacionados con la empresa/proyecto/data_informe_ciu_sartenejas_para_decagene.csv",header = T,sep=";",dec=",")
#convertimos el dataframe a distintas variables con los nombres de sus columnas
attach(df_sj)
#nombres de las columnas
names(df_sj)
#Como no queremos Todas las columnas vamos a tomar solamente las columnas 

#numero de estudiante, tipo de ingreso, año de ingreso e indice 
#y lo almacenaremos en dt_sj Con este archivo trabajaremos
dt_sj<-df_sj[c("NUMERO.DE.ESTUDIANTE","TIPO_DE_INGRESO", "AÑO_INGRESO","DESC_SITUACION_GLOBAL", "IINDICE")]
#nombres de las nuevas columnas
names(dt_sj)

#Haremos primero un estudio general
regular_sj<-dt_sj[dt_sj$TIPO_DE_INGRESO == "ADMISION REGULAR",]
equivalencia_sj<-dt_sj[dt_sj$TIPO_DE_INGRESO == "EQUIVALENCIA",]
ciu_sj<-dt_sj[dt_sj$TIPO_DE_INGRESO == "CIU",]
opsu_sj<-dt_sj[dt_sj$TIPO_DE_INGRESO == "OPSU",]

#Conteo de datos y grafico de torta por porcentajes
total=dim(dt_sj)
total[1]
regular=dim(regular_sj)
regular[1]
equivalencia=dim(equivalencia_sj)
equivalencia[1]
ciu=dim(ciu_sj)
ciu[1]
opsu=dim(opsu_sj)
opsu[1]
proporciones<- c(regular[1], equivalencia[1], ciu[1], opsu[1])
etiquetas <- c("regular","equivalencia","ciu","opsu")
pct <- round(proporciones/sum(proporciones)*100)
etiquetas <- paste(etiquetas, pct)
etiquetas <- paste(etiquetas,"%",sep="")
pie(proporciones,labels = etiquetas,
    col=rainbow(length(etiquetas)),
    main="Estudiantes admitidos")
legend("topright", c("regular","equivalencia","ciu","opsu"), cex = 0.5,
       fill = rainbow(length(proporciones)))


#Numero de inactivos hay ya sea por indice o por retiros
total_retiros <- dt_sj[dt_sj$DESC_SITUACION_GLOBAL == "Inactivo Eliminado indice acad" | dt_sj$DESC_SITUACION_GLOBAL == "Inactivo norma de retiros" | dt_sj$DESC_SITUACION_GLOBAL == "Inactivo Elim.trim.1 no aprob." | dt_sj$DESC_SITUACION_GLOBAL == "Inactivo Elim.trim.2 no aprob." | dt_sj$DESC_SITUACION_GLOBAL == "Inactivo Eliminado 2PP consecu" | dt_sj$DESC_SITUACION_GLOBAL == "Inactivo Eliminado 3PP"  ,]
dim(total_retiros)[1]
retiros_regular_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "ADMISION REGULAR",]
retiros_equivalencia_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "EQUIVALENCIA",]
retiros_ciu_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "CIU",]
retiros_opsu_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "OPSU",]


retiros_regular=dim(retiros_regular_sj)
retiros_regular[1]
retiros_equivalencia=dim(retiros_equivalencia_sj)
retiros_equivalencia[1]
retiros_ciu=dim(retiros_ciu_sj)
retiros_ciu[1]
retiros_opsu=dim(retiros_opsu_sj)
retiros_opsu[1]
proporciones_r<- c(retiros_regular[1], retiros_equivalencia[1], retiros_ciu[1], retiros_opsu[1])
etiquetas <- c("regular","equivalencia","ciu","opsu")
pct <- round(proporciones_r/sum(proporciones_r)*100)
etiquetas <- paste(etiquetas, pct)
etiquetas <- paste(etiquetas,"%",sep="")
pie(proporciones_r,labels = etiquetas, col=rainbow(length(etiquetas)), main="Estudiantes inactivos")
legend("topright", c("regular","equivalencia","ciu","opsu"), cex = 0.5,
       fill = rainbow(length(proporciones_r)))


#Porcentaje de retiros por cada uno
pct_indiv_r<-proporciones_r/proporciones*100
pct_indiv_r

#Cantidad de no retirados
total_nor<-dt_sj[!is.na(dt_sj$IINDICE),]
nor_regular_sj<-total_nor[total_nor$TIPO_DE_INGRESO == "ADMISION REGULAR",]
nor_equivalencia_sj<-total_nor[total_nor$TIPO_DE_INGRESO == "EQUIVALENCIA",]
nor_ciu_sj<-total_nor[total_nor$TIPO_DE_INGRESO == "CIU",]
nor_opsu_sj<-total_nor[total_nor$TIPO_DE_INGRESO == "OPSU",]

mean(total_nor$IINDICE) #media de admision regular
sd(total_nor$IINDICE) #desviacion estandar de admision regular
mean(nor_regular_sj$IINDICE) #media de admision regular
sd(nor_regular_sj$IINDICE) #desviacion estandar de admision regular
mean(nor_equivalencia_sj$IINDICE) #media de equivalencia
sd(nor_equivalencia_sj$IINDICE) #desviacion estandar de equivalencia
mean(nor_ciu_sj$IINDICE) #media de ciu
sd(nor_ciu_sj$IINDICE) #desviacion estandar de ciu
mean(nor_opsu_sj$IINDICE) #media de opsu
sd(nor_opsu_sj$IINDICE) #desviacion estandar de opsu

#Numero de graduados hay por cada metodo de admision
total_graduados <- dt_sj[dt_sj$DESC_SITUACION_GLOBAL == "GRADUADO" ,]
dim(total_graduados)[1]
graduados_regular_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "ADMISION REGULAR",]
graduados_equivalencia_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "EQUIVALENCIA",]
graduados_ciu_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "CIU",]
graduados_opsu_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "OPSU",]

graduados_regular=dim(graduados_regular_sj)
graduados_regular[1]
graduados_equivalencia=dim(graduados_equivalencia_sj)
graduados_equivalencia[1]
graduados_ciu=dim(graduados_ciu_sj)
graduados_ciu[1]
graduados_opsu=dim(graduados_opsu_sj)
graduados_opsu[1]
proporciones_g<- c(graduados_regular[1], graduados_equivalencia[1], graduados_ciu[1], graduados_opsu[1])
etiquetas <- c("regular","equivalencia","ciu","opsu")
pct <- round(proporciones_g/sum(proporciones_g)*100)
etiquetas <- paste(etiquetas, pct)
etiquetas <- paste(etiquetas,"%",sep="")
pie(proporciones_g,labels = etiquetas, col=rainbow(length(etiquetas)), main="Estudiantes graduados")
legend("topright", c("regular","equivalencia","ciu","opsu"), cex = 0.5,
       fill = rainbow(length(proporciones_r)))

#Porcentaje de graduados por cada metodo de admision
pct_indiv_g<-proporciones_g/proporciones*100
pct_indiv_g


#División de los indices por año de ingreso
años<-c("2005","2006","2007","2008","2009","2010","2012","2013","2014","2015","2016","2017")

#Ciclo para estudiar por años
for (i in años) {
  #dividimos por año de ingreso
  sj_año<-dt_sj[dt_sj$AÑO_INGRESO == i,]
  print(c("AÑO ",i))
  
  #dividimos ese año por metodo de admision
  regular<-sj_año[sj_año$TIPO_DE_INGRESO == "ADMISION REGULAR",]
  equivalencia<-sj_año[sj_año$TIPO_DE_INGRESO == "EQUIVALENCIA",]
  ciu<-sj_año[sj_año$TIPO_DE_INGRESO == "CIU",]
  opsu<-sj_año[sj_año$TIPO_DE_INGRESO == "OPSU",]
  
  #Conteo de datos y grafico de torta por porcentajes
  total=dim(sj_año)
  print(c("Estudiantes aceptados TOTAL en ",i, total[1]))
  regular_dim=dim(regular)
  print(c("Estudiantes aceptados REGULAR en ",i, regular_dim[1]))
  equivalencia_dim=dim(equivalencia)
  print(c("Estudiantes aceptados EQUIVALENCIA en ",i, equivalencia_dim[1]))
  ciu_dim=dim(ciu)
  print(c("Estudiantes aceptados CIU en ",i, ciu_dim[1]))
  opsu_dim=dim(opsu)
  print(c("Estudiantes aceptados OPSU en ",i, opsu_dim[1]))
  proporciones<- c(regular_dim[1], equivalencia_dim[1], ciu_dim[1], opsu_dim[1])
  etiquetas <- c("regular","equivalencia","ciu","opsu")
  pct <- round(proporciones/sum(proporciones)*100)
  etiquetas <- paste(etiquetas, pct)
  etiquetas <- paste(etiquetas,"%",sep="")
  pie(proporciones,labels = etiquetas,
      col=rainbow(length(etiquetas)),
      main=c("Estudiantes admitidos en ",i))
  legend("topright", c("regular","equivalencia","ciu","opsu"), cex = 0.5,
         fill = rainbow(length(proporciones)))
  
  #######################################RETIROS########################################################################
  #Numero de inactivos hay ya sea por indice o por retiros
  total_retiros <- sj_año[sj_año$DESC_SITUACION_GLOBAL == "Inactivo Eliminado indice acad" | sj_año$DESC_SITUACION_GLOBAL == "Inactivo norma de retiros" | sj_año$DESC_SITUACION_GLOBAL == "Inactivo Elim.trim.1 no aprob." | sj_año$DESC_SITUACION_GLOBAL == "Inactivo Elim.trim.2 no aprob." | sj_año$DESC_SITUACION_GLOBAL == "Inactivo Eliminado 2PP consecu" | sj_año$DESC_SITUACION_GLOBAL == "Inactivo Eliminado 3PP"  ,]
  print(c("Estudiantes retirados TOTAL en ",i,dim(total_retiros)[1]))
  retiros_regular_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "ADMISION REGULAR",]
  retiros_equivalencia_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "EQUIVALENCIA",]
  retiros_ciu_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "CIU",]
  retiros_opsu_sj<-total_retiros[total_retiros$TIPO_DE_INGRESO == "OPSU",]
  
  
  #Conteo de datos y grafico de torta por porcentajes de retiros
  retiros_regular=dim(retiros_regular_sj)
  print(c("Estudiantes retirados REGULAR en ",i,retiros_regular[1]))
  retiros_equivalencia=dim(retiros_equivalencia_sj)
  print(c("Estudiantes retirados EQUIVALENCIA en ",i,retiros_equivalencia[1]))
  retiros_ciu=dim(retiros_ciu_sj)
  print(c("Estudiantes retirados en CIU",i,retiros_ciu[1]))
  retiros_opsu=dim(retiros_opsu_sj)
  print(c("Estudiantes retirados en OPSU",i,retiros_opsu[1]))
  proporciones_r<- c(retiros_regular[1], retiros_equivalencia[1], retiros_ciu[1], retiros_opsu[1])
  etiquetas <- c("regular","equivalencia","ciu","opsu")
  pct <- round(proporciones_r/sum(proporciones_r)*100)
  etiquetas <- paste(etiquetas, pct)
  etiquetas <- paste(etiquetas,"%",sep="")
  pie(proporciones_r,labels = etiquetas, col=rainbow(length(etiquetas)), main=c("Estudiantes inactivos en ",i))
  legend("topright", c("regular","equivalencia","ciu","opsu"), cex = 0.5,
         fill = rainbow(length(proporciones_r)))
  
  #Porcentaje de retiros por cada uno
  pct_indiv_r<-proporciones_r/proporciones*100
  print(c("Porcentaje de retirados en ",i,"REGULAR EQUIVALENCIA CIU OPSU",pct_indiv_r))
  
  
  #######################################NO RETIRADOS########################################################################
  #Cantidad de no retirados
  total_nor<-sj_año[!is.na(sj_año$IINDICE),]
  nor_regular_sj<-sj_año[sj_año$TIPO_DE_INGRESO == "ADMISION REGULAR",]
  nor_equivalencia_sj<-sj_año[sj_año$TIPO_DE_INGRESO == "EQUIVALENCIA",]
  nor_ciu_sj<-sj_año[sj_año$TIPO_DE_INGRESO == "CIU",]
  nor_opsu_sj<-sj_año[sj_año$TIPO_DE_INGRESO == "OPSU",]
  
  #media de admision total
  print(c("Media de admision regular total de ",i,mean(total_nor$IINDICE)))
  #desviacion estandar de admision total
  print(c("Desviación estandar de admision total de  ",i,sd(total_nor$IINDICE)))
  
  #media de admision total Regular
  print(c("Media de admision regular de ",i,mean(nor_regular_sj$IINDICE)))
  #desviacion estandar de admision regular
  print(c("Desviación estandar regular de  ",i,sd(nor_regular_sj$IINDICE) ))
  
  #media de admision total Equivalencia
  print(c("Media de admision equivalencia de ",i,mean(nor_equivalencia_sj$IINDICE)))
  #desviacion estandar de Equivalencia
  print(c("Desviación estandar equivalencia de  ",i,sd(nor_equivalencia_sj$IINDICE)))

#media de admision total CIU
print(c("Media de admision CIU de ",i,mean(nor_ciu_sj$IINDICE)))
#desviacion estandar de CIU
print(c("Desviación estandar CIU total de  ",i,sd(nor_ciu_sj$IINDICE) ))

#media de admision total OPSU
print(c("Media de admision OPSU de ",i,mean(nor_opsu_sj$IINDICE)))
#desviacion estandar de OPSU
print(c("Desviación estandar OPSU de  ",i,sd(nor_opsu_sj$IINDICE) ))

#Numero de graduados hay por cada metodo de admision
total_graduados <- sj_año[sj_año$DESC_SITUACION_GLOBAL == "GRADUADO" ,]
graduados_regular_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "ADMISION REGULAR",]
graduados_equivalencia_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "EQUIVALENCIA",]
graduados_ciu_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "CIU",]
graduados_opsu_sj<-total_graduados[total_graduados$TIPO_DE_INGRESO == "OPSU",]


graduados_regular=dim(graduados_regular_sj)
graduados_equivalencia=dim(graduados_equivalencia_sj)
graduados_ciu=dim(graduados_ciu_sj)
graduados_opsu=dim(graduados_opsu_sj)
print(c("Estudiantes graduados TOTAL en ",i,dim(total_graduados)[1]))
print(c("Estudiantes graduados REGULAR en ",i,graduados_regular[1]))
print(c("Estudiantes graduados EQUIVALENCIA en ",i,graduados_equivalencia[1]))
print(c("Estudiantes graduados en CIU",i,graduados_ciu[1]))
print(c("Estudiantes graduados en OPSU",i,graduados_opsu[1]))
proporciones_g<- c(graduados_regular[1], graduados_equivalencia[1], graduados_ciu[1], graduados_opsu[1])
etiquetas <- c("regular","equivalencia","ciu","opsu")
pct <- round(proporciones_g/sum(proporciones_g)*100)
etiquetas <- paste(etiquetas, pct)
etiquetas <- paste(etiquetas,"%",sep="")
pie(proporciones_g,labels = etiquetas, col=rainbow(length(etiquetas)), main=c("Estudiantes graduados en ",i))
legend("topright", c("regular","equivalencia","ciu","opsu"), cex = 0.5,
       fill = rainbow(length(proporciones_r)))

#Porcentaje de graduados por cada metodo de admision
pct_indiv_g<-proporciones_g/proporciones*100
print(c("Porcentaje de graduados en ",i,"REGULAR EQUIVALENCIA CIU OPSU",pct_indiv_g))


cat ("Press [enter] to continue")
line <- readline()
}



