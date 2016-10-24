


library(Hmisc)
library(dplyr)
library(brostatistics)

##Leemos los datos-----------------------------------------------------------------
datos1 <- spss.get('Sistema de informaci n Azteca Ola 2_A_Final 12-10-2016 15-39-45.sav')
datos2 <- spss.get('Sistema de informaci n Azteca Ola 2_B_Final 12-10-2016 15-38-30.sav')

#fusiono las bases
datos <- rbind(datos1,datos2)
names(datos)


#####
#corregir el caso que tiene True en P30.1.r24..Es.un.medio.que.tiene.bue.r1..Radio
#verifico que el caso existe
table(datos$P30.1.r24..Es.un.medio.que.tiene.bue.r1..Radio)

#identifico el renglón
datos[datos$P30.1.r24..Es.un.medio.que.tiene.bue.r1..Radio==T,c(781:783)]

#corrijo el valor
datos[936,783]<-F
table(datos$P30.1.r24..Es.un.medio.que.tiene.bue.r1..Radio)


###Reduzco el número de columnas que corresponden al "Conocimiento"-------------

#Obtengo solo las variables que me interesan (top of mind, share y ayudado)
prueba<-datos[,grepl('^P25.|^P26.|^P27.',names(datos))]
names(prueba)
str(prueba)
summary(prueba)

#Convierto a numéricas las variables
prueba<-prueba%>%mutate_each(funs(as.numeric))-1

#Reemplazo los NA's por 0
for(i in 1:nrow(prueba)){
    for(j in 1:length(prueba)){
        if(is.na(prueba[i,j]))
            prueba[i,j]=0
    }
}

#Construyo las etiquetas para las respuestas de cada variable
canales <- c('Canal2','Canal4','Canal5','Canal7','Canal9','Canal11',
             'Canal13','Canal22','Canal28','Canal34','Canal40','Canal21',
             'Canal20','Canal11ninos','Canal27','Canal30','Otro','NSNC')

#Construyo dos vectores que serán los nombres de las columnas donde 
#se almacenarán los resultados
share<-paste('sh',seq(1:7),sep="")
ayudado<-paste('ay',seq(1:18),sep = "")
top<-1

#Construyo un dataframe donde copiaré los valores
tablaConocimiento<-as.data.frame(matrix(ncol=top + length(share) + length(ayudado),
                                        nrow = nrow(prueba)))

names(tablaConocimiento)<-c('tom',share,ayudado)

#
numCanales = length(canales) 
totalColumnas=length(prueba)


for(i in 1:nrow(prueba)){
    k<-9
    for(j in 1:totalColumnas){
        if(j <= numCanales){   #obtenemos el Top of mind
            if(prueba[i,j]>0){
                tablaConocimiento[i,1]= canales[j]
                #cat(paste('\n TOM acum[i,1]:', acum[i,1], ', canales[j]:' , canales[j], '(', i, ', ', j, ')'))
            }
        }else if (j > numCanales & j <= numCanales*8){ #obtengo el share,8 indica el total de bloques (tom + share)
            if(prueba[i,j]>0){
                bloque = ceiling(j / numCanales) #obtengo 'cota superior' para identificar la columna donde se copiará el
                indiceCanal = j %% numCanales #obtengo el modulo para etiquetar de acuerdo a canal
                canal = ifelse(indiceCanal == 0, NA, canales[indiceCanal]) #obtengo la etiqueta                
                if (match (canal, tablaConocimiento[i,],  nomatch = -1) < 0){ #valido que no se repita el canal
                    tablaConocimiento[i, bloque] = canal
                    #cat(paste('\n SH acum[i,', bloque, ']:', acum[i,bloque], ', canal:' , canal, '(', i, ', ', j, ')'))
                }else{
                    #cat(paste('\n', canal, ' repetido en (', i, ', ', j, ')'))
                }
            }  
        } else {
            if(prueba[i,j]>0){ #obtengo el ayudado
                indiceCanal = j %% numCanales #obtengo el modulo para etiquetar de acuerdo a canal
                canal = ifelse(indiceCanal == 0, NA, canales[indiceCanal]) #obtengo la etiqueta                
                if (match (canal, tablaConocimiento[i,],  nomatch = -1) < 0){
                    tablaConocimiento[i, k] = canal
                    #cat(paste('\n AY acum[i,', k, ']:', acum[i,k], ', canal:' , canal, '(', i, ', ', j, ')'))
                    k <- k + 1
                }else{
                    #cat(paste('\n', canal, ' repetido en (', i, ', ', j, ')'))
                }
            }
        }
    }
}

#Convierto a factor las variables

for(j in 1:length(tablaConocimiento)){
    tablaConocimiento[,j]<-factor(tablaConocimiento[,j],
                                  levels = canales, 
                                  exclude = NA, ordered = T)
}

summary(tablaConocimiento)
str(tablaConocimiento)

#Elimino las variables del ayudado que están vacías
tablaConocimiento <- tablaConocimiento[,c(1:15)]

write.table(tablaConocimiento,'conocimiento.csv')

###Fusiono datos con las nuevas variables de conocimiento

datos<-cbind(datos,tablaConocimiento)
# str(datos$sh1)
# levels(datos$tom)
# levels(datos$sh1)

######CANAL MAS FRECUENTE

#Extraigo las columnas 'P28'
p28 <- datos %>% dplyr::select(contains('P28'))

#Convierto a numericas
p28 <-p28 %>% dplyr::mutate_each(funs(as.numeric))-1


#Creo un vector donde almacenaré el 'canal más frecuente'
canalMasFrecuente <- vector()

#Copio los valores al vector
for( i in 1:nrow(datos)){
    for (j in 1:length(p28)){
        if(p28[i,j]>0) {
            canalMasFrecuente[i]=canales[j]
        }
    }
}


canalMasFrecuente <- factor(canalMasFrecuente, 
                            levels = canales,
                            exclude = NA,
                            ordered = T)

datos<-cbind(datos,canalMasFrecuente)

#table(datos$canalMasFrecuente)

################## TTB,NEUTRO Y BTB--------------------------------

datos <- within(datos,{
    
    P23.RadioREC <- NA
    P23.RadioREC[P23.Radio <= 3] <- 3
    P23.RadioREC[P23.Radio >= 4 & P23.Radio <= 7] <- 2
    P23.RadioREC[P23.Radio >= 8] <- 1
    }
    )

datos$P23.RadioREC <- factor(datos$P23.RadioREC,
                             levels = c(1:3),
                             labels = c('TTB','Neutro','BTB'))

###########
datos <- within(datos,{
    
    P23.CanalesTVPagaREC <- NA
    P23.CanalesTVPagaREC[P23.Canales.de.televisi.n.de.paga <= 3] <- 3
    P23.CanalesTVPagaREC[P23.Canales.de.televisi.n.de.paga >= 4 & 
                          P23.Canales.de.televisi.n.de.paga <= 7] <- 2
    P23.CanalesTVPagaREC[P23.Canales.de.televisi.n.de.paga >= 8] <- 1
}
)

datos$P23.CanalesTVPagaREC <- factor(datos$P23.CanalesTVPagaREC,
                             levels = c(1:3),
                             labels = c('TTB','Neutro','BTB'))

##################################



datos <- within(datos,{
    
    P23.InternetREC <- NA
    P23.InternetREC[P23.Internet <= 3] <- 3
    P23.InternetREC[P23.Internet >= 4 & P23.Internet <= 7] <- 2
    P23.InternetREC[P23.Internet >= 8] <- 1
    }
    )

datos$P23.InternetREC <- factor(datos$P23.InternetREC,
                                     levels = c(1:3),
                                     labels = c('TTB','Neutro','BTB'))

#######################

datos <- within(datos,{
    
    P23.CanalesTVAbiertaREC <- NA
    P23.CanalesTVAbiertaREC[P23.Canales.de.televisi.n.abierta <= 3] <- 3
    P23.CanalesTVAbiertaREC[P23.Canales.de.televisi.n.abierta >= 4 & 
                                P23.Canales.de.televisi.n.abierta <= 7] <- 2
    P23.CanalesTVAbiertaREC[P23.Canales.de.televisi.n.abierta >= 8] <- 1
    }
    )

datos$P23.CanalesTVAbiertaREC <- factor(datos$P23.CanalesTVAbiertaREC,
                                     levels = c(1:3),
                                     labels = c('TTB','Neutro','BTB'))

#######################

datos <- within(datos,{
    
    P23.ContenidosInternetREC <- NA
    P23.ContenidosInternetREC[P23.Contenidos.a.trav.s.de.interne <= 3] <- 3
    P23.ContenidosInternetREC[P23.Contenidos.a.trav.s.de.interne >= 4 & 
                                  P23.Contenidos.a.trav.s.de.interne <= 7] <- 2
    P23.ContenidosInternetREC[P23.Contenidos.a.trav.s.de.interne >= 8] <- 1
    }
    )


datos$P23.ContenidosInternetREC <- factor(datos$P23.ContenidosInternetREC,
                                        levels = c(1:3),
                                        labels = c('TTB','Neutro','BTB'))


########## Recodificación P33

p33<-c("P33.El.nuevo.logotipo","P33.Nueva.programaci.n",
       "P33.Cambi..de.algunos.conductores","P33.Nuevos.horarios.en.su.programa")

for(j in 1:length(p33)){
        i=p33[j]
        datos[,i] <- as.character(datos[,i])
        }
        
for(i in 1:nrow(datos)){
    for(j in 1:length(p33)){
        k=p33[j]
        if(is.na(datos[i,k])){
            datos[i,k]<-0
        }
    }
}

 
for(i in 1:nrow(datos)){
    for(j in 1:length(p33)){
        k=p33[j]
        if(datos[i,k]=='1 No me gusto'){
            datos[i,k] <- '1'
        } else if(datos[i,k]=='10 Me gusto mucho'){
            datos[i,k] <- '10'
        }
    }
}

for(j in 1:length(p33)){
    i=p33[j]
    datos[,i] <- as.numeric(datos[,i])
}


######Recodifico las preguntas P33

datos <- within(datos,{
    
    P33.El.nuevo.logotipoREC <- NA
    P33.El.nuevo.logotipoREC[P33.El.nuevo.logotipo >= 1 & P33.El.nuevo.logotipo <= 3] <- 3
    P33.El.nuevo.logotipoREC[P33.El.nuevo.logotipo >= 4 & P33.El.nuevo.logotipo <= 7] <- 2
    P33.El.nuevo.logotipoREC[P33.El.nuevo.logotipo >= 8] <- 1
    }
    )

datos$P33.El.nuevo.logotipoREC <- factor(datos$P33.El.nuevo.logotipoREC,
                             levels = c(1:3),
                             labels = c('TTB','Neutro','BTB'))


#######

datos <- within(datos,{
    
    P33.Nueva.programaci.nREC <- NA
    P33.Nueva.programaci.nREC[P33.Nueva.programaci.n >= 1 & P33.Nueva.programaci.n <= 3] <- 3
    P33.Nueva.programaci.nREC[P33.Nueva.programaci.n >= 4 & P33.Nueva.programaci.n <= 7] <- 2
    P33.Nueva.programaci.nREC[P33.Nueva.programaci.n >= 8] <- 1
    }
    )

datos$P33.Nueva.programaci.nREC <- factor(datos$P33.Nueva.programaci.nREC,
                                         levels = c(1:3),
                                         labels = c('TTB','Neutro','BTB'))

#######

datos <- within(datos,{
    
    P33.Cambi..de.algunos.conductoresREC <- NA
    P33.Cambi..de.algunos.conductoresREC[P33.Cambi..de.algunos.conductores >= 1 & 
                                             P33.Cambi..de.algunos.conductores <= 3] <- 3
    P33.Cambi..de.algunos.conductoresREC[P33.Cambi..de.algunos.conductores >= 4 & 
                                             P33.Cambi..de.algunos.conductores <= 7] <- 2
    P33.Cambi..de.algunos.conductoresREC[P33.Cambi..de.algunos.conductores >= 8] <- 1
    }
    )


datos$P33.Cambi..de.algunos.conductoresREC <- factor(datos$P33.Cambi..de.algunos.conductoresREC,
                                          levels = c(1:3),
                                          labels = c('TTB','Neutro','BTB'))

#######


datos <- within(datos,{
    
    P33.Nuevos.horarios.en.su.programaREC <- NA
    P33.Nuevos.horarios.en.su.programaREC[P33.Nuevos.horarios.en.su.programa >= 1 & 
                                              P33.Nuevos.horarios.en.su.programa <= 3] <- 3
    P33.Nuevos.horarios.en.su.programaREC[P33.Nuevos.horarios.en.su.programa >= 4 & 
                                              P33.Nuevos.horarios.en.su.programa <= 7] <- 2
    P33.Nuevos.horarios.en.su.programaREC[P33.Nuevos.horarios.en.su.programa >= 8] <- 1
    }
    )

datos$P33.Nuevos.horarios.en.su.programaREC <- factor(datos$P33.Nuevos.horarios.en.su.programaREC,
                                                     levels = c(1:3),
                                                     labels = c('TTB','Neutro','BTB'))

#####Ponderación----------------------------------------------------------------

#verificamos que las variables estén como tipo Factor
# table(datos$F1)
# table(datos$Rangos.edad)
# table(datos$Plaza)
# levels(datos$F1)
# levels(datos$Rangos.edad)
# levels(datos$Plaza)

#Seleccionamos las variables que se considerarán para la ponderación 
b2<-datos %>% 
    dplyr::select(F1,Rangos.edad,Plaza) 

summary(b2) 


#install.packages("survey")
library(survey) 

s<-data.frame(F1=levels(b2$F1),Freq=c(458,496))
e<-data.frame(Rangos.edad=levels(b2$Rangos.edad),Freq=c(250,273,242,189)) 
p<-data.frame(Plaza=levels(b2$Plaza),Freq=c(309,268,179,73,126)) 


diseno<-svydesign(data=b2,id=~1) 

diseno2<-rake(diseno,sample=list(~F1,~Rangos.edad,~Plaza),population=list(s,e,p)) 

#obtenemos la media y con la muestra original (sin ponderar) obtenemos los 
#porcentajes originales con una SE 
svymean(~F1,diseno) 
svymean(~Rangos.edad,diseno) 
svymean(~Plaza,diseno) 

#obtenemos la media y con la muestra ya ponderada los porcentajes deben 
#corresponder a los poblacionales, la desviación estándar debe ser cero
svymean(~F1,diseno2) 
svymean(~Rangos.edad,diseno2) 
svymean(~Plaza,diseno2) 

#weights(diseno2)

######Agregar la variable para la ponderación final 
datos$ponderador<-weights(diseno2) 
#xtabs(ponderador~F1,data=datos) 
#w<-as.data.frame(xtabs(ponderador~Plaza,data=datos)) 

# alfa<-weights(diseno2)
# write.csv(alfa,'ponderadores.csv') 
# summary(datos$ponderador) 
# sum(datos$ponderador) 

####Variables banner------------------------------------------------------------------

######Creo la variable 'nseREC'

datos <- within(datos,{
    nseREC <- NA
    nseREC[NSE=='C+'] <- 1
    nseREC[NSE=='C']  <- 2
    nseREC[NSE=='C-' | NSE=='D+'] <- 3
    nseREC[NSE=='D'] <- 4
    }
    )

datos$nseREC <- factor(datos$nseREC,
                       levels = seq(1:4),
                       labels = c('C+','C tipico','C-/D+','D'))


######Creo la variable 'tipo de audiencia'

datos <- within(datos,{
    
    tipoAudiencia <- NA
    
    tipoAudiencia[canalMasFrecuente=='Canal2' |
                      canalMasFrecuente=='Canal4' |
                      canalMasFrecuente=='Canal5' |
                      canalMasFrecuente=='Canal9' ] <- 2
    
    tipoAudiencia[canalMasFrecuente=='Canal7' |
                      canalMasFrecuente=='Canal13' |
                      canalMasFrecuente=='Canal40'] <- 1
    }
    )

#table(datos$tipoAudiencia)

datos$tipoAudiencia <- factor(datos$tipoAudiencia,
                       levels = seq(1:2),
                       labels = c('AudienciaAzteca','AudienciaTelevisa'))



##########################################################################################
# Obtengo variable "total
datos$Total <- factor(1,levels = 1,labels = "Total")

#write.csv(datos,'SIAv1.csv')
##########################################################################################

# Reporte

bandera<- c("Total","F1","Rangos.edad","nseREC","Plaza","tipoAudiencia",
            "P28.Canal.7..azteca.siete",
            "P28.Canal.13..azteca.trece",
            "P28.Canal.40..Proyecto.40",
            "P28.Canal.2..canal.de.las.estrella",
            "P28.Canal.4..foro.tv",
            "P28.Canal.5",
            "P28.Canal.9..Galavisi.n",
            "Generacion","B1")


resultVarBanner <- list(
    genero = frecuentator(fTtabla = datos,fTvariables = 'F1',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
    
    edad = frecuentator(fTtabla = datos,fTvariables = 'Rangos.edad',fTlevels = T,
                        fbanner = bandera, fTponderador = 'ponderador'),
    
    NSE = frecuentator(fTtabla = datos,fTvariables = 'nseREC',fTlevels = T,
                        fbanner = bandera, fTponderador = 'ponderador'),
    
    plaza = frecuentator(fTtabla = datos,fTvariables = 'Plaza',fTlevels = T,
                       fbanner = bandera, fTponderador = 'ponderador'),
    
    audiencia = frecuentator(fTtabla = datos,fTvariables = 'tipoAudiencia',fTlevels = T,
                         fbanner = bandera, fTponderador = 'ponderador'),
    
    canal7 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.7..azteca.siete',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
    
    canal13 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.13..azteca.trece',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
    
    canal40 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.40..Proyecto.40',fTlevels = T,
                           fbanner = bandera, fTponderador = 'ponderador'),
    
    canal2 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.2..canal.de.las.estrella',fTlevels = T,
                           fbanner = bandera, fTponderador = 'ponderador'),
    
    canal4 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.4..foro.tv',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
    
    canal5 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.5',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
    
    canal9 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.9..Galavisi.n',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
    
    generacion = frecuentator(fTtabla = datos,fTvariables = 'Generacion',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
    
    tipoFamilia = frecuentator(fTtabla = datos,fTvariables = 'B1',fTlevels = T,
                              fbanner = bandera, fTponderador = 'ponderador'),
    
    edoCivil = frecuentator(fTtabla = datos,fTvariables = 'EstadoCivil',fTlevels = T,
                               fbanner = bandera, fTponderador = 'ponderador'),
    
    ocupacion = frecuentator(fTtabla = datos,fTvariables = 'Ocupacion',fTlevels = T,
                               fbanner = bandera, fTponderador = 'ponderador'),
    
    A = frecuentator(fTtabla = datos,fTvariables = 'A1',fTlevels = T,
                   fbanner = bandera, fTponderador = 'ponderador'),
    
    P3 = frecuentator(fTtabla = datos,fTvariables = c('P3.Temas.que.me.dejen.un.conocimi',
                                                      'P3.Sobre.el.medio.del.espect.culo',
                                                      'P3.Sobre.chismes',
                                                      'P3.Sobre.religi.n',
                                                      'P3.Pol.tica',
                                                      'P3.Estilo.de.vida',
                                                      'P3.Temas.de.moda',
                                                      'P3.Temas.de.actualidad',
                                                      'P3.Sobre.tecnolog.a',
                                                      'P3.Sobre.deportes',
                                                      'P3.Sobre.viajes',
                                                      'P3.Noticias.nacionales.e.internac',
                                                      'P3.Temas.sobre.la.familia',
                                                      'P3.Sobre.la.educaci.n',
                                                      'P3.Sobre.el.trabajo',
                                                      'P3.Sobre.las.cosas.hice.me.pasaro',
                                                      'P3.Otro..Especificar'),
                     fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
    
    P4 = frecuentator(fTtabla = datos,fTvariables = 'P4',fTlevels = T,
                     fbanner = bandera, fTponderador = 'ponderador'),
    
    P6 = frecuentator(fTtabla = datos,fTvariables = c('P6.Escuchar.m.sica',
                                                      'P6.Hacer.ejercicio',
                                                      'P6.Salir.al.jard.n',
                                                      'P6.Ver.televisi.n..pel.culas..ser',
                                                      'P6.Ver.pel.culas.en.Netflix',
                                                      'P6.Leer..libro..revista..gaceta..',
                                                      'P6.Practicar.deportes',
                                                      'P6.Convivir.con.mi.familia..hijos',
                                                      'P6.Navegar.en.internet',
                                                      'P6.Revisar.redes.sociales',
                                                      'P6.Ver.videos.de.Youtube',
                                                      'P6.Jugar.video.juegos',
                                                      'P6.Jugar.con.mis.mascotas',
                                                      'P6.Darme.un.buen.ba.o',
                                                      'P6.Escuchar.el.radio',
                                                      'P6.Hablar.por.tel.fono',
                                                      'P6.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
    
    P7 = frecuentator(fTtabla = datos,fTvariables = c('P7.Pasear.al.aire.libre',
                                                      'P7.Salir.a.comer',
                                                      'P7.Salir.a.caminar',
                                                      'P7.Ir.bailar',
                                                      'P7.Salir.a.tomar.un.caf',
                                                      'P7.Leer..libro..revista..gaceta..',
                                                      'P7.Practicar.deportes',
                                                      'P7.Ir.al.gimnasio',
                                                      'P7.Ir.al.cine',
                                                      'P7.Visitar.espacios.p.blicos..Par',
                                                      'P7.Ir.a.visitar.a.mi.familia...am',
                                                      'P7.Navegar.en.internet',
                                                      'P7.Revisar.redes.sociales',
                                                      'P7.Ir.de.compras',
                                                      'P7.Jugar.con.mis.mascotas',
                                                      'P7.Salir.de..antro',
                                                      'P7.Practicar.un.hobbie',
                                                      'P7.Convivir.con.amigos...familia',
                                                      'P7.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
    
    P8 = frecuentator(fTtabla = datos,fTvariables = c('P8.Escuchar.m.sica',
                                                      'P8.Hacer.ejercicio',
                                                      'P8.Salir.al.jard.n',
                                                      'P8.Ver.televisi.n..pel.culas..ser',
                                                      'P8.Ver.pel.culas.en.Netflix',
                                                      'P8.Leer..libro..revista..gaceta..',
                                                      'P8.Practicar.deportes',
                                                      'P8.Convivir.con.mi.familia..hijos',
                                                      'P8.Navegar.en.internet',
                                                      'P8.Revisar.redes.sociales',
                                                      'P8.Ver.videos.de.Youtube',
                                                      'P8.Jugar.video.juegos',
                                                      'P8.Jugar.con.mis.mascotas',
                                                      'P8.Darme.un.buen.ba.o',
                                                      'P8.Escuchar.el.radio',
                                                      'P8.Hablar.por.tel.fono',
                                                      'P8.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
    
 P9 = frecuentator(fTtabla = datos,fTvariables = c('P9.Pasear.al.aire.libre',
                                                   'P9.Salir.a.comer',
                                                   'P9.Salir.a.caminar',
                                                   'P9.Ir.bailar',
                                                   'P9.Salir.a.tomar.un.caf',
                                                   'P9.Leer..libro..revista..gaceta..',
                                                   'P9.Practicar.deportes',
                                                   'P9.Ir.al.gimnasio',
                                                   'P9.Ir.al.cine',
                                                   'P9.Visitar.espacios.p.blicos..Par',
                                                   'P9.Ir.a.visitar.a.mi.familia.amig',
                                                   'P9.Navegar.en.internet',
                                                   'P9.Revisar.redes.sociales',
                                                   'P9.Ir.de.compras',
                                                   'P9.Jugar.con.mis.mascotas',
                                                   'P9.Salir.de..antro',
                                                   'P9.Practicar.un.hobbie',
                                                   'P9.Convivir.con.amigos...familia',
                                                   'P9.Salir.de.viaje',
                                                   'P9.Otro..Especificar'),
                   fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
 
 P10 = frecuentator(fTtabla = datos,fTvariables = 'P10',fTlevels = T,
                   fbanner = bandera, fTponderador = 'ponderador'),
 
 P12 = frecuentator(fTtabla = datos,fTvariables = 'P12',fTlevels = T,
                    fbanner = bandera, fTponderador = 'ponderador'),
 
 P14 = frecuentator(fTtabla = datos,fTvariables = 'P14',fTlevels = T,
                    fbanner = bandera, fTponderador = 'ponderador'),
 
 P15 = frecuentator(fTtabla = datos[datos$P14=='No',],fTvariables = c('P15.Siempre.transmiten.lo.mismo',
                                                   'P15.No.pasan.series.actualizadas',
                                                   'P15.El.contenido.es.repetitivo',
                                                   'P15.No.transmiten.pel.culas.nuevas',
                                                   'P15.No.hay.muchas.opciones.de.cont',
                                                   'P15.No.me.gustan.los.programas.que',
                                                   'P15.Porque.no.ofrece.nada.nuevo',
                                                   'P15.Es.aburrida',
                                                   'P15.No.hay.informaci.n.de.calidad',
                                                   'P15.No.me.gusta',
                                                   'P15.Otro..Especificar'),
                   fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
 
 P16 = frecuentator(fTtabla = datos[datos$P14=='Sí',],fTvariables = c('P16.Canales.de.televisi.n.de.paga',
                                                                      'P16.Canales.de.televisi.n.abierta',
                                                                      'P16.Contenidos.a.trav.s.de.interne',
                                                                      'P16.Todas.la.anteriores..Paga..Abi',
                                                                      'P16.Canales.de.Tv.de.paga.y.Tv.abi',
                                                                      'P16.Canales.Tv.de.paga.y.Contenido',
                                                                      'P16.Canales.Tv.abierta.y.Contenido'),
                    fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
 
 P17 = frecuentator(fTtabla = datos %>% dplyr:: filter(P16.Canales.de.televisi.n.de.paga=='TRUE' | 
                                        P16.Todas.la.anteriores..Paga..Abi=='TRUE' |
                                        P16.Canales.de.Tv.de.paga.y.Tv.abi=='TRUE' |  
                                        P16.Canales.Tv.de.paga.y.Contenido=='TRUE'),
                    fTvariables = c('P17.Mis.artistas.favoritos',
                                    'P17.Contenidos.m.s.actuales',
                                    'P17.Mis.programas.favoritos',
                                    'P17.Variedad.de.contenidos',
                                    'P17.Programas.en.horarios.adecuado',
                                    'P17.Talento..artistas.internaciona',
                                    'P17.Talento.artistas.mexicanos',
                                    'P17.Programas.apegados.a.la.realid',
                                    'P17.Mejores.contenidos.en.general',
                                    'P17.Programaci.n.en.otros.idiomas',
                                    'P17.Mayor.calidad.en.las.produccio',
                                    'P17.Contenidos.internacionales',
                                    'P17.Est.n.actualizados.en.los.prog',
                                    'P17.Contenidos.de.calidad',
                                    'P17.Contenidos.exclusivos.como.ser',
                                    'P17.Mayor.variedad.de.canales',
                                    'P17.M.s.contenido.para.ni.os',
                                    'P17.Otro..Especificar'),
                   fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
 
 P19 = frecuentator(fTtabla = datos %>% dplyr:: filter(P16.Canales.de.televisi.n.abierta=='TRUE' | 
                                                           P16.Todas.la.anteriores..Paga..Abi=='TRUE' |
                                                           P16.Canales.de.Tv.de.paga.y.Tv.abi=='TRUE' |  
                                                           P16.Canales.Tv.abierta.y.Contenido=='TRUE'),
                    fTvariables = c('P19.Mis.artistas.favoritos',
                                    'P19.Contenidos.m.s.actuales',
                                    'P19.Mis.programas.favoritos',
                                    'P19.Variedad.de.contenidos',
                                    'P19.Programas.en.horarios.adecuado',
                                    'P19.Talento..artistas.internaciona',
                                    'P19.Talento.artistas.mexicanos',
                                    'P19.Programas.apegados.a.la.realid',
                                    'P19.Mejores.contenidos.en.general',
                                    'P19.Programaci.n.en.otros.idiomas',
                                    'P19.Mayor.calidad.en.las.produccio',
                                    'P19.Contenidos.internacionales',
                                    'P19.Est.n.actualizados.en.los.prog',
                                    'P19.Contenidos.de.calidad',
                                    'P19.Contenidos.exclusivos.como.ser',
                                    'P19.Mayor.variedad.de.canales',
                                    'P19.M.s.contenido.para.ni.os',
                                    'P19.Otro..Especificar'),
                    fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
 
 P21 = frecuentator(fTtabla = datos %>% dplyr:: filter(P16.Contenidos.a.trav.s.de.interne=='TRUE' | 
                                                           P16.Todas.la.anteriores..Paga..Abi=='TRUE' |
                                                           P16.Canales.Tv.de.paga.y.Contenido=='TRUE' |  
                                                           P16.Canales.Tv.abierta.y.Contenido=='TRUE'),
                    fTvariables = c('P21.Mis.artistas.favoritos',
                                    'P21.Contenidos.m.s.actuales',
                                    'P21.Mis.programas.favoritos',
                                    'P21.Variedad.de.contenidos',
                                    'P21.Programas.en.horarios.adecuado',
                                    'P21.Talento..artistas.internaciona',
                                    'P21.Talento.artistas.mexicanos',
                                    'P21.Programas.apegados.a.la.realid',
                                    'P21.Mejores.contenidos.en.general',
                                    'P21.Programaci.n.en.otros.idiomas',
                                    'P21.Mayor.calidad.en.las.produccci',
                                    'P21.Contenidos.internacionales',
                                    'P21.Est.n.actualizados.en.los.prog',
                                    'P21.Contenidos.de.calidad',
                                    'P21.Contenidos.exclusivos.como.ser',
                                    'P21.Mayor.variedad.de.canales',
                                    'P21.Ver.cuando.yo.quiera.y.a.la.ho',
                                    'P21.No.tiene.limitantes.en.horario',
                                    'P21.No.tengo.que.esperar.al.siguie',
                                    'P21.Puedo.ver.todos.cap.tulos.del.',
                                    'P21.M.s.contenido.para.ni.os',
                                    'P21.Otro..Especificar'),
                    fTlevels = F,fbanner = bandera, fTponderador = 'ponderador'),
 
 P23_radio = frecuentator(fTtabla = datos,fTvariables = 'P23.RadioREC',fTlevels = T,
                              fbanner = bandera, fTponderador = 'ponderador'),
 
 P23_canalesTVPaga = frecuentator(fTtabla = datos,fTvariables = 'P23.CanalesTVPagaREC',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador'),
 
 P23_internet = frecuentator(fTtabla = datos,fTvariables = 'P23.InternetREC',fTlevels = T,
                                  fbanner = bandera, fTponderador = 'ponderador'),
 
 P23_canalesTVAbierta = frecuentator(fTtabla = datos,fTvariables = 'P23.CanalesTVAbiertaREC',fTlevels = T,
                             fbanner = bandera, fTponderador = 'ponderador'),
 
 P23_contenidosInternet = frecuentator(fTtabla = datos,fTvariables = 'P23.ContenidosInternetREC',fTlevels = T,
                                     fbanner = bandera, fTponderador = 'ponderador'),
 
 
 P25_TopOfMind = frecuentator(fTtabla = datos,fTvariables = 'tom',fTlevels = T,
                    fbanner = bandera, fTponderador = 'ponderador'),
 
 P26_ShareEspontaneo = frecuentator(fTtabla = datos,fTvariables = c('sh1','sh2','sh3','sh4',
                                                                    'sh5','sh6','sh7'),
                                    fTlevels = T,fbanner = bandera,
                                    fTponderador = 'ponderador'),
 
 P27_ayudado = frecuentator(fTtabla = datos,fTvariables = c('ay1','ay2','ay3','ay4',
                                                            'ay5','ay6','ay7'),
                                    fTlevels = T,fbanner = bandera,
                                    fTponderador = 'ponderador'),
 
 tom_share = frecuentator(fTtabla = datos,fTvariables = c('tom','sh1','sh2',
                                                                  'sh3','sh4','sh5',
                                                                  'sh6','sh7'),
                                  fTlevels = T,fbanner = bandera,
                                  fTponderador = 'ponderador'),

conocimientoTotal = frecuentator(fTtabla = datos,fTvariables = c('tom','sh1','sh2',
                                                                 'sh3','sh4','sh5',
                                                                 'sh6','sh7','ay1',
                                                                 'ay2','ay3','ay4',
                                                                 'ay5','ay6','ay7'),
                                 fTlevels = T,fbanner = bandera,
                                 fTponderador = 'ponderador'),

P28 = frecuentator(fTtabla = datos,fTvariables = 'canalMasFrecuente',fTlevels = T,
                             fbanner = bandera, fTponderador = 'ponderador'),

P29canal13 = frecuentator(fTtabla = datos,fTvariables = c('P29.Canal13.Es.el..nico.canal.que.consider',
                                                   'P29.Canal13.Es.uno.de.2...3.canales.que.co',
                                                   'P29.Canal13.Es.uno.de.muchos.canales.que.c',
                                                   'P29.Canal13.Es.un.canal.que.quiz..consider',
                                                   'P29.Canal13.Es.uno.de.los.canales.que.nunc',
                                                   'P29.Canal13.NS.NC'),
                                 fTlevels = F,fbanner = bandera,
                                 fTponderador = 'ponderador'),

P29canal2 = frecuentator(fTtabla = datos,fTvariables = c('P29.CanalEstrellas.Es.el..nico.canal.que.consider',
                                                          'P29.CanalEstrellas.Es.uno.de.2...3.canales.que.co',
                                                          'P29.CanalEstrellas.Es.uno.de.muchos.canales.que.c',
                                                          'P29.CanalEstrellas.Es.un.canal.que.quiz..consider',
                                                          'P29.CanalEstrellas.Es.uno.de.los.canales.que.nunc',
                                                          'P29.CanalEstrellas.NS.NC'),
                          fTlevels = F,fbanner = bandera,
                          fTponderador = 'ponderador'),

P29canal7 = frecuentator(fTtabla = datos,fTvariables = c('P29.Canal7.Es.el..nico.canal.que.consider',
                                                         'P29.Canal7.Es.uno.de.2...3.canales.que.co',
                                                         'P29.Canal7.Es.uno.de.muchos.canales.que.c',
                                                         'P29.Canal7.Es.un.canal.que.quiz..consider',
                                                         'P29.Canal7.Es.uno.de.los.canales.que.nunc',
                                                         'P29.Canal7.NS.NC'),
                         fTlevels = F,fbanner = bandera,
                         fTponderador = 'ponderador'),

P29canal5 = frecuentator(fTtabla = datos,fTvariables = c('P29.Canal5.Es.el..nico.canal.que.consider',
                                                         'P29.Canal5.Es.uno.de.2...3.canales.que.co',
                                                         'P29.Canal5.Es.uno.de.muchos.canales.que.c',
                                                         'P29.Canal5.Es.un.canal.que.quiz..consider',
                                                         'P29.Canal5.Es.uno.de.los.canales.que.nunc',
                                                         'P29.Canal5.NS.NC'),
                         fTlevels = F,fbanner = bandera,
                         fTponderador = 'ponderador'),

P29canal9 = frecuentator(fTtabla = datos,fTvariables = c('P29.Galavision.Es.el..nico.canal.que.consider',
                                                         'P29.Galavision.Es.uno.de.2...3.canales.que.co',
                                                         'P29.Galavision.Es.uno.de.muchos.canales.que.c',
                                                         'P29.Galavision.Es.un.canal.que.quiz..consider',
                                                         'P29.Galavision.Es.uno.de.los.canales.que.nunc',
                                                         'P29.Galavision.NS.NC'),
                         fTlevels = F,fbanner = bandera,
                         fTponderador = 'ponderador'),


P29canal40 = frecuentator(fTtabla = datos,fTvariables = c('P29.Proyecto40.Es.el..nico.canal.que.consider',
                                                         'P29.Proyecto40.Es.uno.de.2...3.canales.que.co',
                                                         'P29.Proyecto40.Es.uno.de.muchos.canales.que.c',
                                                         'P29.Proyecto40.Es.un.canal.que.quiz..consider',
                                                         'P29.Proyecto40.Es.uno.de.los.canales.que.nunc',
                                                         'P29.Proyecto40.NS.NC'),
                         fTlevels = F,fbanner = bandera,
                         fTponderador = 'ponderador'),


P29canal4 = frecuentator(fTtabla = datos,fTvariables = c('P29.ForoTV.Es.el..nico.canal.que.consider',
                                                          'P29.ForoTV.Es.uno.de.2...3.canales.que.co',
                                                          'P29.ForoTV.Es.uno.de.muchos.canales.que.c',
                                                          'P29.ForoTV.Es.un.canal.que.quiz..consider',
                                                          'P29.ForoTV.Es.uno.de.los.canales.que.nunc',
                                                          'P29.ForoTV.NS.NC'),
                          fTlevels = F,fbanner = bandera,
                          fTponderador = 'ponderador'),

P31 = frecuentator(fTtabla = datos,fTvariables = 'P31',fTlevels = T,
                   fbanner = bandera, fTponderador = 'ponderador'),

P32 = frecuentator(fTtabla = datos[datos$P31=='Sí',] ,fTvariables = c('P32.Cambi..su.logo',
                                                         'P32.Agreg..nuevos.programas',
                                                         'P32.Quit..programas',
                                                         'P32.Cambi..contenidos',
                                                         'P32.Cambi..conductores',
                                                         'P32.Cambi..horarios.de.programaci.',
                                                         'P32.Otro..Especificar'),
                         fTlevels = F,fbanner = bandera,
                         fTponderador = 'ponderador'),

P33_1Logo = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.El.nuevo.logotipoREC',fTlevels = T,
                   fbanner = bandera, fTponderador = 'ponderador'),

P33_2Programacion = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.Nueva.programaci.nREC',fTlevels = T,
                         fbanner = bandera, fTponderador = 'ponderador'),

P33_3Conductores = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.Cambi..de.algunos.conductoresREC',fTlevels = T,
                                 fbanner = bandera, fTponderador = 'ponderador'),

P33_4Horarios = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.Nuevos.horarios.en.su.programaREC',fTlevels = T,
                                fbanner = bandera, fTponderador = 'ponderador'),

P34 = frecuentator(fTtabla = datos,fTvariables = 'P34.1',fTlevels = T,
                   fbanner = bandera, fTponderador = 'ponderador')


 )

exportator(resultVarBanner,"resultadosGrales.csv")

################################################################################
############# D I F E R E N C I A S  S I G N I F I C A T I V A S ###############
################################################################################



resultVarBanner <- list(
    genero = frecuentator(fTtabla = datos,fTvariables = 'F1',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    edad = frecuentator(fTtabla = datos,fTvariables = 'Rangos.edad',fTlevels = T,
                        fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    NSE = frecuentator(fTtabla = datos,fTvariables = 'nseREC',fTlevels = T,
                       fbanner = bandera, fTponderador = 'ponderador', fTprop = T),
    
    plaza = frecuentator(fTtabla = datos,fTvariables = 'Plaza',fTlevels = T,
                         fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    audiencia = frecuentator(fTtabla = datos,fTvariables = 'tipoAudiencia',fTlevels = T,
                             fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    canal7 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.7..azteca.siete',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    canal13 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.13..azteca.trece',fTlevels = T,
                           fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    canal40 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.40..Proyecto.40',fTlevels = T,
                           fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    canal2 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.2..canal.de.las.estrella',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    canal4 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.4..foro.tv',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    canal5 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.5',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    canal9 = frecuentator(fTtabla = datos,fTvariables = 'P28.Canal.9..Galavisi.n',fTlevels = T,
                          fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    generacion = frecuentator(fTtabla = datos,fTvariables = 'Generacion',fTlevels = T,
                              fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    tipoFamilia = frecuentator(fTtabla = datos,fTvariables = 'B1',fTlevels = T,
                               fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    edoCivil = frecuentator(fTtabla = datos,fTvariables = 'EstadoCivil',fTlevels = T,
                            fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    ocupacion = frecuentator(fTtabla = datos,fTvariables = 'Ocupacion',fTlevels = T,
                             fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    A = frecuentator(fTtabla = datos,fTvariables = 'A1',fTlevels = T,
                     fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P3 = frecuentator(fTtabla = datos,fTvariables = c('P3.Temas.que.me.dejen.un.conocimi',
                                                      'P3.Sobre.el.medio.del.espect.culo',
                                                      'P3.Sobre.chismes',
                                                      'P3.Sobre.religi.n',
                                                      'P3.Pol.tica',
                                                      'P3.Estilo.de.vida',
                                                      'P3.Temas.de.moda',
                                                      'P3.Temas.de.actualidad',
                                                      'P3.Sobre.tecnolog.a',
                                                      'P3.Sobre.deportes',
                                                      'P3.Sobre.viajes',
                                                      'P3.Noticias.nacionales.e.internac',
                                                      'P3.Temas.sobre.la.familia',
                                                      'P3.Sobre.la.educaci.n',
                                                      'P3.Sobre.el.trabajo',
                                                      'P3.Sobre.las.cosas.hice.me.pasaro',
                                                      'P3.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P4 = frecuentator(fTtabla = datos,fTvariables = 'P4',fTlevels = T,
                      fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P6 = frecuentator(fTtabla = datos,fTvariables = c('P6.Escuchar.m.sica',
                                                      'P6.Hacer.ejercicio',
                                                      'P6.Salir.al.jard.n',
                                                      'P6.Ver.televisi.n..pel.culas..ser',
                                                      'P6.Ver.pel.culas.en.Netflix',
                                                      'P6.Leer..libro..revista..gaceta..',
                                                      'P6.Practicar.deportes',
                                                      'P6.Convivir.con.mi.familia..hijos',
                                                      'P6.Navegar.en.internet',
                                                      'P6.Revisar.redes.sociales',
                                                      'P6.Ver.videos.de.Youtube',
                                                      'P6.Jugar.video.juegos',
                                                      'P6.Jugar.con.mis.mascotas',
                                                      'P6.Darme.un.buen.ba.o',
                                                      'P6.Escuchar.el.radio',
                                                      'P6.Hablar.por.tel.fono',
                                                      'P6.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P7 = frecuentator(fTtabla = datos,fTvariables = c('P7.Pasear.al.aire.libre',
                                                      'P7.Salir.a.comer',
                                                      'P7.Salir.a.caminar',
                                                      'P7.Ir.bailar',
                                                      'P7.Salir.a.tomar.un.caf',
                                                      'P7.Leer..libro..revista..gaceta..',
                                                      'P7.Practicar.deportes',
                                                      'P7.Ir.al.gimnasio',
                                                      'P7.Ir.al.cine',
                                                      'P7.Visitar.espacios.p.blicos..Par',
                                                      'P7.Ir.a.visitar.a.mi.familia...am',
                                                      'P7.Navegar.en.internet',
                                                      'P7.Revisar.redes.sociales',
                                                      'P7.Ir.de.compras',
                                                      'P7.Jugar.con.mis.mascotas',
                                                      'P7.Salir.de..antro',
                                                      'P7.Practicar.un.hobbie',
                                                      'P7.Convivir.con.amigos...familia',
                                                      'P7.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P8 = frecuentator(fTtabla = datos,fTvariables = c('P8.Escuchar.m.sica',
                                                      'P8.Hacer.ejercicio',
                                                      'P8.Salir.al.jard.n',
                                                      'P8.Ver.televisi.n..pel.culas..ser',
                                                      'P8.Ver.pel.culas.en.Netflix',
                                                      'P8.Leer..libro..revista..gaceta..',
                                                      'P8.Practicar.deportes',
                                                      'P8.Convivir.con.mi.familia..hijos',
                                                      'P8.Navegar.en.internet',
                                                      'P8.Revisar.redes.sociales',
                                                      'P8.Ver.videos.de.Youtube',
                                                      'P8.Jugar.video.juegos',
                                                      'P8.Jugar.con.mis.mascotas',
                                                      'P8.Darme.un.buen.ba.o',
                                                      'P8.Escuchar.el.radio',
                                                      'P8.Hablar.por.tel.fono',
                                                      'P8.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P9 = frecuentator(fTtabla = datos,fTvariables = c('P9.Pasear.al.aire.libre',
                                                      'P9.Salir.a.comer',
                                                      'P9.Salir.a.caminar',
                                                      'P9.Ir.bailar',
                                                      'P9.Salir.a.tomar.un.caf',
                                                      'P9.Leer..libro..revista..gaceta..',
                                                      'P9.Practicar.deportes',
                                                      'P9.Ir.al.gimnasio',
                                                      'P9.Ir.al.cine',
                                                      'P9.Visitar.espacios.p.blicos..Par',
                                                      'P9.Ir.a.visitar.a.mi.familia.amig',
                                                      'P9.Navegar.en.internet',
                                                      'P9.Revisar.redes.sociales',
                                                      'P9.Ir.de.compras',
                                                      'P9.Jugar.con.mis.mascotas',
                                                      'P9.Salir.de..antro',
                                                      'P9.Practicar.un.hobbie',
                                                      'P9.Convivir.con.amigos...familia',
                                                      'P9.Salir.de.viaje',
                                                      'P9.Otro..Especificar'),
                      fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P10 = frecuentator(fTtabla = datos,fTvariables = 'P10',fTlevels = T,
                       fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P12 = frecuentator(fTtabla = datos,fTvariables = 'P12',fTlevels = T,
                       fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P14 = frecuentator(fTtabla = datos,fTvariables = 'P14',fTlevels = T,
                       fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P15 = frecuentator(fTtabla = datos[datos$P14=='No',],fTvariables = c('P15.Siempre.transmiten.lo.mismo',
                                                                         'P15.No.pasan.series.actualizadas',
                                                                         'P15.El.contenido.es.repetitivo',
                                                                         'P15.No.transmiten.pel.culas.nuevas',
                                                                         'P15.No.hay.muchas.opciones.de.cont',
                                                                         'P15.No.me.gustan.los.programas.que',
                                                                         'P15.Porque.no.ofrece.nada.nuevo',
                                                                         'P15.Es.aburrida',
                                                                         'P15.No.hay.informaci.n.de.calidad',
                                                                         'P15.No.me.gusta',
                                                                         'P15.Otro..Especificar'),
                       fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P16 = frecuentator(fTtabla = datos[datos$P14=='Sí',],fTvariables = c('P16.Canales.de.televisi.n.de.paga',
                                                                         'P16.Canales.de.televisi.n.abierta',
                                                                         'P16.Contenidos.a.trav.s.de.interne',
                                                                         'P16.Todas.la.anteriores..Paga..Abi',
                                                                         'P16.Canales.de.Tv.de.paga.y.Tv.abi',
                                                                         'P16.Canales.Tv.de.paga.y.Contenido',
                                                                         'P16.Canales.Tv.abierta.y.Contenido'),
                       fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P17 = frecuentator(fTtabla = datos %>% dplyr:: filter(P16.Canales.de.televisi.n.de.paga=='TRUE' | 
                                                              P16.Todas.la.anteriores..Paga..Abi=='TRUE' |
                                                              P16.Canales.de.Tv.de.paga.y.Tv.abi=='TRUE' |  
                                                              P16.Canales.Tv.de.paga.y.Contenido=='TRUE'),
                       fTvariables = c('P17.Mis.artistas.favoritos',
                                       'P17.Contenidos.m.s.actuales',
                                       'P17.Mis.programas.favoritos',
                                       'P17.Variedad.de.contenidos',
                                       'P17.Programas.en.horarios.adecuado',
                                       'P17.Talento..artistas.internaciona',
                                       'P17.Talento.artistas.mexicanos',
                                       'P17.Programas.apegados.a.la.realid',
                                       'P17.Mejores.contenidos.en.general',
                                       'P17.Programaci.n.en.otros.idiomas',
                                       'P17.Mayor.calidad.en.las.produccio',
                                       'P17.Contenidos.internacionales',
                                       'P17.Est.n.actualizados.en.los.prog',
                                       'P17.Contenidos.de.calidad',
                                       'P17.Contenidos.exclusivos.como.ser',
                                       'P17.Mayor.variedad.de.canales',
                                       'P17.M.s.contenido.para.ni.os',
                                       'P17.Otro..Especificar'),
                       fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P19 = frecuentator(fTtabla = datos %>% dplyr:: filter(P16.Canales.de.televisi.n.abierta=='TRUE' | 
                                                              P16.Todas.la.anteriores..Paga..Abi=='TRUE' |
                                                              P16.Canales.de.Tv.de.paga.y.Tv.abi=='TRUE' |  
                                                              P16.Canales.Tv.abierta.y.Contenido=='TRUE'),
                       fTvariables = c('P19.Mis.artistas.favoritos',
                                       'P19.Contenidos.m.s.actuales',
                                       'P19.Mis.programas.favoritos',
                                       'P19.Variedad.de.contenidos',
                                       'P19.Programas.en.horarios.adecuado',
                                       'P19.Talento..artistas.internaciona',
                                       'P19.Talento.artistas.mexicanos',
                                       'P19.Programas.apegados.a.la.realid',
                                       'P19.Mejores.contenidos.en.general',
                                       'P19.Programaci.n.en.otros.idiomas',
                                       'P19.Mayor.calidad.en.las.produccio',
                                       'P19.Contenidos.internacionales',
                                       'P19.Est.n.actualizados.en.los.prog',
                                       'P19.Contenidos.de.calidad',
                                       'P19.Contenidos.exclusivos.como.ser',
                                       'P19.Mayor.variedad.de.canales',
                                       'P19.M.s.contenido.para.ni.os',
                                       'P19.Otro..Especificar'),
                       fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P21 = frecuentator(fTtabla = datos %>% dplyr:: filter(P16.Contenidos.a.trav.s.de.interne=='TRUE' | 
                                                              P16.Todas.la.anteriores..Paga..Abi=='TRUE' |
                                                              P16.Canales.Tv.de.paga.y.Contenido=='TRUE' |  
                                                              P16.Canales.Tv.abierta.y.Contenido=='TRUE'),
                       fTvariables = c('P21.Mis.artistas.favoritos',
                                       'P21.Contenidos.m.s.actuales',
                                       'P21.Mis.programas.favoritos',
                                       'P21.Variedad.de.contenidos',
                                       'P21.Programas.en.horarios.adecuado',
                                       'P21.Talento..artistas.internaciona',
                                       'P21.Talento.artistas.mexicanos',
                                       'P21.Programas.apegados.a.la.realid',
                                       'P21.Mejores.contenidos.en.general',
                                       'P21.Programaci.n.en.otros.idiomas',
                                       'P21.Mayor.calidad.en.las.produccci',
                                       'P21.Contenidos.internacionales',
                                       'P21.Est.n.actualizados.en.los.prog',
                                       'P21.Contenidos.de.calidad',
                                       'P21.Contenidos.exclusivos.como.ser',
                                       'P21.Mayor.variedad.de.canales',
                                       'P21.Ver.cuando.yo.quiera.y.a.la.ho',
                                       'P21.No.tiene.limitantes.en.horario',
                                       'P21.No.tengo.que.esperar.al.siguie',
                                       'P21.Puedo.ver.todos.cap.tulos.del.',
                                       'P21.M.s.contenido.para.ni.os',
                                       'P21.Otro..Especificar'),
                       fTlevels = F,fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P23_radio = frecuentator(fTtabla = datos,fTvariables = 'P23.RadioREC',fTlevels = T,
                             fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P23_canalesTVPaga = frecuentator(fTtabla = datos,fTvariables = 'P23.CanalesTVPagaREC',fTlevels = T,
                                     fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P23_internet = frecuentator(fTtabla = datos,fTvariables = 'P23.InternetREC',fTlevels = T,
                                fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P23_canalesTVAbierta = frecuentator(fTtabla = datos,fTvariables = 'P23.CanalesTVAbiertaREC',fTlevels = T,
                                        fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P23_contenidosInternet = frecuentator(fTtabla = datos,fTvariables = 'P23.ContenidosInternetREC',fTlevels = T,
                                          fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    
    P25_TopOfMind = frecuentator(fTtabla = datos,fTvariables = 'tom',fTlevels = T,
                                 fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P26_ShareEspontaneo = frecuentator(fTtabla = datos,fTvariables = c('sh1','sh2','sh3','sh4',
                                                                       'sh5','sh6','sh7'),
                                       fTlevels = T,fbanner = bandera,
                                       fTponderador = 'ponderador',fTprop = T),
    
    P27_ayudado = frecuentator(fTtabla = datos,fTvariables = c('ay1','ay2','ay3','ay4',
                                                               'ay5','ay6','ay7'),
                               fTlevels = T,fbanner = bandera,
                               fTponderador = 'ponderador',fTprop = T),
    
    tom_share = frecuentator(fTtabla = datos,fTvariables = c('tom','sh1','sh2',
                                                             'sh3','sh4','sh5',
                                                             'sh6','sh7'),
                             fTlevels = T,fbanner = bandera,
                             fTponderador = 'ponderador',fTprop = T),
    
    # conocimientoTotal = frecuentator(fTtabla = datos,fTvariables = c('tom','sh1','sh2',
    #                                                                  'sh3','sh4','sh5',
    #                                                                  'sh6','sh7','ay1',
    #                                                                  'ay2','ay3','ay4',
    #                                                                  'ay5','ay6','ay7'),
    #                                  fTlevels = T,fbanner = bandera,
    #                                  fTponderador = 'ponderador',fTprop = T),
    
    P28 = frecuentator(fTtabla = datos,fTvariables = 'canalMasFrecuente',fTlevels = T,
                       fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P29canal13 = frecuentator(fTtabla = datos,fTvariables = c('P29.Canal13.Es.el..nico.canal.que.consider',
                                                              'P29.Canal13.Es.uno.de.2...3.canales.que.co',
                                                              'P29.Canal13.Es.uno.de.muchos.canales.que.c',
                                                              'P29.Canal13.Es.un.canal.que.quiz..consider',
                                                              'P29.Canal13.Es.uno.de.los.canales.que.nunc',
                                                              'P29.Canal13.NS.NC'),
                              fTlevels = F,fbanner = bandera,
                              fTponderador = 'ponderador',fTprop = T),
    
    P29canal2 = frecuentator(fTtabla = datos,fTvariables = c('P29.CanalEstrellas.Es.el..nico.canal.que.consider',
                                                             'P29.CanalEstrellas.Es.uno.de.2...3.canales.que.co',
                                                             'P29.CanalEstrellas.Es.uno.de.muchos.canales.que.c',
                                                             'P29.CanalEstrellas.Es.un.canal.que.quiz..consider',
                                                             'P29.CanalEstrellas.Es.uno.de.los.canales.que.nunc',
                                                             'P29.CanalEstrellas.NS.NC'),
                             fTlevels = F,fbanner = bandera,
                             fTponderador = 'ponderador',fTprop = T),
    
    P29canal7 = frecuentator(fTtabla = datos,fTvariables = c('P29.Canal7.Es.el..nico.canal.que.consider',
                                                             'P29.Canal7.Es.uno.de.2...3.canales.que.co',
                                                             'P29.Canal7.Es.uno.de.muchos.canales.que.c',
                                                             'P29.Canal7.Es.un.canal.que.quiz..consider',
                                                             'P29.Canal7.Es.uno.de.los.canales.que.nunc',
                                                             'P29.Canal7.NS.NC'),
                             fTlevels = F,fbanner = bandera,
                             fTponderador = 'ponderador',fTprop = T),
    
    P29canal5 = frecuentator(fTtabla = datos,fTvariables = c('P29.Canal5.Es.el..nico.canal.que.consider',
                                                             'P29.Canal5.Es.uno.de.2...3.canales.que.co',
                                                             'P29.Canal5.Es.uno.de.muchos.canales.que.c',
                                                             'P29.Canal5.Es.un.canal.que.quiz..consider',
                                                             'P29.Canal5.Es.uno.de.los.canales.que.nunc',
                                                             'P29.Canal5.NS.NC'),
                             fTlevels = F,fbanner = bandera,
                             fTponderador = 'ponderador',fTprop = T),
    
    P29canal9 = frecuentator(fTtabla = datos,fTvariables = c('P29.Galavision.Es.el..nico.canal.que.consider',
                                                             'P29.Galavision.Es.uno.de.2...3.canales.que.co',
                                                             'P29.Galavision.Es.uno.de.muchos.canales.que.c',
                                                             'P29.Galavision.Es.un.canal.que.quiz..consider',
                                                             'P29.Galavision.Es.uno.de.los.canales.que.nunc',
                                                             'P29.Galavision.NS.NC'),
                             fTlevels = F,fbanner = bandera,
                             fTponderador = 'ponderador',fTprop = T),
    
    
    P29canal40 = frecuentator(fTtabla = datos,fTvariables = c('P29.Proyecto40.Es.el..nico.canal.que.consider',
                                                              'P29.Proyecto40.Es.uno.de.2...3.canales.que.co',
                                                              'P29.Proyecto40.Es.uno.de.muchos.canales.que.c',
                                                              'P29.Proyecto40.Es.un.canal.que.quiz..consider',
                                                              'P29.Proyecto40.Es.uno.de.los.canales.que.nunc',
                                                              'P29.Proyecto40.NS.NC'),
                              fTlevels = F,fbanner = bandera,
                              fTponderador = 'ponderador',fTprop = T),
    
    
    P29canal4 = frecuentator(fTtabla = datos,fTvariables = c('P29.ForoTV.Es.el..nico.canal.que.consider',
                                                             'P29.ForoTV.Es.uno.de.2...3.canales.que.co',
                                                             'P29.ForoTV.Es.uno.de.muchos.canales.que.c',
                                                             'P29.ForoTV.Es.un.canal.que.quiz..consider',
                                                             'P29.ForoTV.Es.uno.de.los.canales.que.nunc',
                                                             'P29.ForoTV.NS.NC'),
                             fTlevels = F,fbanner = bandera,
                             fTponderador = 'ponderador',fTprop = T),
    
    # P31 = frecuentator(fTtabla = datos,fTvariables = 'P31',fTlevels = T,
    #                    fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P32 = frecuentator(fTtabla = datos[datos$P31=='Sí',] ,fTvariables = c('P32.Cambi..su.logo',
                                                                          'P32.Agreg..nuevos.programas',
                                                                          'P32.Quit..programas',
                                                                          'P32.Cambi..contenidos',
                                                                          'P32.Cambi..conductores',
                                                                          'P32.Cambi..horarios.de.programaci.',
                                                                          'P32.Otro..Especificar'),
                       fTlevels = F,fbanner = bandera,
                       fTponderador = 'ponderador',fTprop = T),
    
    P33_1Logo = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.El.nuevo.logotipoREC',fTlevels = T,
                             fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P33_2Programacion = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.Nueva.programaci.nREC',fTlevels = T,
                                     fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P33_3Conductores = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.Cambi..de.algunos.conductoresREC',fTlevels = T,
                                    fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P33_4Horarios = frecuentator(fTtabla = datos[datos$P31=='Sí',],fTvariables = 'P33.Nuevos.horarios.en.su.programaREC',fTlevels = T,
                                 fbanner = bandera, fTponderador = 'ponderador',fTprop = T),
    
    P34 = frecuentator(fTtabla = datos,fTvariables = 'P34.1',fTlevels = T,
                       fbanner = bandera, fTponderador = 'ponderador',fTprop = T)
    
    
)

exportator(resultVarBanner,"resultadosDifSig.csv")


################################################################################
############################# N O R M A L I Z A C I O N ########################
################################################################################


temp <- datosSIA[,grepl('P30.r',names(datosSIA))]
temp <- temp[,!grepl('Ninguno',names(temp))]
names(temp)
str(temp)


#Las variables son categóricas por lo que debo cambiarlas a numéricas 0 y 1
prueba <- temp%>%mutate_each(funs(as.numeric))-1
summary(prueba)

medios <- c('Radio', 'Internet', 'Tv.abierta', 
            'Tv.de.paga', 'Contenido.por.internet')

bases <- list()

for (j in 1:length(medios)){
    i=medios[j]
    bases[[j]]<-temp[,grepl(i,names(temp))]
    bases[[j]]<-as.data.frame(bases[[j]])
}

w<-as.data.frame(bases[1])
