library(brostatistics)
library(dplyr)
library(foreign)
library(Hmisc)


#cargo la base
datos<-spss.get('BaseOla2.sav')
names(datos)


exportarAbiertas(xpa = datos,xpb = "P1",xpc = "Pregunta1.csv" )
exportarAbiertas(xpa = datos,xpb = "P5",xpc = "Pregunta5.csv" )
exportarAbiertas(xpa = datos,xpb = "P11",xpc = "Pregunta11.csv" )
exportarAbiertas(xpa = datos,xpb = "P13",xpc = "Pregunta13.csv" )
exportarAbiertas(xpa = datos,xpb = "P18",xpc = "Pregunta18.csv" )
exportarAbiertas(xpa = datos,xpb = "P20",xpc = "Pregunta20.csv" )
exportarAbiertas(xpa = datos,xpb = "P22",xpc = "Pregunta22.csv" )
exportarAbiertas(xpa = datos,xpb = "P24",xpc = "Pregunta24.csv" )


exportarAbiertas(xpa = datos,xpb = "Otro.Ocupacion",xpc = "Preguntaf4.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.Estudia",xpc = "Preguntaf4Estudia.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P3",xpc = "PreguntaP3.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P6",xpc = "PreguntaP6.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P7",xpc = "PreguntaP7.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P8",xpc = "PreguntaP8.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P9",xpc = "PreguntaP9.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P15",xpc = "PreguntaP15.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P17",xpc = "PreguntaP17.csv" )
exportarAbiertas(xpa = datos,xpb = "P17A",xpc = "PreguntaP17A.csv" )
exportarAbiertas(xpa = datos,xpb = "P17B",xpc = "PreguntaP17B.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P19",xpc = "PreguntaP19.csv" )
exportarAbiertas(xpa = datos,xpb = "P19A",xpc = "PreguntaP19A.csv" )
exportarAbiertas(xpa = datos,xpb = "P19B",xpc = "PreguntaP19B.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P21",xpc = "PreguntaP21.csv" )
exportarAbiertas(xpa = datos,xpb = "P21A",xpc = "PreguntaP21A.csv" )
exportarAbiertas(xpa = datos,xpb = "P21B",xpc = "PreguntaP21B.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P25",xpc = "PreguntaP25.csv" )
exportarAbiertas(xpa = datos,xpb = "Otro.P32",xpc = "PreguntaP32.csv" )




