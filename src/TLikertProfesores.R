#En primer lugar abrimos la ruta donde estan los datos
setwd("F:/MAES/TFM/TFM2/Respuestas")

#Seguidamente datoscargamos los datos
dProfesores <-read.table( "PROFESORES.csv", sep = ",", header = TRUE )

#Limpiamos los datos, primero quitamos la marca temporal
install.packages("devtools")
library(dplyr) 
dProfesores <- select(dProfesores, -Marca.temporal)

#Anadimos el curso al que pertenecen los alumnos
Curso<- c("BACH", "BACH", "ESO", "ESO", "FP", "BACH", "BACH", "FP", "BACH", "BACH", "BACH", "FP", "ESO", "ESO", "ESO", "ESO", "BACH", "ESO", "FP", "FP", "BACH")
Curso<- c("BACH", "BACH", "ESO", "ESO", "FP", "BACH", "BACH", "FP", "BACH", "BACH", "BACH", "FP", "ESO", "ESO", "ESO", "ESO", "BACH", "ESO", "FP", "FP", "BACH", "N", "N", "N", "N", "N", "N", "N")
Curso<- c("BACH", "BACH", "ESO", "ESO", "FP", "BACH", "BACH", "FP", "BACH", "BACH", "ESO", "FP", "ESO", "ESO", "ESO", "BACH", "BACH", "ESO", "BACH", "FP", "FP", "BACH", "ESO", "FP", "FP", "BACH", "BACH", "BACH", "N", "N", "N", "N", "N", "N", "N")
dAlumnos<-cbind(dAlumnos,Curso)

#Cambiamos los largos nombres de las columnas por otros mas identificativos
names (dProfesores) = c("ID", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32")

#Ponemos el ID como caracter
dProfesores$ID <- as.character(dProfesores$ID)

#Ponemos correctamente las horas que dedican a estudiar y numerico
EstudioH<- c("6", "11", "4", "4", "2", "10", "40", "25", "11", "40", "2", "0", "5", "24", "15", "25", "25", "15", "29", "2", "25", "23", "12", "25", "25", "30", "32", "28", "10", "10", "10", "10", "10", "10", "10")
dAlumnos$P28<-EstudioH
dAlumnos$P28 <- as.numeric(dAlumnos$P28)

#Ponemos correctamente las horas que dedican al deporte y numerico
DeporteH<- c("4", "2", "8", "8", "2", "0", "2", "1", "2", "2", "10", "2", "6", "9", "9", "4", "4", "7", "3", "1", "8", "2", "8", "1", "3", "4", "3", "2", "8", "8", "8", "8", "8", "8", "8")
dAlumnos$P32<-DeporteH
dAlumnos$P32 <- as.numeric(dAlumnos$P32)

#Una vez que tenemos todos los datos limpios podemos proceder al analisis descriptivo



#Se realizan gráficos descriptivos de los datos con el paquete likert
diccProfes<- read.csv("diccionarioProfes.csv", sep=";", header = TRUE, stringsAsFactors = FALSE)
dfLikertProfesores <- dProfesores
dfLikertProfesores <- dProfesores[ , grep( "^P", colnames(dProfesores) ) ]
#Para ello se eliminan las lineas que no son escala likert
dfLikertProfesores <- select(dfLikertProfesores, -P16, -P22, -P23, -P24, -P31, -P32)
nivelesQ     <- c( "Completamente en desacuerdo", "Muy en desacuerdo", "En desacuerdo", "Neutro", "De acuerdo", "Muy de acuerdo", "Completamente en acuerdo" )
dfLikertProfesores[ , 2:27 ] <- lapply( dfLikertProfesores[ , 2:27], factor, labels = nivelesQ )
colnames( dfLikertProfesores ) <- diccProfes[ grep( "^P", diccProfes$item), "spanish2" ]
dfLikertProfesores <- dfLikertProfesores[dfLikertProfesores$ID != "1", ] # Atención a la coma y el espacio al final
dfLikertProfesores <- select(dfLikertProfesores, -ID)

bloque1 <- 1:9
library( likert )
itemsProfes1 <- likert( items = dfLikertProfesores[ , bloque1 ] )
plot( itemsProfes1, centered = TRUE, group.order = colnames( itemsProfes1$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )
bloque2 <- 10:18
itemsProfes2 <- likert( items = dfLikertProfesores[ , bloque2 ] )
plot( itemsProfes2, centered = TRUE, group.order = colnames( itemsProfes2$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )
bloque3 <- 19:26
itemsProfes3 <- likert( items = dfLikertProfesores[ , bloque3 ] )
plot( itemsProfes3, centered = TRUE, group.order = colnames( itemsProfes3$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

plot( itemsProfes1, type = "density" )
plot( itemsProfes2, type = "density" )
plot( itemsProfes3, type = "density" )

plot( itemsProfes1, type = "heat", group.order = colnames( itemsAlumnos1$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

plot( itemsProfes2, type = "heat", group.order = colnames( itemsAlumnos2$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

plot( itemsProfes3, type = "heat", group.order = colnames( itemsAlumnos3$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

#Se realizan gráficos de correlaciones
dProfesores2 <- dProfesores
dProfesores2 <- dProfesores2[dProfesores2$ID != "1", ] # Atención a la coma y el espacio al final
dfCor <- dProfesores2[ , grep( "^P", colnames( dProfesores2 ) ) ]
dfCor <- lapply( dfCor, as.numeric ) # devuelve una lista 
dfCor <- as.data.frame( dfCor )      # coarcionamos a data.frame
corr <- cor( dfCor )
install.packages("corrr")
library("corrr")
corr2 <- correlate( dfCor )
network_plot( corr2, min_cor = 0.4 )

install.packages("corrplot")
library(corrplot)
corrplot.mixed( corr, tl.pos = "lt", diag = 'n', upper = "ellipse",
                number.cex = 0.4, tl.cex = 0.8,
                order = "hclust" )

#Se realizan tablas comparativas para ver el estres por ejemplo
library( tables )
opt <- booktabs()
tablaLatex <- function( tabla, caption = NULL ){
  cat( '\\begin{table} \\centering\n' )
  if( !is.null( caption ) ) cat( paste0( '\\caption{', caption, '}\n' ) )
  latex( tt )
  cat( '\\end{table}' )
}

dProfesoresB <- dProfesores
dProfesoresB <- dProfesoresB[dProfesoresB$ID != "1", ] # Atención a la coma y el espacio al final
tt <- tabular( ( Total = 1 ) ~ ( `Estres` = P10 ) +
                 ( Total = 1 ), data = dProfesoresB )
html( tt, options = htmloptions( HTMLcaption = "Variable estres según curso", pad = TRUE ) )


ic1 <- function(x){
  mean( x ) - qt( 0.975, df = length( x ) - 1 ) * sd( x ) / sqrt( length( x ) ) 
}
ic2 <- function(x){
  mean( x ) + qt( 0.975, df = length( x ) - 1 ) * sd( x ) / sqrt( length( x ) ) 
}

tt <- tabular( ( Total = 1 ) ~ ( Estres = P6 ) *
                 ( ic1 + mean + sd + ic2 + median ) + ( Total = 1 ), data = dProfesoresB )

html( tt, options = htmloptions( HTMLcaption = "Variable estres según curso.", pad = TRUE ) )

data_frame2 <- dAlumnosB
nivelesQ     <- c( "Completamente en desacuerdo", "Muy en desacuerdo", "En desacuerdo", "Neutro", "De acuerdo", "Muy de acuerdo", "Completamente en acuerdo" )
data_frame2[ , 2:16 ] <- lapply( data_frame2[ , 2:16 ], factor, labels = nivelesQ )
data_frame2[ , 19:28 ] <- lapply( data_frame2[ , 19:28 ], factor, labels = nivelesQ )
data_frame2[ , 30:32 ] <- lapply( data_frame2[ , 30:32 ], factor, labels = nivelesQ )
data_frame3 <- data_frame2[data_frame2$Curso != "N", ] # Atención a la coma y el espacio al final
data_frame3$Curso <- as.character(data_frame3$Curso)
data_frame3$Curso <- as.factor(data_frame3$Curso)

data_frame4 <- data_frame3
data_frame2 <- select(data_frame2, -P16, -P17, -P28, -P32, -P33, -P34, -P35, -P36)
dfLikertAlumnosB <- data_frame3[ , grep( "^P", colnames(data_frame3) ) ]
colnames( dfLikertAlumnosB ) <- diccAlumnos[ grep( "^P", diccAlumnos$item), "spanish2" ]


bloque1 <- 1:7
bloque2 <- 8:14
bloque3 <- 15:21
bloque4 <- 22:28



itemsO1 <- likert( items = dfLikertAlumnosB[ , bloque1 ], grouping = data_frame3$Curso )
itemsO2 <- likert( items = dfLikertAlumnosB[ , bloque2 ], grouping = data_frame3$Curso )
itemsO3 <- likert( items = dfLikertAlumnosB[ , bloque3 ], grouping = data_frame3$Curso )
itemsO4 <- likert( items = dfLikertAlumnosB[ , bloque4 ], grouping = data_frame3$Curso )

plot( itemsO1 )
plot( itemsO2 )
plot( itemsO3 )
plot( itemsO4 )

plot( itemsO1, type = "density" )
plot( itemsO2, type = "density" )
plot( itemsO3, type = "density" )
plot( itemsO4, type = "density" )

#Shapiro Wilks para comprobar si los datos son normales teniendo en cuenta los diferentes Cursos
#Shapiro wilks se usa porque el tamano de la muestra es menor de 50
#Se usa para variables nuemricas con 3 grupos o mas
shapiro.test (dAlumnosB$P13 [dAlumnosB$Curso == "ESO"])
shapiro.test (dAlumnosB$P13 [dAlumnosB$Curso == "FP"])
shapiro.test (dAlumnosB$P13 [dAlumnosB$Curso == "BACH"])

#ANOVA se usa para comparar si hay diferencias significativas en la media de 3 grupos o más en variables numericas
#Solo se puede usar si se cumplen dos condiciones
#1 que los datos sean normales (sigan una distribucion normal dentro de cada grupo). Para ello Shapiro>0.05
#2 Que haya igualdad de varianzas dentro de cada grupo(homocedasticidad). Para ello test de levene >0.05
#sI algo se incumple hay que usar test no parametricos como kruskal walis

#KRUSKAL WALIS
kruskal.test(P24 ~ Curso, data = dAlumnosB)
kruskal.test(P26 ~ Curso, data = dAlumnosB)
kruskal.test(P5 ~ Curso, data = dAlumnosB)
kruskal.test(P13 ~ Curso, data = dAlumnosB)

pairwise.wilcox.test(x = dAlumnosB$P24, g =dAlumnosB$Curso, p.adjust.method = "holm" )
pairwise.wilcox.test(x = dAlumnosB$P26, g =dAlumnosB$Curso, p.adjust.method = "holm" )
pairwise.wilcox.test(x = dAlumnosB$P5, g =dAlumnosB$Curso, p.adjust.method = "holm" )
pairwise.wilcox.test(x = dAlumnosB$P13, g =dAlumnosB$Curso, p.adjust.method = "holm" )
