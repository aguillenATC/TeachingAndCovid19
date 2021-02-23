#En primer lugar abrimos la ruta donde estan los datos
setwd("F:/MAES/TFM/TFM2/Respuestas")

#Seguidamente cargamos los datos de los padres
dPadres <-read.table( "PADRES.csv", sep = ",", header = TRUE )

#Limpiamos los datos, primero quitamos la marca temporal
install.packages("devtools")
library(dplyr) 
dPadres <- select(dPadres, -Marca.temporal)

#Anadimos el curso al que pertenecen los hijos de los padres
Curso<- c("BACH", "BACH", "BACH", "BACH", "BACH", "BACH", "FP", "BACH", "ESO", "ESO", "ESO", "ESO", "FP", "ESO", "ESO", "BACH", "ESO", "BACH", "BACH", "ESO", "BACH", "BACH", "BACH", "ESO", "N", "N", "N", "N", "N", "N", "N")
dPadres<-cbind(dPadres,Curso)

#Cambiamos los largos nombres de las columnas por otros mas identificativos
names (dPadres) = c("ID","P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "Curso")

#Ponemos el ID como caracter
dPadres$ID <- as.character(dPadres$ID)

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
diccPadres<- read.csv("diccionarioP.csv", sep=";", header = TRUE, stringsAsFactors = FALSE)
dfLikertPadres <- dPadres[ , grep( "^P", colnames(dPadres) ) ]
#Para ello se eliminan las lineas que no son escala likert
dfLikertPadres <- select(dfLikertPadres, -P11, -P15, -P16, -P17, -P18, -P19)
nivelesQ     <- c( "Completamente en desacuerdo", "Muy en desacuerdo", "En desacuerdo", "Neutro", "De acuerdo", "Muy de acuerdo", "Completamente en acuerdo" )
dfLikertPadres[ , 1:13 ] <- lapply( dfLikertPadres[ , 1:13 ], factor, labels = nivelesQ )
colnames( dfLikertPadres ) <- diccPadres[ grep( "^P", diccPadres$item), "spanish2" ]
bloque1 <- 1:9
library( likert )
itemsAlumnos1 <- likert( items = dfLikertAlumnos[ , bloque1 ] )
plot( itemsAlumnos1, centered = TRUE, group.order = colnames( itemsAlumnos1$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )
bloque2 <- 10:19
itemsAlumnos2 <- likert( items = dfLikertAlumnos[ , bloque2 ] )
plot( itemsAlumnos2, centered = TRUE, group.order = colnames( itemsAlumnos2$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )
bloque3 <- 19:28
itemsAlumnos3 <- likert( items = dfLikertAlumnos[ , bloque3 ] )
plot( itemsAlumnos3, centered = TRUE, group.order = colnames( itemsAlumnos3$items ),
      legend.position = "right" ) +
  theme( axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

plot( itemsAlumnos1, type = "density" )
plot( itemsAlumnos2, type = "density" )
plot( itemsAlumnos3, type = "density" )

plot( itemsAlumnos1, type = "heat", group.order = colnames( itemsAlumnos1$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

plot( itemsAlumnos2, type = "heat", group.order = colnames( itemsAlumnos2$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

plot( itemsAlumnos3, type = "heat", group.order = colnames( itemsAlumnos3$items ) ) +
  theme( axis.text.x = element_text( size = 8 ),
         axis.text.y = element_text( size = 13, hjust = 0 ),
         legend.text = element_text( size = 10 ),
         legend.title = element_text( size = 10 ) )

#Se realizan gráficos de correlaciones
dPadresCor <- dPadres[dPadres$Curso != "N", ] # Atención a la coma y el espacio al final
dfCor <- dPadresCor[ , grep( "^P", colnames( dPadresCor ) ) ]
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

dPadresB <- dPadres
dPadresB$P6<- factor( dPadresB$P6, labels = c( "Completamente en desacuerdo", "Muy en desacuerdo", "En desacuerdo", "Neutro", "De acuerdo", "Muy de acuerdo", "Completamente en acuerdo" ) )
dPadresB$Curso <- as.character(dPadresB$Curso)
dPadresB$Curso <- as.factor(dPadresB$Curso)
dPadresB <- dPadresB[dPadresB$Curso != "N", ] # Atención a la coma y el espacio al final
dPadresB <- dPadresB[dPadresB$Curso != "FP", ] # Atención a la coma y el espacio al final
tt <- tabular( ( Curso = Curso ) + ( Total = 1 ) ~ ( `Estres` = P6) +
                 ( Total = 1 ), data = dPadresB )
html( tt, options = htmloptions( HTMLcaption = "Variable estres según curso", pad = TRUE ) )

dAlumnosB$P24 <- dAlumnos$P24
ic1 <- function(x){
  mean( x ) - qt( 0.975, df = length( x ) - 1 ) * sd( x ) / sqrt( length( x ) ) 
}
ic2 <- function(x){
  mean( x ) + qt( 0.975, df = length( x ) - 1 ) * sd( x ) / sqrt( length( x ) ) 
}

tt <- tabular( ( Curso = Curso )  ~  ( Estres = P6 ) *
                 ( ic1 + mean + sd + ic2 + median ) + ( Total = 1 ), data = dPadresB )

html( tt, options = htmloptions( HTMLcaption = "Variable estres según curso.", pad = TRUE ) )

data_frame2 <- dPadres
nivelesQ     <- c( "Completamente en desacuerdo", "Muy en desacuerdo", "En desacuerdo", "Neutro", "De acuerdo", "Muy de acuerdo", "Completamente en acuerdo" )
data_frame2[ , 2:11 ] <- lapply( data_frame2[ , 2:11 ], factor, labels = nivelesQ )
data_frame2[ , 13:15 ] <- lapply( data_frame2[ , 13:15 ], factor, labels = nivelesQ )
data_frame3 <- data_frame2[data_frame2$Curso != "N", ] # Atención a la coma y el espacio al final
data_frame3 <- data_frame3[data_frame3$Curso != "FP", ] # Atención a la coma y el espacio al final
data_frame3$Curso <- as.character(data_frame3$Curso)
data_frame3$Curso <- as.factor(data_frame3$Curso)

dfLikertPadresB <- data_frame3
dfLikertPadresB <- select(dfLikertPadresB, -ID, -P11, -P15, -P16, -P17, -P18, -P19, -Curso)
colnames( dfLikertPadresB ) <- diccPadres[ grep( "^P", diccPadres$item), "spanish2" ]


bloque1 <- 1:7
bloque2 <- 8:13

itemsO1 <- likert( items = dfLikertPadresB[ , bloque1 ], grouping = data_frame3$Curso )
itemsO2 <- likert( items = dfLikertPadresB[ , bloque2 ], grouping = data_frame3$Curso )

plot( itemsO1 )
plot( itemsO2 )

plot( itemsO1, type = "density" )
plot( itemsO2, type = "density" )
plot( itemsO3, type = "density" )
plot( itemsO4, type = "density" )

#Shapiro Wilks para comprobar si los datos son normales teniendo en cuenta los diferentes Cursos
#Shapiro wilks se usa porque el tamano de la muestra es menor de 50
#Se usa para variables nuemricas con 3 grupos o mas
shapiro.test (dPadresB$P5 [dPadresB$Curso == "ESO"])
shapiro.test (dPadresB$P6 [dPadresB$Curso == "BACH"])

#ANOVA se usa para comparar si hay diferencias significativas en la media de 3 grupos o más en variables numericas
#Solo se puede usar si se cumplen dos condiciones
#1 que los datos sean normales (sigan una distribucion normal dentro de cada grupo). Para ello Shapiro>0.05
#2 Que haya igualdad de varianzas dentro de cada grupo(homocedasticidad). Para ello test de levene >0.05
#sI algo se incumple hay que usar test no parametricos como kruskal walis

#KRUSKAL WALIS
kruskal.test(P7 ~ Curso, data = dPadresB)
kruskal.test(P9 ~ Curso, data = dPadresB)
kruskal.test(P5 ~ Curso, data = dPadresB)
kruskal.test(P6 ~ Curso, data = dPadresB)

pairwise.wilcox.test(x = dPadresB$P24, g =dPadresB$Curso, p.adjust.method = "holm" )
pairwise.wilcox.test(x = dPadresB$P26, g =dPadresB$Curso, p.adjust.method = "holm" )
pairwise.wilcox.test(x = dPadresB$P5, g =dPadresB$Curso, p.adjust.method = "holm" )
pairwise.wilcox.test(x = dPadresB$P13, g =dPadresB$Curso, p.adjust.method = "holm" )
