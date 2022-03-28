
#_____________________ MEDIDAS ___________________

# Se trabajar� con la matriz de datos "BD.xlsx"
# Import dataset/From Excel/Browse/ seleccionar archivo/aceptar/ (Visualizar)/Import 
#----------------------------------------------- 
#      Tendencia central 
#----------------------------------------------- 
# Acortar el nombre de la base de datos 
BD<-penguins
# Explorar la matriz 
dim(BD)
str(BD)
colnames(BD)
anyNA(BD)
# 1.- Media y mediana
summary(BD)


# 2.- Moda

# 2.1.- Se descarga el paquete "modeest"
install.packages("modeest")

# 2.2.- Se abre la librer�a
library(modeest)

# 2.3.- C�lculo de la moda para la variable isla y largo del aleta
mfv(BD$isla) # categorica
mfv(BD$largo_aleta_mm) # numerica

#-----------------------------------------------
#      Medidas de dispersi�n
#-----------------------------------------------

# 1.- C�lculo de la varianza (s�lo para variables cuantitativas)
var(BD$largo_aleta_mm)

# 2.- C�lculo de la desviaci�n est�ndar
sd(BD$largo_aleta_mm)

# 3.- Error
media_aleta<-mean(BD$largo_aleta_mm)
media_aleta

error<-(BD$largo_aleta_mm-(media_aleta))
error


#4.- Coeficiente de variacion
CV<- sd(BD$largo_aleta_mm)/mean(BD$largo_aleta_mm)*100
CV

# 5.- Rango intercuartilico (IQR)
IQR(BD$largo_aleta_mm)

# 6.- Rango
aleta<-BD$largo_aleta_mm
aleta

rango<-max(aleta)-min(aleta)
rango

#-----------------------------------------------
#    Medidas de posici�n
#------------------------------------------------

# 1.- Cuartiles
summary(BD)

# 2.- Quintil
quintil<-quantile(BD[["largo_aleta_mm"]], 
                  p=c(.20, .40, .60, .80))
quintil

# 3.- Decil
decil<-quantile(BD[["largo_aleta_mm"]], 
                p=c(.10, .20, .30, .40, .50, .60,
                    .70, .80, .90))
decil

# Percentil
percentil<-quantile(BD[["largo_aleta_mm"]], 
                    p=c(.33, .66, .99))
percentil

# Interpretacion:
# <192 = Bajo
# 192-209 = Intermedio
# > 209 = Alto

#---------------------------------------
#Ejercicio :v
#---------------------------------------

# 1.- Media y mediana
summary(BD)

# 2.3.- C�lculo de la moda para la variable isla y largo del aleta
mfv(BD$especie) # Salió: "Adelie"
mfv(BD$largo_aleta_mm) # Salió: 190

#-----------------------------------------------
#      Medidas de dispersi�n
#-----------------------------------------------

# 1.- C�lculo de la varianza (s�lo para variables cuantitativas)
var(BD$largo_aleta_mm) #Salió: 198.2214

# 2.- C�lculo de la desviaci�n est�ndar
sd(BD$largo_aleta_mm) #Salió:14.07911

# 3.- Error
media_aleta<-mean(BD$largo_aleta_mm)
media_aleta #Salió: 200.9448

error<-(BD$largo_aleta_mm-(media_aleta))
error #Salió: -19.94476744 -14.94476744  -5.94476744 -10.94476744  -7.94476744 -10.94476744 -19.94476744  -5.94476744  -7.94476744 -10.94476744.....


#4.- Coeficiente de variacion
CV<- sd(BD$largo_aleta_mm)/mean(BD$largo_aleta_mm)*100
CV #Salió: 7.006459

# 5.- Rango intercuartilico (IQR)
IQR(BD$largo_aleta_mm) #Salió: 23.25

# 6.- Rango
aleta<-BD$largo_aleta_mm
aleta #Salió: 181 186 195 190 193 190 181 195 193 190....

rango<-max(aleta)-min(aleta)
rango #Salió: 59

#-----------------------------------------------
#    Medidas de posici�n
#------------------------------------------------

# 1.- Cuartiles
summary(BD)
#Salió: ID              especie              isla           largo_pico_mm  
#Length:344         Length:344         Length:344         Min.   :32.10  
#Class :character   Class :character   Class :character   1st Qu.:39.20  
#Mode  :character   Mode  :character   Mode  :character   Median :44.45  
#Mean   :43.92  
#3rd Qu.:48.50  
#Max.   :59.60  
#grosor_pico_mm  largo_aleta_mm  masa_corporal_g    genero               año      
#Min.   :13.10   Min.   :172.0   Min.   :2700    Length:344         Min.   :2007  
#1st Qu.:15.60   1st Qu.:190.0   1st Qu.:3550    Class :character   1st Qu.:2007  
#Median :17.30   Median :197.0   Median :4050    Mode  :character   Median :2008  
#Mean   :17.15   Mean   :200.9   Mean   :4202                       Mean   :2008  
#3rd Qu.:18.70   3rd Qu.:213.2   3rd Qu.:4756                       3rd Qu.:2009  
#Max.   :21.50   Max.   :231.0   Max.   :6300                       Max.   :2009

# 2.- Quintil
quintil<-quantile(BD[["grosor_pico_mm"]], 
                  p=c(.20, .40, .60, .80))
quintil
#Salió: 
#20%  40%  60%  80% 
#15.0 16.8 17.9 18.9

# 3.- Decil
decil<-quantile(BD[["grosor_pico_mm"]], 
                p=c(.10, .20, .30, .40, .50, .60,
                    .70, .80, .90))
decil
#Salió:
#10%  20%  30%  40%  50%  60%  70%  80%  90% 
#14.3 15.0 15.9 16.8 17.3 17.9 18.5 18.9 19.5


# Percentil
percentil<-quantile(BD[["grosor_pico_mm"]], 
                    p=c(.33, .66, .99))
percentil
#Salió:
#33%    66%    99% 
#16.119 18.200 21.100 

#Interpretacion:
# < 16.119 = Bajo
# ~ 18.200 = Intermedio
# > 21.100 = Alto
