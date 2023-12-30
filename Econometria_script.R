

### Primer modelo:

library(readxl)
library(dplyr)


data <- read.csv("Video Games Sales.csv",sep = ",")
#data<-read.csv("C:\\Users\\DEBBIE\\Desktop\\Econometr?a\\Proyecto-Econometr-a\\Video Games Sales.csv")

#View(data)
# nrow(data) #numero de datos 

#filtremos los datos NA y eligamos 4 tipos de consolas y generos
tipos_consola <- c("PS3", "PS2", "X360","Wii")

tipos_genero <- c("Shooter", "Sports", "Action","Misc")

#construyamos la data
library(dplyr)

data_new <- data %>% filter(Platform %in% tipos_consola & Genre %in% tipos_genero) %>% mutate(Genre=case_when(Genre=="Shooter"~0,
                                                                                                              Genre=="Sports"~1,
                                                                                                              Genre=="Action"~2,
                                                                                                              Genre=="Misc"~3)) %>% mutate(Platform=case_when(Platform=="PS3"~0,
                                                                                                                                                              Platform=="PS2"~1,
                                                                                                                                                              Platform=="X360"~2,
                                                                                                                                                              Platform=="Wii"~3)) %>% select(Global,North.America,Japan,Review,Rank)
#View(data_new)


##Modelo:
mod<-lm(Global~North.America+Japan+Review+Rank,data_new)
summary(mod)


## SST:
SST<-sum((data_new$Global-mean(data_new$Global))^2)
SST
## SSE:

SSE<-sum(mod$residuals^2)
SSE
##SSR:
SSR<-sum((mod$fitted.values-mean(data_new$Global))^2)
SSR
#### R^2

R2<-1-SSE/SST
R2

#### R^2 ajustado:
R_ajustado<-1-(((601-1)/(601-5))*(1-R2))
R_ajustado

# Matriz X:

unos<-rep(1,601)
NorA<-data_new$North.America
Japon<-data_new$Japan
Review<-data_new$Review
Rank<-data_new$Rank
y<-data_new$Global

X<-cbind(unos,NorA,Japon,Review,Rank)

# Matriz Beta estimados:

Beta_e<-solve(t(X)%*%X)%*%(t(X)%*%y)
Beta_e

# Residuos:
u_e<-y-(X%*%Beta_e)
u_e
## Matriz de varianza de los Beta:
sigma2<-t(u_e)%*%u_e/(601-5)
sigma2<-as.vector(sigma2)
Var_beta<-solve(t(X)%*%X)*sigma2
Var_beta



## Pruebas de hip?tesis: Beta significativos:
# Nivel de confianza del 95%:

tc<-qt(0.95,601-5,lower.tail = TRUE)
tc

# B2 (North.America):
t_g<-mod$coefficients[2]/1.947e-02
t_g
# B3 (Japan)
t_p<-mod$coefficients[3]/1.102e-01
t_p
# B4 (Review):
t_r<-mod$coefficients[4]/3.451e-03 
t_r
# B5 (Rank):
t_rk<-mod$coefficients[5]/7.103e-05
t_rk

## Pruebas de hip?tesis sobre combinaciones lineales de par?metros:


## Ho:B2-B3=0 (NorthAmerica-Japan)
tgp<-(mod$coefficients[2]-mod$coefficients[3])/(sqrt(Var_beta[2,2]+Var_beta[3,3]-2*Var_beta[2,3]))
tgp


## Ho: B4-B5=0 (Review-Rank)
trr<-(mod$coefficients[4]-mod$coefficients[5])/(sqrt(Var_beta[4,4]+Var_beta[5,5]-2*Var_beta[4,5]))
trr

## Ho:B2-B4=0 (NorthAmerica-Review)
tgr<-(mod$coefficients[2]-mod$coefficients[4])/(sqrt(Var_beta[2,2]+Var_beta[4,4]-2*Var_beta[2,4]))
tgr

## Ho:B3-B5=0 (Japan-Rank)
tpr<-(mod$coefficients[3]-mod$coefficients[5])/(sqrt(Var_beta[3,3]+Var_beta[5,5]-2*Var_beta[3,5]))
tpr



#### Pruebas de hip?tesis conjuntamente significativas:

### Ho: B2=0, B3=0

## Estad?stico F:
# Con SSE:
Fc<-qf(0.95,2,601-5)
Fc

# Modelo restringido:
mod1<-lm(Global~Review+Rank,data_new)
SSEr<-sum(mod1$residuals^2)
Fe_1<-((SSEr-SSE)/2)/(SSE/(601-5))
Fe_1
# Con R^2
Fe_2<-((summary(mod)$r.squared-summary(mod1)$r.squared)/(1-summary(mod)$r.squared))*(601-5)/2
Fe_2

## ML:

modr1<-lm(mod1$residuals~North.America+Japan+Review+Rank,data_new)
ML1<-601*summary(modr1)$r.squared
ML1
MLc<-qchisq(0.95,2)
MLc

### Ho: B2=0, B4=0

## Estad?stico F:
Fc1<-qf(0.95,2,601-5)
Fc1
# Con SSE:

# Modelo restringido:
mod2<-lm(Global~Japan+Rank,data_new)
SSEr2<-sum(mod2$residuals^2)
# Estad?stico F con SSE:
Fe_12<-((SSEr2-SSE)/2)/(SSE/(601-5))
Fe_12
# Con R^2
Fe_22<-((summary(mod)$r.squared-summary(mod2)$r.squared)/(1-summary(mod)$r.squared))*(601-5)/2
Fe_22

# ML:
modr2<-lm(mod2$residuals~North.America+Japan+Review+Rank,data_new)
ML2<-601*summary(modr2)$r.squared
ML2

## Ho: B2=0, B3=0,B5=0

Fc2<-qf(0.95,3,601-5)
Fc2
# Con SSE:

# Modelo restringido:
mod3<-lm(Global~Review,data_new)
SSEr3<-sum(mod3$residuals^2)
Fe_123<-((SSEr3-SSE)/3)/(SSE/(601-5))
Fe_123
# Con R^2
Fe_223<-((summary(mod)$r.squared-summary(mod3)$r.squared)/(1-summary(mod)$r.squared))*(601-5)/3
Fe_223

# ML:

modr3<-lm(mod3$residuals~North.America+Japan+Review+Rank,data_new)
ML3<-601*summary(modr3)$r.squared
ML3

# ML calculado:
MLc2<-qchisq(0.95,3)
MLc2

### Intervalos de Confianza:

## Para los estimadores Beta:
# B1:
If1<-mod$coefficients[1]-tc*sqrt(Var_beta[1,1])
If1
# Intervalo superior:
Is1<-mod$coefficients[1]+tc*sqrt(Var_beta[1,1])
Is1

# B2(NorthAmerica):
# Intervalo inferior:
If2<-mod$coefficients[2]-tc*sqrt(Var_beta[2,2])
If2
# Intervalo superior:
Is2<-mod$coefficients[2]+tc*sqrt(Var_beta[2,2])
Is2

# B3(Japan):
# Intervalo inferior:
If3<-mod$coefficients[3]-tc*sqrt(Var_beta[3,3])
If3
# Intervalo superior:
Is3<-mod$coefficients[3]+tc*sqrt(Var_beta[3,3])
Is3


# B4 (Review):
# Intervalo inferior:
If4<-mod$coefficients[4]-tc*sqrt(Var_beta[4,4])
If4
# Intervalo superior:
Is4<-mod$coefficients[4]+tc*sqrt(Var_beta[4,4])
Is4

# B5 (Rank):
# Intervalo inferior:
If5<-mod$coefficients[5]-tc*sqrt(Var_beta[5,5])
If5
# Intervalo superior:
Is5<-mod$coefficients[5]+tc*sqrt(Var_beta[5,5])
Is5

# Para valores espec?ficos:
vc<-c(1,15,22,91,54)
vcm<-matrix(vc,ncol=1)
theta_e<-t(vcm)%*%Beta_e
theta_e
# Varianza:
Var_theta<-t(vc)%*%Var_beta%*%vc
Var_theta
#Intervalo inferior:

ift<-theta_e-tc*sqrt(Var_theta)
ift
# Intervalo superior:
ist<-theta_e+tc*sqrt(Var_theta)
ist

# Intervalo para una observaci?n en particular:
xo<-c(1,31,10,27,25)
xom<-matrix(xo,ncol=1)
yo_e<-t(xom)%*%Beta_e
## Varianza eo_e
V_eo<-sigma2*(1+t(xom)%*%solve(t(X)%*%X)%*%xom)
V_eo
# Intervalo inferior para yo:
ifyo<-yo_e-tc*sqrt(V_eo)
ifyo
# Intervalo superior para yo:
isyo<-yo_e+tc*sqrt(V_eo)
isyo





#_________________________________________________________________
#_____________________________________________________________________
#___________________________________________________________________

# SEGUNDO MODELO

#leamos la base de datos

library(readxl)
library(dplyr)

data <- read.csv("Video Games Sales.csv",sep = ",")

#View(data)
# nrow(data) #numero de datos 

# eligamos 4 tipos de consolas y generos
tipos_consola <- c("PS3", "PS2", "X360","Wii")

tipos_genero <- c("Shooter", "Sports", "Action","Misc")


######___________________

# construyamos el modelo con las categorias 

data_new_1 <- data |>
  filter(Platform %in% tipos_consola & Genre %in% tipos_genero)

# modelo

modelo <- lm(Global ~ Rank+Platform+Genre+Review, data = data_new_1)


# R pone la categoria base, genero=Action, plataforma= PS2
summary(modelo)

# del summary obtenemos que ya que el valor Pr(>|t|) es menor a 0.05
# concluimos que las siguientes variables son significativas 

# intersepto 
# Rank
# PlatformWii
# 

#_____________________________________

# intervalos de confianza para los betas

intervalos_confianza <- confint(modelo, level = 0.95)
print(intervalos_confianza)





#_______________________________________________-
# prueba ML (prueba conjunta) 

# DE DOS VARIABLES 


# Consideramos el modelo inicial abriendo las categorias del genero del juego:
# categoria base: Action y PS2
# global = b_0+b_1*GenreMISC+b_2*GenreSHOOTER+b_3*GenreSPORTS+b_4*Review+b_5*Rank+
# b_6*PS3+b_7*X360+b_8*Wii

# construyamos el modelo, creando los grupos de la variable categorica Género del juego
#además creemos grupos para la variable categórica Plataforma
data_categ_1 <- data_new_1 |>
  mutate(Misc = ifelse(Genre == "Misc",1,0),
         Shooter = ifelse(Genre == "Shooter",1,0),
         Sports = ifelse(Genre == "Sports",1,0)) |>
  mutate(PS3 = ifelse(Platform == "PS3",1,0),
         X360 = ifelse(Platform == "X360",1,0),
         Wii = ifelse(Platform == "Wii",1,0))

#View(data_categ_1)

#______________________________________--

#  Probemos: si es conjuntamente significativo b_1 y b_2


# modelo restringido 
modelo_res <- lm(Global ~ Rank+Review+Sports+PS3+X360+Wii, data = data_categ_1)

summary(modelo_res)
#obtengamos los residuos
residuos_res <- modelo_res$residuals

# hagamos el modelo auxiliar

modelo_aux <-lm(residuos_res ~ Rank+Review+Sports+Misc+Shooter+PS3+X360+Wii, data = data_categ_1)

# Obtengamos el valor ML del modelo auxiliar

ML <- summary(modelo_aux)$r.square*601
ML

# chi

chi_2 <- qchisq(0.95,2)

# valor p, es la proba desde el ML para abajo

v_p <- pchisq(ML, 2, lower.tail = FALSE) #false por la colita de la derecha

v_p

# vemos que v_p> 0.05 por lo que se acepta la prueba
# es decir la categoria MISC Y SHOOTER no sson significativos conjuntamente 

#______________________________

#  Probemos: si es conjuntamente significativo b_3 y b_4


# modelo restringido 
modelo_res_1 <- lm(Global ~ Rank+Misc+Shooter+PS3+X360+Wii, data = data_categ_1)

summary(modelo_res_1)
#obtengamos los residuos
residuos_res_1 <- modelo_res_1$residuals

# hagamos el modelo auxiliar

modelo_aux_1 <-lm(residuos_res_1 ~ Rank+Review+Sports+Misc+Shooter+PS3+X360+Wii, data = data_categ_1)

# Obtengamos el valor ML del modelo auxiliar

ML <- summary(modelo_aux_1)$r.square*601
ML

# chi

chi_2 <- qchisq(0.95,2)

# valor p, es la proba desde el ML para abajo

v_p <- pchisq(ML, 2, lower.tail = FALSE) #false por la colita de la derecha

v_p

# vemos que v_p> 0.05 por lo que se acepta la prueba
# es decir la categoria Sports y la variable Review no son significativos conjuntamente 

#______________________________

# PRUEBA DE 3 VARIABLES
#  Probemos: si es conjuntamente significativo b_1,b_2 y b_3


# modelo restringido 
modelo_res_2 <- lm(Global ~ Rank+Review+PS3+X360+Wii, data = data_categ_1)

summary(modelo_res_2)
#obtengamos los residuos
residuos_res_2 <- modelo_res_2$residuals

# hagamos el modelo auxiliar

modelo_aux_2 <-lm(residuos_res_2 ~ Rank+Review+Sports+Misc+Shooter+PS3+X360+Wii, data = data_categ_1)

# Obtengamos el valor ML del modelo auxiliar

ML <- summary(modelo_aux_2)$r.square*601
ML

# chi

chi_2 <- qchisq(0.95,3)

# valor p, es la proba desde el ML para abajo

v_p <- pchisq(ML, 3, lower.tail = FALSE) #false por la colita de la derecha

v_p

# vemos que v_p> 0.05 por lo que se acepta la prueba
# es decir la categoria Sports, MICS, SHooter no son conjuntamente significativos


#__________________________________________
#  Probemos: si es conjuntamente significativo b_1,b_2 y b_4


# modelo restringido 
modelo_res_3 <- lm(Global ~ Rank+Sports+PS3+X360+Wii, data = data_categ_1)


summary(modelo_res_3)
#obtengamos los residuos
residuos_res_3 <- modelo_res_3$residuals

# hagamos el modelo auxiliar

modelo_aux_3 <-lm(residuos_res_3 ~ Rank+Review+Sports+Misc+Shooter+PS3+X360+Wii, data = data_categ_1)

# Obtengamos el valor ML del modelo auxiliar

ML <- summary(modelo_aux_3)$r.square*601
ML

# chi

chi_2 <- qchisq(0.95,3)

# valor p, es la proba desde el ML para abajo

v_p <- pchisq(ML, 3, lower.tail = FALSE) #false por la colita de la derecha

v_p

# vemos que v_p> 0.05 por lo que se acepta la prueba
# es decir la categoria MICS, SHooter y la variable review no son conjuntamente significativos

#_________________________________________________
#______________________________________________

#diferencias significativas entre los betas (Prueba F)

#PRUEBA DE 2 VARIABLES 

# Probemos: si es conjuntamente significativo b_2 y b_3

#modelo completo

mode <-lm(Global ~ Rank+Review+Sports+Misc+Shooter+PS3+X360+Wii, data = data_categ_1)

# modelo restringido

mode_res <- lm(Global ~ Rank+Review+Misc+PS3+X360+Wii, data = data_categ_1)

# ENCONTREMOS EN F

F <- ((summary(mode)$r.square-summary(mode_res)$r.square)/(1-summary(mode)$r.square)*(601-9)/2)

F

F_c <- qf(0.95,2,592)
F_c


# VEMOS QUE F<fc POR LO QUE SE ACEPTA HO ES DECIR NO SON CONJUNTAENTE SIGNIFICATIVAS

#______________________________

# Probemos: si es conjuntamente significativo b_1 y b_4


# modelo restringido

mode_res_1 <- lm(Global ~ Rank+Sports+Shooter+PS3+X360+Wii, data = data_categ_1)

# ENCONTREMOS EN F

F <- ((summary(mode_1)$r.square-summary(mode_res_1)$r.square)/(1-summary(mode_1)$r.square)*(601-9)/2)

F

F_c <- qf(0.95,2,592)
F_c


# VEMOS QUE F<fc POR LO QUE SE ACEPTA HO ES DECIR NO SON CONJUNTAENTE SIGNIFICATIVAS
#la categoria MICS y la variable REview

#______________________________________

# global = b_0+b_1*GenreMISC+b_2*GenreSHOOTER+b_3*GenreSPORTS+b_4*Review+b_5*Rank+
# b_6*PS3+b_7*X360+b_8*Wii


# Probemos: si es conjuntamente significativo b_5 y b_8


# modelo restringido

mode_res_3 <- lm(Global ~ Review+Sports+Misc+Shooter+PS3+X360, data = data_categ_1)

# ENCONTREMOS EN F

F <- ((summary(mode)$r.square-summary(mode_res_3)$r.square)/(1-summary(mode)$r.square)*(601-9)/2)

F

F_c <- qf(0.95,2,592)
F_c


# VEMOS QUE F>fc POR LO QUE SE Rechaza HO ES DECIR SON CONJUNTAENTE SIGNIFICATIVAS
#la categoria Wii y la variable Rank





#______________________________________
# CON TRES VARIABLES 

# Probemos: si es conjuntamente significativo b_1, b_3 Y b_4


# modelo restringido

mode_res_2 <- lm(Global ~ Rank+Shooter+PS3+X360+Wii, data = data_categ_1)

# ENCONTREMOS EN F

F <- ((summary(mode)$r.square-summary(mode_res_2)$r.square)/(1-summary(mode)$r.square)*(601-9)/3)

F

F_c <- qf(0.95,3,592)
F_c


# VEMOS QUE F<fc POR LO QUE SE ACEPTA HO ES DECIR NO SON CONJUNTAENTE SIGNIFICATIVAS
#la categoria Mics, Sports y la variable REview

#______________________________________
# CON TRES VARIABLES 

# Probemos: si es conjuntamente significativo b_1, b_5 Y b_8


# modelo restringido

mode_res_4 <- lm(Global ~ Review+Sports+Shooter+PS3+X360, data = data_categ_1)

# ENCONTREMOS EN F

F <- ((summary(mode)$r.square-summary(mode_res_4)$r.square)/(1-summary(mode)$r.square)*(601-9)/3)

F

F_c <- qf(0.95,3,592)
F_c


# VEMOS QUE F>fc POR LO QUE SE Rechaza HO ES DECIR SON CONJUNTAENTE SIGNIFICATIVAS
#la categoria Wii, Mics y la variable Rank

#_________________________________________________________________________
#________________________________________________________________________
#_________________________________________________________________________












