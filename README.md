# investigacionraga
Investigación Raga

# Introducción Recetas electorales
library(RCurl)
library(tidyverse)

#1. Importar encuestas desde GitHub #### 
encuestas <- read.csv(text=getURL("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv"))

#2. Alistamiento #####
df <- encuestas %>% select(n,fecha, encuestadora,ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras, humberto_delacalle, margen_error, muestra)
df <- df %>% filter(as.Date(fecha, tz="GMT") >= as.Date('2018-01-01', tz="GMT"))
df <- df %>% rename(m_error = margen_error)
df <- df %>% select(-n) %>% gather(candidato, int_voto, ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras,humberto_delacalle)
df <- df %>% mutate(e_max = int_voto+m_error,e_min=int_voto-m_error,fecha=as.Date(fecha))
df <- df %>% transform(candidato=factor(candidato, levels=c("ivan_duque","gustavo_petro"
#,"sergio_fajardo","german_vargas_lleras","humberto_delacalle")
)))
  
resultado <- tribble(~fecha,~candidato,~int_voto,
      "2018-05-27","ivan_duque",39.14,
      #"2018-05-27","german_vargas_lleras",7.28,
      "2018-05-27","gustavo_petro",25.08
      #,"2018-05-27","sergio_fajardo",23.73,
      #"2018-05-27","humberto_delacalle",2.06
      )

# Caracteristicas graficas ####
shape_enc <- c(4,16,17,3,15,25,8)
subtitle <- c("37 encuestas durante 2018, 7 desde la primera vuelta el 27 de mayo. \nCurva LOESS ponderada por el tamaño de la muestra de cada encuesta.")
fuente <- c("Fuente: Cálculos @nelsonamayad con base en las encuestas públicamente disponibles.")
legis <-  geom_vline(xintercept=as.Date("2018-03-11"), size=0.1,linetype="dashed")
pv <-  geom_vline(xintercept=as.Date("2018-05-27"), size=0.1,linetype="dashed")

# 3 La grafica ####
df2 <- df %>% filter(candidato=="ivan_duque" | candidato=="gustavo_petro")
ggplot(df2,aes(x=fecha, y=int_voto)) +
  #Loess
  geom_smooth(aes(fill=candidato, color=candidato, weight=muestra), span=0.5,method="loess", show.legend = F) +
  #Encuestas
  geom_point(aes(shape=encuestadora, size=muestra_int_voto), size=2) +
  #Error
  geom_linerange(aes(ymax=e_max, ymin=e_min),color="grey60") +
  #Primera vuelta y legistlativas
  legis+annotate("text",x=as.Date("2018-03-12"),y=8,label="Legislativas",angle=90,size=2.5)+
  pv+annotate("text",x=as.Date("2018-05-28"),y=8,label="1 era vuelta",angle=90,size=2.5)+
  #Labs
  labs(x="",y="Intencion de voto %",
       title="La única alternativa al chisme y la intuición:", 
       subtitle=subtitle, 
       caption = fuente)+
  #Themes
  theme(legend.position="bottom",
        panel.background=element_rect(fill="white", color="grey50"),
        legend.title=element_blank(),
        legend.key=element_blank(),
        strip.text.x = element_text(size=10),
        strip.background=element_blank()) +
  #Facets
  #facet_wrap(~factor(candidato,levels=c("ivan_duque","gustavo_petro","sergio_fajardo","german_vargas_lleras","humberto_delacalle")),labeller=labeller(candidato=c("gustavo_petro"="Gustavo Petro", "ivan_duque"="Ivan Duque", "sergio_fajardo"="Sergio Fajardo","german_vargas_lleras"="German Vargas Lleras","humberto_delacalle"="Humberto de la Calle"
#)), nrow=1) +
  #Scales
  scale_color_manual(values=c("orangered","gold2"
                              #,"green4","red2","red4"
                              )) +
  scale_fill_manual(values=c("orangered","gold2"
                             #,"green4","red2","red4"
                             )) +
  scale_shape_manual(values=shape_enc)
  
# Poder estadístico de las encuestas
# Preparación para la estimación de poder

  library(RCurl)

# Cargar encuestas desde GitHub
encuestas <- read.csv(text=getURL("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv"), header=T)

#1. Diferencias de proporciones en fichas técnicas
#1.1 Diferencias de proporciones 0.5 para encuestas #: 3-4, 6-9, 16, 29, 33 y Default: 1,10,12,14,18,21,22,24,25,28,31,32,35,37,39,40
cons.1 <- 0.5       
gr1 <- c(1,3,4,6,7,8,9,10,12,14,16,18,21,22,24,25,26,28,29,31,32,33,35,37,39,40)

#1.2 Diferencias de proporciones 0.2 para encuestas #: 2,5,13,17,19,23,34
cons.2 <- 0.2   
gr2 <- c(2,5,13,17,19,23,34)
  
#1.3 Diferencias de proporciones 0.25 para encuestas #11
cons.3 <- 0.25
gr3 <- 11

#1.4 Diferencias de proporciones 0.14 para encuestas # 15 y 20
cons.4 <- 0.14
gr4 <- c(15,20)
  
#1.5 Diferencias de proporciones 0.24 para encuestas # 27
cons.5 <- 0.24
gr5 <- 27
  
#1.6 Diferencias de proporciones 0.35 para encuestas # 30 y 36
cons.6 <- 0.35
gr6 <- c(30,36)

#1.7 Diferencias de proporciones 0.28 para encuestas #38
cons.7 <- 0.28     #Encuestas: 38
gr7 <- 38
  
#2. Tamaños de los efectos
eff.1 <- 0.01   #Efecto a detectar: Diferencia de 1%
eff.2 <- 0.03   #Efecto a detectar: Diferencia de 3%
eff.3 <- 0.05   #Efecto a detectar: Diferencia de 5%
a = 0.05        #Nivel de confianza 95%

#3. Nivel de significancia
a = 0.05         #Nivel de confianza 95%

# poder 1%
library(pwr)
library(tidyverse)

#1. Grupo 1, Efecto 1  ####
pow.1 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[1])
pow.3 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[3])
pow.4 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[4])
pow.6 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[6])
pow.7 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[7])
pow.8 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[8])
pow.9 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[9])
pow.10 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[10])
pow.12 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[12])
pow.14 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[14])
pow.16 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[16])
pow.18 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[18])
pow.21 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[21])
pow.22 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[22])
pow.24 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[24])
pow.25 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[25])
pow.26 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[26])
pow.28 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[28])
pow.29 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[29])
pow.31 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[31])
pow.32 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[32])
pow.33 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[33])
pow.35 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[35])
pow.37 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[37])
pow.39 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[39])
pow.40 <- pwr.p.test(h = ES.h(eff.1+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[40])

#2. Grupo 2, Efecto 1 ####
pow.2 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[2])
pow.5 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[5])
pow.13 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[13])
pow.17 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[17])
pow.19 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[19])
pow.23 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[23])
pow.34 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[34])

#3. Grupo 3, Efecto 1 ####
pow.11 <- pwr.p.test(h = ES.h(eff.1+cons.3, cons.3), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[11])

#4. Grupo 4, Efecto 1 ####
pow.15 <- pwr.p.test(h = ES.h(eff.1+cons.4, cons.4), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[15])
pow.20 <- pwr.p.test(h = ES.h(eff.1+cons.4, cons.4), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[20])

#5. Grupo 5, Efecto 1 ####
pow.27 <- pwr.p.test(h = ES.h(eff.1+cons.5, cons.5), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[27])

#6. Grupo 6, Efecto 1 ####
pow.30 <- pwr.p.test(h = ES.h(eff.1+cons.6, cons.6), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[30])
pow.36 <- pwr.p.test(h = ES.h(eff.1+cons.6, cons.6), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[36])

#7. Grupo 7, Efecto 1 ####
pow.38 <- pwr.p.test(h = ES.h(eff.1+cons.7, cons.7), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[38])

# Tribble con los resultados: ####
d.eff.1 <- tribble(~n, ~power1, 
                   1, pow.1[[4]],
                   2, pow.2[[4]],
                   3, pow.3[[4]],
                   4, pow.4[[4]],
                   5, pow.5[[4]],
                   6, pow.6[[4]],
                   7, pow.7[[4]],
                   8, pow.8[[4]],
                   9, pow.9[[4]],
                   10, pow.10[[4]],
                   11, pow.11[[4]],
                   12, pow.12[[4]],
                   13, pow.13[[4]],
                   14, pow.14[[4]],
                   15, pow.15[[4]],
                   16, pow.16[[4]],
                   17, pow.17[[4]],
                   18, pow.18[[4]],
                   19, pow.19[[4]],
                   20, pow.20[[4]],
                   21, pow.21[[4]],
                   22, pow.22[[4]],
                   23, pow.23[[4]],
                   24, pow.24[[4]],
                   25, pow.25[[4]],
                   26, pow.26[[4]],
                   27, pow.27[[4]],
                   28, pow.28[[4]],
                   29, pow.29[[4]],
                   30, pow.30[[4]],
                   31, pow.31[[4]],
                   32, pow.32[[4]],
                   33, pow.33[[4]],
                   34, pow.34[[4]],
                   35, pow.35[[4]],
                   36, pow.36[[4]],
                   37, pow.37[[4]],
                   38, pow.38[[4]],
                   39, pow.39[[4]],
                   40, pow.40[[4]]
                   )
                   
# Poder 3%
library(pwr)
library(tidyverse)

#1. Grupo 1, Efecto 2  ####
pow.1 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[1])
pow.3 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[3])
pow.4 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[4])
pow.6 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[6])
pow.7 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[7])
pow.8 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[8])
pow.9 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[9])
pow.10 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[10])
pow.12 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[12])
pow.14 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[14])
pow.16 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[16])
pow.18 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[18])
pow.21 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[21])
pow.22 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[22])
pow.24 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[24])
pow.25 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[25])
pow.26 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[26])
pow.28 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[28])
pow.29 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[29])
pow.31 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[31])
pow.32 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[32])
pow.33 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[33])
pow.35 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[35])
pow.37 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[37])
pow.39 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[39])
pow.40 <- pwr.p.test(h = ES.h(eff.2+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[40])


#2. Grupo 2, Efecto 2 ####
pow.2 <- pwr.p.test(h = ES.h(eff.2+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[2])
pow.5 <- pwr.p.test(h = ES.h(eff.2+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[5])
pow.13 <- pwr.p.test(h = ES.h(eff.2+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[13])
pow.17 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[17])
pow.19 <- pwr.p.test(h = ES.h(eff.2+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[19])
pow.23 <- pwr.p.test(h = ES.h(eff.2+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[23])
pow.34 <- pwr.p.test(h = ES.h(eff.2+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[34])

#3. Grupo 3, Efecto 2 ####
pow.11 <- pwr.p.test(h = ES.h(eff.2+cons.3, cons.3), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[11])

#4. Grupo 4, Efecto 2 ####
pow.15 <- pwr.p.test(h = ES.h(eff.2+cons.4, cons.4), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[15])
pow.20 <- pwr.p.test(h = ES.h(eff.2+cons.4, cons.4), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[20])

#5. Grupo 5, Efecto 2 ####
pow.27 <- pwr.p.test(h = ES.h(eff.2+cons.5, cons.5), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[27])

#6. Grupo 6, Efecto 2 ####
pow.30 <- pwr.p.test(h = ES.h(eff.2+cons.6, cons.6), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[30])
pow.36 <- pwr.p.test(h = ES.h(eff.2+cons.6, cons.6), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[36])

#7. Grupo 7, Efecto 3 ####
pow.38 <- pwr.p.test(h = ES.h(eff.2+cons.7, cons.7), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[38])

# Tribble con los resultados: ####
d.eff.2 <- tribble(~n, ~power2, 
                   1, pow.1[[4]],
                   2, pow.2[[4]],
                   3, pow.3[[4]],
                   4, pow.4[[4]],
                   5, pow.5[[4]],
                   6, pow.6[[4]],
                   7, pow.7[[4]],
                   8, pow.8[[4]],
                   9, pow.9[[4]],
                   10, pow.10[[4]],
                   11, pow.11[[4]],
                   12, pow.12[[4]],
                   13, pow.13[[4]],
                   14, pow.14[[4]],
                   15, pow.15[[4]],
                   16, pow.16[[4]],
                   17, pow.17[[4]],
                   18, pow.18[[4]],
                   19, pow.19[[4]],
                   20, pow.20[[4]],
                   21, pow.21[[4]],
                   22, pow.22[[4]],
                   23, pow.23[[4]],
                   24, pow.24[[4]],
                   25, pow.25[[4]],
                   26, pow.26[[4]],
                   27, pow.27[[4]],
                   28, pow.28[[4]],
                   29, pow.29[[4]],
                   30, pow.30[[4]],
                   31, pow.31[[4]],
                   32, pow.32[[4]],
                   33, pow.33[[4]],
                   34, pow.34[[4]],
                   35, pow.35[[4]],
                   36, pow.36[[4]],
                   37, pow.37[[4]],
                   38, pow.38[[4]],
                   39, pow.39[[4]],
                   40, pow.40[[4]]
                   )
                   
# Poder 5%
library(pwr)
library(tidyverse)

#1. Grupo 1, Efecto 3  ####
pow.1 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[1])
pow.3 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[3])
pow.4 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[4])
pow.6 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[6])
pow.7 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[7])
pow.8 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[8])
pow.9 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[9])
pow.10 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[10])
pow.12 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[12])
pow.14 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[14])
pow.16 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[16])
pow.18 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[18])
pow.21 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[21])
pow.22 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[22])
pow.24 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[24])
pow.25 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[25])
pow.26 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[26])
pow.28 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[28])
pow.29 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[29])
pow.31 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[31])
pow.32 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[32])
pow.33 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[33])
pow.35 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[35])
pow.37 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[37])
pow.39 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[39])
pow.40 <- pwr.p.test(h = ES.h(eff.3+cons.1, cons.1), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[40])

#2. Grupo 2, Efecto 3 ####
pow.2 <- pwr.p.test(h = ES.h(eff.3+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[2])
pow.5 <- pwr.p.test(h = ES.h(eff.3+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[5])
pow.13 <- pwr.p.test(h = ES.h(eff.3+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[13])
pow.17 <- pwr.p.test(h = ES.h(eff.1+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[17])
pow.19 <- pwr.p.test(h = ES.h(eff.3+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[19])
pow.23 <- pwr.p.test(h = ES.h(eff.3+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[23])
pow.34 <- pwr.p.test(h = ES.h(eff.3+cons.2, cons.2), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[34])

#3. Grupo 3, Efecto 3 ####
pow.11 <- pwr.p.test(h = ES.h(eff.3+cons.3, cons.3), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[11])

#4. Grupo 4, Efecto 3 ####
pow.15 <- pwr.p.test(h = ES.h(eff.3+cons.4, cons.4), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[15])
pow.20 <- pwr.p.test(h = ES.h(eff.3+cons.4, cons.4), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[20])

#5. Grupo 5, Efecto 3 ####
pow.27 <- pwr.p.test(h = ES.h(eff.3+cons.5, cons.5), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[27])

#6. Grupo 6, Efecto 3 ####
pow.30 <- pwr.p.test(h = ES.h(eff.3+cons.6, cons.6), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[30])
pow.36 <- pwr.p.test(h = ES.h(eff.3+cons.6, cons.6), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[36])

#7. Grupo 7, Efecto 3 ####
pow.38 <- pwr.p.test(h = ES.h(eff.3+cons.7, cons.7), sig.level = a, alternative = "greater", n = encuestas$muestra_int_voto[38])

# Tribble con los resultados: ####
d.eff.3 <- tribble(~n, ~power3, 
                   1, pow.1[[4]],
                   2, pow.2[[4]],
                   3, pow.3[[4]],
                   4, pow.4[[4]],
                   5, pow.5[[4]],
                   6, pow.6[[4]],
                   7, pow.7[[4]],
                   8, pow.8[[4]],
                   9, pow.9[[4]],
                   10, pow.10[[4]],
                   11, pow.11[[4]],
                   12, pow.12[[4]],
                   13, pow.13[[4]],
                   14, pow.14[[4]],
                   15, pow.15[[4]],
                   16, pow.16[[4]],
                   17, pow.17[[4]],
                   18, pow.18[[4]],
                   19, pow.19[[4]],
                   20, pow.20[[4]],
                   21, pow.21[[4]],
                   22, pow.22[[4]],
                   23, pow.23[[4]],
                   24, pow.24[[4]],
                   25, pow.25[[4]],
                   26, pow.26[[4]],
                   27, pow.27[[4]],
                   28, pow.28[[4]],
                   29, pow.29[[4]],
                   30, pow.30[[4]],
                   31, pow.31[[4]],
                   32, pow.32[[4]],
                   33, pow.33[[4]],
                   34, pow.34[[4]],
                   35, pow.35[[4]],
                   36, pow.36[[4]],
                   37, pow.37[[4]],
                   38, pow.38[[4]],
                   39, pow.39[[4]],
                   40, pow.40[[4]]
                   )
# Poder estadístico de las encuestas

library(tidyverse)

#1. Pegar calculos de poder a los datos de las encuestas
pow <- cbind(d.eff.1,d.eff.2[2],d.eff.3[2])
pow <- left_join(select(encuestas, n, fecha, encuestadora, muestra_int_voto), pow, by = "n")

#2. Formato largo
df <- pow %>% gather(enc, power, power1, power2, power3)

#3. Grafica:
ggplot(df %>% filter(n>15), aes(x=power,y=muestra_int_voto,group=encuestadora))+
  geom_jitter(aes(shape=encuestadora, color=enc), size=4) +
  #Corte arbitrario 80%
  geom_vline(xintercept=0.8, linetype="dashed") +   
  theme_classic()+
  theme(legend.position="bottom", legend.title=element_blank(),
        panel.background=element_rect(fill="white",color="grey70")) +
  scale_y_continuous(limits=c(0,4000)) +
  scale_x_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
   annotate("rect",xmin=0.01,xmax=0.2,ymin=2500,ymax=4000,alpha = .1)+
  annotate("text",x=0.1,y=3800,label="Diferencia de 1%",color="red4", size=3)+
  annotate("text",x=0.1,y=3300,label="Diferencia de 3%",color="navy", size=3)+
  annotate("text",x=0.1,y=2800,label="Diferencia de 5%",color="green4", size=3)+
  labs(y="Muestra de la encuesta*", 
       x="Poder de la encuesta", 
       title="3 poderes de las encuestas",
       subtitle="Poder post hoc de las encuestas (N = 25) para detectar diferencias de 1%, 3% y 5%",
       caption="*Muestra que reporta la intención de voto, no necesariamente la muestra total \nFuente: Cálculos @nelsonamayad usando pwr.p.test en R."
       ) +
  scale_shape_manual(values=c(4,16,17,3,15,25,8))+
  scale_color_brewer(palette = "Set1")
  
# Plato simple
# Ingredientes 
# Encuestas
library(RCurl)

# Importar encuestas desde GitHub:
encuestas <- read.csv(text=getURL("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv"))

# Priors 
library(tidyverse)
library(kableExtra)

# Preparar data frame para calcular priors
priors <- encuestas %>% select(n,fecha,ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras,humberto_delacalle) %>% filter(as.Date(fecha)>as.Date("2018-03-11"))
priors <- priors %>% select(-n,-fecha) %>% gather(candidato, int_voto)

# Calcular priors
priors <- priors %>% group_by(candidato) %>% mutate(prior_mu=mean(int_voto),prior_sigma=sd(int_voto))
priors <- priors %>% distinct(prior_mu,prior_sigma) 

# Tabla de priors
priors %>% kable("html", digits=0,caption = "Priors por candidato") %>% kable_styling(full_width = F)

# Preparación
# Alistamiento de datos
library(lubridate)

#1. Depurar encuestas:
df <- encuestas %>% select(n,fecha, encuestadora,ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras, humberto_delacalle, margen_error,tipo, muestra_int_voto)

#2. Solo las encuestas post 2018-03-11:
df <- df %>% filter(as.Date(fecha, tz="GMT") >= as.Date('2018-03-11', tz="GMT"))

#3. Crear variable duracion:
df <- df %>% mutate(dd = as.Date(as.character(today()), format="%Y-%m-%d") - as.Date(as.character(fecha), format="%Y-%m-%d"))
df <- df %>% mutate(dd = as.numeric(dd))
df <- df %>% mutate(dd = 100*(dd/sum(dd)))

#4. Codificar encuestadoras:
df <- df %>% mutate(enc=encuestadora)
df$encuestadora <- match(df$encuestadora, unique(df$encuestadora))

#5. Dummy tipo de encuesta (=1 si presencial):
df <- df %>% mutate(tipo=ifelse(tipo=="Presencial",1,0))

#6. Otros ajustes:
df <- df %>% rename(m_error = margen_error)

#7. Pasar a formato largo:
df <- df %>% select(-fecha,-n) %>% gather(candidato, int_voto, ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras,humberto_delacalle)

#8. Crear data frames por candidato:
id <- df %>% filter(candidato=="ivan_duque") 
gp <- df %>% filter(candidato=="gustavo_petro") 
sf <- df %>% filter(candidato=="sergio_fajardo") 
gvl <- df %>% filter(candidato=="german_vargas_lleras") 
hdlc <- df %>% filter(candidato=="humberto_delacalle") 

# Estimación
data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real muestra_int_voto[N];
    real m_error[N];
    real dd[N];
    real tipo[N];
}
parameters{
    real a1;
    vector[N_encuestadora] a_;
    real a_enc;
    real<lower=0> s_enc;
    real a2;
    real a3;
    real a4;
    real a5;
    real<lower=0> s;
}
model{
    vector[N] m;
    s ~ cauchy( 0 , 5 );
    a5 ~ normal( 0 , 10 );
    a4 ~ normal( 0 , 10 );
    a3 ~ normal( 0 , 10 );
    a2 ~ normal( 0 , 10 );
    s_enc ~ cauchy( 0 , 5 );
    a_enc ~ normal( 0 , 10 );
    a_ ~ normal( a_enc , s_enc );
    a1 ~ normal( 12 , 3 );  //Priors Fajardo: mu=12, sd=3
    for ( i in 1:N ) {
        m[i] = a1+a_[encuestadora[i]]+a2*muestra_int_voto[i]+a3*m_error[i]+a4*dd[i]+a5*tipo[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a1+a_[encuestadora[i]]+a2 * muestra_int_voto[i]+a3*m_error[i]+a4*dd[i]+a5*tipo[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}

# Ahora a RStan
library(rstan)
options(mc.cores = parallel::detectCores())
fajardo_fit <- stan(file='fajardo.stan',data=list(N=17,N_encuestadora=6,int_voto=sf$int_voto,encuestadora=sf$encuestadora, muestra_int_voto=sf$muestra_int_voto,m_error=sf$m_error,dd=sf$dd,tipo=sf$tipo),control=list(adapt_delta=0.95),iter=4000,chains=4)

# Plato mixto
# Ingredientes
# Encuestas
library(RCurl)

# Importar encuestas desde GitHub:
encuestas <- read.csv(text=getURL("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv"))

# Priors
library(tidyverse)
library(kableExtra)

# Preparar data frame para calcular priors
priors <- encuestas %>% select(n,fecha,ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras,humberto_delacalle) %>% filter(as.Date(fecha)>as.Date("2018-03-11"))
priors <- priors %>% select(-n,-fecha) %>% gather(candidato, int_voto)

# Calcular priors
priors <- priors %>% group_by(candidato) %>% mutate(prior_mu=mean(int_voto),prior_sigma=sd(int_voto))
priors <- priors %>% distinct(prior_mu,prior_sigma) 

# Tabla de priors
priors %>% kable("html", digits=0,caption = "Priors por candidato") %>% kable_styling(full_width = F)

# Preparación
# Alistamiento de datos
library(lubridate)

#1. Depurar encuestas:
df <- encuestas %>% select(n,fecha, encuestadora,ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras, humberto_delacalle, margen_error,tipo, muestra_int_voto)

#2. Solo las encuestas post 2018-03-11:
df <- df %>% filter(as.Date(fecha, tz="GMT") >= as.Date('2018-03-11', tz="GMT"))

#3. Crear variable duracion:
df <- df %>% mutate(dd = as.Date(as.character(today()), format="%Y-%m-%d") - as.Date(as.character(fecha), format="%Y-%m-%d"))
df <- df %>% mutate(dd = as.numeric(dd))
df <- df %>% mutate(dd = 100*(dd/sum(dd)))

#4. Codificar encuestadoras:
df <- df %>% mutate(enc=encuestadora)
df$encuestadora <- match(df$encuestadora, unique(df$encuestadora))

#5. Dummy tipo de encuesta (=1 si presencial):
df <- df %>% mutate(tipo=ifelse(tipo=="Presencial",1,0))

#6. Otros ajustes:
df <- df %>% rename(m_error = margen_error)

#7. Pasar a formato largo:
df <- df %>% select(-fecha,-n) %>% gather(candidato, int_voto, ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras,humberto_delacalle)

#8. Crear data frames por candidato:
id <- df %>% filter(candidato=="ivan_duque") 
gp <- df %>% filter(candidato=="gustavo_petro") 
sf <- df %>% filter(candidato=="sergio_fajardo") 
gvl <- df %>% filter(candidato=="german_vargas_lleras") 
hdlc <- df %>% filter(candidato=="humberto_delacalle") 

# Estimación
data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real muestra_int_voto[N];
    real m_error[N];
    real dd[N];
    real tipo[N];
}
parameters{
    vector[N_encuestadora] b4_encuestadora;
    vector[N_encuestadora] b3_encuestadora;
    vector[N_encuestadora] b2_encuestadora;
    vector[N_encuestadora] b1_encuestadora;
    vector[N_encuestadora] a_encuestadora;
    real a;
    real b1;
    real b2;
    real b3;
    real b4;
    vector<lower=0>[5] s_encuestadora;
    real<lower=0> s;
    corr_matrix[5] Rho;
}
transformed parameters{
    vector[5] v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[N_encuestadora];
    vector[5] Mu_ab1b2b3b4;
    cov_matrix[5] SRS_s_encuestadoraRho;
    for ( j in 1:N_encuestadora ) {
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,1] = a_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,2] = b1_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,3] = b2_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,4] = b3_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,5] = b4_encuestadora[j];
    }
    for ( j in 1:5 ) {
        Mu_ab1b2b3b4[1] = a;
        Mu_ab1b2b3b4[2] = b1;
        Mu_ab1b2b3b4[3] = b2;
        Mu_ab1b2b3b4[4] = b3;
        Mu_ab1b2b3b4[5] = b4;
    }
    SRS_s_encuestadoraRho = quad_form_diag(Rho,s_encuestadora);
}
model{
    vector[N] m;
    Rho ~ lkj_corr( 2 );
    s ~ cauchy( 0 , 5 );
    s_encuestadora ~ cauchy( 0 , 5 );
    b4 ~ normal( 0 , 10 );
    b3 ~ normal( 0 , 10 );
    b2 ~ normal( 0 , 10 );
    b1 ~ normal( 0 , 10 );
    a ~ normal( 39 , 4 );
    v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora ~ multi_normal( Mu_ab1b2b3b4 , SRS_s_encuestadoraRho );
    for ( i in 1:N ) {
        m[i] = a_encuestadora[encuestadora[i]] + b1_encuestadora[encuestadora[i]] *      muestra_int_voto[i] + b2_encuestadora[encuestadora[i]] * m_error[i] +      b3_encuestadora[encuestadora[i]] * dd[i] + b4_encuestadora[encuestadora[i]] *      tipo[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a_encuestadora[encuestadora[i]] + b1_encuestadora[encuestadora[i]] *      muestra_int_voto[i] + b2_encuestadora[encuestadora[i]] * m_error[i] +      b3_encuestadora[encuestadora[i]] * dd[i] + b4_encuestadora[encuestadora[i]] *      tipo[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}

# Ahora a RStan
library(rstan)
options(mc.cores = parallel::detectCores())
duque_fit <- stan(file='duque.stan',data=list(N=17,N_encuestadora=6,int_voto=id$int_voto,encuestadora=id$encuestadora, muestra_int_voto=id$muestra_int_voto,m_error=id$m_error,dd=id$dd,tipo=id$tipo),control=list(adapt_delta=0.95),iter=4000,chains=4)
