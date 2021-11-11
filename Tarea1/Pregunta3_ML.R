library(dplyr)
library(readxl)

lineal_horm <- read_excel("Documents/UNAM/7mo Semestre/ML/Tarea 1/lineal_horm.xlsx")

A <- lineal_horm %>% filter(lote == "A") %>% select(-lote)
B <- lineal_horm %>% filter(lote == "B") %>% select(-lote)
C <- lineal_horm %>% filter(lote == "C") %>% select(-lote)

lmA <- lm(monto ~ horas, data = A)
lmB <- lm(monto ~ horas, data = B)
lmC <- lm(monto ~ horas, data = C)

#Intervalos de confianza de la regresión lineal

confint(lmA)
confint(lmB)
confint(lmC)


