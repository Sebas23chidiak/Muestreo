# Creación del dataframe con columnas CH y Y
#Creación del DataFrame: Se crea un DataFrame llamado data con las columnas CH 
#y Y que contienen los números del 1 al 120 y una serie de valores binarios (0 y 1), respectivamente.

CH <- 1:120
Y <- Y <- c(0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,1,1,0,1,1,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,1,1,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,1,0,0,0,1,0,0)
data <- data.frame(CH, Y)

# Establecer semilla aleatoria y seleccionar muestra
#Selección de Muestra Aleatoria: Se establece una semilla aleatoria para garantizar
#la reproducibilidad de los resultados y se selecciona
#una muestra de tamaño 15 sin reemplazo del conjunto de datos original.

set.seed(789)
muestra <- sample(x = 1:120, size = 15, replace = FALSE)

# Calcular la suma de Y correspondiente a la muestra
#Cálculo de la Suma de Y en la Muestra: Se calcula la suma de los valores de
#la columna Y correspondientes a los índices de la muestra seleccionada.
suma_muestra <- sum(data[muestra, "Y"])

# Definir funciones para intervalos de confianza con distribución hipergeométrica
#Definición de Funciones para Distribución Hipergeométrica: Se definen 
#las funciones Ls.conf.hiper.prop y Li.conf.hiper.prop para calcular 
#los límites superior e inferior del intervalo de confianza utilizando la distribución hipergeométrica.
a <- 5
N <- 120
n <- 15
alfa <- 0.03 
Ls.conf.hiper.prop <- function(A, a, N, n, alfa) {
  phyper(a, A, N - A, n) - alfa * 0.5
}
Li.conf.hiper.prop <- function(A, a, N, n, alf) {
  1 - phyper(a - 1, A, N - A, n) - alf / 2
}

# Gráficos de intervalos de confianza usando distribución hipergeométrica
#Gráficos de Intervalos de Confianza con Distribución Hipergeométrica: 
#Se generan gráficos que muestran las funciones definidas anteriormente, 
#visualizando los límites inferior y superior del intervalo de confianza
#utilizando la distribución hipergeométrica.
par(mfrow = c(1, 2))
curve(Li.conf.hiper.prop(x, a, N, n, alfa), from = 0, to = 41, xlab = "LI", las = 2)
abline(h = 0, col = "red")
curve(Ls.conf.hiper.prop(x, a, N, n, alfa), from = 41, to = N, xlab = "LS")
abline(h = 0, col = "red")

#Cálculo de Intervalos de Confianza con Distribución Hipergeométrica:
#Se utilizan las funciones uniroot.all para encontrar las raíces de las ecuaciones 
#y determinar los valores de los límites inferior y superior del intervalo de confianza
#utilizando la distribución hipergeométrica.
# Encontrar límites de confianza con distribución hipergeométrica
require(rootSolve)
fun_LS_hiper <- function(x) { Ls.conf.hiper.prop(x, a, N, n, alfa) }
LimS1 <- uniroot.all(fun_LS_hiper, c(40, 80))
fun_LI_hiper <- function(x) { Li.conf.hiper.prop(x, a, N, n, alfa) }
LimI1 <- uniroot.all(fun_LI_hiper, c(0, 40))

# Mostrar intervalos de confianza hallados mediante distribución hipergeométrica
#Se imprimen en la consola los intervalos de confianza
#para la proporción y el total calculados mediante la distribución hipergeométrica.
cat('IC Proporción = (', c(LimI1/N, LimS1/N), ')\n')
cat('IC Total = (', c(floor(LimI1), ceiling(LimS1)), ')\n')

# Definir funciones para intervalos de confianza con distribución binomial
#Se definen las funciones Ls.conf.binom.prop y Li.conf.binom.prop 
#para calcular los límites superior e inferior del intervalo de confianza utilizando la distribución binomial.
a <- 5 
n <- 15
alfa <- 0.03

Ls.conf.binom.prop <- function(a, n, p, alfa) {
  pbinom(a, n, p) - alfa * 0.5
}
Li.conf.binom.prop <- function(a, n, p, alfa) {
  1 - pbinom(a, n, p) - alfa * 0.5
}

# Gráficos de intervalos de confianza usando distribución binomial
# Se generan gráficos que muestran las funciones definidas anteriormente, 
# visualizando los límites inferior y superior del intervalo de confianza
# utilizando la distribución binomial.
par(mfrow = c(1, 2))
curve(Li.conf.binom.prop(a, n, x, alfa), from = 0, to = a/n, xlab = "LI", las = 2)
abline(h = 0, col = "red")
curve(Ls.conf.binom.prop(a, n, x, alfa), from = a/n, to = 1, xlab = "LS", las = 2)
abline(h = 0, col = "red")

# Encontrar límites de confianza con distribución binomial.
# Se utilizan las funciones uniroot.all para encontrar
# las raíces de las ecuaciones y determinar los valores de los límites inferior 
# y superior del intervalo de confianza utilizando la distribución binomial.
fun_LS_binom <- function(x) { Ls.conf.binom.prop(a, n, x, alfa) }
LimS <- uniroot.all(fun_LS_binom, c(0.20, 0.80))
fun_LI_binom <- function(x) { Li.conf.binom.prop(a, n, x, alfa) }
LimI <- uniroot.all(fun_LI_binom, c(0.10, 0.20))

# Mostrar intervalos de confianza hallados mediante distribución binomial.
# Se imprimen en la consola los intervalos de confianza para la proporción 
# y el total calculados mediante la distribución binomial.
cat('IC Proporción = (', c(LimI, LimS), ')\n')
cat('IC Total = (', c(floor(N * LimI), ceiling(N * LimS)), ')\n')


# Se proporcionan los cálculos necesarios para hallar los intervalos de confianza
#usando tanto la aproximación binomial como la aproximación hipergeométrica para 
#el total de hogares con adultos mayores.

# Punto 3: Calcular intervalo de confianza con aproximación binomial
# Se muestra en texto el cálculo del intervalo de confianza utilizando la aproximación binomial 
# y se presenta el resultado en el formato requerido.
ic_aprox_binom <- paste("(", floor(N * LimI), ",", ceiling(N * LimS), ")")

# Punto 3: Calcular intervalo de confianza con aproximación hipergeométrica
# Este código reorganiza y simplifica el código original mientras mantiene las funcionalidades
# y los cálculos esenciales. Además, proporciona explicaciones detalladas para cada paso, 
# lo que debería ayudar a comprender mejor el flujo y la lógica del código.

ic_aprox_hiper <- paste("(", floor(LimI1), ",", ceiling(LimS1), ")")
