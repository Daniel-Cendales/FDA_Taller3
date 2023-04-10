# ----------------------------------------------- #
#  Vamos a tratar de replicar la columna central  #
# ----------------------------------------------- #
set.seed(3)

t0 <- seq(0, 1, length = 100)

# Normalitas
norms <- sapply(1:100, 
function(k) 5*t0 + rnorm(100, sd = 0.1) + runif(1, min = -5, max = 5))

# Outliers de magnitud
out_mag <- cbind(5*t0 + rnorm(100, sd = 0.1) + 10, 
                 5*t0 + rnorm(100, sd = 0.1) + 19)
# Outliers de forma y magnitud
out_form <- cbind(5*t0+rnorm(100, sd = 0.1)+3*sin(6*pi*t0),
                  5*t0+rnorm(100, sd = 0.1)+5*abs(sin(6*pi*t0)) - 2,
                  5*t0+rnorm(100, sd = 0.1)+3*sin(6*pi*t0) - 15)

# Unión de todas las funciones
datos <- cbind(norms, out_mag, out_form)
par(mfrow = c(2, 2))
matplot(x = t0, y = datos, type = 'l', lty = 1, 
        col = c(rep('gray', 100), 'blue', 'red', 
                'cyan', 'purple', 'green'),
        lwd = rep(c(1, 2), c(102, 3)), frame = FALSE,
        ylim = c(-20, 25), main = 'Datos Originales')
base <- fda::create.bspline.basis(breaks = seq(0, 1, 
                                               length = 20), 
                                  norder = 4)
# AJUSTE
ajuste <- fda::smooth.basis(argvals = t0, y = datos, fdParobj = base)
f_hat <- fda::eval.fd(evalarg = t0, fdobj = ajuste$fd)

matplot(x = t0, y = f_hat, lty = 1, type = 'l',
        col = c(rep('gray', 100), 'blue', 'red', 
                'cyan', 'purple', 'green'),
        lwd = rep(c(1, 2), c(102, 3)), main = 'Suavizados',
        frame = FALSE)

## VAMOS A VER SI ME SALE EL GRÁFICO DE ABAJO
library(fda)
fbplot(x = t0, fit = f_hat, method = 'MBD',
       xlim = c(0, 1), plot = FALSE) -> box_funcional

atipicidades <- 1/box_funcional$depth - 1
f_mediana <- f_hat[, 32]
#lines(x = t0, y = f_mediana, lwd = 3, col = 'black')

plot(x = box_funcional$depth, y = rep(1, 105), pch = 19, 
     col = c(rep('gray', 100), 'blue', 'red', 
                'cyan', 'purple', 'green'), frame = FALSE, 
     xlab = 'Profundidad', ylab = '', main = 'MBD')

#### Gráfico más importante
library(mrfDepth)

o <- sapply(1:100, 
            function(k) outlyingness(x = f_hat[k, ])$outlyingnessX)
v <- t(sapply(1:105,
            function(k) (f_hat[, k] - f_mediana)/
                abs(f_hat[, k] - f_mediana)))
MO_t <- apply(o * v, MAR = 1, mean)

# Varianza
VO_t <- apply(o * v, MAR = 1, var)
##-------- Concatenación
FO_T <- cbind(MO_t, VO_t)
plot(x = FO_T[, 1], y = FO_T[, 2], panel.first = grid(6, 4),
     col = c(rep('gray', 100), 'blue', 'red', 
             'cyan', 'purple', 'green'),
     pch = 19, cex = 0.8, frame = FALSE,
     xlab = 'MO', ylab = 'VO', main = 'dFSDO')

## --------- PRUEBA CON EL PAQUETE QUE EXISTE EN R
library(fdaoutlier)
dir_out(dts = t(f_hat)) -> prueba

datos_4 <- simulation_model4()



## ----- Gráfico 2:
set.seed(5)
t1 <- seq(0, 1, length = 50)
r <- sqrt(runif(n = 100, min = 0, max = 1))
thet <- runif(n = 100, min = 0, max = 2*pi)
x0 <- r*cos(thet)
y0 <- r*sin(thet)
x1 <- 0.7*cos(4*pi*t1)
y1 <- 0.7*sin(4*pi*t1)
x2 <- 0.8*cos(8*pi*t1)
y2 <- 0.8*sin(8*pi*t1)
x3 <- rep(0, 50)
y3 <- rep(-5, 50)
x4 <- rep(5, 50)
y4 <- rep(0, 50)
x5 <- 0.8*cos(4*pi*t1) + 5
y5 <- 0.8*sin(4*pi*t1) - 4

#colores <- c('rgb(166, 122, 122)', 'rgb(0, 255, 255)', 
#             'rgb(128, 0, 128)')
d2 <- data.frame(t = rep(t1, times = 105),
            ind = paste0('m', rep(1:105, each = 50)),
            x = c(rep(x0, each = 50), x1, x2, x3, x4, x5),
            y = c(rep(y0, each = 50), y1, y2, y3, y4, y5))

library(plotly)

graf2 <- plot_ly(d2, x = ~x, y = ~y, z = ~t, split = ~ind,
                 type = 'scatter3d', mode = 'lines',
                 line = list(width = 2, 
                        #color = rep(colores, c(100, 1, 1)),
                             lty = 1),
                 showlegend = FALSE) %>%
layout(scene = list(xaxis = list(range = c(-8, 8), title = ''),
                    yaxis = list(range = c(-8, 8), title = '')))
graf2


## ARREGLO
X <- array(dim = c(105, 50, 2))
X[, , 1] <- t(array(data = d2$x, dim = c(50, 105)))
X[, , 2] <- t(array(data = d2$y, dim = c(50, 105)))

dir_out(dts = X) -> prueba_2
plot(x = prueba_2$mean_outlyingness[, 1],
     y = prueba_2$mean_outlyingness[, 2],
     col = c(rep('gray', 100), 'purple', 'cyan', 
             'blue', 'red', 'green'), pch = 19)
FO_t2 <- cbind(prueba_2$mean_outlyingness, 
               prueba_2$var_outlyingness)

colnames(FO_t2) <- c('MO1', 'MO2', 'VO')
as.data.frame(FO_t2) -> fo_t2
fo_t2$aux <- as.factor(c(rep(1, 100), 2:6))
g3 <- plot_ly(data = fo_t2, 
              x = ~MO1, y = ~MO2, z = ~VO,
        color = ~aux,
        colors = c('gray', 'purple', 'cyan', 
                   'blue', 'red', 'green'))
g3 %>% add_markers()

library(scatterplot3d)
scatterplot3d(fo_t2[,1:3], pch = 16, 
              color = c(rep('gray', 100), 'purple', 'cyan', 
                   'blue', 'red', 'green'),
              grid=TRUE, box=FALSE, type = 'h')
