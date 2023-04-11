library(plotly)
library(roahd)

# Datos de la figura 1.1
t <- seq(from = 0, to = 40, length = 100)
t1 <- t/diff(range(t))
m <- length(t)

## Versión 1
muestras <- vector(mode = 'list', length = 42)
for(k in seq(2, 40, by = 2)){
    muestras[[k]] <- rbind(0.5*rep(sin(k*pi/10), each = m),
                         0.5*rep(cos(k*pi/10), each = m))
    muestras[[k-1]] <- rbind(rep(sin((k-1)*pi/10), each = m),
                           rep(cos((k-1)*pi/10), each = m))
}
muestras[[41]] <- rbind(rep(0, m),
                        rep(0, m))
muestras[[42]] <- rbind(0.5*sin(t*pi/10), 
                        0.5*cos(t*pi/10))

## Versión 2
m2 <- vector(mode = 'list', length = 2)
t(sapply(muestras, FUN = function(k) k[1, ])) -> m2[[1]]
t(sapply(muestras, FUN = function(k) k[2, ])) -> m2[[2]]

## datos 3D
d2 <- data.frame(x = c(t(m2[[1]])),
                 y = c(t(m2[[2]])),
                 t = rep(t1, times = 42),
                 ind = rep(paste0('m', 1:42), each = 100))

graf2 <- plot_ly(d2, x = ~x, y = ~y, z = ~t, split = ~ind,
                 type = 'scatter3d', mode = 'lines',
                 line = list(width = ~rep(c(1, 4), c(41, 1)), 
                        #color = rep(colores, c(100, 1, 1)),
                             lty = 1),
                 showlegend = FALSE) %>%
layout(scene = list(xaxis = list(range = c(-2, 2), title = ''),
                    yaxis = list(range = c(-2, 2), title = '')))
graf2

multi <- mfData(grid = t1, Data_list = m2)
plot(multi)
fbplot(Data = multi, adjust = TRUE, main = 'Boxplot Funcional')



# Gráfico plano
colores <- c(rep(c('gray', 'brown'), times = 20), 'gray', 'blue')
plot(x = 0, y = 0, type = 'n', xlim = c(-1, 1), ylim = c(-1, 1), frame = FALSE)
for(k in 42:1){
    points(x = muestras[[k]][1, ], y = muestras[[k]][2, ], 
           col = colores[k], pch = 19)
}

library(plotly)
f1 <- plot_ly(x = ~muestras[[42]][1, ], y = ~muestras[[42]][2, ], z = ~t1, 
              type = 'scatter3d', 
              mode = 'lines',
              line = list(color = '#1f77b4', width = 3))

f1


