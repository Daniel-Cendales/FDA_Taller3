# Datos de la figura 1.1
t <- seq(from = 0, to = 40, length = 100)
t1 <- t/diff(range(t))
m <- length(t)

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

install.packages(c('mrfDepth'))
