# Grafico 1
yrange <- range(cadena_M1$LP)
plot(cadena_M1$LP, type = "p", pch = 20, cex = 0.8, col = "darkblue",
     ylim = yrange, xlab = "Iteración",ylab = "Log-verosimilitud", 
     main = "Modelo 1")
# Grafico 2
yrange <- range(cadena_M2$LP)
plot(cadena_M2$LP, type = "p", pch = 20, cex = 0.8, col = "red", 
     ylim = yrange, xlab = "Iteración",ylab = "Log-verosimilitud", 
     main = "Modelo 2")
# Grafico 3
yrange <- range(cadena_M3$LP)
plot(cadena_M3$LP, type = "p", pch = 20, cex = 0.8, col = "darkgreen", 
     ylim = yrange, xlab = "Iteración",ylab = "Log-verosimilitud",
     main = "Modelo 3")
# Gr?fico
yrange <- range(cadena_M4$LP)
plot(cadena_M4$LP, type = "p", pch = 20, cex = 0.8, col = "Brown",
     ylim = yrange, xlab = "Iteración",ylab = "Log-verosimilitud", 
     main = "Modelo 4")




