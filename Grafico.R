### Grafico
par(mfrow = c(2,2))
plot(y_test,Y_pron_m1, ylim = c(-1.5,2), 
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 1 EAM = 0.552",
     pch = 20, col = "cyan")
abline(a = 0,b = 1, col= "darkblue")
plot(y_test,Y_pron_m2, ylim = c(-1.5,2),
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 2 EAM = 0.549",
     pch = 20, col = "cyan")
abline(a = 0,b = 1,col= "darkblue")
plot(y_test,Y_pron_m3, ylim = c(-1.5,2),
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 3 EAM = 0.559",
     pch = 20, col = "cyan")
abline(a = 0,b = 1,col= "darkblue")
plot(y_test,Y_pron_m4, ylim = c(-1.5,2),
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 4 EAM = 0.547",
     pch = 20, col = "cyan")
abline(a = 0,b = 1,col= "darkblue")
#########################################################3
#### grafico
###########################################################
par(mfrow = c(2,2))
# Modelo 1
plot(c(MEAN1),c(SD1), ylab = "Desv. Estándar", xlab="Media",
     pch = 4, col = "gray", ylim=c(0.75,1.4), xlim=c(-0.2,0.2)
     ,main = "Modelo 1")
abline(v = mean(y_test), h = sd(y_test), lty = 2, 
       col = "darkred")
points(mean(y_test),sd(y_test), pch = 15, cex= 1.5, 
       col = "darkred")
text(-0.12, 1.35, "PPP media 0.630", family = "sans", cex= 0.75)
text(-0.12, 1.28, "PPP Des E 0.530", family = "sans", cex= 0.75)
# Modelo 2
plot(c(MEAN2),c(SD2), ylab = "Desv. Estándar", xlab="Media",
     pch = 4, col = "gray", ylim=c(0.75,1.4), xlim=c(-0.2,0.2)
     ,main = "Modelo 2")
abline(v = mean(y_test), h = sd(y_test), lty = 2, 
       col = "darkred")
points(mean(y_test),sd(y_test), pch = 15, cex= 1.5, 
       col = "darkred")
text(-0.12, 1.35, "PPP media 0.614", family = "sans", cex= 0.75)
text(-0.12, 1.28, "PPP Des E 0.513", family = "sans", cex= 0.75)
# Modelo 3
plot(c(MEAN3),c(SD3), ylab = "Desv. Estándar", xlab="Media",
     pch = 4, col = "gray", ylim=c(0.75,1.4), xlim=c(-0.2,0.2)
     ,main = "Modelo 3")
abline(v = mean(y_test), h = sd(y_test), lty = 2, 
       col = "darkred")
points(mean(y_test),sd(y_test), pch = 15, cex= 1.5, 
       col = "darkred")
text(-0.12, 1.35, "PPP media 0.642", family = "sans", cex= 0.75)
text(-0.12, 1.28, "PPP Des E 0.618", family = "sans", cex= 0.75)
# Modelo 4
plot(c(MEAN4),c(SD4), ylab = "Desv. Estándar", xlab="Media",
     pch = 4, col = "gray", ylim=c(0.75,1.4), xlim=c(-0.2,0.2)
     ,main = "Modelo 4")
abline(v = mean(y_test), h = sd(y_test), lty = 2, 
       col = "darkred")
points(mean(y_test),sd(y_test), pch = 15, cex= 1.5, 
       col = "darkred")
text(-0.12, 1.35, "PPP media 0.605", family = "sans", cex= 0.75)
text(-0.12, 1.28, "PPP Des E 0.526", family = "sans", cex= 0.75)
####################################################333
### anexo
#######################################################
par(mfrow = c(2,2))
###
yrange <- range(cadena_M4$LP)
plot(cadena_M1$LP, type = "p", pch = 20, cex = 0.8, col = "darkblue", ylim = yrange,
     xlab = "Iteración",ylab = "Log-verosimilitud", main = "Modelo 1")
###
yrange <- range(cadena_M4$LP)
plot(cadena_M2$LP, type = "p", pch = 20, cex = 0.8, col = "red", ylim = yrange, 
     xlab = "Iteración",ylab = "Log-verosimilitud", main = "Modelo 2")
###
yrange <- range(cadena_M4$LP)
plot(cadena_M3$LP, type = "p", pch = 20, cex = 0.8, col = "darkgreen", ylim = yrange,
     xlab = "Iteración",ylab = "Log-verosimilitud", main = "Modelo 3")
###
yrange <- range(cadena_M4$LP)
plot(cadena_M4$LP, type = "p", pch = 20, cex = 0.8, col = "green", ylim = yrange, 
     xlab = "Iteración",ylab = "Log-verosimilitud", main = "Modelo 4")


