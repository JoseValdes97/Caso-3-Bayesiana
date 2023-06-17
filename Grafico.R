### Grafico
par(mfrow = c(2,2))
plot(y_test,Y_pron_m1, ylim = c(-1.5,2), 
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 1 EAM = 0.552")
abline(a = 0,b = 1)
plot(y_test,Y_pron_m2, ylim = c(-1.5,2),
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 2 EAM = 0.549")
abline(a = 0,b = 1)
plot(y_test,Y_pron_m3, ylim = c(-1.5,2),
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 3 EAM = 0.559")
abline(a = 0,b = 1)
plot(y_test,Y_pron_m4, ylim = c(-1.5,2),
     ylab = expression(hat(y)["Test"]),
     xlab = expression(y["Test"]),
     main = "Modelo 4 EAM = 0.547")
abline(a = 0,b = 1)
#########################################################3
#### grafico
###########################################################
par(mfrow = c(2,2))
# Modelo 1
plot(c(MEAN1),c(SD1))
abline(v = mean(y_test), h = sd(y_test))
# Modelo 2
plot(c(MEAN2),c(SD2))
abline(v = mean(y_test), h = sd(y_test))
# Modelo 3
plot(c(MEAN3),c(SD3))
abline(v = mean(y_test), h = sd(y_test))
# Modelo 4
plot(c(MEAN4),c(SD4))
abline(v = mean(y_test), h = sd(y_test))