setwd("C:/Users/domingue/OneDrive - Université du Québec à Trois-Rivières/2 - UQTR/Projets de recherche/Skydive/Donnees/Saut 232")

library("zoo") 
library(BlandAltmanLeh)
library(pracma)

#Flysight traitement
Flysight_data <- read.csv("14-33-25.csv")

time_land = 1000
time_jump = 750
time_start <- -535.5 #-2889.5
Altitude_MSL = 61
Altitude_Exit = 13350/3.28084;
Altitude_Open = 2900/3.28084;


n = nrow(Flysight_data)
time_end <- (n-2)*0.2 + time_start
Flysight_data$hAGL[2:n] <- as.numeric(Flysight_data$hMSL[2:n]) - Altitude_MSL

Flysight_data$second[2:n] <- seq(from = time_start, to = time_end, by = 0.2)
plot(Flysight_data$second[2:n], Flysight_data$hAGL[2:n] , type = "l", xlab="Temps (s)", ylab="Altitude (m)", xlim = c(time_jump,time_land), ylim = c(0,5000), lwd=3)
title("Comparaison des altitudes")


#Protrack II traitementg

Syncho_Protrack <- 791
Protrack_data <- read.csv("profile.csv", sep = ";")

n = nrow(Protrack_data)

Protrack_data$Time..s. <- as.numeric(sub(",", ".", Protrack_data$Time..s., fixed = TRUE))
Protrack_data$Time..s. <- Protrack_data$Time..s.+Syncho_Protrack
Protrack_data$delta[1] <- 0
Protrack_data$delta[2:n] <- diff(Protrack_data$Time..s.)

Protrack_data$Altitude..ft. <- gsub("\u00A0","",Protrack_data$Altitude..ft.) #Remplacer espace insacable!!!! 7la merde
Protrack_data$Altitude..ft. <- as.numeric(Protrack_data$Altitude..ft.)/3.28084
lines(Protrack_data$Time..s.,Protrack_data$Altitude..ft., col="green", lwd=3)

lines(c(0,time_land), c(Altitude_Exit,Altitude_Exit), col = "red", lty=2, lwd=3)
lines(c(0,time_land), c(Altitude_Open,Altitude_Open), col = "blue",lty=2, lwd=3)

#Expedite traitement
Expedite_data <- read.csv("22101601.csv")
Expedite_data <- Expedite_data[!is.na(as.numeric(Expedite_data$millis)), ] 

n = nrow(Expedite_data)
#Expedite_data$hAGL[2:n] <- as.numeric(Expedite_data$hMSL[2:n]) - Altitude_MSL
Expedite_data$h_msl <- as.numeric(Expedite_data$h_msl)/1000

Expedite_data$delta[1] <- 0
Expedite_data$delta[2:n] <- diff(as.numeric(Expedite_data$millis))
Expedite_data$time[1] <- 0
Expedite_data$time <- cumsum((Expedite_data$delta))/1000
Expedite_data$hAGL <- Expedite_data$h_msl-Altitude_MSL
lines(Expedite_data$time,Expedite_data$hAGL, col="blue", lwd=3)
lines(Expedite_data$time,Expedite_data$baro_alt, col = 2, lwd=3)


#légende
legend(x = "topright", legend=c("Flysight", "Expedite", "Protrack", "Baro"),
       col=c("black", "blue", "green", 2), lty=1, cex=1,
       box.lty=1)

#Vitesse verticale
plot(Flysight_data$second[2:n], Flysight_data$velD[2:n] , type = "l", xlab="Temps (s)", ylab="Vitesse (m/s)", xlim = c(time_jump,time_land), ylim = c(0,150), lwd=3)
title("Comparaison des vitesses verticales")

lines(Expedite_data$time,Expedite_data$speed_v/1000, col="blue", lwd=3)


n = nrow(Protrack_data)
Protrack_data$vitesse[1] <- 0
Protrack_data$vitesse[2:n] <- -diff(as.numeric(Protrack_data$Altitude..ft.))/Protrack_data$delta[2:n]
Protrack_data$vitesse[5:n] <- rollmean(Protrack_data$vitesse, k = 5)
lines(Protrack_data$Time..s., Protrack_data$vitesse, col = "green", lwd=3)

#légende
legend(x = "topright", legend=c("Flysight", "Expedite", "Protrack"),
       col=c("black", "blue", "green", 2), lty=1, cex=1,
       box.lty=1)

#Vitesse hor
n = nrow(Flysight_data)
Flysight_data$Vitesse_hor[2:n] <- (as.numeric(Flysight_data$velN[2:n])^2+as.numeric(Flysight_data$velE[2:n])^2)^0.5

plot(Flysight_data$second[2:n], Flysight_data$Vitesse_hor[2:n] , type = "l", xlab="Temps (s)", ylab="Vitesse (m/s)", xlim = c(time_jump,time_land), ylim = c(0,80), lwd=3)
title("Comparaison des vitesses horizontales")

lines(Expedite_data$time,Expedite_data$speed_h/1000, col="blue", lwd=3)

#légende
legend(x = "topright", legend=c("Flysight", "Expedite"),
       col=c("black", "blue", 2), lty=1, cex=1,
       box.lty=1)



#Graphs BA
ys <- interp1(Expedite_data$time, Expedite_data$speed_v/1000, seq(time_jump, time_land, by = 0.05), method = "linear")
xs <- seq(time_jump, time_land, by = 0.05)

yss <- interp1(Flysight_data$second[2:n], as.numeric(Flysight_data$velD[2:n]), seq(time_jump,time_land, by = 0.05), method = "cubic") 
xss <- seq(time_jump,time_land, by = 0.05)

bland.altman.plot(ys, yss, main=" ", xlab="Moyenne", ylab="Expedite - Flysight",cex.lab = 1.7,family = "A",cex.axis = 1.7)
title("Bland-Altman Vitesse Verticale")

#-----

ys <- interp1(Expedite_data$time, Expedite_data$hAGL, seq(time_jump, time_land, by = 0.05), method = "linear")
xs <- seq(time_jump, time_land, by = 0.05)

yss <- interp1(Flysight_data$second[2:n], as.numeric(Flysight_data$hAGL[2:n]), seq(time_jump,time_land, by = 0.05), method = "cubic") 
xss <- seq(time_jump,time_land, by = 0.05)

bland.altman.plot(ys, yss, main=" ", xlab="Moyenne", ylab="Expedite - Flysight",cex.lab = 1.7,family = "A",cex.axis = 1.7)
title("Bland-Altman Altitude")

#-----

ys <- interp1(Expedite_data$time, Expedite_data$speed_h/1000, seq(time_jump, time_land, by = 0.05), method = "linear")
xs <- seq(time_jump, time_land, by = 0.05)

yss <- interp1(Flysight_data$second[2:n], as.numeric(Flysight_data$Vitesse_hor[2:n]), seq(time_jump,time_land, by = 0.05), method = "cubic") 
xss <- seq(time_jump,time_land, by = 0.05)

bland.altman.plot(ys, yss, main=" ", xlab="Moyenne", ylab="Expedite - Flysight",cex.lab = 1.7,family = "A",cex.axis = 1.7)
title("Bland-Altman Vitesse Horizontale")