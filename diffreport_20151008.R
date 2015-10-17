## script used for dealing with XCMS online diffreport
diffreport <- read.csv("F:\\academic\\data\\XCMS results\\20150801_results\\diffreport20150801.csv", header = TRUE)
## find the masses within certain range that we are interested
usefulmass <- subset(diffreport, mzmed < 400 & mzmed >100, select = mzmed)
## divide the up regulated and down regulated masses 
up <- subset(diffreport, updown == "UP" & mzmed < 400 & mzmed >100, select = mzmed)
down <- subset(diffreport, updown == "DOWN" & mzmed < 400 & mzmed >100, select = mzmed)

up_knm <- up*14/14.01565
up_kmd <- round(up_knm)-up_knm

plot(up_kmd$mzmed ~ up_knm$mzmed, main = "up mass defect", xlab = "Kendrick nominal mass", ylab = "Kendrick mass defect")
plot(down_kmd$mzmed ~ down_knm$mzmed, main = "down mass defect", xlab = "Kendrick nominal mass", ylab = "Kendrick mass defect")

up_combine <- cbind(up, up_knm, up_kmd)
colnames(up_combine) <- c ("up_mas", "up_knm", "up_kmd")
target_oxy <- subset(up_combine, up_combine$up_kmd < 0.186 & up_combine$up_kmd >0.184)
target_n_15 <- subset(down_combine, down_combine$down_kmd < 0.1746 & down_combine$down_kmd > 0.1736 & floor(down_combine$down_mas) %% 2 == 0)
target_n_14 <- subset(down_combine, down_combine$down_kmd < 0.1612 & down_combine$down_kmd > 0.1602 & floor(down_combine$down_mas) %% 2 == 0)
target_n_13 <- subset(down_combine, down_combine$down_kmd < 0.1478 & down_combine$down_kmd > 0.1468 & floor(down_combine$down_mas) %% 2 == 0)
target_n_12 <- subset(down_combine, down_combine$down_kmd < 0.1344 & down_combine$down_kmd > 0.1334 & floor(down_combine$down_mas) %% 2 == 0)
target_n_11 <- subset(down_combine, down_combine$down_kmd < 0.1210 & down_combine$down_kmd > 0.1200 & floor(down_combine$down_mas) %% 2 == 0)
target_n_10 <- subset(down_combine, down_combine$down_kmd < 0.1076 & down_combine$down_kmd > 0.1066 & floor(down_combine$down_mas) %% 2 == 0)
target_n_16 <- subset(down_combine, down_combine$down_kmd < 0.1870 & down_combine$down_kmd > 0.1880 & floor(down_combine$down_mas) %% 2 == 0)
target_n <- rbind(target_n_10, target_n_11, target_n_12, target_n_13, target_n_14, target_n_15, target_n_16)
plot(target_n$down_kmd ~ target_n$down_knm, main = "nitrogen containing", xlab = "Kendrick nominal mass", ylab = "Kendrick mass defect")
