## script used for dealing with XCMS online diffreport
diffreport <- read.csv("F:\\academic\\data\\XCMS results\\20151016_results\\diffreport20151016.csv", header = TRUE)
## find the masses within certain range that we are interested
usefulmass <- subset(diffreport, mzmed < 400 & mzmed >100, select = mzmed)
## divide the up regulated and down regulated masses 
up <- subset(diffreport, updown == "UP" & mzmed < 500 & mzmed >100, select = mzmed)
down <- subset(diffreport, updown == "DOWN" & mzmed < 500 & mzmed >100, select = mzmed)

fs <- subset(diffreport, FS.B.20151016 == 3 & mzmed < 500 & mzmed >100, select = mzmed)
ts <- subset(diffreport, TS.B.20151016 == 3 & mzmed < 500 & mzmed >100, select = mzmed)


up_knm <- up*14/14.01565
up_kmd <- round(up_knm)-up_knm

fs_knm <- fs*14/14.01565
fs_kmd <- round(fs_knm)-fs_knm
ts_knm <- ts*14/14.01565
ts_kmd <- round(ts_knm)-ts_knm

plot(up_kmd$mzmed ~ up_knm$mzmed, main = "up mass defect", xlab = "Kendrick nominal mass", ylab = "Kendrick mass defect")
plot(down_kmd$mzmed ~ down_knm$mzmed, main = "down mass defect", xlab = "Kendrick nominal mass", ylab = "Kendrick mass defect")

up_combine <- cbind(up, up_knm, up_kmd)
colnames(up_combine) <- c ("up_mas", "up_knm", "up_kmd")

fs_combine <- cbind(fs, fs_knm, fs_kmd)
ts_combine <- cbind(ts, ts_knm, ts_kmd)
colnames(fs_combine) <- c ("fs_mas", "fs_knm", "fs_kmd")
colnames(ts_combine) <- c ("ts_mas", "ts_knm", "ts_kmd")

fs_n_9 <- subset(fs_combine, fs_combine$fs_kmd < 0.1076 & fs_combine$fs_kmd > 0.1066 & floor(fs_combine$fs_mas) %% 2 == 0)
fs_n_10 <- subset(fs_combine, fs_combine$fs_kmd < 0.1210 & fs_combine$fs_kmd > 0.1200 & floor(fs_combine$fs_mas) %% 2 == 0)
fs_n_11 <- subset(fs_combine, fs_combine$fs_kmd < 0.1344 & fs_combine$fs_kmd > 0.1334 & floor(fs_combine$fs_mas) %% 2 == 0)
fs_n_12 <- subset(fs_combine, fs_combine$fs_kmd < 0.1478 & fs_combine$fs_kmd > 0.1468 & floor(fs_combine$fs_mas) %% 2 == 0)
fs_n_13 <- subset(fs_combine, fs_combine$fs_kmd < 0.1612 & fs_combine$fs_kmd > 0.1602 & floor(fs_combine$fs_mas) %% 2 == 0)
fs_n_14 <- subset(fs_combine, fs_combine$fs_kmd < 0.1746 & fs_combine$fs_kmd > 0.1736 & floor(fs_combine$fs_mas) %% 2 == 0)
fs_n_15 <- subset(fs_combine, fs_combine$fs_kmd < 0.1870 & fs_combine$fs_kmd > 0.1880 & floor(fs_combine$fs_mas) %% 2 == 0)
fs_n <- rbind(fs_n_9, fs_n_10, fs_n_11, fs_n_12, fs_n_13, fs_n_14, fs_n_15)

plot(fs_n$fs_kmd ~ fs_n$fs_knm, main = "feed soil nitrogen containing", xlab ="Kendrick nominal mass", ylab = "Kendrick mass defect")

ts_n_9 <- subset(ts_combine, ts_combine$ts_kmd < 0.1076 & ts_combine$ts_kmd > 0.1066 & floor(ts_combine$ts_mas) %% 2 == 0)
ts_n_10 <- subset(ts_combine, ts_combine$ts_kmd < 0.1210 & ts_combine$ts_kmd > 0.1200 & floor(ts_combine$ts_mas) %% 2 == 0)
ts_n_11 <- subset(ts_combine, ts_combine$ts_kmd < 0.1344 & ts_combine$ts_kmd > 0.1334 & floor(ts_combine$ts_mas) %% 2 == 0)
ts_n_12 <- subset(ts_combine, ts_combine$ts_kmd < 0.1478 & ts_combine$ts_kmd > 0.1468 & floor(ts_combine$ts_mas) %% 2 == 0)
ts_n_13 <- subset(ts_combine, ts_combine$ts_kmd < 0.1612 & ts_combine$ts_kmd > 0.1602 & floor(ts_combine$ts_mas) %% 2 == 0)
ts_n_14 <- subset(ts_combine, ts_combine$ts_kmd < 0.1746 & ts_combine$ts_kmd > 0.1736 & floor(ts_combine$ts_mas) %% 2 == 0)
ts_n_15 <- subset(ts_combine, ts_combine$ts_kmd < 0.1870 & ts_combine$ts_kmd > 0.1880 & floor(ts_combine$ts_mas) %% 2 == 0)
ts_n <- rbind(ts_n_9, ts_n_10, ts_n_11, ts_n_12, ts_n_13, ts_n_14, ts_n_15)

plot(ts_n$ts_kmd ~ ts_n$ts_knm, main = "feed soil nitrogen containing", xlab ="Kendrick nominal mass", ylab = "Kendrick mass defect")

ts_o2_10 <- subset(ts_combine, ts_combine$ts_kmd < 0.1509 & ts_combine$ts_kmd > 0.1499 & floor(ts_combine$ts_mas) %% 2 != 0)
ts_o2_11 <- subset(ts_combine, ts_combine$ts_kmd < 0.1643 & ts_combine$ts_kmd > 0.1633 & floor(ts_combine$ts_mas) %% 2 != 0)
ts_o2_12 <- subset(ts_combine, ts_combine$ts_kmd < 0.1877 & ts_combine$ts_kmd > 0.1867 & floor(ts_combine$ts_mas) %% 2 != 0)
ts_o2_13 <- subset(ts_combine, ts_combine$ts_kmd < 0.2012 & ts_combine$ts_kmd > 0.2002 & floor(ts_combine$ts_mas) %% 2 != 0)
ts_o2_14 <- subset(ts_combine, ts_combine$ts_kmd < 0.2146 & ts_combine$ts_kmd > 0.2136 & floor(ts_combine$ts_mas) %% 2 != 0)
ts_o2_15 <- subset(ts_combine, ts_combine$ts_kmd < 0.2280 & ts_combine$ts_kmd > 0.2270 & floor(ts_combine$ts_mas) %% 2 != 0)
