## script used for dealing with XCMS online diffreport
diffreport <- read.csv("diffreport_20160212.csv", header = TRUE)
## find the masses within certain range that we are interested
usefulmass <- subset(diffreport, mzmed < 400 & mzmed >100, select = mzmed)
## divide features in different samples
t0 <- subset(diffreport, SC_T0 == 3 & mzmed < 500 & mzmed >100, select = mzmed)
t14 <- subset(diffreport, SC_T14 == 3 & mzmed < 500 & mzmed >100, select = mzmed)

t0_knm <- t0*14/14.01565
t0_kmd <- round(t0_knm)-t0_knm
t14_knm <- t14*14/14.01565
t14_kmd <- round(t14_knm)-t14_knm

t0_area <- subset(diffreport, SC_T0 == 3 & mzmed < 500 & mzmed >100, select = mean1)
t14_area <- subset(diffreport, SC_T14 == 3 & mzmed < 500 & mzmed >100, select = mean2)

t0_combine <- cbind(t0, t0_knm, t0_kmd, t0_area)
t14_combine <- cbind(t14, t14_knm, t14_kmd, t14_area)

colnames(t0_combine) <- c ("t0_mas", "t0_knm", "t0_kmd", "t0_area")
colnames(t14_combine) <- c ("t14_mas", "t14_knm", "t14_kmd", "t14_area")


#extract features that contain one Nitrogen atom

t0_n_9 <- subset(t0_combine, t0_combine$t0_kmd < 0.1076 & t0_combine$t0_kmd > 0.1066 & floor(t0_combine$t0_mas) %% 2 == 0)
t0_n_10 <- subset(t0_combine, t0_combine$t0_kmd < 0.1210 & t0_combine$t0_kmd > 0.1200 & floor(t0_combine$t0_mas) %% 2 == 0)
t0_n_11 <- subset(t0_combine, t0_combine$t0_kmd < 0.1344 & t0_combine$t0_kmd > 0.1334 & floor(t0_combine$t0_mas) %% 2 == 0)
t0_n_12 <- subset(t0_combine, t0_combine$t0_kmd < 0.1478 & t0_combine$t0_kmd > 0.1468 & floor(t0_combine$t0_mas) %% 2 == 0)
t0_n_13 <- subset(t0_combine, t0_combine$t0_kmd < 0.1612 & t0_combine$t0_kmd > 0.1602 & floor(t0_combine$t0_mas) %% 2 == 0)
t0_n_14 <- subset(t0_combine, t0_combine$t0_kmd < 0.1746 & t0_combine$t0_kmd > 0.1736 & floor(t0_combine$t0_mas) %% 2 == 0)
t0_n_15 <- subset(t0_combine, t0_combine$t0_kmd < 0.1880 & t0_combine$t0_kmd > 0.1870 & floor(t0_combine$t0_mas) %% 2 == 0)
t0_n_16 <- subset(t0_combine, t0_combine$t0_kmd < 0.2014 & t0_combine$t0_kmd > 0.2004 & floor(t0_combine$t0_mas) %% 2 == 0)

t0_n <- rbind(t0_n_9, t0_n_10, t0_n_11, t0_n_12, t0_n_13, t0_n_14, t0_n_15)

t14_n_9 <- subset(t14_combine, t14_combine$t14_kmd < 0.1076 & t14_combine$t14_kmd > 0.1066 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n_10 <- subset(t14_combine, t14_combine$t14_kmd < 0.1210 & t14_combine$t14_kmd > 0.1200 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n_11 <- subset(t14_combine, t14_combine$t14_kmd < 0.1344 & t14_combine$t14_kmd > 0.1334 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n_12 <- subset(t14_combine, t14_combine$t14_kmd < 0.1478 & t14_combine$t14_kmd > 0.1468 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n_13 <- subset(t14_combine, t14_combine$t14_kmd < 0.1612 & t14_combine$t14_kmd > 0.1602 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n_14 <- subset(t14_combine, t14_combine$t14_kmd < 0.1746 & t14_combine$t14_kmd > 0.1736 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n_15 <- subset(t14_combine, t14_combine$t14_kmd < 0.1880 & t14_combine$t14_kmd > 0.1870 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n_16 <- subset(t14_combine, t14_combine$t14_kmd < 0.2014 & t14_combine$t14_kmd > 0.2004 & floor(t14_combine$t14_mas) %% 2 == 0)
t14_n <- rbind(t14_n_9, t14_n_10, t14_n_11, t14_n_12, t14_n_13, t14_n_14, t14_n_15,t14_n_16)


## to draw area related graph with ggplot2

ggplot(t0_n, aes(t0_n$t0_knm, t0_n$t0_kmd)) + geom_point(aes(size = t0_n$t0_area), color = "blue", alpha = 1/4) + scale_size(range = c(0, 10))
ggplot(t14_n, aes(t14_n$t14_knm, t14_n$t14_kmd)) + geom_point(aes(size = t14_n$t14_area), color = "blue", alpha = 1/4) + scale_size(range = c(0, 5.5))
