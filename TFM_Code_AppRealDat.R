#################################################
# TFM Code - Beatriz Espadanal Gonçalves ########
#################################################
# Application to real data ######################
#################################################

#-------------------------------
# case A - Berkeley growth study
#-------------------------------

library(fda)

data(growth)
boys <- growth$hgtm
girls <- growth$hgtf

par(mfrow=c(1,3))
matplot(boys,type="l",lty=1, col="deepskyblue", xlab="t", ylab="",
        main="Growth curves")
matpoints(girls,type="l",lty=1,col="darkorange")
boys_d <- funspline(t(boys),bs="cr",k=25,grid = range(growth$age))$deriv
girls_d <- funspline(t(girls),bs="cr",k=25,grid = range(growth$age))$deriv
matplot(t(boys_d),type="l",lty=1,col="deepskyblue",xlab="t",
        main="First derivative")
matpoints(t(girls_d),type="l",lty=1,col="darkorange")
boys_d2 <- funspline(t(boys),bs="cr",k=25,grid = range(growth$age))$deriv2
girls_d2 <- funspline(t(girls),bs="cr",k=25,grid = range(growth$age))$deriv2
matplot(t(boys_d2),type="l",lty=1,col="deepskyblue",xlab="t",
        ylab="",main="Second derivative")
matpoints(t(girls_d2),type="l",lty=1,col="darkorange")


# scatter plots 
par(mfrow=c(3,3))
matplot(generate_indices(t(boys),k=30)$dtaEI,generate_indices(t(boys), k=30)$dtaHI, col="deepskyblue", ylab="HI", xlab="EI",pch=10, 
     main = "Growth", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(t(girls),k=25)$dtaEI, generate_indices(t(girls),k=30)$dtaHI, col="darkorange")
matplot(generate_indices(t(boys),k=30)$dtaMEI,generate_indices(t(boys), k=30)$dtaMHI, col="deepskyblue", ylab="MHI", xlab="MEI",pch=10, 
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(t(girls),k=25)$dtaMEI, generate_indices(t(girls),k=30)$dtaMHI, col="darkorange")
matplot(generate_indices(t(boys),k=30)$dtaMMEI,generate_indices(t(boys), k=30)$dtaMMHI, col="deepskyblue", ylab="MMHI", xlab="MMEI",pch=10)
points(generate_indices(t(girls),k=25)$dtaMMEI, generate_indices(t(girls),k=30)$dtaMMHI, col="darkorange")

matplot(generate_indices(t(boys),k=30)$ddtaEI,generate_indices(t(boys), k=30)$ddtaHI, col="deepskyblue", ylab="HI", xlab="EI",pch=10, 
     main = "Growth", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(t(girls),k=25)$ddtaEI, generate_indices(t(girls),k=30)$ddtaHI, col="darkorange")
matplot(generate_indices(t(boys),k=30)$ddtaMEI,generate_indices(t(boys), k=30)$ddtaMHI, col="deepskyblue", ylab="MHI", xlab="MEI",pch=10, 
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(t(girls),k=25)$ddtaMEI, generate_indices(t(girls),k=30)$ddtaMHI, col="darkorange")
matplot(generate_indices(t(boys),k=30)$ddtaMMEI,generate_indices(t(boys), k=30)$ddtaMMHI, col="deepskyblue", ylab="MMHI", xlab="MMEI",pch=10)
points(generate_indices(t(girls),k=25)$ddtaMMEI, generate_indices(t(girls),k=30)$ddtaMMHI, col="darkorange")

matplot(generate_indices(t(boys),k=30)$d2dtaEI,generate_indices(t(boys), k=30)$d2dtaHI, col="deepskyblue", ylab="HI", xlab="EI",pch=10, 
     main = "Growth", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(t(girls),k=25)$d2dtaEI, generate_indices(t(girls),k=30)$d2dtaHI, col="darkorange")
matplot(generate_indices(t(boys),k=30)$d2dtaMEI,generate_indices(t(boys), k=30)$d2dtaMHI, col="deepskyblue", ylab="MHI", xlab="MEI",pch=10, 
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(t(girls),k=25)$d2dtaMEI, generate_indices(t(girls),k=30)$d2dtaMHI, col="darkorange")
matplot(generate_indices(t(boys),k=30)$d2dtaMMEI,generate_indices(t(boys), k=30)$d2dtaMMHI, col="deepskyblue", ylab="MMHI", xlab="MMEI",pch=10)
points(generate_indices(t(girls),k=25)$d2dtaMMEI, generate_indices(t(girls),k=30)$d2dtaMMHI, col="darkorange")

# -------------------------
# Clustering and validation 
# -------------------------


library(caret)set.seed(12345)
data_g <- cbind(boys,girls)
data_list_g <- list(v3,v4,v5,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
                v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)
labels_g <- c(rep(1,39),rep(2,54))

clust_g <- EHyClus_mm(t(data_g), vars_combinations = data_list_g,
                    n_clusters = 2, true_labels = labels_g)
View(clust_g$metrics)
#clust_g$cluster


# clust$cluster$hierarch$hierarch_ward.D2_euclidean_ddtaMMEIddtaMMHI
# wrongly classified 
# boys: 3 
# girls: 11
# accuracy = [(39-3)+(54-11)]/93 = (36+43)/93 = 0.84946 ~ 84% 

#-----------------------------------
# case B - Canadian weather data set 
#-----------------------------------

data("CanadianWeather")
head(CanadianWeather$dailyAv)
length(CanadianWeather$region)

# smoothing
dta <- funspline(data,k=30,bs="cr")
smooth_cw <- dta$smooth
first_cw <- dta$deriv 
second_cw <- dta$deriv2

par(mfrow=c(1,3))
matplot(1:365, smooth_cw, type="l",
        col=c("yellow","pink","skyblue","lightgreen")[c(rep(1,15),rep(2,12),
                                                        rep(3,5),rep(4,3))],
        xlab="t", ylab="", main="Average Daily Temperature")
matplot(1:365, first_cw, type="l",
        col=c("yellow","pink","skyblue","lightgreen")[c(rep(1,15),rep(2,12),
                                                        rep(3,5),rep(4,3))],
        xlab="t", ylab="", main="first derivatives")
matplot(1:365, second_cw, type="l",
        col=c("yellow","pink","skyblue","lightgreen")[c(rep(1,15),rep(2,12),
                                                        rep(3,5),rep(4,3))],
        xlab="t", ylab="", main="second derivatives")
        
# par(mfrow=c(1,3))
# matplot(1:365, funspline(CanadianWeather$dailyAv[,,1],k=35)$smooth, type="l",
#         lty=1,xlab="t", ylab="")
# matplot(1:365,funspline(CanadianWeather$dailyAv[,,1],k=35)$deriv, type="l",
#         lty=1,xlab="t", ylab="")
# matplot(1:365, funspline(CanadianWeather$dailyAv[,,1],k=35)$deriv2, type="l",
#         lty=1,xlab="t", ylab="")

# scatter plots 

region_1 <- as.matrix(CanadianWeather$dailyAv[, 1:15, 1])
region_2 <- as.matrix(cbind(CanadianWeather$dailyAv[, 16:24, 1], CanadianWeather$dailyAv[, 30:32, 1]))
region_3 <- as.matrix(CanadianWeather$dailyAv[, 25:29, 1])
region_4 <- as.matrix(CanadianWeather$dailyAv[, 33:35, 1])

# original 
par(mfrow=c(3,3))
matplot(generate_indices(region_1,k=15)$dtaEI,generate_indices(region_1, k=15)$dtaHI, col="yellow", ylab="HI", xlab="EI",pch=10,
        main = "Original", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(region_2,k=12)$dtaEI, generate_indices(region_2,k=12)$dtaHI, col="pink")
points(generate_indices(region_3,k=5)$dtaEI, generate_indices(region_3,k=5)$dtaHI, col="skyblue")
points(generate_indices(region_4,k=3)$dtaEI, generate_indices(region_4,k=3)$dtaHI, col="lightgreen")

matplot(generate_indices(region_1,k=15)$dtaMEI,generate_indices(region_1, k=15)$dtaMHI, col="yellow", ylab="MHI", xlab="MEI",pch=10,
        xlim=c(0,1),ylim=c(0,1))
points(generate_indices(region_2,k=12)$dtaMEI, generate_indices(region_2,k=12)$dtaMHI, col="pink")
points(generate_indices(region_3,k=5)$dtaMEI, generate_indices(region_3,k=5)$dtaMHI, col="skyblue")
points(generate_indices(region_4,k=3)$dtaMEI, generate_indices(region_4,k=3)$dtaMHI, col="lightgreen")

matplot(generate_indices(region_1,k=15)$dtaMMEI,generate_indices(region_1, k=15)$dtaMMHI, col="yellow",
        ylab="MMHI", xlab="MMEI",pch=10, ylim=c(0,1))
points(generate_indices(region_2,k=12)$dtaMMEI, generate_indices(region_2,k=12)$dtaMMHI, col="pink")
points(generate_indices(region_3,k=5)$dtaMMEI,generate_indices(region_3, k=5)$dtaMMHI, col="skyblue")
points(generate_indices(region_4,k=3)$dtaMMEI, generate_indices(region_4,k=3)$dtaMMHI, col="lightgreen")

# first derivative
matplot(generate_indices(region_1,k=15)$ddtaEI,generate_indices(region_1, k=15)$ddtaHI, col="yellow", ylab="HI", xlab="EI",pch=10,
        main = "first derivatives", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(region_2,k=12)$ddtaEI, generate_indices(region_2,k=12)$ddtaHI, col="pink")
points(generate_indices(region_3,k=5)$ddtaEI, generate_indices(region_3,k=5)$ddtaHI, col="skyblue")
points(generate_indices(region_4,k=3)$ddtaEI, generate_indices(region_4,k=3)$ddtaHI, col="lightgreen")

matplot(generate_indices(region_1,k=15)$ddtaMEI,generate_indices(region_1, k=15)$ddtaMHI, col="yellow", ylab="MHI", xlab="MEI",pch=10,
        xlim=c(0,1),ylim=c(0,1))
points(generate_indices(region_2,k=12)$ddtaMEI, generate_indices(region_2,k=12)$ddtaMHI, col="pink")
points(generate_indices(region_3,k=5)$ddtaMEI, generate_indices(region_3,k=5)$ddtaMHI, col="skyblue")
points(generate_indices(region_4,k=3)$ddtaMEI, generate_indices(region_4,k=3)$ddtaMHI, col="lightgreen")

matplot(generate_indices(region_1,k=15)$ddtaMMEI,generate_indices(region_1, k=15)$ddtaMMHI, col="yellow",
        ylab="MMHI", xlab="MMEI",pch=10, ylim=c(0,1))
points(generate_indices(region_2,k=12)$ddtaMMEI, generate_indices(region_2,k=12)$ddtaMMHI, col="pink")
points(generate_indices(region_3,k=5)$ddtaMMEI,generate_indices(region_3, k=5)$ddtaMMHI, col="skyblue")
points(generate_indices(region_4,k=3)$ddtaMMEI, generate_indices(region_4,k=3)$ddtaMMHI, col="lightgreen")

#second derivative
matplot(generate_indices(region_1,k=15)$d2dtaEI,generate_indices(region_1, k=15)$d2dtaHI, col="yellow", ylab="HI", xlab="EI",pch=10,
        main = "second derivatives", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(region_2,k=12)$d2dtaEI, generate_indices(region_2,k=12)$d2dtaHI, col="pink")
points(generate_indices(region_3,k=5)$d2dtaEI, generate_indices(region_3,k=5)$d2dtaHI, col="skyblue")
points(generate_indices(region_4,k=3)$d2dtaEI, generate_indices(region_4,k=3)$d2dtaHI, col="lightgreen")

matplot(generate_indices(region_1,k=15)$d2dtaMEI,generate_indices(region_1, k=15)$d2dtaMHI, col="yellow", ylab="MHI", xlab="MEI",pch=10,
        xlim=c(0,1),ylim=c(0,1))
points(generate_indices(region_2,k=12)$d2dtaMEI, generate_indices(region_2,k=12)$d2dtaMHI, col="pink")
points(generate_indices(region_3,k=5)$d2dtaMEI, generate_indices(region_3,k=5)$d2dtaMHI, col="skyblue")
points(generate_indices(region_4,k=3)$d2dtaMEI, generate_indices(region_4,k=3)$d2dtaMHI, col="lightgreen")

matplot(generate_indices(region_1,k=15)$d2dtaMMEI,generate_indices(region_1, k=15)$d2dtaMMHI, col="yellow",
        ylab="MMHI", xlab="MMEI",pch=10, ylim=c(0,1))
points(generate_indices(region_2,k=12)$d2dtaMMEI, generate_indices(region_2,k=12)$d2dtaMMHI, col="pink")
points(generate_indices(region_3,k=5)$d2dtaMMEI,generate_indices(region_3, k=5)$d2dtaMMHI, col="skyblue")
points(generate_indices(region_4,k=3)$d2dtaMMEI, generate_indices(region_4,k=3)$d2dtaMMHI, col="lightgreen")

# clustering

set.seed(12345)
data_list_cw <- list(v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
                  v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

labels_cw <- c(rep(1,15), rep(2,12), rep(3,5), rep(4,3))

clust_cw <- EHyClus_mm(t(CanadianWeather$dailyAv[,,1]), vars_combinations = data_list_cw,
                    n_clusters = 4, true_labels = labels_cw)
View(clust_cw$metrics)
