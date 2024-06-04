<<<<<<< HEAD
#############################
# Plots #####################
#############################

# curves
# original 
plt_fun(data_1, data1_labels)
plt_fun(data_3, data3_labels)
plt_fun(data_4, data4_labels)
plt_fun(data_5, data5_labels)
plt_fun(data_6, data6_labels)
plt_fun(data_7, data7_labels)
plt_fun(data_8, data8_labels)

# first derivative 
data_1d <- funspline(data_1,nbasis=25,norder=4)$deriv
data_3d <- funspline(data_3,nbasis=25,norder=4)$deriv
data_4d <- funspline(data_4,nbasis=25,norder=4)$deriv
data_5d <- funspline(data_5,nbasis=25,norder=4)$deriv
data_6d <- funspline(data_6,nbasis=25,norder=4)$deriv
data_7d <- funspline(data_7,nbasis=25,norder=4)$deriv
data_8d <- funspline(data_8,nbasis=25,norder=4)$deriv

plt_fun(data_1d,data1_labels)
plt_fun(data_3d,data3_labels)
plt_fun(data_4d,data4_labels)
plt_fun(data_5d,data5_labels)
plt_fun(data_6d,data6_labels)
plt_fun(data_7d,data7_labels)
plt_fun(data_8d,data8_labels)

# second derivative 
data_1d2 <- funspline(data_1,nbasis=25,norder=4)$deriv2
data_3d2 <- funspline(data_3,nbasis=25,norder=4)$deriv2
data_4d2 <- funspline(data_4,nbasis=25,norder=4)$deriv2
data_5d2 <- funspline(data_5,nbasis=25,norder=4)$deriv2
data_6d2 <- funspline(data_6,nbasis=25,norder=4)$deriv2
data_7d2 <- funspline(data_7,nbasis=25,norder=4)$deriv2
data_8d2 <- funspline(data_8,nbasis=25,norder=4)$deriv2

plt_fun(data_1d2,data1_labels)
plt_fun(data_3d2,data3_labels)
plt_fun(data_4d2,data4_labels)
plt_fun(data_5d2,data5_labels)
plt_fun(data_6d2,data6_labels)
plt_fun(data_7d2,data7_labels)
plt_fun(data_8d2,data8_labels)

# scatter plots for EI, MEI, MMEI, HI, MHI and MMHI
par(mfrow = c(3,3))

# model 1 and 2
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 2")
points(ind(data_2)$dtaEI, ind(data_2)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_2)$dtaMEI, ind(data_2)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_2)$dtaMMEI, ind(data_2)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_2)$ddtaEI, ind(data_2)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_2)$ddtaMEI, ind(data_2)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_2)$ddtaMMEI, ind(data_2)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_2)$d2dtaEI, ind(data_2)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_2)$d2dtaMEI, ind(data_2)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_2)$d2dtaMMEI, ind(data_2)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 3
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 3")
points(ind(data_3)$dtaEI, ind(data_3)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_3)$dtaMEI, ind(data_3)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_3)$dtaMMEI, ind(data_3)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_3)$ddtaEI, ind(data_3)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_3)$ddtaMEI, ind(data_3)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_3)$ddtaMMEI, ind(data_3)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_3)$d2dtaEI, ind(data_3)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_3)$d2dtaMEI, ind(data_3)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_3)$d2dtaMMEI, ind(data_3)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 4
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 4")
points(ind(data_4)$dtaEI, ind(data_4)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_4)$dtaMEI, ind(data_4)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_4)$dtaMMEI, ind(data_4)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_4)$ddtaEI, ind(data_4)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_4)$ddtaMEI, ind(data_4)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_4)$ddtaMMEI, ind(data_4)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_4)$d2dtaEI, ind(data_4)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_4)$d2dtaMEI, ind(data_4)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_4)$d2dtaMMEI, ind(data_4)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 5
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 5")
points(ind(data_5)$dtaEI, ind(data_5)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_5)$dtaMEI, ind(data_5)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_5)$dtaMMEI, ind(data_5)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_5)$ddtaEI, ind(data_5)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_5)$ddtaMEI, ind(data_5)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_5)$ddtaMMEI, ind(data_5)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_5)$d2dtaEI, ind(data_5)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_5)$d2dtaMEI, ind(data_5)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_5)$d2dtaMMEI, ind(data_5)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 6
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 6")
points(ind(data_6)$dtaEI, ind(data_6)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_6)$dtaMEI, ind(data_6)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_6)$dtaMMEI, ind(data_6)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_6)$ddtaEI, ind(data_6)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_6)$ddtaMEI, ind(data_6)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_6)$ddtaMMEI, ind(data_6)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_6)$d2dtaEI, ind(data_6)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_6)$d2dtaMEI, ind(data_6)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_6)$d2dtaMMEI, ind(data_6)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 7
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 7")
points(ind(data_7)$dtaEI, ind(data_7)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_7)$dtaMEI, ind(data_7)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_7)$dtaMMEI, ind(data_7)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_7)$ddtaEI, ind(data_7)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_7)$ddtaMEI, ind(data_7)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_7)$ddtaMMEI, ind(data_7)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_7)$d2dtaEI, ind(data_7)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_7)$d2dtaMEI, ind(data_7)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_7)$d2dtaMMEI, ind(data_7)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 8
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 8")
points(ind(data_8)$dtaEI, ind(data_8)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_8)$dtaMEI, ind(data_8)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_8)$dtaMMEI, ind(data_8)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_8)$ddtaEI, ind(data_8)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_8)$ddtaMEI, ind(data_8)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_8)$ddtaMMEI, ind(data_8)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_8)$d2dtaEI, ind(data_8)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_8)$d2dtaMEI, ind(data_8)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_8)$d2dtaMMEI, ind(data_8)$d2dtaMMHI, col="blue",pch=20)

=======
#############################
# Plots #####################
#############################

# curves
# original 
plt_fun(data_1, data1_labels)
plt_fun(data_3, data3_labels)
plt_fun(data_4, data4_labels)
plt_fun(data_5, data5_labels)
plt_fun(data_6, data6_labels)
plt_fun(data_7, data7_labels)
plt_fun(data_8, data8_labels)

# first derivative 
data_1d <- funspline(data_1,nbasis=25,norder=4)$deriv
data_3d <- funspline(data_3,nbasis=25,norder=4)$deriv
data_4d <- funspline(data_4,nbasis=25,norder=4)$deriv
data_5d <- funspline(data_5,nbasis=25,norder=4)$deriv
data_6d <- funspline(data_6,nbasis=25,norder=4)$deriv
data_7d <- funspline(data_7,nbasis=25,norder=4)$deriv
data_8d <- funspline(data_8,nbasis=25,norder=4)$deriv

plt_fun(data_1d,data1_labels)
plt_fun(data_3d,data3_labels)
plt_fun(data_4d,data4_labels)
plt_fun(data_5d,data5_labels)
plt_fun(data_6d,data6_labels)
plt_fun(data_7d,data7_labels)
plt_fun(data_8d,data8_labels)

# second derivative 
data_1d2 <- funspline(data_1,nbasis=25,norder=4)$deriv2
data_3d2 <- funspline(data_3,nbasis=25,norder=4)$deriv2
data_4d2 <- funspline(data_4,nbasis=25,norder=4)$deriv2
data_5d2 <- funspline(data_5,nbasis=25,norder=4)$deriv2
data_6d2 <- funspline(data_6,nbasis=25,norder=4)$deriv2
data_7d2 <- funspline(data_7,nbasis=25,norder=4)$deriv2
data_8d2 <- funspline(data_8,nbasis=25,norder=4)$deriv2

plt_fun(data_1d2,data1_labels)
plt_fun(data_3d2,data3_labels)
plt_fun(data_4d2,data4_labels)
plt_fun(data_5d2,data5_labels)
plt_fun(data_6d2,data6_labels)
plt_fun(data_7d2,data7_labels)
plt_fun(data_8d2,data8_labels)

# scatter plots for EI, MEI, MMEI, HI, MHI and MMHI
par(mfrow = c(3,3))

# model 1 and 2
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 2")
points(ind(data_2)$dtaEI, ind(data_2)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_2)$dtaMEI, ind(data_2)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_2)$dtaMMEI, ind(data_2)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_2)$ddtaEI, ind(data_2)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_2)$ddtaMEI, ind(data_2)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_2)$ddtaMMEI, ind(data_2)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_2)$d2dtaEI, ind(data_2)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_2)$d2dtaMEI, ind(data_2)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_2)$d2dtaMMEI, ind(data_2)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 3
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 3")
points(ind(data_3)$dtaEI, ind(data_3)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_3)$dtaMEI, ind(data_3)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_3)$dtaMMEI, ind(data_3)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_3)$ddtaEI, ind(data_3)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_3)$ddtaMEI, ind(data_3)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_3)$ddtaMMEI, ind(data_3)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_3)$d2dtaEI, ind(data_3)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_3)$d2dtaMEI, ind(data_3)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_3)$d2dtaMMEI, ind(data_3)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 4
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 4")
points(ind(data_4)$dtaEI, ind(data_4)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_4)$dtaMEI, ind(data_4)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_4)$dtaMMEI, ind(data_4)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_4)$ddtaEI, ind(data_4)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_4)$ddtaMEI, ind(data_4)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_4)$ddtaMMEI, ind(data_4)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_4)$d2dtaEI, ind(data_4)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_4)$d2dtaMEI, ind(data_4)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_4)$d2dtaMMEI, ind(data_4)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 5
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 5")
points(ind(data_5)$dtaEI, ind(data_5)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_5)$dtaMEI, ind(data_5)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_5)$dtaMMEI, ind(data_5)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_5)$ddtaEI, ind(data_5)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_5)$ddtaMEI, ind(data_5)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_5)$ddtaMMEI, ind(data_5)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_5)$d2dtaEI, ind(data_5)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_5)$d2dtaMEI, ind(data_5)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_5)$d2dtaMMEI, ind(data_5)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 6
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 6")
points(ind(data_6)$dtaEI, ind(data_6)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_6)$dtaMEI, ind(data_6)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_6)$dtaMMEI, ind(data_6)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_6)$ddtaEI, ind(data_6)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_6)$ddtaMEI, ind(data_6)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_6)$ddtaMMEI, ind(data_6)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_6)$d2dtaEI, ind(data_6)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_6)$d2dtaMEI, ind(data_6)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_6)$d2dtaMMEI, ind(data_6)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 7
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 7")
points(ind(data_7)$dtaEI, ind(data_7)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_7)$dtaMEI, ind(data_7)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_7)$dtaMMEI, ind(data_7)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_7)$ddtaEI, ind(data_7)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_7)$ddtaMEI, ind(data_7)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_7)$ddtaMMEI, ind(data_7)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_7)$d2dtaEI, ind(data_7)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_7)$d2dtaMEI, ind(data_7)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_7)$d2dtaMMEI, ind(data_7)$d2dtaMMHI, col="blue",pch=20)

# model 1 and 8
plot(ind(data_1)$dtaEI,ind(data_1)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 8")
points(ind(data_8)$dtaEI, ind(data_8)$dtaHI, col="blue",pch=20)
plot(ind(data_1)$dtaMEI, ind(data_1)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_8)$dtaMEI, ind(data_8)$dtaMHI, col="blue",pch=20)
plot(ind(data_1)$dtaMMEI, ind(data_1)$dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_8)$dtaMMEI, ind(data_8)$dtaMMHI, col="blue",pch=20)

plot(ind(data_1)$ddtaEI,ind(data_1)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives")
points(ind(data_8)$ddtaEI, ind(data_8)$ddtaHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMEI, ind(data_1)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_8)$ddtaMEI, ind(data_8)$ddtaMHI, col="blue",pch=20)
plot(ind(data_1)$ddtaMMEI, ind(data_1)$ddtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_8)$ddtaMMEI, ind(data_8)$ddtaMMHI, col="blue",pch=20)

plot(ind(data_1)$d2dtaEI,ind(data_1)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives")
points(ind(data_8)$d2dtaEI, ind(data_8)$d2dtaHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMEI, ind(data_1)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10)
points(ind(data_8)$d2dtaMEI, ind(data_8)$d2dtaMHI, col="blue",pch=20)
plot(ind(data_1)$d2dtaMMEI, ind(data_1)$d2dtaMMHI, col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(ind(data_8)$d2dtaMMEI, ind(data_8)$d2dtaMMHI, col="blue",pch=20)

>>>>>>> 076758afa9e323a4357ab880bd89dcb6f610c1db
