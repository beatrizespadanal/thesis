# check the magnitude if there are any differences 

# models 1 and 2 

# auc
library(pracma)
x_values <- seq(0,1,length.out=30)

curves_func1 <- data_2[1:50,]
curves_func2 <- data_2[51:100,]

auc <- function(x,y){
  return(trapz(x,y))
}

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_2 <- apply(curves_func2, 1, function(y) auc(x_values,y))
print(auc_1)
print(auc_2)

t_test <- t.test(auc_1, auc_2)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms <- function(y) {
  return(sqrt(mean(y^2)))
}

rms_func1 <- apply(curves_func1, 1, rms)
rms_func2 <- apply(curves_func2, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func2)
t_test_rms # not equal! differences in amplitude

# distance metrics

euclidean_distance <- function(curve1, curve2) {
  return(sqrt(sum((curve1 - curve2)^2)))
}

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func2, row(curves_func2)))
distances # similar in both magnitude and shape

###############
# model 1 and 3
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_3[1:50,]
curves_func3 <- data_3[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_3 <- apply(curves_func3, 1, function(y) auc(x_values,y))


t_test <- t.test(auc_1, auc_3)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func3 <- apply(curves_func3, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func3)
t_test_rms # not equal! differences in amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func3, row(curves_func3)))
distances # similar in both magnitude and shape

###############
# model 1 and 4
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_4[1:50,]
curves_func4 <- data_4[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_4 <- apply(curves_func4, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_1, auc_4)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func4 <- apply(curves_func4, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func4)
t_test_rms # not equal! differences in amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func4, row(curves_func4)))
distances # similar in both magnitude and shape

###############
# model 1 and 5
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_5[1:50,]
curves_func5 <- data_5[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_5 <- apply(curves_func5, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_1, auc_5)
t_test # might be equal! 0 is in the ci magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func5 <- apply(curves_func5, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func5)
t_test_rms # might be equal! 0 is in the ci amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func5, row(curves_func5)))
distances # different in shape

###############
# model 1 and 6
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_6[1:50,]
curves_func6 <- data_6[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_6 <- apply(curves_func3, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_1, auc_6)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func6 <- apply(curves_func6, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func6)
t_test_rms # might be equal! 0 in the ci amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func6, row(curves_func6)))
distances # different in shape

###############
# model 1 and 7
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_7[1:50,]
curves_func7 <- data_7[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_7 <- apply(curves_func7, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_1, auc_7)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func7 <- apply(curves_func7, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func7)
t_test_rms # not equal! differences in amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func7, row(curves_func7)))
distances # similar in both magnitude and shape

###############
# model 1 and 8
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_8[1:50,]
curves_func8 <- data_8[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_8 <- apply(curves_func8, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_1, auc_8)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func8 <- apply(curves_func8, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func8)
t_test_rms # not equal! differences in amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func8, row(curves_func8)))
distances # similar in both magnitude and shape

###############
# model 1 and 9
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_9[1:50,]
curves_func9 <- data_9[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_9 <- apply(curves_func9, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_1, auc_9)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func9 <- apply(curves_func9, 1, rms)
t_test_rms <- t.test(rms_func1,rms_func9)
t_test_rms # not equal! differences in amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func1, row(curves_func1)), 
                    split(curves_func9, row(curves_func9)))
distances # similar in both magnitude and shape

#################
# model 10 and 11
#################

x_values <- seq(0,1,length.out=150)

curves_func10 <- data_11[1:50,]
curves_func11 <- data_11[51:100,]

auc_10 <- apply(curves_func10, 1, function(y) auc(x_values,y))
auc_11 <- apply(curves_func11, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_10, auc_11)
t_test # not equal! different auc -> differences in magnitude

# root mean square

rms_func10 <- apply(curves_func10, 1, rms)
rms_func11 <- apply(curves_func11, 1, rms)
t_test_rms <- t.test(rms_func10,rms_func11)
t_test_rms # not equal! differences in amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func10, row(curves_func10)), 
                    split(curves_func11, row(curves_func11)))
distances # similar in both magnitude and shape

#################
# model 10 and 12
#################

x_values <- seq(0,1,length.out=150)

curves_func10 <- data_12[1:50,]
curves_func12 <- data_12[51:100,]

auc_10 <- apply(curves_func10, 1, function(y) auc(x_values,y))
auc_12 <- apply(curves_func12, 1, function(y) auc(x_values,y))

t_test <- t.test(auc_10, auc_12)
t_test # might be equal! 0 in ci magnitude

# root mean square

rms_func10 <- apply(curves_func10, 1, rms)
rms_func12 <- apply(curves_func12, 1, rms)
t_test_rms <- t.test(rms_func10,rms_func12)
t_test_rms # not equal! differences in amplitude

# distance metrics

distances <- mapply(euclidean_distance, split(curves_func10, row(curves_func10)), 
                    split(curves_func12, row(curves_func12)))
distances # different shape
