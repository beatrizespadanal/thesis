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

test_1_2 <- wilcox.test(auc_1, auc_2)
test_1_2 # not equal! different auc -> differences in magnitude

# root mean square

rms <- function(y) {
  return(sqrt(mean(y^2)))
}

rms_func1 <- apply(curves_func1, 1, rms)
rms_func2 <- apply(curves_func2, 1, rms)

test_rms_1_2 <- wilcox.test(rms_func1, rms_func2)
test_rms_1_2 # not equal! differences in amplitude

# dynamic time warping

model1_curves <- data_2[1:50,]
model2_curves <- data_2[51:100,]

library(dtw)

dtw_distance <- function(curve1,curve2) {
  dtw_res <- dtw(curve1,curve2)
  return(dtw_res$distance)
}

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist2 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist2[i,j] <- dtw_distance(model2_curves[i,], model2_curves[j,])
  }
}

test1_2_dtw <- wilcox.test(dtw_dist1, dtw_dist2)
test1_2_dtw # not reject -> same shape 

###############
# model 1 and 3
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_3s[1:50,]
curves_func3 <- data_3s[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_3 <- apply(curves_func3, 1, function(y) auc(x_values,y))

test_1_3 <- wilcox.test(auc_1, auc_3)
test_1_3 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func3 <- apply(curves_func3, 1, rms)

test_rms_1_3 <- wilcox.test(rms_func1, rms_func3)
test_rms_1_3 # not equal! differences in amplitude

# dynamic time warping

model1_curves <- data_3[1:50,]
model3_curves <- data_3[51:100,]

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist3 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist3[i,j] <- dtw_distance(model3_curves[i,], model3_curves[j,])
  }
}

test1_3_dtw <- wilcox.test(dtw_dist1, dtw_dist3)
test1_3_dtw # not reject -> same shape 

###############
# model 1 and 4
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_4[1:50,]
curves_func4 <- data_4[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_4 <- apply(curves_func4, 1, function(y) auc(x_values,y))

test_1_4 <- wilcox.test(auc_1, auc_4)
test_1_4 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func4 <- apply(curves_func4, 1, rms)

test_rms_1_4 <- wilcox.test(rms_func1, rms_func4)
test_rms_1_4 # not equal! differences in amplitude

# dynamic time warping

model1_curves <- data_4[1:50,]
model4_curves <- data_4[51:100,]

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist4 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist4[i,j] <- dtw_distance(model4_curves[i,], model4_curves[j,])
  }
}

test1_4_dtw <- wilcox.test(dtw_dist1, dtw_dist4)
test1_4_dtw # not reject -> same shape 

###############
# model 1 and 5
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_5[1:50,]
curves_func5 <- data_5[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_5 <- apply(curves_func5, 1, function(y) auc(x_values,y))

test_1_5 <- wilcox.test(auc_1, auc_5)
test_1_5 # might be equal! p-value > 0.05 (significance level)

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func5 <- apply(curves_func5, 1, rms)

test_rms_1_5 <- wilcox.test(rms_func1, rms_func5)
test_rms_1_5 # might be equal! p-value > 0.05 (significance level)

# distance metrics

model1_curves <- data_5[1:50,]
model5_curves <- data_5[51:100,]

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist5 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist5[i,j] <- dtw_distance(model5_curves[i,], model5_curves[j,])
  }
}

test1_5_dtw <- wilcox.test(dtw_dist1, dtw_dist5)
test1_5_dtw # reject -> different shape 

###############
# model 1 and 6
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_6[1:50,]
curves_func6 <- data_6[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_6 <- apply(curves_func3, 1, function(y) auc(x_values,y))

test_1_6 <- wilcox.test(auc_1, auc_6)
test_1_6 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func6 <- apply(curves_func6, 1, rms)

test_rms_1_6 <- wilcox.test(rms_func1, rms_func6)
test_rms_1_6 # might be equal! p-value > 0.05 (significance level)

# distance metrics

model1_curves <- data_6[1:50,]
model6_curves <- data_6[51:100,]

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist6 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist6[i,j] <- dtw_distance(model6_curves[i,], model6_curves[j,])
  }
}

test1_6_dtw <- wilcox.test(dtw_dist1, dtw_dist6)
test1_6_dtw # reject -> different shape 

###############
# model 1 and 7
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_7[1:50,]
curves_func7 <- data_7[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_7 <- apply(curves_func7, 1, function(y) auc(x_values,y))

test_1_7 <- wilcox.test(auc_1, auc_7)
test_1_7 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func7 <- apply(curves_func7, 1, rms)

test_rms_1_7 <- wilcox.test(rms_func1, rms_func7)
test_rms_1_7 # Considering 0.05 as the significance level, we can reject the 
# the null hypothesis. So, there are evidences that the two groups differ in 
# amplitude

# distance metrics

model1_curves <- data_7[1:50,]
model7_curves <- data_7[51:100,]

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist7 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist7[i,j] <- dtw_distance(model7_curves[i,], model7_curves[j,])
  }
}

test1_7_dtw <- wilcox.test(dtw_dist1, dtw_dist7)
test1_7_dtw # reject -> different shape 

###############
# model 1 and 8
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_8[1:50,]
curves_func8 <- data_8[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_8 <- apply(curves_func8, 1, function(y) auc(x_values,y))

test_1_8 <- wilcox.test(auc_1, auc_8)
test_1_8 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func8 <- apply(curves_func8, 1, rms)

test_rms_1_8 <- wilcox.test(rms_func1, rms_func8)
test_rms_1_8 # not equal! differences in amplitude

# distance metrics

model1_curves <- data_8[1:50,]
model8_curves <- data_8[51:100,]

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist8 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist8[i,j] <- dtw_distance(model8_curves[i,], model8_curves[j,])
  }
}

test1_8_dtw <- wilcox.test(dtw_dist1, dtw_dist8)
test1_8_dtw # reject -> different shape 

###############
# model 1 and 9
###############

x_values <- seq(0,1,length.out=30)

curves_func1 <- data_9[1:50,]
curves_func9 <- data_9[51:100,]

auc_1 <- apply(curves_func1, 1, function(y) auc(x_values,y))
auc_9 <- apply(curves_func9, 1, function(y) auc(x_values,y))

test_1_9 <- wilcox.test(auc_1, auc_9)
test_1_9 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func9 <- apply(curves_func9, 1, rms)

test_rms_1_9 <- wilcox.test(rms_func1, rms_func9)
test_rms_1_9 # not equal! differences in amplitude

# distance metrics

model1_curves <- data_9[1:50,]
model9_curves <- data_9[51:100,]

dtw_dist1 <- matrix(NA, nrow=50, ncol=50)
dtw_dist9 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist1[i,j] <- dtw_distance(model1_curves[i,], model1_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist9[i,j] <- dtw_distance(model9_curves[i,], model9_curves[j,])
  }
}

test1_9_dtw <- wilcox.test(dtw_dist1, dtw_dist9)
test1_9_dtw # reject -> diff shape 

#################
# model 10 and 11
#################

x_values <- seq(0,1,length.out=150)

curves_func10 <- data_11[1:50,]
curves_func11 <- data_11[51:100,]

auc_10 <- apply(curves_func10, 1, function(y) auc(x_values,y))
auc_11 <- apply(curves_func11, 1, function(y) auc(x_values,y))

test_10_11 <- wilcox.test(auc_10, auc_11)
test_10_11 # not equal! different auc -> differences in magnitude

# root mean square

rms_func10 <- apply(curves_func10, 1, rms)
rms_func11 <- apply(curves_func11, 1, rms)

test_rms_10_11 <- wilcox.test(rms_func10, rms_func11)
test_rms_10_11 # not equal! differences in amplitude

# distance metrics

model10_curves <- data_11[1:50,]
model11_curves <- data_11[51:100,]

dtw_dist10 <- matrix(NA, nrow=50, ncol=50)
dtw_dist11 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist10[i,j] <- dtw_distance(model10_curves[i,], model10_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist11[i,j] <- dtw_distance(model11_curves[i,], model11_curves[j,])
  }
}

test10_11_dtw <- wilcox.test(dtw_dist10, dtw_dist11)
test10_11_dtw # not reject -> same shape 

#################
# model 10 and 12
#################

x_values <- seq(0,1,length.out=150)

curves_func10 <- data_12[1:50,]
curves_func12 <- data_12[51:100,]

auc_10 <- apply(curves_func10, 1, function(y) auc(x_values,y))
auc_12 <- apply(curves_func12, 1, function(y) auc(x_values,y))

test_10_12 <- wilcox.test(auc_10, auc_12)
test_10_12 # might be equal! p-value > 0.05 (significance level)

# root mean square

rms_func10 <- apply(curves_func10, 1, rms)
rms_func12 <- apply(curves_func12, 1, rms)

test_rms_10_12 <- wilcox.test(rms_func10, rms_func12)
test_rms_10_12 # Taking the significance level at 0.05, we can reject the 
# null hypothesis. This means that, there is evidence to conclude that the
# two groups differ in amplitude. 

# distance metrics

model10_curves <- data_12[1:50,]
model12_curves <- data_12[51:100,]

dtw_dist10 <- matrix(NA, nrow=50, ncol=50)
dtw_dist12 <- matrix(NA, nrow=50, ncol=50)

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist10[i,j] <- dtw_distance(model10_curves[i,], model10_curves[j,])
  }
}

for(i in 1:50){
  for (j in 1:50) {
    dtw_dist12[i,j] <- dtw_distance(model12_curves[i,], model12_curves[j,])
  }
}

test10_12_dtw <- wilcox.test(dtw_dist10, dtw_dist12)
test10_12_dtw # reject -> diff shape 

#######################
# Berkeley Growth study
#######################

library(fda)

# Area under the curve

ages <- growth$age
curves_boys <- growth$hgtm
curves_girls <- growth$hgtf

auc_boys <- apply(curves_boys, 2, function(y) auc(ages,y))
auc_girls <- apply(curves_girls, 2, function(y) auc(ages,y))

test_auc <- wilcox.test(auc_boys, auc_girls)
test_auc # Considering a significance level of 5%, we can conclude that there are
# evidences that the two groups differ in magnitude

# root mean square

rms_boys <- apply(curves_boys, 2, rms)
rms_girls <- apply(curves_girls, 2, rms)

test_rms <- wilcox.test(rms_boys,rms_girls)
test_rms # For a significance level of 5%, there are evidences that the two groups
#differ in amplitude

# Dynamic time warping  

curves_boys <- t(growth$hgtm)
curves_girls <- t(growth$hgtf)
dtw_boys <- matrix(NA, nrow=39, ncol=39)
dtw_girls <- matrix(NA, nrow=54, ncol=54)

for(i in 1:39){
  for (j in 1:39) {
    dtw_boys[i,j] <- dtw_distance(curves_boys[i,], curves_boys[j,])
  }
}

for(i in 1:54){
  for (j in 1:54) {
    dtw_girls[i,j] <- dtw_distance(curves_girls[i,], curves_girls[j,])
  }
}

test_growth_dtw <- wilcox.test(dtw_boys, dtw_girls)
test_growth_dtw # reject -> diff shape 

##################
# Canadian Weather
##################

## daily temperature

regions_list <- list(
  region1 = as.matrix(CanadianWeather$dailyAv[,1:15,1]),
  region2 = as.matrix(cbind(CanadianWeather$dailyAv[,16:24,1],
                            CanadianWeather$dailyAv[,30:32,1])),
  region3 = as.matrix(CanadianWeather$dailyAv[,25:29,1]),
  region4 = as.matrix(CanadianWeather$dailyAv[,33:35,1])
)

# Updated function to compute DTW distances
compute_dtw_distances <- function(data1, data2) {
  num_curves1 <- ncol(data1)
  num_curves2 <- ncol(data2)
  
  dist_matrix <- matrix(NA, nrow = num_curves1, ncol = num_curves2)
  
  for (i in 1:num_curves1) {
    for (j in 1:num_curves2) {
      alignment <- tryCatch({
        dtw(data1[, i], data2[, j])$distance
      }, error = function(e) {
        NA
      })
      dist_matrix[i, j] <- alignment
    }
  }
  
  return(mean(dist_matrix, na.rm = TRUE))
}

# Compute DTW distances between regions
dtw_distances <- matrix(NA, nrow = 4, ncol = 4)
for (i in 1:4) {
  for (j in i:4) {
    if (i != j) {
      dtw_distances[i, j] <- compute_dtw_distances(regions_list[[i]], regions_list[[j]])
      dtw_distances[j, i] <- dtw_distances[i, j]
    }
  }
}

print(dtw_distances)

library(reshape2)

combined_data <- data.frame(Temperature = numeric(), Region = factor())

for (region_name in names(regions_list)) {
  # Extract the data matrix for the current region
  region_data <- regions_list[[region_name]]
  
  # Convert matrix to long format: each row is a temperature reading
  region_long <- melt(region_data, varnames = c("Day", "Curve"), value.name = "Temperature")
  
  # Add region information
  region_long$Region <- region_name
  
  # Combine with the overall data frame
  combined_data <- rbind(combined_data, region_long)
}


kruskal_test_result <- kruskal.test(Temperature ~ Region, data = combined_data)
kruskal_test_result # for a significance level of 5%, we reject the null hypothesis
# meaning that there are significant differences in the shape of the four groups.

# post hoc 
pairwise_results <- pairwise.wilcox.test(combined_data$Temperature, combined_data$Region, p.adjust.method = "bonferroni")
pairwise_results # all of them differ from eachother 

## daily precipitation 

regions_list_prec <- list(
  region1 = as.matrix(CanadianWeather$dailyAv[,1:15,2]),
  region2 = as.matrix(cbind(CanadianWeather$dailyAv[,16:24,2],
                            CanadianWeather$dailyAv[,30:32,2])),
  region3 = as.matrix(CanadianWeather$dailyAv[,25:29,2]),
  region4 = as.matrix(CanadianWeather$dailyAv[,33:35,2])
)

dtw_distances <- matrix(NA, nrow = 4, ncol = 4)
for (i in 1:4) {
  for (j in i:4) {
    if (i != j) {
      dtw_distances[i, j] <- compute_dtw_distances(regions_list_prec[[i]], 
                                                   regions_list_prec[[j]])
      dtw_distances[j, i] <- dtw_distances[i, j]
    }
  }
}

print(dtw_distances)

combined_data <- data.frame(Temperature = numeric(), Region = factor())

for (region_name in names(regions_list_prec)) {
  # Extract the data matrix for the current region
  region_data <- regions_list_prec[[region_name]]
  
  region_long <- melt(region_data, varnames = c("Day", "Curve"), 
                      value.name = "Temperature")
  
  region_long$Region <- region_name
  
  combined_data <- rbind(combined_data, region_long)
}


kruskal_test_result <- kruskal.test(Temperature ~ Region, data = combined_data)
kruskal_test_result # for a significance level of 5%, we reject the null hypothesis
# meaning that there are significant differences in the shape of the four groups.

# post hoc 
pairwise_results <- pairwise.wilcox.test(combined_data$Temperature, combined_data$Region, p.adjust.method = "bonferroni")
pairwise_results # all of them differ from each other 

