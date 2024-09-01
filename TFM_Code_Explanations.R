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

test_1_3 <- wilcox.test(auc_1, auc_3)
test_1_3 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func3 <- apply(curves_func3, 1, rms)

test_rms_1_3 <- wilcox.test(rms_func1, rms_func3)
test_rms_1_3 # not equal! differences in amplitude

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

test_1_4 <- wilcox.test(auc_1, auc_4)
test_1_4 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func4 <- apply(curves_func4, 1, rms)

test_rms_1_4 <- wilcox.test(rms_func1, rms_func4)
test_rms_1_4 # not equal! differences in amplitude

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

test_1_5 <- wilcox.test(auc_1, auc_5)
test_1_5 # might be equal! p-value > 0.05 (significance level)

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func5 <- apply(curves_func5, 1, rms)

test_rms_1_5 <- wilcox.test(rms_func1, rms_func5)
test_rms_1_5 # might be equal! p-value > 0.05 (significance level)

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

test_1_6 <- wilcox.test(auc_1, auc_6)
test_1_6 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func6 <- apply(curves_func6, 1, rms)

test_rms_1_6 <- wilcox.test(rms_func1, rms_func6)
test_rms_1_6 # might be equal! p-value > 0.05 (significance level)

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

test_1_8 <- wilcox.test(auc_1, auc_8)
test_1_8 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func8 <- apply(curves_func8, 1, rms)

test_rms_1_8 <- wilcox.test(rms_func1, rms_func8)
test_rms_1_8 # not equal! differences in amplitude

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

test_1_9 <- wilcox.test(auc_1, auc_9)
test_1_9 # not equal! different auc -> differences in magnitude

# root mean square

rms_func1 <- apply(curves_func1, 1, rms)
rms_func9 <- apply(curves_func9, 1, rms)

test_rms_1_9 <- wilcox.test(rms_func1, rms_func9)
test_rms_1_9 # not equal! differences in amplitude

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

test_10_11 <- wilcox.test(auc_10, auc_11)
test_10_11 # not equal! different auc -> differences in magnitude

# root mean square

rms_func10 <- apply(curves_func10, 1, rms)
rms_func11 <- apply(curves_func11, 1, rms)

test_rms_10_11 <- wilcox.test(rms_func10, rms_func11)
test_rms_10_11 # not equal! differences in amplitude

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

distances <- mapply(euclidean_distance, split(curves_func10, row(curves_func10)), 
                    split(curves_func12, row(curves_func12)))
distances # different shape

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

library(dtw)
alignment_g <- dtw(t(growth$hgtm), t(growth$hgtf))
alignment_g$distance #high value -> differences in shape

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

