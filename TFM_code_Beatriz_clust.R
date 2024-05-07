#######################################
### TFM Beatriz Espadanal Gonçalves ### 
#######################################
library("ehymet")

#######################################
### CLUSTERING ########################
#######################################

# NOTE: Add the replicate() function to every model 
#and clustering function so that I can do the 50 
#simulations of the clustering after this i can 
#compute the validations and i should also compare
#the results using either the mean or boxplots. 

#############################
# Hierarchical Clustering
#############################

# Model 1 - original 
index_data1 <- data.frame(HI = hi_data_1, EI = ei_data_1, MEI = mei_data_1, 
                          MHI = mhi_data_1, MMEI = mmei_data_1, 
                          MMHI = mmhi_data_1)

vars1 <- c("EI", "MEI")
vars2 <- c("HI", "MHI")
vars3 <- c("EI", "MMEI")
vars4 <- c("HI", "MMHI")
vars5 <- c("MMEI", "MMHI")
vars6 <- c("MEI", "MMEI")
vars7 <- c("MHI", "MMHI")

list_vars_hierarch <- list(vars1,vars2,vars3,vars4,vars5,vars6,vars7)

results1 <- replicate(50,clustInd_hierarch(index_data1, list_vars_hierarch,
                  method_list = c("single", "complete", "average", "centroid",
                                                           "ward.D2"),
                  dist_list = c("euclidean", "manhattan")))

# first derivative
index_data1_d <- data.frame(dHI = hi_ddat_1, dEI = ei_ddat_1, dMEI = mei_ddat_1,
                            dMHI = mhi_ddat_1, dMMEI = mmei_ddat_1, 
                            dMMHI = mmhi_ddat_1) 
varsd1 <- c("dEI", "dMEI")
varsd2 <- c("dHI", "dMHI")
varsd3 <- c("dEI", "dMMEI")
varsd4 <- c("dHI", "dMMHI")
varsd5 <- c("dMMEI", "dMMHI")
varsd6 <- c("dMEI", "dMMEI")
varsd7 <- c("dMHI", "dMMHI")
list_vars_hierarch_d <- list(varsd1,varsd2,varsd3,varsd4,varsd5,varsd6,varsd7)

results1_d <- replicate(50,clustInd_hierarch(index_data1_d, list_vars_hierarch_d,
                             method_list = c("single", "complete", "average", "centroid",
                                             "ward.D2"),
                             dist_list = c("euclidean", "manhattan")))

# second derivative
index_data1_d2 <- data.frame(d2HI = hi_d2dat_1, d2EI = ei_d2dat_1, d2MEI = mei_d2dat_1,
                            d2MHI = mhi_d2dat_1, d2MMEI = mmei_d2dat_1, 
                            d2MMHI = mmhi_d2dat_1) 
varsdd1 <- c("d2EI", "d2MEI")
varsdd2 <- c("d2HI", "d2MHI")
varsdd3 <- c("d2EI", "d2MMEI")
varsdd4 <- c("d2HI", "d2MMHI")
varsdd5 <- c("d2MMEI", "d2MMHI")
varsdd6 <- c("d2MEI", "d2MMEI")
varsdd7 <- c("d2MHI", "d2MMHI")
list_vars_hierarch_d2 <- list(varsdd1,varsdd2,varsdd3,varsdd4,varsdd5,varsdd6,varsdd7)

results1_d2 <- replicate(50,clustInd_hierarch(index_data1_d2, list_vars_hierarch_d2,
                               method_list = c("single", "complete", "average", "centroid",
                                               "ward.D2"),
                               dist_list = c("euclidean", "manhattan")))

# Model 2 - original 
index_data2 <- data.frame(HI = hi_data_2, EI = ei_data_2, MEI = mei_data_2, 
                          MHI = mhi_data_2, MMEI = mmei_data_2, 
                          MMHI = mmhi_data_2)

results2 <- replicate(50,clustInd_hierarch(index_data2, list_vars_hierarch,
                             method_list = c("single", "complete", "average", "centroid",
                                             "ward.D2"),
                             dist_list = c("euclidean", "manhattan")))


# first derivative
index_data2_d <- data.frame(dHI = hi_ddat_2, dEI = ei_ddat_2, dMEI = mei_ddat_2,
                            dMHI = mhi_ddat_2, dMMEI = mmei_ddat_2, 
                            dMMHI = mmhi_ddat_2) 

results2_d <- replicate(50,clustInd_hierarch(index_data2_d, list_vars_hierarch_d,
                               method_list = c("single", "complete", "average", "centroid",
                                               "ward.D2"),
                               dist_list = c("euclidean", "manhattan")))

# second derivative
index_data2_d2 <- data.frame(d2HI = hi_d2dat_2, d2EI = ei_d2dat_2, d2MEI = mei_d2dat_2,
                             d2MHI = mhi_d2dat_2, d2MMEI = mmei_d2dat_2, 
                             d2MMHI = mmhi_d2dat_2) 

results2_d2 <- replicate(50,clustInd_hierarch(index_data2_d2, list_vars_hierarch_d2,
                                method_list = c("single", "complete", "average", "centroid",
                                                "ward.D2"),
                                dist_list = c("euclidean", "manhattan")))

# Model 3 - original 
index_data3 <- data.frame(HI = hi_data_3, EI = ei_data_3, MEI = mei_data_3, 
                          MHI = mhi_data_3, MMEI = mmei_data_3, 
                          MMHI = mmhi_data_3)

results3 <- replicate(50,clustInd_hierarch(index_data3, list_vars_hierarch,
                             method_list = c("single", "complete", "average", "centroid",
                                             "ward.D2"),
                             dist_list = c("euclidean", "manhattan")))


# first derivative
index_data3_d <- data.frame(dHI = hi_ddat_3, dEI = ei_ddat_3, dMEI = mei_ddat_3,
                            dMHI = mhi_ddat_3, dMMEI = mmei_ddat_3, 
                            dMMHI = mmhi_ddat_3) 

results3_d <- replicate(50,clustInd_hierarch(index_data3_d, list_vars_hierarch_d,
                               method_list = c("single", "complete", "average", "centroid",
                                               "ward.D2"),
                               dist_list = c("euclidean", "manhattan")))

# second derivative
index_data3_d2 <- data.frame(d2HI = hi_d2dat_3, d2EI = ei_d2dat_3, d2MEI = mei_d2dat_3,
                             d2MHI = mhi_d2dat_3, d2MMEI = mmei_d2dat_3, 
                             d2MMHI = mmhi_d2dat_3) 

results3_d2 <- replicate(50,clustInd_hierarch(index_data3_d2, list_vars_hierarch_d2,
                                method_list = c("single", "complete", "average", "centroid",
                                                "ward.D2"),
                                dist_list = c("euclidean", "manhattan")))

# Model 4 - original 
index_data4 <- data.frame(HI = hi_data_4, EI = ei_data_4, MEI = mei_data_4, 
                          MHI = mhi_data_4, MMEI = mmei_data_4, 
                          MMHI = mmhi_data_4)

results4 <- replicate(50,clustInd_hierarch(index_data4, list_vars_hierarch,
                             method_list = c("single", "complete", "average", "centroid",
                                             "ward.D2"),
                             dist_list = c("euclidean", "manhattan")))


# first derivative
index_data4_d <- data.frame(dHI = hi_ddat_4, dEI = ei_ddat_4, dMEI = mei_ddat_4,
                            dMHI = mhi_ddat_4, dMMEI = mmei_ddat_4, 
                            dMMHI = mmhi_ddat_4) 

results4_d <- replicate(50,clustInd_hierarch(index_data4_d, list_vars_hierarch_d,
                               method_list = c("single", "complete", "average", "centroid",
                                               "ward.D2"),
                               dist_list = c("euclidean", "manhattan")))

# second derivative
index_data4_d2 <- data.frame(d2HI = hi_d2dat_4, d2EI = ei_d2dat_4, d2MEI = mei_d2dat_4,
                             d2MHI = mhi_d2dat_4, d2MMEI = mmei_d2dat_4, 
                             d2MMHI = mmhi_d2dat_4)

results4_d2 <- replicate(50,clustInd_hierarch(index_data4_d2, list_vars_hierarch_d2,
                                method_list = c("single", "complete", "average", "centroid",
                                                "ward.D2"),
                                dist_list = c("euclidean", "manhattan")))

# Model 5 - original 
index_data5 <- data.frame(HI = hi_data_5, EI = ei_data_5, MEI = mei_data_5, 
                          MHI = mhi_data_5, MMEI = mmei_data_5, 
                          MMHI = mmhi_data_5)

results5 <- replicate(50,clustInd_hierarch(index_data5, list_vars_hierarch,
                             method_list = c("single", "complete", "average", "centroid",
                                             "ward.D2"),
                             dist_list = c("euclidean", "manhattan")))

# first derivative
index_data5_d <- data.frame(dHI = hi_ddat_5, dEI = ei_ddat_5, dMEI = mei_ddat_5,
                            dMHI = mhi_ddat_5, dMMEI = mmei_ddat_5, 
                            dMMHI = mmhi_ddat_5) 

results5_d <- replicate(50,clustInd_hierarch(index_data5_d, list_vars_hierarch_d,
                               method_list = c("single", "complete", "average", "centroid",
                                               "ward.D2"),
                               dist_list = c("euclidean", "manhattan")))

# second derivative
index_data5_d2 <- data.frame(d2HI = hi_d2dat_5, d2EI = ei_d2dat_5, d2MEI = mei_d2dat_5,
                             d2MHI = mhi_d2dat_5, d2MMEI = mmei_d2dat_5, 
                             d2MMHI = mmhi_d2dat_5) 

results5_d2 <- replicate(50,clustInd_hierarch(index_data5_d2, list_vars_hierarch_d2,
                                method_list = c("single", "complete", "average", "centroid",
                                                "ward.D2"),
                                dist_list = c("euclidean", "manhattan")))

# Model 6 - original 
index_data6 <- data.frame(HI = hi_data_6, EI = ei_data_6, MEI = mei_data_6, 
                          MHI = mhi_data_6, MMEI = mmei_data_6, 
                          MMHI = mmhi_data_6)

results6 <- replicate(50,clustInd_hierarch(index_data6, list_vars_hierarch,
                              method_list = c("single", "complete", "average", "centroid",
                                              "ward.D2"),
                              dist_list = c("euclidean", "manhattan")))

# first derivative
index_data6_d <- data.frame(dHI = hi_ddat_6, dEI = ei_ddat_6, dMEI = mei_ddat_6,
                            dMHI = mhi_ddat_6, dMMEI = mmei_ddat_6, 
                            dMMHI = mmhi_ddat_6) 

results6_d <- replicate(50,clustInd_hierarch(index_data6_d, list_vars_hierarch_d,
                                method_list = c("single", "complete", "average", "centroid",
                                                "ward.D2"),
                                dist_list = c("euclidean", "manhattan")))

# second derivative
index_data6_d2 <- data.frame(d2HI = hi_d2dat_6, d2EI = ei_d2dat_6, d2MEI = mei_d2dat_6,
                             d2MHI = mhi_d2dat_6, d2MMEI = mmei_d2dat_6, 
                             d2MMHI = mmhi_d2dat_6) 

results6_d2 <- replicate(50,clustInd_hierarch(index_data6_d2, list_vars_hierarch_d2,
                                 method_list = c("single", "complete", "average", "centroid",
                                                 "ward.D2"),
                                 dist_list = c("euclidean", "manhattan")))

# Model 7 - original 
index_data7 <- data.frame(HI = hi_data_7, EI = ei_data_7, MEI = mei_data_7, 
                          MHI = mhi_data_7, MMEI = mmei_data_7, 
                          MMHI = mmhi_data_7)

results7 <- replicate(50,clustInd_hierarch(index_data7, list_vars_hierarch,
                              method_list = c("single", "complete", "average", "centroid",
                                              "ward.D2"),
                              dist_list = c("euclidean", "manhattan")))


# first derivative
index_data7_d <- data.frame(dHI = hi_ddat_7, dEI = ei_ddat_7, dMEI = mei_ddat_7,
                            dMHI = mhi_ddat_7, dMMEI = mmei_ddat_7, 
                            dMMHI = mmhi_ddat_7)

results7_d <- replicate(50,clustInd_hierarch(index_data7_d, list_vars_hierarch_d,
                                method_list = c("single", "complete", "average", "centroid",
                                                "ward.D2"),
                                dist_list = c("euclidean", "manhattan")))

# second derivative
index_data7_d2 <- data.frame(d2HI = hi_d2dat_7, d2EI = ei_d2dat_7, d2MEI = mei_d2dat_7,
                             d2MHI = mhi_d2dat_7, d2MMEI = mmei_d2dat_7, 
                             d2MMHI = mmhi_d2dat_7) 

results7_d2 <- replicate(50,clustInd_hierarch(index_data7_d2, list_vars_hierarch_d2,
                                 method_list = c("single", "complete", "average", "centroid",
                                                 "ward.D2"),
                                 dist_list = c("euclidean", "manhattan")))

# Model 8 - original 
index_data8 <- data.frame(HI = hi_data_8, EI = ei_data_8, MEI = mei_data_8, 
                          MHI = mhi_data_8, MMEI = mmei_data_8, 
                          MMHI = mmhi_data_8)

results8 <- replicate(50,clustInd_hierarch(index_data8, list_vars_hierarch,
                              method_list = c("single", "complete", "average", "centroid",
                                              "ward.D2"),
                              dist_list = c("euclidean", "manhattan")))

# first derivative
index_data8_d <- data.frame(dHI = hi_ddat_8, dEI = ei_ddat_8, dMEI = mei_ddat_8,
                            dMHI = mhi_ddat_8, dMMEI = mmei_ddat_8, 
                            dMMHI = mmhi_ddat_8) 

results8_d <- replicate(50,clustInd_hierarch(index_data8_d, list_vars_hierarch_d,
                                method_list = c("single", "complete", "average", "centroid",
                                                "ward.D2"),
                                dist_list = c("euclidean", "manhattan")))

# second derivative
index_data8_d2 <- data.frame(d2HI = hi_d2dat_8, d2EI = ei_d2dat_8, d2MEI = mei_d2dat_8,
                             d2MHI = mhi_d2dat_8, d2MMEI = mmei_d2dat_8, 
                             d2MMHI = mmhi_d2dat_8) 

results8_d2 <- replicate(50,clustInd_hierarch(index_data8_d2, list_vars_hierarch_d2,
                                 method_list = c("single", "complete", "average", "centroid",
                                                 "ward.D2"),
                                 dist_list = c("euclidean", "manhattan")))

#############################
# K means clustering
#############################

# Model 1 - original
list_vars_kmeans <- list(vars1, vars2, vars3, vars4, vars5, vars6, vars7)
kmeans1 <- replicate(50,clustInd_kmeans(index_data1, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
list_varsd_kmeans <- list(varsd1, varsd2, varsd3, varsd4, varsd5, varsd6, varsd7)
kmeans1d <- replicate(50,clustInd_kmeans(index_data1_d, list_varsd_kmeans, dist_list = "euclidean")) # it doesn't work with the mahalanobis distance

# second derivative
list_varsdd_kmeans<- list(varsdd1, varsdd2, varsdd3, varsdd4, varsdd5, varsdd6, varsdd7)
kmeans1dd <- replicate(50,clustInd_kmeans(index_data1_d2, list_varsdd_kmeans, dist_list = "euclidean"))

# Model 2 - original
kmeans2 <- replicate(50,clustInd_kmeans(index_data2, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
kmeans2d <- replicate(50,clustInd_kmeans(index_data2_d, list_varsd_kmeans, dist_list = "euclidean"))


# second derivative
kmeans2dd <- replicate(50,clustInd_kmeans(index_data2_d2, list_varsdd_kmeans, dist_list = "euclidean"))

# Model 3 - original
kmeans3 <- replicate(50,clustInd_kmeans(index_data3, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
kmeans3d <- replicate(50,clustInd_kmeans(index_data3_d, list_varsd_kmeans, dist_list = "euclidean"))


# second derivative
kmeans3dd <- replicate(50,clustInd_kmeans(index_data3_d2, list_varsdd_kmeans, dist_list = "euclidean"))

# Model 4 - original
kmeans4 <- replicate(50,clustInd_kmeans(index_data4, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
kmeans4d <- replicate(50,clustInd_kmeans(index_data4_d, list_varsd_kmeans, dist_list = "euclidean")) 

# second derivative
kmeans4dd <- replicate(50,clustInd_kmeans(index_data4_d2, list_varsdd_kmeans, dist_list = "euclidean"))

# Model 5 - original
kmeans5 <- replicate(50,clustInd_kmeans(index_data5, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
kmeans5d <- replicate(50,clustInd_kmeans(index_data5_d, list_varsd_kmeans, dist_list = "euclidean")) 

# second derivative
kmeans5dd <- replicate(50,clustInd_kmeans(index_data5_d2, list_varsdd_kmeans, dist_list = "euclidean"))

# Model 6 - original
kmeans6 <- replicate(50,clustInd_kmeans(index_data6, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
kmeans6d <- replicate(50,clustInd_kmeans(index_data6_d, list_varsd_kmeans, dist_list = "euclidean")) 

# second derivative
kmeans6dd <- replicate(50,clustInd_kmeans(index_data6_d2, list_varsdd_kmeans, dist_list = "euclidean"))

# Model 7 - original
kmeans7 <- replicate(50,clustInd_kmeans(index_data7, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
kmeans7d <- replicate(50,clustInd_kmeans(index_data7_d, list_varsd_kmeans, dist_list = "euclidean")) 

# second derivative
kmeans7dd <- replicate(50,clustInd_kmeans(index_data7_d2, list_varsdd_kmeans, dist_list = "euclidean"))

# Model 8 - original
kmeans8 <- replicate(50,clustInd_kmeans(index_data8, list_vars_kmeans,dist_list = c("euclidean", "mahalanobis")))

# first derivative
kmeans8d <- replicate(50,clustInd_kmeans(index_data8_d, list_varsd_kmeans, dist_list = "euclidean")) 

# second derivative
kmeans8dd <- replicate(50,clustInd_kmeans(index_data8_d2, list_varsdd_kmeans, dist_list = "euclidean"))

#############################
# kernel kmeans clustering
#############################

# model 1 - original 
list_vars_kkmeans <- list(vars1, vars2, vars3, vars4, vars5, vars6, vars7)
kkmeans1 <- replicate(50,clustInd_kkmeans(index_data1, list_vars_kkmeans))

# first derivative
list_varsd_kkmeans <- list(varsd1, varsd2, varsd3, varsd4, varsd5, varsd6, varsd7)
kkmeans1d <- replicate(50,clustInd_kkmeans(index_data1_d, list_varsd_kkmeans))

# second derivative
list_varsdd_kkmeans<- list(varsdd1, varsdd2, varsdd3, varsdd4, varsdd5, varsdd6, varsdd7)
kkmeans1dd <- replicate(50,clustInd_kkmeans(index_data1_d2, list_varsdd_kkmeans))

# model 2 - original 
kkmeans2 <- replicate(50,clustInd_kkmeans(index_data2, list_vars_kkmeans))

# first derivative
kkmeans2d <- replicate(50,clustInd_kkmeans(index_data2_d, list_varsd_kkmeans))

# second derivative
kkmeans2dd <- replicate(50,clustInd_kkmeans(index_data2_d2, list_varsdd_kkmeans))

# model 3 - original 
kkmeans3 <- replicate(50,clustInd_kkmeans(index_data3, list_vars_kkmeans))

# first derivative
kkmeans3d <- replicate(50,clustInd_kkmeans(index_data3_d, list_varsd_kkmeans))

# second derivative
kkmeans3dd <- replicate(50,clustInd_kkmeans(index_data3_d2, list_varsdd_kkmeans))

# model 4 - original 
kkmeans4 <- replicate(50,clustInd_kkmeans(index_data4, list_vars_kkmeans))

# first derivative
kkmeans4d <- replicate(50,clustInd_kkmeans(index_data4_d, list_varsd_kkmeans))

# second derivative
kkmeans4dd <- replicate(50,clustInd_kkmeans(index_data4_d2, list_varsdd_kkmeans))

# model 5 - original 
kkmeans5 <- replicate(50,clustInd_kkmeans(index_data5, list_vars_kkmeans))

# first derivative
kkmeans5d <- replicate(50,clustInd_kkmeans(index_data5_d, list_varsd_kkmeans))

# second derivative
kkmeans5dd <- replicate(50,clustInd_kkmeans(index_data5_d2, list_varsdd_kkmeans))

# model 6 - original 
kkmeans6 <- replicate(50,clustInd_kkmeans(index_data6, list_vars_kkmeans))

# first derivative
kkmeans6d <- replicate(50,clustInd_kkmeans(index_data6_d, list_varsd_kkmeans))

# second derivative
kkmeans6dd <- replicate(50,clustInd_kkmeans(index_data6_d2, list_varsdd_kkmeans))

# model 7 - original 
kkmeans7 <- replicate(50,clustInd_kkmeans(index_data7, list_vars_kkmeans))

# first derivative
kkmeans7d <- replicate(50,clustInd_kkmeans(index_data7_d, list_varsd_kkmeans))

# second derivative
kkmeans7dd <- replicate(50,clustInd_kkmeans(index_data7_d2, list_varsdd_kkmeans))

# model 8 - original 
kkmeans8 <- replicate(50,clustInd_kkmeans(index_data8, list_vars_kkmeans))

# first derivative
kkmeans8d <- replicate(50,clustInd_kkmeans(index_data8_d, list_varsd_kkmeans))

# second derivative
kkmeans8dd <- replicate(50,clustInd_kkmeans(index_data8_d2, list_varsdd_kkmeans))

#############################
# support vector clustering
#############################

# model 1 - original 
list_vars_svc <- list(vars1, vars2, vars3, vars4, vars5, vars6, vars7)
svc1 <- replicate(50,clustInd_svc(index_data1, list_vars_svc))

# first derivative
list_varsd_svc <- list(varsd1, varsd2, varsd3, varsd4, varsd5, varsd6, varsd7)
svc1d <- replicate(50,clustInd_svc(index_data1_d, list_varsd_svc))

# second derivative
list_varsdd_svc<- list(varsdd1, varsdd2, varsdd3, varsdd4, varsdd5, varsdd6, varsdd7)
svc1dd <- replicate(50,clustInd_svc(index_data1_d2, list_varsdd_svc))

# model 2 - original 
svc2 <- replicate(50,clustInd_svc(index_data2, list_vars_svc))

# first derivative
svc2d <- replicate(50,clustInd_svc(index_data2_d, list_varsd_svc))

# second derivative
svc2dd <- replicate(50,clustInd_svc(index_data2_d2, list_varsdd_svc))

# model 3 - original 
svc3 <- replicate(50,clustInd_svc(index_data3, list_vars_svc))

# first derivative
svc3d <- replicate(50,clustInd_svc(index_data3_d, list_varsd_svc))

# second derivative
svc3dd <- replicate(50,clustInd_svc(index_data3_d2, list_varsdd_svc))

# model 4 - original 
svc4 <- replicate(50,clustInd_svc(index_data4, list_vars_svc))

# first derivative
svc4d <- replicate(50,clustInd_svc(index_data4_d, list_varsd_svc))

# second derivative
svc4dd <- replicate(50,clustInd_svc(index_data4_d2, list_varsdd_svc))

# model 5 - original 
svc5 <- replicate(50,clustInd_svc(index_data5, list_vars_svc))

# first derivative
svc5d <- replicate(50,clustInd_svc(index_data5_d, list_varsd_svc))

# second derivative
svc5dd <- replicate(50,clustInd_svc(index_data5_d2, list_varsdd_svc))

# model 6 - original 
svc6 <- replicate(50,clustInd_svc(index_data6, list_vars_svc))

# first derivative
svc6d <- replicate(50,clustInd_svc(index_data6_d, list_varsd_svc))

# second derivative
svc6dd <- replicate(50,clustInd_svc(index_data6_d2, list_varsdd_svc))

# model 7 - original 
svc7 <- replicate(50,clustInd_svc(index_data7, list_vars_svc))

# first derivative
svc7d <- replicate(50,clustInd_svc(index_data7_d, list_varsd_svc))

# second derivative
svc7dd <- replicate(50,clustInd_svc(index_data7_d2, list_varsdd_svc))

# model 8 - original 
svc8 <- replicate(50,clustInd_svc(index_data8, list_vars_svc))

# first derivative
svc8d <- replicate(50,clustInd_svc(index_data8_d, list_varsd_svc))

# second derivative
svc8dd <- replicate(50,clustInd_svc(index_data8_d2, list_varsdd_svc))

#############################
# spectral clustering
#############################

#### I'm having an error here!!! 

# model 1

list_vars_spc <- list(vars1, vars2, vars3, vars4, vars5, vars6, vars7)

spc1 <- replicate(50,clustInd_spc(index_data1,list_vars_spc))

# list_vars_spc_d <- list(varsd1, varsd2, varsd3, varsd4, varsd5, varsd6, varsd7)
# spc1_d <- clustInd_spc(index_data1_d, list_vars_spc_d)
# list_vars_spc_dd <- list(varsdd1, varsdd2, varsdd3, varsdd4, varsdd5, varsdd6, varsdd7)
# spc1_dd <- clustInd_spc(index_data1_d2, list_vars_spc_dd)

spc2 <- replicate(50,clustInd_spc(index_data2, list_vars_svc))

spc3 <- replicate(50,clustInd_spc(index_data3, list_vars_spc))

spc4 <- replicate(50,clustInd_spc(index_data4, list_vars_spc))

spc5 <- replicate(50,clustInd_spc(index_data5, list_vars_spc))

spc6 <- replicate(50,clustInd_spc(index_data6, list_vars_spc))

spc7 <- replicate(50,clustInd_spc(index_data7, list_vars_spc))

spc8 <- replicate(50,clustInd_spc(index_data8, list_vars_spc))

#############################
# EHyClus method for clustering
#############################

# Model 1
vars_list_ehyclus <- list(EI = ind_data_1$dtaEI, HI = ind_data_1$dtaHI,
                          MEI = ind_data_1$dtaMEI, MHI = ind_data_1$dtaMHI,
                          MMEI = ind_data_1$dtaMMEI, MMHI = ind_data_1$dtaMMHI)

EHyClus <- function(curves, vars_list, nbasis = 30,  n_clusters = 2, norder = 4,
                    clustering_methods = c("hierarch", "kmeans", "kkmeans", "svc", "spc"),
                    indices            = c("EI", "HI", "MEI", "MHI","MMEI", "MMHI"),
                    l_method_hierarch  = c("single", "complete", "average", "centroid", "ward.D2"),
                    l_dist_hierarch    = c("euclidean", "manhattan"),
                    l_dist_kmeans      = c("euclidean", "mahalanobis"),
                    l_kernel           = c("rbfdot", "polydot"),
                    l_method_svc       = c("kmeans", "kernkmeans"),
                    grid_ll = 0, grid_ul = 1,
                    true_labels = NULL, colapse = FALSE, verbose = FALSE, num_cores = 1, ...) {
  
  # vars_list TIENE QUE SER LIST !!!!!
  if (!is.list(vars_list)) {
    stop("input 'vars_list' must be a list.", call. = FALSE)
  }
  
  # list that maps each clustering method to its corresponding function
  default_clustering_methods <- list(
    "hierarch" = clustInd_hierarch,
    "kmeans"   = clustInd_kmeans,
    "kkmeans"  = clustInd_kkmeans,
    "svc"      = clustInd_svc,
    "spc"      = clustInd_spc
  )
  
  # Constants definition
  INDICES            <- c("EI", "HI", "MEI", "MHI", "MMEI", "MMHI")
  METHOD_HIERARCH    <- c("single", "complete", "average", "centroid", "ward.D2")
  DIST_HIERARCH      <- c("euclidean", "manhattan")
  DIST_KMEANS        <- c("euclidean", "mahalanobis")
  KERNEL             <- c("rbfdot", "polydot")
  METHOD_SVC         <- c("kmeans", "kernkmeans")
  CLUSTERING_METHODS <- names(default_clustering_methods)
  
  check_list_parameter(clustering_methods, CLUSTERING_METHODS, "clustering_method")
  check_list_parameter(indices, INDICES, "indices")
  check_list_parameter(l_method_hierarch, METHOD_HIERARCH, "l_method_hierarch")
  check_list_parameter(l_dist_hierarch, DIST_HIERARCH, "l_dist_hierarch")
  check_list_parameter(l_dist_kmeans, DIST_KMEANS, "l_dist_kmeans")
  check_list_parameter(l_kernel, KERNEL, "l_kernel")
  check_list_parameter(l_method_svc, METHOD_SVC, "l_method_svc")
  
  # Generate the dataset with the indexes
  ind_curves <- ind(curves, grid_ll = grid_ll, grid_ul = grid_ul, nbasis, norder, indices)
  
  # common arguments for all the clustering methods that are implemented
  # in the package
  common_clustering_arguments <- list(
    "ind_data"    = ind_curves,
    "vars_list"   = vars_list,
    "n_cluster"   = n_clusters,
    "true_labels" = true_labels,
    "colapse"     = colapse,
    "num_cores"   = num_cores
  )
  
  cluster <- list()
  for (method in clustering_methods) {
    method_args <- switch(method,
                          "hierarch" = append(common_clustering_arguments, list(method_list = l_method_hierarch, dist_list = l_dist_hierarch)),
                          "kmeans"   = append(common_clustering_arguments, list(dist_list   = l_dist_kmeans)),
                          "kkmeans"  = append(common_clustering_arguments, list(kernel_list = l_kernel)),
                          "svc"      = append(common_clustering_arguments, list(method_list = l_method_svc)),
                          "spc"      = append(common_clustering_arguments, list(kernel_list = l_kernel))
    )
    
    cluster[[method]] <- if (verbose) {
      do.call(default_clustering_methods[[method]], method_args)
    } else {
      suppressMessages(quiet(do.call(default_clustering_methods[[method]], method_args)))
    }
  }
  
  if (colapse) {
    metrics <- do.call(rbind, sapply(cluster, "[[", "metrics"))
    result  <- list("cluster" = cluster, "metrics" = metrics)
  } else {
    result <- list("cluster" = cluster)
  }
  
  class(result) <- c("EHyClus", class(result))
  attr(result, "n_clusters") <- n_clusters
  
  result
}

print.EHyClus <- function(x, ...) {
  cat("Clustering methods used:", paste(names(x$cluster), collapse = ", "), "\n")
  cat("Number of clusters:", attr(x, "n_clusters"))
  cat("More and more and more things.........\n")
  cat("......................................")
  
  invisible(x)
}

EHyClus1 <- EHyClus(index_data1, vars_list_ehyclus, nbasis = 6, grid_ll = 0, grid_ul = 1)
