#######################################
### TFM Beatriz Espadanal Gonçalves ### 
#######################################
library("ehymet")

#######################################
### VALIDATION ########################
#######################################

# here I just have to use the true_labels that I have already defined when I plotted
# the indexes and use the clustering done priorly in the form: type$combi$cluster
# and i do this for all the models and combinations which will be very confusing 
# and as of now my eyes cant look at a computer anymore. 

#############################
# Hierarchical Clustering ###
#############################
# Model 1 ###################
#############################

# original
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels1, results1$hierarch_single_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5200   0.6535   0.4958 
valid(true_labels1, results1$hierarch_single_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5100   0.6577   0.4952 
valid(true_labels1, results1$hierarch_single_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5100   0.6577   0.4952
valid(true_labels1, results1$hierarch_single_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5200   0.6535   0.4958 
valid(true_labels1, results1$hierarch_single_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5200   0.6535   0.4958 
valid(true_labels1, results1$hierarch_single_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5100   0.6577   0.4952 
valid(true_labels1, results1$hierarch_single_euclidean_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5200   0.6535   0.4958 


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels1, results1$hierarch_single_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079 
valid(true_labels1, results1$hierarch_single_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5100   0.6577   0.4952
valid(true_labels1, results1$hierarch_single_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5100   0.6577   0.4952
valid(true_labels1, results1$hierarch_single_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5200   0.6535   0.4958
valid(true_labels1, results1$hierarch_single_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5200   0.6535   0.4958 
valid(true_labels1, results1$hierarch_single_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5100   0.6577   0.4952
valid(true_labels1, results1$hierarch_single_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5200   0.6535   0.4958


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels1, results1$hierarch_complete_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079
valid(true_labels1, results1$hierarch_complete_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.7900   0.6631   0.6648 
valid(true_labels1, results1$hierarch_complete_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079 
valid(true_labels1, results1$hierarch_complete_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7600   0.6384   0.6315 
valid(true_labels1, results1$hierarch_complete_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7100   0.6145   0.5840 
valid(true_labels1, results1$hierarch_complete_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604
valid(true_labels1, results1$hierarch_complete_euclidean_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7600   0.6384   0.6315 


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels1, results1$hierarch_complete_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.6500   0.6201   0.5404
valid(true_labels1, results1$hierarch_complete_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.7500   0.6341   0.6212 
valid(true_labels1, results1$hierarch_complete_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079 
valid(true_labels1, results1$hierarch_complete_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079 
valid(true_labels1, results1$hierarch_complete_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7800   0.6524   0.6533 
valid(true_labels1, results1$hierarch_complete_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_complete_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7300   0.6369   0.6018

# hierarchical clustering with average linkage - euclidean distance
valid(true_labels1, results1$hierarch_average_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079 
valid(true_labels1, results1$hierarch_average_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.6000   0.6308   0.5152 
valid(true_labels1, results1$hierarch_average_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_average_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079 
valid(true_labels1, results1$hierarch_average_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5400   0.6460   0.4982 
valid(true_labels1, results1$hierarch_average_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels1, results1$hierarch_average_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.6600   0.6295   0.5467 
valid(true_labels1, results1$hierarch_average_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.7500   0.6341   0.6212
valid(true_labels1, results1$hierarch_average_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604
valid(true_labels1, results1$hierarch_average_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5800   0.6345   0.5079 
valid(true_labels1, results1$hierarch_average_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7700   0.6421   0.6422
valid(true_labels1, results1$hierarch_average_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_average_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7700   0.6506   0.6422 


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels1, results1$hierarch_ward.D2_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.7400   0.6152   0.6113 
valid(true_labels1, results1$hierarch_ward.D2_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.7500   0.6341   0.6212 
valid(true_labels1, results1$hierarch_ward.D2_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_ward.D2_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7400   0.6426   0.6113 
valid(true_labels1, results1$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7800   0.6524   0.6533
valid(true_labels1, results1$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_ward.D2_euclidean_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7400   0.6426   0.6113 


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels1, results1$hierarch_ward.D2_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.7400   0.6152   0.6113 
valid(true_labels1, results1$hierarch_ward.D2_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.7500   0.6341   0.6212
valid(true_labels1, results1$hierarch_ward.D2_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_ward.D2_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7600   0.6384   0.6315 
valid(true_labels1, results1$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7800   0.6524   0.6533 
valid(true_labels1, results1$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.6800   0.6098   0.5604 
valid(true_labels1, results1$hierarch_ward.D2_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.7700   0.6506   0.6422 


# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels1, results1_d$hierarch_single_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949 
valid(true_labels1, results1_d$hierarch_single_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949 
valid(true_labels1, results1_d$hierarch_single_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949
valid(true_labels1, results1_d$hierarch_single_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949 
valid(true_labels1, results1_d$hierarch_single_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6530   0.4949 
valid(true_labels1, results1_d$hierarch_single_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949 
valid(true_labels1, results1_d$hierarch_single_euclidean_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949 


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels1, results1_d$hierarch_single_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949 
valid(true_labels1, results1_d$hierarch_single_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949
valid(true_labels1, results1_d$hierarch_single_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949
valid(true_labels1, results1_d$hierarch_single_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949
valid(true_labels1, results1_d$hierarch_single_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6530   0.4949 
valid(true_labels1, results1_d$hierarch_single_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949
valid(true_labels1, results1_d$hierarch_single_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6437   0.4949


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels1, results1_d$hierarch_complete_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_complete_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
#  0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_complete_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6248   0.4949 
valid(true_labels1, results1_d$hierarch_complete_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949 
valid(true_labels1, results1_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_complete_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6348   0.4949
valid(true_labels1, results1_d$hierarch_complete_euclidean_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949 


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels1, results1_d$hierarch_complete_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_complete_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_complete_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6248   0.4949 
valid(true_labels1, results1_d$hierarch_complete_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949 
valid(true_labels1, results1_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_complete_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6248   0.4949
valid(true_labels1, results1_d$hierarch_complete_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949

# hierarchical clustering with average linkage - euclidean distance
valid(true_labels1, results1_d$hierarch_average_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949 
valid(true_labels1, results1_d$hierarch_average_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949 
valid(true_labels1, results1_d$hierarch_average_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5780   0.4949 
valid(true_labels1, results1_d$hierarch_average_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_average_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_average_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5780   0.4949
valid(true_labels1, results1_d$hierarch_average_euclidean_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 

# hierarchical clustering with average linkage - manhattan distance
valid(true_labels1, results1_d$hierarch_average_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949 
valid(true_labels1, results1_d$hierarch_average_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6343   0.4949 
valid(true_labels1, results1_d$hierarch_average_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5780   0.4949 
valid(true_labels1, results1_d$hierarch_average_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_average_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_average_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5780   0.4949 
valid(true_labels1, results1_d$hierarch_average_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels1, results1_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.4898   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.4898   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949 


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels1, results1_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5000   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.4898   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.6059   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.4898   0.4949 
valid(true_labels1, results1_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)
# Purity Fmeasure       RI 
# 0.5000   0.5965   0.4949 


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels1, results1_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels1, results1_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels1, results1_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels1, results1_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels1, results1_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels1, results1_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels1, results1_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)

# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels1, results1_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels1, results1_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


#############################
# Model 2 ###################
#############################

# original
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels2, results2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels2, results2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels2, results2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels2, results2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels2, results2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels2, results2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels2, results2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels2, results2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels2, results2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels2, results2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels2, results2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels2, results2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels2, results2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels2, results2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels2, results2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels2, results2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels2, results2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels2, results2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels2, results2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels2, results2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels2, results2_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels2, results2_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels2, results2_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels2, results2_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels2, results2_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels2, results2_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels2, results2_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels2, results2_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels2, results2_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels2, results2_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels2, results2_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels2, results2_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels2, results2_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels2, results2_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels2, results2_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels2, results2_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels2, results2_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels2, results2_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels2, results2_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels2, results2_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels2, results2_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels2, results2_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels2, results2_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels2, results2_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels2, results2_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


#############################
# Model 3 ###################
#############################

# original 
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels3, results3$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels3, results3$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels3, results3$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels3, results3$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels3, results3$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels3, results3$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels3, results3$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels3, results3$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels3, results3$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels3, results3$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels3, results3$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels3, results3$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_complete_manhattan_MHIMMHI$cluster)

# hierarchical clustering with average linkage - euclidean distance
valid(true_labels3, results3$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels3, results3$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels3, results3$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels3, results3$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels3, results3$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels3, results3$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels3, results3$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels3, results3$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels3, results3_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels3, results3_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels3, results3_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels3, results3_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels3, results3_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels3, results3_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels3, results3_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels3, results3_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels3, results3_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels3, results3_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels3, results3_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels3, results3_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels3, results3_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels3, results3_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels3, results3_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels3, results3_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels3, results3_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


#############################
# Model 4 ###################
#############################

# original 
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels4, results4$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels4, results4$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels4, results4$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels4, results4$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels4, results4$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels4, results4$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels4, results4$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels4, results4$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels4, results4$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels4, results4$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels4, results4$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels4, results4$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_complete_manhattan_MHIMMHI$cluster)

# hierarchical clustering with average linkage - euclidean distance
valid(true_labels4, results4$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels4, results4$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels4, results4$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels4, results4$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels4, results4$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels4, results4$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels4, results4$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels4, results4$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels4, results4_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels4, results4_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels4, results4_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels4, results4_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels4, results4_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels4, results4_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels4, results4_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels4, results4_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels4, results4_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels4, results4_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels4, results4_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels4, results4_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels4, results4_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels4, results4_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels4, results4_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels4, results4_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels4, results4_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


#############################
# Model 5 ###################
#############################

# original 
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels5, results5$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels5, results5$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels5, results5$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels5, results5$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels5, results5$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels5, results5$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels5, results5$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels5, results5$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels5, results5$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels5, results5$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels5, results5$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels5, results5$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_complete_manhattan_MHIMMHI$cluster)

# hierarchical clustering with average linkage - euclidean distance
valid(true_labels5, results5$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels5, results5$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels5, results5$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels5, results5$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels5, results5$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels5, results5$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels5, results5$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels5, results5$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels5, results5_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels5, results5_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels5, results5_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels5, results5_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels5, results5_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels5, results5_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels5, results5_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels5, results5_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels5, results5_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels5, results5_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels5, results5_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels5, results5_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels5, results5_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels5, results5_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels5, results5_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels5, results5_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels5, results5_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


#############################
# Model 6 ###################
#############################

# original 
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels6, results6$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels6, results6$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels6, results6$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels6, results6$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels6, results6$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels6, results6$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels6, results6$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels6, results6$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels6, results6$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels6, results6$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels6, results6$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels6, results6$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels6, results6$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels6, results6$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels6, results6$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels6, results6$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels6, results6$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels6, results6$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels6, results6$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels6, results6$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels6, results6_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels6, results6_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels6, results6_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels6, results6_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels6, results6_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels6, results6_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels6, results6_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels6, results6_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels6, results6_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels6, results6_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels6, results6_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels6, results6_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels6, results6_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels6, results6_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels6, results6_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels6, results6_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels6, results6_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

#############################
# Model 7 ###################
#############################

# original 
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels7, results7$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels7, results7$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels7, results7$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels7, results7$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels7, results7$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels7, results7$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels7, results7$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels7, results7$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels7, results7$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels7, results7$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels7, results7$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels7, results7$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_complete_manhattan_MHIMMHI$cluster)

# hierarchical clustering with average linkage - euclidean distance
valid(true_labels7, results7$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels7, results7$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels7, results7$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels7, results7$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels7, results7$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels7, results7$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels7, results7$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels7, results7$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels7, results7_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels7, results7_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels7, results7_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels7, results7_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels7, results7_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_average_euclidean_MHIMMHI$cluster)

# hierarchical clustering with average linkage - manhattan distance
valid(true_labels7, results7_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels7, results7_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels7, results7_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels7, results7_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels7, results7_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels7, results7_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels7, results7_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels7, results7_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels7, results7_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels7, results7_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels7, results7_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels7, results7_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


#############################
# Model 8 ###################
#############################

# original 
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels8, results8$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels8, results8$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels8, results8$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels8, results8$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels8, results8$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels8, results8$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels8, results8$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels8, results8$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels8, results8$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical cl8ustering with complete linkage - manhattan distance
valid(true_labels8, results8$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels8, results8$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels8, results8$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_complete_manhattan_MHIMMHI$cluster)

# hierarchical clustering with average linkage - euclidean distance
valid(true_labels8, results8$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels8, results8$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels8, results8$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels8, results8$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels8, results8$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels8, results8$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels8, results8$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels8, results8$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8$hierarch_ward.D2_manhattan_MHIMMHI$cluster)

# first derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels8, results8_d$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels8, results8_d$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels8, results8_d$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels8, results8_d$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels8, results8_d$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels8, results8_d$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels8, results8_d$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels8, results8_d$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d$hierarch_ward.D2_manhattan_MHIMMHI$cluster)


# second derivative
# hierarchical clustering with single linkage - euclidean distance 
valid(true_labels8, results8_d2$hierarch_single_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_single_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_single_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_single_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_single_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_single_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_single_euclidean_MHIMMHI$cluster)


# hierarchical clustering with single linkage - manhattan distance 
valid(true_labels8, results8_d2$hierarch_single_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_single_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_single_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_single_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_single_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_single_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_single_manhattan_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - euclidean distance
valid(true_labels8, results8_d2$hierarch_complete_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_euclidean_MHIMMHI$cluster)


# hierarchical clustering with complete linkage - manhattan distance
valid(true_labels8, results8_d2$hierarch_complete_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_complete_manhattan_MHIMMHI$cluster)


# hierarchical clustering with average linkage - euclidean distance
valid(true_labels8, results8_d2$hierarch_average_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_average_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_average_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_average_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_average_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_average_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_average_euclidean_MHIMMHI$cluster)


# hierarchical clustering with average linkage - manhattan distance
valid(true_labels8, results8_d2$hierarch_average_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_average_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_average_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_average_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_average_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_average_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_average_manhattan_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - euclidean distance
valid(true_labels8, results8_d2$hierarch_ward.D2_euclidean_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_euclidean_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_euclidean_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_euclidean_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_euclidean_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_euclidean_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_euclidean_MHIMMHI$cluster)


# hierarchical clustering with ward D2 linkage - manhattan distance
valid(true_labels8, results8_d2$hierarch_ward.D2_manhattan_EIMEI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_manhattan_HIMHI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_manhattan_EIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_manhattan_HIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_manhattan_MMEIMMHI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_manhattan_MEIMMEI$cluster)
valid(true_labels8, results8_d2$hierarch_ward.D2_manhattan_MHIMMHI$cluster)
