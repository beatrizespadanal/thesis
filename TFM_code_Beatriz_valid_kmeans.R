#######################################
### TFM Beatriz Espadanal Gonçalves ### 
#######################################
library("ehymet")

#######################################
### VALIDATION - K means clustering####
#######################################

#############################
# Model 1 ###################
#############################
# K means clustering with euclidean distance

# original
valid(true_labels1, kmeans1$kmeans_euclidean_EIMEI$cluster)
# 0.7600 0.6352 0.6315
valid(true_labels1, kmeans1$kmeans_euclidean_HIMHI$cluster)
# 0.7700   0.6421   0.6422 
valid(true_labels1, kmeans1$kmeans_euclidean_EIMMEI$cluster)
# 0.7200   0.6196   0.5927 
valid(true_labels1, kmeans1$kmeans_euclidean_HIMMHI$cluster)
#  0.7600   0.6421   0.6315
valid(true_labels1, kmeans1$kmeans_euclidean_MMEIMMHI$cluster)
#  0.7800   0.6524   0.6533 
valid(true_labels1, kmeans1$kmeans_euclidean_MEIMMEI$cluster)
#  0.7200   0.6196   0.5927 
valid(true_labels1, kmeans1$kmeans_euclidean_MHIMMHI$cluster)
#  0.7600   0.6421   0.6315 

# first derivative
valid(true_labels1, kmeans1d$kmeans_euclidean_EIMEI$cluster)
# 0.5100   0.4950   0.4952 
valid(true_labels1, kmeans1d$kmeans_euclidean_HIMHI$cluster)
# 0.5100   0.4950   0.4952 
valid(true_labels1, kmeans1d$kmeans_euclidean_EIMMEI$cluster)
# 0.5000   0.5283   0.4949 
valid(true_labels1, kmeans1d$kmeans_euclidean_HIMMHI$cluster)
# 0.5000   0.5965   0.4949 
valid(true_labels1, kmeans1d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000   0.5283   0.4949 
valid(true_labels1, kmeans1d$kmeans_euclidean_MEIMMEI$cluster)
# 0.5000   0.5283   0.4949 
valid(true_labels1, kmeans1d$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000   0.5965   0.4949 


# second derivative
valid(true_labels1, kmeans1dd$kmeans_euclidean_EIMEI$cluster)
# 0.5000   0.4898   0.4949 
valid(true_labels1, kmeans1dd$kmeans_euclidean_HIMHI$cluster)
# 0.5000   0.4898   0.4949 
valid(true_labels1, kmeans1dd$kmeans_euclidean_EIMMEI$cluster)
# 0.5000   0.5357   0.4949 
valid(true_labels1, kmeans1dd$kmeans_euclidean_HIMMHI$cluster)
# 0.5000   0.5780   0.4949 
valid(true_labels1, kmeans1dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000   0.5602   0.4949 
valid(true_labels1, kmeans1dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.5000   0.5357   0.4949 
valid(true_labels1, kmeans1dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000   0.5780   0.4949 

# k means clustering with mahalanobis distance

# original 
valid(true_labels1, kmeans1$kmeans_mahalanobis_EIMEI$cluster)
# 0.5900 0.6324 0.5113
valid(true_labels1, kmeans1$kmeans_mahalanobis_HIMHI$cluster)
# 0.5800 0.6345 0.5079
valid(true_labels1, kmeans1$kmeans_mahalanobis_EIMMEI$cluster)
# 0.5900 0.6324 0.5113
valid(true_labels1, kmeans1$kmeans_mahalanobis_HIMMHI$cluster)
# 0.5900 0.6324 0.5113
valid(true_labels1, kmeans1$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.7500 0.6236 0.6212
valid(true_labels1, kmeans1$kmeans_mahalanobis_MHIMMHI$cluster)
# 0.6800 0.6248 0.5604
valid(true_labels1, kmeans1$kmeans_mahalanobis_MMEIMMHI$cluster)
# 0.7100 0.6273 0.5840

# cant validate the first and second derivatives of the mahalanobis distance as 
# it's not possible to compute them.

#############################
# Model 2 ###################
#############################
# k means clustering with euclidean distance

#original 
valid(true_labels2, kmeans2$kmeans_euclidean_EIMEI$cluster)
# 0.8100 0.6875 0.6891
valid(true_labels2, kmeans2$kmeans_euclidean_EIMMEI$cluster)
# 0.7600 0.6462 0.6315
valid(true_labels2, kmeans2$kmeans_euclidean_HIMHI$cluster)
# 0.8300 0.7135 0.7149
valid(true_labels2, kmeans2$kmeans_euclidean_HIMMHI$cluster)
# 0.7300 0.6310 0.6018
valid(true_labels2, kmeans2$kmeans_euclidean_MEIMMEI$cluster)
# 0.7600 0.6462 0.6315
valid(true_labels2, kmeans2$kmeans_euclidean_MHIMMHI$cluster)
# 0.7300 0.6310 0.6018
valid(true_labels2, kmeans2$kmeans_euclidean_MMEIMMHI$cluster)
# 0.8300 0.7126 0.7149

# first derivative
valid(true_labels2, kmeans2d$kmeans_euclidean_EIMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels2, kmeans2d$kmeans_euclidean_HIMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels2, kmeans2d$kmeans_euclidean_EIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels2, kmeans2d$kmeans_euclidean_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, kmeans2d$kmeans_euclidean_MEIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels2, kmeans2d$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, kmeans2d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000 0.5516 0.4949

# second derivative
valid(true_labels2, kmeans2dd$kmeans_euclidean_EIMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, kmeans2dd$kmeans_euclidean_EIMMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, kmeans2dd$kmeans_euclidean_HIMHI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, kmeans2dd$kmeans_euclidean_HIMMHI$cluster)
# 0.5000 0.49q5 0.4949
valid(true_labels2, kmeans2dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, kmeans2dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels2, kmeans2dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000 0.5214 0.4949

# k means clustering with mahalanobis distance

valid(true_labels2, kmeans2$kmeans_mahalanobis_EIMEI$cluster)
# 0.8300 0.7126 0.7149
valid(true_labels2, kmeans2$kmeans_mahalanobis_EIMMEI$cluster)
# 0.6600 0.6295 0.5467
valid(true_labels2, kmeans2$kmeans_mahalanobis_HIMHI$cluster)
# 0.6600 0.6295 0.5467
valid(true_labels2, kmeans2$kmeans_mahalanobis_HIMMHI$cluster)
# 0.6600 0.6295 0.5467
valid(true_labels2, kmeans2$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.6100 0.6295 0.5194
valid(true_labels2, kmeans2$kmeans_mahalanobis_MHIMMHI$cluster)
# 0.7200 0.6003 0.5927
valid(true_labels2, kmeans2$kmeans_mahalanobis_MMEIMMHI$cluster)
# 0.5500 0.5867 0.5000


#############################
# Model 3 ###################
#############################
# k means clustering with euclidean distance

# original 
valid(true_labels3, kmeans3$kmeans_euclidean_EIMEI$cluster)
# 0.9500 0.9032 0.9040
valid(true_labels3, kmeans3$kmeans_euclidean_HIMHI$cluster)
# 0.9400 0.8857 0.8861
valid(true_labels3, kmeans3$kmeans_euclidean_EIMMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels3, kmeans3$kmeans_euclidean_HIMMHI$cluster)
# 0.9000 0.8200 0.8182
valid(true_labels3, kmeans3$kmeans_euclidean_MMEIMMHI$cluster)
# 0.9600 0.9217 0.9224
valid(true_labels3, kmeans3$kmeans_euclidean_MEIMMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels3, kmeans3$kmeans_euclidean_MHIMMHI$cluster)
# 0.9300 0.8200 0.8182

# first derivative
valid(true_labels3, kmeans3d$kmeans_euclidean_EIMEI$cluster)
# 0.5200 0.5159 0.4958
valid(true_labels3, kmeans3d$kmeans_euclidean_HIMHI$cluster)
# 0.5200 0.5159 0.4958
valid(true_labels3, kmeans3d$kmeans_euclidean_EIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels3, kmeans3d$kmeans_euclidean_HIMMHI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels3, kmeans3d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels3, kmeans3d$kmeans_euclidean_MEIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels3, kmeans3d$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000 0.5044 0.49494

# second derivative
valid(true_labels3, kmeans3dd$kmeans_euclidean_EIMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, kmeans3dd$kmeans_euclidean_HIMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, kmeans3dd$kmeans_euclidean_EIMMEI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels3, kmeans3dd$kmeans_euclidean_HIMMHI$cluster)
# 0.5000 0.6343 0.4949
valid(true_labels3, kmeans3dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, kmeans3dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels3, kmeans3dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000 0.6343 0.4949

# k means clustering with mahalanobis distance
valid(true_labels3, kmeans3$kmeans_mahalanobis_EIMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels3, kmeans3$kmeans_mahalanobis_EIMMEI$cluster)
# 0.7800 0.6813 0.6533
valid(true_labels3, kmeans3$kmeans_mahalanobis_HIMHI$cluster)
# 0.8700 0.7769 0.7715
valid(true_labels3, kmeans3$kmeans_mahalanobis_HIMMHI$cluster)
# 0.8900 0.8050 0.8022
valid(true_labels3, kmeans3$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.9600 0.9219 0.9224

#############################
# Model 4 ###################
#############################

# k means clustering with euclidean distance
# original 
valid(true_labels4, kmeans4$kmeans_euclidean_EIMEI$cluster)
# 0.5400 0.4996 0.4982
valid(true_labels4, kmeans4$kmeans_euclidean_HIMHI$cluster)
# 0.5500 0.4958 0.5000
valid(true_labels4, kmeans4$kmeans_euclidean_EIMMEI$cluster)
# 0.620 0.549 0.524
valid(true_labels4, kmeans4$kmeans_euclidean_HIMMHI$cluster)
# 0.5700 0.5564 0.5048
valid(true_labels4, kmeans4$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels4, kmeans4$kmeans_euclidean_MEIMMEI$cluster)
# 0.620 0.549 0.524
valid(true_labels4, kmeans4$kmeans_euclidean_MHIMMHI$cluster)
# 0.5700 0.5564 0.5048

# first derivative
valid(true_labels4, kmeans4d$kmeans_euclidean_EIMEI$cluster)
# 0.5600 0.5115 0.5022
valid(true_labels4, kmeans4d$kmeans_euclidean_HIMHI$cluster)
# 0.5600 0.5115 0.5022
valid(true_labels4, kmeans4d$kmeans_euclidean_EIMMEI$cluster)
# 0.6500 0.6118 0.5404
valid(true_labels4, kmeans4d$kmeans_euclidean_HIMMHI$cluster)
# 0.5800 0.6160 0.5079
valid(true_labels4, kmeans4d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5700 0.6183 0.5048
valid(true_labels4, kmeans4d$kmeans_euclidean_MEIMMEI$cluster)
# 0.6500 0.6118 0.5404
valid(true_labels4, kmeans4d$kmeans_euclidean_MHIMMHI$cluster)
# 0.5800 0.6160 0.5079

# second derivative
valid(true_labels4, kmeans4dd$kmeans_euclidean_EIMEI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels4, kmeans4dd$kmeans_euclidean_HIMHI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels4, kmeans4dd$kmeans_euclidean_EIMMEI$cluster)
# 0.7700 0.6634 0.6422
valid(true_labels4, kmeans4dd$kmeans_euclidean_HIMMHI$cluster)
# 0.7100 0.6373 0.5840
valid(true_labels4, kmeans4dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.7600 0.6612 0.6315
valid(true_labels4, kmeans4dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.7700 0.6634 0.6422
valid(true_labels4, kmeans4dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.7100 0.6273 0.5840

# k means clustering with mahalanobis distance
valid(true_labels4, kmeans4$kmeans_mahalanobis_EIMEI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels4, kmeans4$kmeans_mahalanobis_EIMMEI$cluster)
# 0.5400 0.6273 0.4982
valid(true_labels4, kmeans4$kmeans_mahalanobis_HIMHI$cluster)
# 0.5300 0.6026 0.4968
valid(true_labels4, kmeans4$kmeans_mahalanobis_HIMMHI$cluster)
# 0.5400 0.5991 0.4982
valid(true_labels4, kmeans4$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.6100 0.6295 0.5194

#############################
# Model 5 ###################
#############################

# original 
valid(true_labels5, kmeans5$kmeans_euclidean_EIMEI$cluster)
# 0.5400 0.4931 0.4982
valid(true_labels5, kmeans5$kmeans_euclidean_HIMHI$cluster)
# 0.5200 0.4910 0.4958
valid(true_labels5, kmeans5$kmeans_euclidean_EIMMEI$cluster)
# 0.6400 0.6381 0.5345
valid(true_labels5, kmeans5$kmeans_euclidean_HIMMHI$cluster)
# 0.7200 0.6453 0.5927
valid(true_labels5, kmeans5$kmeans_euclidean_MMEIMMHI$cluster)
# 0.6400 0.6196 0.5345
valid(true_labels5, kmeans5$kmeans_euclidean_MEIMMEI$cluster)
# 0.6400 0.6281 0.5345
valid(true_labels5, kmeans5$kmeans_euclidean_MHIMMHI$cluster)
# 0.7200 0.6453 0.5927

# first derivative
valid(true_labels5, kmeans5d$kmeans_euclidean_EIMEI$cluster)
# 0.5300 0.5039 0.4968
valid(true_labels5, kmeans5d$kmeans_euclidean_HIMHI$cluster)
# 0.5300 0.5039 0.4968
valid(true_labels5, kmeans5d$kmeans_euclidean_EIMMEI$cluster)
# 0.6200 0.6286 0.5240
valid(true_labels5, kmeans5d$kmeans_euclidean_HIMMHI$cluster)
# 0.7200 0.6453 0.5927
valid(true_labels5, kmeans5d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.6800 0.6327 0.5604
valid(true_labels5, kmeans5d$kmeans_euclidean_MEIMMEI$cluster)
# 0.6200 0.6286 0.5240
valid(true_labels5, kmeans5d$kmeans_euclidean_MHIMMHI$cluster)
# 0.7200 0.6453 0.5927

# second derivative
valid(true_labels5, kmeans5dd$kmeans_euclidean_EIMEI$cluster)
# 0.5100 0.4901 0.4952
valid(true_labels5, kmeans5dd$kmeans_euclidean_HIMHI$cluster)
# 0.5100 0.4901 0.4952
valid(true_labels5, kmeans5dd$kmeans_euclidean_EIMMEI$cluster)
# 0.6800 0.6327 0.5604
valid(true_labels5, kmeans5dd$kmeans_euclidean_HIMMHI$cluster)
# 0.7100 0.6414 0.5840
valid(true_labels5, kmeans5dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.9200 0.8517 0.8513
valid(true_labels5, kmeans5dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.6800 0.6327 0.5604
valid(true_labels5, kmeans5dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.7100 0.6414 0.5840

# k means clustering with mahalanobis distance
valid(true_labels5, kmeans5$kmeans_mahalanobis_EIMEI$cluster)
# 0.5200 0.6536 0.4958
valid(true_labels5, kmeans5$kmeans_mahalanobis_EIMMEI$cluster)
# 0.5100 0.6485 0.4952
valid(true_labels5, kmeans5$kmeans_mahalanobis_HIMHI$cluster)
# 0.5100 0.6577 0.4952
valid(true_labels5, kmeans5$kmeans_mahalanobis_HIMMHI$cluster)
# 0.7300 0.6498 0.6018
valid(true_labels5, kmeans5$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.6300 0.6282 0.5191


#############################
# Model 6 ###################
#############################

# original 
valid(true_labels6, kmeans6$kmeans_euclidean_EIMEI$cluster)
# 0.6000 0.5200 0.5152
valid(true_labels6, kmeans6$kmeans_euclidean_HIMHI$cluster)
# 0.5800 0.5045 0.5079
valid(true_labels6, kmeans6$kmeans_euclidean_EIMMEI$cluster)
# 0.5700 0.5341 0.5048
valid(true_labels6, kmeans6$kmeans_euclidean_HIMMHI$cluster)
# 0.6300 0.6108 0.5291
valid(true_labels6, kmeans6$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5800 0.5093 0.5079
valid(true_labels6, kmeans6$kmeans_euclidean_MEIMMEI$cluster)
# 0.5700 0.5341 0.5048
valid(true_labels6, kmeans6$kmeans_euclidean_MHIMMHI$cluster)
# 0.6300 0.6108 0.5291

# first derivative
valid(true_labels6, kmeans6d$kmeans_euclidean_EIMEI$cluster)
# 0.5200 0.4906 0.4958
valid(true_labels6, kmeans6d$kmeans_euclidean_HIMHI$cluster)
# 0.5200 0.4906 0.4958
valid(true_labels6, kmeans6d$kmeans_euclidean_EIMMEI$cluster)
# 0.6000 0.5542 0.5152
valid(true_labels6, kmeans6d$kmeans_euclidean_HIMMHI$cluster)
# 0.6500 0.6037 0.5404
valid(true_labels6, kmeans6d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5300 0.5140 0.4968
valid(true_labels6, kmeans6d$kmeans_euclidean_MEIMMEI$cluster)
# 0.6000 0.5542 0.5152
valid(true_labels6, kmeans6d$kmeans_euclidean_MHIMMHI$cluster)
# 0.6500 0.6037 0.5404

# second derivative
valid(true_labels6, kmeans6dd$kmeans_euclidean_EIMEI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels6, kmeans6dd$kmeans_euclidean_HIMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels6, kmeans6dd$kmeans_euclidean_EIMMEI$cluster)
# 0.6000 0.5472 0.5152
valid(true_labels6, kmeans6dd$kmeans_euclidean_HIMMHI$cluster)
# 0.6500 0.6118 0.5404
valid(true_labels6, kmeans6dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.6600 0.5902 0.5467
valid(true_labels6, kmeans6dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.6000 0.5472 0.5152
valid(true_labels6, kmeans6dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.6500 0.6118 0.5404

# k means clustering with mahalanobis distance
valid(true_labels6, kmeans6$kmeans_mahalanobis_EIMEI$cluster)
# 0.5100 0.6485 0.4952
valid(true_labels6, kmeans6$kmeans_mahalanobis_EIMMEI$cluster)
# 0.5100 0.6485 0.4952
valid(true_labels6, kmeans6$kmeans_mahalanobis_HIMHI$cluster)
# 0.5100 0.6485 0.4952
valid(true_labels6, kmeans6$kmeans_mahalanobis_HIMMHI$cluster)
# 0.5100 0.6485 0.4952
valid(true_labels6, kmeans6$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.5000 0.6059 0.4949


#############################
# Model 7 ###################
#############################

# original 
valid(true_labels7, kmeans7$kmeans_euclidean_EIMEI$cluster)
# 0.9200 0.8509 0.8513
valid(true_labels7, kmeans7$kmeans_euclidean_HIMHI$cluster)
# 0.9200 0.8503 0.8513
valid(true_labels7, kmeans7$kmeans_euclidean_EIMMEI$cluster)
# 0.9500 0.9036 0.9040
valid(true_labels7, kmeans7$kmeans_euclidean_HIMMHI$cluster)
# 0.9700 0.9407 0.9412
valid(true_labels7, kmeans7$kmeans_euclidean_MMEIMMHI$cluster)
# 0.9900 0.9798 0.9800
valid(true_labels7, kmeans7$kmeans_euclidean_MEIMMEI$cluster)
# 0.9500 0.9036 0.9040
valid(true_labels7, kmeans7$kmeans_euclidean_MHIMMHI$cluster)
# 0.9700 0.9407 0.9412

# first derivative
valid(true_labels7, kmeans7d$kmeans_euclidean_EIMEI$cluster)
# 0.7800 0.6501 0.6533
valid(true_labels7, kmeans7d$kmeans_euclidean_HIMHI$cluster)
# 0.7800 0.6501 0.6533
valid(true_labels7, kmeans7d$kmeans_euclidean_EIMMEI$cluster)
# 0.6100 0.6116 0.5194
valid(true_labels7, kmeans7d$kmeans_euclidean_HIMMHI$cluster)
# 0.6000 0.5406 0.5152
valid(true_labels7, kmeans7d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.6100 0.6116 0.5194
valid(true_labels7, kmeans7d$kmeans_euclidean_MEIMMEI$cluster)
# 0.6100 0.6116 0.5194
valid(true_labels7, kmeans7d$kmeans_euclidean_MHIMMHI$cluster)
# 0.6000 0.5406 0.5152

# second derivative
valid(true_labels7, kmeans7dd$kmeans_euclidean_EIMEI$cluster)
# 0.5500 0.5117 0.5000
valid(true_labels7, kmeans7dd$kmeans_euclidean_HIMHI$cluster)
# 0.5500 0.5117 0.5000
valid(true_labels7, kmeans7dd$kmeans_euclidean_EIMMEI$cluster)
# 0.5500 0.6426 0.5000
valid(true_labels7, kmeans7dd$kmeans_euclidean_HIMMHI$cluster)
# 0.6700 0.5927 0.5533
valid(true_labels7, kmeans7dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.6200 0.5329 0.5240
valid(true_labels7, kmeans7dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.5500 0.6426 0.5000
valid(true_labels7, kmeans7dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.6700 0.5927 0.5533

# k means clustering with mahalanobis distance
valid(true_labels7, kmeans7$kmeans_mahalanobis_EIMEI$cluster)
# 0.9200 0.8499 0.8513
valid(true_labels7, kmeans7$kmeans_mahalanobis_EIMMEI$cluster)
# 0.9800 0.9600 0.9604
valid(true_labels7, kmeans7$kmeans_mahalanobis_HIMHI$cluster)
# 0.6400 0.6196 0.5345
valid(true_labels7, kmeans7$kmeans_mahalanobis_HIMMHI$cluster)
# 0.9700 0.9407 0.9412
valid(true_labels7, kmeans7$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.9800 0.9600 0.9604

#############################
# Model 8 ###################
#############################

# original 
valid(true_labels8, kmeans8$kmeans_euclidean_EIMEI$cluster)
# 0.9400 0.8850 0.8861
valid(true_labels8, kmeans8$kmeans_euclidean_HIMHI$cluster)
# 0.9400 0.8849 0.8861
valid(true_labels8, kmeans8$kmeans_euclidean_EIMMEI$cluster)
# 0.9600 0.9219 0.9224
valid(true_labels8, kmeans8$kmeans_euclidean_HIMMHI$cluster)
# 1 1 1
valid(true_labels8, kmeans8$kmeans_euclidean_MMEIMMHI$cluster)
# 1 1 1
valid(true_labels8, kmeans8$kmeans_euclidean_MEIMMEI$cluster)
# 0.9600 0.9219 0.9224
valid(true_labels8, kmeans8$kmeans_euclidean_MHIMMHI$cluster)
# 1 1 1

# first derivative
valid(true_labels8, kmeans8d$kmeans_euclidean_EIMEI$cluster)
# 0.7200 0.5916 0.5927
valid(true_labels8, kmeans8d$kmeans_euclidean_HIMHI$cluster)
# 0.7200 0.5916 0.5927
valid(true_labels8, kmeans8d$kmeans_euclidean_EIMMEI$cluster)
# 0.5200 0.4972 0.4958
valid(true_labels8, kmeans8d$kmeans_euclidean_HIMMHI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels8, kmeans8d$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5200 0.4972 0.4958
valid(true_labels8, kmeans8d$kmeans_euclidean_MEIMMEI$cluster)
# 0.5200 0.4972 0.4958
valid(true_labels8, kmeans8d$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000 0.5435 0.4949

# second derivative
valid(true_labels8, kmeans8dd$kmeans_euclidean_EIMEI$cluster)
# 0.5600 0.4971 0.5022
valid(true_labels8, kmeans8dd$kmeans_euclidean_HIMHI$cluster)
# 0.5600 0.4971 0.5022
valid(true_labels8, kmeans8dd$kmeans_euclidean_EIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels8, kmeans8dd$kmeans_euclidean_HIMMHI$cluster)
# 0.5000 0.5516 0.4949
valid(true_labels8, kmeans8dd$kmeans_euclidean_MMEIMMHI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels8, kmeans8dd$kmeans_euclidean_MEIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels8, kmeans8dd$kmeans_euclidean_MHIMMHI$cluster)
# 0.5000 0.5516 0.4949

# k means clustering with mahalanobis distance
valid(true_labels8, kmeans8$kmeans_mahalanobis_EIMEI$cluster)
# 0.6100 0.6295 0.5194
valid(true_labels8, kmeans8$kmeans_mahalanobis_EIMMEI$cluster)
# 0.9600 0.9219 0.9224
valid(true_labels8, kmeans8$kmeans_mahalanobis_HIMHI$cluster)
# 0.9300 0.8672 0.8685
valid(true_labels8, kmeans8$kmeans_mahalanobis_HIMMHI$cluster)
# 1 1 1
valid(true_labels8, kmeans8$kmeans_mahalanobis_MEIMMEI$cluster)
# 0.9700 0.9407 0.9412