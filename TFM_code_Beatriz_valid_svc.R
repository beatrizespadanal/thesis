### TFM Beatriz Espadanal Gonçalves ### 
#######################################
library("ehymet")

#######################################
### VALIDATION - support vector clustering
#######################################

#############################
# Model 1 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels1, svc1$svc_kmeans_EIMEI$cluster)
# 0.7800 0.6501 0.6533
valid(true_labels1, svc1$svc_kmeans_HIMHI$cluster)
# 0.7700 0.6421 0.6422
valid(true_labels1,svc1$svc_kmeans_EIMMEI$cluster)
# 0.7200 0.6196 0.5927
valid(true_labels1, svc1$svc_kmeans_HIMMHI$cluster)
# 0.7600 0.6421 0.6315
valid(true_labels1, svc1$svc_kmeans_MMEIMMHI$cluster)
# 0.7800 0.6524 0.6533
valid(true_labels1, svc1$svc_kmeans_MEIMMEI$cluster)
# 0.7100 0.6207 0.5840
valid(true_labels1, svc1$svc_kmeans_MHIMMHI$cluster)
# 0.7600 0.6421 0.6315

# first derivative
valid(true_labels1, svc1d$svc_kmeans_EIMEI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels1, svc1d$svc_kmeans_HIMHI$cluster)
# 0.5100 0.4950 0.4952
valid(true_labels1, svc1d$svc_kmeans_EIMMEI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels1, svc1d$svc_kmeans_HIMMHI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels1, svc1d$svc_kmeans_MMEIMMHI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels1, svc1d$svc_kmeans_MEIMMEI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels1, svc1d$svc_kmeans_MHIMMHI$cluster)
# 0.5000 0.5357 0.4949

# second derivative
valid(true_labels1, svc1dd$svc_kmeans_EIMEI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels1, svc1dd$svc_kmeans_HIMHI$cluster)
# 0.5000 0.4898 0.4949
valid(true_labels1, svc1dd$svc_kmeans_EIMMEI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels1, svc1dd$svc_kmeans_HIMMHI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels1, svc1dd$svc_kmeans_MMEIMMHI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels1, svc1dd$svc_kmeans_MEIMMEI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels1, svc1dd$svc_kmeans_MHIMMHI$cluster)
# 0.5000 0.5357 0.4949

# support vector clustering with kernel k means
# original 

valid(true_labels1, svc1$svc_kernkmeans_EIMEI$cluster)
# 0.6500 0.5381 0.5404
valid(true_labels1, svc1$svc_kernkmeans_EIMMEI$cluster)
# 0.7200 0.6003 0.5927
valid(true_labels1, svc1$svc_kernkmeans_HIMHI$cluster)
# 0.5600 0.4988 0.5022
valid(true_labels1, svc1$svc_kernkmeans_HIMMHI$cluster)
# 0.6600 0.5512 0.5467
valid(true_labels1, svc1$svc_kernkmeans_MEIMMEI$cluster)
# 0.7600 0.6326 0.6315
valid(true_labels1, svc1$svc_kernkmeans_MHIMMHI$cluster)
# 0.6100 0.5170 0.5194
valid(true_labels1, svc1$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5400 0.4935 0.4982

# first derivative 
valid(true_labels1, svc1d$svc_kernkmeans_EIMEI$cluster)
# 0.5100 0.4926 0.4952
valid(true_labels1, svc1d$svc_kernkmeans_EIMMEI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels1, svc1d$svc_kernkmeans_HIMHI$cluster)
# 0.5200 0.4923 0.4958
valid(true_labels1, svc1d$svc_kernkmeans_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels1, svc1d$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels1, svc1d$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels1, svc1d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.4915 0.4949

# second derivative
valid(true_labels1, svc1dd$svc_kernkmeans_EIMEI$cluster)
# 0.5100 0.5023 0.4952
valid(true_labels1, svc1dd$svc_kernkmeans_EIMMEI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels1, svc1dd$svc_kernkmeans_HIMHI$cluster)
# 0.5100 0.4909 0.4952 
valid(true_labels1, svc1dd$svc_kernkmeans_HIMMHI$cluster)
# 0.5000 0.4898 0.4949
valid(true_labels1, svc1dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.5044 0.5949
valid(true_labels1, svc1dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4898 0.4949
valid(true_labels1, svc1dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.4915 0.4949

#############################
# Model 2 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels2, svc2$svc_kmeans_EIMEI$cluster)
# 0.8100 0.6875 0.6891
valid(true_labels2, svc2$svc_kmeans_HIMHI$cluster)
# 0.8300 0.7135 0.7149
valid(true_labels2, svc2$svc_kmeans_EIMMEI$cluster)
# 0.7600 0.6462 0.6315
valid(true_labels2, svc2$svc_kmeans_HIMMHI$cluster)
# 0.7300 0.6310 0.6018
valid(true_labels2, svc2$svc_kmeans_MMEIMMHI$cluster)
# 0.8300 0.7126 0.7149
valid(true_labels2, svc2$svc_kmeans_MEIMMEI$cluster)
# 0.7600 0.6462 0.6315
valid(true_labels2, svc2$svc_kmeans_MHIMMHI$cluster)
# 0.8100 0.6935 0.6891

# first derivative
valid(true_labels2, svc2d$svc_kmeans_EIMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels2, svc2d$svc_kmeans_HIMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels2, svc2d$svc_kmeans_EIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels2, svc2d$svc_kmeans_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, svc2d$svc_kmeans_MMEIMMHI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels2, svc2d$svc_kmeans_MEIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels2, svc2d$svc_kmeans_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949

# second derivative
valid(true_labels2, svc2dd$svc_kmeans_EIMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, svc2dd$svc_kmeans_HIMHI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, svc2dd$svc_kmeans_EIMMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, svc2dd$svc_kmeans_HIMMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels2, svc2dd$svc_kmeans_MMEIMMHI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, svc2dd$svc_kmeans_MEIMMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, svc2dd$svc_kmeans_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949

# support vector clustering with kernel k means
# original 

valid(true_labels2, svc2$svc_kernkmeans_EIMEI$cluster)
# 0.7300 0.6043 0.6018
valid(true_labels2, svc2$svc_kernkmeans_EIMMEI$cluster)
# 0.7200 0.5939 0.5927
valid(true_labels2, svc2$svc_kernkmeans_HIMHI$cluster)
# 0.7200 0.5968 0.5927
valid(true_labels2, svc2$svc_kernkmeans_HIMMHI$cluster)
# 0.5400 0.4931 0.4982
valid(true_labels2, svc2$svc_kernkmeans_MEIMMEI$cluster)
# 0.6700 0.5998 0.5533
valid(true_labels2, svc2$svc_kernkmeans_MHIMMHI$cluster)
# 0.8300 0.7135 0.7149
valid(true_labels2, svc2$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5500 0.4958 0.5000

# first derivative 
valid(true_labels2, svc2d$svc_kernkmeans_EIMEI$cluster)
# 0.5100 0.4926 0.4952
valid(true_labels2, svc2d$svc_kernkmeans_EIMMEI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels2, svc2d$svc_kernkmeans_HIMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels2, svc2d$svc_kernkmeans_HIMMHI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels2, svc2d$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels2, svc2d$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, svc2d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.4964 0.4949

# second derivative
valid(true_labels2, svc2dd$svc_kernkmeans_EIMEI$cluster)
# 0.5100 0.4950 0.4952
valid(true_labels2, svc2dd$svc_kernkmeans_EIMMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, svc2dd$svc_kernkmeans_HIMHI$cluster)
# 0.5000 0.4935 0.4949 
valid(true_labels2, svc2dd$svc_kernkmeans_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, svc2dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels2, svc2dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, svc2dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.4902 0.4949

#############################
# Model 3 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels3, svc3$svc_kmeans_EIMEI$cluster)
# 0.9200 0.8517 0.8513
valid(true_labels3, svc3$svc_kmeans_HIMHI$cluster)
# 0.9400 0.8857 0.8861
valid(true_labels3, svc3$svc_kmeans_EIMMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels3, svc3$svc_kmeans_HIMMHI$cluster)
# 0.9000 0.8200 0.8182
valid(true_labels3, svc3$svc_kmeans_MMEIMMHI$cluster)
# 0.9600 0.9217 0.9224
valid(true_labels3, svc3$svc_kmeans_MEIMMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels3, svc3$svc_kmeans_MHIMMHI$cluster)
# 0.9000 0.8200 0.8182

# first derivative
valid(true_labels3, svc3d$svc_kmeans_EIMEI$cluster)
# 0.5200 0.5159 0.4958
valid(true_labels3, svc3d$svc_kmeans_HIMHI$cluster)
# 0.5000 0.5094 0.4949
valid(true_labels3, svc3d$svc_kmeans_EIMMEI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels3, svc3d$svc_kmeans_HIMMHI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels3, svc3d$svc_kmeans_MMEIMMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, svc3d$svc_kmeans_MEIMMEI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels3, svc3d$svc_kmeans_MHIMMHI$cluster)
# 0.5000 0.5044 0.4949

# second derivative
valid(true_labels3, svc3dd$svc_kmeans_EIMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, svc3dd$svc_kmeans_HIMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, svc3dd$svc_kmeans_EIMMEI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels3, svc3dd$svc_kmeans_HIMMHI$cluster)
# 0.5000 0.5516 0.4949
valid(true_labels3, svc3dd$svc_kmeans_MMEIMMHI$cluster)
# 0.5000 0.5516 0.4949
valid(true_labels3, svc3dd$svc_kmeans_MEIMMEI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels3, svc3dd$svc_kmeans_MHIMMHI$cluster)
# 0.5000 0.5516 0.4949

# support vector clustering with kernel k means
# original 

valid(true_labels3, svc3$svc_kernkmeans_EIMEI$cluster)
# 0.8100 0.7075 0.6891
valid(true_labels3, svc3$svc_kernkmeans_EIMMEI$cluster)
# 0.8300 0.7281 0.7149
valid(true_labels3, svc3$svc_kernkmeans_HIMHI$cluster)
# 0.8200 0.7175 0.7018
valid(true_labels3, svc3$svc_kernkmeans_HIMMHI$cluster)
# 0.8200 0.7175 0.7018
valid(true_labels3, svc3$svc_kernkmeans_MEIMMEI$cluster)
# 0.9200 0.8517 0.8513
valid(true_labels3, svc3$svc_kernkmeans_MHIMMHI$cluster)
# 0.6700 0.5561 0.5533
valid(true_labels3, svc3$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5500 0.5222 0.4958

# first derivative 
valid(true_labels3, svc3d$svc_kernkmeans_EIMEI$cluster)
# 0.5500 0.5296 0.5000
valid(true_labels3, svc3d$svc_kernkmeans_EIMMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, svc3d$svc_kernkmeans_HIMHI$cluster)
# 0.5200 0.4923 0.4958
valid(true_labels3, svc3d$svc_kernkmeans_HIMMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, svc3d$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, svc3d$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, svc3d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.5214 0.4949

# second derivative
valid(true_labels3, svc3dd$svc_kernkmeans_EIMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, svc3dd$svc_kernkmeans_EIMMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, svc3dd$svc_kernkmeans_HIMHI$cluster)
# 0.5100 0.4950 0.4952 
valid(true_labels3, svc3dd$svc_kernkmeans_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, svc3dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels3, svc3dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels3, svc3dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.5283 0.4949

#############################
# Model 4 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels4, svc4$svc_kmeans_EIMEI$cluster)
# 0.5200   0.4910 0.4958 
valid(true_labels4, svc4$svc_kmeans_HIMHI$cluster)
# 0.5100   0.4950   0.4952 
valid(true_labels4, svc4$svc_kmeans_EIMMEI$cluster)
# 0.620    0.549    0.524 
valid(true_labels4, svc4$svc_kmeans_HIMMHI$cluster)
# 0.5700   0.5564   0.5048 
valid(true_labels4, svc4$svc_kmeans_MMEIMMHI$cluster)
# 0.5600   0.5008   0.5022 
valid(true_labels4, svc4$svc_kmeans_MEIMMEI$cluster)
# 0.620    0.549    0.524 
valid(true_labels4, svc4$svc_kmeans_MHIMMHI$cluster)
# 0.5700   0.5564   0.5048 

# first derivative
valid(true_labels4, svc4d$svc_kmeans_EIMEI$cluster)
# 0.5600   0.5115   0.5022 
valid(true_labels4, svc4d$svc_kmeans_HIMHI$cluster)
# 0.5600   0.5115   0.5022 
valid(true_labels4, svc4d$svc_kmeans_EIMMEI$cluster)
# 0.6500   0.6118   0.5404 
valid(true_labels4, svc4d$svc_kmeans_HIMMHI$cluster)
# 0.5800   0.6160   0.5079 
valid(true_labels4, svc4d$svc_kmeans_MMEIMMHI$cluster)
# 0.6500   0.6118   0.5404 
valid(true_labels4, svc4d$svc_kmeans_MEIMMEI$cluster)
# 0.6500   0.6118   0.5404 
valid(true_labels4, svc4d$svc_kmeans_MHIMMHI$cluster)
# 0.5800   0.6160   0.5079 


# second derivative
valid(true_labels4, svc4dd$svc_kmeans_EIMEI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels4, svc4dd$svc_kmeans_HIMHI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels4, svc4dd$svc_kmeans_EIMMEI$cluster)
# 0.7700 0.6634 0.6422
valid(true_labels4, svc4dd$svc_kmeans_HIMMHI$cluster)
# 0.7100 0.6273 0.5840
valid(true_labels4, svc4dd$svc_kmeans_MMEIMMHI$cluster)
# 0.7600 0.6612 0.6315
valid(true_labels4, svc4dd$svc_kmeans_MEIMMEI$cluster)
# 0.7700 0.6634 0.6422
valid(true_labels4, svc4dd$svc_kmeans_MHIMMHI$cluster)
# 0.7100 0.6273 0.5840

# support vector clustering with kernel k means
# original 

valid(true_labels4, svc4$svc_kernkmeans_EIMEI$cluster)
# 0.5200 0.5008 0.4958
valid(true_labels4, svc4$svc_kernkmeans_EIMMEI$cluster)
# 0.5300 0.5491 0.4968
valid(true_labels4, svc4$svc_kernkmeans_HIMHI$cluster)
# 0.5600 0.4976 0.5022
valid(true_labels4, svc4$svc_kernkmeans_HIMMHI$cluster)
# 0.5800 0.5065 0.5079
valid(true_labels4, svc4$svc_kernkmeans_MEIMMEI$cluster)
# 0.5300 0.5491 0.4968
valid(true_labels4, svc4$svc_kernkmeans_MHIMMHI$cluster)
# 0.5900 0.5471 0.5113
valid(true_labels4, svc4$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5100 0.4983 0.4952

# first derivative 
valid(true_labels4, svc4d$svc_kernkmeans_EIMEI$cluster)
# 0.5400 0.4996 0.4982
valid(true_labels4, svc4d$svc_kernkmeans_EIMMEI$cluster)
# 0.6300 0.5401 0.5291
valid(true_labels4, svc4d$svc_kernkmeans_HIMHI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels4, svc4d$svc_kernkmeans_HIMMHI$cluster)
# 0.5500 0.5071 0.5000
valid(true_labels4, svc4d$svc_kernkmeans_MEIMMEI$cluster)
# 0.6200 0.5431 0.5240
valid(true_labels4, svc4d$svc_kernkmeans_MHIMMHI$cluster)
# 0.6500 0.5561 0.5404
valid(true_labels4, svc4d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.6600 0.5424 0.5467

# second derivative
valid(true_labels4, svc4dd$svc_kernkmeans_EIMEI$cluster)
# 0.5600 0.5008 0.5022
valid(true_labels4, svc4dd$svc_kernkmeans_EIMMEI$cluster)
# 0.5300 0.5265 0.4968
valid(true_labels4, svc4dd$svc_kernkmeans_HIMHI$cluster)
# 0.5400 0.4968 0.4982 
valid(true_labels4, svc4dd$svc_kernkmeans_HIMMHI$cluster)
# 0.5300 0.4967 0.4968
valid(true_labels4, svc4dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.6700 0.5561 0.5533
valid(true_labels4, svc4dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4898 0.4949
valid(true_labels4, svc4dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.7500 0.6436 0.6212

#############################
# Model 5 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels5, svc5$svc_kmeans_EIMEI$cluster)
# 0.5400 0.4931 0.4982 
valid(true_labels5, svc5$svc_kmeans_HIMHI$cluster)
# 0.5200 0.4910 0.4958 
valid(true_labels5, svc5$svc_kmeans_EIMMEI$cluster)
# 0.6400 0.6281 0.5345
valid(true_labels5, svc5$svc_kmeans_HIMMHI$cluster)
# 0.7200 0.6453 0.5927 
valid(true_labels5, svc5$svc_kmeans_MMEIMMHI$cluster)
# 0.7100 0.6342 0.5840 
valid(true_labels5, svc5$svc_kmeans_MEIMMEI$cluster)
# 0.6400 0.6281 0.5345
valid(true_labels5, svc5$svc_kmeans_MHIMMHI$cluster)
# 0.7200 0.6453   0.5927 

# first derivative
valid(true_labels5, svc5d$svc_kmeans_EIMEI$cluster)
# 0.5300   0.5039   0.4968 
valid(true_labels5, svc5d$svc_kmeans_HIMHI$cluster)
# 0.5300   0.5039   0.4968 
valid(true_labels5, svc5d$svc_kmeans_EIMMEI$cluster)
# 0.6200   0.6286   0.5240 
valid(true_labels5, svc5d$svc_kmeans_HIMMHI$cluster)
# 0.7200   0.6453   0.5927 
valid(true_labels5, svc5d$svc_kmeans_MMEIMMHI$cluster)
# 0.6800   0.6327   0.5604 
valid(true_labels5, svc5d$svc_kmeans_MEIMMEI$cluster)
# 0.6200   0.6286   0.5240 
valid(true_labels5, svc5d$svc_kmeans_MHIMMHI$cluster)
# 0.7200   0.6453   0.5927 


# second derivative
valid(true_labels5, svc5dd$svc_kmeans_EIMEI$cluster)
# 0.5100 0.4901 0.4952
valid(true_labels5, svc5dd$svc_kmeans_HIMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels5, svc5dd$svc_kmeans_EIMMEI$cluster)
# 0.6800 0.6327 0.5604
valid(true_labels5, svc5dd$svc_kmeans_HIMMHI$cluster)
# 0.7100 0.6414 0.5840
valid(true_labels5, svc5dd$svc_kmeans_MMEIMMHI$cluster)
# 0.9200 0.8517 0.8513
valid(true_labels5, svc5dd$svc_kmeans_MEIMMEI$cluster)
# 0.6800 0.6327 0.5604
valid(true_labels5, svc5dd$svc_kmeans_MHIMMHI$cluster)
# 0.7100 0.6414 0.5840

# support vector clustering with kernel k means
# original 

valid(true_labels5, svc5$svc_kernkmeans_EIMEI$cluster)
# 0.6500 0.5403 0.5404
valid(true_labels5, svc5$svc_kernkmeans_EIMMEI$cluster)
# 0.5800 0.5404 0.5079
valid(true_labels5, svc5$svc_kernkmeans_HIMHI$cluster)
# 0.5200 0.4910 0.4958
valid(true_labels5, svc5$svc_kernkmeans_HIMMHI$cluster)
# 0.5900 0.5072 0.5113
valid(true_labels5, svc5$svc_kernkmeans_MEIMMEI$cluster)
# 0.6200 0.5329 0.5240
valid(true_labels5, svc5$svc_kernkmeans_MHIMMHI$cluster)
# 0.5800 0.5128 0.5079
valid(true_labels5, svc5$svc_kernkmeans_MMEIMMHI$cluster)
# 0.9500 0.9036 0.9040

# first derivative 
valid(true_labels5, svc5d$svc_kernkmeans_EIMEI$cluster)
# 0.7200 0.5886 0.5927
valid(true_labels5, svc5d$svc_kernkmeans_EIMMEI$cluster)
# 0.7500 0.6489 0.6212
valid(true_labels5, svc5d$svc_kernkmeans_HIMHI$cluster)
# 0.5100 0.4909 0.4952
valid(true_labels5, svc5d$svc_kernkmeans_HIMMHI$cluster)
# 0.5800 0.5045 0.5079
valid(true_labels5, svc5d$svc_kernkmeans_MEIMMEI$cluster)
# 0.7600 0.6558 0.6315
valid(true_labels5, svc5d$svc_kernkmeans_MHIMMHI$cluster)
# 0.6700 0.5860 0.5533
valid(true_labels5, svc5d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.8600 0.7613 0.7568

# second derivative
valid(true_labels5, svc5dd$svc_kernkmeans_EIMEI$cluster)
# 0.5300 0.4917 0.4968
valid(true_labels5, svc5dd$svc_kernkmeans_EIMMEI$cluster)
# 0.8200 0.7175 0.7018
valid(true_labels5, svc5dd$svc_kernkmeans_HIMHI$cluster)
# 0.6100 0.5307 0.5194 
valid(true_labels5, svc5dd$svc_kernkmeans_HIMMHI$cluster)
# 0.5500 0.5117 0.5000
valid(true_labels5, svc5dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.5100 0.4926 0.4952
valid(true_labels5, svc5dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.6100 0.5262 0.5194
valid(true_labels5, svc5dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.9600 0.9216 0.9224

#############################
# Model 6 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels6, svc6$svc_kmeans_EIMEI$cluster)
# 0.6000   0.5200 0.5152 
valid(true_labels6, svc6$svc_kmeans_HIMHI$cluster)
# 0.6000   0.5200   0.5162 
valid(true_labels6, svc6$svc_kmeans_EIMMEI$cluster)
# 0.5700   0.5341  0.5048 
valid(true_labels6, svc6$svc_kmeans_HIMMHI$cluster)
# 0.6300   0.6108   0.5291 
valid(true_labels6, svc6$svc_kmeans_MMEIMMHI$cluster)
# 0.5800   0.5093   0.5079 
valid(true_labels6, svc6$svc_kmeans_MEIMMEI$cluster)
# 0.5700   0.5341   0.5048 
valid(true_labels6, svc6$svc_kmeans_MHIMMHI$cluster)
# 0.5900   0.5182   0.5113 

# first derivative
valid(true_labels6, svc6d$svc_kmeans_EIMEI$cluster)
# 0.5100   0.5023   0.4952 
valid(true_labels6, svc6d$svc_kmeans_HIMHI$cluster)
# 0.5100   0.5023   0.4952 
valid(true_labels6, svc6d$svc_kmeans_EIMMEI$cluster)
# 0.6000   0.5542   0.5152 
valid(true_labels6, svc6d$svc_kmeans_HIMMHI$cluster)
# 0.6500   0.6037   0.5404 
valid(true_labels6, svc6d$svc_kmeans_MMEIMMHI$cluster)
# 0.5300   0.5140   0.4968 
valid(true_labels6, svc6d$svc_kmeans_MEIMMEI$cluster)
# 0.6000   0.5542   0.5152 
valid(true_labels6, svc6d$svc_kmeans_MHIMMHI$cluster)
# 0.6500   0.6037   0.5404 


# second derivative
valid(true_labels6, svc6dd$svc_kmeans_EIMEI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels6, svc6dd$svc_kmeans_HIMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels6, svc6dd$svc_kmeans_EIMMEI$cluster)
# 0.6000 0.5472 0.5152
valid(true_labels6, svc6dd$svc_kmeans_HIMMHI$cluster)
# 0.6500 0.6118 0.5404
valid(true_labels6, svc6dd$svc_kmeans_MMEIMMHI$cluster)
# 0.6600 0.5902 0.5467
valid(true_labels6, svc6dd$svc_kmeans_MEIMMEI$cluster)
# 0.6000 0.5472 0.5152
valid(true_labels6, svc6dd$svc_kmeans_MHIMMHI$cluster)
# 0.6500 0.6118 0.5404

# support vector clustering with kernel k means
# original 

valid(true_labels6, svc6$svc_kernkmeans_EIMEI$cluster)
# 0.5200 0.4910 0.4958
valid(true_labels6, svc6$svc_kernkmeans_EIMMEI$cluster)
# 0.5300 0.4917 0.4968
valid(true_labels6, svc6$svc_kernkmeans_HIMHI$cluster)
# 0.5800 0.5033 0.5079
valid(true_labels6, svc6$svc_kernkmeans_HIMMHI$cluster)
# 0.6100 0.5307 0.5194
valid(true_labels6, svc6$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels6, svc6$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels6, svc6$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5800 0.5404 0.5079

# first derivative 
valid(true_labels6, svc6d$svc_kernkmeans_EIMEI$cluster)
# 0.5500 0.4999 0.5000
valid(true_labels6, svc6d$svc_kernkmeans_EIMMEI$cluster)
# 0.5900 0.5088 0.5113
valid(true_labels6, svc6d$svc_kernkmeans_HIMHI$cluster)
# 0.5500 0.4958 0.5000
valid(true_labels6, svc6d$svc_kernkmeans_HIMMHI$cluster)
# 0.6100 0.5478 0.5194
valid(true_labels6, svc6d$svc_kernkmeans_MEIMMEI$cluster)
# 0.5900 0.5544 0.5113
valid(true_labels6, svc6d$svc_kernkmeans_MHIMMHI$cluster)
# 0.550 0.523 0.500
valid(true_labels6, svc6d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5200 0.4906 0.4958

# second derivative
valid(true_labels6, svc6dd$svc_kernkmeans_EIMEI$cluster)
# 0.5600 0.5072 0.5022
valid(true_labels6, svc6dd$svc_kernkmeans_EIMMEI$cluster)
# 0.6500 0.5512 0.5404
valid(true_labels6, svc6dd$svc_kernkmeans_HIMHI$cluster)
# 0.6000 0.5118 0.5152 
valid(true_labels6, svc6dd$svc_kernkmeans_HIMMHI$cluster)
# 0.6300 0.5320 0.5291
valid(true_labels6, svc6dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.6800 0.5616 0.5604
valid(true_labels6, svc6dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.5600 0.5008 0.5022
valid(true_labels6, svc6dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.7500 0.6236 0.6212

#############################
# Model 7 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels7, svc7$svc_kmeans_EIMEI$cluster)
# 0.9200   0.8509 0.8513 
valid(true_labels7, svc7$svc_kmeans_HIMHI$cluster)
# 0.9200   0.8503   0.8513 
valid(true_labels7, svc7$svc_kmeans_EIMMEI$cluster)
# 0.9500   0.9036   0.9040 
valid(true_labels7, svc7$svc_kmeans_HIMMHI$cluster)
# 0.9700   0.9407   0.9412 
valid(true_labels7, svc7$svc_kmeans_MMEIMMHI$cluster)
# 0.9900   0.9798   0.9800 
valid(true_labels7, svc7$svc_kmeans_MEIMMEI$cluster)
# 0.9500   0.9036   0.9040 
valid(true_labels7, svc7$svc_kmeans_MHIMMHI$cluster)
# 0.9700   0.9407   0.9412 

# first derivative
valid(true_labels7, svc7d$svc_kmeans_EIMEI$cluster)
# 0.7900   0.6615   0.6648 
valid(true_labels7, svc7d$svc_kmeans_HIMHI$cluster)
# 0.8000   0.6759   0.6768 
valid(true_labels7, svc7d$svc_kmeans_EIMMEI$cluster)
# 0.6100   0.6116   0.5194 
valid(true_labels7, svc7d$svc_kmeans_HIMMHI$cluster)
# 0.6000   0.5406   0.5152 
valid(true_labels7, svc7d$svc_kmeans_MMEIMMHI$cluster)
# 0.5900   0.5088   0.5113 
valid(true_labels7, svc7d$svc_kmeans_MEIMMEI$cluster)
# 0.6100   0.6116   0.5194 
valid(true_labels7, svc7d$svc_kmeans_MHIMMHI$cluster)
# 0.6000   0.5406   0.5152 


# second derivative
valid(true_labels7, svc7dd$svc_kmeans_EIMEI$cluster)
# 0.5500 0.5117 0.5000
valid(true_labels7, svc7dd$svc_kmeans_HIMHI$cluster)
# 0.5500 0.5117 0.5000
valid(true_labels7, svc7dd$svc_kmeans_EIMMEI$cluster)
# 0.5500 0.6426 0.5000
valid(true_labels7, svc7dd$svc_kmeans_HIMMHI$cluster)
# 0.6700 0.5927 0.5533
valid(true_labels7, svc7dd$svc_kmeans_MMEIMMHI$cluster)
# 0.6200 0.5329 0.5240
valid(true_labels7, svc7dd$svc_kmeans_MEIMMEI$cluster)
# 0.5500 0.6426 0.5000
valid(true_labels7, svc7dd$svc_kmeans_MHIMMHI$cluster)
# 0.6700 0.5927 0.5533

# support vector clustering with kernel k means
# original 

valid(true_labels7, svc7$svc_kernkmeans_EIMEI$cluster)
# 0.9100 0.8356 0.8345
valid(true_labels7, svc7$svc_kernkmeans_EIMMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels7, svc7$svc_kernkmeans_HIMHI$cluster)
# 0.6900 0.5740 0.5679
valid(true_labels7, svc7$svc_kernkmeans_HIMMHI$cluster)
# 0.5800 0.5029 0.5079
valid(true_labels7, svc7$svc_kernkmeans_MEIMMEI$cluster)
# 0.9000 0.8200 0.8182
valid(true_labels7, svc7$svc_kernkmeans_MHIMMHI$cluster)
# 0.9400 0.8857 0.8861
valid(true_labels7, svc7$svc_kernkmeans_MMEIMMHI$cluster)
# 0.8400 0.7363 0.7285

# first derivative 
valid(true_labels7, svc7d$svc_kernkmeans_EIMEI$cluster)
# 0.6000 0.5118 0.5152
valid(true_labels7, svc7d$svc_kernkmeans_EIMMEI$cluster)
# 0.5900 0.5280 0.5113
valid(true_labels7, svc7d$svc_kernkmeans_HIMHI$cluster)
# 0.7100 0.5819 0.5840
valid(true_labels7, svc7d$svc_kernkmeans_HIMMHI$cluster)
# 0.5400 0.4947 0.4982
valid(true_labels7, svc7d$svc_kernkmeans_MEIMMEI$cluster)
# 0.6100 0.5478 0.5194
valid(true_labels7, svc7d$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels7, svc7d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.710 0.584 0.584

# second derivative
valid(true_labels7, svc7dd$svc_kernkmeans_EIMEI$cluster)
# 0.5400 0.4935 0.4982
valid(true_labels7, svc7dd$svc_kernkmeans_EIMMEI$cluster)
# 0.5300 0.5086 0.4968
valid(true_labels7, svc7dd$svc_kernkmeans_HIMHI$cluster)
# 0.5700 0.5023 0.5048 
valid(true_labels7, svc7dd$svc_kernkmeans_HIMMHI$cluster)
# 0.6000 0.5200 0.5152
valid(true_labels7, svc7dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels7, svc7dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.5900 0.5544 0.5113
valid(true_labels7, svc7dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5400 0.5386 0.4982

#############################
# Model 8 ###################
#############################

# support vector clustering with k means
# original

valid(true_labels8, svc8$svc_kmeans_EIMEI$cluster)
# 0.9400   0.8850   0.8861 
valid(true_labels8, svc8$svc_kmeans_HIMHI$cluster)
# 0.9400   0.8849   0.8861 
valid(true_labels8, svc8$svc_kmeans_EIMMEI$cluster)
# 0.9600   0.9219   0.9224 
valid(true_labels8, svc8$svc_kmeans_HIMMHI$cluster)
# 1 1 1    
valid(true_labels8, svc8$svc_kmeans_MMEIMMHI$cluster)
# 1 1 1 
valid(true_labels8, svc8$svc_kmeans_MEIMMEI$cluster)
# 0.9600   0.9219   0.9224 
valid(true_labels8, svc8$svc_kmeans_MHIMMHI$cluster)
# 1 1 1

# first derivative
valid(true_labels8, svc8d$svc_kmeans_EIMEI$cluster)
# 0.7200   0.5916   0.5927 
valid(true_labels8, svc8d$svc_kmeans_HIMHI$cluster)
# 0.7200   0.5916   0.5927 
valid(true_labels8, svc8d$svc_kmeans_EIMMEI$cluster)
# 0.5200   0.4972   0.4958 
valid(true_labels8, svc8d$svc_kmeans_HIMMHI$cluster)
# 0.5000   0.6530   0.4949 
valid(true_labels8, svc8d$svc_kmeans_MMEIMMHI$cluster)
# 0.5200   0.4972   0.4958 
valid(true_labels8, svc8d$svc_kmeans_MEIMMEI$cluster)
# 0.5200   0.4972   0.4958 
valid(true_labels8, svc8d$svc_kmeans_MHIMMHI$cluster)
# 0.5000   0.5044   0.4949 


# second derivative
valid(true_labels8, svc8dd$svc_kmeans_EIMEI$cluster)
# 0.5600 0.4971 0.5022
valid(true_labels8, svc8dd$svc_kmeans_HIMHI$cluster)
# 0.5600 0.4971 0.5022
valid(true_labels8, svc8dd$svc_kmeans_EIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels8, svc8dd$svc_kmeans_HIMMHI$cluster)
# 0.5000 0.5516 0.4949
valid(true_labels8, svc8dd$svc_kmeans_MMEIMMHI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels8, svc8dd$svc_kmeans_MEIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels8, svc8dd$svc_kmeans_MHIMMHI$cluster)
# 0.5000 0.5516 0.4949

# support vector clustering with kernel k means
# original 

valid(true_labels8, svc8$svc_kernkmeans_EIMEI$cluster)
# 0.5300 0.5336 0.4968
valid(true_labels8, svc8$svc_kernkmeans_EIMMEI$cluster)
# 1 1 1 
valid(true_labels8, svc8$svc_kernkmeans_HIMHI$cluster)
# 0.8800 0.7906 0.7867
valid(true_labels8, svc8$svc_kernkmeans_HIMMHI$cluster)
# 0.6900 0.5657 0.5679
valid(true_labels8, svc8$svc_kernkmeans_MEIMMEI$cluster)
# 0.6100 0.5170 0.5194
valid(true_labels8, svc8$svc_kernkmeans_MHIMMHI$cluster)
# 0.6100 0.5546 0.5194
valid(true_labels8, svc8$svc_kernkmeans_MMEIMMHI$cluster)
# 1 1 1 

# first derivative 
valid(true_labels8, svc8d$svc_kernkmeans_EIMEI$cluster)
# 0.5500 0.4975 0.5000
valid(true_labels8, svc8d$svc_kernkmeans_EIMMEI$cluster)
# 0.5300 0.4926 0.4968
valid(true_labels8, svc8d$svc_kernkmeans_HIMHI$cluster)
# 0.5500 0.4999 0.5000
valid(true_labels8, svc8d$svc_kernkmeans_HIMMHI$cluster)
# 0.5100 0.5070 0.4952
valid(true_labels8, svc8d$svc_kernkmeans_MEIMMEI$cluster)
# 0.5100 0.4983 0.4952
valid(true_labels8, svc8d$svc_kernkmeans_MHIMMHI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels8, svc8d$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.4898 0.4949

# second derivative
valid(true_labels8, svc8dd$svc_kernkmeans_EIMEI$cluster)
# 0.5100 0.4983 0.4952
valid(true_labels8, svc8dd$svc_kernkmeans_EIMMEI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels8, svc8dd$svc_kernkmeans_HIMHI$cluster)
# 0.5400 0.4931 0.4982 
valid(true_labels8, svc8dd$svc_kernkmeans_HIMMHI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels8, svc8dd$svc_kernkmeans_MEIMMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels8, svc8dd$svc_kernkmeans_MHIMMHI$cluster)
# 0.5100 0.5070 0.4952
valid(true_labels8, svc8dd$svc_kernkmeans_MMEIMMHI$cluster)
# 0.5000 0.4935 0.4949