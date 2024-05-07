#######################################
### TFM Beatriz Espadanal Gonçalves ### 
#######################################
library("ehymet")

#######################################
### VALIDATION - Kernel k means clustering
#######################################

#############################
# Model 1 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels1, kkmeans1$kkmeans_rbfdot_EIMEI$cluster)
# 0.7400 0.6224 0.6113
valid(true_labels1, kkmeans1$kkmeans_rbfdot_HIMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels1, kkmeans1$kkmeans_rbfdot_EIMMEI$cluster)
# 0.6700 0.5739 0.5533
valid(true_labels1, kkmeans1$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5100 0.4901 0.4952
valid(true_labels1, kkmeans1$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5100 0.4950 0.4952
valid(true_labels1, kkmeans1$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.7300 0.5985 0.6018
valid(true_labels1, kkmeans1$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.7100 0.6032 0.5840

# first derivative
valid(true_labels1, kkmeans1d$kkmeans_rbfdot_EIMEI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels1, kkmeans1d$kkmeans_rbfdot_HIMHI$cluster)
# 0.5200 0.4906 0.4958
valid(true_labels1, kkmeans1d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels1, kkmeans1d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels1, kkmeans1d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5000 0.5094 0.4949
valid(true_labels1, kkmeans1d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels1, kkmeans1d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5000 0.5151 0.4949

# second derivative
valid(true_labels1, kkmeans1dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.5000 0.5094 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949

# kernel k means with polynomial kernel 

# original 
valid(true_labels1, kkmeans1$kkmeans_polydot_EIMEI$cluster)
# 0.7700 0.6404 0.6422
valid(true_labels1, kkmeans1$kkmeans_polydot_EIMMEI$cluster)
# 0.7500 0.6341 0.6212
valid(true_labels1, kkmeans1$kkmeans_polydot_HIMHI$cluster)
# 0.7800 0.6524 0.6533
valid(true_labels1, kkmeans1$kkmeans_polydot_HIMMHI$cluster)
# 0.7600 0.6421 0.6315
valid(true_labels1, kkmeans1$kkmeans_polydot_MMEIMMHI$cluster)
# 0.7800 0.6501 0.6533
valid(true_labels1, kkmeans1$kkmeans_polydot_MEIMMEI$cluster)
# 0.7200 0.6196 0.5927
valid(true_labels1, kkmeans1$kkmeans_polydot_MHIMMHI$cluster)
# 0.7700 0.6506 0.6422

# first derivative
valid(true_labels1, kkmeans1d$kkmeans_polydot_EIMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels1, kkmeans1d$kkmeans_polydot_EIMMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels1, kkmeans1d$kkmeans_polydot_HIMHI$cluster)
# 0.5100 0.4901 0.4952
valid(true_labels1, kkmeans1d$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels1, kkmeans1d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.5516 0.4949
valid(true_labels1, kkmeans1d$kkmeans_polydot_MEIMMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels1, kkmeans1d$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.5357 0.4949

# second derivative 
valid(true_labels1, kkmeans1dd$kkmeans_polydot_EIMEI$cluster)
# 0.5000 0.4898 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_polydot_EIMMEI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_polydot_HIMHI$cluster)
# 0.5000 0.4898 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels1, kkmeans1dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.5357 0.4949


#############################
# Model 2 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels2, kkmeans2$kkmeans_rbfdot_EIMEI$cluster)
# 0.6600 0.5551 0.5467
valid(true_labels2, kkmeans2$kkmeans_rbfdot_HIMHI$cluster)
# 0.6100 0.5154 0.5194
valid(true_labels2, kkmeans2$kkmeans_rbfdot_EIMMEI$cluster)
# 0.6200 0.5227 0.5240
valid(true_labels2, kkmeans2$kkmeans_rbfdot_HIMMHI$cluster)
# 0.6000 0.5290 0.5152
valid(true_labels2, kkmeans2$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.8000 0.6759 0.6768
valid(true_labels2, kkmeans2$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.7000 0.5746 0.5758
valid(true_labels2, kkmeans2$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.8500 0.7398 0.7424

# first derivative
valid(true_labels2, kkmeans2d$kkmeans_rbfdot_EIMEI$cluster)
# 0.5100 0.5321 0.4952
valid(true_labels2, kkmeans2d$kkmeans_rbfdot_HIMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels2, kkmeans2d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5000 0.5094 0.4949
valid(true_labels2, kkmeans2d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels2, kkmeans2d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels2, kkmeans2d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels2, kkmeans2d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5000 0.4964 0.4949

# second derivative
valid(true_labels2, kkmeans2dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5000 0.5283 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949

# kernel k means with polynomial kernel 

# original 
valid(true_labels2, kkmeans2$kkmeans_polydot_EIMEI$cluster)
# 0.8300 0.7121 0.7149
valid(true_labels2, kkmeans2$kkmeans_polydot_EIMMEI$cluster)
# 0.7700 0.6544 0.6422
valid(true_labels2, kkmeans2$kkmeans_polydot_HIMHI$cluster)
# 0.8300 0.7126 0.7149
valid(true_labels2, kkmeans2$kkmeans_polydot_HIMMHI$cluster)
# 0.8100 0.6935 0.6891
valid(true_labels2, kkmeans2$kkmeans_polydot_MMEIMMHI$cluster)
# 0.8300 0.7126 0.7149
valid(true_labels2, kkmeans2$kkmeans_polydot_MEIMMEI$cluster)
# 0.7700 0.6544 0.6422
valid(true_labels2, kkmeans2$kkmeans_polydot_MHIMMHI$cluster)
# 0.8100 0.6935 0.6891

# first derivative
valid(true_labels2, kkmeans2d$kkmeans_polydot_EIMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels2, kkmeans2d$kkmeans_polydot_EIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels2, kkmeans2d$kkmeans_polydot_HIMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels2, kkmeans2d$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, kkmeans2d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels2, kkmeans2d$kkmeans_polydot_MEIMMEI$cluster)
# 0.5000 0.5435 0.4949
valid(true_labels2, kkmeans2d$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949

# second derivative 
valid(true_labels2, kkmeans2dd$kkmeans_polydot_EIMEI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_polydot_EIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_polydot_HIMHI$cluster)
# 0.5000 0.5214 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.5094 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels2, kkmeans2dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.4902 0.4949


#############################
# Model 3 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels3, kkmeans3$kkmeans_rbfdot_EIMEI$cluster)
# 0.8900 0.8050 0.8022
valid(true_labels3, kkmeans3$kkmeans_rbfdot_HIMHI$cluster)
# 0.6800 0.5559 0.5604
valid(true_labels3, kkmeans3$kkmeans_rbfdot_EIMMEI$cluster)
# 0.9100 0.8356 0.8345
valid(true_labels3, kkmeans3$kkmeans_rbfdot_HIMMHI$cluster)
# 0.6000 0.5118 0.5152
valid(true_labels3, kkmeans3$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5400 0.4931 0.4982
valid(true_labels3, kkmeans3$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.9100 0.8356 0.8345
valid(true_labels3, kkmeans3$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.7000 0.5837 0.5758

# first derivative
valid(true_labels3, kkmeans3d$kkmeans_rbfdot_EIMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, kkmeans3d$kkmeans_rbfdot_HIMHI$cluster)
# 0.5200 0.4972 0.4958
valid(true_labels3, kkmeans3d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels3, kkmeans3d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels3, kkmeans3d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5000 0.4898 0.4949
valid(true_labels3, kkmeans3d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5000 0.4964 0.4949
valid(true_labels3, kkmeans3d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5000 0.5044 0.4949

# second derivative
valid(true_labels3, kkmeans3dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.5100 0.4926 0.4952
valid(true_labels3, kkmeans3dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5000 0.5094 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5000 0.4898 0.4949

# kernel k means with polynomial kernel 

# original 
valid(true_labels3, kkmeans3$kkmeans_polydot_EIMEI$cluster)
# 0.9500 0.9032 0.9040
valid(true_labels3, kkmeans3$kkmeans_polydot_EIMMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels3, kkmeans3$kkmeans_polydot_HIMHI$cluster)
# 0.9400 0.8857 0.8861
valid(true_labels3, kkmeans3$kkmeans_polydot_HIMMHI$cluster)
# 0.9000 0.8200 0.8182
valid(true_labels3, kkmeans3$kkmeans_polydot_MMEIMMHI$cluster)
# 0.9600 0.9217 0.9224
valid(true_labels3, kkmeans3$kkmeans_polydot_MEIMMEI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels3, kkmeans3$kkmeans_polydot_MHIMMHI$cluster)
# 0.9000 0.8200 0.8182

# first derivative
valid(true_labels3, kkmeans3d$kkmeans_polydot_EIMEI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels3, kkmeans3d$kkmeans_polydot_EIMMEI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels3, kkmeans3d$kkmeans_polydot_HIMHI$cluster)
# 0.5200 0.5008 0.4958
valid(true_labels3, kkmeans3d$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels3, kkmeans3d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, kkmeans3d$kkmeans_polydot_MEIMMEI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels3, kkmeans3d$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.5044 0.4949

# second derivative 
valid(true_labels3, kkmeans3dd$kkmeans_polydot_EIMEI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_polydot_EIMMEI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_polydot_HIMHI$cluster)
# 0.5000 0.4902 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.5357 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels3, kkmeans3dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.5435 0.4949


#############################
# Model 4 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels4, kkmeans4$kkmeans_rbfdot_EIMEI$cluster)
# 0.5800 0.5033 0.5079
valid(true_labels4, kkmeans4$kkmeans_rbfdot_HIMHI$cluster)
# 0.5700 0.4999 0.5048
valid(true_labels4, kkmeans4$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5600 0.5221 0.5022
valid(true_labels4, kkmeans4$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5800 0.5093 0.5079
valid(true_labels4, kkmeans4$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5500 0.5071 0.5000
valid(true_labels4, kkmeans4$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5100 0.5124 0.4952
valid(true_labels4, kkmeans4$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5900 0.5112 0.5113

# first derivative
valid(true_labels4, kkmeans4d$kkmeans_rbfdot_EIMEI$cluster)
# 0.6200 0.5377 0.5240
valid(true_labels4, kkmeans4d$kkmeans_rbfdot_HIMHI$cluster)
# 0.5100 0.4909 0.4952
valid(true_labels4, kkmeans4d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.6900 0.5678 0.5679
valid(true_labels4, kkmeans4d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.6300 0.5357 0.5291
valid(true_labels4, kkmeans4d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.6100 0.5694 0.5194
valid(true_labels4, kkmeans4d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.7200 0.5939 0.5927
valid(true_labels4, kkmeans4d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.6300 0.5859 0.5291

# second derivative
valid(true_labels4, kkmeans4dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.5300 0.5336 0.4968
valid(true_labels4, kkmeans4dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.5600 0.4988 0.5022
valid(true_labels4, kkmeans4dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.6600 0.5902 0.5467
valid(true_labels4, kkmeans4dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5700 0.5165 0.5048
valid(true_labels4, kkmeans4dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.7600 0.6558 0.6315
valid(true_labels4, kkmeans4dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.6700 0.5596 0.5533
valid(true_labels4, kkmeans4dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5700 0.5165 0.5048

# kernel k means with polynomial kernel 

# original 
valid(true_labels4, kkmeans4$kkmeans_polydot_EIMEI$cluster)
# 0.5200 0.4910 0.4958
valid(true_labels4, kkmeans4$kkmeans_polydot_EIMMEI$cluster)
# 0.6300 0.5452 0.5291
valid(true_labels4, kkmeans4$kkmeans_polydot_HIMHI$cluster)
# 0.5400 0.4935 0.4982
valid(true_labels4, kkmeans4$kkmeans_polydot_HIMMHI$cluster)
# 0.5700 0.5564 0.5048
valid(true_labels4, kkmeans4$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5300 0.4926 0.4968
valid(true_labels4, kkmeans4$kkmeans_polydot_MEIMMEI$cluster)
# 0.6300 0.5452 0.5291
valid(true_labels4, kkmeans4$kkmeans_polydot_MHIMMHI$cluster)
# 0.5900 0.5338 0.5113

# first derivative
valid(true_labels4, kkmeans4d$kkmeans_polydot_EIMEI$cluster)
# 0.5400 0.5032 0.4982
valid(true_labels4, kkmeans4d$kkmeans_polydot_EIMMEI$cluster)
# 0.6500 0.6118 0.5404
valid(true_labels4, kkmeans4d$kkmeans_polydot_HIMHI$cluster)
# 0.5400 0.5032 0.4982
valid(true_labels4, kkmeans4d$kkmeans_polydot_HIMMHI$cluster)
# 0.5800 0.6160 0.5079
valid(true_labels4, kkmeans4d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5700 0.6090 0.5048
valid(true_labels4, kkmeans4d$kkmeans_polydot_MEIMMEI$cluster)
# 0.6500 0.6118 0.5404
valid(true_labels4, kkmeans4d$kkmeans_polydot_MHIMMHI$cluster)
# 0.5800 0.6160 0.5079

# second derivative 
valid(true_labels4, kkmeans4dd$kkmeans_polydot_EIMEI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels4, kkmeans4dd$kkmeans_polydot_EIMMEI$cluster)
# 0.7700 0.6634 0.6422
valid(true_labels4, kkmeans4dd$kkmeans_polydot_HIMHI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels4, kkmeans4dd$kkmeans_polydot_HIMMHI$cluster)
# 0.7100 0.6145 0.5840
valid(true_labels4, kkmeans4dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.7700 0.6684 0.6422
valid(true_labels4, kkmeans4dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.7700 0.6634 0.6422
valid(true_labels4, kkmeans4dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.7100 0.6145 0.5840

#############################
# Model 5 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels5, kkmeans5$kkmeans_rbfdot_EIMEI$cluster)
# 0.5700 0.5007 0.5048
valid(true_labels5, kkmeans5$kkmeans_rbfdot_HIMHI$cluster)
# 0.7000 0.5718 0.5758
valid(true_labels5, kkmeans5$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5700 0.5646 0.5048
valid(true_labels5, kkmeans5$kkmeans_rbfdot_HIMMHI$cluster)
# 0.7200 0.6090 0.5927
valid(true_labels5, kkmeans5$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.6400 0.5432 0.5345
valid(true_labels5, kkmeans5$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.6100 0.5478 0.5194
valid(true_labels5, kkmeans5$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.7000 0.5718 0.5758

# first derivative
valid(true_labels5, kkmeans5d$kkmeans_rbfdot_EIMEI$cluster)
# 0.6000 0.5290 0.5152
valid(true_labels5, kkmeans5d$kkmeans_rbfdot_HIMHI$cluster)
# 0.5300 0.5039 0.4968
valid(true_labels5, kkmeans5d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.6800 0.5894 0.5604
valid(true_labels5, kkmeans5d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.6500 0.5366 0.5404
valid(true_labels5, kkmeans5d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.9400 0.8850 0.8861
valid(true_labels5, kkmeans5d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5500 0.4999 0.5000
valid(true_labels5, kkmeans5d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.6200 0.5329 0.5240

# second derivative
valid(true_labels5, kkmeans5dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.6100 0.5193 0.5194
valid(true_labels5, kkmeans5dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.6500 0.5366 0.5404
valid(true_labels5, kkmeans5dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.8700 0.7715 0.7715
valid(true_labels5, kkmeans5dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5600 0.4988 0.5022
valid(true_labels5, kkmeans5dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.7800 0.6813 0.6533
valid(true_labels5, kkmeans5dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5400 0.4935 0.4982
valid(true_labels5, kkmeans5dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.6300 0.5244 0.5291

# kernel k means with polynomial kernel 

# original 
valid(true_labels5, kkmeans5$kkmeans_polydot_EIMEI$cluster)
# 0.5100 0.4926 0.4952
valid(true_labels5, kkmeans5$kkmeans_polydot_EIMMEI$cluster)
# 0.6900 0.6274 0.5679
valid(true_labels5, kkmeans5$kkmeans_polydot_HIMHI$cluster)
# 0.5200 0.4910 0.4958
valid(true_labels5, kkmeans5$kkmeans_polydot_HIMMHI$cluster)
# 0.7200 0.6453 0.5927
valid(true_labels5, kkmeans5$kkmeans_polydot_MMEIMMHI$cluster)
# 0.7100 0.6342 0.5840
valid(true_labels5, kkmeans5$kkmeans_polydot_MEIMMEI$cluster)
# 0.6400 0.6281 0.5345
valid(true_labels5, kkmeans5$kkmeans_polydot_MHIMMHI$cluster)
# 0.7200 0.6453 0.5927

# first derivative
valid(true_labels5, kkmeans5d$kkmeans_polydot_EIMEI$cluster)
# 0.5100 0.4926 0.4952
valid(true_labels5, kkmeans5d$kkmeans_polydot_EIMMEI$cluster)
# 0.6200 0.6286 0.5240
valid(true_labels5, kkmeans5d$kkmeans_polydot_HIMHI$cluster)
# 0.5100 0.4926 0.4952
valid(true_labels5, kkmeans5d$kkmeans_polydot_HIMMHI$cluster)
# 0.7300 0.6498 0.6018
valid(true_labels5, kkmeans5d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.6800 0.6327 0.5604
valid(true_labels5, kkmeans5d$kkmeans_polydot_MEIMMEI$cluster)
# 0.6200 0.6286 0.5240
valid(true_labels5, kkmeans5d$kkmeans_polydot_MHIMMHI$cluster)
# 0.7300 0.6498 0.6018

# second derivative 
valid(true_labels5, kkmeans5dd$kkmeans_polydot_EIMEI$cluster)
# 0.5100 0.4901 0.4952
valid(true_labels5, kkmeans5dd$kkmeans_polydot_EIMMEI$cluster)
# 0.5740 0.6549 0.6113
valid(true_labels5, kkmeans5dd$kkmeans_polydot_HIMHI$cluster)
# 0.5100 0.4901 0.4952
valid(true_labels5, kkmeans5dd$kkmeans_polydot_HIMMHI$cluster)
# 0.7400 0.6549 0.6113
valid(true_labels5, kkmeans5dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.9200 0.8517 0.8513
valid(true_labels5, kkmeans5dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.7400 0.6549 0.6113
valid(true_labels5, kkmeans5dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.7100 0.6414 0.5840


#############################
# Model 6 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels6, kkmeans6$kkmeans_rbfdot_EIMEI$cluster)
# 0.5800 0.5045 0.5079
valid(true_labels6, kkmeans6$kkmeans_rbfdot_HIMHI$cluster)
# 0.5100 0.4950 0.4952
valid(true_labels6, kkmeans6$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5500 0.5031 0.5000
valid(true_labels6, kkmeans6$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5100 0.4909 0.4952
valid(true_labels6, kkmeans6$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5500 0.5171 0.5000
valid(true_labels6, kkmeans6$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5400 0.4935 0.4982
valid(true_labels6, kkmeans6$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5700 0.5411 0.5048

# first derivative
valid(true_labels6, kkmeans6d$kkmeans_rbfdot_EIMEI$cluster)
# 0.5600 0.5500 0.5022
valid(true_labels6, kkmeans6d$kkmeans_rbfdot_HIMHI$cluster)
# 0.5100 0.4901 0.4951
valid(true_labels6, kkmeans6d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5800 0.5093 0.5079
valid(true_labels6, kkmeans6d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5900 0.5338 0.5113
valid(true_labels6, kkmeans6d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.6300 0.5508 0.5291
valid(true_labels6, kkmeans6d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5800 0.5475 0.5079
valid(true_labels6, kkmeans6d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5100 0.4909 0.4952

# second derivative
valid(true_labels6, kkmeans6dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.6200 0.5329 0.5240
valid(true_labels6, kkmeans6dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.6100 0.5262 0.5194
valid(true_labels6, kkmeans6dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5100 0.5023 0.4952
valid(true_labels6, kkmeans6dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels6, kkmeans6dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.6300 0.5244 0.5291
valid(true_labels6, kkmeans6dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.6900 0.5678 0.5679
valid(true_labels6, kkmeans6dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5700 0.5079 0.5048

# kernel k means with polynomial kernel 

# original 
valid(true_labels6, kkmeans6$kkmeans_polydot_EIMEI$cluster)
# 0.5900 0.5088 0.5113
valid(true_labels6, kkmeans6$kkmeans_polydot_EIMMEI$cluster)
# 0.5700 0.5341 0.5048
valid(true_labels6, kkmeans6$kkmeans_polydot_HIMHI$cluster)
# 0.5800 0.5045 0.5079
valid(true_labels6, kkmeans6$kkmeans_polydot_HIMMHI$cluster)
# 0.6000 0.5200 0.5152
valid(true_labels6, kkmeans6$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5400 0.4931 0.4982
valid(true_labels6, kkmeans6$kkmeans_polydot_MEIMMEI$cluster)
# 0.5700 0.5341 0.5048
valid(true_labels6, kkmeans6$kkmeans_polydot_MHIMMHI$cluster)
# 0.6000 0.5200 0.5152

# first derivative
valid(true_labels6, kkmeans6d$kkmeans_polydot_EIMEI$cluster)
# 0.5200 0.4906 0.4958
valid(true_labels6, kkmeans6d$kkmeans_polydot_EIMMEI$cluster)
# 0.5900 0.5471 0.5113
valid(true_labels6, kkmeans6d$kkmeans_polydot_HIMHI$cluster)
# 0.5200 0.4906 0.4958
valid(true_labels6, kkmeans6d$kkmeans_polydot_HIMMHI$cluster)
# 0.6500 0.6037 0.5404
valid(true_labels6, kkmeans6d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels6, kkmeans6d$kkmeans_polydot_MEIMMEI$cluster)
# 0.5800 0.5337 0.5079
valid(true_labels6, kkmeans6d$kkmeans_polydot_MHIMMHI$cluster)
# 0.6500 0.6037 0.5404

# second derivative 
valid(true_labels6, kkmeans6dd$kkmeans_polydot_EIMEI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels6, kkmeans6dd$kkmeans_polydot_EIMMEI$cluster)
# 0.6200 0.5431 0.5240
valid(true_labels6, kkmeans6dd$kkmeans_polydot_HIMHI$cluster)
# 0.5000 0.4915 0.4949
valid(true_labels6, kkmeans6dd$kkmeans_polydot_HIMMHI$cluster)
# 0.6700 0.5927 0.5533
valid(true_labels6, kkmeans6dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.7300 0.6498 0.6018
valid(true_labels6, kkmeans6dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.6000 0.5472 0.5152
valid(true_labels6, kkmeans6dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.6800 0.5958 0.5604


#############################
# Model 7 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels7, kkmeans7$kkmeans_rbfdot_EIMEI$cluster)
# 0.9100 0.8356 0.8345
valid(true_labels7, kkmeans7$kkmeans_rbfdot_HIMHI$cluster)
# 0.9100 0.8345 0.8345
valid(true_labels7, kkmeans7$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5700 0.5023 0.5048
valid(true_labels7, kkmeans7$kkmeans_rbfdot_HIMMHI$cluster)
# 0.8100 0.7075 0.6891
valid(true_labels7, kkmeans7$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels7, kkmeans7$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.9800 0.9600 0.9604
valid(true_labels7, kkmeans7$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.7300 0.6498 0.6018

# first derivative
valid(true_labels7, kkmeans7d$kkmeans_rbfdot_EIMEI$cluster)
# 0.5300 0.4999 0.4968
valid(true_labels7, kkmeans7d$kkmeans_rbfdot_HIMHI$cluster)
# 0.5900 0.5182 0.5113
valid(true_labels7, kkmeans7d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.6400 0.5332 0.5345
valid(true_labels7, kkmeans7d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5600 0.4988 0.5022
valid(true_labels7, kkmeans7d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.6700 0.5739 0.5533
valid(true_labels7, kkmeans7d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5400 0.5545 0.4982
valid(true_labels7, kkmeans7d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5400 0.4996 0.4982

# second derivative
valid(true_labels7, kkmeans7dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.5100 0.4950 0.4952
valid(true_labels7, kkmeans7dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.5400 0.5245 0.4982
valid(true_labels7, kkmeans7dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.6000 0.5345 0.5152
valid(true_labels7, kkmeans7dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.6300 0.5357 0.5291
valid(true_labels7, kkmeans7dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.7000 0.5800 0.5758
valid(true_labels7, kkmeans7dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5300 0.5336 0.4968
valid(true_labels7, kkmeans7dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.7100 0.5899 0.5840

# kernel k means with polynomial kernel 

# original 
valid(true_labels7, kkmeans7$kkmeans_polydot_EIMEI$cluster)
# 0.9200 0.8509 0.8513
valid(true_labels7, kkmeans7$kkmeans_polydot_EIMMEI$cluster)
# 0.9800 0.9600 0.9604
valid(true_labels7, kkmeans7$kkmeans_polydot_HIMHI$cluster)
# 0.9100 0.8329 0.8345
valid(true_labels7, kkmeans7$kkmeans_polydot_HIMMHI$cluster)
# 0.9700 0.9407 0.9412
valid(true_labels7, kkmeans7$kkmeans_polydot_MMEIMMHI$cluster)
# 0.9900 0.9798 0.9800
valid(true_labels7, kkmeans7$kkmeans_polydot_MEIMMEI$cluster)
# 0.9800 0.9600 0.9604
valid(true_labels7, kkmeans7$kkmeans_polydot_MHIMMHI$cluster)
# 0.9700 0.9407 0.9412

# first derivative
valid(true_labels7, kkmeans7d$kkmeans_polydot_EIMEI$cluster)
# 0.7700 0.6386 0.6422
valid(true_labels7, kkmeans7d$kkmeans_polydot_EIMMEI$cluster)
# 0.6400 0.6028 0.5345
valid(true_labels7, kkmeans7d$kkmeans_polydot_HIMHI$cluster)
# 0.7900 0.6615 0.6648
valid(true_labels7, kkmeans7d$kkmeans_polydot_HIMMHI$cluster)
# 0.6000 0.5345 0.5152
valid(true_labels7, kkmeans7d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5100 0.4983 0.4952
valid(true_labels7, kkmeans7d$kkmeans_polydot_MEIMMEI$cluster)
# 0.6300 0.6023 0.5291
valid(true_labels7, kkmeans7d$kkmeans_polydot_MHIMMHI$cluster)
# 0.6000 0.5345 0.5152

# second derivative 
valid(true_labels7, kkmeans7dd$kkmeans_polydot_EIMEI$cluster)
# 0.5500 0.5117 0.5000
valid(true_labels7, kkmeans7dd$kkmeans_polydot_EIMMEI$cluster)
# 0.5500 0.6426 0.5000
valid(true_labels7, kkmeans7dd$kkmeans_polydot_HIMHI$cluster)
# 0.5500 0.5117 0.5000
valid(true_labels7, kkmeans7dd$kkmeans_polydot_HIMMHI$cluster)
# 0.6800 0.5835 0.5604
valid(true_labels7, kkmeans7dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.6200 0.5329 0.5240
valid(true_labels7, kkmeans7dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.5700 0.5564 0.5048
valid(true_labels7, kkmeans7dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.6700 0.5927 0.5533


#############################
# Model 8 ###################
#############################

# kernel k means with radial basis function kernel 

# original
valid(true_labels8, kkmeans8$kkmeans_rbfdot_EIMEI$cluster)
# 0.5300 0.5086 0.4968
valid(true_labels8, kkmeans8$kkmeans_rbfdot_HIMHI$cluster)
# 0.8900 0.8050 0.8022
valid(true_labels8, kkmeans8$kkmeans_rbfdot_EIMMEI$cluster)
# 1 1 1
valid(true_labels8, kkmeans8$kkmeans_rbfdot_HIMMHI$cluster)
# 0.9300 0.8685 0.8685
valid(true_labels8, kkmeans8$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.9400 0.8857 0.8861
valid(true_labels8, kkmeans8$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.7800 0.6813 0.6533
valid(true_labels8, kkmeans8$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5300 0.4926 0.4968

# first derivative
valid(true_labels8, kkmeans8d$kkmeans_rbfdot_EIMEI$cluster)
# 0.6100 0.5170 0.5194
valid(true_labels8, kkmeans8d$kkmeans_rbfdot_HIMHI$cluster)
# 0.6600 0.5597 0.5467
valid(true_labels8, kkmeans8d$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5100 0.5124 0.4952
valid(true_labels8, kkmeans8d$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5200 0.4923 0.4958
valid(true_labels8, kkmeans8d$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5200 0.4972 0.4958
valid(true_labels8, kkmeans8d$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5000 0.4935 0.4949
valid(true_labels8, kkmeans8d$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5400 0.5313 0.4982

# second derivative
valid(true_labels8, kkmeans8dd$kkmeans_rbfdot_EIMEI$cluster)
# 0.5600 0.4971 0.5022
valid(true_labels8, kkmeans8dd$kkmeans_rbfdot_HIMHI$cluster)
# 0.5900 0.5280 0.5113
valid(true_labels8, kkmeans8dd$kkmeans_rbfdot_EIMMEI$cluster)
# 0.5100 0.5321 0.4952
valid(true_labels8, kkmeans8dd$kkmeans_rbfdot_HIMMHI$cluster)
# 0.5100 0.5560 0.4952
valid(true_labels8, kkmeans8dd$kkmeans_rbfdot_MMEIMMHI$cluster)
# 0.5000 0.5000 0.4949
valid(true_labels8, kkmeans8dd$kkmeans_rbfdot_MEIMMEI$cluster)
# 0.5100 0.5070 0.4952
valid(true_labels8, kkmeans8dd$kkmeans_rbfdot_MHIMMHI$cluster)
# 0.5100 0.5070 0.4952

# kernel k means with polynomial kernel 

# original 
valid(true_labels8, kkmeans8$kkmeans_polydot_EIMEI$cluster)
# 0.9300 0.8674 0.8685
valid(true_labels8, kkmeans8$kkmeans_polydot_EIMMEI$cluster)
# 0.9600 0.9219 0.9224
valid(true_labels8, kkmeans8$kkmeans_polydot_HIMHI$cluster)
# 0.9300 0.8672 0.8685
valid(true_labels8, kkmeans8$kkmeans_polydot_HIMMHI$cluster)
# 1 1 1 
valid(true_labels8, kkmeans8$kkmeans_polydot_MMEIMMHI$cluster)
# 1 1 1
valid(true_labels8, kkmeans8$kkmeans_polydot_MEIMMEI$cluster)
# 0.9600 0.9219 0.9224
valid(true_labels8, kkmeans8$kkmeans_polydot_MHIMMHI$cluster)
# 1 1 1

# first derivative
valid(true_labels8, kkmeans8d$kkmeans_polydot_EIMEI$cluster)
# 0.7200 0.5916 0.5927
valid(true_labels8, kkmeans8d$kkmeans_polydot_EIMMEI$cluster)
# 0.5200 0.4972 0.4958
valid(true_labels8, kkmeans8d$kkmeans_polydot_HIMHI$cluster)
# 0.7100 0.5819 0.5840
valid(true_labels8, kkmeans8d$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.5044 0.4949
valid(true_labels8, kkmeans8d$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5100 0.5070 0.4952
valid(true_labels8, kkmeans8d$kkmeans_polydot_MEIMMEI$cluster)
# 0.5200 0.4972 0.4958
valid(true_labels8, kkmeans8d$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.5044 0.4949

# second derivative 
valid(true_labels8, kkmeans8dd$kkmeans_polydot_EIMEI$cluster)
# 0.5600 0.4971 0.5022
valid(true_labels8, kkmeans8dd$kkmeans_polydot_EIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels8, kkmeans8dd$kkmeans_polydot_HIMHI$cluster)
# 0.5600 0.4971 0.5022
valid(true_labels8, kkmeans8dd$kkmeans_polydot_HIMMHI$cluster)
# 0.5000 0.5516 0.4949
valid(true_labels8, kkmeans8dd$kkmeans_polydot_MMEIMMHI$cluster)
# 0.5000 0.5602 0.4949
valid(true_labels8, kkmeans8dd$kkmeans_polydot_MEIMMEI$cluster)
# 0.5000 0.5151 0.4949
valid(true_labels8, kkmeans8dd$kkmeans_polydot_MHIMMHI$cluster)
# 0.5000 0.5516 0.4949