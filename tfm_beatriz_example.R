# install.packages("remotes")
remotes::install_github("bpulidob/ehymet")
library(ehymet)
library(dplyr)

data1 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 1, seed = 1221)
data1_labels <- c(rep(1,50), rep(2,50))
head(data1)
dim(data1)

## Generate a matrix with the indices
data1_ind <- ehymet::ind(data1, grid_ll = 0, grid_ul = 1, nbasis = 25,
                         norder = 4, indices = c("EI", "HI", "MEI", "MHI"))
head(data1_ind)

## Clustering with indices

# Indices combinations
# original indexes
VARS1 <- c("dtaEI","dtaHI") 
VARS2 <- c("ddtaEI","ddtaHI")
VARS3 <- c("d2dtaEI","d2dtaHI")
VARS4 <- c(VARS1,VARS2)
VARS5 <- c(VARS1,VARS3)
VARS6 <- c(VARS2,VARS3)
VARS7 <- c(VARS4,VARS3)

# generalized indexes
VARS8 <- c("dtaMEI","ddtaMEI")
VARS9 <- c("dtaMEI","d2dtaMEI")
VARS10 <- c("ddtaMEI","d2dtaMEI")
VARS11 <- c(VARS8,"d2dtaMEI")

# combining both indexes types
VARS12 <- c(VARS1,"dtaMEI")
VARS13 <- c(VARS2,"ddtaMEI") #no
VARS14 <- c(VARS3,"d2dtaMEI") #no
VARS15 <- c(VARS4,VARS8) #no
VARS16 <- c(VARS5,VARS9) #no
VARS17 <- c(VARS6,VARS10) #no
VARS18 <- c(VARS7,VARS11) #no

VARS <- list(VARS1,VARS2,VARS3,VARS4,VARS5,VARS6,VARS7,VARS8,VARS9,VARS10,
          VARS11,VARS12,VARS13,VARS14,VARS15,VARS16,VARS17, VARS18)

## Espectral clustering
data1_ind_spc <- ehymet::clustInd_spc(data1_ind, vars_combinations = VARS,
                                      n_cluster = 2, true_labels = data1_labels)

# Warning messages:
#   1: In check_vars_combinations(vars_combinations, ind_data) :
#   Combination/s of variables with index/indices 2, 3, 4, 5, 6, 7, 13, 14, 15, 16, 17, 18 is/are singular or almost singular. Removing them...
# 2: In check_vars_combinations(vars_combinations, ind_data) :
#   Combination/s of variable/s with index 2, 3, 4, 5, 6, 7, 13, 14, 15, 16, 17, 18 are not valid. Excluding them from any computation...




# This is because some of your indices combinations make no sense. See
head(data1_ind[,VARS2])

# EHyClus function generates all the possible combinations for clustering with
# the indices combinations you give it (except for svc because the package
# where it is implemented is not updated and it does not work properly any more)

# This function test the combinations in vars_combinations to see if they are
# suitable for performing clustering algorithms on them. Moreover if true_labels
# is not NULL it gives a table with all metrics

set.seed(1221)
data1_ehyclus <- ehymet::EHyClus(data1, vars_combinations = VARS, nbasis = 25,
                                 n_clusters = 2, norder = 4,
                                 true_labels = data1_labels)

head(data1_ehyclus$metrics)


# You can create a simulation function with this table

n_sim = 50 # choose simulation number
clasif <- c()
data_labels <- c(rep(1,50), rep(2,50)) #depend on the data you use

for(i in 1:n_sim){
  set.seed(i)
  datos <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 1, seed = 1221) # your data

  clasif_i <- ehymet::EHyClus(datos, vars_combinations = VARS, nbasis = 25,
                              n_clusters = 2, norder = 4,
                              true_labels = data_labels)
  clasif_i <- cbind(names = row.names(clasif_i$metrics),
                    data.frame(clasif_i$metrics, row.names=NULL))
  clasif <- bind_rows(clasif, clasif_i)

}

# Final simulation results
clasif_f <- clasif %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

