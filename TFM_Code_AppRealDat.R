#################################################
# TFM Code - Beatriz Espadanal Gonçalves ########
#################################################
# Application to real data ######################
#################################################

#-------------------------------
# case A - Berkeley growth study
#-------------------------------

library(fda)

data("growth")

boys <- growth$hgtm
girls <- growth$hgtf

data_g <- t(cbind(boys,girls))

# 1 is boys, 2 is girls
labels <- c(rep(1,39),rep(2,54))

plt_fun_growth <- function(data, true_labels, title){
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package 'ggplot2' is required for this functionality", call. = FALSE)
  }
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("package 'tidyr' is required for this functionality", call. = FALSE)
  }
  
  if (!(length(dim(data)) == 2)) {
    stop("This function can be only used with 2-dimensional datasets.", call. = FALSE)
  }
  
  df <-  dplyr::as_tibble(data)
  t_interval <- seq(0, 30, length = ncol(data))
  names(df) <- as.character(t_interval)
  df$id <- 1:nrow(data)
  df$Order <- true_labels
  df_long<- df %>% tidyr::pivot_longer(-c(id, Order), names_to="variable", values_to="values") %>%
    dplyr::mutate(variable=as.numeric(variable))
  pa <- df_long %>% ggplot2::ggplot(ggplot2::aes(x=variable, y=values,group=id, color=factor(Order)))
  
  plt <- pa +
    ggplot2::geom_line(linewidth=0.1)+
    # ggplot2::scale_color_brewer(palette = "Set1")+
    ggplot2::scale_color_manual(values=c("skyblue","orange")) +
    ggplot2::ggtitle(title)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  return(plt)
}

# smooth the data  first 
dta_g <- funspline(data_g,k=30,grid=growth$age)
data_s <- dta_g$smooth
smooth <- plt_fun_growth(data_s, labels, "Growth curves")


#first derivative
data_d <- dta_g$deriv
firstderiv <- plt_fun_growth(data_d,labels, "First derivatives")

#second derivative
data_d2 <- dta_g$deriv2
secondderiv <- plt_fun_growth(data_d2,labels, "Second derivatives")

gridExtra::grid.arrange(smooth, firstderiv, secondderiv, ncol=3)

# scatter plots 
par(mfrow=c(3,3))

ind_growth <- generate_indices(data_g, k=30,grid=c(1,18))
# original data 
plot(ind_growth$dtaEI[1:39],ind_growth$dtaHI[1:39], 
     col="deepskyblue", ylab="HI", xlab="EI",pch=10, main = "original data", 
     xlim=c(0,1),ylim=c(0,1))
points(ind_growth$dtaEI[40:93], ind_growth$dtaHI[40:93], col="orange")
plot(ind$dtaMEI[1:39],ind$ddtaMEI[1:39], col="deepskyblue",
     ylab="dMEI", xlab="MEI",pch=10, 
     main = "Original data and First derivative", xlim=c(0,1),ylim=c(0,1))
points(ind_growth$dtaMEI[40:93], ind_growth$ddtaMEI[40:93],
       col="orange")
plot(ind_growth$dtaMMEI[1:39],ind_growth$dtaMMHI[1:39], 
     col="deepskyblue", ylab="MMHI", xlab="MMEI",pch=10)
points(ind_growth$dtaMMEI[40:93], ind_growth$dtaMMHI[40:93], 
       col="orange")

# first derivatives
plot(ind_growth$ddtaEI[1:39],ind_growth$ddtaHI[1:39], 
     col="deepskyblue", ylab="HI", xlab="EI",pch=10,
     main = "First derivatives", xlim=c(0,1),ylim=c(0,1))
points(ind_growth$ddtaEI[40:93], ind_growth$ddtaHI[40:93], 
       col="orange")
plot(ind_growth$dtaMEI[1:39],ind_growth$d2dtaMHI[1:39],
     col="deepskyblue", ylab="d2MEI", xlab="MEI",pch=10, 
     xlim=c(0,1),ylim=c(0,1), main = "Original data and Second derivative")
points(ind_growth$dtaMEI[40:93], ind_growth$d2dtaMEI[40:93],
       col="orange")
plot(ind_growth$ddtaMMEI[1:39],ind_growth$ddtaMMHI[1:39], 
     col="deepskyblue", ylab="MMHI", xlab="MMEI",pch=10)
points(ind_growth$ddtaMMEI[40:93], ind_growth$ddtaMMHI[40:93], 
       col="orange")

#second derivatives
plot(ind_growth$d2dtaEI[1:39],ind_growth$d2dtaHI[1:39], 
     col="deepskyblue", ylab="HI", xlab="EI",pch=10, main = "Second derivatives", 
     xlim=c(0,1),ylim=c(0,1))
points(ind_growth$d2dtaEI[40:93], ind_growth$d2dtaHI[40:93], 
       col="orange")
plot(ind_growth$ddtaMEI[1:39],ind_growth$d2dtaMEI[1:39],
     col="deepskyblue", ylab="d2MEI", xlab="dMEI",pch=10, 
     xlim=c(0,1),ylim=c(0,1), main ="First and Second derivatives")
points(ind_growth$ddtaMEI[40:93], ind_growth$d2dtaMEI[40:93],
       col="orange")
plot(ind_growth$d2dtaMMEI[1:39],ind_growth$d2dtaMMHI[1:39], 
     col="deepskyblue", ylab="MMHI", xlab="MMEI",pch=10)
points(ind_growth$d2dtaMMEI[40:93], ind_growth$d2dtaMMHI[40:93], 
       col="orange") 

# -------------------------
# Clustering and validation 
# -------------------------


set.seed(12345)

data_list_g_old <- list(v3,v4,v5,v7,v8,v9,v10,v11,v12,v13,v14,v15)
data_list_g_new <- list(v16,v17,v18,v19,v20,v21,v22,v23,
                        v24,v25,v26,v27,v28,v29,v30)

clust_g_old <- EHyClus(data_g, vars_combinations = data_list_g_old,
                       n_clusters = 2, true_labels = labels)
clust_g_new <- EHyClus_new(data_g, vars_combinations = data_list_g_new,
                           n_clusters = 2, true_labels = labels)

View(clust_g_old$metrics)
View(clust_g_new$metrics) #best results! 

#-----------------------------------
# case B - Canadian weather data set 
#-----------------------------------

data("CanadianWeather")

# temperature
data <- t(dailyAv[,,1])

# smoothing
dta <- funspline(data,k=30,bs="cr",grid = c(1,365))
smooth_cw <- dta$smooth
first_cw <- dta$deriv 
second_cw <- dta$deriv2

# 1 is region 1, 2 region 2, 3 region 3 and 4 region 4
labels_cw <- c(rep(1,15), rep(2,12), rep(3,5), rep(4,3))

# plots
plt_fun_cw <- function(data, true_labels, title){
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package 'ggplot2' is required for this functionality", call. = FALSE)
  }
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("package 'tidyr' is required for this functionality", call. = FALSE)
  }
  
  if (!(length(dim(data)) == 2)) {
    stop("This function can be only used with 2-dimensional datasets.", call. = FALSE)
  }
  
  df <-  dplyr::as_tibble(data)
  # t_interval <- seq(0, 30, length = ncol(data))
  # names(df) <- as.character(t_interval)
  df$id <- 1:nrow(data)
  df$Order <- true_labels
  df_long<- df %>% tidyr::pivot_longer(-c(id, Order), names_to="variable", values_to="values") %>%
    dplyr::mutate(variable=as.numeric(variable))
  pa <- df_long %>% ggplot2::ggplot(ggplot2::aes(x=variable, y=values,group=id, color=factor(Order)))
  
  plt <- pa +
    ggplot2::geom_line(linewidth=0.1)+
    # ggplot2::scale_color_brewer(palette = "Set1")+
    ggplot2::scale_color_manual(values=c("red","blue","green","orange")) +
    ggplot2::ggtitle(title)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  return(plt)
}

p_smooth <-plt_fun_cw(smooth_cw,labels_cw,"Average daily temperature curves")
p_firstderiv <- plt_fun_cw(first_cw,labels_cw, "First derivative curves")
p_secondderiv <- plt_fun_cw(second_cw,labels_cw, "Second derivative curves")

gridExtra::grid.arrange(p_smooth,p_firstderiv,p_secondderiv,ncol=3)

# scatter plots
par(mfrow=c(3,3))

# generating indexes
ind <- generate_indices(data,k=30)

#original data
plot(ind$dtaEI[1:15],ind$dtaHI[1:15],
     col="red", ylab="HI", xlab="EI",pch=16,
     main = "Original data", xlim=c(0,1),ylim=c(0,1))
points(cbind(ind$dtaEI[16:24],ind$dtaEI[30:32]), 
       cbind(ind$dtaHI[16:24],ind$dtaHI[30:32]), col="blue", pch=16)
points(ind$dtaEI[25:29], ind$dtaHI[25:29], col="green",pch=16)
points(ind$dtaEI[33:35], ind$dtaHI[33:35], col="orange",pch=16)

plot(ind$dtaMEI[1:15],ind$ddtaMEI[1:15],
     col="red", ylab="dMEI", xlab="MEI",pch=16,
     xlim=c(0,1),ylim=c(0,1), main="Original data and first derivative")
points(cbind(ind$dtaMEI[16:24],ind$dtaMEI[30:32]), 
       cbind(ind$ddtaMEI[16:24],ind$ddtaMEI[30:32]), col="blue", pch=16)
points(ind$dtaMEI[25:29], ind$ddtaMEI[25:29], col="green",pch=16)
points(ind$dtaMEI[33:35], ind$ddtaMEI[33:35], col="orange",pch=16)

plot(ind$dtaMMEI[1:15],ind$dtaMMHI[1:15],
     col="red", ylab="MMHI", xlab="MMEI",pch=16)
points(cbind(ind$dtaMMEI[16:24],ind$dtaMMEI[30:32]), 
       cbind(ind$dtaMMHI[16:24],ind$dtaMMHI[30:32]), col="blue", pch=16)
points(ind$dtaMMEI[25:29], ind$dtaMMHI[25:29], col="green",pch=16)
points(ind$dtaMMEI[33:35], ind$dtaMMHI[33:35], col="orange",pch=16)

# first derivative
plot(ind$ddtaEI[1:15],ind$ddtaHI[1:15],
     col="red", ylab="HI", xlab="EI",pch=16,
     main = "First derivatives", xlim=c(0,1),ylim=c(0,1))
points(cbind(ind$ddtaEI[16:24],ind$ddtaEI[30:32]), 
       cbind(ind$ddtaHI[16:24],ind$ddtaHI[30:32]), col="blue", pch=16)
points(ind$ddtaEI[25:29], ind$ddtaHI[25:29], col="green",pch=16)
points(ind$ddtaEI[33:35], ind$ddtaHI[33:35], col="orange",pch=16)

plot(ind$dtaMEI[1:15],ind$d2dtaMEI[1:15],
     col="red", ylab="d2MEI", xlab="MEI",pch=16,
     xlim=c(0,1),ylim=c(0,1),main="original and second derivatives")
points(cbind(ind$dtaMEI[16:24],ind$dtaMEI[30:32]), 
       cbind(ind$d2dtaMEI[16:24],ind$d2dtaMEI[30:32]), col="blue", pch=16)
points(ind$dtaMEI[25:29], ind$d2dtaMHI[25:29], col="green",pch=16)
points(ind$dtaMEI[33:35], ind$d2dtaMHI[33:35], col="orange",pch=16)

plot(ind$ddtaMMEI[1:15],ind$ddtaMMHI[1:15],
     col="red", ylab="MMHI", xlab="MMEI",pch=16)
points(cbind(ind$ddtaMMEI[16:24],ind$ddtaMMEI[30:32]), 
       cbind(ind$ddtaMMHI[16:24],ind$ddtaMMHI[30:32]), col="blue", pch=16)
points(ind$ddtaMMEI[25:29], ind$ddtaMMHI[25:29], col="green",pch=16)
points(ind$ddtaMMEI[33:35], ind$ddtaMMHI[33:35], col="orange",pch=16)

# second derivative
plot(ind$d2dtaEI[1:15],ind$d2dtaHI[1:15],
     col="red", ylab="HI", xlab="EI",pch=16,
     main = "second derivatives", xlim=c(0,1),ylim=c(0,1))
points(cbind(ind$d2dtaEI[16:24],ind$d2dtaEI[30:32]), 
       cbind(ind$d2dtaHI[16:24],ind$d2dtaHI[30:32]), col="blue", pch=16)
points(ind$d2dtaEI[25:29], ind$d2dtaHI[25:29], col="green",pch=16)
points(ind$d2dtaEI[33:35], ind$d2dtaHI[33:35], col="orange",pch=16)

plot(ind$ddtaMEI[1:15],ind$d2dtaMEI[1:15],
     col="red", ylab="d2MEI", xlab="dMEI",pch=16,
     xlim=c(0,1),ylim=c(0,1), main="first and second derivatives")
points(cbind(ind$ddtaMEI[16:24],ind$ddtaMEI[30:32]), 
       cbind(ind$d2dtaMEI[16:24],ind$d2dtaMEI[30:32]), col="blue", pch=16)
points(ind$ddtaMEI[25:29], ind$d2dtaMEI[25:29], col="green",pch=16)
points(ind$ddtaMEI[33:35], ind$d2dtaMEI[33:35], col="orange",pch=16)

plot(ind$d2dtaMMEI[1:15],ind$d2dtaMMHI[1:15],
     col="red", ylab="MMHI", xlab="MMEI",pch=16)
points(cbind(ind$d2dtaMMEI[16:24],ind$d2dtaMMEI[30:32]), 
       cbind(ind$d2dtaMMHI[16:24],ind$d2dtaMMHI[30:32]), col="blue", pch=16)
points(ind$d2dtaMMEI[25:29], ind$d2dtaMMHI[25:29], col="green",pch=16)
points(ind$d2dtaMMEI[33:35], ind$d2dtaMMHI[33:35], col="orange",pch=16)

# clustering
set.seed(12345)
data_list_cw_old <- list(v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15)
data_list_cw_new <- list(v16,v17,v18,v19,v20,v21,v22,v23,
                         v24,v25,v26,v27,v28,v29,v30)

clust_cw_old <- EHyClus(data, vars_combinations = data_list_cw_old,
                        n_clusters = 4, true_labels = labels_cw)
clust_cw_new <- EHyClus_new(data, vars_combinations = data_list_cw_new,
                            n_clusters = 4, true_labels = labels_cw)

View(clust_cw_old$metrics)
View(clust_cw_new$metrics) # best results!! 

# precipitation

data_prec <- t(CanadianWeather$dailyAv[,,2])

# smoothing
dta_prec <- funspline(data_prec,k=30,bs="cr")
smooth_cw_prec <- dta_prec$smooth
first_cw_prec <- dta_prec$deriv 
second_cw_prec <- dta_prec$deriv2

# plots
p_smooth_prec <-plt_fun_cw(smooth_cw_prec,labels_cw,"Average daily precipitation curves")
p_firstderiv_prec <- plt_fun_cw(first_cw_prec,labels_cw, "First derivative curves")
p_secondderiv_prec <- plt_fun_cw(second_cw_prec,labels_cw, "Second derivative curves")

gridExtra::grid.arrange(p_smooth_prec,p_firstderiv_prec,p_secondderiv_prec,ncol=3)

# scatter plots

# generating indexes
ind_prec <- generate_indices(data_prec,k=30)

#original data
plot(ind_prec$dtaEI[1:15],ind_prec$dtaHI[1:15],
     col="red", ylab="HI", xlab="EI",pch=15,
     main = "Original data", xlim=c(0,1),ylim=c(0,1))
points(cbind(ind_prec$dtaEI[16:24],ind_prec$dtaEI[30:32]), 
       cbind(ind_prec$dtaHI[16:24],ind_prec$dtaHI[30:32]), col="blue", pch=15)
points(ind_prec$dtaEI[25:29], ind_prec$dtaHI[25:29], col="green",pch=15)
points(ind_prec$dtaEI[33:35], ind_prec$dtaHI[33:35], col="orange",pch=15)

plot(ind_prec$dtaMEI[1:15],ind_prec$ddtaMEI[1:15],
     col="red", ylab="dMEI", xlab="MEI",pch=15,
     xlim=c(0,1),ylim=c(0,1), main ="Original data and first derivative")
points(cbind(ind_prec$dtaMEI[16:24],ind_prec$dtaMEI[30:32]), 
       cbind(ind_prec$ddtaMEI[16:24],ind_prec$ddtaMEI[30:32]), col="blue", pch=15)

points(ind_prec$dtaMEI[25:29], ind_prec$ddtaMEI[25:29], col="green",pch=15)
points(ind_prec$dtaMEI[33:35], ind_prec$ddtaMEI[33:35], col="orange",pch=15)

plot(ind_prec$dtaMMEI[1:15],ind_prec$dtaMMHI[1:15],
     col="red", ylab="MMHI", xlab="MMEI",pch=15)
points(cbind(ind_prec$dtaMMEI[16:24],ind_prec$dtaMMEI[30:32]), 
       cbind(ind_prec$dtaMMHI[16:24],ind_prec$dtaMMHI[30:32]), col="blue", pch=15)
points(ind_prec$dtaMMEI[25:29], ind_prec$dtaMMHI[25:29], col="green",pch=15)
points(ind_prec$dtaMMEI[33:35], ind_prec$dtaMMHI[33:35], col="orange",pch=15)

# first derivative
plot(ind_prec$ddtaEI[1:15],ind_prec$ddtaHI[1:15],
     col="red", ylab="HI", xlab="EI",pch=15,
     main = "First derivatives", xlim=c(0,1),ylim=c(0,1))
points(cbind(ind_prec$ddtaEI[16:24],ind_prec$ddtaEI[30:32]), 
       cbind(ind_prec$ddtaHI[16:24],ind_prec$ddtaHI[30:32]), col="blue", pch=15)
points(ind_prec$ddtaEI[25:29], ind_prec$ddtaHI[25:29], col="green",pch=15)
points(ind_prec$ddtaEI[33:35], ind_prec$ddtaHI[33:35], col="orange",pch=15)

plot(ind$dtaMEI[1:15],ind$d2dtaMHI[1:15],
     col="red", ylab="d2MEI", xlab="MEI",pch=15,
     xlim=c(0,1),ylim=c(0,1), main ="original and second derivatives")
points(cbind(ind_prec$dtaMEI[16:24],ind_prec$dtaMEI[30:32]), 
       cbind(ind_prec$d2dtaMEI[16:24],ind_prec$d2dtaMEI[30:32]), col="blue", pch=15)
points(ind_prec$dtaMEI[25:29], ind_prec$d2dtaMHI[25:29], col="green",pch=15)
points(ind_prec$dtaMEI[33:35], ind_prec$d2dtaMHI[33:35], col="orange",pch=15)

plot(ind_prec$ddtaMMEI[1:15],ind_prec$ddtaMMHI[1:15],
     col="red", ylab="MMHI", xlab="MMEI",pch=15)
points(cbind(ind_prec$ddtaMMEI[16:24],ind_prec$ddtaMMEI[30:32]), 
       cbind(ind_prec$ddtaMMHI[16:24],ind_prec$ddtaMMHI[30:32]), col="blue", pch=15)
points(ind_prec$ddtaMMEI[25:29], ind_prec$ddtaMMHI[25:29], col="green",pch=15)
points(ind_prec$ddtaMMEI[33:35], ind_prec$ddtaMMHI[33:35], col="orange",pch=15)

# second derivative
plot(ind_prec$d2dtaEI[1:15],ind_prec$d2dtaHI[1:15],
     col="red", ylab="HI", xlab="EI",pch=15,
     main = "second derivatives", xlim=c(0,1),ylim=c(0,1))
points(cbind(ind_prec$d2dtaEI[16:24],ind_prec$d2dtaEI[30:32]), 
       cbind(ind_prec$d2dtaHI[16:24],ind_prec$d2dtaHI[30:32]), col="blue", pch=15)
points(ind_prec$d2dtaEI[25:29], ind_prec$d2dtaHI[25:29], col="green",pch=15)
points(ind_prec$d2dtaEI[33:35], ind_prec$d2dtaHI[33:35], col="orange",pch=15)

plot(ind_prec$ddtaMEI[1:15],ind_prec$d2dtaMEI[1:15],
     col="red", ylab="d2MEI", xlab="dMEI",pch=15,
     xlim=c(0,1),ylim=c(0,1), main = "first and second derivatives")
points(cbind(ind_prec$ddtaMEI[16:24],ind_prec$ddtaMEI[30:32]), 
       cbind(ind_prec$d2dtaMEI[16:24],ind_prec$d2dtaMEI[30:32]), col="blue", pch=15)
points(ind_prec$ddtaMEI[25:29], ind_prec$d2dtaMEI[25:29], col="green",pch=15)
points(ind_prec$ddtaMEI[33:35], ind_prec$d2dtaMEI[33:35], col="orange",pch=15)

plot(ind_prec$d2dtaMMEI[1:15],ind_prec$d2dtaMMHI[1:15],
     col="red", ylab="MMHI", xlab="MMEI",pch=15)
points(cbind(ind_prec$d2dtaMMEI[16:24],ind_prec$d2dtaMMEI[30:32]), 
       cbind(ind_prec$d2dtaMMHI[16:24],ind_prec$d2dtaMMHI[30:32]), col="blue", pch=15)
points(ind_prec$d2dtaMMEI[25:29], ind_prec$d2dtaMMHI[25:29], col="green",pch=15)
points(ind_prec$d2dtaMMEI[33:35], ind_prec$d2dtaMMHI[33:35], col="orange",pch=15)

# clustering
set.seed(12345)
data_list_cw_old <- list(v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15)
data_list_cw_new <- list(v16,v17,v18,v19,v20,v21,v22,v23,
                         v24,v25,v26,v27,v28,v29,v30)

clust_cw_prec_old <- EHyClus(data_prec, vars_combinations = data_list_cw_old,
                        n_clusters = 4, true_labels = labels_cw)
clust_cw_prec_new <- EHyClus_new(data_prec, vars_combinations = data_list_cw_new,
                            n_clusters = 4, true_labels = labels_cw)

View(clust_cw_prec_old$metrics)
View(clust_cw_prec_new$metrics) # best results!! 
