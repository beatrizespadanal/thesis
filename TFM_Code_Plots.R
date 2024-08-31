#################################################
# TFM Code - Beatriz Espadanal Gonçalves ########
#################################################
# Plots #########################################
#################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ehymet)

plt_fun <- function(data, true_labels){
  
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
  t_interval <- seq(0, 1, length = ncol(data))
  names(df) <- as.character(t_interval)
  df$id <- 1:nrow(data)
  df$Order <- true_labels
  df_long<- df %>% tidyr::pivot_longer(-c(id, Order), names_to="variable", values_to="values") %>%
    dplyr::mutate(variable=as.numeric(variable))
  pa <- df_long %>% ggplot2::ggplot(ggplot2::aes(x=variable, y=values,group=id, color=factor(Order)))
  
  plt <- pa +
    ggplot2::geom_line(linewidth=0.1)+
    ggplot2::scale_color_brewer(palette = "Set1")+
    # scale_color_manual(values=c("#CC6600","#3399FF")) +
    # ggtitle("Original D")+
    # ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::theme(legend.position = "none")
  return(plt)
}

# curves
# smooth original
data_2s <- funspline(data_2,k=30)$smooth
data_3s <- funspline(data_3,k=30)$smooth
data_4s <- funspline(data_4,k=30)$smooth
data_5s <- funspline(data_5,k=30)$smooth
data_6s <- funspline(data_6,k=30)$smooth
data_7s <- funspline(data_7,k=30)$smooth
data_8s <- funspline(data_8,k=30)$smooth
data_9s <- funspline(data_9,k=30)$smooth
data_11s <- funspline(data_11,k=30)$smooth
data_12s <- funspline(data_12,k=30)$smooth

plot2 <- plt_fun(data_2s, data2_labels)
plot3 <- plt_fun(data_3s, data3_labels)
plot4 <- plt_fun(data_4s, data4_labels)
plot5 <- plt_fun(data_5s, data5_labels)
plot6 <- plt_fun(data_6s, data6_labels)
plot7 <- plt_fun(data_7s, data7_labels)
plot8 <- plt_fun(data_8s, data8_labels)
plot9 <- plt_fun(data_9s, data9_labels)
plot11 <- plt_fun(data_11s, data11_labels)
plot12 <- plt_fun(data_12s, data12_labels)

# first derivative 
data_2d <- funspline(data_2,k=30)$deriv
data_3d <- funspline(data_3,k=30)$deriv
data_4d <- funspline(data_4,k=30)$deriv
data_5d <- funspline(data_5,k=30)$deriv
data_6d <- funspline(data_6,k=30)$deriv
data_7d <- funspline(data_7,k=30)$deriv
data_8d <- funspline(data_8,k=30)$deriv
data_9d <- funspline(data_9,k=30)$deriv
data_11d <- funspline(data_11,k=30)$deriv
data_12d <- funspline(data_12,k=30)$deriv

plot2_d <- plt_fun(data_2d,data2_labels)
plot3_d <- plt_fun(data_3d,data3_labels)
plot4_d <- plt_fun(data_4d,data4_labels)
plot5_d <- plt_fun(data_5d,data5_labels)
plot6_d <- plt_fun(data_6d,data6_labels)
plot7_d <- plt_fun(data_7d,data7_labels)
plot8_d <- plt_fun(data_8d,data8_labels)
plot9_d <- plt_fun(data_9d,data9_labels)
plot11_d <- plt_fun(data_11d,data11_labels)
plot12_d <- plt_fun(data_12d,data12_labels)

# second derivative 
data_2d2 <- funspline(data_2,k=30)$deriv2
data_3d2 <- funspline(data_3,k=30)$deriv2
data_4d2 <- funspline(data_4,k=30)$deriv2
data_5d2 <- funspline(data_5,k=30)$deriv2
data_6d2 <- funspline(data_6,k=30)$deriv2
data_7d2 <- funspline(data_7,k=30)$deriv2
data_8d2 <- funspline(data_8,k=30)$deriv2
data_9d2 <- funspline(data_9,k=30)$deriv2
data_11d2 <- funspline(data_11,k=30)$deriv2
data_12d2 <- funspline(data_12,k=30)$deriv2

plot2_dd <- plt_fun(data_2d2,data2_labels)
plot3_dd <- plt_fun(data_3d2,data3_labels)
plot4_dd <- plt_fun(data_4d2,data4_labels)
plot5_dd <- plt_fun(data_5d2,data5_labels)
plot6_dd <- plt_fun(data_6d2,data6_labels)
plot7_dd <- plt_fun(data_7d2,data7_labels)
plot8_dd <- plt_fun(data_8d2,data8_labels)
plot9_dd <- plt_fun(data_9d2,data9_labels)
plot11_dd <- plt_fun(data_11d2,data11_labels)
plot12_dd <- plt_fun(data_12d2,data12_labels)

graphics.off()
grid.arrange(plot2, plot2_d, plot2_dd, ncol=3)
grid.arrange(plot3, plot3_d, plot3_dd, ncol=3)
grid.arrange(plot4, plot4_d, plot4_dd, ncol=3)
grid.arrange(plot5, plot5_d, plot5_dd, ncol=3)
grid.arrange(plot6, plot6_d, plot6_dd, ncol=3)
grid.arrange(plot7, plot7_d, plot7_dd, ncol=3)
grid.arrange(plot8, plot8_d, plot8_dd, ncol=3)
grid.arrange(plot9, plot9_d, plot9_dd, ncol=3)
grid.arrange(plot11, plot11_d, plot11_dd, ncol=3)
grid.arrange(plot12, plot12_d, plot12_dd, ncol=3)

# scatter plots for EI, MEI, MMEI, HI, MHI and MMHI
par(mfrow=c(3,3))

# model 1 and 2
plot(generate_indices(data_2, k=30)$dtaEI[1:50],generate_indices(data_2, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 2", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$dtaEI[51:100], generate_indices(data_2,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_2,k=30)$dtaMEI[1:50], generate_indices(data_2,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_2,k=30)$dtaMEI[51:100], generate_indices(data_2,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_2,k=30)$dtaMMEI[1:50], generate_indices(data_2,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_2,k=30)$dtaMMEI[51:100], generate_indices(data_2,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_2,k=30)$ddtaEI[1:50],generate_indices(data_2,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$ddtaEI[51:100], generate_indices(data_2,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_2,k=30)$dtaMEI[1:50], generate_indices(data_2,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_2,k=30)$dtaMEI[51:100], generate_indices(data_2,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_2,k=30)$ddtaMMEI[1:50], generate_indices(data_2,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_2,k=30)$ddtaMMEI[51:100], generate_indices(data_2,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_2,k=30)$d2dtaEI[1:50],generate_indices(data_2,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$d2dtaEI[51:100], generate_indices(data_2,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_2,k=30)$ddtaMEI[1:50], generate_indices(data_2,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$ddtaMEI[51:100], generate_indices(data_2,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_2,k=30)$d2dtaMMEI[1:50], generate_indices(data_2,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_2,k=30)$d2dtaMMEI[51:100], generate_indices(data_2,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 1 and 3
plot(generate_indices(data_3, k=30)$dtaEI[1:50],generate_indices(data_3, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 3", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$dtaEI[51:100], generate_indices(data_3,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_3,k=30)$dtaMEI[1:50], generate_indices(data_3,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_3,k=30)$dtaMEI[51:100], generate_indices(data_3,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_3,k=30)$dtaMMEI[1:50], generate_indices(data_3,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_3,k=30)$dtaMMEI[51:100], generate_indices(data_3)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_3,k=30)$ddtaEI[1:50],generate_indices(data_3,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$ddtaEI[51:100], generate_indices(data_3,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_3,k=30)$dtaMEI[1:50], generate_indices(data_3,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_3,k=30)$dtaMEI[51:100], generate_indices(data_3,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_3,k=30)$ddtaMMEI[1:50], generate_indices(data_3,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_3,k=30)$ddtaMMEI[51:100], generate_indices(data_3,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_3,k=30)$d2dtaEI[1:50],generate_indices(data_3,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$d2dtaEI[51:100], generate_indices(data_3,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_3,k=30)$ddtaMEI[1:50], generate_indices(data_3,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_3,k=30)$ddtaMEI[51:100], generate_indices(data_3,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_3,k=30)$d2dtaMMEI[1:50], generate_indices(data_3,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_3,k=30)$d2dtaMMEI[51:100], generate_indices(data_3,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 1 and 4
plot(generate_indices(data_4, k=30)$dtaEI[1:50],generate_indices(data_4, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 4", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$dtaEI[51:100], generate_indices(data_4,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_4,k=30)$dtaMEI[1:50], generate_indices(data_4,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_4,k=30)$dtaMEI[51:100], generate_indices(data_4,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_4,k=30)$dtaMMEI[1:50], generate_indices(data_4,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_4,k=30)$dtaMMEI[51:100], generate_indices(data_4,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_4,k=30)$ddtaEI[1:50],generate_indices(data_4,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$ddtaEI[51:100], generate_indices(data_4,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_4,k=30)$dtaMEI[1:50], generate_indices(data_4,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1),main="Original data and Second derivative")
points(generate_indices(data_4,k=30)$dtaMEI[51:100], generate_indices(data_4,k=30)$d2dtaMHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_4,k=30)$ddtaMMEI[1:50], generate_indices(data_4,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_4,k=30)$ddtaMMEI[51:100], generate_indices(data_4,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_4,k=30)$d2dtaEI[1:50],generate_indices(data_4,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$d2dtaEI[51:100], generate_indices(data_4,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_4,k=30)$ddtaMEI[1:50], generate_indices(data_4,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_4,k=30)$ddtaMEI[51:100], generate_indices(data_4,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_4,k=30)$d2dtaMMEI[1:50], generate_indices(data_4,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_4,k=30)$d2dtaMMEI[51:100], generate_indices(data_4,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 1 and 5
plot(generate_indices(data_5, k=30)$dtaEI[1:50],generate_indices(data_5, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 5", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$dtaEI[51:100], generate_indices(data_5,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_5,k=30)$dtaMEI[1:50], generate_indices(data_5,k=30)$ddtaMEI[1:50], col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_5,k=30)$dtaMEI[51:100], generate_indices(data_5,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_5,k=30)$dtaMMEI[1:50], generate_indices(data_5,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_5,k=30)$dtaMMEI[51:100], generate_indices(data_5,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_5,k=30)$ddtaEI[1:50],generate_indices(data_5,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$ddtaEI[51:100], generate_indices(data_5,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_5,k=30)$dtaMEI[1:50], generate_indices(data_5,k=30)$d2dtaMEI[1:50], col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_5,k=30)$dtaMEI[51:100], generate_indices(data_5,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_5,k=30)$ddtaMMEI[1:50], generate_indices(data_5,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_5,k=30)$ddtaMMEI[51:100], generate_indices(data_5,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_5,k=30)$d2dtaEI[1:50],generate_indices(data_5,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$d2dtaEI[51:100], generate_indices(data_5,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_5,k=30)$ddtaMEI[1:50], generate_indices(data_5,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_5,k=30)$ddtaMEI[51:100], generate_indices(data_5,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_5,k=30)$d2dtaMMEI[1:50], generate_indices(data_5,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_5,k=30)$d2dtaMMEI[51:100], generate_indices(data_5,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 1 and 6
plot(generate_indices(data_6, k=30)$dtaEI[1:50],generate_indices(data_6, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 6", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$dtaEI[51:100], generate_indices(data_6,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_6,k=30)$dtaMEI[1:50], generate_indices(data_6,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_6,k=30)$dtaMEI[51:100], generate_indices(data_6,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_6,k=30)$dtaMMEI[1:50], generate_indices(data_6,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_6,k=30)$dtaMMEI[51:100], generate_indices(data_6,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_6,k=30)$ddtaEI[1:50],generate_indices(data_6,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$ddtaEI[51:100], generate_indices(data_6,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_6,k=30)$dtaMEI[1:50], generate_indices(data_6,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_6,k=30)$dtaMEI[51:100], generate_indices(data_6,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_6,k=30)$ddtaMMEI[1:50], generate_indices(data_6,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_6,k=30)$ddtaMMEI[51:100], generate_indices(data_6,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_6,k=30)$d2dtaEI[1:50],generate_indices(data_6,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$d2dtaEI[51:100], generate_indices(data_6,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_6,k=30)$ddtaMEI[1:50], generate_indices(data_6,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_6,k=30)$ddtaMEI[51:100], generate_indices(data_6,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_6,k=30)$d2dtaMMEI[1:50], generate_indices(data_6,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_6,k=30)$d2dtaMMEI[51:100], generate_indices(data_6,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 1 and 7
plot(generate_indices(data_7, k=30)$dtaEI[1:50],generate_indices(data_7, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 7", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$dtaEI[51:100], generate_indices(data_7,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_7,k=30)$dtaMEI[1:50], generate_indices(data_7,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_7,k=30)$dtaMEI[51:100], generate_indices(data_7,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_7,k=30)$dtaMMEI[1:50], generate_indices(data_7,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_7,k=30)$dtaMMEI[51:100], generate_indices(data_7,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_7,k=30)$ddtaEI[1:50],generate_indices(data_7,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$ddtaEI[51:100], generate_indices(data_7,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_7,k=30)$dtaMEI[1:50], generate_indices(data_7,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main= "Original data and Second derivatives")
points(generate_indices(data_7,k=30)$dtaMEI[51:100], generate_indices(data_7,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_7,k=30)$ddtaMMEI[1:50], generate_indices(data_7,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_7,k=30)$ddtaMMEI[51:100], generate_indices(data_7,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_7,k=30)$d2dtaEI[1:50],generate_indices(data_7,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$d2dtaEI[51:100], generate_indices(data_7,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_7,k=30)$ddtaMEI[1:50], generate_indices(data_7,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_7,k=30)$ddtaMEI[51:100], generate_indices(data_7,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_7,k=30)$d2dtaMMEI[1:50], generate_indices(data_7,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_7,k=30)$d2dtaMMEI[51:100], generate_indices(data_7,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 1 and 8
plot(generate_indices(data_8, k=30)$dtaEI[1:50],generate_indices(data_8, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 8", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$dtaEI[51:100], generate_indices(data_8,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_8,k=30)$dtaMEI[1:50], generate_indices(data_8,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_8,k=30)$dtaMEI[51:100], generate_indices(data_8,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_8,k=30)$dtaMMEI[1:50], generate_indices(data_8,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_8,k=30)$dtaMMEI[51:100], generate_indices(data_8,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_8,k=30)$ddtaEI[1:50],generate_indices(data_8,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$ddtaEI[51:100], generate_indices(data_8,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_8,k=30)$dtaMEI[1:50], generate_indices(data_8,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_8,k=30)$dtaMEI[51:100], generate_indices(data_8,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_8,k=30)$ddtaMMEI[1:50], generate_indices(data_8,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_8,k=30)$ddtaMMEI[51:100], generate_indices(data_8,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_8,k=30)$d2dtaEI[1:50],generate_indices(data_8,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$d2dtaEI[51:100], generate_indices(data_8,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_8,k=30)$ddtaMEI[1:50], generate_indices(data_8,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_8,k=30)$ddtaMEI[51:100], generate_indices(data_8,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_8,k=30)$d2dtaMMEI[1:50], generate_indices(data_8,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_8,k=30)$d2dtaMMEI[51:100], generate_indices(data_8,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)


# model 1 and 9
plot(generate_indices(data_9, k=30)$dtaEI[1:50],generate_indices(data_9, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 9", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_9,k=30)$dtaEI[51:100], generate_indices(data_9,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$dtaMEI[1:50], generate_indices(data_9,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_9,k=30)$dtaMEI[51:100], generate_indices(data_9,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$dtaMMEI[1:50], generate_indices(data_9,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_9,k=30)$dtaMMEI[51:100], generate_indices(data_9,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_9,k=30)$ddtaEI[1:50],generate_indices(data_9,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_9,k=30)$ddtaEI[51:100], generate_indices(data_9,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$dtaMEI[1:50], generate_indices(data_9,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_9,k=30)$dtaMEI[51:100], generate_indices(data_9,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$ddtaMMEI[1:50], generate_indices(data_9,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_9,k=30)$ddtaMMEI[51:100], generate_indices(data_9,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_9,k=30)$d2dtaEI[1:50],generate_indices(data_9,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_9,k=30)$d2dtaEI[51:100], generate_indices(data_9,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$ddtaMEI[1:50], generate_indices(data_9,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_9,k=30)$ddtaMEI[51:100], generate_indices(data_9,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$d2dtaMMEI[1:50], generate_indices(data_9,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_9,k=30)$d2dtaMMEI[51:100], generate_indices(data_9,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 10 and 11
plot(generate_indices(data_11, k=30)$dtaEI[1:50],generate_indices(data_11, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 10 and 11", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_11,k=30)$dtaEI[51:100], generate_indices(data_11,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_11,k=30)$dtaMEI[1:50], generate_indices(data_11,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_11,k=30)$dtaMEI[51:100], generate_indices(data_11,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_11,k=30)$dtaMMEI[1:50], generate_indices(data_11,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_11,k=30)$dtaMMEI[51:100], generate_indices(data_11,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_11,k=30)$ddtaEI[1:50],generate_indices(data_11,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_11,k=30)$ddtaEI[51:100], generate_indices(data_11,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_11,k=30)$dtaMEI[1:50], generate_indices(data_11,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_11,k=30)$dtaMEI[51:100], generate_indices(data_11,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_11,k=30)$ddtaMMEI[1:50], generate_indices(data_11,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_11,k=30)$ddtaMMEI[51:100], generate_indices(data_11,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_11,k=30)$d2dtaEI[1:50],generate_indices(data_11,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_11,k=30)$d2dtaEI[51:100], generate_indices(data_11,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_11,k=30)$ddtaMEI[1:50], generate_indices(data_11,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_11,k=30)$ddtaMEI[51:100], generate_indices(data_11,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_11,k=30)$d2dtaMMEI[1:50], generate_indices(data_11,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_11,k=30)$d2dtaMMEI[51:100], generate_indices(data_11,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)

# model 10 and 12
plot(generate_indices(data_12, k=30)$dtaEI[1:50],generate_indices(data_12, k=30)$dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 10 and 12", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_12,k=30)$dtaEI[51:100], generate_indices(data_12,k=30)$dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_12,k=30)$dtaMEI[1:50], generate_indices(data_12,k=30)$ddtaMEI[1:50], col="red", ylab="dMEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and First derivative")
points(generate_indices(data_12,k=30)$dtaMEI[51:100], generate_indices(data_12,k=30)$ddtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_12,k=30)$dtaMMEI[1:50], generate_indices(data_12,k=30)$dtaMMHI[1:50], col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_12,k=30)$dtaMMEI[51:100], generate_indices(data_12,k=30)$dtaMMHI[51:100],
       col="blue",pch=20)

plot(generate_indices(data_12,k=30)$ddtaEI[1:50],generate_indices(data_12,k=30)$ddtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_12,k=30)$ddtaEI[51:100], generate_indices(data_12,k=30)$ddtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_12,k=30)$dtaMEI[1:50], generate_indices(data_12,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="Original data and Second derivative")
points(generate_indices(data_12,k=30)$dtaMEI[51:100], generate_indices(data_12,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_12,k=30)$ddtaMMEI[1:50], generate_indices(data_12,k=30)$ddtaMMHI[1:50],
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_12,k=30)$ddtaMMEI[51:100], generate_indices(data_12,k=30)$ddtaMMHI[51:100], 
       col="blue",pch=20)

plot(generate_indices(data_12,k=30)$d2dtaEI[1:50],generate_indices(data_12,k=30)$d2dtaHI[1:50], col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_12,k=30)$d2dtaEI[51:100], generate_indices(data_12,k=30)$d2dtaHI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_12,k=30)$ddtaMEI[1:50], generate_indices(data_12,k=30)$d2dtaMEI[1:50], col="red", ylab="d2MEI", xlab="dMEI", pch=10,
     xlim=c(0,1),ylim=c(0,1), main="First and Second derivatives")
points(generate_indices(data_12,k=30)$ddtaMEI[51:100], generate_indices(data_12,k=30)$d2dtaMEI[51:100], col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_12,k=30)$d2dtaMMEI[1:50], generate_indices(data_12,k=30)$d2dtaMMHI[1:50], 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_12,k=30)$d2dtaMMEI[51:100], generate_indices(data_12,k=30)$d2dtaMMHI[51:100], 
       col="blue",pch=20)
