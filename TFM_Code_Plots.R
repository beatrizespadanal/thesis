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
    # ggtitle("MEI. First dimension")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::theme(legend.position = "none")
  return(plt)
}

# curves
# original 
plot1 <- plt_fun(data_1, data1_labels)
plot3 <- plt_fun(data_3, data3_labels)
plot4 <- plt_fun(data_4, data4_labels)
plot5 <- plt_fun(data_5, data5_labels)
plot6 <- plt_fun(data_6, data6_labels)
plot7 <- plt_fun(data_7, data7_labels)
plot8 <- plt_fun(data_8, data8_labels)
plot10 <- plt_fun(data_10, data10_labels)

# first derivative 
data_1d <- funspline(data_1,k=30)$deriv
data_3d <- funspline(data_3,k=30)$deriv
data_4d <- funspline(data_4,k=30)$deriv
data_5d <- funspline(data_5,k=30)$deriv
data_6d <- funspline(data_6,k=30)$deriv
data_7d <- funspline(data_7,k=30)$deriv
data_8d <- funspline(data_8,k=30)$deriv
data_10d <- funspline(data_10,k=30)$deriv

plot1_d <- plt_fun(data_1d,data1_labels)
plot3_d <- plt_fun(data_3d,data3_labels)
plot4_d <- plt_fun(data_4d,data4_labels)
plot5_d <- plt_fun(data_5d,data5_labels)
plot6_d <- plt_fun(data_6d,data6_labels)
plot7_d <- plt_fun(data_7d,data7_labels)
plot8_d <- plt_fun(data_8d,data8_labels)
plot10_d <- plt_fun(data_10d,data10_labels)

# second derivative 
data_1d2 <- funspline(data_1,k=30)$deriv2
data_3d2 <- funspline(data_3,k=30)$deriv2
data_4d2 <- funspline(data_4,k=30)$deriv2
data_5d2 <- funspline(data_5,k=30)$deriv2
data_6d2 <- funspline(data_6,k=30)$deriv2
data_7d2 <- funspline(data_7,k=30)$deriv2
data_8d2 <- funspline(data_8,k=30)$deriv2
data_9d2 <- funspline(data_9,k=30)$deriv2
data_10d2 <- funspline(data_10,k=30)$deriv2

plot1_dd <- plt_fun(data_1d2,data1_labels)
plot3_dd <- plt_fun(data_3d2,data3_labels)
plot4_dd <- plt_fun(data_4d2,data4_labels)
plot5_dd <- plt_fun(data_5d2,data5_labels)
plot6_dd <- plt_fun(data_6d2,data6_labels)
plot7_dd <- plt_fun(data_7d2,data7_labels)
plot8_dd <- plt_fun(data_8d2,data8_labels)
plot9_dd <- plt_fun(data_9d2,data9_labels)
plot10_dd <- plt_fun(data_10d2,data10_labels)

graphics.off()
grid.arrange(plot1, plot1_d, plot1_dd, ncol=3)
grid.arrange(plot3, plot3_d, plot3_dd, ncol=3)
grid.arrange(plot4, plot4_d, plot4_dd, ncol=3)
grid.arrange(plot5, plot5_d, plot5_dd, ncol=3)
grid.arrange(plot6, plot6_d, plot6_dd, ncol=3)
grid.arrange(plot7, plot7_d, plot7_dd, ncol=3)
grid.arrange(plot8, plot8_d, plot8_dd, ncol=3)
grid.arrange(plot9, plot9_d, plot9_dd, ncol=3)
grid.arrange(plot10, plot10_d, plot10_dd, ncol=3)

# scatter plots for EI, MEI, MMEI, HI, MHI and MMHI
par(mfrow=c(3,3))

# model 1 and 2
plot(generate_indices(data_1, k=30)$dtaEI,generate_indices(data_1, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 2", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$dtaEI, generate_indices(data_2,k=30)$dtaHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMEI, generate_indices(data_1,k=30)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$dtaMEI, generate_indices(data_2,k=30)$dtaMHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMMEI, generate_indices(data_1,k=30)$dtaMMHI, col="red", 
     ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_2,k=30)$dtaMMEI, generate_indices(data_2,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$ddtaEI,generate_indices(data_1,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$ddtaEI, generate_indices(data_2,k=30)$ddtaHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMEI, generate_indices(data_1,k=30)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$ddtaMEI, generate_indices(data_2,k=30)$ddtaMHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMMEI, generate_indices(data_1,k=30)$ddtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_2,k=30)$ddtaMMEI, generate_indices(data_2,k=30)$ddtaMMHI, 
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$d2dtaEI,generate_indices(data_1,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$d2dtaEI, generate_indices(data_2,k=30)$d2dtaHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMEI, generate_indices(data_1,k=30)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_2,k=30)$d2dtaMEI, generate_indices(data_2,k=30)$d2dtaMHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMMEI, generate_indices(data_1,k=30)$d2dtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_2,k=30)$d2dtaMMEI, generate_indices(data_2,k=30)$d2dtaMMHI, 
       col="blue",pch=20)

# model 1 and 3
plot(generate_indices(data_1, k=30)$dtaEI,generate_indices(data_1, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 3", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$dtaEI, generate_indices(data_3,k=30)$dtaHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMEI, generate_indices(data_1,k=30)$dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$dtaMEI, generate_indices(data_3,k=30)$dtaMHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMMEI, generate_indices(data_1,k=30)$dtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_3,k=30)$dtaMMEI, generate_indices(data_3,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$ddtaEI,generate_indices(data_1,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$ddtaEI, generate_indices(data_3,k=30)$ddtaHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMEI, generate_indices(data_1,k=30)$ddtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$ddtaMEI, generate_indices(data_3,k=30)$ddtaMHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMMEI, generate_indices(data_1,k=30)$ddtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_3,k=30)$ddtaMMEI, generate_indices(data_3,k=30)$ddtaMMHI, 
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$d2dtaEI,generate_indices(data_1,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$d2dtaEI, generate_indices(data_3,k=30)$d2dtaHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMEI, generate_indices(data_1,k=30)$d2dtaMHI, col="red", ylab="MHI", xlab="MEI", pch=10,
     xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_3,k=30)$d2dtaMEI, generate_indices(data_3,k=30)$d2dtaMHI, col="blue",pch=20,
       xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMMEI, generate_indices(data_1,k=30)$d2dtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_3,k=30)$d2dtaMMEI, generate_indices(data_3,k=30)$d2dtaMMHI, 
       col="blue",pch=20)

# model 1 and 4
plot(generate_indices(data_1, k=30)$dtaEI,generate_indices(data_1, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 4", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$dtaEI, generate_indices(data_4,k=30)$dtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMEI, generate_indices(data_1,k=30)$dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10, xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$dtaMEI, generate_indices(data_4,k=30)$dtaMHI,
      col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMMEI, generate_indices(data_1,k=30)$dtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_4,k=30)$dtaMMEI, generate_indices(data_4,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$ddtaEI,generate_indices(data_1,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$ddtaEI, generate_indices(data_4,k=30)$ddtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMEI, generate_indices(data_1,k=30)$ddtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10, xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$ddtaMEI, generate_indices(data_4,k=30)$ddtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMMEI, generate_indices(data_1,k=30)$ddtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_4,k=30)$ddtaMMEI, generate_indices(data_4,k=30)$ddtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$d2dtaEI,generate_indices(data_1,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$d2dtaEI, generate_indices(data_4,k=30)$d2dtaHI, 
       col="blue",pch=20, xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMEI, generate_indices(data_1,k=30)$d2dtaMHI, 
     col="red", ylab="MHI", xlab="MEI", pch=10, xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_4,k=30)$d2dtaMEI, generate_indices(data_4,k=30)$d2dtaMHI,
       col="blue",pch=20, xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMMEI, generate_indices(data_1,k=30)$d2dtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_4,k=30)$d2dtaMMEI, generate_indices(data_4,k=30)$d2dtaMMHI,
       col="blue",pch=20)

# model 1 and 5
plot(generate_indices(data_1, k=30)$dtaEI,generate_indices(data_1, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 5",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$dtaEI, generate_indices(data_5,k=30)$dtaHI, 
       col="blue",pch=20, xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMEI, generate_indices(data_1,k=30)$dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10, xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$dtaMEI, generate_indices(data_5,k=30)$dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMMEI, generate_indices(data_1,k=30)$dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_5,k=30)$dtaMMEI, generate_indices(data_5,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$ddtaEI,generate_indices(data_1,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$ddtaEI, generate_indices(data_5,k=30)$ddtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMEI, generate_indices(data_1,k=30)$ddtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$ddtaMEI, generate_indices(data_5,k=30)$ddtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMMEI, generate_indices(data_1,k=30)$ddtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_5,k=30)$ddtaMMEI, generate_indices(data_5,k=30)$ddtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$d2dtaEI,generate_indices(data_1,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$d2dtaEI, generate_indices(data_5,k=30)$d2dtaHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMEI, generate_indices(data_1,k=30)$d2dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_5,k=30)$d2dtaMEI, generate_indices(data_5,k=30)$d2dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMMEI, generate_indices(data_1,k=30)$d2dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_5,k=30)$d2dtaMMEI, generate_indices(data_5,k=30)$d2dtaMMHI,
       col="blue",pch=20)

# model 1 and 6
plot(generate_indices(data_1, k=30)$dtaEI,generate_indices(data_1, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 6", xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$dtaEI, generate_indices(data_6,k=30)$dtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMEI, generate_indices(data_1,k=30)$dtaMHI, 
     col="red", ylab="MHI", xlab="MEI", pch=10, xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$dtaMEI, generate_indices(data_6,k=30)$dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMMEI, generate_indices(data_1,k=30)$dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_6,k=30)$dtaMMEI, generate_indices(data_6,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$ddtaEI,generate_indices(data_1,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$ddtaEI, generate_indices(data_6,k=30)$ddtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMEI, generate_indices(data_1,k=30)$ddtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$ddtaMEI, generate_indices(data_6,k=30)$ddtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMMEI, generate_indices(data_1,k=30)$ddtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_6,k=30)$ddtaMMEI, generate_indices(data_6,k=30)$ddtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$d2dtaEI,generate_indices(data_1,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$d2dtaEI, generate_indices(data_6,k=30)$d2dtaHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMEI, generate_indices(data_1,k=30)$d2dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_6,k=30)$d2dtaMEI, generate_indices(data_6,k=30)$d2dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMMEI, generate_indices(data_1,k=30)$d2dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_6,k=30)$d2dtaMMEI, generate_indices(data_6,k=30)$d2dtaMMHI,
       col="blue",pch=20)

# model 1 and 7
plot(generate_indices(data_1, k=30)$dtaEI,generate_indices(data_1, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 7",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$dtaEI, generate_indices(data_7,k=30)$dtaHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMEI, generate_indices(data_1,k=30)$dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$dtaMEI, generate_indices(data_7,k=30)$dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMMEI, generate_indices(data_1,k=30)$dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_7,k=30)$dtaMMEI, generate_indices(data_7,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$ddtaEI,generate_indices(data_1,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$ddtaEI, generate_indices(data_7,k=30)$ddtaHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMEI, generate_indices(data_1,k=30)$ddtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$ddtaMEI, generate_indices(data_7,k=30)$ddtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMMEI, generate_indices(data_1,k=30)$ddtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_7,k=30)$ddtaMMEI, generate_indices(data_7,k=30)$ddtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$d2dtaEI,generate_indices(data_1,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$d2dtaEI, generate_indices(data_7,k=30)$d2dtaHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMEI, generate_indices(data_1,k=30)$d2dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_7,k=30)$d2dtaMEI, generate_indices(data_7,k=30)$d2dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMMEI, generate_indices(data_1,k=30)$d2dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_7,k=30)$d2dtaMMEI, generate_indices(data_7,k=30)$d2dtaMMHI,
       col="blue",pch=20)

# model 1 and 8
plot(generate_indices(data_1, k=30)$dtaEI,generate_indices(data_1, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 1 and 8",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$dtaEI, generate_indices(data_8,k=30)$dtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMEI, generate_indices(data_1,k=30)$dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$dtaMEI, generate_indices(data_8,k=30)$dtaMHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$dtaMMEI, generate_indices(data_1,k=30)$dtaMMHI, 
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_8,k=30)$dtaMMEI, generate_indices(data_8,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$ddtaEI,generate_indices(data_1,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$ddtaEI, generate_indices(data_8,k=30)$ddtaHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMEI, generate_indices(data_1,k=30)$ddtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$ddtaMEI, generate_indices(data_8,k=30)$ddtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$ddtaMMEI, generate_indices(data_1,k=30)$ddtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_8,k=30)$ddtaMMEI, generate_indices(data_8,k=30)$ddtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_1,k=30)$d2dtaEI,generate_indices(data_1,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$d2dtaEI, generate_indices(data_8,k=30)$d2dtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMEI, generate_indices(data_1,k=30)$d2dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_8,k=30)$d2dtaMEI, generate_indices(data_8,k=30)$d2dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_1,k=30)$d2dtaMMEI, generate_indices(data_1,k=30)$d2dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_8,k=30)$d2dtaMMEI, generate_indices(data_8,k=30)$d2dtaMMHI,
       col="blue",pch=20)

# model 9 and 10
plot(generate_indices(data_9, k=30)$dtaEI,generate_indices(data_9, k=30)$dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Samples 9 and 10",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_10,k=30)$dtaEI, generate_indices(data_10,k=30)$dtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$dtaMEI, generate_indices(data_9,k=30)$dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_10,k=30)$dtaMEI, generate_indices(data_10,k=30)$dtaMHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$dtaMMEI, generate_indices(data_9,k=30)$dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_10,k=30)$dtaMMEI, generate_indices(data_10,k=30)$dtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_9,k=30)$ddtaEI,generate_indices(data_9,k=30)$ddtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "First Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_10,k=30)$ddtaEI, generate_indices(data_10,k=30)$ddtaHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$ddtaMEI, generate_indices(data_9,k=30)$ddtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_10,k=30)$ddtaMEI, generate_indices(data_10,k=30)$ddtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$ddtaMMEI, generate_indices(data_9,k=30)$ddtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_10,k=30)$ddtaMMEI, generate_indices(data_10,k=30)$ddtaMMHI,
       col="blue",pch=20)

plot(generate_indices(data_9,k=30)$d2dtaEI,generate_indices(data_9,k=30)$d2dtaHI, col="red", ylab="HI", xlab="EI",pch=10, 
     main = "Second Derivatives",xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_10,k=30)$d2dtaEI, generate_indices(data_10,k=30)$d2dtaHI, 
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$d2dtaMEI, generate_indices(data_9,k=30)$d2dtaMHI,
     col="red", ylab="MHI", xlab="MEI", pch=10,xlim=c(0,1),ylim=c(0,1))
points(generate_indices(data_10,k=30)$d2dtaMEI, generate_indices(data_10,k=30)$d2dtaMHI,
       col="blue",pch=20,xlim=c(0,1),ylim=c(0,1))
plot(generate_indices(data_9,k=30)$d2dtaMMEI, generate_indices(data_9,k=30)$d2dtaMMHI,
     col="red", ylab="MMHI", xlab="MMEI", pch=10)
points(generate_indices(data_10,k=30)$d2dtaMMEI, generate_indices(data_10,k=30)$d2dtaMMHI,
       col="blue",pch=20)
