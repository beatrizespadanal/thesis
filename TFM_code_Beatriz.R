#######################################
### TFM Beatriz Espadanal Gonçalves ### 
#######################################

#######################################
### INDEXES (1 dimension) ########################
#######################################

library(dplyr)
library(devtools)
install_github("bpulidob/ehymet")
library("ehymet")

# Data simulation for 1 dimension 


# curves: matrix of function evaluations on **equidistant** grid 
#       (grid in columns, curves in rows)
# output: MMEI

# New Modified Epigraph function 

MMEI <- function(curves) {
  index <- numeric()
  n_curves <- dim(curves)[1]
  lengthcurves <- dim(curves)[2]
  t_curves <- t(curves) # transpose so we can compute diffs all at once
  for(i in 1:n_curves){
    index[i] <- 0
    diffs <- t_curves[,-i] - t_curves[,i]
    diffs <- diffs * (diffs > 0)
    index[i] <- diffs |>  rowMeans() |> sum() }
  return(index)
}

# New Modified Hypograph function

MMHI <- function(curves) {
  return(MMEI(-curves))
}

# Epigraph function for 1 dimension

EI <- function(curves){
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  index <- apply(curves,1, function(y)
    sum(apply(curves,1,function(x)
      sum(x>=y)==l_curves)))/n_curves
  return (1-index)
}

# Hypograph function for 1 dimension 

HI <- function(curves){
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  index <- apply(curves,1, function(y)
    sum(apply(curves,1,function(x)
      sum(x<=y)==l_curves)))/n_curves
  return (index)
}

# Modified Epigraph function for 1 dimension 

MEI <- function(curves){
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  rankm <- apply(curves, 2, function(y) (rank(y, ties.method = "min")))
  n_a <- n_curves-rankm+1
  index <- rowSums(n_a)/(n_curves*l_curves)
  return(1-index)
}

# Modified Hypograph function for 1 dimension 

MHI <- function(curves){
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  rankm <- apply(curves, 2, function(y) (rank(y, ties.method = "max")))
  index <- rowSums(rankm)/(n_curves*l_curves)
  return(index)
}


# first things first: smooth the data and calculate the derivatives 

funspline <- function(curves, nbasis, norder, grid_ll = 0, grid_ul = 1, ...) {
  #Create B-spline basis
  basisobj <- fda::create.bspline.basis(rangeval = c(grid_ll, grid_ul),
                                        nbasis = nbasis, norder = norder, ...)
  
  curves_dim <- length(dim(curves))
  grid <- seq(from = grid_ll, to = grid_ul, length.out = dim(curves)[2])
  
  if(curves_dim == 2){
    
    # Smooth data using B-spline basis
    ys <-  fda::smooth.basis(argvals = grid, y = t(curves), fdParobj = basisobj)
    
    # Evaluate smoothed data and derivatives
    smooth <- t(fda::eval.fd(grid ,ys$fd,0)) # smoothed data
    deriv <- t(fda::eval.fd(grid ,ys$fd,1)) # first derivatives
    deriv2 <- t(fda::eval.fd(grid ,ys$fd,2)) # second derivatives
  } else if(curves_dim == 3){
    n_curves <- dim(curves)[1]
    l_curves <- dim(curves)[2]
    d_curves <- dim(curves)[3]
    
    # Initialize empty dataframes to store the results
    smooth <- array(rep(NaN,n_curves*l_curves),dim=c(n_curves,l_curves,d_curves))
    deriv <- array(rep(NaN,n_curves*l_curves),dim=c(n_curves,l_curves,d_curves))
    deriv2 <- array(rep(NaN,n_curves*l_curves),dim=c(n_curves,l_curves,d_curves))
    
    for(d in 1:dim(curves)[3]){
      # Smooth data using B-spline basis
      ys <-  fda::smooth.basis(argvals = grid, y = t(curves[,,d]),
                               fdParobj = basisobj)
      
      # Evaluate smoothed data and derivatives
      smooth[,,d] <- t(fda::eval.fd(grid ,ys$fd,0)) # smoothed data
      deriv[,,d] <- t(fda::eval.fd(grid ,ys$fd,1)) # first derivatives
      deriv2[,,d] <- t(fda::eval.fd(grid ,ys$fd,2)) # second derivatives
    }
  } else {
    stop("Invalid number of dimensions")
  }
  
  # Return a list containing the data and derivatives
  res <- list(
    "smooth" = smooth,
    "deriv" = deriv,
    "deriv2" = deriv2
  )
  
  return(res)
}


# Second: indexes 

ind <- function(curves, grid_ll = 0, grid_ul = 1, nbasis=25, norder=4,
                indices = c("EI", "HI", "MEI", "MHI", "MMEI", "MMHI"), ...){
  
  # define indices constant
  INDICES <- c("EI", "HI", "MEI", "MHI", "MMEI", "MMHI")
  curves_dim <- length(dim(curves))
  
  # stop conditions
  if (!(curves_dim %in% c(2, 3)) || is.null(curves_dim)) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  check_list_parameter(indices, INDICES, "indices")
  
  # Smoothed data and derivatives
  fun_data <- funspline(curves = curves, grid_ll = grid_ll,
                        grid_ul = grid_ul, nbasis = nbasis,
                        norder = norder, ...)
  
  # Initialize an empty data frame to store the results
  ind_data <- as.data.frame(matrix(NA, nrow = nrow(fun_data$smooth), ncol = 0))
  
  # Loop through the list of functions and apply them to the smoothed and
  # its first and second derivatives
  for (index in indices) {
    smooth_col <- paste0("dta", index)
    deriv_col  <- paste0("ddta", index)
    deriv2_col <- paste0("d2dta", index)
    
    smooth_result <- get(index)(fun_data$smooth)
    deriv_result  <- get(index)(fun_data$deriv)
    deriv2_result <- get(index)(fun_data$deriv2)
    
    ind_data <- cbind(ind_data,
                      stats::setNames(data.frame(smooth_result, deriv_result,
                                                 deriv2_result),
                                      c(smooth_col, deriv_col, deriv2_col)))
  }
  
  ind_data
}

result_to_table <- function(res, tl_null){
  name_res <- names(res)
  len_res <- length(name_res)
  if(tl_null){
    metrics_df <-
      data.frame(Time = sapply(1:len_res, function (i) res[[i]][[2]]))
    row.names(metrics_df) <- name_res
  } else{
    metrics_df <-
      data.frame(t(sapply(1:len_res, function (i) c(res[[i]][[2]],
                                                    "Time" = res[[i]][[3]]))))
    row.names(metrics_df) <- name_res
  }
  return(metrics_df)
}

check_list_parameter <- function(argument, parameter_values, parameter_name) {
  if (length(argument) == 0) {
    stop("parameter '", parameter_name, "' should have at least one element.", call. = FALSE)
  }
  
  if (any(duplicated(argument))) {
    stop("duplicated argument in '", parameter_name,"'.", call. = FALSE)
  }
  
  indices <- pmatch(argument, parameter_values)
  if (any(is.na(indices))) {
    stop("invalid argument in '", parameter_name, "': ", paste(argument[is.na(indices)], collapse = ", "), ".",
         call. = FALSE)
  }
}

#' Generate the name for the results of the clustering methods
#' @noRd
get_result_names <- function(method_name, parameter_combinations, vars_list) {
  args <- list(method_name)
  for (combination in parameter_combinations[-1]) {
    args <- append(args, list(combination))
  }
  
  args <- append(args, list(rep(sapply(vars_list, function(x) paste0(x, collapse = "")),
                                times = nrow(parameter_combinations) / length(vars_list))
  ))
  args[["sep"]] <- "_"
  do.call(paste, args)
}

#' Suppress outputs from cat (by Hadley Wickham)
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# Generate simulated data
data_1 <- sim_model_ex1(i_sim=1,seed=123) #model 1
data_2 <- sim_model_ex1(i_sim=2,seed=122) #model 2
data_3 <- sim_model_ex1(i_sim=3,seed=124) #model 3
data_4 <- sim_model_ex1(i_sim=4,seed=143) #model 4
data_5 <- sim_model_ex1(i_sim=5,seed=153) #model 5
data_6 <- sim_model_ex1(i_sim=6,seed=173) #model 6
data_7 <- sim_model_ex1(i_sim=7,seed=174) #model 7 
data_8 <- sim_model_ex1(i_sim=8,seed=187) #model 8 

# Smooth the data and calculate derivatives
t <- seq(0, 1, length = ncol(data_1))
smooth_data_1 <- funspline(curves = data_1, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)
smooth_data_2 <- funspline(curves = data_2, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)
smooth_data_3 <- funspline(curves = data_3, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)
smooth_data_4 <- funspline(curves = data_4, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)
smooth_data_5 <- funspline(curves = data_5, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)
smooth_data_6 <- funspline(curves = data_6, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)
smooth_data_7 <- funspline(curves = data_7, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)
smooth_data_8 <- funspline(curves = data_8, grid_ll = 0, grid_ul = 1, nbasis = 25, norder = 4)

# Calculate indexes
ind_data_1 <- ind(curves = data_1, grid_ll = 0, grid_ul = 1)
ind_data_2 <- ind(curves = data_2, grid_ll = 0, grid_ul = 1)
ind_data_3 <- ind(curves = data_3, grid_ll = 0, grid_ul = 1)
ind_data_4 <- ind(curves = data_4, grid_ll = 0, grid_ul = 1)
ind_data_5 <- ind(curves = data_5, grid_ll = 0, grid_ul = 1)
ind_data_6 <- ind(curves = data_6, grid_ll = 0, grid_ul = 1)
ind_data_7 <- ind(curves = data_7, grid_ll = 0, grid_ul = 1)
ind_data_8 <- ind(curves = data_8, grid_ll = 0, grid_ul = 1)

# results for Hypograph index
hi_data_1 <- ind_data_1$dtaHI
hi_data_2 <- ind_data_2$dtaHI
hi_data_3 <- ind_data_3$dtaHI
hi_data_4 <- ind_data_4$dtaHI
hi_data_5 <- ind_data_5$dtaHI
hi_data_6 <- ind_data_6$dtaHI
hi_data_7 <- ind_data_7$dtaHI
hi_data_8 <- ind_data_8$dtaHI

# results for the Epigraph index
ei_data_1 <- ind_data_1$dtaEI
ei_data_2 <- ind_data_2$dtaEI
ei_data_3 <- ind_data_3$dtaEI
ei_data_4 <- ind_data_4$dtaEI
ei_data_5 <- ind_data_5$dtaEI
ei_data_6 <- ind_data_6$dtaEI
ei_data_7 <- ind_data_7$dtaEI
ei_data_8 <- ind_data_8$dtaEI

# results for Modified Epigraph index
mei_data_1 <- ind_data_1$dtaMEI
mei_data_2 <- ind_data_2$dtaMEI
mei_data_3 <- ind_data_3$dtaMEI
mei_data_4 <- ind_data_4$dtaMEI
mei_data_5 <- ind_data_5$dtaMEI
mei_data_6 <- ind_data_6$dtaMEI
mei_data_7 <- ind_data_7$dtaMEI
mei_data_8 <- ind_data_8$dtaMEI

# results for Modified Hypograph index
mhi_data_1 <- ind_data_1$dtaMHI
mhi_data_2 <- ind_data_2$dtaMHI
mhi_data_3 <- ind_data_3$dtaMHI
mhi_data_4 <- ind_data_4$dtaMHI
mhi_data_5 <- ind_data_5$dtaMHI
mhi_data_6 <- ind_data_6$dtaMHI
mhi_data_7 <- ind_data_7$dtaMHI
mhi_data_8 <- ind_data_8$dtaMHI

# results for Modified Modified Epigraph index
mmei_data_1 <- ind_data_1$dtaMMEI
mmei_data_2 <- ind_data_2$dtaMMEI
mmei_data_3 <- ind_data_3$dtaMMEI
mmei_data_4 <- ind_data_4$dtaMMEI
mmei_data_5 <- ind_data_5$dtaMMEI
mmei_data_6 <- ind_data_6$dtaMMEI
mmei_data_7 <- ind_data_7$dtaMMEI
mmei_data_8 <- ind_data_8$dtaMMEI

# results for Modified Modified Hypograph index
mmhi_data_1 <- ind_data_1$dtaMMHI
mmhi_data_2 <- ind_data_2$dtaMMHI
mmhi_data_3 <- ind_data_3$dtaMMHI
mmhi_data_4 <- ind_data_4$dtaMMHI
mmhi_data_5 <- ind_data_5$dtaMMHI
mmhi_data_6 <- ind_data_6$dtaMMHI
mmhi_data_7 <- ind_data_7$dtaMMHI
mmhi_data_8 <- ind_data_8$dtaMMHI



# first derivatives indexes
ind_dat1_d <- ind(curves = smooth_data_1$deriv, grid_ll = 0, grid_ul = 1)
ind_dat2_d <- ind(curves = smooth_data_2$deriv, grid_ll = 0, grid_ul = 1)
ind_dat3_d <- ind(curves = smooth_data_3$deriv, grid_ll = 0, grid_ul = 1)
ind_dat4_d <- ind(curves = smooth_data_4$deriv, grid_ll = 0, grid_ul = 1)
ind_dat5_d <- ind(curves = smooth_data_5$deriv, grid_ll = 0, grid_ul = 1)
ind_dat6_d <- ind(curves = smooth_data_6$deriv, grid_ll = 0, grid_ul = 1)
ind_dat7_d <- ind(curves = smooth_data_7$deriv, grid_ll = 0, grid_ul = 1)
ind_dat8_d <- ind(curves = smooth_data_8$deriv, grid_ll = 0, grid_ul = 1)

# first derivatives epigraph 
ei_ddat_1 <- ind_dat1_d$dtaEI
ei_ddat_2 <- ind_dat2_d$dtaEI
ei_ddat_3 <- ind_dat3_d$dtaEI
ei_ddat_4 <- ind_dat4_d$dtaEI
ei_ddat_5 <- ind_dat5_d$dtaEI
ei_ddat_6 <- ind_dat6_d$dtaEI
ei_ddat_7 <- ind_dat7_d$dtaEI
ei_ddat_8 <- ind_dat8_d$dtaEI

# first derivatives hypograph
hi_ddat_1 <- ind_dat1_d$dtaHI
hi_ddat_2 <- ind_dat2_d$dtaHI
hi_ddat_3 <- ind_dat3_d$dtaHI
hi_ddat_4 <- ind_dat4_d$dtaHI
hi_ddat_5 <- ind_dat5_d$dtaHI
hi_ddat_6 <- ind_dat6_d$dtaHI
hi_ddat_7 <- ind_dat7_d$dtaHI
hi_ddat_8 <- ind_dat8_d$dtaHI

# first derivatives modified epigraph 
mei_ddat_1 <- ind_dat1_d$ddtaMEI
mei_ddat_2 <- ind_dat2_d$ddtaMEI
mei_ddat_3 <- ind_dat3_d$ddtaMEI
mei_ddat_4 <- ind_dat4_d$ddtaMEI
mei_ddat_5 <- ind_dat5_d$ddtaMEI
mei_ddat_6 <- ind_dat6_d$ddtaMEI
mei_ddat_7 <- ind_dat7_d$ddtaMEI
mei_ddat_8 <- ind_dat8_d$ddtaMEI

# first derivatives modified hypograph 
mhi_ddat_1 <- ind_dat1_d$ddtaMHI
mhi_ddat_2 <- ind_dat2_d$ddtaMHI
mhi_ddat_3 <- ind_dat3_d$ddtaMHI
mhi_ddat_4 <- ind_dat4_d$ddtaMHI
mhi_ddat_5 <- ind_dat5_d$ddtaMHI
mhi_ddat_6 <- ind_dat6_d$ddtaMHI
mhi_ddat_7 <- ind_dat7_d$ddtaMHI
mhi_ddat_8 <- ind_dat8_d$ddtaMHI

# first derivatives modified modified epigraph 
mmei_ddat_1 <- ind_dat1_d$ddtaMMEI
mmei_ddat_2 <- ind_dat2_d$ddtaMMEI
mmei_ddat_3 <- ind_dat3_d$ddtaMMEI
mmei_ddat_4 <- ind_dat4_d$ddtaMMEI
mmei_ddat_5 <- ind_dat5_d$ddtaMMEI
mmei_ddat_6 <- ind_dat6_d$ddtaMMEI
mmei_ddat_7 <- ind_dat7_d$ddtaMMEI
mmei_ddat_8 <- ind_dat8_d$ddtaMMEI

# first derivatives modified modified hypograph 
mmhi_ddat_1 <- ind_dat1_d$ddtaMMHI
mmhi_ddat_2 <- ind_dat2_d$ddtaMMHI
mmhi_ddat_3 <- ind_dat3_d$ddtaMMHI
mmhi_ddat_4 <- ind_dat4_d$ddtaMMHI
mmhi_ddat_5 <- ind_dat5_d$ddtaMMHI
mmhi_ddat_6 <- ind_dat6_d$ddtaMMHI
mmhi_ddat_7 <- ind_dat7_d$ddtaMMHI
mmhi_ddat_8 <- ind_dat8_d$ddtaMMHI

# second derivatives indexes
ind_dat1_dd <- ind(curves = smooth_data_1$deriv2, grid_ll = 0, grid_ul = 1)
ind_dat2_dd <- ind(curves = smooth_data_2$deriv2, grid_ll = 0, grid_ul = 1)
ind_dat3_dd <- ind(curves = smooth_data_3$deriv2, grid_ll = 0, grid_ul = 1)
ind_dat4_dd <- ind(curves = smooth_data_4$deriv2, grid_ll = 0, grid_ul = 1)
ind_dat5_dd <- ind(curves = smooth_data_5$deriv2, grid_ll = 0, grid_ul = 1)
ind_dat6_dd <- ind(curves = smooth_data_6$deriv2, grid_ll = 0, grid_ul = 1)
ind_dat7_dd <- ind(curves = smooth_data_7$deriv2, grid_ll = 0, grid_ul = 1)
ind_dat8_dd <- ind(curves = smooth_data_8$deriv2, grid_ll = 0, grid_ul = 1)

# second derivatives epigraph 
ei_d2dat_1 <- ind_dat1_dd$dtaEI
ei_d2dat_2 <- ind_dat2_dd$dtaEI
ei_d2dat_3 <- ind_dat3_dd$dtaEI
ei_d2dat_4 <- ind_dat4_dd$dtaEI
ei_d2dat_5 <- ind_dat5_dd$dtaEI
ei_d2dat_6 <- ind_dat6_dd$dtaEI
ei_d2dat_7 <- ind_dat7_dd$dtaEI
ei_d2dat_8 <- ind_dat8_dd$dtaEI

# second derivatives hypograph 
hi_d2dat_1 <- ind_dat1_dd$dtaHI
hi_d2dat_2 <- ind_dat2_dd$dtaHI
hi_d2dat_3 <- ind_dat3_dd$dtaHI
hi_d2dat_4 <- ind_dat4_dd$dtaHI
hi_d2dat_5 <- ind_dat5_dd$dtaHI
hi_d2dat_6 <- ind_dat6_dd$dtaHI
hi_d2dat_7 <- ind_dat7_dd$dtaHI
hi_d2dat_8 <- ind_dat8_dd$dtaHI

# second derivatives modified epigraph 
mei_d2dat_1 <- ind_dat1_dd$ddtaMEI
mei_d2dat_2 <- ind_dat2_dd$ddtaMEI
mei_d2dat_3 <- ind_dat3_dd$ddtaMEI
mei_d2dat_4 <- ind_dat4_dd$ddtaMEI
mei_d2dat_5 <- ind_dat5_dd$ddtaMEI
mei_d2dat_6 <- ind_dat6_dd$ddtaMEI
mei_d2dat_7 <- ind_dat7_dd$ddtaMEI
mei_d2dat_8 <- ind_dat8_dd$ddtaMEI

# second derivatives modified hypograph
mhi_d2dat_1 <- ind_dat1_dd$ddtaMHI
mhi_d2dat_2 <- ind_dat2_dd$ddtaMHI
mhi_d2dat_3 <- ind_dat3_dd$ddtaMHI
mhi_d2dat_4 <- ind_dat4_dd$ddtaMHI
mhi_d2dat_5 <- ind_dat5_dd$ddtaMHI
mhi_d2dat_6 <- ind_dat6_dd$ddtaMHI
mhi_d2dat_7 <- ind_dat7_dd$ddtaMHI
mhi_d2dat_8 <- ind_dat8_dd$ddtaMHI

# second derivatives modified modified epigraph 
mmei_d2dat_1 <- ind_dat1_dd$ddtaMMEI
mmei_d2dat_2 <- ind_dat2_dd$ddtaMMEI
mmei_d2dat_3 <- ind_dat3_dd$ddtaMMEI
mmei_d2dat_4 <- ind_dat4_dd$ddtaMMEI
mmei_d2dat_5 <- ind_dat5_dd$ddtaMMEI
mmei_d2dat_6 <- ind_dat6_dd$ddtaMMEI
mmei_d2dat_7 <- ind_dat7_dd$ddtaMMEI
mmei_d2dat_8 <- ind_dat8_dd$ddtaMMEI

# second derivatives modified modified hypograph 
mmhi_d2dat_1 <- ind_dat1_dd$ddtaMMHI
mmhi_d2dat_2 <- ind_dat2_dd$ddtaMMHI
mmhi_d2dat_3 <- ind_dat3_dd$ddtaMMHI
mmhi_d2dat_4 <- ind_dat4_dd$ddtaMMHI
mmhi_d2dat_5 <- ind_dat5_dd$ddtaMMHI
mmhi_d2dat_6 <- ind_dat6_dd$ddtaMMHI
mmhi_d2dat_7 <- ind_dat7_dd$ddtaMMHI
mmhi_d2dat_8 <- ind_dat8_dd$ddtaMMHI

#############################
# plots: 
#############################
library(dplyr)
par(mfrow=c(4,3))

plt_fun <- function(data, true_labels){
  
  if (!(length(dim(data)) == 2))
    stop("This function can be only used with 2-dimensional datasets.")
  
  df <-  dplyr::as_tibble(data)
  t_interval <- seq(0, 1, length = ncol(data))
  names(df) <- as.character(t_interval)
  df$id <- 1:nrow(data)
  df$Order <- true_labels
  df_long<- df %>% tidyr::pivot_longer(-c(id, Order), names_to="variable", values_to="values") %>%
    dplyr::mutate(variable=as.numeric(variable))
  pa <- df_long %>% ggplot2::ggplot(ggplot2::aes(x=variable, y=values,group=id, color=factor(Order)))
  
  plt<- pa +
    ggplot2::geom_line(linewidth=0.1)+
    ggplot2::scale_color_brewer(palette = "Set1")+
    # scale_color_manual(values=c("#CC6600","#3399FF")) +
    # ggtitle("MEI. First dimension")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::theme(legend.position = "none")
  return(plt)
}

true_labels1 <- c(rep(1,100)) # ? 
#############################
# Model 1 vs. Model 2
#############################
true_labels2 <- c(rep(1,50), rep(2,50))
plt_fun(data_2, true_labels2)

par(mfrow=c(3,3))
plot(ei_data_1, hi_data_1, col="pink", ylab="HI", xlab="EI",pch=16, 
     main = "Original Data")
points(ei_data_2,hi_data_2, col="lightblue",pch=16)

plot(mei_data_1, mhi_data_1, col ="pink", pch=16, ylab = "MHI", xlab="MEI")
points(mei_data_2,mhi_data_2, col = "lightblue", pch=16, ylab="MHI", xlab="MEI")

plot(mmei_data_1,mmhi_data_1, col = "pink", pch=16, ylab="MMHI", xlab = "MMEI")
points(mmei_data_2,mmhi_data_2, col="lightblue", pch = 16)

#first derivative 
#plt_fun(smooth_data_2$deriv, true_labels2)
#plot(smooth_data_1$deriv, smooth_data_2$deriv, col = c("pink", "lightblue"),
#     type = "l", main = "First Derivatives", xlab = "t", ylab="")

plot(ei_ddat_1,hi_ddat_1,col="pink", pch=16, xlab="EI", ylab="HI", 
     main = "First Derivatives")
points(ei_ddat_2,hi_ddat_2,col="lightblue", pch=16)

plot(mei_ddat_1,mhi_ddat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_ddat_2,mhi_ddat_2,col="lightblue", pch=16)

plot(mmei_ddat_1,mmhi_ddat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_ddat_2,mmhi_ddat_2,col="lightblue", pch=16)

#second derivative
#plt_fun(smooth_data_2$deriv2,true_labels2)
#plot(smooth_data_1$deriv2, smooth_data_2$deriv2, col = c("pink", "lightblue"),
#     type = "l", main="Second Derivatives",ylab = "",xlab = "t")

plot(ei_d2dat_1,hi_d2dat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "Second Derivatives")
points(ei_d2dat_2,hi_d2dat_2,col="lightblue", pch=16)

plot(mei_d2dat_1,mhi_d2dat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_d2dat_2,mhi_d2dat_2,col="lightblue", pch=16)

plot(mmei_d2dat_1,mmhi_d2dat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_d2dat_2,mmhi_d2dat_2,col="lightblue", pch=16)

#############################
# Model 1 vs. Model 3
#############################

true_labels3 <- c(rep(1,50), rep(3,50))
plt_fun(data_3, true_labels3)

par(mfrow=c(3,3))

plot(ei_data_1, hi_data_1, col="pink", ylab="HI", xlab="EI",pch=16,
     main = "Original Data")
points(ei_data_3,hi_data_3, col="lightblue",pch=16)

plot(mei_data_1, mhi_data_1, col ="pink", pch=16, ylab = "MHI", xlab="MEI")
points(mei_data_3,mhi_data_3, col = "lightblue", pch=16, ylab="MHI", xlab="MEI")

plot(mmei_data_1,mmhi_data_1, col = "pink", pch=16, ylab="MMHI", xlab = "MMEI")
points(mmei_data_3,mmhi_data_3, col="lightblue", pch = 16)

#first derivative 
#plot(smooth_data_1$deriv, smooth_data_3$deriv, col = c("pink", "lightblue"),
#     type = "l", main = "First Derivatives", xlab = "t", ylab="")

plot(ei_ddat_1,hi_ddat_1,col="pink", pch=16, xlab="EI", ylab="HI", 
     main = "First Derivatives")
points(ei_ddat_3,hi_ddat_3,col="lightblue", pch=16)

plot(mei_ddat_1,mhi_ddat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_ddat_3,mhi_ddat_3,col="lightblue", pch=16)

plot(mmei_ddat_1,mmhi_ddat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_ddat_3,mmhi_ddat_3,col="lightblue", pch=16)

#second derivative
#plot(smooth_data_1$deriv2, smooth_data_3$deriv2, col = c("pink", "lightblue"),
#     type = "l", main="Second Derivatives",ylab = "",xlab = "t")

plot(ei_d2dat_1,hi_d2dat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "Second Derivatives")
points(ei_d2dat_3,hi_d2dat_3,col="lightblue", pch=16)

plot(mei_d2dat_1,mhi_d2dat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_d2dat_3,mhi_d2dat_3,col="lightblue", pch=16)

plot(mmei_d2dat_1,mmhi_d2dat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_d2dat_3,mmhi_d2dat_3,col="lightblue", pch=16)

#############################
# Model 1 vs. Model 4
#############################

true_labels4 <- c(rep(1,50), rep(4,50))
plt_fun(data_4, true_labels4)

plot(ei_data_1, hi_data_1, col="pink", ylab="HI", xlab="EI",pch=16,
     main = "Original Data")
points(ei_data_4,hi_data_4, col="lightblue",pch=16)

plot(mei_data_1, mhi_data_1, col ="pink", pch=16, ylab = "MHI", xlab="MEI")
points(mei_data_4,mhi_data_4, col = "lightblue", pch=16, ylab="MHI", xlab="MEI")

plot(mmei_data_1,mmhi_data_1, col = "pink", pch=16, ylab="MMHI", xlab = "MMEI")
points(mmei_data_4,mmhi_data_4, col="lightblue", pch = 16)

#first derivative 
#plot(smooth_data_1$deriv, smooth_data_4$deriv, col = c("pink", "lightblue"),
#     type = "l", main = "First Derivatives", xlab = "t", ylab="")

plot(ei_ddat_1,hi_ddat_1,col="pink", pch=16, xlab="EI", ylab="HI", 
     main = "First Derivatives")
points(ei_ddat_4,hi_ddat_4,col="lightblue", pch=16)

plot(mei_ddat_1,mhi_ddat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_ddat_4,mhi_ddat_4,col="lightblue", pch=16)

plot(mmei_ddat_1,mmhi_ddat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_ddat_4,mmhi_ddat_4,col="lightblue", pch=16)

#second derivative
#plot(smooth_data_1$deriv2, smooth_data_4$deriv2, col = c("pink", "lightblue"),
#     type = "l", main="Second Derivatives",ylab = "",xlab = "t")

plot(ei_d2dat_1,hi_d2dat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "Second Derivatives")
points(ei_d2dat_4,hi_d2dat_4,col="lightblue", pch=16)

plot(mei_d2dat_1,mhi_d2dat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_d2dat_4,mhi_d2dat_4,col="lightblue", pch=16)

plot(mmei_d2dat_1,mmhi_d2dat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_d2dat_4,mmhi_d2dat_4,col="lightblue", pch=16)

#############################
# Model 1 vs. Model 5
#############################

true_labels5 <- c(rep(1,50), rep(5,50))
plt_fun(data_5, true_labels5)

plot(ei_data_1, hi_data_1, col="pink", ylab="HI", xlab="EI",pch=16, 
     main = "Original Data")
points(ei_data_5,hi_data_5, col="lightblue",pch=16)

plot(mei_data_1, mhi_data_1, col ="pink", pch=16, ylab = "MHI", xlab="MEI")
points(mei_data_5,mhi_data_5, col = "lightblue", pch=16, ylab="MHI", xlab="MEI")

plot(mmei_data_1,mmhi_data_1, col = "pink", pch=16, ylab="MMHI", xlab = "MMEI")
points(mmei_data_5,mmhi_data_5, col="lightblue", pch = 16)

#first derivative 
#plot(smooth_data_1$deriv, smooth_data_5$deriv, col = c("pink", "lightblue"),
#     type = "l", main = "First Derivatives", xlab = "t", ylab="")

plot(ei_ddat_1,hi_ddat_1,col="pink", pch=16, xlab="EI", ylab="HI", 
     main = "First Derivative")
points(ei_ddat_5,hi_ddat_5,col="lightblue", pch=16)

plot(mei_ddat_1,mhi_ddat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_ddat_5,mhi_ddat_5,col="lightblue", pch=16)

plot(mmei_ddat_1,mmhi_ddat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_ddat_5,mmhi_ddat_5,col="lightblue", pch=16)

#second derivative
#plot(smooth_data_1$deriv2, smooth_data_5$deriv2, col = c("pink", "lightblue"),
#     type = "l", main="Second Derivatives",ylab = "",xlab = "t")

plot(ei_d2dat_1,hi_d2dat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "Second Derivative")
points(ei_d2dat_5,hi_d2dat_5,col="lightblue", pch=16)

plot(mei_d2dat_1,mhi_d2dat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_d2dat_5,mhi_d2dat_5,col="lightblue", pch=16)

plot(mmei_d2dat_1,mmhi_d2dat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_d2dat_5,mmhi_d2dat_5,col="lightblue", pch=16)

#############################
# Model 1 vs. Model 6
#############################

true_labels6 <- c(rep(1,50), rep(6,50))
plt_fun(data_6, true_labels6)

plot(ei_data_1, hi_data_1, col="pink", ylab="HI", xlab="EI",pch=16, 
     main = "Original Data")
points(ei_data_6,hi_data_6, col="lightblue",pch=16)

plot(mei_data_1, mhi_data_1, col ="pink", pch=16, ylab = "MHI", xlab="MEI")
points(mei_data_6,mhi_data_6, col = "lightblue", pch=16, ylab="MHI", xlab="MEI")

plot(mmei_data_1,mmhi_data_1, col = "pink", pch=16, ylab="MMHI", xlab = "MMEI")
points(mmei_data_6,mmhi_data_6, col="lightblue", pch = 16)

#first derivative 
#plot(smooth_data_1$deriv, smooth_data_6$deriv, col = c("pink", "lightblue"),
#     type = "l", main = "First Derivatives", xlab = "t", ylab="")

plot(ei_ddat_1,hi_ddat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "First Derivative")
points(ei_ddat_6,hi_ddat_6,col="lightblue", pch=16)

plot(mei_ddat_1,mhi_ddat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_ddat_6,mhi_ddat_6,col="lightblue", pch=16)

plot(mmei_ddat_1,mmhi_ddat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_ddat_6,mmhi_ddat_6,col="lightblue", pch=16)

#second derivative
#plot(smooth_data_1$deriv2, smooth_data_6$deriv2, col = c("pink", "lightblue"),
#     type = "l", main="Second Derivatives",ylab = "",xlab = "t")

plot(ei_d2dat_1,hi_d2dat_1,col="pink", pch=16, xlab="EI", ylab="HI", 
     main = "Second Derivative")
points(ei_d2dat_6,hi_d2dat_6,col="lightblue", pch=16)

plot(mei_d2dat_1,mhi_d2dat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_d2dat_6,mhi_d2dat_6,col="lightblue", pch=16)

plot(mmei_d2dat_1,mmhi_d2dat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_d2dat_6,mmhi_d2dat_6,col="lightblue", pch=16)

#############################
# Model 1 vs. Model 7
#############################

true_labels7 <- c(rep(1,50), rep(7,50))
plt_fun(data_7, true_labels7)

plot(ei_data_1, hi_data_1, col="pink", ylab="HI", xlab="EI",pch=16,
     main = "Original Data") 
points(ei_data_7,hi_data_7, col="lightblue",pch=16)

plot(mei_data_1, mhi_data_1, col ="pink", pch=16, ylab = "MHI", xlab="MEI")
points(mei_data_7,mhi_data_7, col = "lightblue", pch=16, ylab="MHI", xlab="MEI")

plot(mmei_data_1,mmhi_data_1,
     col = "pink", pch=16, ylab="MMHI", xlab = "MMEI")
points(mmei_data_7,mmhi_data_7, col="lightblue", pch = 16)

#first derivative 
#plot(smooth_data_1$deriv, smooth_data_7$deriv, col = c("pink", "lightblue"),
#     type = "l", main = "First Derivatives", xlab = "t", ylab="")

plot(ei_ddat_1,hi_ddat_1,col="pink", pch=16, xlab="EI", ylab="HI", 
     main = "First Derivative")
points(ei_ddat_7,hi_ddat_7,col="lightblue", pch=16)

plot(mei_ddat_1,mhi_ddat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_ddat_7,mhi_ddat_7,col="lightblue", pch=16)

plot(mmei_ddat_1,mmhi_ddat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_ddat_7,mmhi_ddat_7,col="lightblue", pch=16)

#second derivative
#plot(smooth_data_1$deriv2, smooth_data_7$deriv2, col = c("pink", "lightblue"),
#     type = "l", main="Second Derivatives",ylab = "",xlab = "t")

plot(ei_d2dat_1,hi_d2dat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "Second Derivative")
points(ei_d2dat_7,hi_d2dat_7,col="lightblue", pch=16)

plot(mei_d2dat_1,mhi_d2dat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_d2dat_7,mhi_d2dat_7,col="lightblue", pch=16)

plot(mmei_d2dat_1,mmhi_d2dat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_d2dat_7,mmhi_d2dat_7,col="lightblue", pch=16)

#############################
#Model 1 vs. Model 8
#############################

true_labels8 <- c(rep(1,50), rep(8,50))
plt_fun(data_8, true_labels8)

plot(ei_data_1, hi_data_1, col="pink", ylab="HI", xlab="EI",pch=16, 
     main = "Original Data")
points(ei_data_8,hi_data_8, col="lightblue",pch=16)

plot(mei_data_1, mhi_data_1, col ="pink",pch=16, ylab = "MHI", xlab="MEI")
points(mei_data_8,mhi_data_8, col = "lightblue", pch=16, ylab="MHI", xlab="MEI")

plot(mmei_data_1,mmhi_data_1, col = "pink", pch=16, ylab="MMHI", xlab = "MMEI")
points(mmei_data_8,mmhi_data_8, col="lightblue", pch = 16)

#first derivative 
#plot(smooth_data_1$deriv, smooth_data_8$deriv, col = c("pink", "lightblue"),
#     type = "l", main = "First Derivatives", xlab = "t", ylab="")

plot(ei_ddat_1,hi_ddat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "First Derivative")
points(ei_ddat_8,hi_ddat_8,col="lightblue", pch=16)

plot(mei_ddat_1,mhi_ddat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_ddat_8,mhi_ddat_8,col="lightblue", pch=16)

plot(mmei_ddat_1,mmhi_ddat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_ddat_8,mmhi_ddat_8,col="lightblue", pch=16)

#second derivative
#plot(smooth_data_1$deriv2, smooth_data_8$deriv2, col = c("pink", "lightblue"),
#     type = "l", main="Second Derivatives",ylab = "",xlab = "t")

plot(ei_d2dat_1,hi_d2dat_1,col="pink", pch=16, xlab="EI", ylab="HI",
     main = "Second Derivative")
points(ei_d2dat_8,hi_d2dat_8,col="lightblue", pch=16)

plot(mei_d2dat_1,mhi_d2dat_1,col="pink", pch=16, xlab="MEI", ylab="MHI")
points(mei_d2dat_8,mhi_d2dat_8,col="lightblue", pch=16)

plot(mmei_d2dat_1,mmhi_d2dat_1,col="pink", pch=16, xlab="MMEI", ylab="MMHI")
points(mmei_d2dat_8,mmhi_d2dat_8,col="lightblue", pch=16)


