library("ehymet")

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

check_vars_combinations <- function(vars_combinations, ind_curves) {
  vars_combinations_to_remove <- c()
  
  vars_empty           <- c()
  vars_invalid_name    <- c()
  vars_almost_singular <- c()
  
  
  for (i in seq_along(vars_combinations)) {
    if (length(vars_combinations[[i]]) == 0) {
      vars_combinations_to_remove <- c(vars_combinations_to_remove, i)
      vars_empty <- c(vars_empty, i)
      
      next
    }
    
    if (length(vars_combinations[[i]]) == 1) {
      warning(paste0("Combination of varaibles '", vars_combinations[[i]],
                     "' with index ", i, " is only one variable, which ",
                     "does not have much sense in this context...")
      )
    }
    
    if (!all(vars_combinations[[i]] %in% names(ind_curves))) {
      vars_combinations_to_remove <- c(vars_combinations_to_remove, i)
      vars_invalid_name <- c(vars_invalid_name, i)
      
      next
    }
    
    if (det(stats::var(ind_curves[,vars_combinations[[i]]])) == 0) {
      vars_combinations_to_remove <- c(vars_combinations_to_remove, i)
      vars_almost_singular <- c(vars_almost_singular, i)
    }
  }
  
  if (length(vars_empty)) {
    warning(paste("Index/indices ", paste0(vars_empty, collapse = ", "), "of 'vars_combinations' is/are empty.",
                  "Removing them..."))
  }
  
  if (length(vars_invalid_name)) {
    warning(paste("Invalid variable name in 'vars_combinations' for index/indices ",
                  paste0(vars_invalid_name, collapse = ", "),
                  ". Removing them..."))
  }
  
  if (length(vars_almost_singular)) {
    warning(paste("Combination/s of variables with index/indices", paste0(vars_almost_singular, collapse = ", "),
                  "is/are singular or almost singular. Removing them..."))
  }
  
  if (length(vars_combinations_to_remove)) {
    warning(paste("Combination/s of variable/s with index", paste0(vars_combinations_to_remove, collapse = ", "),
                  "are not valid. Excluding them from any computation..."))
  }
  
  if (length(vars_combinations_to_remove) == length(vars_combinations)) {
    stop("none of the combinations provided in 'vars_combinations' is valid.", call. = FALSE)
  }
  
  vars_combinations_to_remove
}

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

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

# data models
data1 <- sim_model_ex1(n = 50, p = 30, i_sim = 1, seed = 1221)
data2 <- sim_model_ex1(n = 50, p = 30, i_sim = 2, seed = 1222)
data3 <- sim_model_ex1(n = 50, p = 30, i_sim = 3, seed = 1223)
data4 <- sim_model_ex1(n = 50, p = 30, i_sim = 4, seed = 1224)
data5 <- sim_model_ex1(n = 50, p = 30, i_sim = 5, seed = 1225)
data6 <- sim_model_ex1(n = 50, p = 30, i_sim = 6, seed = 1226)
data7 <- sim_model_ex1(n = 50, p = 30, i_sim = 7, seed = 1227)
data8 <- sim_model_ex1(n = 50, p = 30, i_sim = 8, seed = 1228)

data1_labels <- c(rep(1,50), rep(2,50))
data2_labels <- c(rep(2,50), rep(1,50))
data3_labels <- c(rep(3,50), rep(1,50))
data4_labels <- c(rep(4,50), rep(1,50))
data5_labels <- c(rep(5,50), rep(1,50))
data6_labels <- c(rep(6,50), rep(1,50))
data7_labels <- c(rep(7,50), rep(1,50))
data8_labels <- c(rep(8,50), rep(1,50))

# matrix with the indices
data1_ind <- ind(data1, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI", 
                                         "MHI", "MMEI", "MMHI"))
data2_ind <- ind(data2, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI",
                                         "MHI", "MMEI", "MMHI"))
data3_ind <- ind(data3, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI",
                                         "MHI", "MMEI", "MMHI"))
data4_ind <- ind(data4, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI",
                                         "MHI", "MMEI", "MMHI"))
data5_ind <- ind(data5, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI",
                                         "MHI", "MMEI", "MMHI"))
data6_ind <- ind(data6, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI",
                                         "MHI", "MMEI", "MMHI"))
data7_ind <- ind(data7, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI",
                                         "MHI", "MMEI", "MMHI"))
data8_ind <- ind(data8, grid_ll = 0, grid_ul = 1, nbasis = 25,
                 norder = 4, indices = c("EI", "HI", "MEI",
                                         "MHI", "MMEI", "MMHI"))

# clustering with the indices

# combinations 

# original indices
vars1 <- c("dtaEI", "dtaHI")
vars2 <- c("ddtaEI", "ddtaHI") # doesn't make sense! 
vars3 <- c("d2dtaEI", "d2dtaHI") # doesn't make sense!
vars4 <- c(vars1, vars2) # doesn't make sense!
vars5 <- c(vars1, vars3) # same
vars6 <- c(vars2, vars3) # same 
vars7 <- c(vars4, vars3) # same

# general indices 
vars8 <- c("dtaMEI", "dtaMHI")
vars9 <- c("ddtaMEI", "ddtaMHI")
vars10 <- c("d2dtaMEI", "d2dtaMHI")
vars11 <- c(vars8, vars9)
vars12 <- c(vars8, vars10)
vars13 <- c(vars9, vars10)
vars14 <- c(vars11, vars10)

# new modified indices
vars15 <- c("dtaMMEI", "dtaMMHI")
vars16 <- c("ddtaMMEI", "ddtaMMHI")
vars17 <- c("d2dtaMMEI", "d2dtaMMHI")
vars18 <- c(vars14, vars15)
vars19 <- c(vars14, vars16)
vars20 <- c(vars15, vars16)
vars21 <- c(vars18, vars17)

# combination of all types
vars22 <- c(vars1, "dtaMEI")
vars23 <- c(vars2, "ddtaMEI")
vars24 <- c(vars3, "d2dtaMEI")
vars25 <- c(vars20, "dtaMHI")
vars26 <- c(vars21, "ddtaMHI")
vars27 <- c(vars22, "d2dtaMHI")
vars28 <- c(vars23, "dtaMMEI")
vars29 <- c(vars24, "ddtaMMEI")
vars30 <- c(vars25, "d2dtaMMEI")
vars31 <- c(vars26, "dtaMMHI")
vars32 <- c(vars27, "ddtaMMHI")
vars33 <- c(vars28, "d2dtaMMHI")

vars <- list(vars1, vars2, vars3, vars4, vars5, vars6, vars7,vars8, vars9,
             vars10, vars11, vars12, vars13, vars14, vars15, vars16, vars17,
             vars18, vars19, vars20, vars21, vars22, vars23, vars24, vars25,
             vars26, vars27, vars28, vars29, vars30, vars31, vars32, vars33)

set.seed(123)

EHyClus <- function(curves, vars_combinations, nbasis = 30,  n_clusters = 2, norder = 4,
                    clustering_methods = c("hierarch", "kmeans", "kkmeans", "spc"),
                    indices            = c("EI", "HI", "MEI", "MHI", "MMEI", "MMHI"),
                    l_method_hierarch  = c("single", "complete", "average", "centroid", "ward.D2"),
                    l_dist_hierarch    = c("euclidean", "manhattan"),
                    l_dist_kmeans      = c("euclidean", "mahalanobis"),
                    l_kernel           = c("rbfdot", "polydot"),
                    grid_ll = 0, grid_ul = 1,
                    true_labels = NULL, verbose = FALSE, num_cores = 1, ...) {
  # vars_combinations TIENE QUE SER LIST !!!!!
  if (!is.list(vars_combinations)) {
    stop("input 'vars_combinations' must be a list", call. = FALSE)
  }
  
  if (!length(vars_combinations)) {
    stop("input 'vars_combinations' is empty", call. = FALSE)
  }
  
  if (!is.null(true_labels) && length(true_labels) != dim(curves)[1]) {
    stop("'true labels' should have the same length as the number of curves", call. = FALSE)
  }
  
  # list that maps each clustering method to its corresponding function
  default_clustering_methods <- list(
    "hierarch" = clustInd_hierarch,
    "kmeans"   = clustInd_kmeans,
    "kkmeans"  = clustInd_kkmeans,
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
  
  # Generate the dataset with the indexes
  ind_curves <- ind(curves, grid_ll = grid_ll, grid_ul = grid_ul, nbasis, norder, indices)
  
  # Check for correct vars combinations
  vars_combinations_to_remove <- check_vars_combinations(vars_combinations, ind_curves)
  
  if (length(vars_combinations_to_remove)) {
    vars_combinations <- vars_combinations[-vars_combinations_to_remove]
  }
  
  # common arguments for all the clustering methods that are implemented
  # in the package
  common_clustering_arguments <- list(
    "ind_data"          = ind_curves,
    "vars_combinations" = vars_combinations,
    "n_cluster"         = n_clusters,
    "true_labels"       = true_labels,
    "num_cores"         = num_cores
  )
  
  cluster <- list()
  for (method in clustering_methods) {
    method_args <- switch(method,
                          "hierarch" = append(common_clustering_arguments, list(method_list = l_method_hierarch, dist_list = l_dist_hierarch)),
                          "kmeans"   = append(common_clustering_arguments, list(dist_list   = l_dist_kmeans)),
                          "kkmeans"  = append(common_clustering_arguments, list(kernel_list = l_kernel)),
                          "spc"      = append(common_clustering_arguments, list(kernel_list = l_kernel))
    )
    
    cluster[[method]] <- if (verbose) {
      do.call(default_clustering_methods[[method]], method_args)
    } else {
      suppressMessages(quiet(do.call(default_clustering_methods[[method]], method_args)))
    }
  }
  
  if (!is.null(true_labels)) {
    methods <- c()
    metrics <- data.frame(Purity = numeric(0), Fmeasure = numeric(0), RI = numeric(0), Time = numeric(0))
    for (clustering_method in names(cluster)) {
      for (method in names(cluster[[clustering_method]])) {
        methods <- c(methods, method)
        metrics <- rbind(metrics,
                         c(cluster[[clustering_method]][[method]][["valid"]], cluster[[clustering_method]][[method]][["time"]]))
      }
    }
    names(metrics) <- c("Purity", "Fmeasure", "RI", "Time")
    rownames(metrics) <- methods
    
    result  <- list("cluster" = cluster, "metrics" = metrics)
  } else {
    result <- list("cluster" = cluster)
  }
  
  class(result) <- c("EHyClus", class(result))
  attr(result, "n_clusters") <- n_clusters
  
  result
}

data1_ehyclus <- EHyClus(data1, vars_combinations = vars, nbasis = 25,
                         n_clusters = 2, norder = 4,
                         true_labels = data1_labels)
