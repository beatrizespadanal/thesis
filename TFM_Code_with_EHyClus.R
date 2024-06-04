library("ehymet")
library("dplyr")

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

# Epigraph index 
EI <- function(curves, ...) {
  UseMethod("EI")
}

EI.matrix <- function(curves, ...) {
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  
  index <- apply(curves,1, function(y)
    sum(apply(curves,1,function(x)
      sum(x>=y)==l_curves)))/n_curves
  
  return(1 - index)
}

EI.array <- function(curves, ...) {
  if (length(dim(curves)) != 3) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  d_curves <- dim(curves)[3]
  
  index <- colSums(Reduce('*',lapply(1:d_curves, function(k)
    sapply(1:n_curves, function(j)
      colSums(sapply(1:n_curves, function(i)
        curves[i,,k] >= curves[j,,k]))==l_curves))))
  
  return(1 - index/n_curves)
}

EI.default <- function(curves, ...) {
  stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
}

# Hypograph index
HI <- function(curves, ...) {
  UseMethod("HI")
}

HI.matrix <- function(curves, ...) {
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  
  index <- apply(curves,1, function(y)
    sum(apply(curves,1,function(x)
      sum(x<=y)==l_curves)))/n_curves
  
  return(index)
}

HI.array <- function(curves, ...) {
  if (length(dim(curves)) != 3) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  d_curves <- dim(curves)[3]
  
  index <- colSums(Reduce('*',lapply(1:d_curves, function(k)
    sapply(1:n_curves, function(j)
      colSums(sapply(1:n_curves, function(i)
        curves[i,,k] <= curves[j,,k]))==l_curves))))
  
  return(index/n_curves)
}

HI.default <- function(curves, ...) {
  stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
}

# Modified Epigraph 
MEI <- function(curves, ...) {
  UseMethod("MEI")
}

MEI.matrix <- function(curves, ...) {
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  rankm <- apply(curves, 2, function(y) (rank(y, ties.method = "min")))
  n_a <- n_curves-rankm+1
  index <- rowSums(n_a)/(n_curves*l_curves)
  return(1 - index)
}

MEI.array <- function(curves, ...) {
  if (length(dim(curves)) != 3) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  d_curves <- dim(curves)[3]
  
  index <- sapply(1:n_curves, function(j)
    sum(Reduce('*', lapply(1:d_curves, function(k)
      sapply(1:n_curves, function(i)
        curves[i,,k] >= curves[j,,k])))))
  
  return (1 - index / (n_curves * l_curves))
}

MEI.default <- function(curves, ...) {
  stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
}

# Modified Hypograph 
MHI <- function(curves, ...) {
  UseMethod("MHI")
}

MHI.matrix <- function(curves, ...) {
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  rankm <- apply(curves, 2, function(y) (rank(y, ties.method = "max")))
  index <- rowSums(rankm) / (n_curves * l_curves)
  return(index)
}

MHI.array <- function(curves, ...) {
  if (length(dim(curves)) != 3) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  d_curves <- dim(curves)[3]
  
  index <- sapply(1:n_curves, function(j)
    sum(Reduce('*', lapply(1:d_curves, function(k)
      sapply(1:n_curves, function(i)
        curves[i,,k] <= curves[j,,k])))))
  
  return (index / (n_curves * l_curves))
}

MHI.default <- function(curves, ...) {
  stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
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
data1_labels <- c(rep(1,50), rep(2,50))
data2_labels <- c(rep(2,50), rep(1,50))
data3_labels <- c(rep(3,50), rep(1,50))
data4_labels <- c(rep(4,50), rep(1,50))
data5_labels <- c(rep(5,50), rep(1,50))
data6_labels <- c(rep(6,50), rep(1,50))
data7_labels <- c(rep(7,50), rep(1,50))
data8_labels <- c(rep(8,50), rep(1,50))

# clustering with the indices

set.seed(123)
clustInd_hierarch_aux <- function(ind_data, vars, method = "single",
                                  dist = "euclidean", n_cluster = 2,
                                  true_labels=NULL){
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.")
  }
  
  # Check if variables exist in the data frame
  if (!all(vars %in% names(ind_data))) {
    stop("Invalid variable name.")
  }
  
  if(!all(method %in% c("single","complete","average", "centroid","ward.D2"))) {
    stop("Invalid method name.")
  }
  
  if(!all(dist %in% c("euclidean", "manhattan"))) {
    stop("Invalid distance name.")
  }
  
  t0 <- Sys.time()
  
  # Perform hierarchical clustering
  d <- stats::dist(ind_data[,vars], method = dist)
  met <- stats::hclust(d, method = method)
  clus <- stats::cutree(met, k = n_cluster)
  
  t1 <- Sys.time()
  
  # Calculate execution time
  et <- data.frame(difftime(t1,t0,'secs'))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(et))
  } else {
    valid <- valid(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(et))
  }
  
  return(res)
}

clustInd_hierarch <- function(ind_data, vars_combinations,
                              method_list = c("single","complete","average",
                                              "centroid","ward.D2"),
                              dist_list = c("euclidean", "manhattan"),
                              n_cluster=2, true_labels = NULL,
                              num_cores=1) {
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if(!is.list(vars_combinations)) {
    stop("input 'vars_combinations' must be a list.", call. = FALSE)
  }
  
  # Check for correct vars combinations
  vars_combinations_to_remove <- check_vars_combinations(vars_combinations, ind_data)
  
  if (length(vars_combinations_to_remove)) {
    vars_combinations <- vars_combinations[-vars_combinations_to_remove]
  }
  
  # Check if indices, methods and distances lists are provided
  if (!is.character(method_list) ||
      !is.character(dist_list) || length(vars_combinations) == 0 ||
      length(method_list) == 0 || length(dist_list) == 0) {
    stop("invalid 'method_list' or 'dist_list'. Both must be non-empty character vectors.", call. = FALSE)
  }
  
  if (is.null(names(vars_combinations))) {
    names(vars_combinations) <- paste0("vars", seq_along(vars_combinations))
  }
  
  # Generate all the possible combinations of indices, methods and distances
  parameter_combinations <- expand.grid(vars = names(vars_combinations),
                                        method = method_list, distance = dist_list)
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    met  <- parameter_combinations$method[i]
    dist <- parameter_combinations$distance[i]
    
    clustInd_hierarch_aux(ind_data, vars, met, dist, n_cluster, true_labels)
  }, mc.cores = num_cores)
  
  result_name <- get_result_names("hierarch", parameter_combinations, vars_combinations)
  names(result) <- result_name
  
  return(result)
}

kmeans_mahal <- function(ind_data, n_cluster){
  
  # Check if input is numeric matrix or array
  if (!(is.numeric(ind_data) || is.matrix(ind_data) || is.array(ind_data) ||
        is.data.frame(ind_data))) {
    stop("Input must be a numeric matrix, array or data frame.")
  }
  
  # Convert data to matrix
  if (! is.matrix(ind_data)) {
    data_matrix <- as.matrix(ind_data)
  } else {
    data_matrix <- ind_data
  }
  
  # Cholesky transformation
  c <- chol(stats::var(data_matrix))
  data_transform <- data_matrix %*% solve(c)
  
  #vector containing the clustering partition
  km <- stats::kmeans(data_transform, centers=n_cluster, iter.max=1000,
                      nstart=100)$cluster
  return(km)
}

clustInd_kmeans_aux <- function(ind_data, vars, dist = "euclidean",
                                n_cluster = 2, true_labels = NULL){
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.")
  }
  
  # Check if variables exist in the data frame
  if (!all(vars %in% names(ind_data))) {
    stop("Invalid variable names.")
  }
  
  # Check if the distance given can be used
  if (!dist %in% c("euclidean", "mahalanobis")) {
    stop("Invalid distance.")
  }
  
  t0 <- Sys.time()
  
  if(dist=="euclidean"){
    clus <- stats::kmeans(ind_data[,vars], centers = n_cluster,
                          iter.max = 1000, nstart = 100)$cluster
  } else{
    clus <- kmeans_mahal(ind_data[,vars], n_cluster)
  }
  t1 <- Sys.time()
  t <- data.frame(difftime(t1,t0,'secs'))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(t))
  } else {
    valid <- valid(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(t))
  }
  return(res)
}

clustInd_kmeans <- function(ind_data, vars_combinations,
                            dist_list = c("euclidean", "mahalanobis"),
                            n_cluster = 2, true_labels = NULL,
                            num_cores = 1) {
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if(!is.list(vars_combinations)) {
    stop("Input 'vars_combinations' must be a list.", call. = FALSE)
  }
  
  # Check for correct vars combinations
  vars_combinations_to_remove <- check_vars_combinations(vars_combinations, ind_data)
  
  if (length(vars_combinations_to_remove)) {
    vars_combinations <- vars_combinations[-vars_combinations_to_remove]
  }
  
  # Check if indices, methods and distances lists are provided
  if (length(vars_combinations) == 0 || length(dist_list) == 0) {
    stop("Invalid 'vars_combinations' or 'dist_list'. Both must be non-empty
         character vectors.", call. = FALSE)
  }
  
  if (is.null(names(vars_combinations))) {
    names(vars_combinations) <- paste0("vars", seq_along(vars_combinations))
  }
  
  # Generate all the possible combinations of indices, methods and distances
  parameter_combinations <- expand.grid(vars = names(vars_combinations),
                                        distance = dist_list)
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    dist <- parameter_combinations$distance[i]
    
    clustInd_kmeans_aux(ind_data = ind_data, vars =vars, dist = dist,
                        n_cluster = n_cluster, true_labels = true_labels)
  }, mc.cores = num_cores)
  
  result_name <- get_result_names("kmeans", parameter_combinations, vars_combinations)
  names(result) <- result_name
  
  return(result)
}

clustInd_kkmeans_aux <- function(ind_data, vars, kernel = "rbfdot",
                                 n_cluster = 2, true_labels = NULL, ...){
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.")
  }
  
  # Check if variables exist in the data frame
  if (!all(vars %in% names(ind_data))) {
    stop("Invalid variable names.")
  }
  
  # Check if the kernel given can be used
  if (!kernel %in% c("rbfdot", "polydot")) {
    stop("Invalid kernel.")
  }
  
  
  t0 <- Sys.time()
  kkmeans_out <- kernlab::kkmeans(as.matrix(ind_data[,vars]),
                                  centers= n_cluster, kernel = kernel, ...)
  clus <- kkmeans_out@.Data
  t1 <- Sys.time()
  t <- data.frame(difftime(t1,t0,'secs'))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(t))
  } else {
    valid <- valid(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(t))
  }
  return(res)
}

clustInd_kkmeans <- function(ind_data, vars_combinations,
                             kernel_list = c("rbfdot", "polydot"),
                             n_cluster = 2, true_labels = NULL,
                             num_cores = 1, ...) {
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if(!is.list(vars_combinations)) {
    stop("Input 'vars_combinations' must be a list.", call. = FALSE)
  }
  
  # Check for correct vars combinations
  vars_combinations_to_remove <- check_vars_combinations(vars_combinations, ind_data)
  
  if (length(vars_combinations_to_remove)) {
    vars_combinations <- vars_combinations[-vars_combinations_to_remove]
  }
  
  # Check if indices, and kernel lists are provided
  if (!is.character(kernel_list) || length(vars_combinations) == 0 || length(kernel_list) == 0) {
    stop("Invalid 'kernel_list' or 'vars_combinations'. Both must be non-empty
         character vectors.", call. = FALSE)
  }
  
  if (is.null(names(vars_combinations))) {
    names(vars_combinations) <- paste0("vars", seq_along(vars_combinations))
  }
  
  # Generate all the possible combinations of indices, methods and distances
  parameter_combinations <- expand.grid(vars = names(vars_combinations),
                                        kernel = kernel_list)
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    kern <- as.character(parameter_combinations$kernel[i])
    
    clustInd_kkmeans_aux(ind_data, vars, kern, n_cluster, true_labels)
  }, mc.cores = num_cores)
  
  result_name <- get_result_names("kkmeans", parameter_combinations, vars_combinations)
  names(result) <- result_name
  
  return(result)
}

clustInd_spc_aux <- function(ind_data, vars, kernel = "rbfdot", n_cluster = 2,
                             true_labels=NULL, ...){
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  # Check if variables exist in the data frame
  if (!all(vars %in% names(ind_data))) {
    stop("Invalid variable names.", call. = FALSE)
  }
  
  t0 <- Sys.time()
  
  # create target variable
  nrow_ind_data <- nrow(ind_data)
  specc_output <- kernlab::specc(as.matrix(ind_data[,vars]),
                                 centers = n_cluster,  kernel = kernel, ...)
  clus <- specc_output@.Data
  
  t1 <- Sys.time()
  t <- data.frame(difftime(t1,t0,'secs'))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(t))
  } else {
    valid <- valid(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(t))
  }
  return(res)
}

clustInd_spc <- function(ind_data, vars_combinations,
                         kernel_list = c("rbfdot", "polydot"),
                         n_cluster = 2, true_labels = NULL,
                         num_cores = 1, ...) {
  
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if(!is.list(vars_combinations)) {
    stop("Input 'vars_combinations' must be a data frame.", call. = FALSE)
  }
  
  # Check for correct vars combinations
  vars_combinations_to_remove <- check_vars_combinations(vars_combinations, ind_data)
  
  if (length(vars_combinations_to_remove)) {
    vars_combinations <- vars_combinations[-vars_combinations_to_remove]
  }
  
  # Check if indices, methods and distances lists are provided
  if (!is.character(kernel_list) || length(vars_combinations) == 0 ||
      length(kernel_list) == 0) {
    stop("Invalid 'kernel_list' or 'vars_combinations'. Both must be non-empty
         character vectors.", call. = FALSE)
  }
  
  if (is.null(names(vars_combinations))) {
    names(vars_combinations) <- paste0("vars", seq_along(vars_combinations))
  }
  
  # Generate all the possible combinations of indices, methods and distances
  parameter_combinations <- expand.grid(vars = names(vars_combinations),
                                        kernel = kernel_list)
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    kern <- as.character(parameter_combinations$kernel[i])
    
    clustInd_spc_aux(ind_data, vars, kern, n_cluster, true_labels)
  }, mc.cores = num_cores)
  
  result_name <- get_result_names("spc", parameter_combinations, vars_combinations)
  names(result) <- result_name
  
  return(result)
}

get_result_names <- function(method_name, parameter_combinations, vars_combinations) {
  args <- list(method_name)
  for (combination in parameter_combinations[-1]) {
    args <- append(args, list(combination))
  }
  
  args <- append(args, list(rep(sapply(vars_combinations, function(x) paste0(x, collapse = "")),
                                times = nrow(parameter_combinations) / length(vars_combinations))
  ))
  args[["sep"]] <- "_"
  do.call(paste, args)
}

EHyClus_mm <- function(curves, vars_combinations, nbasis = 30,  n_clusters = 2, norder = 4,
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
  ind_curves$dtaMMEI <- scale(ind_curves$dtaMMEI)
  ind_curves$dtaMMHI <- scale(ind_curves$dtaMMHI)
  ind_curves$ddtaMMEI <- scale(ind_curves$ddtaMMEI)
  ind_curves$ddtaMMHI <- scale(ind_curves$ddtaMMHI)
  ind_curves$d2dtaMMEI <- scale(ind_curves$d2dtaMMEI)
  ind_curves$d2dtaMMHI <- scale(ind_curves$d2dtaMMHI)
  
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

# combinations 

v1 <- c("dtaMEI", "dtaMHI") 
v2 <- c("ddtaMEI", "ddtaMHI") 
v3 <- c("d2dtaMEI", "d2dtaMHI") 
v4 <- c(v1, v2) 
v5 <- c(v1, v3)
v6 <- c(v2, v3)
v7 <- c(v4, v3) 

# separately
v8 <- c("dtaMEI", "ddtaMEI") 
v9 <- c("dtaMEI", "d2dtaMEI")  
v10 <- c("ddtaMEI", "d2dtaMEI")  
v11 <- c(v8, "d2dtaMEI") 

v12 <- c("dtaMHI", "ddtaMHI") 
v13 <- c("dtaMHI", "d2dtaMHI") 
v14 <- c("ddtaMHI", "d2dtaMHI") 
v15 <- c(v12, "d2dtaMHI")

# new generalized indexes

v16 <- c("dtaMMEI", "dtaMMHI")
v17 <- c("ddtaMMEI", "ddtaMMHI") 
v18 <- c("d2dtaMMEI", "d2dtaMMHI") 
v19 <- c(v16, v17) 
v20 <- c(v16, v18)
v21 <- c(v17,v18)
v22 <- c(v19, v18)

# separately
v23 <- c("dtaMMEI", "ddtaMMEI") 
v24 <- c("dtaMMEI", "d2dtaMMEI") 
v25 <- c("ddtaMMEI", "d2dtaMMEI") 
v26 <- c(v23, "d2dtaMMEI") 

v27 <- c("dtaMMHI", "ddtaMMHI") 
v28 <- c("dtaMMHI", "d2dtaMMHI") 
v29 <- c("ddtaMMHI", "d2dtaMMHI") 
v30 <- c(v27, "d2dtaMMHI") 

v_list1 <- list(v8, v9, v11, v12, v13, v15, v19)

## simulation function 
n_sim = 50
clasif1 <- c()

# Model 1 
set.seed(1234)

for(i in 1:n_sim){
  set.seed(i)
  
  data1 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 1, seed = i)
  
  clasif1_i <- EHyClus_mm(data1, vars_combinations = v_list1, nbasis = 25,
                              n_clusters = 2, norder = 4,
                              true_labels = data1_labels)
  clasif1_i <- cbind(names = row.names(clasif1_i$metrics),
                    data.frame(clasif1_i$metrics, row.names=NULL))
  clasif1 <- bind_rows(clasif1, clasif1_i)
  
}

# Final simulation results
clasif_f1 <- clasif1 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

# Model 2 

v_list2 <- list(v8, v9, v11, v12, v13, v15, v19)
clasif2 <- c()

for(i in 1:n_sim){
  set.seed(i)
  
  data2 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 2, seed = i)
  
  
  clasif2_i <- EHyClus_mm(data2, vars_combinations = v_list2, nbasis = 25,
                         n_clusters = 2, norder = 4,
                         true_labels = data2_labels)
  clasif2_i <- cbind(names = row.names(clasif2_i$metrics),
                    data.frame(clasif2_i$metrics, row.names=NULL))
  clasif2 <- bind_rows(clasif2, clasif2_i)
  
}

# Final simulation results
clasif_f2 <- clasif2 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

# Model 3 

clasif3 <- c()

v_list3 <- list()

for(i in 1:n_sim){
  set.seed(i)
  
  data3 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 3, seed = i)
  
  clasif3_i <- EHyClus_mm(data3, vars_combinations = v_list1, nbasis = 25,
                          n_clusters = 2, norder = 4,
                          true_labels = data3_labels)
  clasif3_i <- cbind(names = row.names(clasif3_i$metrics),
                     data.frame(clasif3_i$metrics, row.names=NULL))
  clasif3 <- bind_rows(clasif3, clasif3_i)
  
}

# Final simulation results
clasif_f3 <- clasif3 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

# model 4 
clasif4 <- c()

#v_list4 <- list(v2,v3,v6,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,
#            v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)
v_list4 <- list(v8,v9,v10,v11,v12,v13,v14)

for(i in 1:n_sim){
  set.seed(i)
  
  data4 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 4, seed = i)
  
  clasif4_i <- EHyClus_mm(data4, vars_combinations = v_list4, nbasis = 25,
                          n_clusters = 2, norder = 4,
                          true_labels = data4_labels)
  clasif4_i <- cbind(names = row.names(clasif4_i$metrics),
                     data.frame(clasif4_i$metrics, row.names=NULL))
  clasif4 <- bind_rows(clasif4, clasif4_i)
  
}

# Final simulation results
clasif_f4 <- clasif4 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)


# model 5

clasif5 <- c()

v_list5 <- list(v8,v9,v10,v13)


for(i in 1:n_sim){
  set.seed(i)
  
  data5 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 5, seed = i)
  
  clasif5_i <- EHyClus_mm(data3, vars_combinations = v_list5, nbasis = 25,
                          n_clusters = 2, norder = 4,
                          true_labels = data5_labels)
  clasif5_i <- cbind(names = row.names(clasif3_i$metrics),
                     data.frame(clasif3_i$metrics, row.names=NULL))
  clasif5 <- bind_rows(clasif3, clasif3_i)
  
}

# Final simulation results
clasif_f5 <- clasif5 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)


# model 6
clasif6 <- c()

v_list6 <- list(v8,v9,v10,v11,v12,v14,v15,v16)


for(i in 1:n_sim){
  set.seed(i)
  
  data6 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 6, seed = i)
  
  clasif6_i <- EHyClus_mm(data6, vars_combinations = v_list6, nbasis = 25,
                          n_clusters = 2, norder = 4,
                          true_labels = data6_labels)
  clasif6_i <- cbind(names = row.names(clasif6_i$metrics),
                     data.frame(clasif6_i$metrics, row.names=NULL))
  clasif6 <- bind_rows(clasif6, clasif6_i)
  
}

# Final simulation results
clasif_f6 <- clasif6 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)


# model 7 
clasif7 <- c()

v_list7 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v19,v20,v21,v22,v23)

for(i in 1:n_sim){
  set.seed(i)
  
  data7 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 7, seed = i)
  
  clasif7_i <- EHyClus_mm(data7, vars_combinations = v_list7, nbasis = 25,
                          n_clusters = 2, norder = 4,
                          true_labels = data7_labels)
  clasif7_i <- cbind(names = row.names(clasif7_i$metrics),
                     data.frame(clasif7_i$metrics, row.names=NULL))
  clasif7 <- bind_rows(clasif7, clasif7_i)
  
}

# Final simulation results
clasif_f7 <- clasif7 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

# model 8
clasif8 <- c()

v_list8 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v19,v20,v21,v22,v23,v24,
                v25)

for(i in 1:n_sim){
  set.seed(i)
  
  data8 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 8, seed = i)
  
  clasif8_i <- EHyClus_mm(data7, vars_combinations = v_list8, nbasis = 25,
                          n_clusters = 2, norder = 4,
                          true_labels = data7_labels)
  clasif8_i <- cbind(names = row.names(clasif8_i$metrics),
                     data.frame(clasif8_i$metrics, row.names=NULL))
  clasif8 <- bind_rows(clasif8, clasif8_i)
  
}

# Final simulation results
clasif_f8 <- clasif8 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

## Box-plots for comparison 

# model 1
library(ggplot2)
library(reshape2)
library(tidyr)

clasif_f1_b <- tibble(names = c(rep("hierarchical_clust",70), rep("kkmeans_clust",14),
                              rep("kmeans_clust",14), rep("spc_clust",14)),
                    Metric = c(rep("RI",70), rep("RI", 14), rep("RI",14),
                               rep("RI", 14)),
                    Value = clasif_f1$RI)

ggplot(clasif_f1_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 1",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 2
clasif_f2_b <- tibble(names = c(rep("hierarchical_clust", 70), rep("kkmeans_clust",14),
                                rep("kmeans_clust", 14), rep("spc_clust", 14)),
                      Metric = c(rep("RI", 70), rep("RI", 14), rep("RI", 14),
                                 rep("RI", 14)),
                      Value = clasif_f2$RI)

ggplot(clasif_f2_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 2",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 3
clasif_f3_b <- tibble(names = c(rep("hierarchical_clust", 70), rep("kkmeans_clust",14),
                                rep("kmeans_clust", 14), rep("spc_clust", 14)),
                      Metric = c(rep("RI", 70), rep("RI", 14), rep("RI", 14),
                                 rep("RI", 14)),
                      Value = clasif_f3$RI)

ggplot(clasif_f3_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 3",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 4
clasif_f4_b <- tibble(names = c(rep("hierarchical_clust", 70), rep("kkmeans_clust",14),
                                rep("kmeans_clust", 14), rep("spc_clust", 14)),
                      Metric = c(rep("RI", 70), rep("RI", 14), rep("RI", 14),
                                 rep("RI", 14)),
                      Value = clasif_f4$RI)

ggplot(clasif_f4_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 4",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 5
clasif_f5_b <- tibble(names = c(rep("hierarchical_clust", 70), rep("kkmeans_clust",14),
                                rep("kmeans_clust", 14), rep("spc_clust", 14)),
                      Metric = c(rep("RI", 70), rep("RI", 14), rep("RI", 14),
                                 rep("RI", 14)),
                      Value = clasif_f5$RI)

ggplot(clasif_f5_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 5",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 6
clasif_f6_b <- tibble(names = c(rep("hierarchical_clust", 80), rep("kkmeans_clust",16),
                                rep("kmeans_clust", 16), rep("spc_clust", 16)),
                      Metric = c(rep("RI", 80), rep("RI", 16), rep("RI", 16),
                                 rep("RI", 16)),
                      Value = clasif_f6$RI)

ggplot(clasif_f6_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 6",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 7
clasif_f7_b <- tibble(names = c(rep("hierarchical_clust", 150), rep("kkmeans_clust",30),
                                rep("kmeans_clust", 30), rep("spc_clust", 30)),
                      Metric = c(rep("RI", 150), rep("RI", 30), rep("RI", 30),
                                 rep("RI", 30)),
                      Value = clasif_f7$RI)

ggplot(clasif_f7_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 7",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 8
clasif_f8_b <- tibble(names = c(rep("hierarchical_clust", 170), rep("kkmeans_clust",34),
                                rep("kmeans_clust", 34), rep("spc_clust", 34)),
                      Metric = c(rep("RI", 170), rep("RI", 34), rep("RI", 34),
                                 rep("RI", 34)),
                      Value = clasif_f8$RI)

ggplot(clasif_f8_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 8",
       x = "Clustering Method", y = "RI") +
  theme_minimal()



