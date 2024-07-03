#################################################
# TFM Code - Beatriz Espadanal Gonçalves ########
#################################################
# Clustering with EHyClust ######################
#################################################

check_vars_combinations <- function(vars_combinations, ind_curves) {
  vars_combinations_to_remove <- c()
  
  vars_empty <- c()
  vars_invalid_name <- c()
  vars_almost_singular <- c()
  
  
  for (i in seq_along(vars_combinations)) {
    if (length(vars_combinations[[i]]) == 0) {
      vars_combinations_to_remove <- c(vars_combinations_to_remove, i)
      vars_empty <- c(vars_empty, i)
      
      next
    }
    
    if (length(vars_combinations[[i]]) == 1) {
      warning(paste0(
        "Combination of varaibles '", vars_combinations[[i]],
        "' with index ", i, " is only one variable, which ",
        "does not have much sense in this context..."
      ))
    }
    
    if (!all(vars_combinations[[i]] %in% names(ind_curves))) {
      vars_combinations_to_remove <- c(vars_combinations_to_remove, i)
      vars_invalid_name <- c(vars_invalid_name, i)
      
      next
    }
    
    if (det(stats::var(ind_curves[, vars_combinations[[i]]])) == 0) {
      vars_combinations_to_remove <- c(vars_combinations_to_remove, i)
      vars_almost_singular <- c(vars_almost_singular, i)
    }
  }
  
  if (length(vars_empty)) {
    warning(paste(
      "Index/indices ", paste0(vars_empty, collapse = ", "), "of 'vars_combinations' is/are empty.",
      "Removing them..."
    ))
  }
  
  if (length(vars_invalid_name)) {
    warning(paste(
      "Invalid variable name in 'vars_combinations' for index/indices ",
      paste0(vars_invalid_name, collapse = ", "),
      ". Removing them..."
    ))
  }
  
  if (length(vars_almost_singular)) {
    warning(paste(
      "Combination/s of variables with index/indices", paste0(vars_almost_singular, collapse = ", "),
      "is/are singular or almost singular. Removing them..."
    ))
  }
  
  if (length(vars_combinations_to_remove)) {
    warning(paste(
      "Combination/s of variable/s with index", paste0(vars_combinations_to_remove, collapse = ", "),
      "are not valid. Excluding them from any computation..."
    ))
  }
  
  if (length(vars_combinations_to_remove) == length(vars_combinations)) {
    stop("none of the combinations provided in 'vars_combinations' is valid.", call. = FALSE)
  }
  
  vars_combinations_to_remove
}


set.seed(123)
clustInd_hierarch_aux <- function(ind_data, vars, method = "single",
                                  dist = "euclidean", n_cluster = 2,
                                  true_labels = NULL) {
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.")
  }
  
  # Check if variables exist in the data frame
  if (!all(vars %in% names(ind_data))) {
    stop("Invalid variable name.")
  }
  
  if (!all(method %in% c("single", "complete", "average", "centroid", "ward.D2"))) {
    stop("Invalid method name.")
  }
  
  if (!all(dist %in% c("euclidean", "manhattan"))) {
    stop("Invalid distance name.")
  }
  
  t0 <- Sys.time()
  
  # Perform hierarchical clustering
  d <- stats::dist(ind_data[, vars], method = dist)
  met <- stats::hclust(d, method = method)
  clus <- stats::cutree(met, k = n_cluster)
  
  t1 <- Sys.time()
  
  # Calculate execution time
  et <- data.frame(difftime(t1, t0, "secs"))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(et))
  } else {
    valid <- clustering_validation(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(et))
  }
  
  return(res)
}

clustInd_hierarch <- function(ind_data, vars_combinations,
                              method_list = c(
                                "single", "complete", "average",
                                "centroid", "ward.D2"
                              ),
                              dist_vector = c("euclidean", "manhattan"),
                              n_cluster = 2, true_labels = NULL,
                              n_cores = 1) {
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if (!is.list(vars_combinations)) {
    stop("input 'vars_combinations' must be a list.", call. = FALSE)
  }
  
  n_cores <- check_n_cores(n_cores)
  
  # Check for correct vars combinations
  vars_combinations_to_remove <- check_vars_combinations(vars_combinations, ind_data)
  
  if (length(vars_combinations_to_remove)) {
    vars_combinations <- vars_combinations[-vars_combinations_to_remove]
  }
  
  # Check if indices, methods and distances lists are provided
  if (!is.character(method_list) ||
      !is.character(dist_vector) || length(vars_combinations) == 0 ||
      length(method_list) == 0 || length(dist_vector) == 0) {
    stop("invalid 'method_list' or 'dist_vector'. Both must be non-empty character vectors.", call. = FALSE)
  }
  
  if (is.null(names(vars_combinations))) {
    names(vars_combinations) <- paste0("vars", seq_along(vars_combinations))
  }
  
  # Generate all the possible combinations of indices, methods and distances
  parameter_combinations <- expand.grid(
    vars = names(vars_combinations),
    method = method_list, distance = dist_vector
  )
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    met <- parameter_combinations$method[i]
    dist <- parameter_combinations$distance[i]
    
    clustInd_hierarch_aux(ind_data, vars, met, dist, n_cluster, true_labels)
  }, mc.cores = n_cores)
  
  result_name <- get_result_names("hierarch", parameter_combinations, vars_combinations)
  names(result) <- result_name
  
  return(result)
}


kmeans_mahal <- function(ind_data, n_cluster) {
  # Check if input is numeric matrix or array
  if (!(is.numeric(ind_data) || is.matrix(ind_data) || is.array(ind_data) ||
        is.data.frame(ind_data))) {
    stop("Input must be a numeric matrix, array or data frame.")
  }
  
  # Convert data to matrix
  if (!is.matrix(ind_data)) {
    data_matrix <- as.matrix(ind_data)
  } else {
    data_matrix <- ind_data
  }
  
  # Cholesky transformation
  c <- chol(stats::var(data_matrix))
  data_transform <- data_matrix %*% solve(c)
  
  # vector containing the clustering partition
  km <- stats::kmeans(data_transform,
                      centers = n_cluster, iter.max = 1000,
                      nstart = 100
  )$cluster
  return(km)
}

clustInd_kmeans_aux <- function(ind_data, vars, dist = "euclidean",
                                n_cluster = 2, true_labels = NULL) {
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
  
  if (dist == "euclidean") {
    clus <- stats::kmeans(ind_data[, vars],
                          centers = n_cluster,
                          iter.max = 1000, nstart = 100
    )$cluster
  } else {
    clus <- kmeans_mahal(ind_data[, vars], n_cluster)
  }
  t1 <- Sys.time()
  t <- data.frame(difftime(t1, t0, "secs"))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(t))
  } else {
    valid <- clustering_validation(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(t))
  }
  return(res)
}

clustInd_kmeans <- function(ind_data, vars_combinations,
                            dist_vector = c("euclidean", "mahalanobis"),
                            n_cluster = 2, true_labels = NULL,
                            n_cores = 1) {
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if (!is.list(vars_combinations)) {
    stop("Input 'vars_combinations' must be a list.", call. = FALSE)
  }
  
  n_cores <- check_n_cores(n_cores)
  
  DIST_VECTOR <- c("euclidean", "mahalanobis")
  check_list_parameter(dist_vector, DIST_VECTOR, "dist")
  
  # Check for correct vars combinations
  vars_combinations_to_remove <- check_vars_combinations(vars_combinations, ind_data)
  
  if (length(vars_combinations_to_remove)) {
    vars_combinations <- vars_combinations[-vars_combinations_to_remove]
  }
  
  # Check if indices, methods and distances lists are provided
  if (length(vars_combinations) == 0 || length(dist_vector) == 0) {
    stop("Invalid 'vars_combinations' or 'dist_vector'. Both must be non-empty
         character vectors.", call. = FALSE)
  }
  
  if (is.null(names(vars_combinations))) {
    names(vars_combinations) <- paste0("vars", seq_along(vars_combinations))
  }
  
  # Generate all the possible combinations of indices, methods and distances
  parameter_combinations <- expand.grid(
    vars = names(vars_combinations),
    distance = dist_vector
  )
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    dist <- parameter_combinations$distance[i]
    
    clustInd_kmeans_aux(
      ind_data = ind_data, vars = vars, dist = dist,
      n_cluster = n_cluster, true_labels = true_labels
    )
  }, mc.cores = n_cores)
  
  result_name <- get_result_names("kmeans", parameter_combinations, vars_combinations)
  names(result) <- result_name
  
  return(result)
}

clustInd_kkmeans_aux <- function(ind_data, vars, kernel = "rbfdot",
                                 n_cluster = 2, true_labels = NULL, ...) {
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
  kkmeans_out <- kernlab::kkmeans(as.matrix(ind_data[, vars]),
                                  centers = n_cluster, kernel = kernel, ...
  )
  clus <- kkmeans_out@.Data
  t1 <- Sys.time()
  t <- data.frame(difftime(t1, t0, "secs"))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(t))
  } else {
    valid <- clustering_validation(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(t))
  }
  return(res)
}

clustInd_kkmeans <- function(ind_data, vars_combinations,
                             kernel_list = c("rbfdot", "polydot"),
                             n_cluster = 2, true_labels = NULL,
                             n_cores = 1) {
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if (!is.list(vars_combinations)) {
    stop("Input 'vars_combinations' must be a list.", call. = FALSE)
  }
  
  n_cores <- check_n_cores(n_cores)
  
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
  parameter_combinations <- expand.grid(
    vars = names(vars_combinations),
    kernel = kernel_list
  )
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    kern <- as.character(parameter_combinations$kernel[i])
    
    clustInd_kkmeans_aux(ind_data, vars, kern, n_cluster, true_labels)
  }, mc.cores = n_cores)
  
  result_name <- get_result_names("kkmeans", parameter_combinations, vars_combinations)
  names(result) <- result_name
  
  return(result)
}

clustInd_spc_aux <- function(ind_data, vars, kernel = "rbfdot", n_cluster = 2,
                             true_labels = NULL, ...) {
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
  specc_output <- kernlab::specc(as.matrix(ind_data[, vars]),
                                 centers = n_cluster, kernel = kernel, ...
  )
  clus <- specc_output@.Data
  
  t1 <- Sys.time()
  t <- data.frame(difftime(t1, t0, "secs"))
  
  if (is.null(true_labels)) {
    res <- list("cluster" = clus, "time" = as.numeric(t))
  } else {
    valid <- clustering_validation(true_labels, clus)
    res <- list("cluster" = clus, "valid" = valid, "time" = as.numeric(t))
  }
  return(res)
}

clustInd_spc <- function(ind_data, vars_combinations,
                         kernel_list = c("rbfdot", "polydot"),
                         n_cluster = 2, true_labels = NULL,
                         n_cores = 1) {
  # Check if input is a data frame
  if (!is.data.frame(ind_data)) {
    stop("Input 'ind_data' must be a data frame.", call. = FALSE)
  }
  
  if (!is.list(vars_combinations)) {
    stop("Input 'vars_combinations' must be a data frame.", call. = FALSE)
  }
  
  n_cores <- check_n_cores(n_cores)
  
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
  parameter_combinations <- expand.grid(
    vars = names(vars_combinations),
    kernel = kernel_list
  )
  
  tl_null <- is.null(true_labels)
  
  n_comb <- nrow(parameter_combinations)
  
  result <- parallel::mclapply(1:n_comb, function(i) {
    vars <- vars_combinations[[parameter_combinations$vars[i]]]
    kern <- as.character(parameter_combinations$kernel[i])
    
    clustInd_spc_aux(ind_data, vars, kern, n_cluster, true_labels)
  }, mc.cores = n_cores)
  
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

EHyClus_mm <- function(curves, vars_combinations, k = 30, n_clusters = 2, bs = "cr",
                    clustering_methods = c("hierarch", "kmeans", "kkmeans", "spc"),
                    l_method_hierarch = c("single", "complete", "average", "centroid", "ward.D2"),
                    l_dist_hierarch = c("euclidean", "manhattan"),
                    l_dist_kmeans = c("euclidean", "mahalanobis"),
                    l_kernel = c("rbfdot", "polydot"),
                    grid,
                    true_labels = NULL, only_best = FALSE, verbose = FALSE, n_cores = 1) {
  if (length(dim(curves)) > 3 || length(dim(curves)) < 2) {
    stop("'curves' should be 2-dimensional or 3-dimensional", call. = FALSE)
  }
  
  if (!missing(vars_combinations) && !is.list(vars_combinations) && !is.numeric(vars_combinations)) {
    stop("input 'vars_combinations' must be a list", call. = FALSE)
  }
  
  if (!missing(vars_combinations) && is.list(vars_combinations) && !length(vars_combinations)) {
    stop("input 'vars_combinations' is an empty list", call. = FALSE)
  }
  
  if (!is.null(true_labels) && length(true_labels) != dim(curves)[1]) {
    stop("'true labels' should have the same length as the number of curves", call. = FALSE)
  }
  
  if (!is.numeric(k) || k %% 1 != 0) {
    stop("'k' should be an integer number", call. = FALSE)
  }
  
  if (!missing(grid)) {
    if (length(grid) != 2 && !is.numeric(grid)) {
      stop("'grid' should be a numeric atomic vector with two elements", call. = FALSE)
    } else if (diff(grid) <= 0) {
      stop("the second element of 'grid' should be greater than the first one", call. = FALSE)
    } else if (grid[1] < 1) {
      stop("the first element of 'grid' should be equal or greater than one", call. = FALSE)
    }
  }
  
  if (missing(vars_combinations)) {
    vars_combinations <-
      generic_vars_combinations(length(dim(curves)) == 3)
  }
  
  # list that maps each clustering method to its corresponding function
  default_clustering_methods <- list(
    "hierarch" = clustInd_hierarch,
    "kmeans"   = clustInd_kmeans,
    "kkmeans"  = clustInd_kkmeans,
    "spc"      = clustInd_spc
  )
  
  # Constants definition
  METHOD_HIERARCH <- c("single", "complete", "average", "centroid", "ward.D2")
  DIST_HIERARCH <- c("euclidean", "manhattan")
  DIST_KMEANS <- c("euclidean", "mahalanobis")
  KERNEL <- c("rbfdot", "polydot")
  METHOD_SVC <- c("kmeans", "kernkmeans")
  CLUSTERING_METHODS <- names(default_clustering_methods)
  
  check_list_parameter(clustering_methods, CLUSTERING_METHODS, "clustering_method")
  check_list_parameter(l_method_hierarch, METHOD_HIERARCH, "l_method_hierarch")
  check_list_parameter(l_dist_hierarch, DIST_HIERARCH, "l_dist_hierarch")
  check_list_parameter(l_dist_kmeans, DIST_KMEANS, "l_dist_kmeans")
  check_list_parameter(l_kernel, KERNEL, "l_kernel")
  
  n_cores <- check_n_cores(n_cores)
  
  # Generate the dataset with the indices
  
  generate_indices_parameters <- list(
    curves  = curves,
    k       = k,
    bs      = bs,
    indices = c("EI", "HI", "MEI", "MHI","MMEI", "MMHI")
  )
  
  if (k) {
    generate_indices_parameters[["k"]] <- k
  }
  
  if (!missing(grid)) {
    generate_indices_parameters[["grid"]] <- grid
  }
  
  ind_curves <- do.call(generate_indices, generate_indices_parameters)
  
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
    "n_cores"           = n_cores
  )
  
  cluster <- list()
  for (method in clustering_methods) {
    method_args <- switch(method,
                          "hierarch" = append(common_clustering_arguments, list(method_list = l_method_hierarch, dist_vector = l_dist_hierarch)),
                          "kmeans"   = append(common_clustering_arguments, list(dist_vector = l_dist_kmeans)),
                          "kkmeans"  = append(common_clustering_arguments, list(kernel_list = l_kernel)),
                          "spc"      = append(common_clustering_arguments, list(kernel_list = l_kernel))
    )
    
    cluster_res <- tryCatch(
      {
        if (verbose) {
          do.call(default_clustering_methods[[method]], method_args)
        } else {
          suppressMessages(quiet(do.call(default_clustering_methods[[method]], method_args)))
        }
      },
      error = function(x) NA
    )
    
    if (!all(is.na(cluster_res))) {
      cluster[[method]] <- cluster_res
    }
  }
  
  if (!is.null(true_labels)) {
    methods <- c()
    metrics <- data.frame(Purity = numeric(0), Fmeasure = numeric(0), RI = numeric(0), Time = numeric(0))
    for (clustering_method in names(cluster)) {
      for (method in names(cluster[[clustering_method]])) {
        methods <- c(methods, method)
        metrics <- rbind(
          metrics,
          c(cluster[[clustering_method]][[method]][["valid"]], cluster[[clustering_method]][[method]][["time"]])
        )
      }
    }
    names(metrics) <- c("Purity", "Fmeasure", "RI", "Time")
    rownames(metrics) <- methods
    
    metrics <- metrics[order(metrics$RI, decreasing = TRUE), ]
    
    if (only_best) {
      metrics <- metrics[1, ]
      best_clustering <- NA
      
      for (clustering_method in cluster) {
        if (rownames(metrics)[[1]] %in% names(clustering_method)) {
          best_clustering <- clustering_method[rownames(metrics)[[1]]]
          break
        }
      }
      
      cluster <- best_clustering
    }
    
    result <- list("cluster" = cluster, "metrics" = metrics)
  } else {
    result <- list("cluster" = cluster)
  }
  
  class(result) <- c("EHyClus", class(result))
  
  attr(result, "n_clusters") <- n_clusters
  attr(result, "vars_combinations") <- vars_combinations
  
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

v_list1 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
                v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

## simulation function 
n_sim = 50
clasif1 <- c()

# Model 1 
set.seed(1234)

for(i in 1:n_sim){
  set.seed(i)
  
  data1 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 1)
  
  clasif1_i <- EHyClus_mm(data1, vars_combinations = v_list1,
                          n_clusters = 2, true_labels = data1_labels)
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

v_list2 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
                v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)
clasif2 <- c()

for(i in 1:n_sim){
  set.seed(i)
  
  data2 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 2)
  
  clasif2_i <- EHyClus_mm(data2, vars_combinations = v_list2,
                          n_clusters = 2, true_labels = data2_labels)
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

v_list3 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
                v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

for(i in 1:n_sim){
  set.seed(i)
  
  data3 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 3)
  
  clasif3_i <- EHyClus_mm(data3, vars_combinations = v_list3,
                          n_clusters = 2, true_labels = data3_labels)
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

v_list4 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,
            v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

for(i in 1:n_sim){
  set.seed(i)
  
  data4 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 4)
  
  clasif4_i <- EHyClus_mm(data4, vars_combinations = v_list4,
                          n_clusters = 2, true_labels = data4_labels)
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

v_list5 <- list(v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,
                v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)


for(i in 1:n_sim){
  set.seed(i)
  
  data5 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 5)
  
  clasif5_i <- EHyClus_mm(data3, vars_combinations = v_list5,
                          n_clusters = 2, true_labels = data5_labels)
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

v_list6 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,
                v17,v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)


for(i in 1:n_sim){
  set.seed(i)
  
  data6 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 6)
  
  clasif6_i <- EHyClus_mm(data6, vars_combinations = v_list6,
                          n_clusters = 2, true_labels = data6_labels)
  clasif6_i <- cbind(names = row.names(clasif6_i$metrics),
                     data.frame(clasif6_i$metrics, row.names=NULL))
  clasif6 <- bind_rows(clasif6, clasif6_i)
  
}

#  Final simulation results
clasif_f6 <- clasif6 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)


# model 7 
clasif7 <- c()

v_list7 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,
                v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

for(i in 1:n_sim){
  set.seed(i)
  
  data7 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 7)
  
  clasif7_i <- EHyClus_mm(data7, vars_combinations = v_list7,
                          n_clusters = 2, true_labels = data7_labels)
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

v_list8 <- list(v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
                v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

for(i in 1:n_sim){
  set.seed(i)
  
  data8 <- ehymet::sim_model_ex1(n = 50, p = 30, i_sim = 8)
  
  clasif8_i <- EHyClus_mm(data8, vars_combinations = v_list8,
                          n_clusters = 2, true_labels = data8_labels)
  clasif8_i <- cbind(names = row.names(clasif8_i$metrics),
                     data.frame(clasif8_i$metrics, row.names=NULL))
  clasif8 <- bind_rows(clasif8, clasif8_i)
  
}

# Final simulation results
clasif_f8 <- clasif8 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

# model 9
clasif9 <- c()

v_list9 <- list(
  v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
                v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

for(i in 1:n_sim){
  set.seed(i)
  
  data9 <- ehymet::sim_model_ex2(n = 50, p = 150, i_sim = 1)
  
  clasif9_i <- EHyClus_mm(data9, vars_combinations = v_list9,
                          n_clusters = 2, true_labels = data9_labels)
  clasif9_i <- cbind(names = row.names(clasif9_i$metrics),
                     data.frame(clasif9_i$metrics, row.names=NULL))
  clasif9 <- bind_rows(clasif9, clasif9_i)
  
}

# Final simulation results
clasif_f9 <- clasif9 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)


# model 10
clasif10 <- c()

v_list10 <- list(
  v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,
  v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30)

for(i in 1:n_sim){
  set.seed(i)
  
  data10 <- ehymet::sim_model_ex2(n = 50, p = 150, i_sim = 2)
  
  clasif10_i <- EHyClus_mm(data10, vars_combinations = v_list10,
                          n_clusters = 2, true_labels = data10_labels)
  clasif10_i <- cbind(names = row.names(clasif10_i$metrics),
                     data.frame(clasif10_i$metrics, row.names=NULL))
  clasif10 <- bind_rows(clasif10, clasif10_i)
  
}

# Final simulation results
clasif_f10 <- clasif10 %>%
  group_by(names) %>%
  mutate(count = n()) %>%
  summarise_all(mean)

## Box-plots for comparison 

# model 1
library(ggplot2)
library(reshape2)
library(tidyr)

clasif_f1_b <- tibble(names = c(rep("hierarchical_clust",230), rep("kkmeans_clust",46),
                                rep("kmeans_clust",46)),
                      Metric = c(rep("RI",230), rep("RI", 46), rep("RI",46)),
                      Value = clasif_f1$RI)

ggplot(clasif_f1_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 1",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 2
clasif_f2_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46)),
                      Value = clasif_f2$RI)

ggplot(clasif_f2_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 2",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 3
clasif_f3_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46)),
                      Value = clasif_f3$RI)

ggplot(clasif_f3_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 3",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 4
clasif_f4_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust", 46), rep("spc_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46),
                                 rep("RI", 46)),
                      Value = clasif_f4$RI)

ggplot(clasif_f4_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 4",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 5
clasif_f5_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46)),
                      Value = clasif_f5$RI)

ggplot(clasif_f5_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 5",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 6
clasif_f6_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust", 46), rep("spc_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46),
                                 rep("RI", 46)),
                      Value = clasif_f6$RI)

ggplot(clasif_f6_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 6",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 7
clasif_f7_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust", 46), rep("spc_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46),
                                 rep("RI", 46)),
                      Value = clasif_f7$RI)

ggplot(clasif_f7_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 7",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 8
clasif_f8_b <- tibble(names = c(rep("hierarchical_clust", 270), rep("kkmeans_clust",54),
                                rep("spc_clust", 54)),
                      Metric = c(rep("RI", 270), rep("RI", 54), rep("RI", 54)),
                      Value = clasif_f8$RI)

ggplot(clasif_f8_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 8",
       x = "Clustering Method", y = "RI") +
  theme_minimal()
# dev.off()
# model 9
clasif_f9_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust",46),rep("spc_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46), rep("RI",46)),
                      Value = clasif_f9$RI)

ggplot(clasif_f9_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 9",
       x = "Clustering Method", y = "RI") +
  theme_minimal()

# model 10
clasif_f10_b <- tibble(names = c(rep("hierarchical_clust", 230), rep("kkmeans_clust",46),
                                rep("kmeans_clust",46),rep("spc_clust", 46)),
                      Metric = c(rep("RI", 230), rep("RI", 46), rep("RI", 46),rep("RI", 46)),
                      Value = clasif_f10$RI)

ggplot(clasif_f10_b, aes(x=names, y=Value)) + 
  geom_boxplot() + 
  labs(title = "RI of different clustering methods for n = 50 simulations of Model 10",
       x = "Clustering Method", y = "RI") +
  theme_minimal()
