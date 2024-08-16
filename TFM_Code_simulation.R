#################################################
# TFM Code - Beatriz Espadanal Gonçalves ########
#################################################
# Data simulation ###############################
#################################################


library("ehymet")
library("dplyr")

check_list_parameter <- function(argument, parameter_values, parameter_name) {
  if (length(argument) == 0) {
    stop("parameter '", parameter_name, "' should have at least one element.", call. = FALSE)
  }
  
  if (any(duplicated(argument))) {
    stop("duplicated argument in '", parameter_name, "'.", call. = FALSE)
  }
  
  indices <- pmatch(argument, parameter_values)
  if (any(is.na(indices))) {
    stop("invalid argument in '", parameter_name, "': ", paste(argument[is.na(indices)], collapse = ", "), ".",
         call. = FALSE
    )
  }
}

check_n_cores <- function(n_cores) {
  if (n_cores < 1 || n_cores %% 1 != 0) {
    stop("'n_cores' must be an integer number greater than '1'", call. = FALSE)
  }
  
  if (.Platform$OS.type != "unix" && n_cores > 1) {
    warning(
      "Running this function using multiples cores is only supported on unix systems.",
      "Setting 'n_cores' parameter to '1'"
    )
    return(1)
  }
  
  return(n_cores)
}

funspline <- function(curves, k, bs = "cr", grid) {
  curves_dim <- length(dim(curves))
  
  tfb_params <- list(bs = bs)
  
  if (!missing(k)) {
    tfb_params[["k"]] <- k
  }
  
  if (!missing(grid)) {
    grid <- seq(grid[1], grid[2], length.out = dim(curves)[2])
    tfb_params[["arg"]] <- grid
  }
  
  if (curves_dim == 2) {
    tfb_params[["data"]] <- curves
    
    ys <- suppressMessages(do.call(tf::tfb, tfb_params))
    
    # Evaluate smoothed data and derivatives
    smooth <- as.matrix(ys) # smoothed data
    
    if (!missing(grid)) {
      deriv <- as.matrix(tf::tf_derive(ys, arg = grid, order = 1))
      deriv2 <- as.matrix(tf::tf_derive(ys, arg = grid, order = 2))
    } else {
      deriv <- as.matrix(tf::tf_derive(ys, order = 1))
      deriv2 <- as.matrix(tf::tf_derive(ys, order = 2))
    }
  } else {
    n_curves <- dim(curves)[1]
    l_curves <- dim(curves)[2]
    d_curves <- dim(curves)[3]
    
    # Initialize empty dataframes to store the results
    smooth <- array(rep(NaN, n_curves * l_curves), dim = c(n_curves, l_curves, d_curves))
    deriv <- array(rep(NaN, n_curves * (l_curves)), dim = c(n_curves, l_curves, d_curves))
    deriv2 <- array(rep(NaN, n_curves * (l_curves)), dim = c(n_curves, l_curves, d_curves))
    
    for (d in seq_len(dim(curves)[3])) {
      tfb_params[["data"]] <- curves[, , d]
      
      # Smooth data using B-spline basis
      ys <- suppressMessages(do.call(tf::tfb, tfb_params))
      
      # Evaluate smoothed data and derivatives
      smooth[, , d] <- as.matrix(ys) # smoothed data
      if (!missing(grid)) {
        deriv[, , d] <- as.matrix(tf::tf_derive(ys, arg = grid, order = 1))
        deriv2[, , d] <- as.matrix(tf::tf_derive(ys, arg = grid, order = 2))
      } else {
        deriv[, , d] <- as.matrix(tf::tf_derive(ys, order = 1))
        deriv2[, , d] <- as.matrix(tf::tf_derive(ys, order = 2))
      }
    }
  }
  
  list(
    "smooth" = smooth,
    "deriv"  = deriv,
    "deriv2" = deriv2
  )
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

map_index_name_to_function <- function(index) {
  switch(index,
         "EI"  = EI,
         "HI"  = HI,
         "MEI" = MEI,
         "MHI" = MHI,
         "MMEI" = MMEI,
         "MMHI" = MMHI
  )
}


# Epigraph index 
EI <- function(curves, ...) {
  UseMethod("EI")
}

EI.matrix <- function(curves, ...) {
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  
  index <- apply(curves, 1, function(y) {
    sum(apply(curves, 1, function(x) {
      sum(x >= y) == l_curves
    }))
  }) / n_curves
  
  return(1 - index)
}

EI.array <- function(curves, ...) {
  if (length(dim(curves)) != 3) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  d_curves <- dim(curves)[3]
  
  index <- colSums(Reduce("*", lapply(1:d_curves, function(k) {
    sapply(1:n_curves, function(j) {
      colSums(sapply(1:n_curves, function(i) {
        curves[i, , k] >= curves[j, , k]
      })) == l_curves
    })
  })))
  
  return(1 - index / n_curves)
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
  
  index <- apply(curves, 1, function(y) {
    sum(apply(curves, 1, function(x) {
      sum(x <= y) == l_curves
    }))
  }) / n_curves
  
  return(index)
}

HI.array <- function(curves, ...) {
  if (length(dim(curves)) != 3) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  d_curves <- dim(curves)[3]
  
  index <- colSums(Reduce("*", lapply(1:d_curves, function(k) {
    sapply(1:n_curves, function(j) {
      colSums(sapply(1:n_curves, function(i) {
        curves[i, , k] <= curves[j, , k]
      })) == l_curves
    })
  })))
  
  return(index / n_curves)
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
  n_a <- n_curves - rankm + 1
  index <- rowSums(n_a) / (n_curves * l_curves)
  return(1 - index)
}

MEI.array <- function(curves, ...) {
  if (length(dim(curves)) != 3) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  n_curves <- dim(curves)[1]
  l_curves <- dim(curves)[2]
  d_curves <- dim(curves)[3]
  
  index <- sapply(1:n_curves, function(j) {
    sum(Reduce("*", lapply(1:d_curves, function(k) {
      sapply(1:n_curves, function(i) {
        curves[i, , k] >= curves[j, , k]
      })
    })))
  })
  
  return(1 - index / (n_curves * l_curves))
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
  
  index <- sapply(1:n_curves, function(j) {
    sum(Reduce("*", lapply(1:d_curves, function(k) {
      sapply(1:n_curves, function(i) {
        curves[i, , k] <= curves[j, , k]
      })
    })))
  })
  
  return(index / (n_curves * l_curves))
}

MHI.default <- function(curves, ...) {
  stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
}


# New Modified Epigraph function
MMEI <- function(curves,...){
  UseMethod("MMEI")
}

MMEI.matrix <- function(curves) {
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

MMEI.default <- function(curves,...) {
  stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
}

# New Modified Hypograph function
MMHI <- function(curves,...){
  UseMethod("MMHI")
}

MMHI.matrix <- function(curves) {
  return(MMEI(-curves))
}

MMHI.default <- function(curves, ...){
  stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
}


generate_indices <- function(curves, k, grid, bs = "cr",
                             indices = c("EI", "HI", "MEI", "MHI", "MMEI", "MMHI"), ...) {
  # define indices constant
  INDICES <- c("EI", "HI", "MEI", "MHI", "MMEI", "MMHI")
  curves_dim <- length(dim(curves))
  
  # stop conditions
  if (!(curves_dim %in% c(2, 3)) || is.null(curves_dim)) {
    stop("'curves' should be a matrix or a 3-dimensional array", call. = FALSE)
  }
  
  check_list_parameter(indices, INDICES, "indices")
  
  funspline_parameters <- list(
    curves  = curves,
    bs      = bs
  )
  
  if (!missing(k)) {
    funspline_parameters[["k"]] <- k
  }
  
  if (!missing(grid)) {
    funspline_parameters[["grid"]] <- grid
  }
  
  fun_data <- do.call(funspline, funspline_parameters)
  
  ind_data <- as.data.frame(matrix(NA, nrow = dim(fun_data$smooth)[1], ncol = 0))
  
  # Loop through the list of functions and apply them to the smoothed and
  # its first and second derivatives
  for (index in indices) {
    smooth_col <- paste0("dta", index)
    deriv_col <- paste0("ddta", index)
    deriv2_col <- paste0("d2dta", index)
    
    smooth_result <- map_index_name_to_function(index)(fun_data$smooth)
    deriv_result <- map_index_name_to_function(index)(fun_data$deriv)
    deriv2_result <- map_index_name_to_function(index)(fun_data$deriv2)
    
    ind_data <- cbind(
      ind_data,
      stats::setNames(
        data.frame(smooth_result, deriv_result, deriv2_result),
        c(smooth_col, deriv_col, deriv2_col)
      )
    )
  }
  
  # Standardize MMEI and MMHI
  if ("MMEI" %in% indices){
    mmei_cols <- grep("dtaMMEI|ddtaMMEI|d2dtaMMEI", names(ind_data), value = TRUE)
    ind_data[mmei_cols] <- scale(ind_data[mmei_cols])
  }
  
  if ("MMHI" %in% indices){
    mmhi_cols <- grep("dtaMMHI|ddtaMMHI|d2dtaMMHI", names(ind_data), value = TRUE)
    ind_data[mmhi_cols] <- scale(ind_data[mmhi_cols])
  }
  
  ind_data
}

# data models ex1
set.seed(1234)
data_2 <- sim_model_ex1(n = 50, p = 30, i_sim = 1)
data_3 <- sim_model_ex1(n = 50, p = 30, i_sim = 2)
data_4 <- sim_model_ex1(n = 50, p = 30, i_sim = 3)
data_5 <- sim_model_ex1(n = 50, p = 30, i_sim = 4)
data_6 <- sim_model_ex1(n = 50, p = 30, i_sim = 5)
data_7 <- sim_model_ex1(n = 50, p = 30, i_sim = 6)
data_8 <- sim_model_ex1(n = 50, p = 30, i_sim = 7)
data_9 <- sim_model_ex1(n = 50, p = 30, i_sim = 8)

data2_labels <- c(rep(1,50), rep(2,50))
data3_labels <- c(rep(1,50), rep(3,50))
data4_labels <- c(rep(1,50), rep(4,50))
data5_labels <- c(rep(1,50), rep(5,50))
data6_labels <- c(rep(1,50), rep(6,50))
data7_labels <- c(rep(1,50), rep(7,50))
data8_labels <- c(rep(1,50), rep(8,50))
data9_labels <- c(rep(1,50), rep(9,50))

# data models ex2
set.seed(123)
data_11 <- sim_model_ex2(i_sim = 1)
data_12 <- sim_model_ex2(i_sim = 2)

data11_labels <- c(rep(10,50), rep(11,50))
data12_labels <- c(rep(10,50), rep(12,50))
