#' @title Oversample a dataset by SMOTE.
#' @name .smote_oversample
#' @keywords internal
#' @noRd
#' @description
#' Lifted from R package "scutr" by Keenan Ganz
#'
#' @param data Dataset to be oversampled.
#' @param cls Class to be oversampled.
#' @param cls_col Column containing class information.
#' @param m Desired number of samples in the oversampled data.
#' @param method Oversampling engine, one of "smote" or "gsmote".
#' @param truncation_factor G-SMOTE truncation factor, in [-1, 1].
#'                          Ignored when \code{method = "smote"}.
#' @param deformation_factor G-SMOTE deformation factor, in [0, 1].
#'                          Ignored when \code{method = "smote"}.
#'
#' @return The oversampled dataset.
#'
.smote_oversample <- function(data,
                               cls,
                               cls_col,
                               m,
                               method = "smote",
                               truncation_factor = 1.0,
                               deformation_factor = 0.0) {
    col_ind <- which(names(data) == cls_col)
    orig_cols <- names(data)
    dup_size <- ceiling(m / sum(data[[cls_col]] == cls))
    # set the class to whether it is equal to the minority class
    data[[cls_col]] <- as.factor(data[[cls_col]] == cls)
    # SMOTE breaks for one-dim datasets. This adds a dummy column
    # so SMOTE can execute in that case. This does not affect how data is
    # synthesized
    if (ncol(data) == 2L) {
        data[["dummy__col__"]] <- 0.0
    }
    # perform SMOTE or G-SMOTE
    smote_ret <- if (method == "gsmote") {
        .gsmote_apply(
            data = data[, -col_ind],
            target = data[, col_ind],
            dup_size = dup_size,
            truncation_factor = truncation_factor,
            deformation_factor = deformation_factor
        )
    } else {
        .smote_apply(
            data = data[, -col_ind],
            target = data[, col_ind],
            dup_size = dup_size
        )
    }
    # rbind the original observations and sufficient samples of the synthetic
    # ones
    orig <- smote_ret[["orig_p"]]
    target_samp <- m - nrow(orig)
    synt <- smote_ret[["syn_data"]][
        sample.int(
            nrow(smote_ret[["syn_data"]]),
            size = target_samp,
            replace = target_samp > nrow(smote_ret[["syn_data"]])
        ),
    ]
    d_prime <- rbind(orig, synt)
    colnames(d_prime)[[ncol(d_prime)]] <- cls_col
    d_prime[[cls_col]] <- cls
    # remove the dummy column if necessary
    d_prime <- d_prime[, names(d_prime) != "dummy__col__"]
    # reorder the columns to be the same as the original data
    d_prime[, orig_cols]
}

#' @title Oversample a dataset by SMOTE.
#' @name .smote_apply
#' @keywords internal
#' @noRd
#' @description
#' Lifted from R package "smotefamily"
#' to reduce number of dependencies in "sits".
#' @author Wacharasak Siriseriwan <wacharasak.s@gmail.com>
#'
#'
#' @param data Dataset to be oversampled.
#' @param target Target data set
#' @param k The number of nearest neighbors during sampling process
#' @param dup_size The maximum times of synthetic minority instances
#'                  over original majority instances in the oversampling.
#'
#' @references
#'   Chawla, N., Bowyer, K., Hall, L. and Kegelmeyer, W. 2002.
#'   SMOTE: Synthetic minority oversampling technique.
#'   Journal of Artificial Intelligence Research. 16, 321-357.
#' @return A list with the following values.
#'
.smote_apply <- function(data, target, k = 5L, dup_size = 0L) {
    ncol_data <- ncol(data) # The number of attributes
    n_target <- table(target)
    # Extract a set of positive instances
    p_set <- subset(
        data,
        target == names(which.min(n_target))
    )[sample(min(n_target)), ]
    n_set <- subset(
        data,
        target != names(which.min(n_target))
    )
    p_class <- rep(names(which.min(n_target)), nrow(p_set))

    n_class <- target[target != names(which.min(n_target))]
    # The number of positive instances
    size_p <- nrow(p_set)
    # Get k nearest neighbors
    knear <- .smote_knearest(p_set, p_set, k)
    sum_dup <- dup_size
    syn_dat <- NULL
    for (i in seq_len(size_p)) {
        if (is.matrix(knear)) {
            pair_idx <- knear[i, ceiling(stats::runif(sum_dup) * k)]
        } else {
            pair_idx <- rep(knear[i], sum_dup)
        }
        g <- stats::runif(sum_dup)
        p_i <- matrix(unlist(p_set[i, ]), sum_dup, ncol_data, byrow = TRUE)
        q_i <- as.matrix(p_set[pair_idx, ])
        syn_i <- p_i + g * (q_i - p_i)
        syn_dat <- rbind(syn_dat, syn_i)
    }

    p_set[, ncol_data + 1L] <- p_class
    colnames(p_set) <- c(colnames(data), "class")
    n_set[, ncol_data + 1L] <- n_class
    colnames(n_set) <- c(colnames(data), "class")

    rownames(syn_dat) <- NULL
    syn_dat <- data.frame(syn_dat)
    syn_dat[, ncol_data + 1L] <- rep(names(which.min(n_target)), nrow(syn_dat))
    colnames(syn_dat) <- c(colnames(data), "class")
    new_data <- rbind(p_set, syn_dat, n_set)
    rownames(new_data) <- NULL
    d_result <- list(
        data = new_data,
        syn_data = syn_dat,
        orig_n = n_set,
        orig_p = p_set,
        k = k,
        k_all = NULL,
        dup_size = sum_dup,
        outcast = NULL,
        eps = NULL,
        method = "SMOTE"
    )
    class(d_result) <- "gen_data"

    return(d_result)
}
#' @title Find K nearest neighbors
#' @keywords internal
#' @noRd
#' @param q_data  Query data matrix
#' @param p_data  Input data matrix
#' @param n_clust maximum number of nearest neighbors to search
#' @return Index matrix of K nearest neighbor for each instance
.smote_knearest <- function(q_data, p_data, n_clust) {
    .check_require_packages("FNN")

    kn_dist <- FNN::knnx.index(q_data, p_data,
        k = (n_clust + 1L), algorithm = "kd_tree"
    )
    kn_dist <- kn_dist * (kn_dist != row(kn_dist))
    que <- which(kn_dist[, 1L] > 0.0)
    for (i in que) {
        kn_dist[i, which(kn_dist[i, ] == 0.0)] <- kn_dist[[i, 1L]]
        kn_dist[[i, 1L]] <- 0.0
    }
    kn_dist[, 2L:(n_clust + 1L)]
}

#' @title Euclidean norm of a vector
#' @name .gsmote_norm
#' @keywords internal
#' @noRd
#' @param x Numeric vector.
#' @return The Euclidean (L2) norm of \code{x}.
.gsmote_norm <- function(x) {
    sqrt(sum(x^2))
}

#' @title Generate one synthetic sample using Geometric SMOTE
#' @name .gsmote_sample
#' @keywords internal
#' @noRd
#' @description
#' Generates an artificial point inside a (possibly truncated and
#' deformed) hyper-sphere centered on \code{center}, using
#' \code{surface_point} to set the direction and radius. Direct port of
#' \code{make_geometric_sample()} from the Python package
#' "imbalanced-learn-extra".
#'
#' @param center              Center point (numeric vector).
#' @param surface_point       Neighbor point defining direction and radius.
#' @param truncation_factor   Truncation factor, in [-1, 1].
#' @param deformation_factor  Deformation factor, in [0, 1].
#'
#' @references
#' Douzas, G., Bacao, F. (2019). Geometric SMOTE: a geometrically
#' enhanced drop-in replacement for SMOTE. Information Sciences, 501,
#' 118-135. \doi{10.1016/j.ins.2019.06.007}.
#'
#' @return A new synthetic point (numeric vector).
.gsmote_sample <- function(center,
                            surface_point,
                            truncation_factor = 1.0,
                            deformation_factor = 0.0) {
    # zero radius case
    if (all(center == surface_point)) {
        return(center)
    }
    # generate a point on the surface of a unit hyper-sphere
    radius <- .gsmote_norm(center - surface_point)
    normal_samples <- stats::rnorm(length(center))
    point_on_unit_sphere <- normal_samples / .gsmote_norm(normal_samples)
    point <- (stats::runif(1L)^(1.0 / length(center))) * point_on_unit_sphere

    # parallel unit vector
    parallel_unit_vector <- (surface_point - center) /
        .gsmote_norm(surface_point - center)

    # truncation
    proj <- sum(point * parallel_unit_vector)
    close_to_opposite_boundary <- truncation_factor > 0.0 &&
        proj < truncation_factor - 1.0
    close_to_boundary <- truncation_factor < 0.0 &&
        proj > truncation_factor + 1.0
    if (close_to_opposite_boundary || close_to_boundary) {
        point <- point - 2.0 * proj * parallel_unit_vector
    }

    # deformation
    parallel_point_position <- sum(point * parallel_unit_vector) *
        parallel_unit_vector
    perpendicular_point_position <- point - parallel_point_position
    point <- parallel_point_position +
        (1.0 - deformation_factor) * perpendicular_point_position

    # translation
    center + radius * point
}

#' @title Oversample a dataset by Geometric SMOTE.
#' @name .gsmote_apply
#' @keywords internal
#' @noRd
#' @description
#' Drop-in replacement for \code{.smote_apply()}: same input/output
#' contract, but synthesizes each new point with \code{.gsmote_sample()}
#' instead of linear interpolation. Only the "minority" selection
#' strategy is implemented (neighbors within the same class), since
#' callers already isolate one class against the rest before calling
#' this function.
#'
#' @param data              Dataset to be oversampled.
#' @param target            Target data set.
#' @param k                 The number of nearest neighbors during
#'                          sampling process.
#' @param dup_size          The maximum times of synthetic minority
#'                          instances over original majority instances
#'                          in the oversampling.
#' @param truncation_factor G-SMOTE truncation factor, in [-1, 1].
#' @param deformation_factor G-SMOTE deformation factor, in [0, 1].
#'
#' @references
#' Douzas, G., Bacao, F. (2019). Geometric SMOTE: a geometrically
#' enhanced drop-in replacement for SMOTE. Information Sciences, 501,
#' 118-135. \doi{10.1016/j.ins.2019.06.007}.
#'
#' @return A list with the same structure as \code{.smote_apply()}.
.gsmote_apply <- function(data,
                           target,
                           k = 5L,
                           dup_size = 0L,
                           truncation_factor = 1.0,
                           deformation_factor = 0.0) {
    ncol_data <- ncol(data) # The number of attributes
    n_target <- table(target)
    # Extract a set of positive instances
    p_set <- subset(
        data,
        target == names(which.min(n_target))
    )[sample(min(n_target)), ]
    n_set <- subset(
        data,
        target != names(which.min(n_target))
    )
    p_class <- rep(names(which.min(n_target)), nrow(p_set))

    n_class <- target[target != names(which.min(n_target))]
    # The number of positive instances
    size_p <- nrow(p_set)
    # Get k nearest neighbors
    knear <- .smote_knearest(p_set, p_set, k)
    sum_dup <- dup_size
    syn_dat <- NULL
    for (i in seq_len(size_p)) {
        if (is.matrix(knear)) {
            pair_idx <- knear[i, ceiling(stats::runif(sum_dup) * k)]
        } else {
            pair_idx <- rep(knear[i], sum_dup)
        }
        center <- as.numeric(p_set[i, ])
        syn_i <- do.call(rbind, purrr::map(pair_idx, function(j) {
            .gsmote_sample(
                center = center,
                surface_point = as.numeric(p_set[j, ]),
                truncation_factor = truncation_factor,
                deformation_factor = deformation_factor
            )
        }))
        syn_dat <- rbind(syn_dat, syn_i)
    }

    p_set[, ncol_data + 1L] <- p_class
    colnames(p_set) <- c(colnames(data), "class")
    n_set[, ncol_data + 1L] <- n_class
    colnames(n_set) <- c(colnames(data), "class")

    rownames(syn_dat) <- NULL
    syn_dat <- data.frame(syn_dat)
    syn_dat[, ncol_data + 1L] <- rep(names(which.min(n_target)), nrow(syn_dat))
    colnames(syn_dat) <- c(colnames(data), "class")
    new_data <- rbind(p_set, syn_dat, n_set)
    rownames(new_data) <- NULL
    d_result <- list(
        data = new_data,
        syn_data = syn_dat,
        orig_n = n_set,
        orig_p = p_set,
        k = k,
        k_all = NULL,
        dup_size = sum_dup,
        outcast = NULL,
        eps = NULL,
        method = "G-SMOTE"
    )
    class(d_result) <- "gen_data"

    return(d_result)
}
