#' @title Reduce imbalance in a set of samples
#' @name sits_reduce_imbalance
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description
#' Takes a sits tibble with different labels and
#' returns a new tibble. Deals with class imbalance
#' using the synthetic minority oversampling technique (SMOTE) or its
#' geometric variant (G-SMOTE) for oversampling. Undersampling is done
#' using the SOM methods available in the sits package.
#'
#' @param  samples              Sample set to rebalance
#' @param  n_samples_over       Number of samples to oversample
#'                              for classes with samples less than this number.
#' @param  n_samples_under      Number of samples to undersample
#'                              for classes with samples more than this number.
#' @param  method               Method for oversampling, one of "smote" or
#'                              "gsmote" (default = "smote").
#' @param  truncation_factor    G-SMOTE truncation factor, in [-1, 1].
#'                              Ignored when \code{method = "smote"}
#'                              (default = 1.0).
#' @param  deformation_factor   G-SMOTE deformation factor, in [0, 1].
#'                              Ignored when \code{method = "smote"}
#'                              (default = 0.0).
#' @param  multicores           Number of cores to process the data (default 2).
#'
#' @return A sits tibble with reduced sample imbalance.
#'
#'
#' @note
#' Many training samples for Earth observation data analysis are imbalanced.
#' This situation arises when the distribution of samples associated
#' with each label is uneven.
#' Sample imbalance is an undesirable property of a training set.
#' Reducing sample imbalance improves classification accuracy.
#'
#' The function \code{sits_reduce_imbalance} increases the number of samples
#' of least frequent labels, and reduces the number of samples of most
#' frequent labels. To generate new samples, \code{sits}
#' uses the SMOTE method that estimates new samples by considering
#' the cluster formed by the nearest neighbors of each minority label.
#'
#' G-SMOTE (\code{method = "gsmote"}) generalizes the linear
#' interpolation of SMOTE between a minority point and a neighbor to
#' sampling inside a hyper-sphere (optionally truncated and deformed into a
#' hyper-spheroid via \code{truncation_factor} and
#' \code{deformation_factor}) around each minority point, which produces
#' more varied synthetic data.
#'
#' To perform undersampling, \code{sits_reduce_imbalance}) builds a SOM map
#' for each majority label based on the required number of samples.
#' Each dimension of the SOM is set to ceiling(sqrt(new_number_samples/4))
#' to allow a reasonable number of neurons to group similar samples.
#' After calculating the SOM map, the algorithm extracts four samples
#' per neuron to generate a reduced set of samples that approximates
#' the variation of the original one.
#' See also \code{\link[sits]{sits_som_map}}.
#'
#' @references
#' The reference paper on SMOTE is
#' N. V. Chawla, K. W. Bowyer, L. O.Hall, W. P. Kegelmeyer,
#' “SMOTE: synthetic minority over-sampling technique”,
#' Journal of artificial intelligence research, 321-357, 2002,
#' \doi{10.1613/jair.953}.
#'
#' The SOM map technique for time series is described in the paper:
#' Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picoli,
#' Rolf Simoes, “Quality control and class noise reduction of satellite
#' image time series”. ISPRS Journal of Photogrammetry and Remote Sensing,
#' vol. 177, pp 75-88, 2021. \doi{10.1016/j.isprsjprs.2021.04.014}.
#'
#' The G-SMOTE method is described in:
#' Douzas, G., Bacao, F., "Geometric SMOTE: Effective oversampling for
#' imbalanced learning through a geometric extension of SMOTE",
#' arXiv:1709.07377, 2017. \url{https://arxiv.org/abs/1709.07377}.
#'
#' G-SMOTE was later published as:
#' Douzas, G., Bacao, F., "Geometric SMOTE: a geometrically enhanced
#' drop-in replacement for SMOTE", Information Sciences, vol. 501,
#' pp. 118-135, 2019. \doi{10.1016/j.ins.2019.06.007}.
#'
#' @examples
#' if (sits_run_examples()) {
#'     # print the labels summary for a sample set
#'     summary(samples_modis_ndvi)
#'     # reduce the sample imbalance
#'     new_samples <- sits_reduce_imbalance(samples_modis_ndvi,
#'         n_samples_over = 200,
#'         n_samples_under = 200,
#'         multicores = 1
#'     )
#'     # print the labels summary for the rebalanced set
#'     summary(new_samples)
#'     # reduce the sample imbalance using G-SMOTE
#'     new_samples_gsmote <- sits_reduce_imbalance(samples_modis_ndvi,
#'         n_samples_over = 200,
#'         n_samples_under = 200,
#'         method = "gsmote",
#'         multicores = 1
#'     )
#'     summary(new_samples_gsmote)
#' }
#' @export
sits_reduce_imbalance <- function(samples,
                                  n_samples_over = 200L,
                                  n_samples_under = 400L,
                                  method = "smote",
                                  truncation_factor = 1.0,
                                  deformation_factor = 0.0,
                                  multicores = 2L) {
    # set caller to show in errors
    .check_set_caller("sits_reduce_imbalance")
    # pre-conditions
    .check_samples_train(samples)
    .check_int_parameter(n_samples_over)
    .check_int_parameter(n_samples_under)
    .check_chr_within(method, within = c("smote", "gsmote"))
    .check_num_parameter(truncation_factor, min = -1.0, max = 1.0)
    .check_num_parameter(deformation_factor, min = 0.0, max = 1.0)

    # check if number of required samples are correctly entered
    .check_that(n_samples_under >= n_samples_over,
        msg = .conf("messages", "sits_reduce_imbalance_samples")
    )
    # get the bands and the labels
    bands <- .samples_bands(samples)
    samples_labels <- .samples_labels(samples)
    # params of output tibble
    lat <- 0.0
    long <- 0.0
    start_date <- samples[["start_date"]][[1L]]
    end_date <- samples[["end_date"]][[1L]]
    cube <- samples[["cube"]][[1L]]
    timeline <- .samples_timeline(samples)
    # get classes to undersample
    classes_under <- samples |>
        summary() |>
        dplyr::filter(.data[["count"]] >= n_samples_under) |>
        dplyr::pull("label")
    # get classes to oversample
    classes_over <- samples |>
        summary() |>
        dplyr::filter(.data[["count"]] <= n_samples_over) |>
        dplyr::pull("label")
    # create an output tibble
    new_samples <- .tibble()
    # under sampling
    if (.has(classes_under)) {
        # undersample classes with lots of data
        samples_under_new <- .som_undersample(
            samples = samples,
            classes_under = classes_under,
            n_samples_under = n_samples_under,
            multicores = multicores
        )

        # join get new samples
        new_samples <- dplyr::bind_rows(new_samples, samples_under_new)
    }
    # oversampling
    if (.has(classes_over)) {
        # Prepare parallel processing
        if (.parallel_start(workers = multicores)) {
            on.exit(.parallel_stop(), add = TRUE)
        }
        # for each class, build synthetic samples using SMOTE
        samples_over_new <- .parallel_map(classes_over, function(cls) {
            # select the samples for the class
            samples_bands <- purrr::map(bands, function(band) {
                # selection of band
                dist_band <- samples |>
                    .samples_select_bands(band) |>
                    dplyr::filter(.data[["label"]] == cls) |>
                    .predictors()
                dist_band <- dist_band[-1L]
                # oversampling of band for the class
                dist_over <- .smote_oversample(
                    data = dist_band,
                    cls = cls,
                    cls_col = "label",
                    m = n_samples_over,
                    method = method,
                    truncation_factor = truncation_factor,
                    deformation_factor = deformation_factor
                )
                # put the oversampled data into a samples tibble
                samples_band <- slider::slide_dfr(dist_over, function(row) {
                    time_series <- tibble::tibble(
                        Index = as.Date(timeline),
                        values = unname(as.numeric(row[-1L]))
                    )
                    colnames(time_series) <- c("Index", band)
                    tibble::tibble(
                        longitude = long,
                        latitude = lat,
                        start_date = as.Date(start_date),
                        end_date = as.Date(end_date),
                        label = row[["label"]],
                        cube = cube,
                        time_series = list(time_series)
                    )
                })
                class(samples_band) <- c("sits", class(samples_band))
                samples_band
            })
            tb_class_new <- samples_bands[[1L]]
            for (i in seq_along(samples_bands)[-1L]) {
                tb_class_new <- sits_merge(tb_class_new, samples_bands[[i]])
            }
            tb_class_new
        })
        # bind oversampling results
        samples_over_new <- dplyr::bind_rows(samples_over_new)
        new_samples <- dplyr::bind_rows(new_samples, samples_over_new)
    }
    # keep classes (no undersampling nor oversampling)
    classes_ok <- samples_labels[!(samples_labels %in% classes_under |
        samples_labels %in% classes_over)]
    if (.has(classes_ok)) {
        samples_classes_ok <- dplyr::filter(
            samples,
            .data[["label"]] %in% classes_ok
        )
        new_samples <- dplyr::bind_rows(new_samples, samples_classes_ok)
    }
    # remove SOM additional columns
    colnames_sits <- setdiff(colnames(new_samples), c("id_neuron", "id_sample"))
    # return new sample set
    return(new_samples[, colnames_sits])
}
