
#' \code{johnson_loindenstrauss_min_dim} estimated the minimal size of the
#' random subspace to guarantee a bounded distortion introduced by the random projection.
#'
#' @param n_samples is the size of the subspace
#' @param eps the epsilon error which is acceptable.
#' @return the conservative estiamtion of the minimal size of the random subspace give eps.
#' @export
JohnsonLindenstraussMinDim <- function(n_samples, eps=0.1) {
    denominator = (eps ** 2 / 2) - (eps ** 3 / 3)
    return (min(floor(4 * log(n_samples) / denominator),n_samples))
}

#' gaussian_random_proj generates the dimension reduction "rotation" which is to be applied to
#' the numeric matrix of interest.
#'
#' @param row is the number of rows in this rotation matrix.
#' @param col is the number of columns for this rotation matrix.
#' @return the rotation matrix of dimensions \code{row} by \code{col}
#' @export
GenerateGaussianRandomProj <- function(row, col) {
    mat <- rnorm(row*col, 0, 1/(row**0.5))
    dim(mat) <- c(row, col)
    return(mat)
}


#' gaussian_random_projection applies the projection onto the data.frame or matrix with the given
#' dimension or epsilon parameter.
#'
#' @param A is the data.frame or matrix which is of interest
#' @param n_features is the number of dimensions to be reduced.
#' @param eps is the epsilon error to be reduced according to the Johnson Loindenstrauss Lemma
#' @return the dimension original information, rotation and the resulting dimension reduced matrix
#' @export
RandomProjection <- function(A, n_features=NULL, eps=0.1) {
    if (is.data.frame(A)) {
        if (sum(sapply(A, is.numeric)) != length(names(A))){
            warning("Not all columns are numeric. Non-numeric columns will be ignored.")
        }
        A <- as.matrix(A[, sapply(A, is.numeric)])
    }
    
    get_dim <- dim(A)
    if (is.null(n_features)){
        n_features = JohnsonLindenstraussMinDim(get_dim[2])
    }
    
    R = GenerateGaussianRandomProj(get_dim[2], n_features)
    return(list(A=A, R=R, RP=A %*% as.matrix(R)))
}


