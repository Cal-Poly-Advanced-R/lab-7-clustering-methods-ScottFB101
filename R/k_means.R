#' Implements K means by hands
#'
#' @param dat A data set
#' @param k The number of desired clusters
#' @param pca Boolean value indicating whether or not to run Principal Component Analysis prior to K means
#'
#' @return
#'
#' @import dplyr
#'
#' @export

k_means <- function(dat, k, pca = FALSE) {

    #Number of rows/observations
    num_obs <- nrow(dat)

    #Randomly select K observations
    random_obs <- sample(num_obs, size = k)
    centers <- slice(dat, random_obs)

    #Loop to calculate Euclidean distance of each point from the randomly selected centers
    for(i in 1:num_obs) {



    }
    dist(dat[i, ], method = "euclidean")

    return(centers)


}
#Randomly select K points for K clusters

#Measure distance between K1 and (K2 and K3) MSE, Euclidean Distance sqrt(x^2 + y^2)

#Measure distance between all points in data and all K points. Assign to cluster that has smallest MSE

#Assess quality of cluster by looking at variation in each cluster
