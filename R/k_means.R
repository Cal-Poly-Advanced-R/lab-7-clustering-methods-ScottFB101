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

    #Taking only numeric data
    dat <- dat %>%
        select(. ,where(is.numeric))

    #Number of rows/observations
    num_obs <- nrow(dat)

    #Randomly select K observations
    random_obs <- sample(num_obs, size = k)
    centers <- slice(dat, random_obs)

    #Vectors of clusters
    clusters <- c()
    last_cluster <- c()
    dist <- c()

    #Loop to calculate Euclidean distance of each point from the randomly selected centers
    for(i in 1:num_obs) {

        center_and_point <- dat[i, ] %>% rbind(centers)

        dist <- dist(center_and_point, method = "euclidean", upper = FALSE)

        closest_cluster <- dist[1:k] %>%
            which.min()

        clusters[i] <- closest_cluster

    }

    #Check if cluster assignment has changed
    stopifnot(clusters != last_cluster)

    #Assigning new cluster assignments to variable so we can perform the check above
    last_cluster <- clusters

    #Attaching cluster assignment to original data frame
    cluster_assignments <- dat %>%
        cbind(clusters)

    #Output cluster assignments, total sum of squares, at the minimum
    return(cluster_assignments)


}
#Randomly select K points for K clusters

#Measure distance between K1 and (K2 and K3) MSE, Euclidean Distance sqrt(x^2 + y^2)

#Measure distance between all points in data and all K points. Assign to cluster that has smallest MSE

#Assess quality of cluster by looking at variation in each cluster
