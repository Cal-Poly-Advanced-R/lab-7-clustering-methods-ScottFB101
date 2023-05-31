#' Implements K means by hands
#'
#' @param dat A data set
#' @param k The number of desired clusters
#' @param pca Boolean value indicating whether or not to run Principal Component Analysis prior to K means
#'
#' @return
#'
#' @import dplyr
#' @import stats
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
    #Create centers for reference
    centers <- slice(dat, random_obs)

    #Vectors of clusters
    clusters <- c(1)
    last_clusters <- c(0)
    smallest_distances <- c(0)
    dist <- c(0)
    iterations <- 0

    #Center point of whole data set for Total Sum of Squares calculation
    center_of_dat <- colMeans(dat)

    #Repeat until we end up on the same cluster
    repeat {

        #Loop to calculate Euclidean distance of each point from the randomly selected centers
        for(i in 1:num_obs) {

            #Attaching observation to data frame of the randomly chosen k-clusters
            center_and_point <- dat[i, ] %>% rbind(centers)

            #Measuring Euclidean distance between observation and k-clusters
            dist <- dist(center_and_point, method = "euclidean", upper = FALSE)

            #Taking smallest distance out of the measurements for each k-clusters
            closest_cluster <- dist[1:k] %>%
                which.min()

            #Pulling smallest distance for sum of squares
            smallest_distances[i] <- dist[closest_cluster]

            #Assigning cluster number to observation
            clusters[i] <- closest_cluster

        }

        #Checking to make sure the cluster arraignment updated
        if(all(last_clusters == clusters)) {
            break
        }

        #Setting last cluster value
        last_clusters <- clusters

        #Finding new centers by taking mean of data points in each k-cluster, then removing cluster column
        centers <- dat %>%
            cbind(clusters) %>%
            group_by(clusters) %>%
            summarize_all(mean) %>%
            select(-clusters)

        #Counting number of iterations to get repeated k-means clustering
        iterations <- iterations + 1

    }

    #Attaching cluster assignment to original data frame
    cluster_assignments <- dat %>%
        cbind(clusters) %>%
        cbind(smallest_distances)

    #Total Sum of Squares
    TSS <- total_sum_squares(center_of_dat, dat)

    #Output cluster assignments, total sum of squares, at the minimum
    return(c(cluster_assignments, iterations))

}


#' Retrieves Total Sum of Squares
#'
#' @param center The center point of the data that all Euclidean distances will be calculated on
#' @param dat A data set

total_sum_squares <- function (center, dat) {



}
