
#' Agglomerative Hierarchical Clustering Method`
#'
#' @param data A data frame
#' @param method A string that states which distance method the user wants to use
#'
#' @return A list with clustering path and
#'
#' @export
#'
hier_clust <- function(data, method = "euclidean") {

    avail_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

    if(!(method %in% avail_methods)) {
        stop("Please put your distance calculation method in all lower case")
    }

    n <- nrow(data)
    iterations <- 0 #will count the height of the dendrogram by adding 1 to each
    distances <- dist(data, method = method) #distance matrix

    clusters <- list() #the list from which the clusters will be stored and manipulated
    for(i in 1:n) {
        clusters[[i]] <- i
        print(clusters[[i]])
    }

    while(length(clusters) > 1) { #keep iterating until there is only one cluster left

        min_dist <- Inf
        merge_index <- NULL

        for(i in 1:length(clusters)) {
            for(j in 1:length(clusters)) {
                cluster1 <- clusters[[i]] #pulls the index of the cluster
                cluster2 <- clusters[[j]]

                print(clusters[[i]])
                print(cluster2[[j]])

                dist <- distances[cluster1, cluster2] #puts the indices of the clusters into the dist matrix

                if(dist < min_dist) { #Sets the smallest distance between two points by finding the smallest distance value
                    min_dist <- dist
                    merge_index <- c(i, j) #index in clusters
                }

            }

        }

        merged_cluster <- c(clusters[[merge_index[1]]], clusters[[merge_index[2]]]) #create new cluster that adds in the min dist cluster pairing
        clusters <- clusters[-merge_index] #deletes the indexes of both clusters in the pair
        clusters <- c(clusters, list(merged_cluster)) #puts the new cluster back into the clusters list

        #Average Linkage

        for(i in 1:length(clusters) - 1) { #-1 to account for the potential cluster value

            cluster_i <- clusters[[i]]

            avg_dist <- sum(distances[merge_index[1], cluster_i], distances[merge_index[2], cluster_i]) / 2
            distances[merge_index[1], cluster_i] <- avg_dist
            distances[cluster_i, merge_index[1]] <- avg_dist
        }

        distances[-merge_index[2], -merge_index[2]]

    }

    iterations <- iterations + 1

    return()

}
