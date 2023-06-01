
#' Agglomerative Hierarchical Clustering Method`
#'
#' @import
#'
#' @param
#'
#' @return
#'
#' @export
#'
hier_clust <- function(data, method = "euclidean") {

    avail_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

    if(!(method %in% avail_methods)) {
    #STOP AND WARNING HERE
    }

    iterations <- 0 #will count the height of the dendrogram by adding 1 to each

    distances <- dist(data, method = method) #distance matrix

    #while loop that checks the # of clusters isn't 2
        #each loop checks for which two rows are most similar - includes cluster means
            #can change later to not only compare based on means but also closest/furthest point in the cluster
        #check for if either

    clusters <- list() #the list from which the clusters will be stored and manipulated

    for(val in 1:n) {
        clusters[[i]] <- i
    }


    while(length(clusters) > 1) {

        min_dist <- Inf
        merge_index <- NULL

        for(i in 1:length(clusters)) {
            for(j in (i + 1:length(clusters))) {
                cluster1 <- clusters[[i]] #pulls the index of the cluster
                cluster2 <- clusters[[j]]

                dist <- distances[cluster1, cluster2] #puts the indices of the clusters into the dist matrix

                if(dist < min_dist) {
                    min_dist <- dist
                    merge_index <- c(i, j)
                }

            }

        }

        merged_cluster <- c(clusters[[merge_index[1]]], clusters[[merge_index[2]]])
        clusters <- clusters[-merge_index]
        clusters <- c(clusters, list(merged_cluster))

        #Complete Linkage

        #Single Linkage

        #Average Linkage

        ######RESOURCES SAY TO DO distances <- distances[-merge_index[2], -merge_index[2]] HERE ######

        for(i in length(clusters)) {

            cluster_i <- clusters[[i]]
            avg_dist <- sum(distances[merge_index[1], cluster_i], distances[merge_index[2], cluster_i]) / 2
            distances[merge_index[1], cluster[i]] <- avg_dist

        }

        distances[-merge_index[2], merge_index[2]]

        #######BUT I FEEL LIKE ITS ONLY RIGHT TO MAKE THAT ROW/COL REMOVAL AFTER I'VE WORKED WITH THE VALUE IN THE FOR LOOP ABOVE

    }

    iterations <- iterations + 1

    return()

}
