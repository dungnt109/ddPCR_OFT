mark.outlier <- function(intensities, range = 10) {

	#quantiles <- quantile(intensities)
	lower.quantile.value <- quantile(intensities, quantile.range)
	higher.quantile.value <- quantile(intensities, 1- quantile.range)
	iqr <- higher.quantile.value - lower.quantile.value
	upper.bound <- higher.quantile.value + range * iqr
	lower.bound <- higher.quantile.value - range * iqr
	mask <- (intensities < upper.bound) & (intensities > lower.bound)
	return(list(upper.bound = upper.bound, lower.bound = lower.bound, mask = mask))
}

## using k-means to divide positive control into 2 clusters and determine the threshold
## using c1 to represent for the positive cluster and c2 for the negative one
clustering_and_threshold <- function(intensities) {
	
	
	## initialize from max and min
	km <- kmeans(as.matrix(intensities), c(max(intensities), min(intensities)), nstart=1000)
	c1 <- intensities[km$clust==1]
	c2 <- intensities[km$clust==2]
	c1.median <- median(c1)
	c2.median <- median(c2)	
	if (c1.median < c2.median) {
		c1 <- intensities[km$clust==2]
		c2 <- intensities[km$clust==1]
		c1.median <- median(c1)
		c2.median <- median(c2)
	}
	
	solution <- (min(c1) + max(c2)) / 2
	

	## identify outlier in high cluster
	intensities.highcluster <- intensities[intensities > solution]
	upper.bound <- mark.outlier(intensities.highcluster, niqr)$upper.bound
	
	return(list(c1.median=c1.median, c2.median=c2.median, threshold=solution, upper.bound = upper.bound, mask = intensities < upper.bound))
}

manual_clustering_and_threshold <- function(intensities, threshold) {

	## initialize from max and min
	km <- kmeans(as.matrix(intensities), c(max(intensities), min(intensities)), nstart=1000)
	c1 <- intensities[intensities >= threshold]
	c2 <- intensities[intensities < threshold]
	c1.median <- median(c1)
	c2.median <- median(c2)
	solution <- threshold
	## identify outlier in high cluster
	intensities.highcluster <- intensities[intensities > solution]
	upper.bound <- mark.outlier(intensities.highcluster, niqr)$upper.bound
	return(list(c1.median=c1.median, c2.median=c2.median, threshold=solution, upper.bound = upper.bound, mask = intensities < upper.bound))

}