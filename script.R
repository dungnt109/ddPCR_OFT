### configurations

# channels
args = commandArgs(trailingOnly=TRUE)

separator <- "/"

#
library(rmarkdown)

silence <- FALSE

runmode <- "interactive"

if (length(args) > 0 && args[1] == "Silence") {
	silence <- TRUE
	runmode <- "silence"
}

cat(paste(args[1], "\n"))
cat(paste(silence, "\n"))

### to get less sensitive outlier detection, either:
# 1. increase niqr
# 2. decrease quantile.range
# suggested default: 
niqr <- 3
quantile.range <- 0.02


show_outliers <- TRUE



marker.channel <- 1
gus.channel <- 2





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




##### load sample sheet
sample_sheet_file <-  file.choose(new = FALSE)



folder <- dirname(sample_sheet_file)

cat("Do you want to perform absolute or relative OFT?\n1. Absolute\n2. Relative\n")

answer <- readLines("stdin",n=1)

runType <- switch(
	answer, 
	"1" = "absolute",
	"2" = "relative", 
	"relative")

cat("\nVerified by?\n1. Prof Allen Yeoh\n2. Dr Lu Yi\n3. Blank\n")
answer <- readLines("stdin",n=1)


verifier <- switch(
	answer, 
	"1" = "Prof Allen Yeoh",
	"2" = "Dr Lu Yi", 
	"3" = "",
	answer)


cat("\nRun by?\n1. Amanda Lee\n2. Huan Pei Tee\n3. Nurhilya\n4. Dr Lu Yi\n5. Others\n6. Blank\n")
answer <- readLines("stdin",n=1)

if (answer == "5"){
	cat("\nPlease specify:\n")
	run_by <- readLines("stdin",n=1)
} else {
	run_by <- switch(
	answer, 
	"1" = "Amanda Lee", 
	"2" = "Huan Pei Tee", 
	"3" = "Nurhilya", 
	"4" = "Dr Lu Yi",
	"5" = "Others",
	"6" = "", 
	answer 
	)
}

cat("\nDate?\n1.",format(Sys.Date(), "%d/%m/%Y") ,"\n2. blank\n")
answer <- readLines("stdin",n=1)

run_date <- switch(
	answer, 
	"1" = format(Sys.Date(), "%d/%m/%Y"), 
	"2" = "", 
	answer 
	)

cat("\nReported by?\n1. Shirley Kham\n2. Huan Pei Tee\n3. Nurhilya\n4. Dr Lu Yi\n5. Others\n6. Blank\n")
answer <- readLines("stdin",n=1)

if (answer == "5"){
	cat("\nPlease specify:\n")
	reported_by <- readLines("stdin",n=1)
} else {
	reported_by <- switch(
		answer, 
		"1" = "Shirley Kham", 
		"2" = "Huan Pei Tee", 
		"3" = "Nurhilya", 
		"4" = "Dr Lu Yi", 
		"5" = "Others",
		"6" = "", 
		answer 
		)
}


is_manual_threshold <- FALSE
manual_threshold = 0

if (runmode == "interactive"){

	cat("\nUse manual threshold?\n1. No\n2. Yes\n")
	answer <- readLines("stdin",n=1)

	is_manual_threshold <- switch(
		answer, 
		"1" = FALSE, 
		"2" = TRUE, 
		FALSE
		)

	 

	if (is_manual_threshold){

		cat("\nPlease key in the OFT assay's intensities threshold!\n")

		manual_threshold = as.numeric(trimws(readLines("stdin",n=1)))

	}
}




cat(paste("Analyzing sample sheet", sample_sheet_file, "\n", sep=""))

cat("Please key in the OFT/GUSB baseline ratio in HL60, or press Enter to use the calculated value from the test.\n")

hl60_baseline <- trimws(readLines("stdin",n=1))
hl60_baseline <- as.numeric(hl60_baseline)

if (is.na(hl60_baseline)) {
	manual_hl60_baseline <- FALSE 
} else {
	manual_hl60_baseline <- TRUE
}


cat("Please key in the HL60 dilution factor or hit enter to use the default value (100x).\n")

hl60.gus.dilutionX <- trimws(readLines("stdin",n=1))
hl60.gus.dilutionX <- as.numeric(hl60.gus.dilutionX)



if (is.na(hl60.gus.dilutionX)) {
	hl60.gus.dilutionX <- 100
	cat("Using 100x.\n")
} else {
	cat(paste("Using user-specified value ", hl60.gus.dilutionX, "\n", sep=""))
}




#folder <- choose.dir()


sample_sheet <- read.table(sample_sheet_file, header=TRUE, row.names=1)




row.col.combinations <- expand.grid(rownames(sample_sheet), colnames(sample_sheet))




samples <- rep("", nrow(row.col.combinations))






for (i in 1:nrow(row.col.combinations)) {
	samples[i] <- as.character(sample_sheet[row.col.combinations[i, 1], row.col.combinations[i, 2]])
}

row.col.combinations[,2] <- sprintf("%02d", as.numeric(gsub("X", "", row.col.combinations[,2])))

names(samples) <- apply(row.col.combinations, 1, paste, collapse="")



#### get the files
files <- list.files(folder, pattern = "*.csv")



well.position <- sapply(lapply(strsplit(files , "_", fixed=TRUE), rev), "[[", 2)

names(files) <- well.position





#### get the control
## get GUS, HL60
hl60.gus <- samples[grepl("^HL60_GUS_", samples)]    ##MNC_ALB  => HL60_GUS 


hl60.gus.file <- paste(folder, files[names(hl60.gus)], sep=separator) 

hl60.gus.int <- read.csv(hl60.gus.file, header=TRUE)

hl60.gus.int <- hl60.gus.int[sample(nrow(hl60.gus.int)), ]

hl60.gus.clust <- clustering_and_threshold(hl60.gus.int[, gus.channel])





##if (!silence) {
##	plot(hl60.gus.int[,gus.channel],pch=16, cex=0.5, xlab="MNC, ALB", ylab="Amplitude")
##	lines(c(-1e6, 1e6), c(hl60.gus.clust$threshold, hl60.gus.clust$threshold), lty=2, col=2)
##}

# NTC, GUS
h2o.gus <- samples[grepl("^H2O_GUS_", samples)]                      ##H20_ALB => H2O_GUS
h2o.gus.file <-paste(folder, files[names(h2o.gus)], sep=separator) 
h2o.gus.int <- read.csv(h2o.gus.file, header=TRUE)
h2o.gus.int <- h2o.gus.int[sample(nrow(h2o.gus.int)), ]


### get QC list
qc.marker.samples <- samples[grepl("^QC_OFT_", samples)]                                #
qc.marker.files <- paste(folder, files[names(qc.marker.samples)], sep=separator)
qc.sample.sid <- sapply(strsplit(qc.marker.samples, "_", fixed=TRUE), "[[", 3)
qc.sample.pid <- sapply(qc.sample.sid, function(x) {
	reg <- regexpr("[A-Z]+[0-9]+", x)
	substring(x, reg[1], reg[1] + attr(reg, "match.length")[1] - 1)
})
qc.sample.mid <- sapply(strsplit(qc.marker.samples, "_", fixed=TRUE), "[[", 4)








### get Dx list
dx.marker.samples <- samples[grepl("^Dx_OFT_", samples)]                                #
dx.marker.files <- paste(folder, files[names(dx.marker.samples)], sep=separator)
dx.sample.sid <- sapply(strsplit(dx.marker.samples, "_", fixed=TRUE), "[[", 3)
dx.sample.pid <- sapply(dx.sample.sid, function(x) {
	reg <- regexpr("[A-Z]+[0-9]+", x)
	substring(x, reg[1], reg[1] + attr(reg, "match.length")[1] - 1)
})
dx.sample.mid <- sapply(strsplit(dx.marker.samples, "_", fixed=TRUE), "[[", 4)
dx.sample.ng <- as.numeric(gsub("ng", "", sapply(strsplit(dx.marker.samples, "_", fixed=TRUE), "[[", 5)))








### get fu list
fu.marker.samples <- samples[grepl("^FU_OFT_", samples)]
fu.marker.files <- paste(folder, files[names(fu.marker.samples)], sep=separator)
fu.sample.sid <- sapply(strsplit(fu.marker.samples, "_", fixed=TRUE), "[[", 3)
fu.sample.pid <- sapply(fu.sample.sid, function(x) {
	reg <- regexpr("[A-Z]+[0-9]+", x)
	substring(x, reg[1], reg[1] + attr(reg, "match.length")[1] - 1)
})
fu.sample.mid <- sapply(strsplit(fu.marker.samples, "_", fixed=TRUE), "[[", 4)
fu.sample.ng <- as.numeric(gsub("ng", "", sapply(strsplit(fu.marker.samples, "_", fixed=TRUE), "[[", 5)))







### get HL60 marker list
hl60.marker.samples <- samples[grepl("^HL60_OFT_", samples)]                            ##MNC_OFT => HL60_OFT 
hl60.marker.files <- paste(folder, files[names(hl60.marker.samples)], sep=separator)
hl60.sample.sid <- sapply(strsplit(hl60.marker.samples, "_", fixed=TRUE), "[[", 3)
hl60.sample.pid <- sapply(hl60.sample.sid, function(x) {
	reg <- regexpr("[A-Z]+[0-9]+", x)
	substring(x, reg[1], reg[1] + attr(reg, "match.length")[1] - 1)
})
hl60.sample.mid <- sapply(strsplit(hl60.marker.samples, "_", fixed=TRUE), "[[", 4)
hl60.sample.ng <- as.numeric(gsub("ng", "", sapply(strsplit(hl60.marker.samples, "_", fixed=TRUE), "[[", 5)))







### get H2O marker list
h2o.marker.samples <- samples[grepl("^H2O_OFT_", samples)]
h2o.marker.files <- paste(folder, files[names(h2o.marker.samples)], sep=separator)
h2o.sample.sid <- sapply(strsplit(h2o.marker.samples, "_", fixed=TRUE), "[[", 3)
h2o.sample.pid <- sapply(h2o.sample.sid, function(x) {
	reg <- regexpr("[A-Z]+[0-9]+", x)
	substring(x, reg[1], reg[1] + attr(reg, "match.length")[1] - 1)
})
h2o.sample.mid <- sapply(strsplit(h2o.marker.samples, "_", fixed=TRUE), "[[", 4)
h2o.sample.ng <- as.numeric(gsub("ng", "", sapply(strsplit(h2o.marker.samples, "_", fixed=TRUE), "[[", 5)))







### get GUS samples
gus.samples <- samples[grepl("GUS", samples)]                                 ## ALB => GUS 
gus.files <- paste(folder, files[names(gus.samples)], sep=separator)







gus.samples.h2o <- gus.samples[grepl("^H2O_GUS", gus.samples)]                 ## H2O_ALB => H2O_GUS 
gus.files <- gus.files[!grepl("^H2O_GUS", gus.samples)]
gus.samples <- gus.samples[!grepl("^H2O_GUS", gus.samples)]
gus.samples.split <- strsplit(gus.samples, "_", fixed=TRUE)
gus.samples.info <- sapply(gus.samples.split, function(x) {x[1:3]})


gus.h2o.samples <- samples[grepl("^H2O_GUS", samples)]
gus.h2o.files <- paste(folder, files[names(gus.h2o.samples)], sep=separator)






cat("Analyzing gus wells...\n")


gus.results.individual <- lapply(1:length(gus.samples), function(i) {
	ff <- gus.files[i]


	gus.int <- read.csv(ff, header=TRUE)
	gus.int <- gus.int[sample(nrow(gus.int)), ]

	
	outlier.in.silence.channel <- mark.outlier(gus.int[, marker.channel], niqr)
	gus.clust <- clustering_and_threshold(gus.int[outlier.in.silence.channel$mask, gus.channel])
	
	mask <- outlier.in.silence.channel$mask
	mask[mask] <- gus.clust$mask
	
	if (!silence) {
		windows()
		plot(gus.int[, marker.channel], gus.int[, gus.channel], col = (gus.int[, gus.channel] > gus.clust$threshold) + 1,
			pch=c(4, 16)[mask + 1],
		main=paste(gus.samples[i], names(gus.samples)[i], "\n Click to remove, and click the middle key when done." ), xlab = "Channel 1", ylab="Channel 2")
		
		lines(c(-1e8, 1e8), c(gus.clust$threshold, gus.clust$threshold), col=2, lwd=1.5)
		
		to.inverse <- identify(gus.int[,1], gus.int[,2], c("✓", "x")[mask + 1], col="blue")
		
		mask[to.inverse] <- !mask[to.inverse]
	}
	
	gus.clust2 <- clustering_and_threshold(gus.int[mask, gus.channel])
	#plot(gus.int[,marker.channel], gus.int[,gus.channel], col = (gus.int[,gus.channel] > gus.clust2$threshold) + 1,
	#		pch = c(4, 16)[mask + 1],
	#		main=paste(gus.samples[i], names(gus.samples)[i], "\n Final" ), xlab = "Channel 1", ylab="Channel 2")
	#
	#lines(c(-1e8, 1e8), c(gus.clust2$threshold, gus.clust2$threshold), col=2, lwd=1.5)
	#Sys.sleep(5)
	n.positive.droplets <- sum(gus.int[,gus.channel][mask] > gus.clust2$threshold)
	n.droplets <- sum(mask)
	n.outliers <- sum(!mask)
	concentration <- -log(1-(n.positive.droplets/n.droplets))/0.00085
	list(intensities=gus.int, threshold=gus.clust2$threshold, mask=mask, n.positive.droplets=n.positive.droplets, n.droplets = n.droplets, concentration=concentration, name=gus.samples[i], upper = gus.clust2$upper, n.outliers = n.outliers)
})






gus.h2o.results.individual <- lapply(1:length(gus.h2o.samples), function(i) {
	ff <- gus.h2o.files[i]


	gus.h2o.int <- read.csv(ff, header=TRUE)
	gus.h2o.int <- gus.h2o.int[sample(nrow(gus.h2o.int)), ]

	
	gus.hl60 <- gus.results.individual[grepl("^HL60_GUS", gus.samples)]   ##MNC_ALB => HL60_GUS
	gus.hl60[[1]]$threshold
	
	gus.min <- min(c(gus.hl60[[1]]$intensities[, gus.channel], gus.h2o.int[, gus.channel]))
	gus.max <- max(c(gus.hl60[[1]]$intensities[, gus.channel], gus.h2o.int[, gus.channel]))
	
	
	outlier.in.silence.channel <- mark.outlier(gus.h2o.int[, marker.channel], niqr)
	mask <- outlier.in.silence.channel$mask & gus.h2o.int[, gus.channel] < gus.hl60[[1]]$upper
	
	if (!silence) {
		plot(gus.h2o.int[, marker.channel], gus.h2o.int[, gus.channel], col = (gus.h2o.int[, gus.channel] > gus.hl60[[1]]$threshold) + 1,
			pch=c(4, 16)[mask + 1], ylim = c(gus.min, gus.max),
		main=paste(gus.h2o.samples[i], names(gus.h2o.samples)[i], "\n Click to remove, and click the middle key when done." ), xlab = "Channel 1", ylab="Channel 2")
		
		lines(c(-1e8, 1e8), c(gus.hl60[[1]]$threshold, gus.hl60[[1]]$threshold), col=2, lwd=1.5)
		
		to.inverse <- identify(gus.h2o.int[,1], gus.h2o.int[,2], c("✓", "x")[mask + 1], col="blue")
		
		mask[to.inverse] <- !mask[to.inverse]
	}
	
	n.positive.droplets <- sum(gus.h2o.int[,gus.channel][mask] > gus.hl60[[1]]$threshold)
	n.droplets <- sum(mask)
	n.outliers <- sum(!mask)
	concentration <- -log(1-(n.positive.droplets/n.droplets))/0.00085
	list(intensities=gus.h2o.int, threshold=gus.hl60[[1]]$threshold, mask=mask, n.positive.droplets=n.positive.droplets, n.droplets = n.droplets, concentration=concentration, name=gus.h2o.samples[i], n.outliers = n.outliers)
})



cat("Analyzing OFT wells...\n")

dx.sample.clust <- lapply(1:length(dx.marker.samples), function(i) {
	ff <- dx.marker.files[i]



	
	pid <- dx.sample.pid[i]


	dx.sid <- dx.sample.sid[i]

	
	mid <- dx.sample.mid[i]

	
	dx.marker.int <- read.csv(ff, header=TRUE)
	dx.marker.int <- dx.marker.int[sample(nrow(dx.marker.int)), ]
	
	#if (!silence) {
	#	winDialog(type = c("ok"), paste("Start processing", pid))
	#}
	### identify outlier in the other channel
	outlier.in.silence.channel <- mark.outlier(dx.marker.int[, gus.channel], niqr)
	


	if (!is_manual_threshold) {

		dx.marker.clust <- clustering_and_threshold(dx.marker.int[outlier.in.silence.channel$mask, marker.channel])

	} else {

		dx.marker.clust <- manual_clustering_and_threshold(dx.marker.int[outlier.in.silence.channel$mask, marker.channel], manual_threshold)
	}


	
	
	
	dx.mask <- outlier.in.silence.channel$mask



	dx.mask[dx.mask] <- dx.marker.clust$mask
	
	if (!silence) {
		plot(dx.marker.int[,marker.channel], dx.marker.int[,gus.channel], col = (dx.marker.int[,marker.channel] > dx.marker.clust$threshold) + 1,
				pch = c(4, 16)[dx.mask + 1],
				main=paste(dx.marker.samples[i], names(dx.marker.samples)[i], "\n Click to remove, and click the middle key when done." ), xlab = "Channel 1", ylab="Channel 2")
		
		lines(c(dx.marker.clust$threshold, dx.marker.clust$threshold), c(-1e8, 1e8), col=2, lwd=1.5)
		
		to.inverse <- identify(dx.marker.int[,1], dx.marker.int[,2], c("✓", "x")[dx.mask + 1], col="blue")
		
		
		
		
		dx.mask[to.inverse] <- !dx.mask[to.inverse]
	}
	
	if (!is_manual_threshold) {

		dx.marker.clust2 <- clustering_and_threshold(dx.marker.int[dx.mask, marker.channel])

	} else {

		dx.marker.clust2 <- manual_clustering_and_threshold(dx.marker.int[dx.mask, marker.channel], manual_threshold)
	}
	
	
	#plot(dx.marker.int[,marker.channel], dx.marker.int[,gus.channel], col = (dx.marker.int[,marker.channel] > dx.marker.clust2$threshold) + 1,
	#		pch = c(4, 16)[dx.mask + 1],
	#		main=paste(dx.marker.samples[i], names(dx.marker.samples)[i], "\n Final" ), xlab = "Channel 1", ylab="Channel 2")
	#
	#lines(c(dx.marker.clust2$threshold, dx.marker.clust2$threshold), c(-1e8, 1e8), col=2, lwd=1.5)
	#
	
	n.positive.droplets <- sum(dx.marker.int[,marker.channel][dx.mask] > dx.marker.clust2$threshold)


	n.droplets <- sum(dx.mask)


	n.outliers <- sum(!dx.mask)


	concentration <- -log(1-(n.positive.droplets/n.droplets))/0.00085

	
	dx.results <- list(list(intensities=dx.marker.int, 
						threshold=dx.marker.clust2$threshold, 
						mask=dx.mask, 
						n.positive.droplets=n.positive.droplets, 
						n.droplets = n.droplets, 
						concentration=concentration, 
						name=dx.marker.samples[i], 
						n.outliers = n.outliers))

	##QC


	qc.samples <- qc.marker.samples[qc.sample.pid == pid & qc.sample.mid == mid]

	qc.files <- qc.marker.files[qc.sample.pid == pid & qc.sample.mid == mid]


	qc.results <- lapply(1:length(qc.samples), function(j) {
		qc.file <- paste(folder, files[names(qc.samples)[j]], sep=separator)
		qc.marker.int <- read.csv(qc.file, header=TRUE)
		qc.marker.int <- qc.marker.int[sample(nrow(qc.marker.int)), ]


		qc.mask <- (qc.marker.int[, gus.channel] < outlier.in.silence.channel$upper.bound) & (qc.marker.int[, gus.channel] > outlier.in.silence.channel$lower.bound) & (qc.marker.int[, marker.channel] < dx.marker.clust2$upper.bound)
		
		
		if (!silence) {
		
			## get plot range
			x.min <- min(c(qc.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
			x.max <- max(c(qc.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
			y.min <- min(c(qc.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
			y.max <- max(c(qc.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
			
			
			plot(qc.marker.int[,marker.channel], qc.marker.int[,gus.channel], 
				    col = (qc.marker.int[,marker.channel] > dx.marker.clust2$threshold) + 1,
					pch = c(4, 16)[qc.mask + 1],
					main=paste(qc.samples[j], names(qc.samples)[j], 
						"\n Click to remove, and click the middle key when done." ), 
					xlab = "Channel 1", ylab="Channel 2", xlim=c(x.min, x.max), ylim=c(y.min, y.max))
			lines(c(dx.marker.clust2$threshold, dx.marker.clust2$threshold), c(-1e8, 1e8), col=2, lwd=1.5)
			qc.to.inverse <- identify(qc.marker.int[,1], qc.marker.int[,2], c("✓", "x")[qc.mask + 1], col="blue")
			qc.mask[qc.to.inverse] <- !qc.mask[qc.to.inverse]
		}
		n.positive.droplets <- sum(qc.marker.int[,marker.channel][qc.mask] > dx.marker.clust2$threshold)
		n.droplets <- sum(qc.mask)
		n.outliers <- sum(!qc.mask)
		concentration <- -log(1-(n.positive.droplets/n.droplets))/0.00085

	
	
		list(intensities=qc.marker.int, 
						threshold=dx.marker.clust2$threshold, 
						mask=qc.mask, 
						n.positive.droplets=n.positive.droplets, 
						n.droplets = n.droplets, 
						concentration=concentration, 
						name=qc.samples[j],   
						n.outliers = n.outliers)
	})


	qc.concentration <- qc.results[[1]]$concentration




	## HL60 marker files
	hl60.samples <- hl60.marker.samples[hl60.sample.pid == pid & hl60.sample.mid == mid]

	hl60.files <- hl60.marker.files[hl60.sample.pid == pid & hl60.sample.mid == mid]


	
	hl60.results <- lapply(1:length(hl60.samples), function(j) {
		hl60.file <- paste(folder, files[names(hl60.samples)[j]], sep=separator)
		hl60.marker.int <- read.csv(hl60.file, header=TRUE)
		hl60.marker.int <- hl60.marker.int[sample(nrow(hl60.marker.int)), ]


		hl60.mask <- (hl60.marker.int[, gus.channel] < outlier.in.silence.channel$upper.bound) & (hl60.marker.int[, gus.channel] > outlier.in.silence.channel$lower.bound) & (hl60.marker.int[, marker.channel] < dx.marker.clust2$upper.bound)
		
		
		if (!silence) {
		
			## get plot range
			x.min <- min(c(hl60.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
			x.max <- max(c(hl60.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
			y.min <- min(c(hl60.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
			y.max <- max(c(hl60.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
			
			
			plot(hl60.marker.int[,marker.channel], hl60.marker.int[,gus.channel], col = (hl60.marker.int[,marker.channel] > dx.marker.clust2$threshold) + 1,
					pch = c(4, 16)[hl60.mask + 1],
					main=paste(hl60.samples[j], names(hl60.samples)[j], "\n Click to remove, and click the middle key when done." ), xlab = "Channel 1", ylab="Channel 2", xlim=c(x.min, x.max), ylim=c(y.min, y.max))
			lines(c(dx.marker.clust2$threshold, dx.marker.clust2$threshold), c(-1e8, 1e8), col=2, lwd=1.5)
			hl60.to.inverse <- identify(hl60.marker.int[,1], hl60.marker.int[,2], c("✓", "x")[hl60.mask + 1], col="blue")
			hl60.mask[hl60.to.inverse] <- !hl60.mask[hl60.to.inverse]
		}
		n.positive.droplets <- sum(hl60.marker.int[,marker.channel][hl60.mask] > dx.marker.clust2$threshold)
		n.droplets <- sum(hl60.mask)
		n.outliers <- sum(!hl60.mask)
		concentration <- -log(1-(n.positive.droplets/n.droplets))/0.00085
	
		list(intensities=hl60.marker.int, 
						threshold=dx.marker.clust2$threshold, 
						mask=hl60.mask, 
						n.positive.droplets=n.positive.droplets, 
						n.droplets = n.droplets, 
						concentration=concentration, 
						name=hl60.samples[j],   #### updated from name=hl60.marker.samples[j],
						n.outliers = n.outliers)
	})
	
	

	h2o.samples <- h2o.marker.samples[h2o.sample.pid == pid & h2o.sample.mid == mid]


	h2o.files <- h2o.marker.files[h2o.sample.pid == pid & h2o.sample.mid == mid]


	
	h2o.results <- lapply(1:length(h2o.samples), function(j) {
		h2o.file <- paste(folder, files[names(h2o.samples)[j]], sep=separator)
		h2o.marker.int <- read.csv(h2o.file, header=TRUE)
		h2o.marker.int <- h2o.marker.int[sample(nrow(h2o.marker.int)), ]

		
		
		h2o.mask <- (h2o.marker.int[, gus.channel] < outlier.in.silence.channel$upper.bound) & (h2o.marker.int[, gus.channel] > outlier.in.silence.channel$lower.bound) & (h2o.marker.int[, marker.channel] < dx.marker.clust2$upper.bound)
		
		if (!silence) {
			## get plot range
			x.min <- min(c(h2o.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
			x.max <- max(c(h2o.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
			y.min <- min(c(h2o.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
			y.max <- max(c(h2o.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
			
			
			plot(h2o.marker.int[,marker.channel], h2o.marker.int[,gus.channel], col = (h2o.marker.int[,marker.channel] > dx.marker.clust2$threshold) + 1,
					pch = c(4, 16)[h2o.mask + 1],
					main=paste(h2o.samples[j], names(h2o.samples)[j], "\n Click to remove, and click the middle key when done." ), xlab = "Channel 1", ylab="Channel 2",
					xlim=c(x.min, x.max), ylim=c(y.min, y.max))
			lines(c(dx.marker.clust2$threshold, dx.marker.clust2$threshold), c(-1e8, 1e8), col=2, lwd=1.5)
			h2o.to.inverse <- identify(h2o.marker.int[,1], h2o.marker.int[,2], c("✓", "x")[h2o.mask + 1], col="blue")
			h2o.mask[h2o.to.inverse] <- !h2o.mask[h2o.to.inverse]
			#pos.droplets <- sum(h2o.marker.int[h2o.mask, marker.channel] > dx.marker.clust2$threshold)
			#n.droplets <- sum(h2o.mask)
			#list(int = h2o.marker.int, h2o.mask = h2o.mask, pos.droplets = pos.droplets)
		}
		
		n.positive.droplets <- sum(h2o.marker.int[,marker.channel][h2o.mask] > dx.marker.clust2$threshold)
		n.droplets <- sum(h2o.mask)
		n.outliers <- sum(!h2o.mask)
		concentration <- -log(1-(n.positive.droplets/n.droplets))/0.00085
	
		list(intensities=h2o.marker.int, 
						threshold=dx.marker.clust2$threshold, 
						mask=h2o.mask, 
						n.positive.droplets=n.positive.droplets, 
						n.droplets = n.droplets, 
						concentration=concentration, 
						name=h2o.samples[j],   ########### updated from name=h2o.marker.samples[j],
						n.outliers = n.outliers)
						
		
	})
	
	
	
	### search for follow up channels
	
	
	
	fu.sids <- unique(fu.sample.sid[fu.sample.pid == pid & fu.sample.mid == mid])











	
	
	for (fu.sid in fu.sids) {

	
		#dx.baseline <- readline(prompt = paste("Processing ", fu.sid, "_", mid, ". Please key in the Dx Baseline value or hit enter to use in-plate Dx baseline.\n", sep=""))
		#dx.baseline <- as.numeric(dx.baseline)


		if (runType == "relative") {
		
				cat(paste("Processing ", fu.sid, "_", mid, ". Please key in the OFT/GUSB baseline ratio in Dx or hit enter to use the value calculated from the current test.\n", sep=""));
				dx.baseline <- trimws(readLines("stdin",n=1))
				dx.baseline <- as.numeric(dx.baseline)
				if(is.na(dx.baseline)) {
					cat("Using calculated Dx baseline ratio.\n")
				} else {
					cat("Using preset Dx baseline ratio.\n")
				
				}
		} else {

			dx.baseline <- 0 
		}
		
		fu.samples <- fu.marker.samples[fu.sample.pid == pid & fu.sample.mid == mid & fu.sample.sid == fu.sid]
		fu.files <- fu.marker.files[fu.sample.pid == pid & fu.sample.mid == mid & fu.sample.sid == fu.sid]
		fu.results <- lapply (1:length(fu.samples), function(j) {
			fu.file <- paste(folder, files[names(fu.samples)[j]], sep=separator)
			fu.marker.int <- read.csv(fu.file, header=TRUE)
			fu.marker.int <- fu.marker.int[sample(nrow(fu.marker.int)), ]

			fu.mask <- (fu.marker.int[, gus.channel] < outlier.in.silence.channel$upper.bound) & (fu.marker.int[, gus.channel] > outlier.in.silence.channel$lower.bound) & (fu.marker.int[, marker.channel] < dx.marker.clust2$upper.bound)
			
			if (!silence) {
				## get plot range
				x.min <- min(c(fu.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
				x.max <- max(c(fu.marker.int[, marker.channel], dx.marker.int[, marker.channel]))
				y.min <- min(c(fu.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
				y.max <- max(c(fu.marker.int[, gus.channel], dx.marker.int[, gus.channel]))
			
			
				
				plot(fu.marker.int[,marker.channel], fu.marker.int[,gus.channel], col = (fu.marker.int[,marker.channel] > dx.marker.clust2$threshold) + 1,
						pch = c(4, 16)[fu.mask + 1],
						main=paste(fu.samples[j], names(fu.samples)[j], "\n Click to remove, and click the middle key when done." ), xlab = "Channel 1", ylab="Channel 2")
				lines(c(dx.marker.clust2$threshold, dx.marker.clust2$threshold), c(-1e8, 1e8), col=2, lwd=1.5)
				fu.to.inverse <- identify(fu.marker.int[,1], fu.marker.int[,2], c("✓", "x")[fu.mask + 1], col="blue")
				fu.mask[fu.to.inverse] <- !fu.mask[fu.to.inverse]
			}
			
			pos.droplets <- sum(fu.marker.int[fu.mask, marker.channel] > dx.marker.clust2$threshold)
			
			list(int = fu.marker.int, fu.mask = fu.mask, pos.droplets = pos.droplets)
			
			
			n.positive.droplets <- sum(fu.marker.int[,marker.channel][fu.mask] > dx.marker.clust2$threshold)
			n.droplets <- sum(fu.mask)
			n.outliers <- sum(!fu.mask)
			concentration <- -log(1-(n.positive.droplets/n.droplets))/0.00085
		
			list(intensities=fu.marker.int, 
							threshold=dx.marker.clust2$threshold, 
							mask=fu.mask, 
							n.positive.droplets=n.positive.droplets, 
							n.droplets = n.droplets, 
							concentration=concentration, 
							name=fu.samples[j],           ##### updated from name=fu.marker.samples[j], 
							n.outliers = n.outliers)
				
		})
		
		
		dx.gus <- gus.results.individual[gus.samples.info[1,] == "Dx" &  gus.samples.info[3,] == dx.sid]
		fu.gus <- gus.results.individual[gus.samples.info[1,] == "FU" &  gus.samples.info[3,] == fu.sid]
		hl60.gus <- gus.results.individual[gus.samples.info[1,] == "HL60"]    ##MNC => HL60
		h2o.gus <- gus.h2o.results.individual
		
		dx.marker <- dx.results
		fu.marker <- fu.results
		hl60.marker <- hl60.results
		h2o.marker <- h2o.results
		
		## draw figure 
		render("./ddPCR_rTemplate_report.Rmd", params = list(
			sid = fu.sid, 
			mid = mid, 
			dx.gus = dx.gus, 
			fu.gus = fu.gus, 
			hl60.gus = hl60.gus, 
			h2o.gus = h2o.gus, 
			dx.marker = dx.marker, 
			fu.marker=fu.marker, 
			hl60.marker=hl60.marker, 
			h2o.marker=h2o.marker, 
			show_outliers=show_outliers, 
			dx.baseline=dx.baseline, 
			hl60.gus.dilutionX=hl60.gus.dilutionX, 
			hl60_baseline=hl60_baseline,
			manual_hl60_baseline=manual_hl60_baseline,
			run_by=run_by, 
			run_date=run_date, 
			reported_by=reported_by, 
			verifier=verifier, 
			runType=runType,
			is_manual_threshold=is_manual_threshold, 
			manual_threshold=manual_threshold,
			qc.concentration=qc.concentration,
			date=Sys.time()), 
			output_file = paste(folder, separator, fu.sid, "_", mid, "_", runmode, "_report.pdf", sep="")
		)
		## draw figure 
		render("./ddPCR_rTemplate_2dFigs.Rmd", params = list(
			sid = fu.sid, 
			mid = mid, 
			dx.gus = dx.gus, 
			fu.gus = fu.gus, 
			hl60.gus = hl60.gus, 
			h2o.gus = h2o.gus, 
			dx.marker = dx.marker, 
			fu.marker=fu.marker, 
			hl60.marker=hl60.marker, 
			h2o.marker=h2o.marker, 
			show_outliers=show_outliers, 
			dx.baseline=dx.baseline, 
			hl60.gus.dilutionX=hl60.gus.dilutionX, 
			date=Sys.time()), 
			output_file = paste(folder, separator, fu.sid, "_", mid, "_", runmode, "_2dFigs.pdf", sep="")
		)
	
	}
	
	#msk <- rep(FALSE, nrow(dx.marker.int))
	
	#msk[to.exclude] <- TRUE
	#dx.marker.clust <- clustering_and_threshold(dx.marker.int[!msk, marker.channel])
	#list(dx.marker.int, dx.marker.clust, to.exclude)
	
})







