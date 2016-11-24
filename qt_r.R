setwd("/Users/ryanquigley/Documents/SJSU/264/project")

############# NOTES #############
# Consider adding points to KDE plot
# 	for QT movies outside of the dataset
#	ex: Sin City


#################################



## Death count data (Quentin Taratino movies removed)
dc <- read.csv("filmdeathcounts_no-qt.csv", 
	header = TRUE, 
	stringsAsFactors = FALSE)
dc$Kill_Rate <- dc$Body_Count/dc$Length_Minutes * 60

## Project dataset meta data
qt <- read.table("qt_data.txt", 
                 header = FALSE, 
                 col.names = c("movie", "body.count", "rating", "hours", "year", "genre"),
                 sep = ";",
                 strip.white = TRUE,
                 stringsAsFactors = FALSE)

## Genre list for project dataset
qt.g <- unique(unlist(strsplit(paste(qt$genre, collapse = ", ", sep = ""), ", ")))

## Splitting genre list for each movie in death count dataset
dc.g <- strsplit(tolower(dc$Genre), "|", fixed = TRUE)

## Checking if any genres match those of project genre list
dc$Genre_Match <- logical(1)
for (i in seq_along(dc.g)) {
	dc$Genre_Match[i] <- any(dc.g[[i]] %in% qt.g)
}

## Filter death count dataset
## 1) Rating R or Not Rates
## 2) Year later than 1991 (earliest is 1992)
## 3) Genre Match
dc.filt <- dc[dc$MPAA_Rating %in% c("R", "Unrated", "NR") & dc$Year > 1991 & dc$Genre_Match == TRUE, ]
rownames(dc.filt) <- 1:nrow(dc.filt)

## Death count summary
summary(dc.filt$Body_Count)


#### Death count histogram
h <- hist(dc.filt$Body_Count, right = FALSE, ann = FALSE, axes = FALSE, xlim = c(0,650), ylim = c(0,100), breaks = seq.int(0,650,25), col = "royalblue", border = "aquamarine")
axis(1, at = seq.int(0,650,25), labels = NA)
axis(1, at = seq.int(0,650,50), lwd = 0, lwd.tick = 0)
axis(2, at = seq.int(0,100,20))

#### Death county kernel density estimate
h <- 20
d <- density(dc.filt$Body_Count, kernel = "epanechnikov", bw = h)
plot(d, ann = FALSE, axes = FALSE,
	xlim = c(-100, 700), ylim = c(0, 0.02), 
	bty = "n", col = "violetred", lwd = 1.5)
axis(1, at = seq.int(-50, 700, 50), labels = NA)
axis(1, at = seq.int(0, 600, 100), lwd = 0, lwd.ticks = 0)
axis(2, at = seq.int(0, 0.02, 0.005), pos = -75)

## Gamma parameter controls
k <- 1.50
theta <- 35
g.mean <- k*theta
g.mode <- (k-1)*theta
g.var <- k*theta^2

## Plot theoretical gamma distribution
curve(dgamma(x, shape = k, scale = theta), from = 0, to = 400, add = TRUE, lty = 2, col = "slateblue")
legend("topright", legend = paste(c("k (shape) =", "theta (scale) =", "Mean =", "Mode =", "Variance ="), c(k, theta, g.mean, g.mode, g.var)), bty = "n")


