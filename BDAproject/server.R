library(shiny)

############# NOTES #############
# Consider adding points to KDE plot
# 	for QT movies outside of the dataset
#	ex: Sin City


#################################

## Death count data (Quentin Taratino movies removed)
dc <- read.csv("data/filmdeathcounts_no-qt.csv", 
               header = TRUE, 
               stringsAsFactors = FALSE)

## Project dataset meta data
qt <- read.table("data/qt_meta.txt", 
                 header = FALSE, 
                 col.names = c("movie", "rating", "duration", "year", "genre"),
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


server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    d <- density(dc.filt$Body_Count, kernel = "epanechnikov", bw = input$h, from = 0)
    plot(d, ann = FALSE, axes = FALSE,
         xlim = c(-100, 700), ylim = c(0, 0.02), 
         bty = "n", col = "violetred", lwd = 1.5)
    axis(1, at = seq.int(-50, 700, 50), labels = NA)
    axis(1, at = seq.int(0, 600, 100), lwd = 0, lwd.ticks = 0)
    axis(2, at = seq.int(0, 0.02, 0.005), pos = -75)
    
    ## Gamma parameter controls
    g.mean <- input$a*input$b
    g.mode <- (input$a-1)*input$b
    g.var <- input$a*(input$b)^2
    
    ## Plot theoretical gamma distribution
    curve(dgamma(x, shape = input$a, scale = input$b), from = 0, to = 400, add = TRUE, lty = 2, col = "slateblue")
    legend("top", legend = paste(c("Mean =", "Mode =", "Variance ="), c(g.mean, g.mode, g.var)), bty = "n")
  })
}