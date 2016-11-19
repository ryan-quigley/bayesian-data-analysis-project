#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MATH 264: BDA Project"),
   plotOutput("distPlot"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(3, offset = 1,
         sliderInput("h",
                     "Banwidth:",
                     min = 1,
                     max = 20,
                     value = 10,
                     step = 0.5)
      ),
      column(4,
         sliderInput("k",
                     "Gamma Shape (k):",
                     min = 0,
                     max = 5,
                     value = 1.5,
                     step = 0.1)
      ),
      column(4,
         sliderInput("theta",
                      "Gamma Scale (theta):",
                      min = 0,
                      max = 100,
                      value = 35,
                      step = 0.5)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     d <- density(dc.filt$Body_Count, kernel = "epanechnikov", bw = input$h)
     plot(d, ann = FALSE, axes = FALSE,
          xlim = c(-100, 700), ylim = c(0, 0.02), 
          bty = "n", col = "violetred", lwd = 1.5)
     axis(1, at = seq.int(-50, 700, 50), labels = NA)
     axis(1, at = seq.int(0, 600, 100), lwd = 0, lwd.ticks = 0)
     axis(2, at = seq.int(0, 0.02, 0.005), pos = -75)
     
     ## Gamma parameter controls
     g.mean <- input$k*input$theta
     g.mode <- (input$k-1)*input$theta
     g.var <- input$k*(input$theta)^2
     
     ## Plot theoretical gamma distribution
     curve(dgamma(x, shape = input$k, scale = input$theta), from = 0, to = 400, add = TRUE, lty = 2, col = "slateblue")
     legend("top", legend = paste(c("Mean =", "Mode =", "Variance ="), c(g.mean, g.mode, g.var)), bty = "n")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

