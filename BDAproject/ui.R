library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("MATH 264: BDA Project"),
  plotOutput("distPlot"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(3, offset = 1,
           sliderInput("h",
                       "Bandwidth:",
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