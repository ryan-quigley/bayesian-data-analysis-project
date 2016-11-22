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
                       value = 5,
                       step = 0.5)
    ),
    column(4,
           sliderInput("a",
                       "Gamma Shape (alpha):",
                       min = 0,
                       max = 5,
                       value = 1,
                       step = 0.1)
    ),
    column(4,
           sliderInput("b",
                       "Gamma Scale (1/beta):",
                       min = 0,
                       max = 100,
                       value = 58,
                       step = 0.5)
    )
  )
)