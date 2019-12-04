library(shiny)

shinyUI(fluidPage(
  titlePanel("Falconer additive-dominance model"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderP", "p",
                  min = 0.0, max = 1.0, value = 0.3),
      numericInput("numericP", min = 0.0, max = 1.0, value = 0.3, step = 0.1, ""),
      sliderInput("sliderA", "a",
                  min = -10.0, max = 10.0, value = 4.0),
      numericInput("numericA", min = -10.0, max = 10.0, value = 4.0, ""),
      sliderInput("sliderD", "d",
                  min = -10.0, max = 10.0, value = 2.0),
      numericInput("numericD", min = -10.0, max = 10.0, value = 2.0, ""),
      h3("Variance components"),
      textOutput("varianceA"),
      textOutput("varianceD"),
      textOutput("varianceG"),
      textOutput("varianceRatio")
    ),

    mainPanel(
      div(img(src='model.jpg'), br(), "Arbitrarily assigned genotypic values.", style="text-align: center;"),

      plotOutput("plot")
    )
  )
))
