#' Interactive polynomial explorer
#'
#' @return interactive shiny app
#' @import shiny
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @export

poly_explorer <-
  function() {shiny::shinyApp(ui = poly_explorer_ui, server = poly_explorer_server)}

#' @rdname  poly_explorer

poly_explorer_ui <-
  shinyUI(fluidPage(

    # Application title
    titlePanel("Polynomial Explorer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput("b_0",
                    "b_0",
                    min = -20,
                    max = 20,
                    value = 0,
                    step = .1),
        sliderInput("b_1",
                    "b_1",
                    min = -20,
                    max = 20,
                    value = 0,
                    step = .1),
        sliderInput("b_2",
                    "b_2",
                    min = -20,
                    max = 20,
                    value = 0,
                    step = .1),
        sliderInput("b_3",
                    "b_3",
                    min = -20,
                    max = 20,
                    value = 0,
                    step = .1),
        sliderInput("b_4",
                    "b_4",
                    min = -20,
                    max = 20,
                    value = 0,
                    step = .1),
        sliderInput("b_5",
                    "b_5",
                    min = -20,
                    max = 20,
                    value = 0,
                    step = .1),
        sliderInput("x_range",
                    "x_range:",
                    min = -200,
                    max = 200,
                    value = c(0, 20),
                    step = 1)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("polyPlot")
      )
    )
  ))



#' @rdname  poly_explorer

# Define server logic required to draw a histogram
poly_explorer_server <-
  shinyServer(function(input, output) {

    output$polyPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x_min = 0
      x_steps <- 200
      D <-
        data_frame(x = seq(input$x_range[1], input$x_range[2], length.out = x_steps)) %>%
        mutate(y = input$b_0 + input$b_1 * x +
                 input$b_2 * x^2 +
                 input$b_3 * x^3 +
                 input$b_4 * x^4 +
                 input$b_5 * x^5)
      # draw graph
      D %>%
        ggplot(aes(x = x, y = y)) +
        geom_path()
    })
  })






