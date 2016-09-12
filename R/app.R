#' Interactive uncanny valley explorer
#'
#' @return interactive shiny app
#' @import shiny
#' @import plyr
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @import mascutils
#' @export

uv_explorer <-
  function() {shiny::shinyApp(ui = uv_explorer_ui, server = uv_explorer_server)}

#' @rdname  uv_explorer

uv_explorer_ui <-
  shinyUI(fluidPage(

   # Application title
   titlePanel("Uncanny Valley"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("part_info_uptake",
                     "part_info_uptake",
                     min = 0,
                     max = 5,
                     value = 1),
         sliderInput("part_shock",
                     "part_shock",
                     min = 0,
                     max = 5,
                     value = 1),
         sliderInput("part_reluctance",
                     "part_reluctance",
                     min = 0,
                     max = 10,
                     value = 5),
         sliderInput("n_stimuli",
                     "n_stimuli",
                     min = 1,
                     max = 200,
                     value = 20)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("G_response")
      )
   )
))


#' @rdname  uv_explorer

# Define server logic required to draw a histogram
uv_explorer_server <-
  shiny::shinyServer(function(input, output) {
   output$G_response <- renderPlot({
     D_input <-
       expand_grid(stim_humLik = seq(-10, 50, length.out = 80),
                   cond_prtime = c(100, 200, 800, 2000))


     D_sim <-
       uv_response(as.matrix(D_input[1:2]),
                       part_info_uptake = input$part_info_uptake,
                       part_shock = input$part_shock,
                       part_reluctance = input$part_reluctance) %>%
       z_score(emotion) %>%
       mutate(emotion = inv_logit(emotion * .2))
     plot(plot_dashboard(D_sim))
   })
})







