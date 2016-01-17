require(shiny)
shinyUI(
    fluidPage(
        tags$head(
            tags$title('ANN for Protein Secondary Structure Prediction'),
            tags$link(rel = "stylesheet", href = "lib/font-awesome/css/font-awesome.min.css"),
            tags$script(type = 'text/javascript', src = 'custom.js'),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        uiOutput('page', class = 'holder')
    )
)