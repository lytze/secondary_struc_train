require(shiny)
shinyUI(
    fluidPage(
        tags$head(
            tags$title('ANN for Protein Secondary Structure Prediction'),
            tags$link(rel = "stylesheet",
                      href = "https://use.fontawesome.com/releases/v5.5.0/css/all.css",
                      integrity = "sha384-B4dIYHKNBt8Bc12p+WXckhzcICo0wtJAoU8YZTY5qE0Id1GSseTk6S+L3BlXeVIU",
                      crossorigin="anonymous"),
            tags$script(type = 'text/javascript', src = 'custom.js'),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        uiOutput('page', class = 'holder')
    )
)