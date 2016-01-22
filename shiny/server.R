require(shiny)
require(shinysky)
require(knitr)
require(lattice)
require(shinyAce)
helpTip <- function(cont, ...) {
    a(class = 'fa fa-question-circle help-tool-tip', `data-toggle` = 'tooltip', `data-html` = T,
      `data-title` = span(cont), ...)
}
helpPop <- function(cont, ti = NULL, ...) {
    a(class = 'fa fa-question-circle help-tool-tip', `data-toggle` = 'popover', `data-html` = T,
      `data-trigger` = 'hover', `data-title` = ti, `data-placement` = 'top', 
      `data-content` = span(cont), ...)
}

shinyServer(function(input, output, session) {
    exist <- readLines('.data/exist_wid')
    pg_qry <- reactiveValues(visit = 'home')
    timer <- reactiveTimer(200, session)
    
    ## Util Functions
    generateCAPTCHA <- function(fn) {
        src = sample(c(LETTERS, rep(0:9, 3)), 5)
        png(fn, width = 136, height = 34, pointsize = 34)
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 5), c(0, 1), type = 'n', frame = F, axes = F)
        points(runif(10, 0, 5), runif(10, 0, 1), col = 'grey50', cex = runif(10, 0.5, 3))
        for (i in 1:5) {
            text(i - 0.5 + runif(1, -0.2, 0.2), 0.5 + runif(1, -0.2, 0.2), src[i],
                 cex = runif(0.8, 1), srt = runif(1, -20, 20),
                 col = paste0('grey', ceiling(runif(1, 0, 50))))
            abline(runif(1, -1, 1), runif(1, 0, 1), col = 'grey50')
        }
        dev.off()
        paste(src, collapse = '')
    }
    refreshCaptcha <- function() {
        f <- tempfile('captcha', '.tmp')
        pg_qry$captcha <- list(ans = generateCAPTCHA(f), uri = image_uri(f))
        unlink(f)
    }
    
    ## Panel Rendering Functions
    getTrainingProgressPanel <- function() {
        wid <- isolate(pg_qry$wid)
        list(
            div(class = 'panel panel-success panel-work-progress',
                div(class = 'panel-heading',
                    div(class = 'panel-title', h4('ANN Model Training in Progress', tags$small('Work ID #', span(class = 'work-id', wid))),
                        tags$small('Remember or Note down Your Work ID (without the hash sign), by which you can leave and close the window now, and get back to the progress using this ID'))),
                htmlOutput('workProgress', class = 'panel-body')
            ),
            actionButton('backToHomeAction', 'Back to Home', block = T, icon = 'chevron-left', icon.library = 'font awesome'),
            actionButton('WIDAnother', 'Change Work ID', block = T, icon = 'undo', icon.library = 'font awesome')
        )
    }
    getQryWIDPanel <- function() {
        list(
            div(class = 'panel panel-primary',
                div(class = 'panel-heading',
                    div(class = 'panel-title', h4('Enter Work ID to Extract Your ANN Model'))),
                div(class = 'panel-body',
                    textInput('checkWID', 'Enter Your Work ID Code', width = '100%'),
                    uiOutput('checkUIDWrong')
                )
            ),
            actionButton('checkWIDAction', 'Submit Work ID', styleclass = 'primary', block = T, icon = 'check', icon.library = 'font awesome'),
            actionButton('backToHomeAction', 'Back to Home', block = T, icon = 'chevron-left', icon.library = 'font awesome')
        )
    }
    getPredictionResultPanel <- function(res) {
        pres <- character()
        maxline <- length(res)
        if (maxline > 5) maxline <- 5
        for (i in 1:maxline) {
            i <- i ## reactive env!!
            pres <- paste0(pres, res[i], '\n')
        }
        pres <- substr(pres, 1, nchar(pres) - 1)
        list(
            tags$hr(), h4('Prediction Results', tags$small('Only First 5 Lines Are Presented In the Text Box')),
            aceEditor('predRedult', pres, readOnly = T, mode = 'plain_text', theme = 'monokai', height = '90px'),
            div(style = 'display:block',
                span(class = 'fa fa-download'), span(style = 'font-weight:500', 'Download'), 'This Prediction List as:',
                span(class = 'fa fa-file-text-o'), downloadLink('downloadPred', 'Text File')
            )
        )
    }
    
    ## Page Rendering Functions
    getHomePage <- function() {
        list(
            div(class = 'jumbotron',
                h1('ANN Model'),
                h2('for Protein Secondary Structure'),
                p('Train your own', span(style = 'font-weight:400', 'Artificial Neural Network'),
                  helpPop('Click the question mark to see Artificial Nueral Network page in Wikipedia.', href = 'https://en.wikipedia.org/wiki/Artificial_neural_network', target = '_blank'),
                  'model and use it to predict protein secondary structure!'),
                br(), hr(), br(),
                actionButton('addNewModelAction', 'Train A New ANN Model', styleclass = 'primary', block = T, icon = 'chevron-right', icon.library = 'font awesome'),
                actionButton('predictUsingExistingModelsAction', 'Predict Using Existing Models', block = T, icon = 'random', icon.library = 'font awesome'),
                actionButton('checkTrainingProgressAction', 'Check Your Model Training Progress', block = T, icon = 'check-square-o', icon.library = 'font awesome')
            ),
            tags$script(type = 'text/javascript', src = 'popover-regist.js')
        )
    }
    getAddNewPage <- function() {
        list(
            div(class = 'panel panel-primary',
                div(class = 'panel-heading',
                    div(class = 'panel-title', h4('Add and Train Your Own ANN'))
                ),
                div(class = 'panel-body',
                    div(class = 'row',
                        div(class = 'col-md-6',
                            div(class = 'panel panel-primary',
                                div(class = 'panel-heading',div(class = 'panel-tile', h4('Model Parameters'))),
                                div(class = 'panel-body',
                                    sliderInput('addNewIWS', list('Input Layer Window Size', helpPop(list('Number of residuals', strong('both'), 'to the left and to the right of the target residual that are input.'))),
                                                min = 5, max = 25, value = 14, step = 1, width = '100%'),
                                    numericInput('addNewHLN', 'Number of Hidden Layers', min = 1, max = 4, value = 1, step = 1, width = '100%'),
                                    uiOutput('addNewHiddenLayers'),
                                    selectInput('addNewOTY',
                                                list('Output Layer Structure Type', helpPop(list('In', strong('2 Units with Default as Coils'), 'we have 2 units represents Helices and Sheets, Coils are output when neither units gives output larger than 0.5.', tags$br(),
                                                                                                 'In', strong('3 Units Each for On Case'), 'we have 3 units represents each output case, and the one have the greatest value are output.'))),
                                                choices = c('2 Units with Default as Coils' = 2, '3 Units Each for On Case' = 3), width = '100%')
                                )
                            )
                        ),
                        div(class = 'col-md-6',
                            div(class = 'panel panel-primary',
                                div(class = 'panel-heading', div(class = 'panel-tile', h4('Training Settings'))),
                                div(class = 'panel-body',
                                    numericInput('addNewDCO', list('Displacement Coefficient', helpPop('The factor multiplied to the gradient that are subtracted from the weights and biases in the back probagation process')),
                                                                   min = 0, max = 10, value = 0.1, step = 0.05, width = '100%'),
                                    sliderInput('addNewCER', list('Critical Error Rate', helpPop('The training will end when the in-sample error rate for the current iteration is lower than this critical value')),
                                                min = 0.01, max = 0.4, value = 0.2, step = 0.01, width = '100%'),
                                    sliderInput('addNewMFL', list('Maxium Iteration Number', helpPop('The training will end when the iteration number excess this number')),
                                                min = 20, max = 150, value = 20, step = 1, width = '100%'),
                                    numericInput('addNewRSD', list('Set Random Seed', helpPop('This seed number will ensure the reproducibility if you are using the same data and parameters to run a second time')),
                                                 min = 1, max = 999, value = 1, step = 1, width = '100%')
                                )
                            )
                        ),
                        div(class = 'col-md-12',
                            div(class = 'panel panel-primary',
                                div(class = 'panel-heading',
                                    div(class = 'panel-tile', h4('Upload Training Data'),
                                        tags$small(strong('File Format:'), tags$br(), 'Only text files are valid. The file should have 2N lines for N peptids. For the i', tags$sup('th'), 'peptide, in the 2i-1 line there should be the peptide sequence in one-char abbriviation, and in the 2i line there should be the correct 2nd structure where \'H\' means helices, \'E\' means sheets and \'-\' means coils.'))),
                                div(class = 'panel-body',
                                    fileInput('addNewTrain', 'Upload Training Set File', width = '100%'),
                                    fileInput('addNewTest', 'Upload Testing Set File', width = '100%')
                                )
                            )
                        )
                    ),
                    uiOutput('captchaPanel', class = 'row'),
                    uiOutput('addNewWrong')
                )
            ),
            actionButton('addNewSubmitAction', 'Train the Model', styleclass = 'primary', block = T, icon = 'check', icon.library = 'font awesome'),
            actionButton('backToHomeAction', 'Back to Home', block = T, icon = 'chevron-left', icon.library = 'font awesome'),
            tags$script(type = 'text/javascript', src = 'popover-regist.js')
        )
    }
    getPredictPage <- function() {
        if (is.null(pg_qry$wid)) {
            getQryWIDPanel()
        } else {
            if (pg_qry$state == 'running') {
                getTrainingProgressPanel()
            } else {
                wid <- pg_qry$wid
                list(
                    div(class = 'panel panel-success',
                        div(class = 'panel-heading',
                            div(class = 'panel-title', h4('Secondary Structure Prediction', tags$small('Work ID #', span(class = 'work-id', wid))),
                                tags$small('Enter sequences or Upload Sequence File to Get Secondary Structure Prediction', tags$br(),
                                           strong('File Format:'), tags$br(), 'Only text files are valid. The file should have N lines for N peptides where each line is a pepteide sequence in one-char abbriviation.'))),
                        div(class = 'panel-body',
                            tabsetPanel(id = 'predSrcType',
                                        tabPanel(title = span(style = 'font-weight:400', 'Text Input'), value = 'text',
                                                 aceEditor('predSrcT', value = 'ENTER\nPEPTIDES\nHERE',
                                                           mode = 'plain_text', theme = 'monokai', height = '90px')
                                        ),
                                        tabPanel(title = span(style = 'font-weight:400', 'File Input'), value = 'file',
                                                 fileInput('predSrcF', label = NULL, width = '100%')
                                        )
                            ),
                            uiOutput('predictionResult'),
                            tags$a(id = 'predictingBusy', style = 'display:block', 'data-toggle' = 'tooltip', 'data-placement' = 'bottom', 'data-trigger' = 'manual', 'data-html' = T,
                                   'data-title' = span(span(class = 'fa fa-cog fa-spin'), 'Predicting...'))
                        )
                    ), # UKKE7J
                    actionButton('predictAction', 'Go Predicting', styleclass = 'success', block = T, icon = 'random', icon.library = 'font awesome'),
                    actionButton('toCheckAction', 'Check Model Status', styleclass = 'primary', block = T, icon = 'check-square-o', icon.library = 'font awesome'),
                    actionButton('backToHomeAction', 'Back to Home', block = T, icon = 'chevron-left', icon.library = 'font awesome'),
                    actionButton('WIDAnother', 'Change Work ID', block = T, icon = 'undo', icon.library = 'font awesome'),
                    tags$script(type = 'text/javascript', src = 'tooltip-regist.js')
                )
            }
        }
    }
    getProgressPage <- function() {
        if (is.null(pg_qry$wid)) {
            getQryWIDPanel()
        } else {
            if (pg_qry$state == 'running') {
                getTrainingProgressPanel()
            } else {
                wid <- isolate(pg_qry$wid)
                f <- readLines(paste0('.data/', wid, '_rec'))
                list(
                    div(class = 'panel panel-success',
                        div(class = 'panel-heading',
                            div(class = 'panel-title', h4('ANN Model Evaluation', tags$small('Work ID #', span(class = 'work-id', wid))),
                                tags$small('Remember or Note down Your Work ID (without the hash sign), by which you can leave and close the window now, and get back to the progress using this ID'))
                        ),
                        div(class = 'panel-body row',
                            div(class = 'col-md-6', 
                                div(class = 'panel panel-default',
                                    div(class = 'panel-heading', div(class = 'panel-title', 'In-Sample Error Rate:', span(class = 'error-rate', f[1]), helpPop(list('The rate of false prediction in the', strong('training'), 'data set')))),
                                    div(class = 'panel-body', tags$img(class = 'img-responsive', src = f[2]))
                                )
                            ),
                            div(class = 'col-md-6', 
                                div(class = 'panel panel-default',
                                    div(class = 'panel-heading', div(class = 'panel-title', 'Out-of-Sample Error Rate:', span(class = 'error-rate', f[3]), helpPop(list('The rate of false prediction in the', strong('validating/testing'), 'data set')))),
                                    div(class = 'panel-body', tags$img(class = 'img-responsive', src = f[4]))
                                )
                            ),
                            div(class = 'col-xs-12', span(class = 'fa fa-download'), span(style = 'font-weight:500', 'Download'), 'This Model as:',
                                span(class = 'fa fa-cubes'), downloadLink('downLoadRObj', list('R Object', helpPop(list('The model object and methods saved in an Rdata file, use', tags$code('load()'), 'function in R to extract the model')))), 'or',
                                span(class = 'fa fa-table'), downloadLink('downLoadCSVCoef', list('Coefficient Tables', helpPop('Zipped file contains CSV files of all weights and bias tables'))))
                        )
                    ),
                    actionButton('toPredictAction', 'Use This Model to Predict', styleclass = 'success', block = T, icon = 'random', icon.library = 'font awesome'),
                    actionButton('backToHomeAction', 'Back to Home', block = T, icon = 'chevron-left', icon.library = 'font awesome'),
                    actionButton('WIDAnother', 'Change Work ID', block = T, icon = 'undo', icon.library = 'font awesome'),
                    tags$script(type = 'text/javascript', src = 'popover-regist.js')
                )
            }
        }
    }
    
    
    ## Simple Output Renderers
    output$captchaPanel <- renderUI({
        list(
            div(class = 'col-md-6 col-xs-12', textInput('userCaptcha', NULL, width = '100%')),
            div(class = 'col-md-3 col-xs-6', tags$img(class = 'img-responsive', src = pg_qry$captcha$uri)),
            div(class = 'col-md-3 col-xs-6', actionButton('changeCaptchaAction', 'Change CAPTCHA', block = T))
        )
    })
    output$workProgress <- renderText({
        paste0('<p>', paste0(pg_qry$progress, collapse = '<br/>'), '</p>')
    })
    output$addNewHiddenLayers <- renderUI({
        li <- list(
            sliderInput('addNewHLU1', 'Number of Units in Hidden Layer #1',
                        min = 2, max = 14, value = 2, step = 1, width = '100%'),
            sliderInput('addNewHLU2', 'Number of Units in Hidden Layer #2',
                        min = 2, max = 14, value = 2, step = 1, width = '100%'),
            sliderInput('addNewHLU3', 'Number of Units in Hidden Layer #3',
                        min = 2, max = 14, value = 2, step = 1, width = '100%'),
            sliderInput('addNewHLU4', 'Number of Units in Hidden Layer #4',
                        min = 2, max = 14, value = 2, step = 1, width = '100%')
        )
        li[1:(input$addNewHLN)]
    })
    output$downLoadRObj <- downloadHandler(
        filename = function() {paste0('ANN_model_', pg_qry$wid, '.Rdata')},
        content = function(file) {file.copy(paste0('.data/', pg_qry$wid, '_model'), file)}
    )
    output$downLoadCSVCoef <- downloadHandler(
        filename = function() {paste0('ANN_model_coefs_', pg_qry$wid, '.zip')},
        content = function(file) {file.copy(paste0('.data/', pg_qry$wid, '_coef.zip'), file)}
    )
    
    ## Observers
    observeEvent(pg_qry$visit, {
        if (pg_qry$visit == 'home') {
            output$page <- renderUI(getHomePage())
        } else if (pg_qry$visit == 'add') {
            refreshCaptcha()
            output$page <- renderUI(getAddNewPage())
        } else if (pg_qry$visit == 'predict') {
            output$page <- renderUI(getPredictPage())
        } else if (pg_qry$visit == 'check') {
            output$page <- renderUI(getProgressPage())
        }
    })
    observeEvent(input$backToHomeAction, {
        pg_qry$visit <- 'home'
    })
    observeEvent(input$addNewModelAction, {
        pg_qry$visit <- 'add'
    })
    observeEvent(input$predictUsingExistingModelsAction, {
        pg_qry$visit <- 'predict'
    })
    observeEvent(input$checkTrainingProgressAction, {
        pg_qry$visit <- 'check'
    })
    observeEvent(input$changeCaptchaAction, {
        refreshCaptcha()
    })
    observeEvent(input$addNewSubmitAction, {
        if (input$userCaptcha != pg_qry$captcha$ans) {
            output$addNewWrong <- renderUI(helpText('Wrong Captcha!'))
            refreshCaptcha()
        } else if (!all(is.null(c(input$addNewTest, input$addNewTrain)))) {
            f <- readLines(input$addNewTrain$datapath)
            valid <- !any(grepl('[^ARNDCQEGHILKMFPSTWYV]', f[2 * 1:length(f) - 1])) && !any(grepl('[^-HE]', f[2 * 1:length(f)]))
            f <- readLines(input$addNewTest$datapath)
            valid <- !any(grepl('[^ARNDCQEGHILKMFPSTWYV]', f[2 * 1:length(f) - 1])) && !any(grepl('[^-HE]', f[2 * 1:length(f)])) && valid
            if (valid) {
                wid <- paste(sample(c(0:9, LETTERS), 6, replace = T), collapse = '')
                while (wid %in% exist) {
                    wid <- paste(sample(c(0:9, LETTERS), 6, replace = T), collapse = '')
                }
                file.copy(from = input$addNewTrain$datapath,
                          to = paste0('.data/', wid, '_train'))
                file.remove(input$addNewTrain$datapath)
                file.copy(from = input$addNewTest$datapath,
                          to = paste0('.data/', wid, '_test'))
                file.remove(input$addNewTest$datapath)
                hlu <- numeric()
                for (i in 1:input$addNewHLN) {
                    i <- i
                    hlu[i] <- input[[paste0('addNewHLU', i)]]
                }
                hlu <- paste(hlu, collapse = ' ')
                system(paste('./new.R', wid, input$addNewIWS, input$addNewHLN, hlu, input$addNewOTY,
                             input$addNewDCO, input$addNewCER, input$addNewMFL, input$addNewRSD),
                       wait = FALSE)
                pg_qry$visit <- 'check'
                pg_qry$wid <- wid
                if (file.exists(paste0('./data', wid, '_sta'))) {
                    pg_qry$state <- readLines(paste0('./data', wid, '_sta'))
                } else {
                    pg_qry$state <- 'running'
                }
                exist <<- c(exist, wid)
                cat(paste0(wid, '\n'), file = paste0('.data/exist_wid'), append = T)
            } else {
                output$addNewWrong <- renderUI(helpText('Invalid File!'))
                refreshCaptcha()
            }
        }
    })
    observeEvent(input$checkWIDAction, {
        if (input$checkWID %in% exist) {
            wid <- input$checkWID
            if (file.exists(paste0('.data/', wid, '_sta'))) {
                pg_qry$state <- readLines(paste0('.data/', wid, '_sta'))
            } else {
                pg_qry$state <- 'running'
            }
            if (pg_qry$state == 'idle') {
                pg_qry$env <- new.env()
                load(paste0('.data/', wid, '_model'), envir = pg_qry$env)
            }
            pg_qry$wid <- wid
        } else {
            output$checkUIDWrong <- renderUI(helpText('Work ID Do Not Exist'))
        }
    })
    observeEvent(input$WIDAnother, {
        pg_qry$wid <- NULL
    })
    observeEvent(input$toPredictAction, {
        pg_qry$visit <- 'predict'
    })
    observeEvent(input$toCheckAction, {
        pg_qry$visit <- 'check'
    })
    observeEvent(input$predictAction, {
        # make sources into input sequences format, check the validity of input
        if (input$predSrcType == 'file') {
            if (!is.null((input$predSrcF$datapath))) {
                pred_input <- readLines(input$predSrcF$datapath)
            } else {
                pred_input <- character(0)
            }
        } else {
            pred_input <- strsplit(input$predSrcT, '\n')[[1]]
        }
        pred_input <- toupper(pred_input)
        if (any(grepl('[^ARNDCQEGHILKMFPSTWYV]', pred_input)) || !length(pred_input)) {
            output$predictionResult <- renderUI(helpText('Invalide Input Strings!'))
        } else {
            session$sendCustomMessage('predicting', list(busy = T))
            res <- isolate(pg_qry$env$ann$predict(pred_input))
            session$sendCustomMessage('predicting', list(busy = F))
            output$predictionResult <- renderUI(getPredictionResultPanel(res))
            output$downloadPred <- downloadHandler(
                filename = function() {paste0('Prediction_', pg_qry$wid, '.txt')},
                content = function(file) {writeLines(res, file)}
            )
        }
    })
    observe({
        input$predSrcType
        input$predSrcT
        input$predSrcF
        output$predictionResult <- renderUI(NULL)
    })
    
    ## Timing Observer
    observeEvent(timer(), {
        if (!is.null(pg_qry$wid)) {
            if (file.exists(paste0('.data/', pg_qry$wid, '_sta'))) {
                pg_qry$state <- readLines(paste0('.data/', pg_qry$wid, '_sta'))
                if (pg_qry$state == 'running') {
                    pg_qry$progress <- system(
                        paste0('tail .data/', pg_qry$wid, '_log'), intern = T
                    )
                }
            }
        }
    })
})