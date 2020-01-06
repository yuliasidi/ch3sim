
# Define the UI
ui <- shiny::bootstrapPage(
  
  shiny::selectInput(inputId = 'benefit_vars',label = 'Benefit',
                     choices = names(dt_out),multiple = TRUE,selected = 'bcva_48w'),
  shiny::selectInput(inputId = 'risk_vars',label = 'Risk',
                     choices = names(dt_out),multiple = TRUE,selected = 'ae_noc'),
  rhandsontable::rHandsontableOutput('crit_table'),
  rhandsontable::rHandsontableOutput('sub_table')
)


# Define the server code
server <- function(input, output,session) {

  shiny::observeEvent(c(input$benefit_vars,input$risk_vars),{
    output$crit_table <- rhandsontable::renderRHandsontable({
      bv <- input$benefit_vars
      rv <- input$risk_vars
      cv <- c(bv,rv)
      
      idx <- c(length(bv),length(rv))

      tbl <- tibble::tibble(br = rep(c('Benefit','Risk'),idx),
                            criteria = cv, 
                            edit = FALSE,
                            weight_baseline = NA_character_,
                            percent = NA_real_)
      
      rhandsontable::rhandsontable(tbl)
    })  
    
    output$sub_table <- rhandsontable::renderRHandsontable({
      
      tbl <- tibble::tibble(
        variable = 'bcvac_bl',
        level = unique(dt_out$bcvac_bl),
        mean = NA_real_,
        sd = NA_real_)
      
      rhandsontable::rhandsontable(tbl)
    })  
  })
  

}

# Return a Shiny app object
shiny::shinyApp(ui = ui, server = server)
