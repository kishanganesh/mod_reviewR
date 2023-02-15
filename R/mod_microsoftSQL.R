
# UI ----
#' microsoftSQL Setup UI
#'
#' This module is designed to guide a user through the process of authenticating your database
#' 
#' @param id The module namespace
#' 
#' @return The microsoftSQL Setup UI
#' @keywords internal
#' 
#' 
#' @importFrom shiny NS
#'

microsoftSQL_setup_ui <- function(id) { 
  ns <- NS(id)
  tagList(
    ## UI widgets here ----
    textInput(ns("server"), "Server"),
    textInput(ns("database"), "Database"),
    #passwordInput(ns("password"), "Password"),
    actionButton(ns("connect"), "Connect")
  )
}

# Server ----
#' microsoftSQL Setup Server
#'
#' @param id The Module namespace
#'
#' @return microsoftSQL connection variables
#' @keywords internal
#'
#' @importFrom DBI dbConnect
#' 
microsoftSQL_setup_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## microsoftSQL Export Values ----
      microsoftSQL_export <- reactiveValues(
        ### Module Info
        moduleName = 'microsoftSQL',
        moduleType = 'database',
        setup_ui = microsoftSQL_setup_ui,
        is_connected = 'no',       
        db_con = NULL
      )
      # Server Code Here ----
      
      observeEvent(input$connect, {
        con <- DBI::dbConnect(odbc::odbc(), 
                              Driver = "SQL Server", 
                              Server = input$server, 
                              Database = input$database, 
                              Trusted_Connection = "True")
        microsoftSQL_export$is_connected <- 'yes'
        microsoftSQL_export$db_con <- con
      })
      
      # Return ----
      return(microsoftSQL_export)
    }
  )
}
