#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(dplyr)

# global values -----
start_1 <- "09:00:00"
start_2 <- "10:00:00"

# create directory
path <- "~/Desktop/timestamps"
path_start_time <- "start_time"
path_timestamp <- "timestamp"

# global functions -----
#' get_time_dif
#'
#' @param start_time Start time `Sys.time()`.
#' @param end_time End time `Sys.time()`.
#'
#' @return Durantion in HH:MM:SS
#'
get_time_dif <- function(start_time, end_time) {
    # get difference
    tmp <- as.numeric(difftime(strptime(end_time,"%H:%M:%S"),
                               strptime(start_time,"%H:%M:%S"),
                               units = "secs"))
    # convert
    minutes <- tmp / 60
    seconds <- tmp %% 60
    hours <- minutes / 60
    minutes <- minutes %% 60
    # minpulate
    hours <- sprintf("%02d", as.integer(hours))
    minutes <- sprintf("%02d", as.integer(minutes))
    seconds <- sprintf("%02d", as.integer(seconds))
    # return
    return(paste0(hours, ":", minutes,":", seconds))
}

#' do_not_overwirte
#'
#' @param file_path file path of end distination.
#'
#' @return updated file path.
#'
do_not_overwirte <- function(file_path) {
    iter <- 2
    while(file.exists(file_path)) {
        file_path <- paste0(unlist(strsplit(file_path, ".rda")), "_", iter, ".rda")
    }
    # return
    return(file_path)
}

# Define server logic ----
#' server
#'
#' @param input input list from client.
#' @param output output list to client.
#' @param session current session variable.
#'
server <- function(input, output, session) {

    # create directories ----
    if (!dir.exists(paste0(path,"/",path_start_time))) { dir.create(paste0(path,"/",path_start_time), recursive = T) }
    if (!dir.exists(paste0(path,"/",path_timestamp))) { dir.create(paste0(path,"/",path_timestamp), recursive = T) }

    # Side panel ----
    times <- reactiveValues(start_time_3km = start_2,
                            start_time_5km = start_2,
                            start_time_5km = start_2,
                            start_time_10km = start_2,
                            start_time_halv = start_1,
                            start_time_26km = start_2,
                            start_time_52km = start_1)
    output$time_3km <- renderUI({ timeInput("time_3km", "Tid 3km:", value = strptime(times$start_time_3km, "%T")) })
    output$time_5km <- renderUI({ timeInput("time_5km", "Tid 5km:", value = strptime(times$start_time_5km, "%T")) })
    output$time_10km <- renderUI({ timeInput("time_10km", "Tid 10km:", value = strptime(times$start_time_10km, "%T")) })
    output$time_26km <- renderUI({ timeInput("time_26km", "Tid 26km:", value = strptime(times$start_time_26km, "%T")) })
    output$time_halv <- renderUI({ timeInput("time_halv", "Tid Halvmar.:", value = strptime(times$start_time_halv, "%T")) })
    output$time_52km <- renderUI({ timeInput("time_52km", "Tid 52km:", value = strptime(times$start_time_52km, "%T")) })

    # start time
    output$update_time <- renderUI({ radioButtons("update_time", "Vælg tid for start",
                                                  choices = c("Std.", "Timestamp", "Manuel")#,
                                                  # choiceNames = c("Std.", "Timestamp", "Manuel"),
                                                  # choiceValues = c("Std.", "Timestamp", "Manuel")
                                                  ) })
    output$start_1 <- renderUI({ actionButton("start_1", "Start løb 1") })
    output$start_2 <- renderUI({ actionButton("start_2", "Start løb 2") })
    observeEvent(input$start_1, {
        #
        if (input$update_time == "Timestamp"){
            times$start_time_halv <- times$start_time_52km <- format(Sys.time(), format = "%H:%M:%S")
        } else if (input$update_time == "Manuel"){
            times$start_time_halv <- format(input$time_halv, format = "%H:%M:%S")
            times$start_time_52km <- format(input$time_52km, format = "%H:%M:%S")
        } else {
            times$start_time_halv <- times$start_time_52km <- start_1
        }
        # update output
        output$start_1_msg <- renderText({ paste0("Starttider: \n",
                                                  "- Halvmar.:\t", times$start_time_halv,"\n",
                                                  "- 52km:\t\t", times$start_time_52km,"\n") })

        output$time_halv <- renderUI({ timeInput("time_halv", "Tid Halvmar.:", value = strptime(times$start_time_halv, "%T")) })
        output$time_52km <- renderUI({ timeInput("time_52km", "Tid 52km:", value = strptime(times$start_time_52km, "%T")) })
        # save




        tmp <- times$start_time_halv
        save(tmp, file = do_not_overwirte(paste0(path, "/", path_start_time, "/start_time_halv.rda")))
        tmp <- times$start_time_52km
        save(tmp, file = do_not_overwirte(paste0(path, "/", path_start_time, "/start_time_52km.rda")))

    })
    observeEvent(input$start_2, {
        #
        if (input$update_time == "Timestamp"){
            times$start_time_3km <- times$start_time_5km <- times$start_time_10km <- times$start_time_26km <- format(Sys.time(), format = "%H:%M:%S")
        } else if (input$update_time == "Manuel"){
            times$start_time_3km <- format(input$time_3km, format = "%H:%M:%S")
            times$start_time_5km <- format(input$time_5km, format = "%H:%M:%S")
            times$start_time_10km <- format(input$time_10km, format = "%H:%M:%S")
            times$start_time_26km <- format(input$time_26km, format = "%H:%M:%S")
        } else {
            times$start_time_3km <- times$start_time_5km <- times$start_time_10km <- times$start_time_26km <- start_2
        }
        # update output
        output$start_2_msg <- renderText({ paste0("Starttider: \n",
                                                  "- 3km:\t\t", times$start_time_3km,"\n",
                                                  "- 5km:\t\t", times$start_time_5km,"\n",
                                                  "- 10km:\t\t", times$start_time_10km,"\n",
                                                  "- 25km:\t\t", times$start_time_26km,"\n") })

        output$time_3km <- renderUI({ timeInput("time_3km", "Tid 3km:", value = strptime(times$start_time_3km, "%T")) })
        output$time_5km <- renderUI({ timeInput("time_5km", "Tid 5km:", value = strptime(times$start_time_5km, "%T")) })
        output$time_10km <- renderUI({ timeInput("time_10km", "Tid 10km:", value = strptime(times$start_time_10km, "%T")) })
        output$time_26km <- renderUI({ timeInput("time_26km", "Tid 26km:", value = strptime(times$start_time_26km, "%T")) })
        # save
        tmp <- times$start_time_3km
        save(tmp, file = do_not_overwirte(paste0(path, "/", path_start_time, "/start_time_3km.rda")))
        tmp <- times$start_time_5km
        save(tmp, file = do_not_overwirte(paste0(path, "/", path_start_time, "/start_time_5km.rda")))
        tmp <- times$start_time_10km
        save(tmp, file = do_not_overwirte(paste0(path, "/", path_start_time, "/start_time_10km.rda")))
        tmp <- times$start_time_26km
        save(tmp, file = do_not_overwirte(paste0(path, "/", path_start_time, "/start_time_26km.rda")))
    })

    # main panel ----
    df_table <- reactiveValues(df = data.frame(nummer = integer(),
                                               timestamp = character(),
                                               km3 = character(),
                                               km5 = character(),
                                               km10 = character(),
                                               kmhalv = character(),
                                               km26 = character(),
                                               km52 = character()))
    output$df_table <- renderDataTable(df_table$df)
    output$timestamp <- renderUI({ numericInput("timestamp", "Løbsnummer", value = NA, step = 1, min = 1, max = 500) })
    output$timestamp_btn <- renderUI({ actionButton("timestamp_btn", "Stamp") })
    observeEvent(input$timestamp_btn, {
        # get time
        tmp_time <- format(Sys.time(), format = "%H:%M:%S")
        tmp_number <- input$timestamp
        #
        if (!is.na(tmp_number)){
            output$timestamp_msg <- renderText({ paste0("\nNr.: ", tmp_number, " klokken: ", tmp_time, "\n \n") })
            updateNumericInput(session, "timestamp", "Løbsnummer", NA)
            # save
            save(tmp_time, file = do_not_overwirte(paste0(path, "/", path_timestamp, paste0("/runner_", tmp_number, ".rda"))))
            # update data
            df_table$df <- tmp <- rbind(df_table$df, data.frame(nummer = tmp_number, timestamp = tmp_time,
                                                                km3 = get_time_dif(times$start_time_3km, tmp_time),
                                                                km5 = get_time_dif(times$start_time_5km, tmp_time),
                                                                km10 = get_time_dif(times$start_time_10km, tmp_time),
                                                                kmhalv = get_time_dif(times$start_time_halv, tmp_time),
                                                                km26 = get_time_dif(times$start_time_26km, tmp_time),
                                                                km52 = get_time_dif(times$start_time_52km, tmp_time))) %>%
                arrange(desc(timestamp))
            #
            save(tmp, file = paste0(path, "/", path_timestamp, "/tot_df.rda"))
            # render table
            output$df_table <- renderDataTable(df_table$df)
        } else {
            output$timestamp_msg <- renderText({ "\nIkke validt nummer.\n \n" })
        }
    })
    # end ----
}

# Define UI for application ----
ui <- fluidPage(

    # Application title
    titlePanel("Timing Krengerupløbet"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Sæt starttider"),
            uiOutput("time_3km"),
            uiOutput("time_5km"),
            uiOutput("time_10km"),
            uiOutput("time_halv"),
            uiOutput("time_26km"),
            uiOutput("time_52km"),
            br(),
            h4("Start "),
            uiOutput("default_time"),
            uiOutput("update_time"),
            uiOutput("start_1"),
            verbatimTextOutput("start_1_msg"),
            br(),
            uiOutput("start_2"),
            verbatimTextOutput("start_2_msg"),
            br()
        ),
        # Show a plot of the generated distribution
        mainPanel(
            h4("Sæt starttider"),
            br(),
            fluidRow(column(width = 3,
                            uiOutput("timestamp"),
                            uiOutput("timestamp_btn")),
                     column(width = 5,
                            verbatimTextOutput("timestamp_msg"))),
            br(),
            br(),
            dataTableOutput("df_table"),
            br()
        )
    )
)

# Run the application ----
#' Run the application
#'
#' @param ui user interface "variable".
#' @param server all the server functionality.
#' @return Exucute the application.
#'
shinyApp(ui, server)
