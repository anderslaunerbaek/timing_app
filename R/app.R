#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list = ls())
library(shiny)
# library(shinyTime)
library(dplyr)
library(lubridate)

# global values -----

# halvmarton
start_1 <- ydm_hms("2019-22-04 09:00:30", tz = "CEST")
# resten
start_2 <- ydm_hms("2019-22-04 10:00:43", tz = "CEST")


# create directory
path <- "~/dev/timing_app/data/2019/"
path_start_time <- paste0(path, "start_time/")
path_timestamp <- paste0(path, "timestamp/")

# global functions -----
#' get_time_dif
#'
#' @param start_time Start time `Sys.time()`.
#' @param end_time End time `Sys.time()`.
#'
#' @return Durantion in HH:MM:SS
#'
get_time_dif <- function(start_time, tmp_time) {
    # get difference in hh:mm:ss
    hms::as.hms(round(difftime(tmp_time, start_time, units = "secs"), 0))
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
        iter <- iter + 1
    }

    # return
    file_path
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
    # if (!dir.exists(path_start_time)) { dir.create(path_start_time, recursive = T) }
    if (!dir.exists(path_timestamp)) { dir.create(path_timestamp, recursive = T) }

    # main panel ----
    if (file.exists(paste0(path_timestamp, "/tot_df.rds"))) {
        df_table <- reactiveValues("df" = readRDS(file = paste0(path_timestamp, "/tot_df.rds")))
    } else {
        tmp_time <- now()
        df_table <- reactiveValues("df" = tibble("nummer" = as.integer(0),
                                                 "timestamp" = tmp_time,
                                                 "km3" = get_time_dif(start_2, tmp_time),
                                                 "km5" = get_time_dif(start_2, tmp_time),
                                                 "km10" = get_time_dif(start_2, tmp_time),
                                                 "kmhalv" = get_time_dif(start_1, tmp_time)) %>%
                                       slice(0)
        )

    }
    output$df_table <- renderDataTable(df_table$df)


    # TODO
    output$timestamp <- renderUI({ numericInput("timestamp", "Løbsnummer", value = NA, step = 1, min = 1, max = 700) })
    output$timestamp_btn <- renderUI({ actionButton("timestamp_btn", "-- Stamp -- Stamp -- Stamp -- Stamp --") })

    observeEvent(input$timestamp_btn, {
        # get timestamp
        tmp_time <- now()
        tmp_number <- isolate(input$timestamp)

        #
        if (!is.na(tmp_number)){
            # create
            output$timestamp_msg <- renderText({ paste0("\nNr.: ", tmp_number, " time: ", tmp_time, "\n \n") })

            # clear number input
            updateNumericInput(session, "timestamp", "Løbsnummer", NA)

            # save
            saveRDS(tmp_time, file = do_not_overwirte(paste0(path_timestamp, paste0("/runner_", tmp_number, ".rds"))))

            # update data
            df_table$df <- tmp <- rbind(df_table$df,
                                        tibble("nummer" = as.integer(tmp_number),
                                               "timestamp" = tmp_time,
                                               "km3" = get_time_dif(start_2, tmp_time),
                                               "km5" = get_time_dif(start_2, tmp_time),
                                               "km10" = get_time_dif(start_2, tmp_time),
                                               "kmhalv" = get_time_dif(start_1, tmp_time))) %>%
                arrange(desc(timestamp))
            #
            saveRDS(tmp, file = paste0(path_timestamp, "/tot_df.rds"))

            # render table
            output$df_table <- renderDataTable(df_table$df)
        }  else {
            output$timestamp_msg <- renderText({ "\nNot a valid number...\n \n" })
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
        p("Create timestamp"),
        br(),
        uiOutput("timestamp"),
        uiOutput("timestamp_btn"),
        br(),
        verbatimTextOutput("timestamp_msg")),
        # Show a plot of the generated distribution
        mainPanel(
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
