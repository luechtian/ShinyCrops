library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(plotly)

######## Get data ########
gs4_deauth()

google_sensor_data <- "https://docs.google.com/spreadsheets/d/1DB3vyAnWFX0UHGzlBXV8oAxxW2aEw77zBdNm9LU0QfA/edit#gid=0"

devices <- read_sheet(google_sensor_data, sheet = "devices")

# mutate date_time-values into a format for better readability and join with devices-df
dummy_data <- read_sheet(google_sensor_data, sheet = "dummy_data") %>%
    mutate(date_time = parse_datetime(date_time),
           date = date(date_time),
           time = strftime(date_time, format="%H:%M", tz = "GMT")) %>%
    left_join(devices, by = "device_id")

# replace odd humidity data with nonsense numbers for better plotting
dummy_data$humidity_percent <- round(1200/dummy_data$temperature_celsius, 0)

# dummy data for water-tank level 
tank_level <- tibble(
    unique(dummy_data$field),
    c(20, 50, 80))
colnames(tank_level) <- c("field", "level_percent")
##########################

dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(href='https://mikrolawifeu.humhub.com/dashboard',
                                           tags$img(src='logo.png')
                                           )
ui <- dashboardPage(
    
    skin = "green",
    
    dbHeader,
    
    dashboardSidebar(
        selectInput(
            "field",
            "Acker:",
            choices = unique(as.character(dummy_data$field))
        ),
        selectInput(
            "lot",
            "Feldstück:",
            choices = unique(as.character(dummy_data$lot))
        )
    ),
    
    dashboardBody(
        # dashboardbody_box_1
        box(
            title = "Overview current data",
            status = "success",
            solidHeader = TRUE,
            width = NULL,
            
            valueBoxOutput("h2oBox", width = 3),
            valueBoxOutput("batteryBox", width = 3),
            valueBoxOutput("tempBox", width = 3),
            valueBoxOutput("humidBox", width = 3)
        ),
        
        # dashboardbody_box_2
        box(
            title = "Temperatur-/Luftfeuchtigkeitsplot des letzen Tages",
            status = "success",
            solidHeader = TRUE,
            width = NULL,
            
            plotlyOutput(outputId = 'plot')
        )
    )
    
)

server <- function(input, output, session) {
    
    # reactive selectInput "lot"; choices are dependent of selected input from "field"-selector
    observe({
        updateSelectInput(session,
                          "lot",
                          choices = unique(as.character(dummy_data %>%
                                                            filter(field == input$field) %>%
                                                            select(lot) %>%
                                                            pull())))
    })
    
    #### dashboardbody_box_1 output ####
    
    output$h2oBox <- renderValueBox({
        valueBox(
            paste(
                tank_level %>%
                    filter(field == input$field) %>%
                    pull(level_percent),
                "%"
            ),
            "Füllstand Wassertanks",
            icon = icon("oil", lib = "glyphicon"),
            color = "blue"
        )
    })
    
    output$batteryBox <- renderValueBox({
        valueBox(
            paste(
                dummy_data %>%
                    group_by(device_id) %>%
                    filter(field == input$field,
                           lot == input$lot,
                           date_time == max(date_time)) %>%
                    pull(battery_level),
                "%"
            ),
            "Ladestand Sensorbatterie",
            icon = icon("flash", lib = "glyphicon"),
            color = "lime"
        )
    })
    
    output$tempBox <- renderValueBox({
        valueBox(
            paste(
                dummy_data %>%
                    group_by(device_id) %>%
                    filter(field == input$field,
                           date_time == max(date_time)) %>%
                    summarise(mean = mean(temperature_celsius)) %>%
                    pull() %>%
                    round(),
                "°C"
            ),
            "Durchschnittstemp. der letzten Messung aller Sensoren eines Ackers",
            icon = icon("cloud", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$humidBox <- renderValueBox({
        valueBox(
            paste(
                dummy_data %>%
                    group_by(device_id) %>%
                    filter(field == input$field,
                           date_time == max(date_time)) %>%
                    summarise(mean = mean(humidity_percent)) %>%
                    pull() %>%
                    round(),
                "%"
            ),
            "Durchschnittsluftfeucht. der letzten Messung aller Sensoren eines Ackers",
            icon = icon("tint", lib = "glyphicon"),
            color = "aqua"
        )
    })
    
    ####################################
    
    
    #### dashboardbody_box_2 output ####
    
    output$plot <- renderPlotly({
        plotly_data <- dummy_data %>%
            select(field,
                   lot,
                   temperature_celsius,
                   humidity_percent,
                   date,
                   time) %>%
            filter(field == input$field,
                   lot == input$lot) %>%
            filter(date == max(date))
        
        current_date <- max(plotly_data$date)
        
        plot_ly(plotly_data, x = ~time, y = ~temperature_celsius, type = "bar", name = 'Temperature') %>%
            add_trace(x = ~time, y = ~humidity_percent, type = 'scatter', mode = 'lines', name = 'Humidity',
                      yaxis = 'y2',
                      hoverinfo = "text",
                      text = ~paste(humidity_percent, '%')) %>%
            layout(title = paste(current_date, 'Temperatur & Luftfeuchtigkeit'),
                   xaxis = list(title = ""),
                   yaxis = list(side = 'left', title = 'Temperatur [°C]', showgrid = FALSE, zeroline = FALSE, hoverformat = '.2f'),
                   yaxis2 = list(side = 'right', overlaying = "y", title = 'Luftfeuchtigkeit [%]', showgrid = FALSE, zeroline = FALSE, hoverformat = '.2f'))
    })
    
    ####################################
    
}

shinyApp(ui, server)