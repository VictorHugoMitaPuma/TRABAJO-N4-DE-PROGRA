library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(sp)
#solo funciona con data de terremotos
ui <- fluidPage(
  titlePanel("TAREA 4"),
  sidebarLayout(
    sidebarPanel(
      fileInput("ARCHIVO", "Ingresar CSV Aqui",
                multiple = FALSE,
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
      ),
      tags$hr(),
      h5(helpText("Seleccione los parametros:")),
      checkboxInput(inputId = 'header', 'Header', T),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", T),
      br(),
      radioButtons("sep", "Separa por:",
                   choices = c(Coma = ",",
                               puntoyComa = ";",
                               Tab = "\t",
                               espacio=''),
                   selected = ","),
      radioButtons("quote", "Cita",
                   choices = c(Ninguna = "",
                               "cita doble" = '"',
                               "cita simple" = "'"),
                   selected = '"')
    ),
    mainPanel(uiOutput("tb"))
  )
)

server <- function(input,output){
  data <- reactive({
    file <- input$ARCHIVO
    if(is.null(file)){return()} 
    read.table(file=file$datapath,
               sep=input$sep,
               header = input$header,
               stringsAsFactors = input$stringAsFactors)
  })
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$ARCHIVO
  })
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
  })
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  output$map<- renderLeaflet({
    MARCO <- read.csv(input$ARCHIVO$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    mapa<-leaflet() %>% 
      addCircles(data = MARCO, lat = MARCO$Latitude, lng = MARCO$Longitude)%>%
      addTiles()
  })
  output$plot <- renderPlot({
    MARCO1 <- read.csv(input$ARCHIVO$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    sf <- st_as_sf(MARCO1,coords = c("Longitude", "Latitude"),
                   crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    plot(sf)
  })
  output$tb <- renderUI({
    if(is.null(data()))
      h5("desarrollado con", tags$img(src='RStudio-Ball.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("Origen", tableOutput("filedf")),
                  tabPanel("Datos", tableOutput("table")),
                  tabPanel("Resumen", tableOutput("sum")),
                  tabPanel("Mapa", leafletOutput("map")),
                  tabPanel("Objeto sf", plotOutput("plot"))
      )
  })
  
}

shinyApp(ui = ui, server = server)
