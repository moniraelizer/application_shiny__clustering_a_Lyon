
#
#

library(shiny)
library(sf)
library(dplyr)
library(mapview)
library(ggplot2)
library(dbscan)
library(rgdal) #crs
library(tidyr)
library(sp)
library(mapview)
library(RColorBrewer)
library(stringr)
library(tmap)

#" chargement des données "  

GLyonCom2 <- st_read(dsn="DATALyon/MaillesIris/ComArr2016GLyon.shp", 
                     stringsAsFactors = FALSE, 
                     options = "ENCODING=latin1")
GLyonCom2<-st_transform(GLyonCom2,2154)
GLyon<-st_union(GLyonCom2)

GLyonBPE_2 <- st_read(dsn="DATALyon/Equipements/GLyonBPE2.shp", 
                     stringsAsFactors = FALSE, 
                     options = "ENCODING=latin1")
st_crs(GLyonBPE_2)
GLyonBPE_2<-st_transform(GLyonBPE_2,2154)

# fonction du cluster
function_dbscan=function(equipement,eps, minPts){
  CommerceXY = GLyonBPE_2 %>%
    filter (lbGrndF == equipement) %>%
    st_coordinates()
  clust <- dbscan(CommerceXY, eps, minPts)
  return(clust)
}

# Configuration 

ui <- fluidPage(
  titlePanel("Cluster Par Type de Commerce dans la ville de Lyon"),
  
  wellPanel(
    
    # on utilise un widget select pour sélectionnerun type d'équipement 
    selectInput("equipement", "Choisir un type d'équipement:", choices = unique(GLyonBPE2$labGrandFam)),
    
    #les deux input numérique servent à la fonction qui nous permettra de construire le cluster
    
    numericInput("pNelt", "Choisir le minimum de points pour chaque cluster :", value=2,min=2,max=5),
    numericInput("pR", "Choisir le rayon :", value=750,min=100,max=1000)
  ), #wellpanel
  
  
  fluidRow(
    column(width=6,
           dataTableOutput("choix_commerce")
    ),#column
    column(width=6,
           tabsetPanel(
             tabPanel("Carte des cluster",
                        tmapOutput("Plot", height = 600)
                        ), # tabPannel2
             tabPanel("graphique des types d'équiepements",
                      plotOutput("displot")
                      ),
                      #tabPannel
           )#tabsPannel
      )#column
    )#fluidRow
)
           

# la partie server


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$choix_commerce <- renderDataTable({
    GLyonBPE_2 %>% 
      filter(lbGrndF==input$equipement)
  })
  
  output$Plot <- renderTmap({
    
    CommerceFilter =  GLyonBPE_2 %>% 
      filter (lbGrndF==input$equipement) %>%
      mutate (cluster = as.character(function_dbscan(input$equipement,
                                                     input$pR,
                                                     input$pNelt)$cluster)) %>%
      filter (cluster != "0")
    tmap_options(max.categories = 31)
    tmap_mode("view")
    tm_shape(CommerceFilter) +
      tm_symbols(col = "cluster")
    
  })
  
  output$displot <- renderPlot({

      ggplot(GLyonBPE_2) + geom_bar(aes(x = lbGrndF))
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)