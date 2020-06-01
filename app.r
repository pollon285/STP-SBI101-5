### libraries
library(shiny)
library(readxl)

### source functions
source("CreateQueryTable.R")
source("proms_fun.R")


qt <- CreatQueryTable("data/GeraldFile_200313.csv","data/FormsInfo_200313.csv","data/FormLabels_200313.csv")

### datasets
mapcombos<-qt[c("FTID", "PROM_TOOL", "DW_views", "Pathway")] #main data (FTIDs/PROM TOOLS/PATHWAYS/DW VIEWS)
colnames(mapcombos)[4]<-"PATHWAY" # just because I have used the capital letters in all the rest of the scripts

formnames <-qt[c("FTID", "Form_Title", "Collection method", "Tool", "Patient-friendly label (English)",
                 "Patient-friendly label (Welsh)", "Clinician-friendly label")] #additional data for PTID


# used to build the drop down menus
FTID_items <- sort(unique(mapcombos$FTID)) #this can be used as a list for dropdown menu, I can possibly try and remove the title of the column
PROM_TOOL_items <- sort(unique(mapcombos$PROM_TOOL)) 
PATHWAY_items <- sort(unique(mapcombos$PATHWAY))
DW_views_items <- sort(unique(mapcombos$DW_views))


### user interface
ui <- fluidPage(
  
  titlePanel(
    tags$h1(
      tags$em(
        tags$strong("Wales PROMs")
      ), 
      "Data Mapping Tool")
  ),
  tags$hr(),
  sidebarLayout(
    
    ###inputs 
    
    sidebarPanel(
      
      helpText(tags$h3("SELECTION TOOL")),
      
      tags$hr(),
      
      radioButtons("QUERY_CAT", 
                   label = "Choose query type",
                   choices = list( "FTID" = 1,
                                   "PROM Tool" = 2,
                                   "Pathway" = 3,
                                   "Data Warehouse View" = 4)
      ),
      
      tags$hr(),
      
      conditionalPanel (
        condition = "input.QUERY_CAT == 1",
        selectInput("FTID", 
                    label = "Choose FTID",
                    choices = FTID_items,
                    selected = NA
        )
      ),
      
      conditionalPanel (
        condition = "input.QUERY_CAT == 2",
        selectInput("PROM_TOOL", 
                    label = "Choose a PROM Tool",
                    choices = PROM_TOOL_items,
                    selected = NA
        )
      ),
      
      conditionalPanel (
        condition = "input.QUERY_CAT == 3",
        selectInput("PATHWAY", 
                    label = "Choose a Pathway",
                    choices = PATHWAY_items,
                    selected = NA
        )
      ),
      
      conditionalPanel (
        condition = "input.QUERY_CAT == 4",
        selectInput("DW_views", 
                    label = "Choose a Database View",
                    choices = DW_views_items,
                    selected = NA
        )
      ),
      
      #button for downloading a report
      downloadButton(outputId = "downloadReport", label = "Download report")
      
    ),
    
    
    
    mainPanel(
      
      ###outputs 
      
      htmlOutput("selected_ITEM"), #I want to use HTML formatting for this line
      
      tags$hr(),
      
      tableOutput("MainTable"),
      
      tableOutput("ExtraTable_FTID")
      
    )
    
    
  )
)

###server tool
server <- function(input, output) {

  
### REACTIVE OBJECTS ### 
### I need reactive objects to obtain an output that I can download 
  
  selected_ITEM_Data <- reactive(
    if (input$QUERY_CAT == 1){
      paste("You have selected <b>FTID</b> = ", tags$b(input$FTID)) 
    }
    
    else if (input$QUERY_CAT == 2){
      paste("You have selected <b>PROM Tool</b> = ", tags$b(input$PROM_TOOL))
    }
    
    else if (input$QUERY_CAT == 3){
      paste("You have selected <b>Pathway</b> = ", tags$b(input$PATHWAY))
    }
    
    else if (input$QUERY_CAT == 4){
      paste("You have selected <b>Data Warehouse View</b> = ", tags$b(input$DW_views))
    }
  )
  
  
  selected_ITEM_Data_mod <- reactive(
    
    gsub("<b>|</b>"," ", selected_ITEM_Data()) ##when it is saved as .csv, the html tags for bold need to disappear
    
  )
  
  ### (MainTableData can possibly go as unique argument in output$MainTable)
  MainTableData <- reactive(
    if (input$QUERY_CAT == 1){
      getmap(mapcombos,formnames,"FTID",input$FTID)[[1]]
    }
    
    else if (input$QUERY_CAT == 2){
      getmap(mapcombos,formnames,"PROM_TOOL",input$PROM_TOOL)
    }
    
    else if (input$QUERY_CAT == 3){
      getmap(mapcombos,formnames,"PATHWAY",input$PATHWAY)
    }
    
    else if (input$QUERY_CAT == 4){
      getmap(mapcombos,formnames,"DW_views",input$DW_views)
    }
    
  )
  
  ExtraTable_FTID_Data <- reactive(
    if (input$QUERY_CAT == 1){
      getmap(mapcombos,formnames,"FTID",input$FTID)[[2]]
    }
  )
  
  
  #before it can be downloaded, the table needs to be rewritten removing html tags 
  #(the newlines have been coded as <br>) on the app
  MainTableData_mod<- reactive(  
    data.frame(lapply(MainTableData(), function(x){
      gsub("<br>"," // ", x)  ##when it is saved as .csv, the html tags for break element need to disappear
    })
    
    )
    
    
  )
  
  
  ### FUNCTIONS CONNECTING TO THE UI
  output$selected_ITEM <- renderText({ 
    
    selected_ITEM_Data()    
    
  })
  
  
  output$MainTable <- renderTable({
    
    MainTableData()
    
  }, sanitize.text.function=identity) #this is to have multiple line in the same dataframe cell
  
  output$ExtraTable_FTID <- renderTable({
    
    ExtraTable_FTID_Data()

  })
  
  
  ### DOWNLOAD the selected query as a .csv file ###
  output$downloadReport <- downloadHandler(
    
    # specify the filename
    filename = function() {
      "ReportProms.csv" #to save things as a csv file
    },
    
    content = function(file){
      
      write.table(selected_ITEM_Data_mod(), file, sep = ',', row.names = FALSE, col.names=FALSE) #to save things as a csv file   
      write.table(NULL, file, sep = ',', row.names = FALSE, append= TRUE)
      write.table(MainTableData_mod(), file, sep = ',', row.names = FALSE, append = TRUE) #to save things as a csv file
      write.table(NULL, file, sep = ',', row.names = FALSE, append= TRUE)
      write.table(ExtraTable_FTID_Data(), file, sep = ',', row.names = FALSE, append = TRUE) #to save things as a csv file
      
    } 
    
  )
  
  
}

### shiny function to connect ui and server
shinyApp(ui = ui, server = server)
