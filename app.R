###*initiate library####
library(shiny)
library(shinydashboard)

source("functional_capacity.R")

outputDir <- "responses"
  
# Which fields get saved
fieldsSystem <- c("sys11q01","sys12q01","sys21q01","sys22q01","sys23q01","sys24q01","sys25q01","sys25q02","sys31q01","sys32q01","sys33q01","sys34q01","sys35q01",
                "sys71q01","sys71q02","sys71q03","sys71q04","sys71q05","sys71q06","sys71q07","sys71q08","sys71q09","sys71q10","sys71q11","sys71q12","sys71q13","sys71q14","sys71q15","sys71q16","sys71q17","sys71q18","sys71q19",
                "sys72q01","sys72q02","sys72q03","sys72q04","sys72q05","sys72q06","sys72q07","sys72q08","sys72q09","sys72q10","sys72q11","sys72q12","sys72q13","sys72q14","sys72q15","sys72q16","sys72q17","sys72q18","sys72q19",
                "sys73q01","sys73q02","sys91q01","sys91q02","sys91q03","sys91q04","sys91q05","sys92q01","sys92q02","sys92q03","sys92q04","sys92q05","sys93q01","sys93q02","sys93q03")
fieldsOrganisation <- c("org41q01","org41q02","org42q01","org42q02","org42q03","org43q01","org43q02","org44q01","org44q02","org44q03","org44q04",
                      "org45q01","org45q02","org45q03","org46q01","org46q02","org47q01","org47q02","org47q03","org47q04","org47q05","org47q06",
                      "org47q07","org51q01","org51q02","org51q03","org51q04","org51q05","org51q06","org51q07","org52q01","org53q01","org54q01",
                      "org54q02","org55q01","org55q02","org81q01","org81q02","org81q03","org81q04","org82q01","org82q02","org82q03","org83q01","org83q02")
fieldsIndividu <- c("ind61q01","ind61q02",
                  "ind62q01","ind62q02","ind62q03","ind62q04","ind62q05","ind62q06","ind62q07","ind62q08","ind62q09",
                  "ind63q01","ind63q02","ind63q03","ind63q04","ind63q05","ind63q06","ind63q07","ind63q08","ind63q09","ind64q01","ind64q02","ind64q03")

saveData <- function(data) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(x = data, file = file.path(outputDir, fileName), row.names = FALSE)
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}


###*setup dashboard page####
source('interface.R')

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  tablesCDA <- reactiveValues(tableSystem=data.frame(), tableOrganisation=data.frame(), tableIndividu=data.frame())
  
  formSystem <- reactive({
    data <- sapply(fieldsSystem, function(x) input[[x]])
    data <- t(data)
    data
  })
  
  formOrganisation <- reactive({
    data <- sapply(fieldsOrganisation, function(x) input[[x]])
    data <- t(data)
    data
  })
  
  formIndividu <- reactive({
    data <- sapply(fieldsIndividu, function(x) input[[x]])
    data <- t(data)
    data
  })
  
  output$rateTbl <- renderDataTable({
    desc <- c("REGULASI", "INTEGRASI DALAM PERENCANAAN PEMBANGUNAN DAERAH", "PROSES", "ORGANISASI", "SDM", "DATA DAN INFORMASI", "PEMANTAUAN EVALUASI DAN PELAPORAN")

    tblSystem <- tablesCDA$tableSystem

    # regulasi
    regulasi <- mean(c(tblSystem[1, 1], tblSystem[1, 2]))
    # integrasi dlm perencanaan pembangunan daerah
    integrasi <- mean(c(tblSystem[1, 3], tblSystem[1, 4], tblSystem[1, 5], tblSystem[1, 6], mean(tblSystem[1, 7], tblSystem[1, 8]) ))
    # proses
    proses <- mean(c(unlist(tblSystem[1, 9:13])))
    # organisasi
    organisasi <- 0
    # sdm 
    sdm <- 0
    # data dan informasi
    informasi <- mean(c(mean(c(unlist(tblSystem[1, 14:32]))), 
                        mean(c(unlist(tblSystem[1, 33:51]))),
                        mean(c(tblSystem[1, 52], tblSystem[1, 53]))
                        ))
    # pep
    pep <- mean(c(mean(c(unlist(tblSystem[1, 54:58]))),
                  mean(c(unlist(tblSystem[1, 59:63]))),
                  mean(c(unlist(tblSystem[1, 64:66])))
                  ))

    level <- c(regulasi, integrasi, proses, organisasi, sdm, informasi, pep)
    gap <- c(5, 5, 5, 5, 5, 5, 5)

    summary <- data.frame(SUMMARY=desc, LEVEL=level, GAP=gap)
    summary
  })
  
  # Append data
  observeEvent(input$submitSys, {
    tbl <- formSystem()
    tablesCDA$tableSystem <- rbind(tablesCDA$tableSystem, tbl)
  })
  
  observeEvent(input$submitOrg, {
    tbl <- formOrganisation()
    tablesCDA$tableOrganisation <- rbind(tablesCDA$tableOrganisation, tbl)
  })
  
  observeEvent(input$submitInd, {
    tbl <- formIndividu()
    tablesCDA$tableIndividu <- rbind(tablesCDA$tableIndividu, tbl)
  })
  
  # Export data
  observeEvent(input$exportInd, {
    saveData(tablesCDA$tableIndividu)
  })
}

###*run the apps#### 
shinyApp(ui = ui, server = server)

