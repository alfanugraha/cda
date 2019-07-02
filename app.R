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

###*header####
header <- dashboardHeader(title="CDNA", titleWidth = "500px")

###*sidebar####
sidebar <- dashboardSidebar(width = "500px",
  sidebarMenu(
    menuItem("Home", icon = icon("home"), tabName = "home"),
    ###sidebar-historis####
    menuItem("Sistem",  #icon = icon("history"), 
      menuSubItem(system_func_capacity$func_capacity[1], tabName = "sys11"),
      menuSubItem(system_func_capacity$func_capacity[2], tabName = "sys12"),
      menuSubItem(system_func_capacity$func_capacity[3], tabName = "sys21"),
      menuSubItem(system_func_capacity$func_capacity[4], tabName = "sys22"),
      menuSubItem(system_func_capacity$func_capacity[5], tabName = "sys23"),
      menuSubItem(system_func_capacity$func_capacity[6], tabName = "sys24"),
      menuSubItem(system_func_capacity$func_capacity[7], tabName = "sys25"),
      menuSubItem(system_func_capacity$func_capacity[8], tabName = "sys31"),
      menuSubItem(system_func_capacity$func_capacity[9], tabName = "sys32"),
      menuSubItem(system_func_capacity$func_capacity[10], tabName = "sys33"),
      menuSubItem(system_func_capacity$func_capacity[11], tabName = "sys34"),
      menuSubItem(system_func_capacity$func_capacity[12], tabName = "sys35"),
      menuSubItem(system_func_capacity$func_capacity[13], tabName = "sys71"),
      menuSubItem(system_func_capacity$func_capacity[14], tabName = "sys72"),
      menuSubItem(system_func_capacity$func_capacity[15], tabName = "sys73"),
      menuSubItem(system_func_capacity$func_capacity[16], tabName = "sys91"),
      menuSubItem(system_func_capacity$func_capacity[17], tabName = "sys92"),
      menuSubItem(system_func_capacity$func_capacity[18], tabName = "sys93"), 
      actionButton("submitSys", "Submit")
    ),
    ###sidebar-bau####
    menuItem("Organisasi", #icon = icon("exchange"), 
      menuSubItem(organisation_func_capacity$func_capacity[1], tabName = "org41"),
      menuSubItem(organisation_func_capacity$func_capacity[2], tabName = "org42"),
      menuSubItem(organisation_func_capacity$func_capacity[3], tabName = "org43"),
      menuSubItem(organisation_func_capacity$func_capacity[4], tabName = "org44"),
      menuSubItem(organisation_func_capacity$func_capacity[5], tabName = "org45"),
      menuSubItem(organisation_func_capacity$func_capacity[6], tabName = "org46"),
      menuSubItem(organisation_func_capacity$func_capacity[7], tabName = "org47"),
      menuSubItem(organisation_func_capacity$func_capacity[8], tabName = "org51"),
      menuSubItem(organisation_func_capacity$func_capacity[9], tabName = "org52"),
      menuSubItem(organisation_func_capacity$func_capacity[10], tabName = "org53"),
      menuSubItem(organisation_func_capacity$func_capacity[11], tabName = "org54"),
      menuSubItem(organisation_func_capacity$func_capacity[12], tabName = "org55"),
      menuSubItem(organisation_func_capacity$func_capacity[13], tabName = "org81"),
      menuSubItem(organisation_func_capacity$func_capacity[14], tabName = "org82"),
      menuSubItem(organisation_func_capacity$func_capacity[15], tabName = "org83"), 
      actionButton("submitOrg", "Submit")
    ),
    ###sidebar-intervention####
    menuItem("Individu",# icon = icon("random"), 
      menuSubItem(individu_func_capacity$func_capacity[1], tabName = "ind61"),
      menuSubItem(individu_func_capacity$func_capacity[2], tabName = "ind62"),
      menuSubItem(individu_func_capacity$func_capacity[3], tabName = "ind63"),
      menuSubItem(individu_func_capacity$func_capacity[4], tabName = "ind64"), 
      actionButton("submitInd", "Submit"),
      actionButton("exportInd", "Export")
    ),
    menuItem("Help", icon = icon("question-circle"), tabName="help")
  )
)

###*body####
body <- dashboardBody(
  tabItems(
    ###*tab-home####
    tabItem(tabName = "home"),
    ###*tab-system####=
    tabItem(tabName = "sys11",
      h3(cda_system$Indikator[1]),
      radioButtons("sys11q01", label = h4(cda_system$Pertanyaan[1]), choiceNames = list(as.character(cda_system$L1[1]), as.character(cda_system$L2[1]), as.character(cda_system$L3[1]), as.character(cda_system$L4[1]), as.character(cda_system$L5[1])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys12",
      h3(cda_system$Indikator[2]),
      radioButtons("sys12q01", label = h4(cda_system$Pertanyaan[2]), choiceNames = list(as.character(cda_system$L1[2]), as.character(cda_system$L2[2]), as.character(cda_system$L3[2]), as.character(cda_system$L4[2]), as.character(cda_system$L5[2])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys21",
      h3(cda_system$Indikator[3]),
      radioButtons("sys21q01", label = h4(cda_system$Pertanyaan[3]), choiceNames = list(as.character(cda_system$L1[3]), as.character(cda_system$L2[3]), as.character(cda_system$L3[3]), as.character(cda_system$L4[3]), as.character(cda_system$L5[3])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys22",
      h3(cda_system$Indikator[4]),
      radioButtons("sys22q01", label = h4(cda_system$Pertanyaan[4]), choiceNames = list(as.character(cda_system$L1[4]), as.character(cda_system$L2[4]), as.character(cda_system$L3[4]), as.character(cda_system$L4[4]), as.character(cda_system$L5[4])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys23",
      h3(cda_system$Indikator[5]),
      radioButtons("sys23q01", label = h4(cda_system$Pertanyaan[5]), choiceNames = list(as.character(cda_system$L1[5]), as.character(cda_system$L2[5]), as.character(cda_system$L3[5]), as.character(cda_system$L4[6]), as.character(cda_system$L5[5])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys24",
      h3(cda_system$Indikator[6]),
      radioButtons("sys24q01", label = h4(cda_system$Pertanyaan[6]), choiceNames = list(as.character(cda_system$L1[6]), as.character(cda_system$L2[6]), as.character(cda_system$L3[6]), as.character(cda_system$L4[6]), as.character(cda_system$L5[6])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")            
    ),
    tabItem(tabName = "sys25",
      h3(cda_system$Indikator[7]),
      radioButtons("sys25q01", label = h4(cda_system$Pertanyaan[7]), choiceNames = list(as.character(cda_system$L1[7]), as.character(cda_system$L2[7]), as.character(cda_system$L3[7]), as.character(cda_system$L4[7]), as.character(cda_system$L5[7])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[8]),
      radioButtons("sys25q02", label = h4(cda_system$Pertanyaan[8]), choiceNames = list(as.character(cda_system$L1[8]), as.character(cda_system$L2[8]), as.character(cda_system$L3[8]), as.character(cda_system$L4[8]), as.character(cda_system$L5[8])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys31",
      h3(cda_system$Indikator[9]),
      radioButtons("sys31q01", label = h4(cda_system$Pertanyaan[9]), choiceNames = list(as.character(cda_system$L1[9]), as.character(cda_system$L2[9]), as.character(cda_system$L3[9]), as.character(cda_system$L4[9]), as.character(cda_system$L5[9])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys32",
      h3(cda_system$Indikator[10]),
      radioButtons("sys32q01", label = h4(cda_system$Pertanyaan[10]), choiceNames = list(as.character(cda_system$L1[10]), as.character(cda_system$L2[10]), as.character(cda_system$L3[10]), as.character(cda_system$L4[10]), as.character(cda_system$L5[10])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys33",
      h3(cda_system$Indikator[11]),
      radioButtons("sys33q01", label = h4(cda_system$Pertanyaan[11]), choiceNames = list(as.character(cda_system$L1[11]), as.character(cda_system$L2[11]), as.character(cda_system$L3[11]), as.character(cda_system$L4[11]), as.character(cda_system$L5[11])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys34",
      h3(cda_system$Indikator[12]),
      radioButtons("sys34q01", label = h4(cda_system$Pertanyaan[12]), choiceNames = list(as.character(cda_system$L1[12]), as.character(cda_system$L2[12]), as.character(cda_system$L3[12]), as.character(cda_system$L4[12]), as.character(cda_system$L5[12])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys35",
      h3(cda_system$Indikator[13]),
      radioButtons("sys35q01", label = h4(cda_system$Pertanyaan[13]), choiceNames = list(as.character(cda_system$L1[13]), as.character(cda_system$L2[13]), as.character(cda_system$L3[13]), as.character(cda_system$L4[13]), as.character(cda_system$L5[13])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys71",
      h3(cda_system$Indikator[14]), radioButtons("sys71q01", label = h4(cda_system$Pertanyaan[14]), choiceNames = list(as.character(cda_system$L1[14]), as.character(cda_system$L2[14]), as.character(cda_system$L3[14]), as.character(cda_system$L4[14]), as.character(cda_system$L5[14])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[15]), radioButtons("sys71q02", label = h4(cda_system$Pertanyaan[15]), choiceNames = list(as.character(cda_system$L1[15]), as.character(cda_system$L2[15]), as.character(cda_system$L3[15]), as.character(cda_system$L4[15]), as.character(cda_system$L5[15])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[16]), radioButtons("sys71q03", label = h4(cda_system$Pertanyaan[16]), choiceNames = list(as.character(cda_system$L1[16]), as.character(cda_system$L2[16]), as.character(cda_system$L3[16]), as.character(cda_system$L4[16]), as.character(cda_system$L5[16])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[17]), radioButtons("sys71q04", label = h4(cda_system$Pertanyaan[17]), choiceNames = list(as.character(cda_system$L1[17]), as.character(cda_system$L2[17]), as.character(cda_system$L3[17]), as.character(cda_system$L4[17]), as.character(cda_system$L5[17])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[18]), radioButtons("sys71q05", label = h4(cda_system$Pertanyaan[18]), choiceNames = list(as.character(cda_system$L1[18]), as.character(cda_system$L2[18]), as.character(cda_system$L3[18]), as.character(cda_system$L4[18]), as.character(cda_system$L5[18])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[19]), radioButtons("sys71q06", label = h4(cda_system$Pertanyaan[19]), choiceNames = list(as.character(cda_system$L1[19]), as.character(cda_system$L2[19]), as.character(cda_system$L3[19]), as.character(cda_system$L4[19]), as.character(cda_system$L5[19])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[20]), radioButtons("sys71q07", label = h4(cda_system$Pertanyaan[20]), choiceNames = list(as.character(cda_system$L1[20]), as.character(cda_system$L2[20]), as.character(cda_system$L3[20]), as.character(cda_system$L4[20]), as.character(cda_system$L5[20])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[21]), radioButtons("sys71q08", label = h4(cda_system$Pertanyaan[21]), choiceNames = list(as.character(cda_system$L1[21]), as.character(cda_system$L2[21]), as.character(cda_system$L3[21]), as.character(cda_system$L4[21]), as.character(cda_system$L5[21])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[22]), radioButtons("sys71q09", label = h4(cda_system$Pertanyaan[22]), choiceNames = list(as.character(cda_system$L1[22]), as.character(cda_system$L2[22]), as.character(cda_system$L3[22]), as.character(cda_system$L4[22]), as.character(cda_system$L5[22])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[23]), radioButtons("sys71q10", label = h4(cda_system$Pertanyaan[23]), choiceNames = list(as.character(cda_system$L1[23]), as.character(cda_system$L2[23]), as.character(cda_system$L3[23]), as.character(cda_system$L4[23]), as.character(cda_system$L5[23])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[24]), radioButtons("sys71q11", label = h4(cda_system$Pertanyaan[24]), choiceNames = list(as.character(cda_system$L1[24]), as.character(cda_system$L2[24]), as.character(cda_system$L3[24]), as.character(cda_system$L4[24]), as.character(cda_system$L5[24])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[25]), radioButtons("sys71q12", label = h4(cda_system$Pertanyaan[25]), choiceNames = list(as.character(cda_system$L1[25]), as.character(cda_system$L2[25]), as.character(cda_system$L3[25]), as.character(cda_system$L4[25]), as.character(cda_system$L5[25])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[26]), radioButtons("sys71q13", label = h4(cda_system$Pertanyaan[26]), choiceNames = list(as.character(cda_system$L1[26]), as.character(cda_system$L2[26]), as.character(cda_system$L3[26]), as.character(cda_system$L4[26]), as.character(cda_system$L5[26])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[27]), radioButtons("sys71q14", label = h4(cda_system$Pertanyaan[27]), choiceNames = list(as.character(cda_system$L1[27]), as.character(cda_system$L2[27]), as.character(cda_system$L3[27]), as.character(cda_system$L4[27]), as.character(cda_system$L5[27])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[28]), radioButtons("sys71q15", label = h4(cda_system$Pertanyaan[28]), choiceNames = list(as.character(cda_system$L1[28]), as.character(cda_system$L2[28]), as.character(cda_system$L3[28]), as.character(cda_system$L4[28]), as.character(cda_system$L5[28])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[29]), radioButtons("sys71q16", label = h4(cda_system$Pertanyaan[29]), choiceNames = list(as.character(cda_system$L1[29]), as.character(cda_system$L2[29]), as.character(cda_system$L3[29]), as.character(cda_system$L4[29]), as.character(cda_system$L5[29])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[30]), radioButtons("sys71q17", label = h4(cda_system$Pertanyaan[30]), choiceNames = list(as.character(cda_system$L1[30]), as.character(cda_system$L2[30]), as.character(cda_system$L3[30]), as.character(cda_system$L4[30]), as.character(cda_system$L5[30])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[31]), radioButtons("sys71q18", label = h4(cda_system$Pertanyaan[31]), choiceNames = list(as.character(cda_system$L1[31]), as.character(cda_system$L2[31]), as.character(cda_system$L3[31]), as.character(cda_system$L4[31]), as.character(cda_system$L5[31])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),  
      h3(cda_system$Indikator[32]), radioButtons("sys71q19", label = h4(cda_system$Pertanyaan[32]), choiceNames = list(as.character(cda_system$L1[32]), as.character(cda_system$L2[32]), as.character(cda_system$L3[32]), as.character(cda_system$L4[32]), as.character(cda_system$L5[32])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")            
    ),
    tabItem(tabName = "sys72",
      h3(cda_system$Indikator[33]), radioButtons("sys72q01", label = h4(cda_system$Pertanyaan[33]), choiceNames = list(as.character(cda_system$L1[33]), as.character(cda_system$L2[33]), as.character(cda_system$L3[33]), as.character(cda_system$L4[33]), as.character(cda_system$L5[33])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[34]), radioButtons("sys72q02", label = h4(cda_system$Pertanyaan[34]), choiceNames = list(as.character(cda_system$L1[34]), as.character(cda_system$L2[34]), as.character(cda_system$L3[34]), as.character(cda_system$L4[34]), as.character(cda_system$L5[34])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[35]), radioButtons("sys72q03", label = h4(cda_system$Pertanyaan[35]), choiceNames = list(as.character(cda_system$L1[35]), as.character(cda_system$L2[35]), as.character(cda_system$L3[35]), as.character(cda_system$L4[35]), as.character(cda_system$L5[35])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[36]), radioButtons("sys72q04", label = h4(cda_system$Pertanyaan[36]), choiceNames = list(as.character(cda_system$L1[36]), as.character(cda_system$L2[36]), as.character(cda_system$L3[36]), as.character(cda_system$L4[36]), as.character(cda_system$L5[36])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[37]), radioButtons("sys72q05", label = h4(cda_system$Pertanyaan[37]), choiceNames = list(as.character(cda_system$L1[37]), as.character(cda_system$L2[37]), as.character(cda_system$L3[37]), as.character(cda_system$L4[37]), as.character(cda_system$L5[37])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[38]), radioButtons("sys72q06", label = h4(cda_system$Pertanyaan[38]), choiceNames = list(as.character(cda_system$L1[38]), as.character(cda_system$L2[38]), as.character(cda_system$L3[38]), as.character(cda_system$L4[38]), as.character(cda_system$L5[38])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[39]), radioButtons("sys72q07", label = h4(cda_system$Pertanyaan[39]), choiceNames = list(as.character(cda_system$L1[39]), as.character(cda_system$L2[39]), as.character(cda_system$L3[39]), as.character(cda_system$L4[39]), as.character(cda_system$L5[39])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[40]), radioButtons("sys72q08", label = h4(cda_system$Pertanyaan[40]), choiceNames = list(as.character(cda_system$L1[40]), as.character(cda_system$L2[40]), as.character(cda_system$L3[40]), as.character(cda_system$L4[40]), as.character(cda_system$L5[40])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[41]), radioButtons("sys72q09", label = h4(cda_system$Pertanyaan[41]), choiceNames = list(as.character(cda_system$L1[41]), as.character(cda_system$L2[41]), as.character(cda_system$L3[41]), as.character(cda_system$L4[41]), as.character(cda_system$L5[41])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[42]), radioButtons("sys72q10", label = h4(cda_system$Pertanyaan[42]), choiceNames = list(as.character(cda_system$L1[42]), as.character(cda_system$L2[42]), as.character(cda_system$L3[42]), as.character(cda_system$L4[42]), as.character(cda_system$L5[42])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[43]), radioButtons("sys72q11", label = h4(cda_system$Pertanyaan[43]), choiceNames = list(as.character(cda_system$L1[43]), as.character(cda_system$L2[43]), as.character(cda_system$L3[43]), as.character(cda_system$L4[43]), as.character(cda_system$L5[43])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[44]), radioButtons("sys72q12", label = h4(cda_system$Pertanyaan[44]), choiceNames = list(as.character(cda_system$L1[44]), as.character(cda_system$L2[44]), as.character(cda_system$L3[44]), as.character(cda_system$L4[44]), as.character(cda_system$L5[44])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[45]), radioButtons("sys72q13", label = h4(cda_system$Pertanyaan[45]), choiceNames = list(as.character(cda_system$L1[45]), as.character(cda_system$L2[45]), as.character(cda_system$L3[45]), as.character(cda_system$L4[45]), as.character(cda_system$L5[45])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[46]), radioButtons("sys72q14", label = h4(cda_system$Pertanyaan[46]), choiceNames = list(as.character(cda_system$L1[46]), as.character(cda_system$L2[46]), as.character(cda_system$L3[46]), as.character(cda_system$L4[46]), as.character(cda_system$L5[46])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[47]), radioButtons("sys72q15", label = h4(cda_system$Pertanyaan[47]), choiceNames = list(as.character(cda_system$L1[47]), as.character(cda_system$L2[47]), as.character(cda_system$L3[47]), as.character(cda_system$L4[47]), as.character(cda_system$L5[47])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[48]), radioButtons("sys72q16", label = h4(cda_system$Pertanyaan[48]), choiceNames = list(as.character(cda_system$L1[48]), as.character(cda_system$L2[48]), as.character(cda_system$L3[48]), as.character(cda_system$L4[48]), as.character(cda_system$L5[48])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[49]), radioButtons("sys72q17", label = h4(cda_system$Pertanyaan[49]), choiceNames = list(as.character(cda_system$L1[49]), as.character(cda_system$L2[49]), as.character(cda_system$L3[49]), as.character(cda_system$L4[49]), as.character(cda_system$L5[49])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[50]), radioButtons("sys72q18", label = h4(cda_system$Pertanyaan[50]), choiceNames = list(as.character(cda_system$L1[50]), as.character(cda_system$L2[50]), as.character(cda_system$L3[50]), as.character(cda_system$L4[50]), as.character(cda_system$L5[50])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),  
      h3(cda_system$Indikator[51]), radioButtons("sys72q19", label = h4(cda_system$Pertanyaan[51]), choiceNames = list(as.character(cda_system$L1[51]), as.character(cda_system$L2[51]), as.character(cda_system$L3[51]), as.character(cda_system$L4[51]), as.character(cda_system$L5[51])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys73",
      h3(cda_system$Indikator[52]),
      radioButtons("sys73q01", label = h4(cda_system$Pertanyaan[52]), choiceNames = list(as.character(cda_system$L1[52]), as.character(cda_system$L2[52]), as.character(cda_system$L3[52]), as.character(cda_system$L4[52]), as.character(cda_system$L5[52])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[53]),
      radioButtons("sys73q02", label = h4(cda_system$Pertanyaan[53]), choiceNames = list(as.character(cda_system$L1[53]), as.character(cda_system$L2[53]), as.character(cda_system$L3[53]), as.character(cda_system$L4[53]), as.character(cda_system$L5[53])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys91",
      h3(cda_system$Indikator[54]), radioButtons("sys91q01", label = h4(cda_system$Pertanyaan[54]), choiceNames = list(as.character(cda_system$L1[54]), as.character(cda_system$L2[54]), as.character(cda_system$L3[54]), as.character(cda_system$L4[54]), as.character(cda_system$L5[54])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[55]), radioButtons("sys91q02", label = h4(cda_system$Pertanyaan[55]), choiceNames = list(as.character(cda_system$L1[55]), as.character(cda_system$L2[55]), as.character(cda_system$L3[55]), as.character(cda_system$L4[55]), as.character(cda_system$L5[55])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[56]), radioButtons("sys91q03", label = h4(cda_system$Pertanyaan[56]), choiceNames = list(as.character(cda_system$L1[56]), as.character(cda_system$L2[56]), as.character(cda_system$L3[56]), as.character(cda_system$L4[56]), as.character(cda_system$L5[56])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[57]), radioButtons("sys91q04", label = h4(cda_system$Pertanyaan[57]), choiceNames = list(as.character(cda_system$L1[57]), as.character(cda_system$L2[57]), as.character(cda_system$L3[57]), as.character(cda_system$L4[57]), as.character(cda_system$L5[57])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),  
      h3(cda_system$Indikator[58]), radioButtons("sys91q05", label = h4(cda_system$Pertanyaan[58]), choiceNames = list(as.character(cda_system$L1[58]), as.character(cda_system$L2[58]), as.character(cda_system$L3[58]), as.character(cda_system$L4[58]), as.character(cda_system$L5[58])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys92",
      h3(cda_system$Indikator[59]), radioButtons("sys92q01", label = h4(cda_system$Pertanyaan[59]), choiceNames = list(as.character(cda_system$L1[59]), as.character(cda_system$L2[59]), as.character(cda_system$L3[59]), as.character(cda_system$L4[59]), as.character(cda_system$L5[59])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[60]), radioButtons("sys92q02", label = h4(cda_system$Pertanyaan[60]), choiceNames = list(as.character(cda_system$L1[60]), as.character(cda_system$L2[60]), as.character(cda_system$L3[60]), as.character(cda_system$L4[60]), as.character(cda_system$L5[60])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[61]), radioButtons("sys92q03", label = h4(cda_system$Pertanyaan[61]), choiceNames = list(as.character(cda_system$L1[61]), as.character(cda_system$L2[61]), as.character(cda_system$L3[61]), as.character(cda_system$L4[61]), as.character(cda_system$L5[61])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[62]), radioButtons("sys92q04", label = h4(cda_system$Pertanyaan[62]), choiceNames = list(as.character(cda_system$L1[62]), as.character(cda_system$L2[62]), as.character(cda_system$L3[62]), as.character(cda_system$L4[62]), as.character(cda_system$L5[62])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),  
      h3(cda_system$Indikator[63]), radioButtons("sys92q05", label = h4(cda_system$Pertanyaan[63]), choiceNames = list(as.character(cda_system$L1[63]), as.character(cda_system$L2[63]), as.character(cda_system$L3[63]), as.character(cda_system$L4[63]), as.character(cda_system$L5[63])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "sys93",
      h3(cda_system$Indikator[64]), radioButtons("sys93q01", label = h4(cda_system$Pertanyaan[64]), choiceNames = list(as.character(cda_system$L1[64]), as.character(cda_system$L2[64]), as.character(cda_system$L3[64]), as.character(cda_system$L4[64]), as.character(cda_system$L5[64])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_system$Indikator[65]), radioButtons("sys93q02", label = h4(cda_system$Pertanyaan[65]), choiceNames = list(as.character(cda_system$L1[65]), as.character(cda_system$L2[65]), as.character(cda_system$L3[65]), as.character(cda_system$L4[65]), as.character(cda_system$L5[65])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),  
      h3(cda_system$Indikator[66]), radioButtons("sys93q03", label = h4(cda_system$Pertanyaan[66]), choiceNames = list(as.character(cda_system$L1[66]), as.character(cda_system$L2[66]), as.character(cda_system$L3[66]), as.character(cda_system$L4[66]), as.character(cda_system$L5[66])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    
    ###*tab-organisation####
    tabItem(tabName = "org41",
      h3(cda_organisation$Indikator[1]),
      radioButtons("org41q01", label = h4(cda_organisation$Pertanyaan[1]), choiceNames = list(as.character(cda_organisation$L1[1]), as.character(cda_organisation$L2[1]), as.character(cda_organisation$L3[1]), as.character(cda_organisation$L4[1]), as.character(cda_organisation$L5[1])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[2]),
      radioButtons("org41q02", label = h4(cda_organisation$Pertanyaan[2]), choiceNames = list(as.character(cda_organisation$L1[2]), as.character(cda_organisation$L2[2]), as.character(cda_organisation$L3[2]), as.character(cda_organisation$L4[2]), as.character(cda_organisation$L5[2])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org42", 
      h3(cda_organisation$Indikator[3]),
      radioButtons("org42q01", label = h4(cda_organisation$Pertanyaan[3]), choiceNames = list(as.character(cda_organisation$L1[3]), as.character(cda_organisation$L2[3]), as.character(cda_organisation$L3[3]), as.character(cda_organisation$L4[3]), as.character(cda_organisation$L5[3])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[4]),
      radioButtons("org42q02", label = h4(cda_organisation$Pertanyaan[4]), choiceNames = list(as.character(cda_organisation$L1[4]), as.character(cda_organisation$L2[4]), as.character(cda_organisation$L3[4]), as.character(cda_organisation$L4[4]), as.character(cda_organisation$L5[4])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[5]),
      radioButtons("org42q03", label = h4(cda_organisation$Pertanyaan[5]), choiceNames = list(as.character(cda_organisation$L1[5]), as.character(cda_organisation$L2[5]), as.character(cda_organisation$L3[5]), as.character(cda_organisation$L4[5]), as.character(cda_organisation$L5[5])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org43",
      h3(cda_organisation$Indikator[6]),
      radioButtons("org43q01", label = h4(cda_organisation$Pertanyaan[6]), choiceNames = list(as.character(cda_organisation$L1[6]), as.character(cda_organisation$L2[6]), as.character(cda_organisation$L3[6]), as.character(cda_organisation$L4[6]), as.character(cda_organisation$L5[6])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[7]),
      radioButtons("org43q02", label = h4(cda_organisation$Pertanyaan[7]), choiceNames = list(as.character(cda_organisation$L1[7]), as.character(cda_organisation$L2[7]), as.character(cda_organisation$L3[7]), as.character(cda_organisation$L4[7]), as.character(cda_organisation$L5[7])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org44",
      h3(cda_organisation$Indikator[8]),
      radioButtons("org44q01", label = h4(cda_organisation$Pertanyaan[8]), choiceNames = list(as.character(cda_organisation$L1[8]), as.character(cda_organisation$L2[8]), as.character(cda_organisation$L3[8]), as.character(cda_organisation$L4[8]), as.character(cda_organisation$L5[8])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[9]),
      radioButtons("org44q02", label = h4(cda_organisation$Pertanyaan[9]), choiceNames = list(as.character(cda_organisation$L1[9]), as.character(cda_organisation$L2[9]), as.character(cda_organisation$L3[9]), as.character(cda_organisation$L4[9]), as.character(cda_organisation$L5[9])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[10]),
      radioButtons("org44q03", label = h4(cda_organisation$Pertanyaan[10]), choiceNames = list(as.character(cda_organisation$L1[10]), as.character(cda_organisation$L2[10]), as.character(cda_organisation$L3[10]), as.character(cda_organisation$L4[10]), as.character(cda_organisation$L5[10])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[11]),
      radioButtons("org44q04", label = h4(cda_organisation$Pertanyaan[11]), choiceNames = list(as.character(cda_organisation$L1[11]), as.character(cda_organisation$L2[11]), as.character(cda_organisation$L3[11]), as.character(cda_organisation$L4[11]), as.character(cda_organisation$L5[11])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org45",
      h3(cda_organisation$Indikator[12]),
      radioButtons("org45q01", label = h4(cda_organisation$Pertanyaan[12]), choiceNames = list(as.character(cda_organisation$L1[12]), as.character(cda_organisation$L2[12]), as.character(cda_organisation$L3[12]), as.character(cda_organisation$L4[12]), as.character(cda_organisation$L5[12])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[13]),
      radioButtons("org45q02", label = h4(cda_organisation$Pertanyaan[13]), choiceNames = list(as.character(cda_organisation$L1[13]), as.character(cda_organisation$L2[13]), as.character(cda_organisation$L3[13]), as.character(cda_organisation$L4[13]), as.character(cda_organisation$L5[13])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[14]),
      radioButtons("org45q03", label = h4(cda_organisation$Pertanyaan[14]), choiceNames = list(as.character(cda_organisation$L1[14]), as.character(cda_organisation$L2[14]), as.character(cda_organisation$L3[14]), as.character(cda_organisation$L4[14]), as.character(cda_organisation$L5[14])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org46",
      h3(cda_organisation$Indikator[15]),
      radioButtons("org46q01", label = h4(cda_organisation$Pertanyaan[15]), choiceNames = list(as.character(cda_organisation$L1[15]), as.character(cda_organisation$L2[15]), as.character(cda_organisation$L3[15]), as.character(cda_organisation$L4[15]), as.character(cda_organisation$L5[15])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[16]),
      radioButtons("org46q02", label = h4(cda_organisation$Pertanyaan[16]), choiceNames = list(as.character(cda_organisation$L1[16]), as.character(cda_organisation$L2[16]), as.character(cda_organisation$L3[16]), as.character(cda_organisation$L4[16]), as.character(cda_organisation$L5[16])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org47",
      h3(cda_organisation$Indikator[17]),
      radioButtons("org47q01", label = h4(cda_organisation$Pertanyaan[17]), choiceNames = list(as.character(cda_organisation$L1[17]), as.character(cda_organisation$L2[17]), as.character(cda_organisation$L3[17]), as.character(cda_organisation$L4[17]), as.character(cda_organisation$L5[17])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[18]),
      radioButtons("org47q02", label = h4(cda_organisation$Pertanyaan[18]), choiceNames = list(as.character(cda_organisation$L1[18]), as.character(cda_organisation$L2[18]), as.character(cda_organisation$L3[18]), as.character(cda_organisation$L4[18]), as.character(cda_organisation$L5[18])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[19]),
      radioButtons("org47q03", label = h4(cda_organisation$Pertanyaan[19]), choiceNames = list(as.character(cda_organisation$L1[19]), as.character(cda_organisation$L2[19]), as.character(cda_organisation$L3[19]), as.character(cda_organisation$L4[19]), as.character(cda_organisation$L5[19])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[20]),
      radioButtons("org47q04", label = h4(cda_organisation$Pertanyaan[20]), choiceNames = list(as.character(cda_organisation$L1[20]), as.character(cda_organisation$L2[20]), as.character(cda_organisation$L3[20]), as.character(cda_organisation$L4[20]), as.character(cda_organisation$L5[20])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[21]),
      radioButtons("org47q05", label = h4(cda_organisation$Pertanyaan[21]), choiceNames = list(as.character(cda_organisation$L1[21]), as.character(cda_organisation$L2[21]), as.character(cda_organisation$L3[21]), as.character(cda_organisation$L4[21]), as.character(cda_organisation$L5[21])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[22]),
      radioButtons("org47q06", label = h4(cda_organisation$Pertanyaan[22]), choiceNames = list(as.character(cda_organisation$L1[22]), as.character(cda_organisation$L2[22]), as.character(cda_organisation$L3[22]), as.character(cda_organisation$L4[22]), as.character(cda_organisation$L5[22])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[23]),
      radioButtons("org47q07", label = h4(cda_organisation$Pertanyaan[23]), choiceNames = list(as.character(cda_organisation$L1[23]), as.character(cda_organisation$L2[23]), as.character(cda_organisation$L3[23]), as.character(cda_organisation$L4[23]), as.character(cda_organisation$L5[23])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org51",
      h3(cda_organisation$Indikator[24]),
      radioButtons("org51q01", label = h4(cda_organisation$Pertanyaan[24]), choiceNames = list(as.character(cda_organisation$L1[24]), as.character(cda_organisation$L2[24]), as.character(cda_organisation$L3[24]), as.character(cda_organisation$L4[24]), as.character(cda_organisation$L5[24])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[25]),
      radioButtons("org51q02", label = h4(cda_organisation$Pertanyaan[25]), choiceNames = list(as.character(cda_organisation$L1[25]), as.character(cda_organisation$L2[25]), as.character(cda_organisation$L3[25]), as.character(cda_organisation$L4[25]), as.character(cda_organisation$L5[25])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[26]),
      radioButtons("org51q03", label = h4(cda_organisation$Pertanyaan[26]), choiceNames = list(as.character(cda_organisation$L1[26]), as.character(cda_organisation$L2[26]), as.character(cda_organisation$L3[26]), as.character(cda_organisation$L4[26]), as.character(cda_organisation$L5[26])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[27]),
      radioButtons("org51q04", label = h4(cda_organisation$Pertanyaan[27]), choiceNames = list(as.character(cda_organisation$L1[27]), as.character(cda_organisation$L2[27]), as.character(cda_organisation$L3[27]), as.character(cda_organisation$L4[27]), as.character(cda_organisation$L5[27])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[28]),
      radioButtons("org51q05", label = h4(cda_organisation$Pertanyaan[28]), choiceNames = list(as.character(cda_organisation$L1[28]), as.character(cda_organisation$L2[28]), as.character(cda_organisation$L3[28]), as.character(cda_organisation$L4[28]), as.character(cda_organisation$L5[28])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[29]),
      radioButtons("org51q06", label = h4(cda_organisation$Pertanyaan[29]), choiceNames = list(as.character(cda_organisation$L1[29]), as.character(cda_organisation$L2[29]), as.character(cda_organisation$L3[29]), as.character(cda_organisation$L4[29]), as.character(cda_organisation$L5[29])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[30]),
      radioButtons("org51q07", label = h4(cda_organisation$Pertanyaan[30]), choiceNames = list(as.character(cda_organisation$L1[30]), as.character(cda_organisation$L2[30]), as.character(cda_organisation$L3[30]), as.character(cda_organisation$L4[30]), as.character(cda_organisation$L5[30])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org52",
      h3(cda_organisation$Indikator[31]),
      radioButtons("org52q01", label = h4(cda_organisation$Pertanyaan[31]), choiceNames = list(as.character(cda_organisation$L1[31]), as.character(cda_organisation$L2[31]), as.character(cda_organisation$L3[31]), as.character(cda_organisation$L4[31]), as.character(cda_organisation$L5[31])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org53",
      h3(cda_organisation$Indikator[32]),
      radioButtons("org53q01", label = h4(cda_organisation$Pertanyaan[32]), choiceNames = list(as.character(cda_organisation$L1[32]), as.character(cda_organisation$L2[32]), as.character(cda_organisation$L3[32]), as.character(cda_organisation$L4[32]), as.character(cda_organisation$L5[32])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org54",
      h3(cda_organisation$Indikator[33]),
      radioButtons("org54q01", label = h4(cda_organisation$Pertanyaan[33]), choiceNames = list(as.character(cda_organisation$L1[33]), as.character(cda_organisation$L2[33]), as.character(cda_organisation$L3[33]), as.character(cda_organisation$L4[33]), as.character(cda_organisation$L5[33])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[34]),
      radioButtons("org54q02", label = h4(cda_organisation$Pertanyaan[34]), choiceNames = list(as.character(cda_organisation$L1[34]), as.character(cda_organisation$L2[34]), as.character(cda_organisation$L3[34]), as.character(cda_organisation$L4[34]), as.character(cda_organisation$L5[34])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org55",
      h3(cda_organisation$Indikator[35]),
      radioButtons("org55q01", label = h4(cda_organisation$Pertanyaan[35]), choiceNames = list(as.character(cda_organisation$L1[35]), as.character(cda_organisation$L2[35]), as.character(cda_organisation$L3[35]), as.character(cda_organisation$L4[35]), as.character(cda_organisation$L5[35])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[36]),
      radioButtons("org55q02", label = h4(cda_organisation$Pertanyaan[36]), choiceNames = list(as.character(cda_organisation$L1[36]), as.character(cda_organisation$L2[36]), as.character(cda_organisation$L3[36]), as.character(cda_organisation$L4[36]), as.character(cda_organisation$L5[36])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org81",
      h3(cda_organisation$Indikator[37]),
      radioButtons("org81q01", label = h4(cda_organisation$Pertanyaan[37]), choiceNames = list(as.character(cda_organisation$L1[37]), as.character(cda_organisation$L2[37]), as.character(cda_organisation$L3[37]), as.character(cda_organisation$L4[37]), as.character(cda_organisation$L5[37])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[38]),
      radioButtons("org81q02", label = h4(cda_organisation$Pertanyaan[38]), choiceNames = list(as.character(cda_organisation$L1[38]), as.character(cda_organisation$L2[38]), as.character(cda_organisation$L3[38]), as.character(cda_organisation$L4[38]), as.character(cda_organisation$L5[38])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[39]),
      radioButtons("org81q03", label = h4(cda_organisation$Pertanyaan[39]), choiceNames = list(as.character(cda_organisation$L1[39]), as.character(cda_organisation$L2[39]), as.character(cda_organisation$L3[39]), as.character(cda_organisation$L4[39]), as.character(cda_organisation$L5[39])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[40]),
      radioButtons("org81q04", label = h4(cda_organisation$Pertanyaan[40]), choiceNames = list(as.character(cda_organisation$L1[40]), as.character(cda_organisation$L2[40]), as.character(cda_organisation$L3[40]), as.character(cda_organisation$L4[40]), as.character(cda_organisation$L5[40])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org82",
      h3(cda_organisation$Indikator[41]),
      radioButtons("org82q01", label = h4(cda_organisation$Pertanyaan[41]), choiceNames = list(as.character(cda_organisation$L1[41]), as.character(cda_organisation$L2[41]), as.character(cda_organisation$L3[41]), as.character(cda_organisation$L4[41]), as.character(cda_organisation$L5[41])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[42]),
      radioButtons("org82q02", label = h4(cda_organisation$Pertanyaan[42]), choiceNames = list(as.character(cda_organisation$L1[42]), as.character(cda_organisation$L2[42]), as.character(cda_organisation$L3[42]), as.character(cda_organisation$L4[42]), as.character(cda_organisation$L5[42])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[43]),
      radioButtons("org82q03", label = h4(cda_organisation$Pertanyaan[43]), choiceNames = list(as.character(cda_organisation$L1[43]), as.character(cda_organisation$L2[43]), as.character(cda_organisation$L3[43]), as.character(cda_organisation$L4[43]), as.character(cda_organisation$L5[43])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "org83",
      h3(cda_organisation$Indikator[44]),
      radioButtons("org83q01", label = h4(cda_organisation$Pertanyaan[44]), choiceNames = list(as.character(cda_organisation$L1[44]), as.character(cda_organisation$L2[44]), as.character(cda_organisation$L3[44]), as.character(cda_organisation$L4[44]), as.character(cda_organisation$L5[44])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_organisation$Indikator[45]),
      radioButtons("org83q02", label = h4(cda_organisation$Pertanyaan[45]), choiceNames = list(as.character(cda_organisation$L1[45]), as.character(cda_organisation$L2[45]), as.character(cda_organisation$L3[45]), as.character(cda_organisation$L4[45]), as.character(cda_organisation$L5[45])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    
    ###*tab-individu####
    tabItem(tabName = "ind61", 
      h3(cda_individu$Indikator[1]),
      h3(cda_individu$Indikator[5]),
      radioButtons("ind61q01", label = h4(cda_individu$Pertanyaan[1]), choiceNames = list(as.character(cda_individu$L1[1]), as.character(cda_individu$L2[1]), as.character(cda_individu$L3[1]), as.character(cda_individu$L4[1]), as.character(cda_individu$L5[1])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[2]),
      h3(cda_individu$Indikator[5]),
      radioButtons("ind61q02", label = h4(cda_individu$Pertanyaan[2]), choiceNames = list(as.character(cda_individu$L1[2]), as.character(cda_individu$L2[2]), as.character(cda_individu$L3[2]), as.character(cda_individu$L4[2]), as.character(cda_individu$L5[2])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "ind62",
      h3(cda_individu$Indikator[3]),
      radioButtons("ind62q01", label = h4(cda_individu$Pertanyaan[3]), choiceNames = list(as.character(cda_individu$L1[3]), as.character(cda_individu$L2[3]), as.character(cda_individu$L3[3]), as.character(cda_individu$L4[3]), as.character(cda_individu$L5[3])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[4]),
      radioButtons("ind62q02", label = h4(cda_individu$Pertanyaan[4]), choiceNames = list(as.character(cda_individu$L1[4]), as.character(cda_individu$L2[4]), as.character(cda_individu$L3[4]), as.character(cda_individu$L4[4]), as.character(cda_individu$L5[4])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[5]),
      radioButtons("ind62q03", label = h4(cda_individu$Pertanyaan[5]), choiceNames = list(as.character(cda_individu$L1[5]), as.character(cda_individu$L2[5]), as.character(cda_individu$L3[5]), as.character(cda_individu$L4[5]), as.character(cda_individu$L5[5])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[6]),
      radioButtons("ind62q04", label = h4(cda_individu$Pertanyaan[6]), choiceNames = list(as.character(cda_individu$L1[6]), as.character(cda_individu$L2[6]), as.character(cda_individu$L3[6]), as.character(cda_individu$L4[6]), as.character(cda_individu$L5[6])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[7]),
      radioButtons("ind62q05", label = h4(cda_individu$Pertanyaan[7]), choiceNames = list(as.character(cda_individu$L1[7]), as.character(cda_individu$L2[7]), as.character(cda_individu$L3[7]), as.character(cda_individu$L4[7]), as.character(cda_individu$L5[7])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[8]),
      radioButtons("ind62q06", label = h4(cda_individu$Pertanyaan[8]), choiceNames = list(as.character(cda_individu$L1[8]), as.character(cda_individu$L2[8]), as.character(cda_individu$L3[8]), as.character(cda_individu$L4[8]), as.character(cda_individu$L5[8])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[9]),
      radioButtons("ind62q07", label = h4(cda_individu$Pertanyaan[9]), choiceNames = list(as.character(cda_individu$L1[9]), as.character(cda_individu$L2[9]), as.character(cda_individu$L3[9]), as.character(cda_individu$L4[9]), as.character(cda_individu$L5[9])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[10]),
      radioButtons("ind62q08", label = h4(cda_individu$Pertanyaan[10]), choiceNames = list(as.character(cda_individu$L1[10]), as.character(cda_individu$L2[10]), as.character(cda_individu$L3[10]), as.character(cda_individu$L4[10]), as.character(cda_individu$L5[10])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[11]),
      radioButtons("ind62q09", label = h4(cda_individu$Pertanyaan[11]), choiceNames = list(as.character(cda_individu$L1[11]), as.character(cda_individu$L2[11]), as.character(cda_individu$L3[11]), as.character(cda_individu$L4[11]), as.character(cda_individu$L5[11])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "ind63",
      h3(cda_individu$Indikator[12]),
      radioButtons("ind63q01", label = h4(cda_individu$Pertanyaan[12]), choiceNames = list(as.character(cda_individu$L1[12]), as.character(cda_individu$L2[12]), as.character(cda_individu$L3[12]), as.character(cda_individu$L4[12]), as.character(cda_individu$L5[12])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[13]),
      radioButtons("ind63q02", label = h4(cda_individu$Pertanyaan[13]), choiceNames = list(as.character(cda_individu$L1[13]), as.character(cda_individu$L2[13]), as.character(cda_individu$L3[13]), as.character(cda_individu$L4[13]), as.character(cda_individu$L5[13])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[14]),
      radioButtons("ind63q03", label = h4(cda_individu$Pertanyaan[14]), choiceNames = list(as.character(cda_individu$L1[14]), as.character(cda_individu$L2[14]), as.character(cda_individu$L3[14]), as.character(cda_individu$L4[14]), as.character(cda_individu$L5[14])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[15]),
      radioButtons("ind63q04", label = h4(cda_individu$Pertanyaan[15]), choiceNames = list(as.character(cda_individu$L1[15]), as.character(cda_individu$L2[15]), as.character(cda_individu$L3[15]), as.character(cda_individu$L4[15]), as.character(cda_individu$L5[15])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[16]),
      radioButtons("ind63q05", label = h4(cda_individu$Pertanyaan[16]), choiceNames = list(as.character(cda_individu$L1[16]), as.character(cda_individu$L2[16]), as.character(cda_individu$L3[16]), as.character(cda_individu$L4[16]), as.character(cda_individu$L5[16])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[17]),
      radioButtons("ind63q06", label = h4(cda_individu$Pertanyaan[17]), choiceNames = list(as.character(cda_individu$L1[17]), as.character(cda_individu$L2[17]), as.character(cda_individu$L3[17]), as.character(cda_individu$L4[17]), as.character(cda_individu$L5[17])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[18]),
      radioButtons("ind63q07", label = h4(cda_individu$Pertanyaan[18]), choiceNames = list(as.character(cda_individu$L1[18]), as.character(cda_individu$L2[18]), as.character(cda_individu$L3[18]), as.character(cda_individu$L4[18]), as.character(cda_individu$L5[18])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[19]),
      radioButtons("ind63q08", label = h4(cda_individu$Pertanyaan[19]), choiceNames = list(as.character(cda_individu$L1[19]), as.character(cda_individu$L2[19]), as.character(cda_individu$L3[19]), as.character(cda_individu$L4[19]), as.character(cda_individu$L5[19])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[20]),
      radioButtons("ind63q09", label = h4(cda_individu$Pertanyaan[20]), choiceNames = list(as.character(cda_individu$L1[20]), as.character(cda_individu$L2[20]), as.character(cda_individu$L3[20]), as.character(cda_individu$L4[20]), as.character(cda_individu$L5[20])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    tabItem(tabName = "ind64",
      h3(cda_individu$Indikator[21]),
      radioButtons("ind64q01", label = h4(cda_individu$Pertanyaan[21]), choiceNames = list(as.character(cda_individu$L1[21]), as.character(cda_individu$L2[21]), as.character(cda_individu$L3[21]), as.character(cda_individu$L4[21]), as.character(cda_individu$L5[21])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[22]),
      radioButtons("ind64q02", label = h4(cda_individu$Pertanyaan[22]), choiceNames = list(as.character(cda_individu$L1[22]), as.character(cda_individu$L2[22]), as.character(cda_individu$L3[22]), as.character(cda_individu$L4[22]), as.character(cda_individu$L5[22])), choiceValues = list(1, 2, 3, 4, 5), width = "100%"),
      h3(cda_individu$Indikator[23]),
      radioButtons("ind64q03", label = h4(cda_individu$Pertanyaan[23]), choiceNames = list(as.character(cda_individu$L1[23]), as.character(cda_individu$L2[23]), as.character(cda_individu$L3[23]), as.character(cda_individu$L4[23]), as.character(cda_individu$L5[23])), choiceValues = list(1, 2, 3, 4, 5), width = "100%")
    ),
    
    ###*tab-help####
    tabItem(tabName = "help"
    )
  )
)

###*setup dashboard page####
ui <- dashboardPage(
  skin = 'yellow', 
  header,
  sidebar,
  body
)

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
  
  # Append data
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

