rm(list = ls())
library(shiny)
library(shinydashboard)
library(readxl)
library(leaflet)
library(plotly)

my_username <- "test"
my_password <- "abc"
# user_list<-read_excel("Yumna/Login/user_list.xlsx")
# my_username<-user_list$user
# my_password<-user_list$pass
###########################/ui.R/##################################

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),
                  actionButton("Login", "Log in"),
                  verbatimTextOutput("dataInfo")
        )
    ),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){
  # tagList("You did it!")
}

header <- dashboardHeader(title="CDNA", titleWidth = "200px")
sidebar <- dashboardSidebar(width = "200px", collapsed = TRUE,
                            sidebarMenu(
                              menuItem("Home", icon = icon("home"), tabName = "home"),
                              ###sidebar-profil####
                              # menuItem("Profil", tabName = "profil"),
                              ###sidebar-system####
                              # menuItem("Tes",
                              #   lapply(1:nrow(system_func_capacity), function(i){
                              #     menuSubItem(system_func_capacity$func_capacity[i], tabName = paste0("tab", system_func_capacity$id[i]))
                              #   })
                              # ),
                              # menuItem("Kuesioner",
                              #          menuSubItem(tabName = "Sistem"),
                              #          menuSubItem(tabname = "Organisasi"),
                              #          menuSubItem(tabName =  "Individu")),
                              menuItem("Pengaturan", #icon = icon("history"),
                                       selectInput("categoryProvince", label = "Pilih provinsi", 
                                                   list(`Barat` = list("Aceh", "Bangka Belitung", "Bengkulu", "Jambi", "Kepulauan Riau",
                                                                       "Lampung", "Riau", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"),
                                                        `Tengah` = list("Bali","Banten", "Jawa Barat",
                                                                        "Jawa Tengah","Jawa Timur","Kalimantan Barat",
                                                                        "Kalimantan Selatan","Kalimantan Tengah",
                                                                        "Nusa Tenggara Barat","Nusa Tenggara Timur","Yogyakarta"),
                                                        `Timur` = list("Gorontalo", "Kalimantan Timur", "Maluku", "Maluku Utara",
                                                                       "Papua", "Papua Barat", "Sulawesi Selatan", "Sulawesi Tengah",
                                                                       "Sulawesi Tenggara", "Sulawesi Barat", "Sulawesi Utara"))
                                       ),
                                       textInput("fullname", label = "Nama lengkap", value = NULL,
                                                 width = NULL, placeholder = NULL),
                                       textInput("username", label = "Nama Pengguna", value = NULL,
                                                 width = NULL, placeholder = NULL),
                                       passwordInput("password", label = "Masukkan password", value = "", width=NULL,placeholder=NULL),
                                       actionButton("inputSetting", label = "Simpan")
                              ),
                              menuItem("Sistem",  #icon = icon("history"), 
                                       menuSubItem("Analisis Tingkat Sistem", tabName = "resTbl"),
                                       # actionButton("submitSys", "Submit")
                                       actionButton("expSys", "Export")
                              ),
                              ###sidebar-organisasi####
                              menuItem("Organisasi", #icon = icon("exchange"), 
                                       menuSubItem("Analisis Tingkat Organisasi", tabName = "resTblOrg"),
                                       # actionButton("submitOrg", "Submit")
                                       actionButton("expInd", "Export")
                              ),
                              ###sidebar-individu####
                              menuItem("Individu",# icon = icon("random"), 
                                       menuSubItem("Analisis Tingkat Individu", tabName = "resTblInd"),
                                       # actionButton("submitInd", "Submit")
                                       actionButton("expInd", "Export")
                              ),
                              menuItem("Rangkuman",# icon = icon("random"), 
                                       menuSubItem("Hasil Analisis Keseluruhan", tabName = "resTblSumm"),
                                       # actionButton("submitInd", "Submit")
                                       actionButton("exportSummary", "Export")
                              ),
                              menuItem("Help", icon = icon("question-circle"), tabName="help")
                            )
)
body <- dashboardBody(
  tabItems(
    ###*tab-home####
    # tabItem(tabName = "home", jumbotron(img(src="ps3.png"), " ", button=FALSE), jumbotron(img(src="tingkatan.png"), " ", button=FALSE)
    # hr(),
    # fluidRow(
    #   column(10, thumbnail_label(image='tingkatan.png', label='CDNA',
    #                             content = 'CDNA memiliki 3 tingkatan yaitu Sistem, Organisasi dan Individu',
    #                             button_link='#shiny-tab-pageOne',button_label = 'OK'))
    #   )
    # ),
    tabItem(tabName = "home",
            leafletOutput("koboMap")
    ),
    ###*tab-profil
    
    # tabItem(tabName = "profil",
    #         fluidPage(#theme = shinytheme("cyborg"),
    #           shinyjs::useShinyjs(),
    #           shinyjs::inlineCSS(appCSS),
    #           div(
    #             selectInput("prov", labelMandatory("Provinsi"),c("Pilih salah satu"="",provinces)),
    #             textInput("nama", labelMandatory("Nama Lengkap"), value="", width=NULL, placeholder=""),
    #             textInput("institusi", labelMandatory("Institusi"), value="", width=NULL, placeholder=""),
    #             textInput("posisi", labelMandatory("Posisi"), value="", width=NULL, placeholder=""),
    #             textInput("jabatan", labelMandatory("Jabatan"), value="", width=NULL, placeholder=""),
    #             dateInput('date', label = labelMandatory('Tanggal'), value = as.character(Sys.Date()), format = "dd/mm/yy", startview = 'year'),
    #             actionButton("submitProfil", "Submit")
    #           )
    #         )
    # ),
    
    tabItem(tabName = "resTbl",
            dataTableOutput("resTblSys"),
            plotlyOutput("resChartSys"),
            actionButton("expSys", "Export")
    ),
    
    tabItem(tabName = "resTblOrg",
            dataTableOutput("resTblOrg"),
            plotlyOutput("resChartOrg"),
            actionButton("expOrg", "Export")
    ),
    tabItem(tabName = "resTblInd",
            dataTableOutput("resTblInd"),
            plotlyOutput("resChartInd"),
            actionButton("expInd", "Export")
    ),
    
    ###*tab-ringkasan####
    tabItem(tabName = "resTblSumm",
            dataTableOutput("resTblSumm"),
            plotlyOutput("resChartSumm"),
            actionButton("exportSummary", "Export")
    ),
    
    ###*tab-help####
    tabItem(tabName = "help"
    )
  ),
  tags$head(tags$style("#dataInfo{color: red")),
  htmlOutput("page")
)

ui <- dashboardPage(header, sidebar, body)

###########################/server.R/##################################

server = (function(input, output,session) {
  
  Logged <- FALSE
  Security <- TRUE
  
  USER <- reactiveValues(Logged = Logged)
  SEC <- reactiveValues(Security = Security)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(my_username == Username & my_password == Password) {
            USER$Logged <- TRUE
          } else {SEC$Security <- FALSE}
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {output$page <- renderUI({ui1()})}
    if (USER$Logged == TRUE) {output$page <- renderUI({ui2()})}
  })
  
  observe({
    output$dataInfo <- renderText({
      if (SEC$Security) {""}
      else {"Username atau Password yang dimasukkan salah"}
    })
  })
  
})

runApp(list(ui = ui, server = server))