# provinces <- c("Aceh","Sumatera Utara","Sumatera Barat","Riau","Kepulauan Riau","Jambi","Sumatera Selatan","Bangka Belitung","Bengkulu","Lampung","DKI Jakarta",
#             "Jawa Barat","Banten","Jawa Tengah","Daerah Istimewa Yogyakarta","Jawa Timur","Bali","Nusa Tenggara Barat","Nusa Tenggara Timur","Kalimantan Utara",
#             "Kalimantan Barat","Kalimantan Tengah","Kalimantan Selatan","Kalimantan Timur","Sulawesi Utara","Sulawesi Barat","Sulawesi Tengah","Sulawesi Tenggara",
#             "Sulawesi Selatan","Gorontalo","Maluku","Maluku Utara","Papua Barat","Papua")

#Sort by Name
provinces<- c("Aceh", "Bali", "Bangka Belitung", "Banten", "Bengkulu", "Daerah Istimewa Yogyakarta", "DKI Jakarta", "Gorontalo", "Jambi", "Jawa Barat", "Jawa Tengah",
              "Jawa Timur", "Kalimantan Barat","Kalimantan Selatan", "Kalimantah Tengah", "Kalimantan Timur", "Kalimantan Utara", "Kepulauan Riau", "Lampung", "Maluku", 
              "Maluku Utara", "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Papua", "Papua Barat", "Riau", "Sulawesi Barat", "Sulawesi Selatan", "Sulawesi Tengah", 
              "Sulawesi Tenggara", "Sulawesi Utara", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara")

##Mandatory Field
fieldsMandatory <- c("name","favourite_pkg")
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }"

dashboardPage(
  skin = 'blue', 
  ###*header####
  header = dashboardHeader(title="Penilaian Mandiri Kapasitas PPRKD", titleWidth = "400px"),
  
  ###*sidebar####
  sidebar = dashboardSidebar(width = "400px",
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
  ),
  
  ###*body####
  body = dashboardBody(
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
    )
  )
)
