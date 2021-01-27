# #Sort by Name
# provinces<- c("Aceh", "Bali", "Bangka Belitung", "Banten", "Bengkulu", "Daerah Istimewa Yogyakarta", "DKI Jakarta", "Gorontalo", "Jambi", "Jawa Barat", "Jawa Tengah",
#               "Jawa Timur", "Kalimantan Barat","Kalimantan Selatan", "Kalimantah Tengah", "Kalimantan Timur", "Kalimantan Utara", "Kepulauan Riau", "Lampung", "Maluku", 
#               "Maluku Utara", "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Papua", "Papua Barat", "Riau", "Sulawesi Barat", "Sulawesi Selatan", "Sulawesi Tengah", 
#               "Sulawesi Tenggara", "Sulawesi Utara", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara")
# 
# ##Mandatory Field
# fieldsMandatory <- c("name","favourite_pkg")
# labelMandatory <- function(label) {
#   tagList(
#     label,
#     span("*", class = "mandatory_star")
#   )
# }
# appCSS <- ".mandatory_star { color: red; }"

dashboardPage(
  skin = 'black', 
  ###*header####
  header = dashboardHeader(title="Penilaian Kapasitas Mandiri", titleWidth = "300px"),
  
  ###*sidebar####
  sidebar = dashboardSidebar(width = "300px",
                             sidebarMenu(
                               ###sidebar-home####
                               menuItem("Beranda", icon = icon("home"), tabName = "home"),
                               ###sidebar-pengaturan####
                               menuItem("Pengaturan", icon = icon("cogs"),
                                        # selectInput("categoryProvince", label = "Pilih provinsi", 
                                        #             list(`Barat` = list("Aceh", "Bangka Belitung", "Bengkulu", "Jambi", "Kepulauan Riau",
                                        #                                 "Lampung", "Riau", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"),
                                        #                  `Tengah` = list("Bali","Banten", "Jawa Barat",
                                        #                                  "Jawa Tengah","Jawa Timur","Kalimantan Barat",
                                        #                                  "Kalimantan Selatan","Kalimantan Tengah", "Kalimantan Timur",
                                        #                                  "Nusa Tenggara Barat","Nusa Tenggara Timur","Yogyakarta"),
                                        #                  `Timur` = list("Gorontalo", "Maluku", "Maluku Utara",
                                        #                                 "Papua", "Papua Barat", "Sulawesi Selatan", "Sulawesi Tengah",
                                        #                                 "Sulawesi Tenggara", "Sulawesi Barat", "Sulawesi Utara"))
                                        # ),
                                        textInput("username", label = "Nama Pengguna", value = "",
                                                  width = NULL, placeholder = "Masukkan nama pengguna Anda"),
                                        passwordInput("password", label = "Kata Sandi", value = "", width=NULL, placeholder="Masukkan kata sandi Anda"),
                                        selectInput("selectedYear", label = "Tahun Analisis", choices = 2019:2030, selected=2019, multiple = FALSE),
                                        actionButton("inputSetting", label = "Masuk")
                               ),
                               ###sidebar-sistem####
                               menuItem("Sistem", icon = icon("sitemap"), 
                                        menuSubItem("Ringkasan Hasil Sistem", tabName = "resTbl")
                                        # actionButton("submitSys", "Submit")
                                        # actionButton("expSys", "Export")
                               ),
                               ###sidebar-organisasi####
                               menuItem("Organisasi", icon = icon("users"), 
                                        menuSubItem("Hasil setiap OPD", tabName = "resTblOrg"),
                                        uiOutput("selectizeInstitution"),
                                        menuSubItem("Ringkasan Hasil Organisasi", tabName = "resTblOrgAll")
                                        # actionButton("submitOrg", "Submit")
                                        # actionButton("expInd", "Export")
                               ),
                               ###sidebar-individu####
                               menuItem("Individu", icon = icon("address-card"), 
                                        # menuSubItem("Hasil setiap Individu", tabName = "resTblInd"),
                                        # uiOutput("selectizeName"),
                                        menuSubItem("Ringkasan Hasil Individu", tabName = "resTblIndAll")
                                        # actionButton("submitInd", "Submit")
                                        # actionButton("expInd", "Export")
                               ),
                               ###sidebar-rangkuman####
                               menuItem("Rangkuman", icon = icon("list-alt"), 
                                        menuSubItem("Hasil Analisis", tabName = "resTblSumm")
                                        # actionButton("submitInd", "Submit")
                                        # actionButton("exportSummary", "Export")
                               ),
                               ###sidebar-bantuan####
                               menuItem("Bantuan", icon = icon("info-circle"), tabName = "help"),
                               # HTML("<li><a href='javascript:window.location.reload(true)'>Keluar</a></li>"),
                               HTML("<li><a href='javascript:window.location.reload(true)'><i class='fa fa-sign-out-alt' style='padding-right: 5px'></i>Keluar</a></li>")
                               
                             )
  ),
  
  ###*body####
  body = dashboardBody(
    tabItems(
      ###*tab-home####
      tabItem(tabName = "home",
              jumbotron(img(src="landingpage.png", width="100%"), " ", button = FALSE)
      ),
      ###*tab-sistem####
      tabItem(tabName = "resTbl",
              h2(textOutput("titleSistem")),
              dataTableOutput("resTblSys"),
              plotlyOutput("resChartSys")
              #actionButton("expSys", "Export")
      ),
      ###*tab-organisasi####
      tabItem(tabName = "resTblOrg",
              h2(textOutput("titleOPD")),
              dataTableOutput("resTblOrg"),
              plotlyOutput("resChartOrg")
              #actionButton("expOrg", "Export")
      ),
      tabItem(tabName = "resTblOrgAll",
              h2(textOutput("titleOrganisasi")),
              dataTableOutput("resTblOrgAll"),
              plotlyOutput("resChartOrgAll")
              #actionButton("expOrg", "Export")
      ),
      ###*tab-individu####
      # tabItem(tabName = "resTblInd",
      #         h2("Hasil Analisis Setiap Individu"),
      #         dataTableOutput("resTblInd"),
      #         plotlyOutput("resChartInd")
      #         #actionButton("expInd", "Export")
      # ),
      tabItem(tabName = "resTblIndAll",
              h2(textOutput("titleIndividu")),
              dataTableOutput("resTblIndAll"),
              plotlyOutput("resChartIndAll")
              #actionButton("expInd", "Export")
      ),
      ###*tab-rangkuman####
      tabItem(tabName = "resTblSumm",
              h2(textOutput("titleRangkuman")),
              h3(textOutput("yearCDNA")),
              dataTableOutput("resTblSumm"),
              plotlyOutput("resChartSumm"),
              downloadButton('downloadResults', 'Unduh Hasil Analisis', style="color: #fff; background-color: #00a65a; border-color: #008d4c"),
      ),
      ###*tab-help####
      tabItem(tabName = "help",
              jumbotron(img(src="help2-01.png", width="100%"), " ", button = FALSE)
      )
    )
  )
)
