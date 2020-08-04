login <- FALSE
USER <- reactiveValues(login = login)

observe({ 
  if (USER$login == FALSE) {
    if (!is.null(input$loginButton)) {
      if (input$loginButton > 0) {
        Username <- isolate(input$username)
        Password <- isolate(input$password)
        if(length(which(user_database$username==Username))==1) { 
          pasmatch  <- user_database["hash1"][which(user_database$username==Username),]
          pasverify <- password_verify(pasmatch, Password)
          if(pasverify) {
            USER$login <- TRUE
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } else {
          shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
        }
      } 
    }
  }    
})

output$mainbody <- renderUI({
  if(USER$login == TRUE){
    navbarPage(
      title = div(img(src="img/rancang.png", height = "75px"), style = "padding-left:100px;"),
      id = "navbar",
      selected = "home",
      theme = "styles.css", 
      fluid = T,
      home,
      tabAnalysis,
      tabSatellite,
      navbarMenu("Skenario & Simulasi",
                 bauScenario,
                 intScenario
      ),
      # tabScenario,
      tabTradeoff,
      tabHelp
    )
  } else {
    login_page
  }
})