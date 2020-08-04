login_page <- div(
  titlePanel(title = div(img(src="img/rancang.svg", height = "75px"))), style = "width:500px; max-width:100%; margin:0 auto; padding:20px; text-align:center;",
  wellPanel(shinyjs::useShinyjs(),
            tags$h2("Log In", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
            textInput("username", placeholder="Username", label = tagList(icon("user"), "Username")),
            passwordInput("password", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
            br(),
            div(
              style = "text-align: center;",
              actionButton("loginButton", "MASUK", style = "color: white; background-color:#3c8dbc; padding: 10px 15px; width: 150px; cursor: pointer; font-size: 18px; font-weight: 600;"),
              shinyjs::hidden(
                div(id = "nomatch",
                    tags$p("Oops! Email atau password Anda salah", style = "color: red; font-weight: 600; padding-top: 5px;font-size:16px;", class = "text-center")
                )
              )
            )
  )
)