library(shiny)
library(shinyjs)
library(shinyalert)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


loadPkg <- function(pkgname){
  # Test to see if package pkgname is installed. 
  # character.only=TRUE means pkgname is a character string with the name of the package we want to use. 
  if(require(pkgname,character.only = TRUE)){
    # paste0() concatenates strings without any separator
    print(paste0("'",pkgname,"' is loaded correctly"))
  } else {
    # The require() function returned FALSE so we will try to install the package from the CRAN site
    print(paste0("Trying to install '",pkgname,"'"))
    install.packages(pkgname,character.only = TRUE,repos="http://cran.us.r-project.org")
    if(require(pkgname,character.only = TRUE)){
      print(paste0("'",pkgname,"' is installed and loaded."))
    } else {
      print(paste0("Could not install '",pkgname,"'"))
    }
  }
}

# If we provide a vector of package names, we can load them all as follows:
loadPkgs <- function(pkgnames){
  for (pkgname in pkgnames)loadPkg(pkgname)
}

pkgnames <- c("tidyverse","shiny","DBI", "stringr","jsonlite","shinydashboard",'stringr')
loadPkgs(pkgnames)

#### login and register button section ####
passwordModal <- function(failed = FALSE) {
  modalDialog(
    title = "Create a new password",
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    "If successful, you will be assigned a Player Name to go with this password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("passwordok", "OK")
    )
  )
}

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("playername", "Enter your assigned Player Name"),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student083",
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student083",
    password = "nv!*kkZv@c6u")
  conn
}


getPlayerID <- function(playername,password){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE playername=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  #print(result)
  #print(playerid)
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}

getRandomPlayerName <- function(conn){
  #Given a connection, call the View 'LeaderRandomName' and return the resulting name
  result <- dbGetQuery(conn,"SELECT * FROM LeaderRandomName")
  # result should be a dataframe with a single row and a column named 'playername'
  playername <- result$playername[1]
  # To test what happens when there is a duplicate entry, we can override the random result
  #playername <- "SophisticatedImaginaryZoo" # This matches an existing player in my database
  playername
}

createNewPlayerQuery <- function(conn,playername,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO LeaderPlayer (playername,password) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
}

registerPlayer <- function(password){
  #open the connection
  conn <- getAWSConnection()
  playername <- getRandomPlayerName(conn)
  query <- createNewPlayerQuery(conn,playername,password)
  print(query) #for debug
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print(result)
        success <- TRUE
      }, error=function(cond){print("registerPlayer: ERROR")
        print(cond)
        # The query failed, likely because of a duplicate playername
        playername <- getRandomPlayerName(conn)
        query <- createNewPlayerQuery(conn,playername,password) }, 
      warning=function(cond){print("registerPlayer: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  # This may not have been successful
  if (!success) playername = NULL
  #Close the connection
  dbDisconnect(conn)
  playername
}

getGameVariantList <- function(){
  #open the connection
  conn <- getAWSConnection()
  gamevariants <- dbGetQuery(conn,"SELECT * FROM LeaderGameVariant")
  variantids <- gamevariants$gamevariantid # a vector
  variantnames <- gamevariants$variantname # a vector
  names(variantids) <- variantnames
  #Close the connection
  dbDisconnect(conn)
  variantids
}

publishScore <- function(playerid,gamevariantid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderScore (playerid,gamevariantid,asoftime,score) VALUES (?id1,?id2,NOW(),?id3)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=gamevariantid,id3=score)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

getLeaderBoard <- function(gamevariantid){
  conn <- getAWSConnection()
  #First, we need to know whether highscorewins for this game variant
  query <- paste0("SELECT highscorewins FROM LeaderGameVariant WHERE gamevariantid=",gamevariantid)
  result <- dbGetQuery(conn,query)
  #result should return a single row
  highscorewins <- result$highscorewins[1]
  #Assemble the query for this gamevariantid
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp"
  query <- paste0(query," ON (ls.playerid=lp.playerid) WHERE ls.gamevariantid =")
  query <- paste0(query,gamevariantid)
  if (highscorewins)
    query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  else
    query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  
  query <- paste0(query, " LIMIT 50")
  
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

#### the login and register section end here #### 


#### generating customer section ####

gen_all_cust_grp <- function() {
  # initialize variables
  num_turns <- 8
  total_customers <- 0
  customer_groups <- list()
  
  # generate customer groups for each turn; while loop means no turn has 0 cust
  for (turn in 1:num_turns) {
    num_groups <- 0
    while (num_groups == 0) {
      if (turn <= 4) {
        # for the first 4 turns, maintain a similar number of customers
        num_groups <- min(rpois(1, lambda = 2), 2)
      } else if (turn == 5 || turn == 6) {
        # spike in customer groups at turns 5 and 6 (peak hour)
        num_groups <- min(rpois(1, lambda = 5), 5)
      } else {
        # taper down the number of customers for remaining turns
        num_groups <- min(rpois(1, lambda = 5), 3)
      }
    }
    
    # generate random group sizes
    group_sizes <- sample(1:5, size = num_groups, replace = TRUE)
    
    # store the customer groups and update the total number of customers
    customer_groups[[turn]] <- as.numeric(group_sizes)
  }
  # return the customer groups and total number of customers
  return(customer_groups)
}

gen_all_cust_grp_2 <- function() {
  # initialize variables
  num_turns <- 8
  total_customers <- 0
  customer_groups <- list()
  
  # generate customer groups for each turn; while loop means no turn has 0 cust
  for (turn in 1:num_turns) {
    num_groups <- 0
    while (num_groups == 0) {
      if (turn <= 4) {
        # for the first 4 turns, maintain a similar number of customers
        num_groups <- min(rpois(1, lambda = 4), 4)
      } else if (turn == 5 || turn == 6) {
        # spike in customer groups at turns 5 and 6 (peak hour)
        num_groups <- min(rpois(1, lambda = 5), 6)
      } else {
        # taper down the number of customers for remaining turns
        num_groups <- min(rpois(1, lambda = 3), 3)
      }
    }
    
    # generate random group sizes
    group_sizes <- sample(1:5, size = num_groups, replace = TRUE)
    
    # store the customer groups and update the total number of customers
    customer_groups[[turn]] <- as.numeric(group_sizes)
  }
  # return the customer groups and total number of customers
  return(customer_groups)
}


### ui section ###
ui <- dashboardPage(
 
  
  skin = "purple",
  dashboardHeader(title = tags$span("Sushi Sensei", style = "font-family: 'Arial', sans-serif;")),
  dashboardSidebar(
    sidebarMenu(
      tags$style(HTML("
        .skin-purple .sidebar .treeview-menu>li>a {
         font-family: 'Arial', sans-serif;
        }
      ")),
      menuItem(tags$span("Welcome", style = "font-family: 'Arial', sans-serif;"), tabName = "welcome", icon = icon("door-open")),
      menuItem(tags$span("Game", style = "font-family: 'Arial', sans-serif;"), tabName = "game", icon = icon("chess-board")),
      menuItem(tags$span("Scores", style = "font-family: 'Arial', sans-serif;"), tabName = "scores", icon = icon("cash-register"))      
    )
  ),
  dashboardBody(
    tags$style(HTML("body {
                    background-image: url('bg.png');
                    background-repeat: no-repeat;
                    background-attachment: fixed;
                    background-size: cover;
                    background-position: right center;
                  }
                  .seat {
                    background-image: url('table.jpg');
                    background-blend-mode: darken;
                  }
                  .table-container {
                  display: flex;
                  justify-content: space-between;
                  margin-bottom: 5px;
                  }

                  .table-label {
                  flex: 1;
                  text-align: left;
                  font-weight: bold;
                  margin-right: 50px;
                  }

                  .table-value {
                  flex: 1;
                  text-align: right;
                  margin-left: 50px;
                  }"
                  
      
    )),
    
    
    tabItems(
      # First tab content
      tabItem(
              useShinyjs(),  # Enable shinyjs
              includeCSS("www/styles.css"),
              tabName = "welcome",
              h2("Welcome to Sushi Sensei! Register below!"),
              actionButton("register", "Register"),
              actionButton("login", "Login"),
              tags$h4("You are currently logged in as:"),
              htmlOutput("loggedInAs"),
              tags$br(),
              tags$h4("Here's a tutorial for the game, but feel free to play it however you like!"),
              uiOutput("instruction_display")
              
      ),
      
      # Second tab content (Game)
      tabItem(tabName = "game",
              style = "background-image: url('bg.png'); background-repeat: no-repeat; background-attachment: fixed; background-size: 50% 100%; background-position: right center ;",
              useShinyjs(),  # Enable shinyjs
              includeCSS("www/styles.css"),
            
              # Kitchen area and ingredients display
              fluidRow(
                column( 
                  align = "center",
                  width=6,
                  uiOutput("logo_display"),
                  uiOutput("lane1"),
                  uiOutput("lane2"),
                  uiOutput("lane3"),
                  uiOutput("lane4"),
                  uiOutput("lane5")),
                column(
                  align = "center",
                  width=6,
                  uiOutput("ingredients_display",
                           style = "margin-top: 100px; margin-bottom: 50px;",
                           align = "center"),
                  tags$div(
                    style = "display: inline-block; width: 520px; height: 220px; border: 3px solid black; border-radius: 10px; background-color: #f2f2f2;",
                    fluidRow(
                      column(width=1),
                      column(width=8,
                             tags$div(
                               style = "display: inline-block; width: 420px; height: 180px; border: 1px transparent; background-color: transparent;",
                               #ui counters
                               uiOutput("day_count"),
                               uiOutput("turn_count"),
                               uiOutput("cash_count"),
                               uiOutput("tc_queued"),
                               uiOutput("tc_seated"),
                               uiOutput("to_taken"))),
                      column(width =2)),
                      
                    fluidRow(style = "margin-top: 50px; margin-bottom: 25px;",
                             # Set up the Customer Queue Display ####
                             column(align="left", width=2,
                                    tags$img(src = 'queue.png', height = '50', width = '50', align = "center")),
                             column(align="left", width=10,
                                    uiOutput("customer_queue_table"))),
                    
                  
                    tags$div(
                      tags$div(
                        
                        id = "fixed-row",  # Add this id to target the fluidRow in CSS
                        style = "display: inline-block; width: 520px; height: 240px; border: 3px solid black; border-radius: 10px; background-color: #f2f2f2;",
                        fluidRow(
                          tags$div(
                            
                            id = "fixed-row",  # Add this id to target the fluidRow in CSS
                            style = "display: inline-block; width: 420px; height: 180px; border: 1px transparent; background-color: transparent;",
                            column(width = 1),
                            column(
                              width = 4,
                              offset = 0,
                              align = "left",
                              numericInput("customer_group", "Select Customer Group", 1, max = 20, min = 1),
                              numericInput("sel_lane", "Select Lane", 1, max = 5, min = 1)
                            ),
                            column(
                              width = 4,
                              offset = 0,
                              align = "right",
                              actionButton("next_button", "Next turn"),
                              actionButton("seat_customers", "Seat Customer Groups"),
                              actionButton("take_orders", "Take Customer Orders"),
                              br(),br(),br(),br(),
                              actionButton("exit_game", tags$span("Exit Game", style = "font-size: 20px;"), class = "btn-danger")
                            ),
                            column(width = 1)
                          )))),
                    tags$br(),
                    
                    
                  )))),
              

      tabItem(
              useShinyjs(),  # Enable shinyjs
              includeCSS("www/styles.css"),
              tabName = "scores",
              h2("Publish Your Score and See Where You Stand"),
              fluidRow(
                box(
                  width=12,
                  
                  h4("Current Score"),
                  htmlOutput("score"),
                  uiOutput("moreControls"))))
    )
  ))


server <- function(input, output, session) {

###########################################
#### login and register button section ####
  vals <- reactiveValues(password = NULL,playerid=NULL,playername=NULL,gamevariantid=1)
  
  #Fire some code if the user clicks the Register button
  observeEvent(input$register, {
    showModal(passwordModal(failed=FALSE))
  })
  # Fire some code if the user clicks the passwordok button
  observeEvent(input$passwordok, {
    # Check that password1 exists and it matches password2
    if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
      #store the password and close the dialog
      vals$password <- input$password1
      print(vals$password) # for debugging
      vals$playername <- registerPlayer(vals$password)
      if (!is.null(vals$playername)){
        vals$playerid <- getPlayerID(vals$playername,vals$password)
      }
      print(vals$playerid) # for debugging
      removeModal()
    } else {
      showModal(passwordModal(failed = TRUE))
    }
  })
  #Fire some code if the user clicks the Login button
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  # Fire some code if the user clicks the loginok button
  observeEvent(input$loginok, {
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$playername,input$password3)
    if (playerid>0) {
      #store the playerid and playername and close the dialog
      vals$playerid <- playerid
      #print(vals$playerid) # for debugging
      vals$playername <- input$playername
      #print(vals$playername) # for debugging
      removeModal()
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  
  # React to successful login
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername))
      "Not logged in yet."
    else
      vals$playername
  })
  


  # React to completion of game
  output$score <- renderUI({
    if (is.null(score_rv()))
      "No score yet."
    else
      as.character(score_rv())
  })
  
  output$moreControls <- renderUI({
    req(score_rv(),vals$playerid) # if vals$score is NULL, the controls will not be visible
    tagList(
      actionButton("publishscore", "Publish Your Score"),
      tableOutput("leaderboard")
    )
  })
  observeEvent(input$publishscore,{
    publishScore(vals$playerid,vals$gamevariantid,score_rv())
  })
  output$leaderboard <- renderTable({numclicks <- input$publishscore +input$playgame #to force a refresh whenever one of these buttons is clicked
  leaderboard <- getLeaderBoard(vals$gamevariantid)
  leaderboard}
  )
  
#############################################
#### Button to update the customer queue ####
  
  #### initialize the variables needed ####
  
  # total number of days set:
  total_days <- 7
  # day counter
  day_rv <- reactiveValues(day = 1)
  
  # overall customer groups list
  all_cust_grp <- gen_all_cust_grp()
  
  ## statistics counters; continues tracking beyond each day
  
  # reactive value to track the total number of customers generated
  total_cust_gen_rv <- reactiveValues(total_cust_gen = sum(all_cust_grp[[1]]))
  
  # reactive value to track the total number of customers seated
  total_cust_seated_rv <- reactiveValues(total_cust_seated = 0)
  
  # reactive value to track the total number of orders taken
  total_orders_taken_rv <- reactiveValues(total_orders_taken = 0)
  
  # main turns counter; inside day loop
  turns_rv <- reactiveValues(turn = 1)
  total_turns <- 8
  
  #### DATA STRUCTURES FOR TRACKING OF GAME EVENTS ####
  # main queue data structure
  queue_rv <- reactiveValues(queue = list())
  # main seating matrix structure
  seats_rv <- reactiveValues(seating_matrix = matrix(0,nrow = 6,ncol = 5))
  # main inventory list; initialized to default at start
  inventory_rv <- reactiveValues(
    rice = 50,
    salmon = 35,
    eggs = 20,
    tuna = 25,
    cash = 1000
  )
  
  # kitchen queue for orders and removing customers
  kitchen_queue_rv <- reactiveValues(kitchen_queue = list())
  
  # reactive value to track whether orders have been taken for the current turn
  orders_taken_rv <- reactiveValues(orders_taken = FALSE)
  
  # list to store served customer groups
  served_customers_rv <- reactiveValues(served_customers = list())
  
  # Before the server function, add the following line:
  inv_alert_shown <- reactiveValues(val = FALSE)
  
  # inv = 0 handling
  game_state_rv <- reactiveValues(state = "active")
  
  # score rv
  score_rv <- reactiveValues(score = 0)
  
  #initialize turn 1 variables
  queue_rv$queue <- all_cust_grp[[1]]
  
  # ui linkage
  
  # inventory display
  output$ingredients_display <- renderUI({
    tagList(
      div(style = "display: inline-block; margin-right: 10px; text-align: center;", 
          tags$img(src = 'rice.png', height = '50', width = '50'), 
          div(HTML(paste0("<b style='font-size:20px;'>Rice: ", inventory_rv$rice, "</b>")))
      ),
      div(style = "display: inline-block; margin-right: 10px; text-align: center;", 
          tags$img(src = 'salmon.png', height = '50', width = '50'),
          div(HTML(paste0("<b style='font-size:20px;'>Salmon: ", inventory_rv$salmon, "</b>")))
      ),
      div(style = "display: inline-block; margin-right: 10px; text-align: center;", 
          tags$img(src = 'egg.png', height = '50', width = '50'),
          div(HTML(paste0("<b style='font-size:20px;'>Eggs: ", inventory_rv$eggs, "</b>")))
      ),
      div(style = "display: inline-block; margin-right: 10px; text-align: center;", 
          tags$img(src = 'tuna.png', height = '50', width = '50'),
          div(HTML(paste0("<b style='font-size:20px;'>Tuna: ", inventory_rv$tuna, "</b>")))
      )
    )
  })
  
  output$instruction_display<- renderUI({
    tags$img(src = "instructions.png", alt = "Instruction", width='640', height='640')
  })
  
  # game logo 
  output$logo_display <- renderUI({
    tags$img(src = "sushi sensei logo.png", alt = "Sushi Sensei", width = '300', height = '300', style = "position: relative; top: 00px; left: 0px;")
  })
  
  # customer queue display
  output$customer_queue_table <- renderUI({
    tmp <- queue_rv$queue
    res <- lapply(seq_along(tmp), function(i) {
      grp_size <- sum(tmp[[i]])
      list(
        div(style = "display: inline-block; margin-right: 10px; text-align: center;", 
            div(paste0("Group ", i)), 
            tags$img(src = paste0("group_size_", grp_size, ".png"), height = '40', width = '40')
        ) 
      )
    })
  })
  
  # seats and tables display
  # @param lane is the lane number to draw the UI for
  draw_seats_and_tables <- function(lane) {
    # Access the seating matrix
    seats <- seats_rv$seating_matrix
    
    # Build the seating layout HTML using lapply
    seats_html <- lapply(
      1:6, 
      function(i) {
        img_src = if(seats[i, lane] == 1) 'sitted.png' else 'chair.png'
        tags$div(
          style = paste("display: inline-block;"), 
          tags$img(src = img_src, width = '50', height = '50')
        )
      }
    )
    
    # Spacer div
    spacer <- tags$div(style = "display: inline-block; width: 20px;")
    
    # Alignment div
    alignment_spacer <- tags$div(style = "display: inline-block; width: 30px;")
    
    # Build the table layout HTML
    table_html <- tags$div(
      tags$img(src = 'table.png', width = '360', height = '50')
    )
    
    # Add lane information
    lane_html <- paste0("Lane ", lane)
    
    # Combine the lane information, seating and table layouts
    complete_layout <- tags$div(style = "margin-bottom: 10px;",
                                alignment_spacer, seats_html, lane_html,
                                tags$br(), table_html)
    
    # Return the complete layout HTML
    return(complete_layout)
  } 
  
  # each lane output display
  output$lane1 <- renderUI({ draw_seats_and_tables(1) })
  output$lane2 <- renderUI({ draw_seats_and_tables(2) })
  output$lane3 <- renderUI({ draw_seats_and_tables(3) })
  output$lane4 <- renderUI({ draw_seats_and_tables(4) })
  output$lane5 <- renderUI({ draw_seats_and_tables(5) })
  
  # Function to reset all game variables and inventory to the initial state
  reset_game <- function() {
    # Reset the turn and day counters to 1
    turns_rv$turn <- 1
    day_rv$day <- 1
    
    # reset customer groups generated
    all_cust_grp <<- generate_customer_groups(day_rv$day)
    
    # Reset the queue, kitchen queue, and served customers
    queue_rv$queue <- all_cust_grp[[1]]
    kitchen_queue_rv$kitchen_queue <- list()
    served_customers_rv$served_customers <- list()
    
    # Reset the orders_taken reactive value to FALSE
    orders_taken_rv$orders_taken <- FALSE
    
    # Before the server function, add the following line:
    inv_alert_shown <- reactiveValues(val = FALSE)
    
    # Reset the seating matrix and total customers seated
    seats_rv$seating_matrix <- matrix(0, nrow = 6, ncol = 5)
    total_cust_seated_rv$total_cust_seated <- 0
    
    # Reset the inventory to its initial state
    inventory_rv$rice <- 50
    inventory_rv$salmon <- 35
    inventory_rv$eggs <- 20
    inventory_rv$tuna <- 25
    inventory_rv$cash <- 1000
    
    # Reset the total customers generated and total orders taken counters
    total_cust_gen_rv$total_cust_gen <- sum(all_cust_grp[[1]])
    total_orders_taken_rv$total_orders_taken <- 0
    
    # reset score graph
    score_df <<- data.frame(
      cash = score_cash,
      tot_cgen = score_totcgen,
      tot_cseat = score_totcseat,
      tot_order = score_totorder
    )
  }
  
  # outputs of counters on UI
 
  
  output$day_count <- renderUI({ 
    HTML(paste("<div class='table-container'><div class='table-label' style='font-size: 13px;'>Day:</div><div class='table-value' style='font-size: 16px;'>",
               day_rv$day,
               "</div></div>"))
  })
  
  output$turn_count <- renderUI({ 
    HTML(paste("<div class='table-container'><div class='table-label' style='font-size: 13px;'>Hour:</div><div class='table-value' style='font-size: 16px;'>",
               turns_rv$turn,
               "</div></div>"))
  })
  
  output$cash_count <- renderUI({ 
    HTML(paste("<div class='table-container'><div class='table-label' style='font-size: 13px;'>Cash:</div><div class='table-value' style='font-size: 16px;'>",
               inventory_rv$cash,
               "</div></div>"))
  })
  
  output$tc_queued <- renderUI({ 
    HTML(paste("<div class='table-container'><div class='table-label' style='font-size: 13px;'>Total customers who entered queue:</div><div class='table-value' style='font-size: 16px;'>",
               total_cust_gen_rv$total_cust_gen,
               "</div></div>"))
  })
  
  output$tc_seated <- renderUI({ 
    HTML(paste("<div class='table-container'><div class='table-label' style='font-size: 13px;'>Total customers who were seated:</div><div class='table-value' style='font-size: 16px;'>",
               total_cust_seated_rv$total_cust_seated,
               "</div></div>"))
  })
  
  output$to_taken <- renderUI({ 
    HTML(paste("<div class='table-container'><div class='table-label' style='font-size: 13px;'>Total customer orders taken:</div><div class='table-value' style='font-size: 16px;'>",
               total_orders_taken_rv$total_orders_taken,
               "</div></div>"))
  })
  
  #### FUNCTIONS SECTION ####
  
  ## QUEUE FUNCTIONS
  # enqueue a customer group into the queue
  enqueue <- function(turn) {
    if (turn <= total_turns) {
      queue_rv$queue <- append(queue_rv$queue, all_cust_grp[[turn + 1]])
    }
  }
  
  # dequeue a customer group from the queue
  dequeue <- function(index) {
    if (length(queue_rv$queue) > 0 && index >= 1 && index <= length(queue_rv$queue)) {
      curr_grp_size <- queue_rv$queue[[index]]
      total_cust_seated_rv$total_cust_seated <- total_cust_seated_rv$total_cust_seated + sum(curr_grp_size)
      queue_rv$queue <- queue_rv$queue[-index]
      
    } else {
      # show shinyalert when there are no more customers in the queue
      shinyalert::shinyalert(text = "Nothing to seat!",
                             type = "warning",
                             closeOnClickOutside = TRUE,
                             showConfirmButton = TRUE,
                             confirmButtonText = "OK")
    }
  }
  
  ## SEATING FUNCTIONS
  # function to check if a lane can seat the customer group
  check_lane_availability <- function(lane, group_size) {
    # get the current occupied seats in the selected lane
    occupied_seats <- sum(seats_rv$seating_matrix[, lane])
    
    # check if there are enough available seats in the lane
    if (6 - occupied_seats >= group_size) {
      return(TRUE)  # lane can seat the customer group
    } else {
      return(FALSE)  # lane cannot seat the customer group
    }
  }
  
  # function to seat the customer group in the selected lane
  seat_customer_group <- function(lane, group_size) {
    # get the current occupied seats in the selected lane
    occupied_seats <- sum(seats_rv$seating_matrix[, lane])
    
    # check if there are enough available seats in the lane
    if (6 - occupied_seats >= group_size) {
      # seat the customer group in the lane
      for (i in 1:group_size) {
        empty_seat_row <- which(seats_rv$seating_matrix[, lane] == 0)[1]
        seats_rv$seating_matrix[empty_seat_row, lane] <- 1
      }
    } 
  }
  
  # take orders from customers
  process_orders <- function(total_customers_taken) {
    # probability distribution for sushi orders
    sushi_probs <- c(egg_sushi = 0.25, tuna_sushi = 0.30, salmon_sushi = 0.45)
    # price of each sushi type
    sushi_prices <- c(egg_sushi = 3, tuna_sushi = 5, salmon_sushi = 4)
    # ingredients required for each sushi type
    sushi_ingredients <- list(egg_sushi = "eggs", tuna_sushi = "tuna", salmon_sushi = "salmon")
    
    # randomly select sushi orders for each customer in the group
    orders <- sample(names(sushi_probs), size = total_customers_taken, replace = TRUE, prob = sushi_probs)
    
    # calculate the revenue for the orders
    revenue <- sum(sushi_prices[orders])
    
    # add the revenue to the cash balance
    inventory_rv$cash <- inventory_rv$cash + revenue
    
    # deduct the corresponding amount from the inventory for each sushi type
    for (order in orders) {
      sushi_type <- sushi_ingredients[[order]]
      inventory_rv[[sushi_type]] <- inventory_rv[[sushi_type]] - 1
      inventory_rv$rice <- inventory_rv$rice - 1
    }
    # increment the total_orders_taken reactive value by the number of orders taken
    total_orders_taken_rv$total_orders_taken <- total_orders_taken_rv$total_orders_taken + total_customers_taken
  }
  
  # function to remove served customers from the seating matrix
  remove_customers <- function(served_customers) {
    for (customer_group in served_customers) {
      lane <- customer_group[[1]]
      group_size <- sum(customer_group[[2]])
      
      # clear seats for served customers in the lane; turn 1 to 0; note that this runs from top down
      for (i in 1:group_size) {
        filled_seat_row <- which(seats_rv$seating_matrix[, lane] == 1)[1]
        seats_rv$seating_matrix[filled_seat_row, lane] <- 0
      }
    }
  }
  
  # check if inv is Zero
  check_inventory <- function(inventory_rv) {
    # ingredients required for each sushi type
    sushi_ingredients <- list(egg_sushi = "eggs", tuna_sushi = "tuna", salmon_sushi = "salmon")
    
    # Check if any ingredient is 0
    for (order in names(sushi_ingredients)) {
      sushi_type <- sushi_ingredients[[order]]
      if (inventory_rv[[sushi_type]] <= 0 && !inv_alert_shown$val && game_state_rv$state == "active") {
        # Show shinyalert when an ingredient has finished
        shinyalert::shinyalert(
          text = "An ingredient has finished! Your restaurant is done for the day!",
          type = "error",
          closeOnClickOutside = FALSE,
          showConfirmButton = TRUE
        )
        # Set the inv_alert_shown reactiveVal to TRUE to prevent multiple alerts
        inv_alert_shown$val <- TRUE
        # Set the game state to "inactive" to prevent further actions for the current day
        game_state_rv$state <- "inactive"
        # Increment the day counter to progress to the next day
        day_rv$day <- day_rv$day + 1
        turns_rv$turn <- 1
        # reset customer groups generated
        all_cust_grp <<- generate_customer_groups(day_rv$day)
        # Reset the queue, kitchen queue, and served customers
        queue_rv$queue <- all_cust_grp[[1]]
        kitchen_queue_rv$kitchen_queue <- list()
        served_customers_rv$served_customers <- list()
        # Reset the orders_taken reactive value to FALSE
        orders_taken_rv$orders_taken <- FALSE
        # Before the server function, add the following line:
        inv_alert_shown <- reactiveValues(val = FALSE)
        # Reset the seating matrix and total customers seated
        seats_rv$seating_matrix <- matrix(0, nrow = 6, ncol = 5)
        # Prompt the player to input inventory for the next day
        input_inventory_for_next_day()
        break
      }
    }
  }
  
  # Function to prompt the player to input inventory for the next day
  input_inventory_for_next_day <- function() {
    # Show a modal dialog to prompt the player for inventory input
    showModal(
      modalDialog(
        "Please order for the next day! You'll start the day with the amounts you have ordered!",
        numericInput("next_day_rice", "Rice:", value = 50,min = 1),
        numericInput("next_day_salmon", "Salmon:", value = 30,min = 1),
        numericInput("next_day_eggs", "Eggs:", value = 20,min = 1),
        numericInput("next_day_tuna", "Tuna:", value = 25,min = 1),
        footer = tagList(
          actionButton("submit_inventory", "Submit", class = "btn-primary")
        )
      )
    )
    # Set the game state back to "active" to allow further actions for the new day
    game_state_rv$state <- "active"
  }
  
  # Function to generate customer groups based on day number
  generate_customer_groups <- function(day_num) {
    if (day_num >= 5) {
      return(gen_all_cust_grp_2())
    } else {
      return(gen_all_cust_grp())
    }
  }
  
  # secretly dequeue a customer group from the queue
  dequeue_secret <- function(index) {
    if (length(queue_rv$queue) > 0 && index >= 1 && index <= length(queue_rv$queue)) {
      queue_rv$queue <- queue_rv$queue[-index]
    } else {
      return()
    }
  }
  # Function to randomly dequeue varying amounts of customer groups from the queue until its size is reduced to 6
  random_dequeue <- function() {
    # Check if the current turn is greater than 8, and return without dequeuing if it is
    if (turns_rv$turn > 8) {
      return()
    }
    
    max_dequeue_count <- 2  # Maximum number of customer groups to dequeue in a single turn
    
    # Check if the queue has more than 6 customer groups to dequeue
    if (length(queue_rv$queue) > 6) {
      while (length(queue_rv$queue) > 6) {
        # Randomly select the number of customer groups to dequeue (between 1 and max_dequeue_count)
        num_to_dequeue <- min(sample(1:max_dequeue_count, 1), length(queue_rv$queue))
        
        # Randomly select num_to_dequeue indexes from the queue to dequeue
        indexes_to_dequeue <- sample(1:length(queue_rv$queue), num_to_dequeue)
        
        # Dequeue the selected customer groups
        for (index in indexes_to_dequeue) {
          dequeue_secret(index)
        }
      }
    }
  }
  
  # for ggplot graph
  score_totorder <- isolate(total_orders_taken_rv$total_orders_taken)
  score_totcseat <- isolate(total_cust_seated_rv$total_cust_seated)
  score_totcgen <- isolate(total_cust_gen_rv$total_cust_gen)
  score_cash <- isolate(inventory_rv$cash)
  
  score_df <- data.frame(
    cash = score_cash,
    tot_cgen = score_totcgen,
    tot_cseat = score_totcseat,
    tot_order = score_totorder
  )
  
  # for scoring
  score_rv <- reactive(0.03 * (inventory_rv$cash) - 0.6 * (total_cust_seated_rv$total_cust_seated - total_orders_taken_rv$total_orders_taken) + 0.1*total_cust_gen_rv$total_cust_gen)
  
  # graph stuff
  # Define a reactive expression to get the dynamic x-axis range
  xy_axis_range <- reactive({
    list(x = seq_len(nrow(score_df)),
         y = range(c(score_df$cash, score_df$tot_cgen, score_df$tot_cseat, score_df$tot_order)))
  })
  
  # Plot the data using ggplot2
  output$score_graph <- renderPlot({
    ggplot(score_df, aes(x = xy_axis_range()$x)) +
      geom_line(aes(y = cash), linewidth = 1, color = "blue") +
      geom_line(aes(y = tot_cgen), linewidth = 1, color = "red") +
      geom_line(aes(y = tot_cseat), linewidth = 1, color = "green") +
      geom_line(aes(y = tot_order), linewidth = 1, color = "orange") +
      labs(x = "All Turns", y = "Values", title = "Blue:Cash, Red:Total Queued, Green:Total Seated, Orange:Total Served") +
      coord_cartesian(xlim = c(1, xy_axis_range()$x[length(xy_axis_range()$x)]),
                      ylim = xy_axis_range()$y)
  })
  
  #### Button Outputs ####
  
  # next turn button
  observeEvent(input$next_button, {
    message("Next turn button was pressed")
    # Call random_dequeue function at the beginning of each turn
    random_dequeue()
    if (turns_rv$turn < total_turns) {
      
      # add following turn's customer groups into queue
      enqueue(turns_rv$turn)
      
      # increase customer generated tracker
      total_cust_gen_rv$total_cust_gen <- total_cust_gen_rv$total_cust_gen + sum(all_cust_grp[[turns_rv$turn]])
      
      # increase turn count
      turns_rv$turn <- turns_rv$turn + 1
      
      # remove served customers from the seating matrix
      remove_customers(served_customers_rv$served_customers)
      
      # clear the served_customers list for the new turn
      served_customers_rv$served_customers <- list()
      
      # reset the orders_taken reactive value for the new turn; can only press once per hour
      orders_taken_rv$orders_taken <- FALSE
      
    } else {
      # Check if it's the last day
      if (day_rv$day == total_days) {
        # Show a modal dialog to prompt the player to play again or exit
        showModal(
          modalDialog(
            plotOutput("score_graph"),
            HTML("Congratulations! You have completed all 7 days.<br><br>"),
            HTML(paste("Your score is: ", score_rv(), "<br><br>")),
            HTML("Do you want to play again?"),
            footer = tagList(
              actionButton("play_again", "Play Again", class = "btn-primary"),
              #actionButton("exit_game", "Exit Game", class = "btn-danger"),
              modalButton("Close")
            )
          )
        )
        
      } else {
        # increase day by 1
        day_rv$day <- day_rv$day + 1
        # Reset the turn and day counters to 1
        turns_rv$turn <- 1
        
        # reset customer groups generated
        all_cust_grp <<- generate_customer_groups(day_rv$day)
        
        # Reset the queue, kitchen queue, and served customers
        queue_rv$queue <- all_cust_grp[[1]]
        kitchen_queue_rv$kitchen_queue <- list()
        served_customers_rv$served_customers <- list()
        
        # Reset the orders_taken reactive value to FALSE
        orders_taken_rv$orders_taken <- FALSE
        
        # Before the server function, add the following line:
        inv_alert_shown <- reactiveValues(val = FALSE)
        
        # Reset the seating matrix and total customers seated
        seats_rv$seating_matrix <- matrix(0, nrow = 6, ncol = 5)
        
        # reorder for the next day
        input_inventory_for_next_day()
      }
    }
  })
  
  # seat customers in seating matrix
  observeEvent(input$seat_customers, {
    message("Seat customer button was pressed")
    # get the index selected by the player
    index_to_dequeue <- input$customer_group
    # get the lane selected by the player
    selected_lane <- input$sel_lane
    # get the customer group sizes
    if (length(queue_rv$queue) > 0 && index_to_dequeue >= 1 && index_to_dequeue <= length(queue_rv$queue)) {
      customer_group_sizes <- queue_rv$queue[[index_to_dequeue]]
      
      # check if the lane can seat the customer group; T/F
      if (check_lane_availability(selected_lane, sum(customer_group_sizes))) {
        
        # seat the customer group in the lane
        seat_customer_group(selected_lane, sum(customer_group_sizes))
        
        # add seated customer group to kitchen queue; each index is [lane, cust grp size]
        kitchen_queue_rv$kitchen_queue <- append(kitchen_queue_rv$kitchen_queue, list(c(selected_lane,customer_group_sizes)))
        
        # call the dequeue function with the selected index
        dequeue(index_to_dequeue)
      } else {
        # show shinyalert when there are not enough seats in the lane
        shinyalert::shinyalert(text = "Not Enough Seats in the Lane!",
                               type = "warning",
                               closeOnClickOutside = TRUE,
                               showConfirmButton = TRUE,
                               confirmButtonText = "OK")
      } 
    } else {
      # show shinyalert when there are no customer in queue to be seated
      shinyalert::shinyalert(text = "Nothing else to seat!",
                             type = "warning",
                             closeOnClickOutside = TRUE,
                             showConfirmButton = TRUE,
                             confirmButtonText = "OK")
    }
  })
  
  # processing orders
  observeEvent(input$take_orders, {
    message("Take Orders button was pressed")
    if (length(kitchen_queue_rv$kitchen_queue) == 0) {
      # show shinyalert when kitchen queue is empty
      shinyalert::shinyalert(text = "Kitchen queue is empty. No orders to process.",
                             type = "warning",
                             closeOnClickOutside = TRUE,
                             showConfirmButton = TRUE,
                             confirmButtonText = "OK")
      return()
    }
    
    if (!orders_taken_rv$orders_taken) {
      
      # track the first 10 customers in the kitchen queue
      cumulative_sum <- 0
      indexes_to_extract <- c()
      x <- sample(10:15,1) # no. of cust to take
      
      # loop through the kitchen_queue list
      for (i in seq_along(kitchen_queue_rv$kitchen_queue)) {
        # extract the number of customers for the current group
        customers_in_group <- kitchen_queue_rv$kitchen_queue[[i]][2]
        
        # check if adding the current group's customers exceeds the limit of 10
        if (cumulative_sum + customers_in_group <= x) {
          # if not, add the current index to the indexes_to_extract list
          indexes_to_extract <- c(indexes_to_extract, i)
          cumulative_sum <- cumulative_sum + customers_in_group
        } else {
          # if adding the current group's customers exceeds 10, stop the loop
          break
        }
        
      }
      
      # access the first 10 customer groups using the extracted indexes
      first_x_customers <- kitchen_queue_rv$kitchen_queue[indexes_to_extract]
      
      # get the total number of customers taken
      total_customers_taken <- sum(sapply(first_x_customers, `[`, 2))
      
      # call the process_orders function; pass in total number of customers taken
      process_orders(total_customers_taken)
      
      # append served customer groups to the served_customers list
      for (customer_group in first_x_customers) {
        served_customers_rv$served_customers <- append(served_customers_rv$served_customers, list(customer_group))
      }
      
      # clear only the first 10 customers from the kitchen queue after processing their orders
      kitchen_queue_rv$kitchen_queue <- kitchen_queue_rv$kitchen_queue[-indexes_to_extract]
      
      # show confirmation msg using shinyalert
      shinyalert::shinyalert(text = paste("Orders processed for first ",x," customers!"),
                             type = "success",
                             closeOnClickOutside = TRUE,
                             showConfirmButton = TRUE,
                             confirmButtonText = "OK")
      
      # set orders_taken to TRUE to prevent taking orders again in same turn
      orders_taken_rv$orders_taken <- TRUE
    } else {
      # show shinyalert when orders have already been taken for this turn
      shinyalert::shinyalert(text = "Orders have already been taken for this turn!",
                             type = "warning",
                             closeOnClickOutside = TRUE,
                             showConfirmButton = TRUE,
                             confirmButtonText = "OK")
    }
  })
  
  # Event to handle the submission of inventory for the next day
  observeEvent(input$submit_inventory, {
    # Calculate the total cost of ingredients for the next day
    next_day_rice_cost <- input$next_day_rice * 0.5 #rice cost
    next_day_salmon_cost <- input$next_day_salmon * 2 #salmon cost
    next_day_eggs_cost <- input$next_day_eggs * 1 # eggs cost
    next_day_tuna_cost <- input$next_day_tuna * 2.5 #tuna cost
    
    # Calculate the total cost of ingredients
    total_cost <- next_day_rice_cost + next_day_salmon_cost + next_day_eggs_cost + next_day_tuna_cost
    
    # Check if the player has enough cash to purchase the ingredients
    if (total_cost > inventory_rv$cash) {
      # Show a warning message if the player does not have enough cash
      shinyalert::shinyalert(
        text = "Cash balance is insufficient! You will now go into DEBT!",
        type = "error",
        closeOnClickOutside = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "OK"
      )
    }
    # Deduct the total cost of ingredients from the cash balance
    inventory_rv$cash <- inventory_rv$cash - total_cost
    
    # Update the inventory_rv reactive values with the player's inputs
    inventory_rv$rice <- input$next_day_rice
    inventory_rv$salmon <- input$next_day_salmon
    inventory_rv$eggs <- input$next_day_eggs
    inventory_rv$tuna <- input$next_day_tuna
    
    # Close the modal dialog
    removeModal()
    
    # Show the receipt with costs and cash balance
    receipt_text <- paste(
      "Receipt:",
      "\n",
      "\nRice Qty: ", inventory_rv$rice,
      "\nSalmon Qty: ", inventory_rv$salmon,
      "\nEggs Qty: ", inventory_rv$eggs,
      "\nTuna Qty: ", inventory_rv$tuna,
      "\n",
      "\nRice cost: ", next_day_rice_cost,
      "\nSalmon cost: ", next_day_salmon_cost,
      "\nEggs cost: ", next_day_eggs_cost,
      "\nTuna cost: ", next_day_tuna_cost,
      "\n",
      "\nTotal Cost: ", total_cost,
      "\nCash Balance: ", inventory_rv$cash
    )
    shinyalert::shinyalert(
      text = receipt_text,
      type = "success",
      closeOnClickOutside = FALSE,
      showConfirmButton = TRUE,
      confirmButtonText = "OK"
    )
  })
  
  # Event to handle the "Play Again" button click
  observeEvent(input$play_again, {
    # Reset the game state and close the modal dialog
    reset_game()
    removeModal()
  })
  
  # Event to handle the "Exit Game" button click
  observeEvent(input$exit_game, {
    # Close the modal dialog and stop the Shiny app
    removeModal()
    stopApp()
  })
  
  # Observer function -> continuously check inventory and end the day if any ingredient is depleted
  observe({
    check_inventory(inventory_rv)
  })
  
  # graph stuff
  # Observe changes in day_rv and turn_rv
  observeEvent(turns_rv$turn, {
    score_totorder <- isolate(total_orders_taken_rv$total_orders_taken)
    score_totcseat <- isolate(total_cust_seated_rv$total_cust_seated)
    score_totcgen <- isolate(total_cust_gen_rv$total_cust_gen)
    score_cash <- isolate(inventory_rv$cash)
    
    # Update the data frame with the current values
    new_row <- data.frame(
      cash = score_cash,
      tot_cgen = score_totcgen,
      tot_cseat = score_totcseat,
      tot_order = score_totorder
    )
    # Add the new row to the data frame
    score_df <<- rbind(score_df, new_row)
  })
  
  
}

shinyApp(ui = ui, server = server)