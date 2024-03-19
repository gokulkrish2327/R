library(shiny)
library(shinythemes)
library(leaflet)
library(htmltools)
library(emojifont)
library(shinyjs)

default_store_longitude <- 80.1221
default_store_latitude <- 12.9211

# Initialize cart_items as a reactiveValues object to store cart items and prices
cart_items <- reactiveValues(items = character(0), prices = numeric(0))

# Cafe Website UI
cafe_ui <- fluidPage(
  theme = shinytheme("readable"),
  tags$head(
    tags$style(
      HTML("
        .title-center {
          text-align: center;
          font-family: 'Cursive', sans-serif;
        }
        body {
          background-color: #eed9c4;
        }
      ")
    )
  ),
  titlePanel(
    div(class = "title-center", "Lupa mini cafe")
  ),
  navbarPage(
    "Cravings?",
    tabPanel("Home",
             div(
               img(src = 'sandwiches.jpg', width = "300px", height = "400px"),
               img(src = "waffles.jpg", width = "300px", height = "400px"),
               img(src = "icecream.jpg", width = "300px", height = "400px"),
               style = "display: flex; justify-content: space-between;"
             ),
             p("Discover our Chef's Culinary Secrets", style = "text-align: center;"),
             div(
               img(src = 'chocolate1.jpg', width = "300px", height = "400px"),
               img(src = "cupcake.jpg", width = "300px", height = "400px"),
               img(src = "chocolate2.jpg", width = "300px", height = "400px"),
               style = "display: flex; justify-content: space-between;"
             ),
             h5("officially sponsored by jaleel")
    ),
    tabPanel("Cart",
             fluidRow(
               selectInput("item", "Select Item:", choices = c("mini", "waffles", "ice cream")),
               numericInput("quantity", "Quantity:", value = 1, min = 1),
               actionButton("add_button", "Add to Cart"),
               actionButton("clear_button", "Clear Cart"),
               actionButton("place_order_button", "Place Order")
             ),
             h3("Cart"),
             tableOutput("cart_table"),
             h4("Total Price:"),
             textOutput("total_price")
    ),
    tabPanel("About us",
             h4("Don't sleep :)"),
             p("Lupa mini was established by the great pastry chef Paul in 1969 in Chennai."),
             h6("The Whimsical Tale of Lupa Mini Cafe"),
             p("In a world perpetually yearning for caffeine and whimsy, Lupa Mini Cafe emerged as an extraordinary oasis, helmed by the enigmatic pastry sorcerer, Paul, who boldly claimed lineage to the very coffee beans he brewed. The cafe's narrative unfolds as an enchanting blend of coffee and capriciousness, where each chapter adds a sprinkle of eccentricity"),
             p("From the mystical discovery of the enchanted coffee bean that birthed the cafe to the audacious 'mini' revolution, a concept so snug that only one patron could enter at a time, ensuring the utmost solitude for coffee aficionados. Then came the legendary Sandwich Skirmish, sparked by a barista's accidental cappuccino baguette, birthing the infamous 'Latte Panini,' forever celebrated on the cafe's menu."),
             p("And let us not forget the Danish Conspiracy—rumors abound that the Danish pastries communicate with extraterrestrials, their mission: intergalactic peace through buttery flakiness. Finally, there are the Time-Traveling Waffles, each bite a teleportation to moments past or future, where Marie Antoinette and Albert Einstein might be your brunch companions. At Lupa Mini Cafe, it's more than a cafe; it's an enchanting odyssey, a portal to the whimsical, where the absurdity is celebrated, and coffee flows like magic. Cheers to the caffeinated adventures!"),
             leafletOutput("map")
    ),
    tabPanel("text me \U0001F36D",  # New tab for the chatbot
             titlePanel("Lubna"),
             p("still i can reply faster than your ex"),
             sidebarLayout(
               sidebarPanel(
                 textInput("userMessage", "Ask me about the menu"),
                 actionButton("sendMessage", "Send")
               ),
               mainPanel(
                 verbatimTextOutput("chatOutput")
               )
             )
    )
  ),
  div(
    id = "footer",
    style = "color: #112839; text-align: center; padding: 10px;",
    "© 2023 gokul krish"
  )
)

# Define Cafe Website Server Logic
cafe_server <- function(input, output, session) {
  # Function to calculate the total price of items in the cart
  calculate_total_price <- function() {
    total_price <- sum(cart_items$prices)
    return(total_price)
  }
  
  # Add item to cart and clear the input fields
  observeEvent(input$add_button, {
    item <- input$item
    quantity <- input$quantity
    price <- switch(
      item,
      "mini" = 5.99,
      "waffles" = 7.99,
      "ice cream" = 3.99,
      0
    )
    
    if (price > 0) {
      cart_items$items <- c(cart_items$items, paste(quantity, item))
      cart_items$prices <- c(cart_items$prices, price * quantity)
    }
    
    # Clear the input fields
    updateSelectInput(session, "item", selected = NULL)
    updateNumericInput(session, "quantity", value = 1)
  })
  
  # Clear cart
  observeEvent(input$clear_button, {
    cart_items$items <- character(0)
    cart_items$prices <- numeric(0)
  })
  
  # Render cart table
  output$cart_table <- renderTable({
    data.frame(Item = cart_items$items, Price = cart_items$prices)
  })
  
  # Render total price
  output$total_price <- renderText({
    total_price <- calculate_total_price()
    formatted_price <- format(total_price, big.mark = ",", decimal.mark = ".", nsmall = 2, scientific = FALSE, trim = FALSE)
    paste("Total Price: $", formatted_price)
  })
  
  # Load the shinyjs library for JavaScript functionality
  library(shinyjs)
  
  # Place an order and show a modal
  observeEvent(input$place_order_button, {
    showModal(modalDialog(
      title = "Order Placed",
      "Your order has been placed successfully!",
      footer = NULL
    ))
    
    # Clear the cart
    cart_items$items <- character(0)
    cart_items$prices <- numeric(0)
    
    # Use JavaScript to delay the redirection after displaying the modal
    jsCode <- "setTimeout(function() {Shiny.setInputValue('redirect_home', true);}, 2000);"
    runjs(jsCode)
  })
  
  # Use observeEvent to listen for the redirection trigger
  observeEvent(input$redirect_home, {
    if (input$redirect_home) {
      # Redirect to the "Home" tab
      updateTabsetPanel(session, "tabs", "Home")
    }
  })
}

# Chatbot Server Logic
chatbot_server <- function(input, output, session) {
  chat_history <- reactiveVal(list())  # Store chat history
  
  observeEvent(input$sendMessage, {
    user_message <- tolower(input$userMessage)
    
    # Define chatbot responses based on user input
    bot_response <- switch(
      user_message,
      "menu" = "Here is our menu:\n1. Coffee\n2. Tea\n3. Sandwiches\n4. Pastries\n5. waffles",
      "coffee" = "We offer a variety of coffee options, including \n1. espresso\n2. cappuccino\n3. drip coffee.",
      "tea" = "Our tea selection includes\n1. black tea\n2. green tea\n3. herbal tea.",
      "sandwiches" = "We have a selection of delicious sandwiches, including\n1. BLT\n2. turkey club.",
      "pastries" = "Our pastries include\n1. croissants\n2. muffins\n3. Danish pastries.",
      "waffles" = "We also have a selection of delicious waffles, including\n1. Brussels Waffle\n2. Belgium Waffle\n3. American Waffle.",
      "espresso" = "Would you like to order espresso?",
      "cappuccino" = "Would you like to order cappuccino?",
      "drip coffee" = "Would you like to order drip coffee?",
      "black tea" = "Would you like to order black tea?",
      "green tea" = "Would you like to order green tea?",
      "herbal tea" = "Would you like to order herbal tea?",
      "BLT" = "Would you like to order BLT?",
      "turkey club" = "Would you like to order turkey club?",
      "croissants" = "Would you like to order croissants?",
      "muffins" = "Would you like to order muffins?",
      "danish pastries" = "Would you like to order Danish pastries?",
      "brussels waffle" = "Would you like to order Brussels Waffle?",
      "belgium waffle" = "Would you like to order Belgium Waffle?",
      "american waffle" = "Would you like to order American Waffle?",
      "hi" = "Hi! Welcome to Lupa's mini cafe. Type 'menu' to see our menu.",
      "i love you" = "I love you too, as a friend \U0001F612.",
      "you look cute" = "Don't expect the same in return\U0001F971  .",
      "who created you" = "I was created by ... (provide creator's name here).",
      "lupa means?" = "The name 'Lupa' was created by Paul after a lot of struggles\n Lupa is the rearranged words of Paul\U0001F480 .",
      "yes"="Your order has been placed. Thank you"
      # Add more responses for other user queries
    )
    
    if (is.null(bot_response)) {
      bot_response <- "I'm sorry, I don't understand. Please ask about our menu."
    }
    
    # Update chat history
    chat_history(c(chat_history(), paste("You: ", input$userMessage), paste("Lubna: ", bot_response)))
    
    # Clear the input field
    updateTextInput(session, "userMessage", value = "")
  })
  
  output$chatOutput <- renderText({
    paste(chat_history(), collapse = "\n")
  })
}

# Define the Shiny App
shinyApp(
  ui = cafe_ui,
  server = function(input, output, session) {
    cafe_server(input, output, session)  # Cafe Website Server Logic
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = default_store_longitude, lat = default_store_latitude, zoom = 15)
       })
    chatbot_server(input, output, session)  # Chatbot Server Logic
  }
)