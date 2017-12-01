library(shiny)
shinyUI(fluidPage(
  titlePanel("App for suggesting popular books in openlibrary"),
  sidebarLayout(
    sidebarPanel(
      strong("Documentation"),
      br(),
      em("Description:"),
      p("This is an app which suggests books based on interest"),
      em("Data:"),
      a("http://openlibrary.org"),
      p("The data is taken from the openlibrary (above)"),
      em("How it works:"),
      p("Openlibrary API is used to fetch data about books in openlibrary. Goodreads API is used to fetch ratings for the books."),
      em("Instructions for using the app:"),
      p("Initially when the app is loaded, enter the book of interest and then click submit button. It will take a while to render the values after clicking the 'submit' button for the first time."),
      textInput("itext1", label = strong("Enter the title of the book of your interest that is available in open library"), value="gathering blue"),
      br(),
      submitButton("Submit")
      
    ),
    mainPanel(
      h5("Number of authors matching the title"),
      verbatimTextOutput("otext1"),
      h5("Author of the book is"),
      verbatimTextOutput("otext2"),
      h5("Most popular books of this author in openlibrary as per Goodreads ratings are "),
      lapply(1:5, function(x) {uiOutput(paste0("image",x))}),
      conditionalPanel(
        condition = "output.otext1 != ' '",
        selectInput(
          "oradio1", "Choose the author",
          c("let", "shine")
        )
      )
      
    )
  )
))