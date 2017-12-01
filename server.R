library(shiny)
library(httr)
library(jsonlite)
library(stringi)
library(data.table)
initurl <- "http://openlibrary.org/"

titlecall <- function(x) {
  urlsear <- paste0("http://openlibrary.org/search.json?title=", x)
  gh <- GET(urlsear)
  if (status_code(gh) != 200) {
    stop(sprintf("API call failed"), call.= TRUE)
  }
  else {
    ctb <- content(gh, "text")
    infob <- jsonlite::fromJSON(ctb)
    return(infob$docs$author_name)
  }
}

isbncall <- function(x) {
  prts <- paste0("api/books?bibkeys=ISBN:",x, "&format=json&jscmd=data")
  rs <- modify_url(initurl,path =prts)
  gt <- GET(rs)
  if (status_code(gt) != 200) {
    stop(sprintf("API call failed"), call.= TRUE)
  }
  else return(gt)
}

isbnpull <- function(x){
  gt1 <- isbncall(x)
  ct1 <- content(gt1, "text")
  info3 <- jsonlite::fromJSON(ct1)
}

titlefind <- function(x){
  detls <- isbnpull(x)
  vart1 <- paste0("`","ISBN:", x,"`")
  nrhj1 <- paste0("nu11 <-   detls$",vart1, "$title")
  mu2 <- eval(parse(text = nrhj1))
  nrhj11 <- paste0("nu11 <-   detls$",vart1, "$cover$medium")
  mu21 <- eval(parse(text = nrhj11))
  nrhj12 <- paste0("nu11 <-   detls$",vart1, "$url")
  mu22 <- eval(parse(text = nrhj12))
  wt1 <- "Popular book by this author is not available in openlibrary"
  if (is.null(mu2)) {return(wt1) } else {return(list(mu2, mu21, mu22))}
}

morethanfive <- function(x) {
  if (nrow(x) >= 5) {
    rtbk <- tail(x,5)
  }
  else {
    rtbk <- x
  }
}

getfirstelem <- function(x) {
  return(x[[1]])
}

shinyServer(
  function(input,output){
    tlnm <- reactive({stri_replace_all_fixed(as.character(input$itext1), " ", "+")})
    authorget <- reactive({titlecall(tlnm())})
    numauthors <- reactive({length(unique(authorget()))})
    output$otext1 <- renderText({numauthors()})
    nameauthor <- reactive({authorget()[[1]]})
    wt <- "No such book exists in open library. please try with different title"
    
    output$otext2 <- renderText({if (is.null(nameauthor())) {wt} else {nameauthor()}})
    spacecnt <- reactive(stri_replace_all_fixed(nameauthor()," ","+"))
    nwot <- reactive({paste0("search.json?author=", spacecnt())})
    nwrs <- reactive({modify_url(initurl, path = nwot())})
    yy <- reactive({GET(nwrs())})
    reactive({if (status_code(yy()) != 200) {
      stop(sprintf("API call failed"), call.= TRUE)
    }
    })
    cnnt <- reactive({content(yy(), "text")})
    info1 <- reactive({jsonlite::fromJSON(cnnt())})
    listisbn <- reactive({lapply((info1()$docs$isbn), getfirstelem)})

    sndn <- reactive({stri_flatten(listisbn(), ",")})
    newmsg <- reactive(paste0("https://www.goodreads.com/book/review_counts.json?isbns=", sndn(),"&key=bvJD7P7eIYXKqyfuQc7JDA"))
    getmsg <- reactive({GET(newmsg())})
    isbnjson <- reactive(content(getmsg(), "text"))
    info2 <- reactive({jsonlite::fromJSON(isbnjson())})
    tbbl <- reactive({data.frame("isbn" = as.numeric(info2()$books$isbn), "isbn13" = as.numeric(info2()$books$isbn13), "rating" = as.numeric(info2()$books$average_rating))})
    tbbln <- reactive({setkeyv(as.data.table(tbbl()), "rating")})
    tbblx <- reactive(morethanfive(tbbln()))
    listbn <- reactive({as.character(tbblx()$isbn13)})
    ft <- reactive({lapply(listbn(), titlefind)})
    lapply(1:5, function(x) {
                              output[[paste0("image",x)]] <- renderUI({tryCatch({
                              images <-  (ft()[[x]])[[2]]
                              imglink <- (ft()[[x]])[[3]]
                              tags$p(tags$a(href = imglink, tags$img(src= images)),
                              tags$a(href = imglink, (ft()[[x]])[[1]]))
                              }
                              , error = function(e) {e <- " "})
                              })
                              })
    
  })
