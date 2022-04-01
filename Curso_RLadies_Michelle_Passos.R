################################################################################
#################################### PACOTE ####################################
################################################################################

### Script elaborado por Michelle Passos - Estatística/UFBA; Mestranda em Matemática 
### com área de concentração em Estatística na UFBA para o meetup do R-LadiesSalvador
### intitulado "Dando adeus ao Shiny App padrão: themes e shinyWidgets" - Abril/2022

## install.packages("C:/Users/chell/Downloads/shinydashboard_0.7.2.tar.gz", repos = NULL, type = "source")

library(shiny)
library(shinydashboard)
library(shinyDashboardThemes)
library(dashboardthemes)
library(plotly)
library(highcharter)

################################################################################
#################################### PARTE 1 ###################################
################################################################################

# INTERFACE BÁSICA

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)


server <- function(input, output) { }


shinyApp(ui, server)

# SKINS

ui <- dashboardPage(skin = "black",
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)


server <- function(input, output) { }


shinyApp(ui, server)

# CUSTOMIZAÇÃO FONTE - CSS

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "Seu Título"),
                    
                    dashboardSidebar(),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: "Lucida Console", Courier, monospace;
                      font-weight: bold;
                      font-size: 24px;
                      }
                    ')))))


server <- function(input, output) { }


shinyApp(ui, server)

# TÍTULOS LONGOS

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "Um Título MUITO Grande",
                                    titleWidth = 380),
                    
                    dashboardSidebar(),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: "Lucida Console", Courier, monospace;
                      font-weight: bold;
                      font-size: 24px;
                      }
                    ')))))


server <- function(input, output) { }


shinyApp(ui, server)

## LARGURA DA BARRA LATERAL

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "Um Título MUITO Grande",
                                    titleWidth = 380),
                    
                    dashboardSidebar(width = 380),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: "Lucida Console", Courier, monospace;
                      font-weight: bold;
                      font-size: 24px;
                      }
                    ')))))


server <- function(input, output) { }


shinyApp(ui, server)

# ABAS NO MENU

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "R Ladies",
                                    titleWidth = 200),
                    
                    dashboardSidebar(width = 200,
                                     sidebarMenu(
                                       menuItem("Pink Power"),
                                       menuItem("Strong Woman"),
                                       menuItem("Information")
                                     )
                    ),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: "Lucida Console", Courier, monospace;
                      font-weight: bold;
                      font-size: 24px;
                      }
                    ')))))


server <- function(input, output) { }


shinyApp(ui, server)

# ÍCONES EM ABAS DO MENU

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "R Ladies",
                                    titleWidth = 200),
                    
                    dashboardSidebar(
                      width = 200,
                      sidebarMenu(
                        menuItem("Pink Power", tabName = "item1", icon = icon("battery-full")),
                        menuItem("Strong Woman", tabName = "item2", icon = icon("bahai")),
                        menuItem("Information", tabName = "item3", icon = icon("info"))
                      )),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: "Lucida Console", Courier, monospace;
                      font-weight: bold;
                      font-size: 24px;
                      }
                    ')))))


server <- function(input, output) { }


shinyApp(ui, server)

# SUB-ABAS DO MENU

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "R Ladies",
                                    titleWidth = 200),
                    
                    dashboardSidebar(
                      width = 200,
                      sidebarMenu(
                        menuItem("Pink Power", tabName = "item1", icon = icon("battery-full"),
                                 menuItem("Power", tabName = "subitem1", icon = icon("plug"))),
                        menuItem("Strong Woman", tabName = "item2", icon = icon("bahai")),
                        menuItem("Information", tabName = "item3", icon = icon("info"))
                      )),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: "Lucida Console", Courier, monospace;
                      font-weight: bold;
                      font-size: 24px;
                      }
                    ')))))


server <- function(input, output) { }


shinyApp(ui, server)


# TEMA PRONTO

ui <- dashboardPage(
  dashboardHeader(title = "R Ladies",
                  titleWidth = 200),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Pink Power", tabName = "item1", icon = icon("battery-full"),
               menuItem("Power", tabName = "subitem1", icon = icon("plug"))),
      menuItem("Strong Woman", tabName = "item2", icon = icon("bahai")),
      menuItem("Information", tabName = "item3", icon = icon("info"))
    )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    )
    
  )
)

server <- function(input, output) { }


shinyApp(ui, server)

################################################################################
#################################### PARTE 2 ###################################
################################################################################


# EXEMPLO BÁSICO 1

ui <- dashboardPage(
  dashboardHeader(title = "R Ladies",
                  titleWidth = 200),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Strong Woman", tabName = "item2", icon = icon("bahai")),
      menuItem("Pink Power", tabName = "item1", icon = icon("battery-full"),
               menuItem("Power", tabName = "subitem1", icon = icon("plug"))),
      menuItem("Information", tabName = "item3", icon = icon("info"))
    )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
    
    tabItems(
      tabItem(tabName = "item2",
              
              fluidRow(
                
                box(
                    solidHeader=TRUE, status="info",
                    plotlyOutput("ex1")
                )
              )
      )
    )
    
  )
)

server <- function(input, output) {
  output$ex1 <- renderPlotly({
    input$action
    
    df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
    df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                               "Fruits", total.fruits, "Veggies", total.veggies,
                               "<br>", "Wheat", wheat, "Corn", corn))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig <- plot_geo(df, locationmode = 'USA-states')
    fig <- fig %>% add_trace(
      z = ~total.exports, text = ~hover, locations = ~code,
      color = ~total.exports, colors = 'Purples'
    )
    fig <- fig %>% colorbar(title = "Millions USD")
    fig <- fig %>% layout(
      title = '2011 US Agriculture Exports by State',
      geo = g
    )
    
    fig
  
    })
}


shinyApp(ui, server)

# EXEMPLO BÁSICO 2

ui <- dashboardPage(
  dashboardHeader(title = "R Ladies",
                  titleWidth = 200),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Strong Woman", tabName = "item2", icon = icon("bahai")),
      menuItem("Pink Power", tabName = "item1", icon = icon("battery-full"),
               menuItem("Power", tabName = "subitem1", icon = icon("plug"))),
      menuItem("Information", tabName = "item3", icon = icon("info"))
    )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
    
    tabItems(
      tabItem(tabName = "item2",
              
              fluidRow(box(
                solidHeader=TRUE, status="info",
                plotlyOutput("ex1")
              ))),
      
      
      tabItem(tabName = "subitem1",
              
              fluidRow(box(
                solidHeader=TRUE, status="info",
                plotlyOutput("ex2")
              )))
      
    )
    
  )
)

server <- function(input, output) {
  
  output$ex2 <- renderPlotly({
    input$action
    
    fig <- plot_ly(ggplot2::diamonds, y = ~price, color = ~cut, type = "box")
    
    fig
    
  })
  
  
  output$ex1 <- renderPlotly({
    input$action
    
    df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
    df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                               "Fruits", total.fruits, "Veggies", total.veggies,
                               "<br>", "Wheat", wheat, "Corn", corn))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig <- plot_geo(df, locationmode = 'USA-states')
    fig <- fig %>% add_trace(
      z = ~total.exports, text = ~hover, locations = ~code,
      color = ~total.exports, colors = 'Purples'
    )
    fig <- fig %>% colorbar(title = "Millions USD")
    fig <- fig %>% layout(
      title = '2011 US Agriculture Exports by State',
      geo = g
    )
    
    fig
    
  })
}


shinyApp(ui, server)

################################################################################
#################################### PARTE 3 ###################################
################################################################################


# WIDGETS

ui <- dashboardPage(
  dashboardHeader(title = "R Ladies",
                  titleWidth = 200),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Strong Woman", tabName = "item2", icon = icon("bahai")),
      menuItem("Pink Power", tabName = "item1", icon = icon("battery-full"),
               menuItem("Power", tabName = "subitem1", icon = icon("plug"))),
      menuItem("Information", tabName = "item3", icon = icon("info"))
    )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
    
    tabItems(
      tabItem(tabName = "item2",
              
              fluidRow(box(
                solidHeader=TRUE, status="info",
                plotlyOutput("ex1")
              ))),
      
      
      tabItem(tabName = "subitem1",
              
              fluidRow(box(
                solidHeader=TRUE, status="info",
                plotlyOutput("ex2"),
                radioGroupButtons(
                  inputId = "Wid1",
                  label = "",
                  choices = c("Linear", 
                              "Logaritmo"),
                  justified = TRUE
                )
              )
              ))
      
    )
    
  )
)

server <- function(input, output) {
  
  output$ex2 <- renderPlotly({
    input$action
    
    ifelse(input$Wid1=="Linear",
           fig <- plot_ly(ggplot2::diamonds, 
                          y = ~price, 
                          color = ~cut, 
                          type = "box"),
           fig <- plot_ly(ggplot2::diamonds, 
                          y = ~log(price), 
                          color = ~cut, 
                          type = "box")
      
    )
    
    fig
    
  })
  
  
  output$ex1 <- renderPlotly({
    input$action
    
    df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
    df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                               "Fruits", total.fruits, "Veggies", total.veggies,
                               "<br>", "Wheat", wheat, "Corn", corn))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig <- plot_geo(df, locationmode = 'USA-states')
    fig <- fig %>% add_trace(
      z = ~total.exports, text = ~hover, locations = ~code,
      color = ~total.exports, colors = 'Purples'
    )
    fig <- fig %>% colorbar(title = "Millions USD")
    fig <- fig %>% layout(
      title = '2011 US Agriculture Exports by State',
      geo = g
    )
    
    fig
    
  })
}


shinyApp(ui, server)

## WIDGETS - mudando ''cor''

ui <- dashboardPage(
  dashboardHeader(title = "R Ladies",
                  titleWidth = 200),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Strong Woman", tabName = "item2", icon = icon("bahai")),
      menuItem("Pink Power", tabName = "item1", icon = icon("battery-full"),
               menuItem("Power", tabName = "subitem1", icon = icon("plug"))),
      menuItem("Information", tabName = "item3", icon = icon("info"))
    )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
    
    tabItems(
      tabItem(tabName = "item2",
              
              fluidRow(box(
                solidHeader=TRUE, status="info",
                plotlyOutput("ex1")
              ))),
      
      
      tabItem(tabName = "subitem1",
              
              fluidRow(box(
                solidHeader=TRUE, status="info",
                plotlyOutput("ex2"),
                radioGroupButtons(
                  inputId = "Wid1",
                  label = "",
                  choices = c("Linear", 
                              "Logaritmo"),
                  justified = TRUE,
                  status = "success"
                )
              )
              ))
      
    )
    
  )
)

server <- function(input, output) {
  
  output$ex2 <- renderPlotly({
    input$action
    
    ifelse(input$Wid1=="Linear",
           fig <- plot_ly(ggplot2::diamonds, y = ~price, color = ~cut, type = "box"),
           fig <- plot_ly(ggplot2::diamonds, y = ~log(price), color = ~cut, type = "box")
           
    )
    
    fig
    
  })
  
  
  output$ex1 <- renderPlotly({
    input$action
    
    df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
    df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                               "Fruits", total.fruits, "Veggies", total.veggies,
                               "<br>", "Wheat", wheat, "Corn", corn))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig <- plot_geo(df, locationmode = 'USA-states')
    fig <- fig %>% add_trace(
      z = ~total.exports, text = ~hover, locations = ~code,
      color = ~total.exports, colors = 'Purples'
    )
    fig <- fig %>% colorbar(title = "Millions USD")
    fig <- fig %>% layout(
      title = '2011 US Agriculture Exports by State',
      geo = g
    )
    
    fig
    
  })
}


shinyApp(ui, server)
