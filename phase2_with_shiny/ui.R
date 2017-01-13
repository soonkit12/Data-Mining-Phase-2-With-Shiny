#install missing packages and load library
if(require("shinydashboard")==FALSE){install.packages("shinydashboard")}
library(shinydashboard)
if(require("DT")==FALSE){install.packages("DT")}
library(DT)
if(require("ggplot2")==FALSE){install.packages("ggplot2")}
library(ggplot2)
if(require("Amelia")==FALSE){install.packages("Amelia")}
library(Amelia)
if(require("arules")==FALSE){install.packages("arules")}
library(arules)
if(require("arulesViz")==FALSE){install.packages("arulesViz")}
library(arulesViz)

#UI Design

dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "Association Rules Mining Extended Bakery",
    titleWidth  = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("home")),
      menuItem("References", tabName = "references", icon = icon("newspaper-o"),
        menuSubItem("Item List", tabName = "itemlist", icon = icon("list")),
        menuSubItem("Item Dataset", tabName = "itemdataset", icon = icon("database"))
      ),
      menuItem("Item Frequency", tabName = "itemfeq", icon = icon("bar-chart")),
      menuItem("Rules", tabName = "rules", icon = icon("tag")),
      menuItem("Team members", tabName = "team", icon = icon("group"),
        menuSubItem(imageOutput("Ben"),tabName = "ben", icon = "Benjamin Au"),
        menuSubItem(imageOutput("Mah"),tabName = "mah", icon = "Mah Siew Chin"),
        menuSubItem(imageOutput("Ronald"),tabName = "ronald", icon = "Oh Soon Kit"),
        menuSubItem(imageOutput("CR"),tabName = "cr", icon = "Tan Chong Raen")
      )
    )
  ),
  dashboardBody(
    tags$head(tags$script(HTML(
      'Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          eval(message.code);
        }
      );'
        )
      ),
      tags$style(HTML("
      #rInspect, #rSummary {
        overflow-y:visible;
        height:300px;
      }
      #Ben,#Mah,#Ronald,#CR{
          height:180px!important;
      }
      #Ben img,#Mah img,#Ronald img,#CR img{
        border-radius: 50%;
        box-shadow: 1px 1px 10px 1px black;
        height: 180px;
        width: 180px;
      }
      .skin-purple .main-header .logo{
        font-family: 'Georgia', Times, 'Times New Roman', serif;
        font-weight: bold;
        font-size: 14px;
      }
      .skin-purple .sidebar-menu>li.active>a{
        border-left-color: rgb(183, 180, 233)!important;
      }
      #shiny-tab-rules > div > div:first-child > div > div.box-header {
        background-color:darkgrey;
        color:white;
      }
      #shiny-tab-rules > div > div:first-child > div > div.box-header > div > button{
        color:white;
      }
      #shiny-tab-rules > div > div:last-child > div > div.box-body{
        height:700px;
      }
        ")
      )
    ),
    tabItems(
      #About Datasets
      tabItem(tabName = "about",
        fluidRow(
         box(
          title = "About",
          solidHeader = TRUE,
          width = 12,
          status = "primary",
          "Objectives:",br(),
          "1) ", br(),
          "2) ", br(),
          "3) ", br(),
          "4) ", br()
         ) 
        )
      ),
      tabItem(tabName = "itemlist",
        fluidRow(
          box(
          title = "Item List",
          solidHeader = TRUE,
          width = 12,
          status = "primary",
          tableOutput("itemsTable")
         )
        )
      ),
      tabItem(tabName = "itemdataset",
        fluidRow(
          box(
          title = "Item Dataset",
          solidHeader = TRUE,
          width = 12,
          status = "primary",
          dataTableOutput("itemsDataset")
         )
        )
      ),
      tabItem(tabName = "itemfeq",
        fluidRow(
          box(
            title = "Configuration",
            solidHeader = TRUE,
            width = 12,
            status = "warning",
            sliderInput("feqTopN","By Top N",1,50,25),
            textInput("feqSupport","By Support",0.05)
          ),
          box(
            title = "Graph",
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            "Item Frequency : ",verbatimTextOutput("feq_plot_clickInfo"),
            plotOutput("feqPlot",
              click = "feq_plot_click"
            )
          )
        )
      ),
      tabItem(tabName = "rules",
        fluidRow(
          box(
            title = "Configuration",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            column(width = 3,
              selectInput("rbtnRules", "Rules type:",
                              c("Default Setting" = "default",
                                "With Redundant" = "wredundant",
                                "Without Redundant" = "woredundant"))
            ),
            column(width=3,
              numericInput("rConf","By Confindence",0.5,0,1,0.1)
            ),
            column(width=3,
                   numericInput("rSupport","By Support",0.01,0,1,0.01)
            ),
            column(width=3,
                   numericInput("rMinLen","By Minimum Length",1,0,10,1)
            )
          ),
          box(
            title = "Interesting Rules",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "warning",
            dataTableOutput("rInterestRules")
          ),
          box(
            title = "Rules",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "danger",
            dataTableOutput("rRules")
          ),
          box(
            title = "Summary",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "info",
            verbatimTextOutput("rSummary")
          ),
          box(
            title = "Inspect",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "success",
            column(width=6,
              selectInput("rbtnInspectType", "Sort By:",
                            c("Default" = "default",
                              "Support" = "support",
                              "Confidence" = "confidence",
                              "Lift" = "lift"))
            ),
            column(width=6,
              selectInput("rbtnInspectOrder", "Order By:",
                            c("Ascending" = FALSE,
                              "Descending" = TRUE))
            ),
            verbatimTextOutput("rInspect")
          ),
          box(
            title = "Graph",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            column(width = 6,
              selectInput("rbtnPlotsType", "Plot type:",
                               c("Default" = "default",
                                 "Graph" = "graph",
                                 "Grouped" = "grouped",
                                 "Paracoord" = "paracoord"))
            ),
            column(width = 6,
              selectInput("rbtnPlotsControl", "Plot Control:",
                               c("Default" = "default",
                                 "K = 5" = "k",
                                 "Type = Items" = "type",
                                 "Alpha=.5, Reorder=TRUE" = "alpha"))
            ),
            plotOutput("rulesPlot",
              height = 550)
          )
        )
      )
    )
  )
)