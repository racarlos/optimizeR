## app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)

ui <- dashboardPage( 
    
  skin = "blue",	# Styling the Dashboard Page 

  # Header Content
  dashboardHeader(
    title = "CMSC 150 Project"		# Title of the project
  ),
  
  #Sidebar content
  dashboardSidebar(
    
    # Choices for SideBar Operations
    sidebarMenu(
      menuItem("Polynomial Regression", tabName = "regres", icon = icon("chart-area")),
      menuItem("Quadratic Spline", tabName = "qsi", icon = icon("chart-line")),
      menuItem("Simplex", tabName = "simplex", icon = icon("calculator"))
    )
  ),
  
  # Body content
  dashboardBody(
    
    # Contents of each tab
    tabItems(
      
      # First tab content
      tabItem( tabName = "regres",
        
        fluidRow(
          
          # Box Getting the CSV File
          box(
            solidHeader = TRUE,
            background = "black",
            fileInput("prfile", "Choose CSV File : ", accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
            width = 4,    
            height = 100,  #pixels
          ),     
          
          # Box for Displaying the PR function
          valueBoxOutput("prfunc",width = 7),
          
          fluid = TRUE
        ),         
               
        fluidRow(
          # Box For getting the PR degree
          box(
            solidHeader = TRUE,
            background = "black",
            numericInput("prdegree","Input the Degree : ",value=NA),
            width = 4,    
            height = 100,  #pixels
          ),

          # Box for Displaying the PR Estimate
          valueBoxOutput("prapprox",width = 7),
          fluid = TRUE
        ),         
        
        fluidRow(
          
          # Box For Getting the PR Estimate
          box(
            solidHeader = TRUE,
            background = "black",
            numericInput("prapprox","Input the Estimate : ",value=NA),
            width = 4,    
            height = 100,  #pixels
          ),  
          fluid = TRUE
        ),
      ),
    
      # Second tab content
      tabItem( tabName = "qsi",
               
           fluidRow(
      
             # Box Getting the CSV File
             box(
                solidHeader = TRUE,
                background = "black",
                fileInput("qsfile", "Choose CSV File : ", accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
                width = 4,    
                height = 100,  #pixels
             ),     
             
             # Box for displaying the value of the Estimate
             valueBoxOutput("qsanswer",width = 7),
           ),

           fluidRow(
      
              # Box For Getting the Approximate
               box(
                  solidHeader = TRUE,
                  background = "black",
                  numericInput("qsapprox","Input the Estimate : ",value=NA),
                  width = 4,    
                  height = 100,  #pixels
               ),
                
               # Box for Displaying the functions for each Interval 
               box(
                 solidHeader = TRUE,
                 background = "black",
                 tableOutput("qsfuncs"),
                 width = 7,    
               ),
           ),
      ),
      
      #Third tab Content
      tabItem( tabName = "simplex",
          
         fluidRow(
           
           # box with table for taking simplex inputs
           box(
             title = "Input - Supply and Shipping Costs",
             rHandsontableOutput("simplexinput"),
             status = "primary",
             width = 8,    
           ),
           
           # Value box for displaying optimal value
           valueBoxOutput("optimalvalue",width = 4),
       
         ),
         
        fluidRow( 
          
           # Box for displaying out of products to be shipped
           box(
             title = "Output - Products to be Shipped to Each Warehouse",
             rHandsontableOutput("simplexoutput"),
             status = "primary",
             width = 8,    
           ),
           
           valueBoxOutput("totaliterations",width = 4),
        ),
        
        fluidRow( 
          
          # Box for selecting and displaying the tableau of an iteration 
          box(
            title = "Tableau Iterations",
            status ="success",
            selectInput("choice","Select Iteration to Display",choices =  c("Hide")),
            conditionalPanel(condition = "input.choice != 'Hide",tableOutput("tableaulist")),
            width = 12,
          ),
        ),
      )
    )
  )
)
