library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Heart disease death" = "hddall",
  "High school education" = "nohsd25",
  "Poverty" = "poverty",
  "Median income" = "medianincome",
  "Population" = "totalpop"
)


shinyUI(navbarPage("Heart disease", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Heart Disease Explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled from CNC, US Census, and UCI'
      )
    )
  ),

   tabPanel("Diagnosis",
      sidebarPanel(
      submitButton("Diagnose"),
      # Age input, numeric input
      numericInput('age', label ="Age", value = 50),
      
      # Sex 
      selectizeInput('sex', 'Sex', choices = list(`Male`=factor(1), `Female` = factor(0),`Unknown`=NA)),
      
      # Chest Pain
      selectizeInput('cp', 'Chest Pain', choices = list(`typical angina` = factor(1), 
                                                        `atypical angina` = factor(2), `non-anginal pain` = factor(3), 
                                                        asymptomatic  = factor(4),`Unknown`=NA)),
      
      # Resting blood pressure
      numericInput('trestbps', label ="Resting blood pressure (in mm Hg)", value = 150),
      
      # Chol
      numericInput('chol', label ="Serum cholestoral in mm/dl", value = 250),
      
      # FBS
      selectizeInput('fbs', label ="Fasting blood sugar more than 120mg/dl?", 
                     choices = list(`Yes`=1, `No` = 0,`Unknown`=NA)),
      
      # restecg
      selectizeInput('restecg', label ="Resting electrocariographic results", 
                     choices = list(`Normal`=factor(0), `ST-T wave abnormality` = factor(1),
                                    `Probable or definite left ventricular hypertropy by Estes Criteria` = factor(2),
                                    `Unknown`=NA )),
      
      # thalach
      numericInput('thalach', label ="Maximum hearthrate achieved during exercise test", value = 150),
      
      # exang
      selectizeInput('exang', label ="Exercise induced angina", 
                     choices = list(`Yes`=factor(1), `No`=factor(0),`Unknown`=NA)),
      
      # oldpeak
      numericInput('oldpeak', label ="ST depression induced by exercise relative to rest", value = 1.5),
      
      # slope
      selectizeInput('slope', label ="Slope of the peak exercise ST segment", 
                   choices = list(`Upsloping`=factor(1), `Flat`=factor(2), 
                                  `Downsloping`=factor(3),`Unknown`=NA)),
      
      # ca
      numericInput('ca', label ="Numer of major vessels (0-3) colored by flourosopy", value = 0),
      
      # Thal
      selectizeInput('thal', label ="Thal", 
                   choices = list(`Normal`=factor(3), `Fixed defect`=factor(6), 
                                  `Reversable defect`=factor(7),`Unknown`=NA)),
      submitButton("Diagnose")
    ),
    mainPanel(
      verbatimTextOutput('prediction')
    )
   ), title = 'Variables for probable heart disease'

 ))
