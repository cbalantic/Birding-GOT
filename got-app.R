# got-app.R
# Cathleen Balantic
# 03/15/2018

# App combines a map of ASOIAF continents with a Google form interface for submitting birding-by-ear observations from watching Game of Thrones

library(leaflet)
library(dplyr)
library(rgdal)
library(data.table)
library(shiny)
library(shinyjs)
library(googlesheets)

# This app's code is heavily drawn from: 
#  1. The Rstudio superzip example: 
#     https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
#  
#  2. Dean Attali's code for mimicking a Google form in R shiny:
#     https://deanattali.com/2015/06/14/mimicking-google-form-shiny/


# DATA TO USE IN APP: 
# Identify the Google sheet we are working with 
#   (VIEW: everyone; EDIT: Cathleen only)
GOT_sheet_key <- '1B5_IP0N8AgQ8GscEWm9X7vq_GO7j7N6YQgBJ-D8wPZo'

# Locations to display in submission form:
show.location.names <- sort(
  c(
    # From 'locations' shapefile:
    'Astapor', 'Baelish Keep', 'Braavos', 'Casterly Rock',
    'Castle Black', "Craster's Keep", 'Deepwood Motte', 
    'Dragonstone', 'Eastwatch-by-the-Sea', 
    'Fist of the First Men',  'Harrenhal', 'Highgarden', 
    'Karhold', "King's Landing", 'Lannisport', 'Last Hearth', 
    'Meereen', 'Moat Cailin', "Mole's Town", 'Nightfort', 
    'Oldtown', 'Pentos','Qarth', 'Queenscrown', 'Riverrun', 
    'Shadow Tower', "Storm's End", 'Sunspear', 
    'The Bloody Gate', 'The Dreadfort', 'The Eyrie', 
    'The Red Keep', 'The Twins', 'Tower of Joy', 'Vaes Dothrak',
    'Valyria','Volantis','White Harbor', 'Winterfell', 'Yunkai',
    
    # From 'regions' shapefile: 
    'Iron Islands', 'Kingswood', 'Lhazar', 'The Dothraki Sea',
    'The Haunted Forest', 'The Narrow Sea', 'The Land of Always Winter', 
    'The Red Waste', 'The Smoking Sea', 'Whispering Wood', 'Wolfswood', 
    
    # From 'islands' shapefile: 
    'Bear Island', 'Isle of Cedars','Dragonstone', 'Naath', 'Pyke', 'Tarth',
    
    'UNKNOWN')
)

humanTime <- function() format(Sys.time(), '%Y%m%d-%H%M%OS')

fieldsMandatory <- c('your_name', 'species_name')

labelMandatory <- function(label) {
  tagList(
    label,
    span('*', class = 'mandatory_star')
  )
}

appCSS <-
  '.mandatory_star { color: red; }
#error { color: red; }'


fieldsAll <- c('your_name', 'species_name', 'species_latin', 
               'location', 'season', 'episode', 'minute', 'notes')
responsesDir <- file.path('responses')
epochTime <- function() {
  as.integer(Sys.time())
}




ui <- navbarPage('Birding Game of Thrones (by ear)', 
                 id = 'nav', 
                 
                 tabPanel('About',
                          includeMarkdown('about.md')
                 ),
                 
                 tabPanel('Observation Map', 
                          div(class = 'outer', 
                              tags$head(
                                 # add custom css
                                 includeCSS('styles.css') #,
                                 # includeScript('gomap.js')
                               ),
                              
                              # If not using custom CSS, set height of leafletOutput to # instead of %
                              leafletOutput(outputId = 'map', width = '100%', height = 850),
                              
                              # not sure what this next line is doing
                              # oh, this would be a set up for a little data explorer tab if i
                              # chose to have that
                              absolutePanel(id = 'controls', class = 'panel panel-default',
                                            fixed = TRUE, draggable = TRUE, top = 60, left = 'auto',
                                            right = 20, bottom = 'auto',
                                            width = 330, height = 'auto',
                                            
                                            h2('Observations Explorer') #,
                                            
                                            # opportunity to explore data wouldgo here?
                              )
                          )
                 ), # end map tab
                 
                 tabPanel('Submit Observations',
                          
                          shinyjs::useShinyjs(),
                          shinyjs::inlineCSS(appCSS),
                          
                          sidebarLayout(
                            sidebarPanel(
                              # markdown here would give tips on how to submit observations
                              includeMarkdown('submit_observations.md')
                            ),
                            mainPanel(
                              
                              # submit observations form here
                              div(
                                id = 'form',
                                textInput(inputId = 'your_name', 
                                          label = labelMandatory('Your Name'), ''),
                                textInput(inputId = 'species_name', 
                                          label = labelMandatory('Species you observed (common name)')),
                                textInput(inputId = 'species_latin', 
                                          label = 'Species you observed (latin name, if you know it)'),
                                selectInput(inputId = 'location',
                                            label = labelMandatory('Location'),
                                            choices = show.location.names,
                                            selected = 'Winterfell'),
                                selectInput(inputId = 'season',
                                            label = labelMandatory('Season'),
                                            choices = 1:7,
                                            selected = 1),
                                selectInput(inputId = 'episode',
                                            label = labelMandatory('Episode'),
                                            choices = 1:10,
                                            selected = 1),
                                selectInput(inputId = 'minute',
                                            label = labelMandatory('Minute of Episode'),
                                            choices = 1:100,
                                            selected = 1),
                                textInput(inputId = 'notes',label = 'Observation Notes'),
                                actionButton(inputId = "submit", label = "Submit", class = "btn-primary")
                              ), # end submit obs DIV
                              
                              # Confirm to user that submission has been saved:
                              div(id = 'form'),
                              shinyjs::hidden(
                                div(
                                  id = 'thankyou_msg',
                                  h3('Thanks, your response was submitted successfully!'),
                                  actionLink('submit_another', 'Submit another response')
                                )
                              ),  
                              
                              # Provide error feedback
                              shinyjs::hidden(
                                span(id = 'submit_msg', 'Submitting...'),
                                div(id = 'error',
                                    div(br(), tags$b('Error: '), span(id = 'error_msg'))
                                )
                              )
                            ) # end mainPanel() with submission info
                          ) # end sidebarLayout() 
                 ) # end "submit observations" tab
) # end ui

server <- function(input, output, session) {
  
  # FORM SUBMISSION
  
  # Check that mandatory fields have been filled
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = 'submit', condition = mandatoryFilled)
  })
  
  # Get submission into desired format: 
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  # Save new observation data to google sheet:
  saveData <- function(data){
    GOT.sheet <- gs_key(x = GOT.sheet.key) %>% 
      gs_add_row(input = data)
  }
  
  # Action to take when submit button is pressed:
  #   + Reset form, hide form, show thank you msg
  observeEvent(input$submit, {
    shinyjs::disable('submit')
    shinyjs::show('submit_msg')
    shinyjs::hide('error')
    
    # Trycatch for submission errors
    tryCatch({
      saveData(formData())
      shinyjs::reset('form')
      shinyjs::hide('form')
      shinyjs::show('thankyou_msg')
    },
    error = function(err) {
      shinyjs::html('error_msg', err$message)
      shinyjs::show(id = 'error', anim = TRUE, animType = 'fade')
    },
    finally = {
      shinyjs::enable('submit')
      shinyjs::hide('submit_msg')
    })
  })
  
  # Action to take if you want to submit another observation:
  observeEvent(input$submit_another, {
    shinyjs::show('form')
    shinyjs::hide('thankyou_msg')
  })  
  
  # MAP RENDERING: 
  output$map <- renderLeaflet({
   GOT_map(map_data, bird_data) 
  })
  
} # end server

shinyApp(ui = ui, server = server)
