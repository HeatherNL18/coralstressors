#ADD LIBRARIES HERE
library(shiny)
library(tidyverse)
library(bslib) #themes for shinyapp
library(here)

# SETUP DATASETS HERE
corals_info <- read_csv(here("data", "corals_info.csv"))

#for first graph -- individual species' vulnerabilities to different stressors
top10_species <- corals_info %>%
  filter(species == c("acanthastrea brevis", "acanthastrea echinata", "acanthastrea hemprichii")) %>%  #change these later depending on what species we want
  return(top10_species)

#for second graph -- one stressor for each species
# stressors_top10_species <- corals_info %>%
#   filter(species == c("acanthastrea brevis", "acanthastrea echinata", "acanthastrea hemprichii")) %>%  #change these later depending on what species we want
#   return(stressors_top10_species)


#SETUP THE THEME - copied from lab last week we can change
#my_theme <- bs_theme(darkly)
  #bg = "rgba(170, 208, 243)", #copy and pasted from the theme preview, background
  #fg = "blue",
  #primary = "black",
  #base_font = font_google("Times")
#)
thematic::thematic_shiny()

#bs_theme_preview() lets you use a style sheet to make it pretty
#another way to do this during lab week 3's video, making a css file



######USER INTERFACE########
ui <- fluidPage(theme = bs_theme(bootswatch = "lux"),
                navbarPage(
                  "Coral Vulnerability to Stressors",






                  #MAP ONE - ELERI
               #   tabPanel("Thing 1",  #tabs up at the top we can select between
                #           sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                 #            sidebarPanel("Widgets",
                  #                        checkboxGroupInput(
                   #                         inputId = "pick_species", label = "Choose Species:",
                       #                     choices = unique(dataset$columnn_name)
                        #                  )
                         #    ), #end sidebarPanel
                          #   mainPanel("Output",
                           #            plotOutput("plot_1")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                          # ) #end sidebar layout
             #     ), #end tabPanel("Thing 1")





































#GRAPHS - HEATHER
tabPanel("Species Vulnerability Graphs",  #tabs up at the top we can select between
                           sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                             sidebarPanel("",
                                         selectInput(
                                            inputId = "pick_species", label = "Choose Coral Species:",  #what goes in the input id?
                                            choices = unique(top10_species$species) #gives the options for the checkboxes
                                          )
                             ), #end sidebarPanel
                             mainPanel("Individual Species Vulnerability to All Stressors",
                                       plotOutput("species_graph")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                           ) #end sidebar layout
                  ), #end tabPanel("Thing 2")


tabPanel("Stressor Graph",
                          sidebarLayout(
                            sidebarPanel("",
                                          selectInput(
                                            inputId = "pick_stressor", label = "Choose Stressor:",
                                            choices = unique(top10_species$stressor)
                                          )
                              ), #end sidebar panel
                            mainPanel("Species Vulnerability to Selected Stressor",
                                      plotOutput("stressor_graph")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                          ) #end sidebar layout
                        ), #end tabPanel





































                  #MAP TWO - MELISSA
             #     tabPanel("Thing 4",  #tabs up at the top we can select between
                       #    sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                         #    sidebarPanel("Widgets",
                                     #     checkboxGroupInput(
                                      #      inputId = "pick_species", label = "Choose Species:",
                                        #    choices = unique(dataset$columnn_name)
                                    #      )
                        #     ), #end sidebarPanel
                         #    mainPanel("Output",
                        #               plotOutput("plot_1")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                         #  ) #end sidebar layout
                #  ), #end tabPanel("Thing 4")





































                  #BACKGROUND INFO - HEATHER
                  tabPanel("App Information",
                           mainPanel(h2("Background Information"),
                                     p("This app provides information about ten coral species and their vulnerabilites to climate and other stressors across the globe.
                                       Coral species were selected based on (their endangered status/being the most common/etc.) The ten species included in this study are:"),
br(),

                                     p("The stressors included in this study are:"),
p("1) biomass removal,"),
p("2) bycatch,"),
p("3) entanglement in macroplastic,"),
p("4) eutrophication and nutrient pollution,"),
p("5) habitat loss and degradation,"),
p("6) inorganic pollution,"),
p("7) light pollution,"),
p("8) marine heat waves,"),
p("9) ocean acidification,"),
p("10) oceanographic,"),
p("11) organic pollution,"),
p("12) microplastic pollution,"),
p("13) poisons and toxins,"),
p("14) salinity changes,"),
p("15) sedimentation,"),
p("16) sea level rise,"),
p("17) sea surface temperature rise,"),
p("18) storm disturbance,"),
p("19) UV radiation, and"),
p("20) wildlife strikes."),
p("Each coral is given a vulnerability ranking between 0 and 1 for each of these stressors."),

br(),
  h2("Methodology"),
  p("The initial dataset contained information on over X number of coral species,"),
br(),
  h2("Data Sources"),
  p("Data were collected courtesy of . . . ")


                           ) #close MainPanel
                            )  #Close tabPanel














#LEAVE THIS HERE TO END USER INTERFACE
                ) #end navbarPage()
) #end fluidPage(theme = my_theme)


########SERVER########
#input from user interface, output is getting passed back to user interface to run and show users
server <- function(input, output) {


  # MAP ONE REACTIVE - ELERI
  #sw_reactive <- reactive((
    #data %>%
      #filter(data_column %>% input$pick_species)  #from above
    #return(newdataframe)
  #))

  #output$plot_name <- #graph or map function like in R markdown here

    #now we need to tell user interface where to put the plot we created. go back up to UI and show where you want it to go








































  # GRAPHS REACTIVE - HEATHER
  #graph one
  graph_byspecies <- reactive((
    top10_species %>%
     filter(species == input$pick_species)
  ))

  output$species_graph <- renderPlot(
    ggplot(data = graph_byspecies(), aes(x = stressor, y = vuln)) +
      geom_col() +
      scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 10)) +
      ylim(0,1) +
      labs(x = "Stressor", y = "Vulnerability") +
      theme_minimal() +
    scale_fill_manual(values = c("inorganic_pollution" = "#329ea8"))) #how do i get this color actually on the graph?

  #graph two
  graph_bystressor <- reactive((
     top10_species %>%
          filter(stressor %in% input$pick_stressor)
    ))

  output$stressor_graph <- renderPlot(
    ggplot(data = graph_bystressor(), aes(x = species, y = vuln)) +
      geom_col() +
      scale_x_discrete(labels = function(x)
        stringr::str_wrap(x, width = 10)) +
    ylim(0,1) +
      labs(x = "Species", y = "Vulnerability") +
      #scale_fill_manual(values = c("acanthastrea brevis" = "blue", "acanthastrea echinata" = "red", "acanthastrea hemprichii" = "green")) +
      theme_minimal()) #something is wrong with this, output isn't showing what I want, but it's generally working. I want all species from the dataset on the x-axis




































  # MAP TWO REACTIVE - MELISSA

  #sw_reactive_2 <- reactive((

 # ))

  #output$plot_name <- #graph or map function like in R markdown here

    #now we need to tell user interface where to put the plot we created. go back up to UI and show where you want it to go







































  # LEAVE THIS HERE TO CLOSE SERVER PANEL
}

# LEAVE THIS HERE TO RUN THE APPLICATION
shinyApp(ui = ui, server = server)
