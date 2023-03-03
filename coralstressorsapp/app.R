#ADD LIBRARIES HERE
library(shiny)
library(tidyverse)
library(bslib) #themes for shinyapp
library(here)

#for maps
library(tmap)
library(tmaptools)
tmap_mode("plot")
library(sf)
library(raster)
library(tiff)
library(leaflet)



# SETUP DATASETS HERE
corals_info <- read_csv(here("data", "corals_info.csv"))

#stressor map files
filename = here("data", "stressor_maps", "sst_extremes_2020.tif" )
sst <- raster(filename)
filename2 = here("data", "stressor_maps", "ocean_acidification_2020.tif")
oa <- raster(filename2)





#for first graph -- individual species' vulnerabilities to different stressors
top10_species <- corals_info %>%
  filter(species == c("acanthastrea brevis", "acanthastrea echinata", "acanthastrea hemprichii")) %>%  #change these later depending on what species we want
  return(top10_species)

#for second graph -- one stressor for each species
stressors_top10_species <- corals_info %>%
  filter(species == c("acanthastrea brevis", "acanthastrea echinata", "acanthastrea hemprichii")) %>%  #change these later depending on what species we want
  return(stressors_top10_species)


#SETUP THE THEME - copied from lab last week we can change
my_theme <- bs_theme(
  bg = "rgba(170, 208, 243)", #copy and pasted from the theme preview, background
  fg = "blue",
  primary = "black",
  base_font = font_google("Times")
)

#bs_theme_preview() lets you use a style sheet to make it pretty
#another way to do this during lab week 3's video, making a css file




######USER INTERFACE########
ui <- fluidPage(#theme = my_theme  #when we put the theme in here before the navbarPage, we get an HTTP 400 error, Casey, any ideas?? Or, Melissa and Eleri if you can figure this out??
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
                             sidebarPanel("Coral Species Options",
                                         selectInput(
                                            inputId = "pick_species", label = "Choose Species:",  #what goes in the input id?
                                            choices = unique(top10_species$species) #gives the options for the checkboxes
                                          )
                             ), #end sidebarPanel
                             mainPanel("Output",
                                       plotOutput("species_graph")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                           ) #end sidebar layout
                  ), #end tabPanel("Thing 2")


tabPanel("Stressor Graphs",
                          sidebarLayout(
                            sidebarPanel("Stressor Options",
                                          selectInput(
                                            inputId = "pick_stressor", label = "Choose Stressor:",
                                            choices = unique(stressors_top10_species$stressor)
                                          )
                              ), #end sidebar panel
                            mainPanel("Output",
                                      plotOutput("stressor_graph")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                          ) #end sidebar layout
                        ), #end tabPanel





































#MAP TWO - MELISSA
tabPanel("Map of Environmental Stressors",  #tabs up at the top we can select between
                       #sidebarLayout( #creates a page that has a sidebar on one side that we can put widgets/explanations on one side, and then a larger panel on the right for graph/map
                       sidebarPanel("Stressor Options",
                                    checkboxGroupInput(
                                     inputId = "pick_stressor", label = "Choose Stressor:",
                                     choices = unique(corals_info$stressor,
                                                        #"sst" = "sst_extremes_2020.tif",
                                                     #"oa" = "ocean_acidification_2020.tif"
                                                      #"uv" = "uv_radiation_2020.tif" #a map of the specified stressor will appear in this tab
                                     )
                                                      , #end sidebarPanel
                         mainPanel(
                           tmapOutput(outputId = "stressor_Tmap")) #call your graph or thing from below here, this line of code comes from what you called your plot in output$plot below in the server
                          ) #end sidebar layout
                  )), #end tabPanel("Thing 4")





































                  #BACKGROUND INFO - HEATHER
                  tabPanel("Background Information",
                          mainPanel("This app provides information about ten coral species and their vulnerabilites to different stressors. Coral species were selected based on (their endangered status/being the most common/etc.) Stressors include:
1) biomass removal,
2) bycatch,
3) entanglement in macroplastic,
4) eutrophication and nutrient pollution,
5) habitat loss and degradation,
6) inorganic pollution,
7) light pollution,
8) marine heat waves,
9) ocean acidification,
10) oceanographic,
11) organic pollution,
12) microplastic pollution,
13) poisons and toxins,
14) salinity changes,
15) sedimentation,
16) sea level rise,
17) sst rise,
18) storm disturbance,
19) UV radiation, and
20) wildlife strikes. Each coral is given a vulnerability ranking between 0 and 1 for each of these stressors. This data is courtesy of Casey O'Hara.


                                    "))  #What is 17? ALso, how do we make different sections and segments here?













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
    scale_fill_manual("#329ea8")) #how do i get this color actually on the graph?
      #figure out text wrapping for x-axis labels

  #graph two
  graph_bystressor <- reactive((
      stressors_top10_species %>%
          filter(stressor == input$pick_stressor)
    ))

  output$stressor_graph <- renderPlot(
    ggplot(data = graph_bystressor(), aes(x = species, y = vuln)) +
      geom_col())  #something is wrong with this, output isn't showing what I want, but it's generally working. I want all species from the dataset on the x-axis, and I want the y-axis to go from 0 to 1 all the time




































  # MAP TWO REACTIVE - MELISSA


#map_bystressor <- reactive((filename = here("data", "stressor_maps", input$pickstressor )))
#filename_r <- raster(map_bystressor)


stressor_Tmap <- tm_shape() + tm_raster(palette = "Oranges") + tm_layout(legend.outside = TRUE)
#output$stressor_map <- tmap_leaflet(stressor_Tmap) #tried leaflet
    #now we need to tell user interface where to put the plot we created. go back up to UI and show where you want it to go







































  # LEAVE THIS HERE TO CLOSE SERVER PANEL
}

# LEAVE THIS HERE TO RUN THE APPLICATION
shinyApp(ui = ui, server = server)
