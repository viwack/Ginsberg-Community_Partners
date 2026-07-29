#install.packages("shinydashboard")

library(leaflet) #mapping package
library(shiny) #builds web application
library(bslib) #interactive widgets
library(readr) #helps read the files
library(stringr) #supports with cleaning the files
library(sf) #dataframe objects
library(dplyr) #data manip
library(ggplot2) #data viz
library(scales) #supports data viz
library(shinydashboard) #supports web UI
library(htmltools) #when building the website makes it easier
library(tigris) #contains geographical information
library(readxl)
library(tidyr)

# Read in the Community Partner Network
df <- read.csv("Accounts_wideCategories_Geocoded.csv")
# mhvillage_df$Sites <- as.integer(mhvillage_df$Sites)
# pivot wider accounts w/ categories


#function that, depending on the users' selections, creates the corresponding markers needed to be displayed on the map
build_marker_layer <- function(map, layer_name) {
  #so this part is if you clicked for mhvillage markers
  if (layer_name == "MHVillage Markers (includes site info)") {
    df$label <- paste(
      "Name: ", as.character(df$Account.Name),
      br(),
      "Address: ", df$Full_Address,
      br(),
      # "House District: ", as.character(mhvillage_df$`House district`),
      # br(),
      # "Senate District: ", as.character(mhvillage_df$`Senate district`),
      # br(),
      "Source: MHVillage"
    )
    map <- map %>% addMarkers(lng = as.numeric(df$longitude),
                              lat = as.numeric(df$latitude),
                              group = "MHVillage Markers (includes site info)",
                              popup = df$label,
                              clusterOptions = markerClusterOptions())
  } else if (layer_name == "MHVillage Circles (location only)") {
    map <- map %>% addCircleMarkers(lng = as.numeric(df$longitude),
                                    lat = as.numeric(df$latitude),
                                    color = "maroon",
                                    opacity = 0.9,
                                    radius = 1,
                                    fillOpacity = 1,
                                    group = "MHVillage Circles (location only)")
  }
  return(map)
}


#Most Important Thing PART 1
#creates the "template" on which all of the features sit
ui <- navbarPage( 
  imageOutput("", inline = TRUE),
  # First tab with existing content
  
  tabPanel(
    "Map Tool",
    fluidPage(
      titlePanel("Ginsberg Center Community Partners"),
      fluidRow(
        column(
          width = 5,
          card(
            card_header(h6("Instructions")),
            HTML("
              <ol>
                <li>Select the layers you would like to view.
                  <ul>
                    <li>Markers will include additional information in a Pop-up message.</li>
                    <li>Circle layers will only pinpoint the coordinates of each MHC.</li>
                    <li>LARA Circles are selected as default.</li>
                  </ul>
                </li>
              </ol>
            "),
            selectizeInput("layerlist", "Choose a Layer:", choices = c("Base Map Only", 
                                                                       "LARA Circles (location only)",
                                                                       "LARA Markers (includes site info)",
                                                                       "MHVillage Circles (location only)",
                                                                       "MHVillage Markers (includes site info)"),
                           
                           selected = "LARA Circles (location only)",
                           multiple = FALSE),
            HTML("
              <ol start='2'>
                <li>Select a district layer in the upper-right corner of the map.</li>
                <br> Note: To visualize circle sites clearly, please add a House or Senate districting layer before selecting a marker.
              </ol>
            "),
            height = "500px"
          )
        ),
        column(
          width = 7,
          card(
            leafletOutput("leafletMap"),
            full_screen = TRUE,
            height = "800px")
        )
      )
    )),
  #About page UI
  tabPanel(
    "About",
    page_fillable(
      titlePanel(
        "Manufactured Housing Communities in Michigan"
      ),
      p("By ", 
        a("INFORMS", href = "https://informs.engin.umich.edu/", target = "_blank"), 
        " and ", 
        a("CTAC", href = "https://ginsberg.umich.edu/ctac", target = "_blank"),
        " at the University of Michigan")), br(),
    layout_columns(
      card(
        card_title("About"),
        p("The MHAction Mapping Tool is a visualization application designed to highlight the distribution of manufactured housing communities (MHC's) across the state of Michigan. The project began in late December 2023, initially between INFORMs and MHAction.
            In May 2024, INFORMs transitioned the project to the Community Technical Assistance Collaborative under the Ginsberg Center for Community Service and Learning. 
            The new development team aimed to improve the number of users allowed on the webpage, transitioning the webpage to GitHub pages. This allows for easy access to source data and code files in the future, as well as improved deployment capacity.
             Additional functional capabilities were added, including downloadable .csv files, user interface (UI) improvements, and data visibility. Package versioning was not optimized under the Python Shiny GitHub Pages website, so the app was re-written in RShiny using the automatic deployment tool."),
        p("Two sources of data were used to create this application. Michigan Department of Licensing and Regulatory Affairs (LARA) data was obtained by a FOIA request January 2026. This contains a complete list of MHC's in the state of Michigan, per state regulatory guidelines for registration. 
            MHVillage data was obtained from the website in February 2024. The data sources can differ significantly, including by MHC name and management, and MHVillage data is typically incomplete. However, it is important to include data from both sources, as 
            this enhances the reliability and accuracy of the map visualization tool by allowing users to cross-verify information and identify discrepancies."),
        p("Please note that data is updated annually."),
        p("State House and Senate districting information is essential to help identify legislators and communities most significantly impacted by laws surrounding Manufactured Housing Communities. State districting information was based off of the 2021 Linden (State Senate) and Hickory (State House) maps. 
            These were drafted by the Michigan Independent Citizens Redistricting Commission following the decennial US Census results in 2020."),
        p("For more information, please visit ",
          a("MHAction.org", href = "https://www.mhaction.org/", target = "_blank"))
      )),
    layout_columns(
      card(
        card_title("Navigation"),
        "About: Background information for this MHAction mapping project.",
        br(), "Map Tool: Visualize Manufactured Housing Communities and view data for the state of Michigan.",
        br(), "Infographics: Static bar graphs demonstrating MHVillage and LARA dataset capabilities. Also allows user to download full table data.",
        br(), "Tables: Dynamic tables that output MHC rows based on data source, geographic boundary type, and boundary selections. Users can download a full table with County, House, and Senate districting numbers.",
        br(), "Other: Credits, source files, and additional information."
      )
    ),
    layout_columns(
      card(
        card_title("Key Terms"),
        "MHC: Manufactured Housing Community",
        br(), "LARA: Michigan Department of Licensing and Regulatory Affairs. Michigan MHC's are required to register with LARA.",
        br(), "MHVillage: Online marketplace for buying and selling manufactured homes. Data may be incomplete."))),
  
  
  #Site List Tables Page UI
  tabPanel(
    "MHC Site List Tables",
    fluidPage(
      titlePanel("Tables"),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            selectInput("datasource", "Select LARA or MHVillage Data Source:", choices = c("LARA", "MHVillage")),
            selectInput("main_category", "Select a Geographic Boundary Type:", choices = c("County", "House district", "Senate district")),
            uiOutput("sub_category_ui"),
            h6("Site List Summary"),
            tableOutput("site_list_summary"),
            downloadButton("site_list_download", "Download Site List")
            # br(), br(),
            # h5("Interested in the full district communities?"),
            # downloadLink("mhvillage_all", "Download MHVillage data"),
            # br(),
            # downloadLink("lara_all", "Download LARA data")
          )
        ),
        column(
          width = 8,
          tableOutput("site_list")
        )
      )
    ))
  
  
)


#Most Important PART 2
# actually reflects the changes made in the UI on the resulting website
server <- function(input, output, session) {
  
  #creates an empty list
  # circlelist_mh <- list()
  # mklist_mh <- list()
  # circlelist_lara <- list()
  # mklist_lara <- list()
  # 
  # 
  # #renders the logos onto the website
  # output$ctac_logo <- renderImage({
  #   
  #   list(src = "ctac_logo.png",
  #        height = 60)
  #   
  # }, deleteFile = F)
  # 
  # output$mhaction_logo <- renderImage({
  #   
  #   list(src = "mhaction_logo.png",
  #        height = 50)
  #   
  # }, deleteFile = F)
  # 
  # output$mhaction_logo_large <- renderImage({
  #   
  #   list(src = "mhaction_logo.png",
  #        height = 80)
  #   
  # }, deleteFile = F)
  # 
  # output$informs_logo <- renderImage({
  #   
  #   list(src = "informs_logo.png",
  #        height = 70)
  #   
  # }, deleteFile = F)
  
  #renders the map based on selections in the UI
  output$leafletMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -85.6024, lat = 44.3148, zoom = 6) #%>%
      # addPolygons(
      #   data = house_districts,
      #   weight = 1,
      #   opacity = 1,
      #   color = "green",
      #   fillOpacity = 0.4,
      #   label = ~paste0("House District: ", as.integer(NAME)),
      #   labelOptions = labelOptions(
      #     style = list("color" = "black", "font-size" = "12px"),
      #     textOnly = TRUE
      #   ),
      #   highlightOptions = highlightOptions(
      #     weight = 1,
      #     color = "yellow",
      #     fillColor =  "orange",
      #     bringToFront = FALSE
      #   ),
      #   group = "House Districts"
      # ) %>%
      # addPolygons(
      #   data = senate_districts,
      #   weight = 1,
      #   opacity = 1,
      #   color = "purple",
      #   fillOpacity = 0.4,
      #   label = ~paste0("Senate District: ", as.integer(NAME)),
      #   labelOptions = labelOptions(
      #     style = list("color" = "black", "font-size" = "12px"),
      #     textOnly = TRUE
      #   ),
      #   highlightOptions = highlightOptions(
      #     weight = 1,
      #     color = "yellow",
      #     fillColor =  "orange",
      #     bringToFront = FALSE
      #   ),
      #   group = "Senate Districts"
      # ) %>%
      # addLayersControl(
      #   overlayGroups = c("House Districts", "Senate Districts"),
      #   options = layersControlOptions(collapsed = TRUE)
      # ) %>%
      # hideGroup(c("House Districts", "Senate Districts"))
  })
  
  #updates layers and clusters based on UI selections that occur
  observeEvent(input$layerlist, {
    leafletProxy("leafletMap") %>%
      clearMarkers() %>% 
      clearMarkerClusters()
    
    map <- leafletProxy("leafletMap")
    
    for (layer in input$layerlist) {
      if (layer != " ") {
        map <- build_marker_layer(map, layer)
      }
    }
  })

  
  #renders site list
  # total_sites_by_county <- df %>%
  #   filter(!is.na(`Total_#_Sites`)) %>%
  #   group_by(County) %>%
  #   summarise(Total_Sites = sum(`Total_#_Sites`, na.rm = TRUE)) %>%
  #   arrange(desc(Total_Sites))
  # 
  # 
  # total_sites_by_name <- mhvillage_df %>%
  #   group_by(County) %>%
  #   summarise(Average_rent = mean(Average_rent, na.rm = TRUE)) %>%
  #   arrange(Average_rent)
  # 
  # df_clean <- mhvillage_df %>%
  #   select(County, Average_rent)
  # 
  # county_counts <- df_clean %>%
  #   count(County)
  # 
  # total_sites_by_name_count <- merge(total_sites_by_name, county_counts, by = "County")
  # 
  # 
  # 
  # observeEvent(input$main_category, {
  #   if (input$datasource == 'MHVillage') {
  #     if (input$main_category == 'County') {
  #       subcategory_choices <- sort(unique(mhvillage_df$County))
  #     } else {
  #       subcategory_choices <- sort(unique(mhvillage_df[[input$main_category]]))
  #     }
  #   } else {
  #     if (input$main_category == 'County') {
  #       subcategory_choices <- sort(unique(lara_df$County))
  #     } else {
  #       subcategory_choices <- sort(unique(lara_df[[input$main_category]]))
  #     }
  #   }
  #   
  #   updateSelectInput(session, "sub_category", choices = subcategory_choices)
  # })
  # 
  # 
  # output$sub_category_ui <- renderUI({
  #   selectInput("sub_category", "Select County/District Boundary:", choices = c())
  # })
  
  # reactive_site_list <- reactive({
  #   req(input$datasource)
  #   req(input$main_category)
  #   req(input$sub_category)
  #   
  #   if (input$datasource == 'MHVillage') {
  #     if (input$main_category == 'County') {
  #       df <- mhvillage_df %>%
  #         filter(County == input$sub_category) %>%
  #         select(Name, Sites, FullstreetAddress) %>%
  #         rename(`Number of Sites` = Sites,
  #                Address = FullstreetAddress)
  #       
  #       df <- df %>%
  #         arrange(desc(`Number of Sites`)) %>%
  #         mutate(`Number of Sites` = as.integer(`Number of Sites`))
  #     } else {
  #       df <- mhvillage_df %>%
  #         filter(!!sym(input$main_category) == as.integer(as.numeric(input$sub_category))) %>%
  #         select(Name, Sites, FullstreetAddress) %>%
  #         rename(`Number of Sites` = Sites,
  #                Address = FullstreetAddress)
  #       
  #       df <- df %>%
  #         arrange(desc(`Number of Sites`)) %>%
  #         mutate(`Number of Sites` = as.integer(`Number of Sites`))
  #     }
  #     
  #   } else {
  #     if (input$main_category == 'County') {
  #       df <- lara_df %>%
  #         filter(County == input$sub_category) %>%
  #         select(DBA, `Owner / Community_Name`, `Total_#_Sites`, `Location_Address`)
  #     } else {
  #       house_district <- as.integer(as.numeric(input$sub_category))
  #       df <- lara_df %>%
  #         filter(!!sym(input$main_category) == house_district) %>%
  #         select(DBA, `Owner / Community_Name`, `Total_#_Sites`, `Location_Address`)
  #     }
  #     
  #     df <- df %>%
  #       mutate(Name = ifelse(!is.na(DBA) & DBA != '', DBA, `Owner / Community_Name`)) %>%
  #       select(-DBA, -`Owner / Community_Name`) %>%
  #       rename(`Number of Sites` = `Total_#_Sites`,
  #              Address = `Location_Address`)
  #     
  #     df <- df %>%
  #       arrange(desc(`Number of Sites`)) %>%
  #       mutate(`Number of Sites` = as.integer(`Number of Sites`)) %>%
  #       select(Name, `Number of Sites`, `Address`)
  #   }
  #   
  #   return(df)
  # })
  
  # sub_categories <- reactiveVal(c())
  # 
  # # Update sub_category_ui based on main_category
  # observe({
  #   req(input$main_category) # Ensure that input$main_category is available
  #   if (input$main_category == "County") {
  #     # Example values, replace with your actual values
  #     sub_categories(mi_counties$NAME)
  #   } else if (input$main_category == "House district") {
  #     sub_categories(c("District 1", "District 2", "District 3"))
  #   } else if (input$main_category == "Senate district") {
  #     sub_categories(c("Senate 1", "Senate 2", "Senate 3"))
  #   }
  #   
  #   output$sub_category_ui <- renderUI({
  #     selectInput("sub_category", "Select a County, House, or Senate District: ", choices = sub_categories(), selected = sub_categories()[1])
  #   })
  # })
  
  # Update table based on selected inputs
  # output$site_list <- renderTable({
  #   req(input$sub_category)
  #   reactive_site_list()
  # })
  # 
  # output$site_list_summary <- renderTable({
  #   df <- reactive_site_list()
  #   if (nrow(df) > 0) {
  #     summary_df <- data.frame(
  #       "Number of MHC's" = nrow(df),
  #       "Total Sites" = sum(df$`Number of Sites`, na.rm = TRUE)
  #     ) 
  #   } else {
  #     summary_df <- data.frame(
  #       "Number of MHC's" = 0,
  #       "Total Sites" = 0
  #     )
  #   }
  #   summary_df %>%
  #     rename_with(~ gsub("\\.", " ", .))
  # })
  # 
  # 
  # observeEvent(input$main_category, {
  #   if (input$datasource == 'MHVillage') {
  #     if (input$main_category == 'County') {
  #       subcategory_choices <- sort(unique(mhvillage_df$County))
  #     } else {
  #       subcategory_choices <- sort(unique(mhvillage_df[[input$main_category]]))
  #     }
  #   } else {
  #     if (input$main_category == 'County') {
  #       subcategory_choices <- sort(unique(lara_df$County))
  #     } else {
  #       subcategory_choices <- sort(unique(lara_df[[input$main_category]]))
  #     }
  #   }
  #   
  #   updateSelectInput(session, "sub_category", choices = subcategory_choices)
  # })
  # 
  # output$sub_category_ui <- renderUI({
  #   selectInput("sub_category", "Select County/District Boundary:", choices = c())
  # })
  # 
  # reactive_site_list <- reactive({
  #   req(input$datasource)
  #   req(input$main_category)
  #   req(input$sub_category)
  #   
  #   if (input$datasource == 'MHVillage') {
  #     if (input$main_category == 'County') {
  #       df <- mhvillage_df %>%
  #         filter(County == input$sub_category) %>%
  #         select(Name, Sites, FullstreetAddress) %>%
  #         rename(`Number of Sites` = Sites,
  #                Address = FullstreetAddress)
  #       
  #       df <- df %>%
  #         arrange(desc(`Number of Sites`)) %>%
  #         mutate(`Number of Sites` = as.integer(`Number of Sites`))
  #     } else {
  #       df <- mhvillage_df %>%
  #         filter(!!sym(input$main_category) == as.integer(as.numeric(input$sub_category))) %>%
  #         select(Name, Sites, FullstreetAddress) %>%
  #         rename(`Number of Sites` = Sites,
  #                Address = FullstreetAddress)
  #       
  #       df <- df %>%
  #         arrange(desc(`Number of Sites`)) %>%
  #         mutate(`Number of Sites` = as.integer(`Number of Sites`))
  #     }
  #     
  #   } else {
  #     if (input$main_category == 'County') {
  #       df <- lara_df %>%
  #         filter(County == input$sub_category) %>%
  #         select(DBA, `Owner / Community_Name`, `Total_#_Sites`, `Location_Address`)
  #     } else {
  #       house_district <- as.integer(as.numeric(input$sub_category))
  #       df <- lara_df %>%
  #         filter(!!sym(input$main_category) == house_district) %>%
  #         select(DBA, `Owner / Community_Name`, `Total_#_Sites`, `Location_Address`)
  #     }
  #     
  #     df <- df %>%
  #       mutate(Name = ifelse(!is.na(DBA) & DBA != '', DBA, `Owner / Community_Name`)) %>%
  #       select(-DBA, -`Owner / Community_Name`) %>%
  #       rename(`Number of Sites` = `Total_#_Sites`,
  #              Address = `Location_Address`)
  #     
  #     df <- df %>%
  #       arrange(desc(`Number of Sites`)) %>%
  #       mutate(`Number of Sites` = as.integer(`Number of Sites`)) %>%
  #       select(Name, Address, `Number of Sites`)
  #   }
  #   
  #   return(df)
  # })
  # 
  # output$site_list <- renderTable({
  #   reactive_site_list()
  # })
  # 
  # output$site_list_summary <- renderTable({
  #   df <- reactive_site_list()
  #   if (nrow(df) > 0) {
  #     summary_df <- data.frame(
  #       "Number of MHC's" = nrow(df),
  #       "Total Sites" = sum(df$`Number of Sites`, na.rm = TRUE)
  #     ) 
  #   } else {
  #     summary_df <- data.frame(
  #       "Number of MHC's" = 0,
  #       "Total Sites" = 0
  #     )
  #   }
  #   summary_df %>%
  #     rename_with(~ gsub("\\.", " ", .))
  # })
  # 
  # output$site_list_download <- downloadHandler(
  #   filename = function() {
  #     paste("site_list_", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(reactive_site_list(), file, row.names = FALSE)
  #   }
  # )
  
  
}

shinyApp(ui = ui, server = server)