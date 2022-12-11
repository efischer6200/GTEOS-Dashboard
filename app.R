
##########################
#Header
##########################

#Install packages 
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("shinydashboardplus")
#install.packages("leaflet")
#install.packages("leaflet.extras")
#install.packages("rgdal")
#install.packages("dplyr")
#install.packages("flextable")
#install.packages("officer")
#install.packages("ggplot2")

# shiny libraries 
library(shiny)
#
library(shinydashboard)
#
library(shinydashboardPlus)


#Additional libraries for dashboard components 
library(leaflet)
#
library(dplyr)
#
library(leaflet.extras)
#
library(rgdal)
#
library(flextable)
#
library(officer)
#
library(ggplot2)

#import data

###MAKE SURE FILE PATH IS CORRECT FOR IMAGES HELP1&2###

FISHGUIDE_2020 <- readRDS("FISHGUIDE_2020_MAP.rds")
FISHGUIDE_2020_T <- readRDS("FISHGUIDE_2020_TABLE.rds")
Fish_Bound <- readRDS("FishBound.rds")
# help1<-'C:/Users/Erich Fischer/downloads/Help1.png'
# help2<-'C:/Users/Erich Fischer/downloads/Help2.png'



#Change Toxin ID label
names(FISHGUIDE_2020_T)[27] <- "Toxin ID"


#set boarder and icon styling

brdr = fp_border_default()
brdr2 = fp_border(color = 'black', style = 'solid', width=3)

icons <- awesomeIcons(
  icon = 'circle',
  iconColor = "#FFFFFF",
  library = 'fa',
  markerColor = 'blue'
)
icons2 <- awesomeIcons(
  icon = 'circle',
  iconColor = "#FFFFFF",
  library = 'fa',
  markerColor = 'black'
)
icons3 <- awesomeIcons(
  icon = 'circle',
  iconColor = "#FFFFFF",
  library = 'fa',
  markerColor = 'green'
)

marker_options <- markerOptions(
  zIndexOffset = 100
)
# DASHBOARD HEADER ############################################################### 

header_img<-img(src="	https://www.oacph.ca/docs/new-government-of-ontario-logo/?bp-attachment=ON_POS_LOGO_RGB.png", height="55px") #image of gov ont
header_txt <- h1(strong("GUIDE TO EATING SPORTFISH"), style = "font-size:19px;color: white;") # title for dashboard

header <-  htmltools::tagQuery(dashboardHeader(title = header_img, titleWidth = 300, userOutput("user")))  
header <- header$
  addAttrs(style = "position: relative")$ # change position
  find(".navbar.navbar-static-top")$ # find the header
  append(header_txt)$ # Place ontario image at top
  allTags()

# DASHBOARD SIDEBAR ##############################################################

# Create menu tab (General Search)
sidebar <- dashboardSidebar(width = 300,minified = FALSE,
  sidebarMenu(id="menu1", #Creates a menu of tab names 
              menuItem("General Search", tabName="general"),
              tags$hr() # solid line
              ),
  
  #Conditional panel that will only show features when selected (general search)
  conditionalPanel(condition = "input.menu1=='general'",
                   
                   p("Please make selections from the dropdown menus or by selecting features from the map"),
                   selectInput(
                     inputId = "FMZ",
                     label = "Select FMZ",
                     choices = c("", "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"),
                     ),
                  
                  selectInput(
                    inputId = "LAKE",
                    label = "Select Waterbody",
                    choices = ""
                    ),
                  selectInput(
                    inputId = "SUBLAKE",
                    label = "Select Waterbody Division",
                    choices = ""
                    )
                  ), 
  
  #Create tab for (Advanced Search)
  sidebarMenu(id="menu1", #Creates a menu of tab names 
              menuItem("Advanced Search", tabName="advanced"),
              tags$hr()),
  
  #Conditional panel that will only show features when selected (Advanced search)
  conditionalPanel(condition = "input.menu1 == 'advanced'",
                   p("Please make selections from the dropdown meanus and sliding bars"),
                   selectInput(
                     inputId = "FISH",
                     label = "Select Sportfish",
                     choices = ""
                   ),  
                   sliderInput("LN", "Maximum Fish Length (cm):", 15, 80, 15,step = 5),
                   sliderInput("FIL", "Maximum Monthly Fillet Consumption (fillet/month):", 1, 60, 1),
                   tags$hr(),
                   checkboxInput("POP", "Please check box if you are a women of child-bearing age or under the age of 15 ", FALSE),
                   checkboxInput("DIS", "Click to view proportion of lakes with selected criteria by FMZ", FALSE),
                  ),
  
  # Create sidebar menu for additional information
  sidebarMenu(id="menu1", #Creates a menu of tab names
              menuItem("Information", tabName="INFO")
             ),
 
  # Conditional panel that will only show features when selected (information)
  conditionalPanel(condition = "input.menu1 == 'INFO'",
                   tags$hr(),
                   actionButton("TOU", "Terms of Use"),
                   actionButton("PD", "Data Sources")
                  )
  )

# DASHBOARD BODY ##############################################################

body <- dashboardBody(

  #change table colours etc.
  tags$style(type = "text/css", "#mymap {height: calc(100vh-490px) !important;}", # position and size leaflet map
             ".nav-tabs-custom .nav-tabs li.active {border-top-color: #00a65a;}"),
  
  tags$head(tags$style(HTML('.skin-green .main-sidebar {background-color:	#B8B8B8;}',
                            '.skin-green .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #454545;
                              color: #FDFEFE ;
                            }')
                       )
            ), 
  
  #make leaflet map 
  leafletOutput(outputId = "mymap"),
  
  # tabs for general 
  tabItems(
  tabItem(tabName = 'general',
          fluidRow(
            tabBox(
              id = "tabset1", height = 490, width = 12,
              tabPanel("Fish Consumption Table", 
                       h4(textOutput("txt1")),
                       (div(style='height:385px;overflow-y: scroll;',uiOutput("table1")))),
              tabPanel("Help?",h4("See labels for description of table items"), (div(style='height:385px;overflow-y: scroll;text-align: center;',imageOutput("Help1"))))
                  )
                  )
          ),
  
######################################################### Split tabs

  # tabs for advanced
  tabItem(tabName = 'advanced',
  fluidRow(
    tabBox(
      id = "tabset2", height = 490, width = 12,
      tabPanel("Fish Consumption Chart", 
               h4(textOutput("txt2")),
               plotOutput("PLOT", height = "385px")),
      tabPanel("Help?",h4("See labels for description of chart items"), (div(style='height:385px;overflow-y: scroll;scroll;text-align: center;',imageOutput("Help2"))))
          )
          )
        )
      )
)

# DASHBOARD UI COMBINE ######################################################
ui<-dashboardPage(header,sidebar, body, skin = 'green')


# SERVER ####################################################################
server = function(input, output, session) {
  
  #modal on startup with welcome message and purpose
  showModal(modalDialog(
    tags$h3('Welcome to the Ontario Guide to Eating Sportfish Dashboard'),
    size="m",
    tags$hr(),
    p("The purpose of this dashboard is to create an interactive platform where users can easily access sportfish
      consumption advisories for most of Ontario’s lakes and rivers. The dashboard provides consumption guidelines
      by waterbody, fish species, fish length and monthly fillet consumption, allowing users to make informed decisions when consuming fish. Regardless of your
      fishing style or ability, sportfish guidelines can be finely tuned with search criteria to accommodate your specific
      requirements."),

    footer=tagList(
      modalButton('Get Started')
      )
    )
  ) 
  
  
  #first help image
  output$Help1<-renderImage({list(src="help1.png")
                                    },deleteFile =FALSE)
  #second help image  
  output$Help2<-renderImage({list(src="help2.png")
  },deleteFile =FALSE)
  
  #terms of use when action button selected 
  observeEvent(input$TOU,{
      showModal(modalDialog(
    tags$h3('TERMS OF USE'),
    size="m",
    tags$hr(),
    p("This dashboard was created by Erich Fischer using rShiny, and data obtained from Ontario Geohub and Open Government in accordance with the terms and conditions outlined in Open Government Licence - Ontario. Use of this dashboard and its associated data, maps, tables, plots and all additional content is subject to the terms and conditions set out below(“Terms of Use”)."),
    
    p("This dashboard is made for private viewing by permission of the author. No part of this dashboard including but not limited to any charts, maps, tables or plots shall be used without permission of the author.\n"),
    
    p("All data used in this dashboard is publicly available through the Government of Ontario and additional information can be found under (Information -> Data Source). This dashboard and its content are made available by the author for private viewing without warranties of any kind.
    This dashboard is meant to advise the user on the appropriate consumption of sportfish in Ontario, any use outside the intended purpose is not permitted. The author does not guarantee the authenticity, reliability or accuracy of the information found on this dashboard.
    As such, by using this dashboard you acknowledge that the author is not responsible in any way for loss, damage or injury, arising from the use or reliance on this dashboard. Use of this dashboard does not relinquish the user’s responsibility to consult local laws and regulations that may apply."),
    footer=tagList(
      actionButton('agree', 'Agree'),
      modalButton('cancel')
      )
     )
    )
  })
  #data sources and links
  observeEvent(input$PD,{
    showModal(modalDialog(
      tags$h3('Data Source'),
      size="m",
      tags$hr(),
      p("The following datasets were used to create this dashboard."),
      br(),
      
      h4("Fish Contaminant Monitoring Locations"),
      p("Available from - ", a(href="https://open.canada.ca/data/en/dataset/d4551ff4-be93-4d16-ad84-812e5ba0b1d5","https://open.canada.ca/data/en/dataset/d4551ff4-be93-4d16-ad84-812e5ba0b1d5")),
      p("Link to metadata - ", a(href="https://open.canada.ca/data/en/dataset/d4551ff4-be93-4d16-ad84-812e5ba0b1d5/resource/ac72daed-c6bc-4d02-b5e8-ebd8a720606a","https://open.canada.ca/data/en/dataset/d4551ff4-be93-4d16-ad84-812e5ba0b1d5/resource/ac72daed-c6bc-4d02-b5e8-ebd8a720606a")),
      p("Attributes used in dashboard: Lake Name, Fish Species, Advised Monthly Consumption, Location, Contaminant ID"),
      br(),
      
      h4("Recreational Fishing Regulations Data"),
      p("Available from - ", a(href="https://data.ontario.ca/dataset/74d0fda9-a83b-4191-a6b8-1b6109057f91/resource/e4892489-e6b6-466f-be12-9dc4db5e4860/download/fishingregulationsexceptionopendata_2022-01-01.xlsx","https://data.ontario.ca/dataset/74d0fda9-a83b-4191-a6b8-1b6109057f91/resource/e4892489-e6b6-466f-be12-9dc4db5e4860/download/fishingregulationsexceptionopendata_2022-01-01.xlsx")),
      p("Link to metadata - ", a(href="https://data.ontario.ca/dataset/recreational-fishing-regulations-data/resource/e4892489-e6b6-466f-be12-9dc4db5e4860","https://data.ontario.ca/dataset/recreational-fishing-regulations-data/resource/e4892489-e6b6-466f-be12-9dc4db5e4860")),
      p("Attributes used in dashboard: Lake Name, Location, Fisheries Management Zone"),
      br(),
      
      h4("Fisheries Management Zone"),
      p("Available from - ",a(href="https://geohub.lio.gov.on.ca/datasets/lio::fisheries-management-zone/explore?location=50.580480%2C-84.745000%2C4.74","https://geohub.lio.gov.on.ca/datasets/lio::fisheries-management-zone/explore?location=50.580480%2C-84.745000%2C4.74")),
      p("Link to metadata - ", a(href="https://geohub.lio.gov.on.ca/datasets/lio::fisheries-management-zone/about","https://geohub.lio.gov.on.ca/datasets/lio::fisheries-management-zone/about")),
      p("Attributes used in dashboard: Fisheries Management Zone shapefile"),
      
        footer=tagList(
        modalButton('Close')
        )
       )
      )
   })

  #dasboard author and date created 
  output$user <- renderUser({
    dashboardUser(
      name = "", 
      image = "https://media-exp1.licdn.com/dms/image/C4D03AQGrdCY5OAU1vA/profile-displayphoto-shrink_800_800/0/1579569975075?e=2147483647&v=beta&t=GjVGZAPrDkR6pvO2unQ0_xXdqmQwdxBwjkAhYv_WRQc", 
      title = "Erich Fischer-",
      subtitle = "Author", 
      footer = p("email: efischer@uoguelph.ca", class = "text-center",br(), p("Date Created: 2022-12-08",class = "text-center")
                 )
                )
    })
  

     
  #create the main map
  output$mymap <- renderLeaflet({
      leaflet() %>%
      addTiles() %>%
      addPolygons(data = Fish_Bound, weight = 1, smoothFactor = 0.5,layerId = ~FMZ_ID,
                  opacity = 1.0, fillOpacity = 0, color = 'blue',label=~as.character(Fish_Bound$FMZ_ID),
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE))
                  })
  

  ########### these change the choices based on user selection 

  observe({
    if (input$FMZ>0) {
  ##################################################
    observe({
    LTC = filter(FISHGUIDE_2020, FMZONE == input$FMZ)

    LTC<-LTC %>% distinct(GUIDE_LOCNAME_ENG, .keep_all = TRUE)
    choicesLTC<- LTC$GUIDE_LOCNAME_ENG
    updateSelectInput(session,"LAKE", choices =  c("",choicesLTC))
   })

  ##################################################
  }})

  observe({
    if (input$FMZ>0) {
      ##################################################
      observe({
        FC = filter(FISHGUIDE_2020, FMZONE == input$FMZ)
        #
        # if(input$LAKE!=""){
        # FC = filter(FISHGUIDE_2020, GUIDE_LOCNAME_ENG == input$LAKE)
        # FC = filter(FISHGUIDE_2020, GUIDE_LOCDESC == input$SUBLAKE)
        # }
        FC<-FC %>% distinct(SPECNAME, .keep_all = TRUE)
        choicesFC<- FC$SPECNAME
        updateSelectInput(session,"FISH", choices =  c("",choicesFC))
      })

      ##################################################
    }})
  observe({
    if (input$LAKE!="") {
      ##################################################
      observe({
        DC = filter(FISHGUIDE_2020, GUIDE_LOCNAME_ENG == input$LAKE)
        #
        DC<-DC %>% distinct(GUIDE_LOCDESC, .keep_all = TRUE)
        choicesDC<- DC$GUIDE_LOCDESC
        updateSelectInput(session,"SUBLAKE", choices = choicesDC)})
              } 
    else{
  observe({
      DC = filter(FISHGUIDE_2020, GUIDE_LOCNAME_ENG == input$LAKE)
      #
      DC<-DC %>% distinct(GUIDE_LOCDESC, .keep_all = TRUE)
      choicesDC<- DC$GUIDE_LOCDESC
      updateSelectInput(session,"SUBLAKE", choices = c("",choicesDC))})
          }
      ##################################################
    })


  # adds initial markers when FMZ is selected 
    observe({
      proxy <- leafletProxy("mymap")
    if (input$FMZ>0 & input$FISH=="") {

      LakeZone <- filter(FISHGUIDE_2020, FMZONE == input$FMZ)
      LakeZone <-LakeZone%>%distinct(GUIDE_LOCNAME_ENG, .keep_all = TRUE)

      proxy %>% clearGroup("FMZ")
      proxy %>% clearGroup("FTR")
      proxy%>%addAwesomeMarkers(data =LakeZone,group = "FMZ", lat = ~ Latitude, lng = ~ Longitude, layerId = ~GUIDE_LOCNAME_ENG, icon=icons, label=~as.character(GUIDE_LOCNAME_ENG))
      }
    })
      
      # adds selected lake marker
    observe({
      proxy <- leafletProxy("mymap")
      if (input$FMZ>0 & input$LAKE!="") {
      
        LakeZone2 <- filter(FISHGUIDE_2020, FMZONE == input$FMZ)
        LakeZone2 <- filter(LakeZone2, GUIDE_LOCNAME_ENG == input$LAKE)
        LakeZone2 <-LakeZone2%>%distinct(GUIDE_LOCNAME_ENG, .keep_all = TRUE)
        proxy %>% clearGroup("SLCT")
        if (nrow(LakeZone2)>0){
        proxy%>%addAwesomeMarkers(data =LakeZone2,group = "SLCT", lat = ~ Latitude, lng = ~ Longitude, icon = icons3 ,layerId = "SCLT", label=~as.character(GUIDE_LOCNAME_ENG),options = marker_options)
        }
      }
      else {
        proxy %>% clearGroup("SLCT")
      }
    })
      
      # filters markers for advanced selection 
      observe({
        proxy <- leafletProxy("mymap")
        if (input$FMZ>0 & input$FISH!="") {
          
          proxy %>% clearGroup("FMZ")
          
          
          LakeZone = filter(FISHGUIDE_2020, SPECNAME == input$FISH)
          LakeZone = filter(LakeZone, FMZONE == input$FMZ)
          Criteria = filter(LakeZone, LENGTH_CATEGORY_ID < (input$LN))
         
          
          #########
          if (input$POP){
            Criteria <-filter(Criteria, POPULATION_TYPE_DESC == 'S')
          }else{
            Criteria <-filter(Criteria, POPULATION_TYPE_DESC == 'G')
          }
          ########
          Criteria$ADV_LEVEL <- ave(Criteria$ADV_LEVEL, Criteria$GUIDE_LOCNAME_ENG, FUN = min)
          Criteria = filter(Criteria, ADV_LEVEL >= input$FIL) 
          Criteria <- Criteria%>%distinct(GUIDE_LOCNAME_ENG,.keep_all=TRUE)
          
          proxy %>% removeControl("MAIN")
          proxy %>% addLegend(layerId = "MAIN", colors = c("black",'#0099C8'), labels = c("No","yes"), title = "Meets Criteria")
          if (nrow(LakeZone)>0) {
          if (nrow(Criteria)<1) {
            proxy %>% clearGroup("FTR")
            proxy%>%addAwesomeMarkers(data = LakeZone,group = "FTR", lat = ~ Latitude, lng = ~ Longitude, layerId = ~GUIDE_LOCNAME_ENG, icon=icons2, label=~as.character(GUIDE_LOCNAME_ENG))

          }
          else{
          
              proxy %>% clearGroup("FTR")
              proxy%>%addAwesomeMarkers(data = LakeZone,group = "FTR", lat = ~ Latitude, lng = ~ Longitude, layerId = ~GUIDE_LOCNAME_ENG, icon=icons2, label=~as.character(GUIDE_LOCNAME_ENG))
              proxy%>%addAwesomeMarkers(data = Criteria,group = "FTR", lat = ~ Latitude, lng = ~ Longitude, layerId = ~GUIDE_LOCNAME_ENG, icon=icons, label=~as.character(GUIDE_LOCNAME_ENG))

           }
          }
        }
      })  
      
      # choropleth map
      observe({
        proxy <- leafletProxy("mymap")
        if (input$DIS & input$FISH!="") {
          
          Criteria1 = filter(FISHGUIDE_2020, SPECNAME == input$FISH)
          Criteria = filter(Criteria1, LENGTH_CATEGORY_ID  < (input$LN))
          #########
          if (input$POP){
            Criteria <-filter(Criteria, POPULATION_TYPE_DESC == 'S')
          }else{
            Criteria <-filter(Criteria, POPULATION_TYPE_DESC == 'G')
          }
          #########
          Criteria$ADV_LEVEL <- ave(Criteria$ADV_LEVEL, Criteria$GUIDE_LOCNAME_ENG, FUN = min)
          Criteria = filter(Criteria, ADV_LEVEL >= input$FIL) 
          
          if (nrow(Criteria)>1) {
            y <- Criteria%>%distinct(GUIDE_LOCNAME_ENG, .keep_all = TRUE)
            y<-y%>%count(FMZONE)
            y2<-Criteria1%>%distinct(GUIDE_LOCNAME_ENG, .keep_all = TRUE)
            y2<-y2%>%count(FMZONE)
            y$m<-y2$n[match(y$FMZONE, y2$FMZONE)]
            y$div<-(y$n/y$m)*100
            
            Fish_Bound$x<-y$div[match(Fish_Bound$FMZ_ID, y$FMZONE)]
            
            pal <- colorNumeric(
              palette = "YlOrRd",
              domain = c(0,100),
              reverse = TRUE)
            
            proxy %>% clearGroup("SHP")
            proxy %>% removeControl("LGND")
            proxy %>%addPolygons(data = Fish_Bound, weight = 1, smoothFactor = 0.5,group = "SHP",layerId = ~FMZ_ID,
                                 opacity = 1.0, fillOpacity = 0.8, fillColor = ~pal(x),label=~as.character(Fish_Bound$FMZ_ID),
                                 highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE))%>%
              addLegend("topright", pal = pal,layerId = "LGND",
                        values = c(0,100),
                        title = "Percentage of<br> waterbodies that<br> meet criteria",
                        opacity = 1
              )
            
           }
          } 
        else{
            proxy %>% clearGroup("SHP")
            proxy %>% removeControl("LGND")
            
           }
      })
      
      
      ##### TABLE AND CHART
      
      observe({
        if (input$LAKE!=""){
          df<- filter(FISHGUIDE_2020, GUIDE_LOCNAME_ENG == input$LAKE)
          if (n_distinct(df$GUIDE_LOCDESC)>1){

            showModal(modalDialog(
            tags$h3('Please select a Waterbody division'),
            size="s",
            selectInput(
              inputId = "SELECT",
              label = "Select Waterbody",
              choices = c("",df$GUIDE_LOCDESC)
            ),
            footer=tagList(
              actionButton('submit', 'Submit'),
              modalButton('cancel')
            )))
        }else{ 
          updateSelectInput(session, "SUBLAKE",
                                 selected = "")
        }
      }
  })
          
          
          
          observe({
            
            if (input$SUBLAKE != "x") {
              
              
              #################### inside loop one location

                df<- filter(FISHGUIDE_2020_T, GUIDE_LOCNAME_ENG == input$LAKE)
                df<- filter(df, GUIDE_LOCDESC == input$SUBLAKE)
                tb <- df[c(7,8,14:27)]
                tb2<- filter(tb, SPECNAME == "x")
                tb<-tb %>% distinct(SPECNAME, POPULATION_TYPE_DESC, .keep_all = TRUE)

                
                ft <- flextable(tb)
                ft<- theme_box(ft)
                ft<- border_inner_v(ft, border=brdr)
                ft <- hline_top(ft,border=brdr2)
                ft <- hline(ft, i=seq(2,nrow(tb),2),border=brdr2)
                ft <- add_header_lines(ft, values=paste("",input$SUBLAKE), top=TRUE)
                ft <- add_header_lines(ft, values=paste("",input$LAKE), top=TRUE)
                ft <- merge_h(ft)
                ft<- merge_v(ft, j=1)
                ft <- fix_border_issues(ft)
                ft <- fontsize(ft, i=1, size = 14, part = "header")
                ft <- fontsize(ft, i=2, size = 8, part = "header")
                ft<-add_footer_lines(ft,values= " \n
                                      1 - Mercury\n2 - Polychlorinated biphenyls (PCBs)\n3 - Dioxin-like PCBs\n
                                     4 - Dioxins/Furans\n5 - Toxaphene \n6 - PerFluoroAlkyl and PolyFluoroAlkyl Substances (PFAS)\n
                                     7 - Selenium\n8 - Arsenic\n9 - PolyBrominated Diphenyl Ethers (PBDEs)\n10 - PolyChlorinated Naphthalenes (PCNs)\n
                                     11 - Chromium\n12 - Photomirex\n13 - Mirex\n
                                     14 - Lead\n15 - Cadmium")
                ft<-italic(ft, italic = TRUE, part = "footer")
                ft<-italic(ft, i=2, italic = TRUE, part = "header")

                ft <- set_header_labels(ft,SPECNAME = "Species",POPULATION_TYPE_DESC ="Population")
                ft<-align(ft,j = 3:16,align = "center",part = "body")
                ft<-align(ft,j = 3:16, i = 3,align = "center",part = "header")

                colnb <- ncol(tb)-1
                rownb <- nrow(tb)
                for (col in 3:(colnb)) {
                  for (row in 1:(rownb)) {

                    tg<-tb[row, col]

                    if (isTruthy(tg == 0) & isTruthy(is.na(tg)==FALSE)) {
                      ft <- color(ft, i = row, j = col, color = "red")
                    }
                    if (isTruthy(tg >= 0) & isTruthy(tb[row, 2]=='S') & isTruthy(is.na(tg)==FALSE)){

                    ft <- bg(ft, i = row, j = col, bg = "grey")

                    }
                  }
            }
                output$table1<-renderUI({ft%>%htmltools_value()})

              #Second if for plot
              
              if (input$FISH!="x") {
                
                df<- filter(FISHGUIDE_2020, GUIDE_LOCNAME_ENG == input$LAKE)
                df<- filter(df, GUIDE_LOCDESC == input$SUBLAKE)
                rg <-filter(df, SPECNAME == input$FISH)
                #########
                if (input$POP){
                  rg <-filter(rg, POPULATION_TYPE_DESC == 'S')
                }else{
                  rg <-filter(rg, POPULATION_TYPE_DESC == 'G')
                }
                #########
                rg<-rg%>%mutate(MeetsCriteria=case_when(rg$ADV_LEVEL>=input$FIL & rg$LENGTH_CATEGORY_ID <(input$LN)~ "yes",TRUE~"no"))
                rg$LENGTH_CATEGORY_ID<-rg$LENGTH_CATEGORY_ID+2.5
                
                plt<-ggplot(data=rg, aes(x=LENGTH_CATEGORY_ID, y=ADV_LEVEL,fill=MeetsCriteria)) +
                  theme_classic()+
                  scale_x_continuous(breaks=seq(15,80,5),limits = c(15,80))+
                  geom_col(color="black")+
                  scale_fill_manual(values=c('no'='#999999',
                                              'yes'='#0099C8'))+
                  geom_vline(xintercept=input$LN,color='#3CB371', size = 1)+
                  geom_hline(yintercept=input$FIL,color='#3CB371', size = 1)+
                  geom_text(aes(label = ADV_LEVEL), vjust = -1, colour = "black")+
                  labs(x = "Fish Length (cm)", y = "Advised Consumption (fillet/month)") +
                  ggtitle(paste("Advised Consumption vs Fish Length\n(", input$LAKE, ")")) +
                  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold.italic"),axis.title.x = element_text(vjust = -1, size = 13),
                  axis.title.y = element_text(vjust = 3, size = 13))+ scale_y_continuous(expand = c(0, 0),limits = c(0,65))
      
                output$PLOT <- renderPlot(plt)
              }
              
            }}) ####### END OF OBSERVE TABLE AND CHART
          
          observe({
               if (input$LAKE ==""){
               output$txt1<-renderText({
               "Select a zone and waterbody to display consumption table"
               })
              }
            else{
                output$txt1<-renderText({
                  " "
                })
              }
            })

          
          
          observeEvent (input$submit,{
                 removeModal()
            SL<- input$SELECT
            updateSelectInput(session, "SUBLAKE",
                              selected = SL)
          })
          observeEvent (input$agree,{
            removeModal()
          })
          
          observe({
            #Change first to H1
            if(input$LAKE =="" | input$FISH=="" | input$FMZ ==""){
                output$txt2<-renderText({
                  "Select a zone and waterbody and fish to display consumption chart"
                })
            }else{
              output$txt2<-renderText({
                "" 
                  })
            }
          })


  # enter FMZ when selected 
  observeEvent(input$mymap_shape_click, {
    sp <- input$mymap_shape_click
    sp <- sp$id
    updateSelectInput(session, "FMZ",
                      selected = sp)
  })  
  # enter lake when marker selected
  observeEvent(input$mymap_marker_click, {
    ln <- input$mymap_marker_click
    ln <- ln$id
    updateSelectInput(session, "LAKE",
                      selected = ln)
  })

  
} ############################### END OF SERVER ################################################




# Run Shiny App
shinyApp(ui, server)

