library(shiny)
library(shinydashboard)
library(leaflet)

ui <- function () {
  dashboardPage(
  dashboardHeader(title = "Xplorer Score",
  tags$li(
    class = "dropdown",  # Ensures it appears on the header bar
    tags$div(
      style = "display: flex; align-items: center; justify-content: flex-end; gap: 10px; padding-right: 10px;",
      tags$img(src = "BCI.png", height = "30px", style = "margin-top: 10px;"),
      tags$img(src = "eBird.png", height = "30px", style = "margin-top: 10px;")
      )
    )
  ),
  dashboardSidebar(
    tags$head( 
      tags$style(HTML("
      /* Custom styles for helpText */
      .shiny-input-container .help-block {
        color: white; /* Makes the help text white */
      }
      .shiny-input-container .help-block a {
        color: blue; /* Makes links in help text blue */
        text-decoration: underline; /* Adds an underline for links */
      }
      .shiny-input-container .help-block a:hover {
        color: #0000EE; /* Changes link color on hover */
      }
    "))
    ),    
    fileInput("ebird_zip", "Upload eBird ZIP File", accept = ".zip"),
    helpText(HTML("Upload the eBird ZIP file that you obtain from <a href='https://ebird.org/downloadMyData' target='_blank'>this link</a>. Ensure the file is the same as downloaded.")),
    helpText(HTML(paste("Your Explorer Score reflects the extent and intensity of your bird exploration. It calculates the ratio of bird species recorded by you in each district of India and averages this ratio across the entire country. The result is then normalized on a scale of 10,000. A score exceeding 300 is considered excellent!"))),
    textOutput("goodScorePlaceholder", inline = TRUE),  # Dynamic variable
    selectInput("state_filter", "Select State:", choices = "India", selected = "India")  # Dynamic dropdown
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @media (max-width: 768px) { 
          .sidebar-collapse .main-sidebar { 
            transform: translateX(0) !important; 
          }
          .main-sidebar {
            transition: none !important;  /* Disable transition for the sidebar on mobile */
          }
        },
        
        /* Header colours - I have no idea why this code is needed twice, but it is needed */
        .skin-blue .main-header .navbar {
          background-color: #93B791;
        }
        .skin-blue .main-header .navbar {
          background-color: #93B791;
        }
        
        .skin-blue .main-header .logo {
          background-color: #93B791;
        }
        
        /* Sidebar colours */
        .skin-blue .main-sidebar {
          background-color: #93B791;
        }
        .skin-blue .sidebar-menu > li > a {
          color: white;
        }
        .skin-blue .sidebar-menu > li > a:hover {
          background-color: #1a252f;
          color: white;
        }
      ")),
      tags$script(HTML("
        $(document).on('shiny:connected', function() {
          if ($(window).width() <= 768) {
            $('body').addClass('sidebar-open'); // Force sidebar to open on mobile
            // Open the sidebar if it is closed
            if ($('.sidebar-toggle').length > 0) {
              $('.sidebar-toggle').click(); // Simulate click on the hamburger menu to open the sidebar
            }
          }
        });
      "))
    ),
    # Tabbox with four panes
    tabBox(
      id = "main_tabs", width = 12, height = "700px", 
      tabPanel("Explorer Score", 
               fluidRow(column(12, uiOutput("score_panel_explorer_name_heading"))),
               fluidRow(column(12, uiOutput("score_panel_explorer_score_heading"))),
               fluidRow(column(12, uiOutput("score_panel_district_count_heading"))),
               fluidRow(
                 column(8, leafletOutput("score_panel_map", height = "700px")),
                 column(4, uiOutput("score_panel_district_summary_heading")),
                 column(4, tableOutput("score_panel_district_summary"))
               ),
               fluidRow(
                 column(
                   8,
                   p(
                     HTML(
                       paste(
                         "When you share your card, use hashtag <b>#explorerscore</b> and tag us at ",
                         tags$a(href = "https://www.instagram.com/birdcountindia", 
                                "@birdcountindia", 
                                target = "_blank") # Opens the link in a new tab
                       )
                     )
                   )
                 ),                 
                 column(
                   8,
                   align = "left", # Align buttons to the left
                   div(
                     style = "display: flex; gap: 10px;", # Ensures buttons are on the same line with some spacing
                     downloadButton("download_card", "Download Card (Instagram)"),
                     downloadButton("download_html", "Download Card (HTML)")
                   ))
                )
      ),
      tabPanel("Explorer Targets", 
               fluidRow(column(12, uiOutput("target_panel_explorer_name_heading"))),
               fluidRow(column(12, uiOutput("target_panel_explorer_score_heading"))),
               fluidRow(column(12, uiOutput("target_panel_district_count_heading"))),
               fluidRow(
                 column(8, leafletOutput("target_panel_map", height = "700px")),
                 column(4, uiOutput("target_panel_district_summary_heading")),
                 column(4, tableOutput("target_panel_district_summary"))
               ),
      ),
      tabPanel("About", 
               fluidRow(column(12, 
                               p(strong("Bird Count India Tools: Explorer Score")),
                               p("Birds are ubiquitous in our surroundings, but the distribution of species is not uniform. Many bird species occur only in specific habitats or regions, compelling birdwatchers (birders) to venture into areas that are typically off the beaten path. In doing so, birders often gather valuable data that contributes to scientific research and conservation efforts. This tool is designed to quantify the extent of your birding exploration within India, based on data from your personal eBird account."),
                               p(paste("The Explorer Score is a metric that reflects the scope and intensity of your bird exploration activities. We first find the ratio of the species you have recorded in every district and the total number of species for that district. This ratio of species you have recorded within that district is averaged across the country for your India Explorer Score, which is subsequently normalized to a scale of 10,000. A score exceeding 300 is considered indicative of excellent birding exploration. We hope this tool encourages birders to identify regions that would enhance their exploration efforts and contribute further to the understanding of India's avian biodiversity.")),
                               p(paste("To understand better, let us take an example. If you have seen 330 species in Thane, but eBird already has 422 species reported from there, then your score for Thane district would be 0.78 (i.e. you have seen 78% of birds ever recorded from Thane). Such scores from every district you have visited are generated. Note, there will be many districts with 0 score as you have not visited them yet. All these scores are added, say we get 32.5 as the total score. This total score is divided by the number of districts (735 in India) to obtain 0.0442. This value is multiplied by 10,000 to get your Explorer Score as 422. The logic stays the same at state level, only the total number of districts change.")),
                               p(paste("Now that we know our scores, it would be useful to know how can a person increase their scores. Hence, the explorer targets provide a list of suggested districts where you have not explored enough; i.e., have only seen less than 30% of the species found in that district. The tool also gives preference to districts from your most active states based on your state explorer scores. These states may be indicative of your current residence, work place, or some form of interest or investment. Hence, it is easy to target districts from such states. Maximum of up to five target districts in each state is provided. The map provided in this tab is exactly the inverse of the previous map.")),          
                               tags$p(
                                 "Developer Contact: Praveen J (paintedstork@gmail.com)",
                                 style = "font-style: italic; font-size: smaller; text-align: right;"
                               )
               )),
      )
     )
  )
)
}
