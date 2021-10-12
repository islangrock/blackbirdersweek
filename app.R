# to update app at shinyapps.io
# install rsconnect
# run: deployApp("shinyapp")

library(shiny)
library(tidyverse)
library(visNetwork)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Black Birders Week & Networked knowledge production on Wikipedia"),
  
  # Sidebar layout -- text and description here  ----
  sidebarLayout(
    sidebarPanel(
      h4("Wikipedia Links & Public Spheres"), 
           p("On May 31, 2020 the Twitter account @BlackAFinSTEM wrote, “We can’t even organize for one Black trauma before another one happens” 
           addressing the murder of George Floyd by a police officer in Minneapolis on May 25th that had started to reach national attention
           by the end of week. A week that had started with the viral video of a white woman calling the cops on a Christian Cooper,
           a Black man and birdwatcher, who asked her to leash her dog. Out of this highly charged week where the extent and breadth 
           of racism in the American police state was made vividly palpable, the people behind the Twitter account designed a weeklong 
           series of virtual events to make Black birders visible and celebrate their love of birding, birds, and nature.
           This event brought together sets of discourses across several different publics, making clear how their aims often intersect: 
           from the Black Lives Matter movement to scientists and ecology enthusiasts."),
           br(),
           p("The Wikipedia page for Black Birders Week makes these intersections of frequently disparate worlds visible. Indeed, Wikipedia is
           often used as a  record of public issues of concern where interventions into the public sphere are made by counterpublics
           who want to share and make visible their particular knowledges, often engaging in a process of collective memory",
             a("(Twyman, Keegan, & Shaw (2017)", href="https://dl.acm.org/doi/pdf/10.1145/2998181.2998232"),
           "And importantly, through the crucial, but simple affordance of hyperlinks across Wikipedia articles, 
           connections can be mapped and followed."),
           br(),
           p("In taking up", a("Catherine Squires'",
               href="https://academic.oup.com/ct/article-abstract/12/4/446/4110444?redirectedFrom=fulltext"),
             "charge to focus 
             less on the identity of publics and more on how they “interact and intersect,” this project aims to highlight
             the overlapping nature of the many publics that make up this one particular intersection of public discourse.
             Black Birders Week connects Black Lives Matter and its focus on police brutality and the history of systemic racism with ongoing conversations 
             in the STEM community about representation and diversity, as well as with the often overlooked  
             “joy and unapologetic strength and style of Black birders” (described by Tykee James in an",
             a("interview with CNN.)", 
               hfref= "https://www.cnn.com/2020/06/03/us/black-birders-week-black-in-stem-christian-cooper-scn-trnd/index.html"),
            "This site provides an experiential description of how the various publics engaged in Black Birders Week overlap. 
          To the right, you can see all the incoming and outgoing links to the Black Birders 
          Week Wikipedia page from all other Wikipedia pages (as of Dec. 2020)."),
           br(),
           h4("Features (Or, How to Use this App)"),
           p("- Zoom in or out with your computer's scroll feature"), 
           p("- Use the drop down menus in the corner to highlight a particular node (top) or public (bottom)"),
           p("- Hover your mouse over any node to select that node and find a link to the Wikipedia page"),
           p("- Click to drag the network around or move a node"),
           br(),
           h5("Thank you for visiting!"),
           p("Send further ideas or comments via",
             a("Twitter",
               href="https://twitter.com/ILangrock"),
             "or email at isabelle.langrock @ asc.upenn.edu"),
           p(em(a("Find the code on Github"))), 
          br(),
          p("created by: Isabelle Langrock"),
          p("last updated: 12.09.2020"), width=4),
    # Main panel for displaying outputs ----
    mainPanel(
      h2("Ego Network: Black Birders Week", align="left"),
      visNetworkOutput("network", height="1000px", width="100%"), width=8)
)
)

# Define server logic -- Using VisNetwork to create an interactive network 
server <- function(input, output) {

  output$network <- renderVisNetwork({
    
    colors <- data.frame(Public = c("Birding", "BLM", "BLM & Birding", "Media", "Other", "Science", "Wikipedia"),
                         color = c("Green", "Yellow", "Orange", "#930DE0", "#F57B7E", "#119FF7", "Grey"))
    
    nodes<-read.csv("data/bbw_nodes.csv")%>%
      rename(id = "NAME") %>% 
      mutate(label = id, 
             title = html_title, 
             font.size= "20") %>%
      left_join(colors, by="Public")
    
    nodes$title <- sapply(nodes$site, function(x) 
      toString(tags$a(href=paste0(x), x)))
    
    edges<-read.csv("data/bbw_edges.csv")
      
    visNetwork(nodes, edges, height="1000px", width="100%") %>%
      visEdges(arrows="from") %>%
      visOptions(highlightNearest = list(enabled=T, degree=1, hover=T),
                 selectedBy="Public",
                 nodesIdSelection = TRUE) %>%
      visPhysics(  solver="forceAtlas2Based",
                   forceAtlas2Based = list(gravitationalConstant=-75)) %>%
      visLayout(randomSeed=123)
    
  })
  
}


shinyApp(ui=ui, server = server)


