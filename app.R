#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(data.table)
library(ggplot2)
library(datasets)
library(reshape)
library(shinycssloaders)
library(shiny)
library(magrittr)
library(markdown)
# library(bslib)
library(dplyr)
library(ggpubr)
library(ggrepel)
library(shinythemes)




source("plot.R")
options(bitmapType='cairo')
# Define UI for application that draws a histogram
kegg.pathways1 = readRDS("data/annotations/hsa_KEGG.list.rds") 
kegg.pathways = readRDS("data/all.kegg.pathway.list.Rds") 
save.data = readRDS("data/save.data.rds")
kegg.output =readRDS("data/KEGG1_150.Rds")
kegg.pathways.names = kegg.pathways$name

kegg.output

str2Table <- function(pathway.str, dir="data/results/")   sprintf("%s/%s.csv", dir, pathway.str) %>% fread
str2pdf <- function(pathway.str, dir="data/results/")  sprintf("%s/%s.pdf", dir, pathway.str)

main.text1x="BipotentR identifies targets for bipotent drugs – single drugs that can kill cancer by multiple mechanisms. 
BipotentR was developed to tackle the low response rates to existing targeted therapies and the high rates of relapse due to evolved resistance to a single targeted mechanism.  
It allows researchers to systematically identify 
single bipotent targets whose inhibition delivers a <b>one-two punch</b> against advanced and resistant cancers.
BipotentR identified bipotent immunotherapeutic targets that also regulate other hallmark cancer pathways, including energy metabolism (38 immune-metabolic regulators), 
angiogenesis (14 regulators), and evasion of growth suppressor (14 regulators)." 


main.text1="BipotentR identifies targets for bipotent drugs – single drugs that can kill cancer by multiple mechanisms. 
BipotentR was developed to address the low response rates to existing therapies targeting single mechanisms that also suffer from high relapse rates due to evolved resistance to single mechanisms.  
It allows researchers to systematically identify bipotent targets whose inhibition act against advanced and resistant cancers by inducing immunity and suppressing an input oncogenic pathway. BipotentR identified bipotent immunotherapeutic targets that also regulate other hallmark cancer pathways, including energy metabolism (38 immune-metabolic regulators), 
angiogenesis (14 regulators), and evasion of growth suppressor (14 regulators)."


main.text2 = "The website provides an interface to explore bipotent gene regulators that modulate antitumor immunity and input pathways (among KEGG pathways)." 

# main.text2 = "The website provides interface to explore bipotent gene regulators that modulate antitumor immunity and an input pathways (among KEGG pathways)." 

main.text3 = "BipotentR is a software to design therapies with dual anticancer potentials. It identifies gene targets that kill tumors by modulating anti-tumor immunity and an input pathways. 
The website provides interactive interface for users to explore bipotent gene regulators that modulate antitumor immunity and an input pathways (among the KEGG patwhays). 
For identifying bipotent regulator of any input pathway, the website also provides a tutorial for using R package of BipotentR."


  ui <-  fluidPage(
  theme = shinytheme("journal"),
    # tags$head(includeHTML(("google-analytics.html"))),
    # tags$head(tags$style(".butt{background-color:#337ab7; color: white;}",
    #                      HTML(".shiny-notification {
    #           height: 100px;
    #           width: 800px;
    #           position:fixed;
    #           top: calc(50% + 120px);;
    #           left: calc(50% - 400px);;
    #         }
    #        "
    # ))),
    # visualizing, and analyzing an extensive collection of syngeneic mouse model data. We uniformly processed raw sequencing data for 1,518 mouse samples, covering 68 cell lines and 19 cancer types, to generate gene expression and immune cell infiltration profiles, of which 832 were from immune checkpoint blockade (ICB) studies. In addition, TISMO hosts 605 in vitro samples of which 195 were cytokine treated. Sample metadata, including cancer type, cell line, mouse strain, transplantation site, ICB treatment, and response status were manually curated. TISMO provides interactive interfaces for users to explore gene expression and immune infiltration, and allows systematic comparisons between different model characteristics, and treatment and response groups.
        # tags$head(HTML('<link rel="icon", href="icon.png", type="image/png" />')),
 div(
    # img(src="icon.png", height =150, width = 180), 
    
    HTML('
    <h1 style="background-color:lightgray;">
    <img src="icon.png" height =100, width = 140>
  BipotentR
    </h1>
         ')
    ),
        navbarPage(
          title = NULL,
          
            # title = div(img(src="icon.png", height =80, width = 100), "BipotentR", style='color:black;font-size:150%'),
           id = "tabs",
            tabPanel("Home",
                       value = "homepanel",fluid = T,
                     br(),
                     br(),
                     mainPanel(
                         align="justify",
                         style = "font-size: 12pt",
                         # p(style = "font-size:90%;text-align:right",a(strong('User Visiting Statistics'),href='https://datastudio.google.com/reporting/ea5bd962-fc91-408e-b0c1-919434446040',target="_blank")),
                     # DT::dataTableOutput("table"),
                         p(HTML(main.text1)),
                         p(HTML(main.text2)),
                         helpText(em("Find bipotent targets of :")),
                         fluidRow(
                             column(3, align="center", actionButton("jumpToEnergy", style="height:70px; width=70px; font-size:100%", HTML("<b>Energy <br/> Metabolism</b>"))),
                             column(3, align="center", actionButton("jumpToAngiogenesis", style="height:70px; width=70px; font-size:100%", HTML("<b>Angiogenesis<br/></b>"))),
                             column(3, align="center", actionButton("jumpToGrowth", style="height:70px; width=70px; font-size:100%", HTML("<b>Growth suppressor <br/> Evasion </b>"))),
                             column(3, align="center", actionButton("jumpToKEGG", style="height:70px; width=70px; font-size:100%", HTML("<b>KEGG <br/> pathways</b>")))
                         ),
                         hr(),
                         # p("For identifying bipotent targets of user defined pathways, we recommend using R-package."),
                         p("For identifying bipotent regulator of any other input pathway, the website also contains detailed instructions for downloading and using a R  package for more advanced applications."),
                         # em(strong("R package")),
                         actionButton("jumpToRpackage", style="height:35px; width=70px; font-size:100%", HTML("<b> R package</b>")),
                         
                                            
                         hr(),
                          em(strong("Machine learning based estimation of bipotent targets")),
                          div(style="font-size:90%;margin-bottom:8px",
                              p("Please use the",
                              a("[BTAS website]", href="https://rconnect.dfci.harvard.edu/BTAS/"),
                           "to access  machine learning and deep learning models which  estimate tumor activity of bipotent target that are predictive of immunotherapy response for melanoma patients.",
                           ),
                          ),
                     actionButton(inputId='ab1', label="BTAS", 
                                         icon = icon("th"), 
                                         onclick ="window.open('https://rconnect.dfci.harvard.edu/BTAS/', '_blank')"),
                    
                     div(style="font-size:90%;margin-bottom:8px",
                     p("Standalone (R and Python based) software of BTAS is available",
                       a("[here.]", href="https://github.com/vinash85/deepBTAS/tree/master")
                     ),
                     ),
                      
                     hr(),
                              em(strong("Download data and code of BipotentR manuscript")),
                              div(style="font-size:90%;margin-bottom:8px",
                                  p("For reprodicibility, data and code from the manuscript are shared publicly at",
                                    a("Zenodo", href="https://zenodo.org/record/7043369")
                                    
                                  ),
                              ),
                              actionButton(inputId='ab1', label="Data", 
                                           icon = icon("th"), 
                                           onclick ="window.open('https://zenodo.org/record/7043369', '_blank')"),
                             
                     div(style="font-size:90%;margin-bottom:8px",
                         p("Bulk and single RNA-seq data generated from the current study are available at",
                           a("[RNA-seq]", href="https://www.dropbox.com/sh/qoqlx9724k0k869/AADZe7oZp0vs4gKzqeRwTUjaa?dl=0"),
                           "and will be submitted to a public repository upon publication."
                           
                         ),
                     ),
                     actionButton(inputId='ab1', label="RNA-seq data", 
                                  icon = icon("th"), 
                                  onclick ="window.open('https://www.dropbox.com/sh/qoqlx9724k0k869/AADZe7oZp0vs4gKzqeRwTUjaa?dl=0', '_blank')"),
                     
          
                              # includeMarkdown("BTAS.md")
                     
                         # div(
                         #     align="left",
                         #     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/4QaM-0RllYM?start=1" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>') 
                         # ),
                         # helpText("Some text and then ", code("some code"), "."),
                         hr(),
                         em(strong("How do I cite BipotentR?")),
                         div(style="font-size:90%;margin-bottom:8px",
                             p("Sahu AD, Wang X, Munson P, Wang X, Gu S, Nicol P, Qian G, Zeng Z, Brown M, Liu JS, Juric D,  Meyer C, Liu XS, Fisher DE, Flaherty KT. 
                               Data-driven discovery of targets for bipotent anticancer drugs identifies Estrogen Related Receptor Alpha",
                               # strong(em("XXXX")), "2021",
                               a("[Bioarxiv]", href="https://www.biorxiv.org/content/10.1101/2021.10.25.465724v1"),
                             ),
                         ),
                         hr(),
                         em(strong("Contact:")),
                         p(
                             "Avinash D. Sahu:", a("asahu@ds.dfci.harvard.edu",href="mailto:asahu@ds.dfci.harvard.edu"), HTML("<br/>"),
                             "David E. Fisher:", a("dfisher3@mgh.harvard.edu",href="mailto:dfisher3@mgh.harvard.edu"), HTML("<br/>"),
                             "Keith T. Flaherty:", a("KFLAHERTY@mgh.harvard.edu",href="mailto:KFLAHERTY@mgh.harvard.edu")),
                         # em(a(strong('User Visiting Statistics'),href='https://datastudio.google.com/reporting/ea5bd962-fc91-408e-b0c1-919434446040',target="_blank")),
                         hr(),
                         p(style = "font-size:90%;text-align:center", "BipotentR | © Dana Farber Cancer Institute")
                         
                     )
            ),
            tabPanel("Energy metabolism", value = "energypanel",
                     fluid = T,
                     br(),
                     br(),
                     br(),
                     # sidebarLayout(
                     fluidRow(

                      
                             HTML(paste0("Bipotent immune-metabolic regulators that modulate the energy metabolism pathways and
                                         antitumor immunity concurrently"
                             )),
                             br(),
                             HTML({"
                                 Energy metabolism pathways included :
                                 <ul>
                                     <li> Glycolysis</li>
                                     <li> Oxidative phosphorylation </li>
                                     <li> Tricarboxylic acid cycle (TCA cycle) </li>
                                     <li> Fatty acid metabolism</li>
                                     </ul>
                                     "
                             }),
                     ),
                     hr(),
                     fluidRow(
                       plotOutput("plot.energy"),
                     ),
                     hr(),
                     fluidRow(
                     # tableOutput("table"),
                      DT::dataTableOutput("table.energy"),
                     )

            ),
           tabPanel("Angiogenesis", value = "angiogenesispanel",
                    fluid = T,
                    br(),
                    br(),
                    br(),
                    # sidebarLayout(
                    fluidRow(
                      HTML(paste0("Bipotent  regulators that modulate the angiogenesis and
                                         antitumor immunity concurrently"
                      )),
                    ),
                    hr(),
                    fluidRow(
                      plotOutput("plot.angiogenesis"),
                    ),
                    hr(),
                    fluidRow(
                      DT::dataTableOutput("table.angiogenesis"),
                    )

           ),
           tabPanel("Growth suppressor evasion", value = "growthpanel",
                    fluid = T,
                    br(),
                    br(),
                    br(),
                    # sidebarLayout(
                    fluidRow(
                      HTML(paste0("Bipotent  regulators that modulate the evasion of growth supressor and
                                         antitumor immunity concurrently"
                      )),
                    ),
                    hr(),
                    fluidRow(
                      plotOutput("plot.growth"),
                    ),
                    hr(),
                    fluidRow(
                      DT::dataTableOutput("table.growth"),
                    )

           ),
           
            tabPanel("KEGG", value = "KEGGpanel",
                     br(),
                     br(),
                     br(),
                     sidebarLayout(
                         sidebarPanel(
                             selectizeInput(inputId = "KEGG.input", label = "Select/Type input pathway", multiple = F, choices = kegg.pathways.names, selected =NULL)
                         ),
                         mainPanel(
                             align="justify", 
                             style = "font-size: 12pt",
                             HTML(paste0("Identify bipotent regulators that modulate the selected input pathway and 
                                         antitumor immunity concurrently" 
                             )),
                             hr(),
                             textOutput('pathway.message'),
                             tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                             textOutput('gene.list'),
                             
                         )
                     ),
                     hr(),
                     
                     plotOutput("plot.kegg"),
                     # div( style="display:inline-block", uiOutput("infiltration_ICBJPG")),
                     # div(style="display:inline-block", uiOutput("infiltration_ICBPDF")), 
                     # div(style="display:inline-block",  uiOutput("infiltration_ICBTXT")),
                     # uiOutput("infiltration_ICB_figure"),
                     br(),
                     
            ),
           
            tabPanel("Rpackage", value = "Rpackagepanel",
                     br(),
                     br(),
                     br(),
                     includeMarkdown("Rpackage.md")
            ),
            tabPanel("BTAS", value = "BTASpanel",
                     br(),
                     br(),
                     br(),
                     em(strong("Machine learning based estimation of bipotent targets")),
                     div(style="font-size:90%;margin-bottom:8px",
                         p("Please use the",
                           a("[BTAS website https://rconnect.dfci.harvard.edu/BTAS/]", href="https://rconnect.dfci.harvard.edu/BTAS/"),
                           "to access  machine learning and deep learning models which  estimate tumor activity of bipotent target that are predictive of immunotherapy response for melanoma patients.",
                         ),
                     ),
                     actionButton(inputId='ab1', label="BTAS", 
                                  icon = icon("th"), 
                                  onclick ="window.open('https://rconnect.dfci.harvard.edu/BTAS/', '_blank')"),
                     
                     
                     div(style="font-size:90%;margin-bottom:8px",
                     p("Standalone (R and Python based) software of BTAS is available",
                       a("[here.]", href="https://github.com/vinash85/deepBTAS/tree/master")
                     ),
                     ),
                     # includeMarkdown("BTAS.md")
            ),
          tabPanel("Data", value = "Datapanel",
                   br(),
                   br(),
                   br(),
                   em(strong("Download data and code of BipotentR manuscript")),
                   div(style="font-size:90%;margin-bottom:8px",
                       p("For reprodicibility, data and code from the manuscript are shared publicly at Zenodo",
                         a("https://zenodo.org/record/7043369", href="https://zenodo.org/record/7043369")
                         
                       ),
                   ),
                   actionButton(inputId='ab1', label="Data", 
                                icon = icon("th"), 
                                onclick ="window.open('https://zenodo.org/record/7043369', '_blank')"),
                   
                   div(style="font-size:90%;margin-bottom:8px",
                       p("Bulk and single RNA-seq data generated from the current study are available at",
                         a("[RNA-seq]", href="https://www.dropbox.com/sh/qoqlx9724k0k869/AADZe7oZp0vs4gKzqeRwTUjaa?dl=0"),
                         "and will be submitted to a public repository upon publication."
                         
                       ),
                   ),
                   actionButton(inputId='ab1', label="RNA-seq data", 
                                icon = icon("th"), 
                                onclick ="window.open('https://www.dropbox.com/sh/qoqlx9724k0k869/AADZe7oZp0vs4gKzqeRwTUjaa?dl=0', '_blank')"),
                   
                   # includeMarkdown("BTAS.md")
          )
        )
    
    
    # Sidebar with a slider input for number of bins 
    
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  # output$table <- renderTable({
    # save.data$energy$table})    

  
    # output$table1 <- DT::renderDataTable(DT::datatable({
    #   save.data$energy$table
    # }))
    # browser()
    observeEvent(input$jumpToKEGG, {updateTabsetPanel(session, "tabs", selected = "KEGGpanel")})
    observeEvent(input$jumpToEnergy, {updateTabsetPanel(session, "tabs", selected = "energypanel")})
    observeEvent(input$jumpToAngiogenesis, {updateTabsetPanel(session, "tabs", selected = "angiogenesispanel")})
    observeEvent(input$jumpToGrowth, {updateTabsetPanel(session, "tabs", selected = "growthpanel")})
    observeEvent(input$jumpToRpackage, {updateTabsetPanel(session, "tabs", selected = "Rpackagepanel")})
    observeEvent(input$jumpToBTAS, {updateTabsetPanel(session, "tabs", selected = "BTASpanel")})
    observeEvent(input$jumpToData, {updateTabsetPanel(session, "tabs", selected = "Datapanel")})
    output$pathway.message = renderText({sprintf("Genes in %s", input$KEGG.input)})
    
    output$gene.list = renderText({
      kegg.pathways[name==input$KEGG.input]$V1 %>% unlist %>% sort %>% 
        paste(.,collapse = " ") 
    })
    output$plot.energy <- renderPlot({
      get.plot(save.data$comb.imm2,save.data$energy$sum.model, save.data$energy$show.names)
    }, res = 96)
    output$table.energy <- DT::renderDataTable(DT::datatable({
      save.data$energy$table
    }))
    output$plot.angiogenesis <- renderPlot({
      get.plot(save.data$comb.imm2,save.data$angiogenesis$sum.model, save.data$angiogenesis$show.names)
    }, res = 96)
    output$table.angiogenesis <- DT::renderDataTable(DT::datatable({
      save.data$angiogenesis$table
    }))
    output$plot.growth <- renderPlot({
      get.plot(save.data$comb.imm2,save.data$growth$sum.model, save.data$growth$show.names)
    }, res = 96)
    output$table.growth <- DT::renderDataTable(DT::datatable({
      save.data$growth$table
    }))
    
    output$plot.kegg <- renderPlot({
      term=kegg.pathways[name==input$KEGG.input]$term
      avi.dt = kegg.output[Pathway==term]
      # browser()
      get.volcanoplot(avi.dt)
    }, res = 96)
    # output$table.kegg <- DT::renderDataTable(DT::datatable({
    #   save.data$kegg$table
    # }))
    # output$plot1 <- renderPlot({
    #   get.plot(save.data$comb.imm2,save.data$energy.sum.model, save.data$energy.show.names)
    #   # plot(mtcars$wt, mtcars$mpg)
    # }, res = 96)
    

        
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
