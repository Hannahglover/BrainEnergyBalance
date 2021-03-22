
library(DT)
library(shinythemes)
library(tidyverse)
options(rsconnect.max.bundle.size="xxxlarge")

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage("Brain Energy Homeostasis Atlas",
                           
                           #Abstract page
                           tabPanel("Abstract",
                                    
                                    headerPanel(HTML("<center><img src ='Image web portal_horizontal_MCDR_05 21 2020.jpg' height='200px'</center>")), ####EXTERNAL
                                    fluidRow(
                                      column(8, offset=2,
                                             hr(),
                                             h3("Gene expression atlas of mouse brain regions involved in the regulation of energy homeostasis"),
                                             #h4("Abstract"),
                                             
                                             div(style="text-align:justify",
                                                 p(HTML("<br>Energy balance is controlled by interconnected brain regions in the hypothalamus, brain stem, cortex and limbic system; the details of the transcript expression “signatures” of these regions can help elucidate the pathophysiology underlying obesity. RNA sequencing was conducted on P56 C57BL/6NTac male mice and E14.5 C57BL/6NTac embryos punch-biopsies in 16 obesity-relevant brain regions. Self-renewing mouse embryonic stem cells (mESCs) were included as a control. <br><br><b>This data portal is designed to serve as a resource for researchers to interrogate their genes of interest across three metrics: transcripts/million reads, differential expression between all brain regions, or differential expression compared to mESCs. </b>"))),
                                             hr(),
                                             h4("Corresponding Paper"),
                                             HTML("De Rosa MC, Glover H, Stratigopoulos G, LeDuc CA, Su Q, Shen Y, Sleeman MW, Chung WK, Leibel RL, Altarejos J, Doege CA (2021)  <b>Gene expression atlas of mouse brain regions involved in the regulation of energy balance.</b>  <i> Under Review.</i><br>"),
                                             h4(HTML("<br>Abstract")),
                                             div(style="text-align:justify",
                                                 p(HTML("Energy balance is controlled by interconnected brain regions in the hypothalamus, brain stem, cortex and limbic system; the details of the transcript expression “signatures” of these regions can help elucidate the pathophysiology underlying obesity. RNA sequencing was conducted on P56 C57BL/6NTac male mice and E14.5 C57BL/6NTac embryos punch-biopsies in 16 obesity-relevant brain regions. Transcript sequences were interrogated by anatomic region and the expression profiles of 190 known obesity-associated genes (monogenic, rare and low-frequency coding variants, genome-wide association studies (GWAS), syndromic) were mapped. Genes associated within distinct genetic categories of obesity showed specific expression distributions across brain regions. Known monogenic obesity causal genes were highly enriched in the arcuate nucleus of the hypothalamus and developing hypothalamus. The obesity-associated genes clustered into distinct ‘modules’ of similar expression profile and these are distinct from expression ‘modules’ formed by similar analysis with genes known to be associated with other disease phenotypes (type 1 and 2 diabetes, autism, breast cancer) in the same energy balance-relevant brain regions. "))),
                                      )
                                    )
                                    
                                    
                           ),
                           
                           
                           
                           tabPanel("Data Portal",
                                    
                                    
                                    headerPanel(HTML("<center><img src ='Image web portal_horizontal_MCDR_05 21 2020.jpg' height='200px'</center>")),
                                    
                                    #Side panel controls
                                    fluidRow(
                                      tags$div(style = "padding: 0px 50px 0px 20px;",      
                                               column(3, offset=1,
                                                      wellPanel( 
                                                        selectInput(inputId = "dataset", label = "Choose metric", choices= c("Transcripts/Million", "DEGs between brain regions", "DEGs compared to mESCs"), multiple = F), #Drop down menu
                                                        textInput(inputId = "GeneName", label = "Official Gene Name"), #Text input
                                                        
                                                        div(style="display:inline-block",actionButton("Submit", "Submit"),width=6),
                                                        div(style="display:inline-block",downloadButton('downloadData', 'Download Table'))),
                                                      
                                                      
                                                      
                                                      #Side panel text
                                                      hr(),
                                                      wellPanel(
                                                        h5("Brain regions"),
                                                        HTML("<p class=small><font size=1.7><b>Embryonic Brain (E14.5) <br>THy/Phy:</b> Terminal Hypothalamus (Rostral Hypothalamus)/Peduncular (Caudal) Hypothalamus <br>  <b>F:</b> Forebrain <br>   <b>M:</b> Midbrain <br> <b>H:</b> Hindbrain <br> 
                                                    <br> <b>Adult Brain (P56) <br>FRP:</b> Frontal Pole, Cerebral Cortex <br> <b>ACA:</b> Anterior  Cingulate Area <br> <b>ACB:</b> Nucleus Accumbens <br> <b>VTA:</b> Ventral Tegmental Area <br> <b>LHA:</b> Lateral  Hypothalamic Area <br> <b>PVH:</b> Paraventricular Nucleus <br> <b>DMH:</b> Dorsomedial Nucleus of the Hypothalamus <br> <b>VMH:</b>  Ventromedial Hypothalamic Nucleus <br> <b>ARH:</b> Arcuate Hypothalamic Nucleus <br> <b>PB:</b> Parabrachial Nucleus <br> <b>NTS:</b>   Nucleus of the Solitary Tract <br> <b>DVC:</b> Dorsal-Vagal Complex <br> <b>ENT:</b> Entorhinal Area <br> <b>CENT2:</b> Central Lobule II <br> <b>CUL4,5:</b> Culmen Lobules IV-V <br> <b>UVU:</b> Uvula (IX) <br> <b>CBN:</b> Cerebellar Nuclei  <br> <b>FL:</b> Flocculus <br> <br> <b>Mouse embryonic stem cells <br> mESC (B6-1):</b> Mouse Embryonic Stem Cells, clone 1<br> <b>mESC (B6-2):</b> Mouse Embryonic Stem Cells, clone 2</font></p>"),
                                                        
                                                        hr(style = "border-top: 1px solid #d4d8d9;"),
                                                        
                                                        h5("Metrics"),
                                                        h6("Transcripts/million"),
                                                        HTML("<p class=small><font size=1.7>Outputs transcripts per million (TPM) for up to 4 replicates, with mean and standard deviation</font></p>"),
                                                        h6("DEGs: Differential Gene Expression"),
                                                        HTML("<p class=small><font size=1.7>Outputs log<sub>2</sub> fold change and standard error for each <b>brain region comparared to all 
                                                             other brain regions </b> or <b>each brain region compared to mESCs. </b> Also outputs corresponding <em>p</em>-value, 
                                                             Benjamini-Hochberg adjusted <em>p</em>-value and mean counts</font></p>"),
                                                        hr(style = "border-top: 1px solid #d4d8d9;"),
                                                        h5("Further details"),
                                                        HTML("<p class=small><font size=1.7>De Rosa MC, Glover H, Stratigopoulos G, LeDuc CA, Su Q, Shen Y, Sleeman MW, Chung WK, Leibel RL, Altarejos J, Doege CA (2021)  <b>Gene expression atlas of mouse brain regions involved in the regulation of energy balance.</b>  <i> Under Review.</i></font></p>"),
                                                        hr(style = "border-top: 1px solid #d4d8d9;"),
                                                        h5("Contact"), 
                                                        HTML("<p class=small><font size=1.7>Please contact hg2553@cumc.columbia.edu with any questions or issues.</font></p>"),                         
                                                        hr(style = "border-top: 1px solid #d4d8d9;"),
                                                        HTML("<center><img src ='ColumbiaLogo.png' height='20px' hspace='20'><img src ='Regeneron.svg' height='10px'></center>")))),
                                      
                                      
                                      #Main panel
                                      column(7,offset=1,
                                             tableOutput(outputId = "summary"),
                                             plotOutput(outputId = "plotdata"))
                                               )
                                      ),
                           
                           
                           
                           #Supress errors
                           tags$style(type="text/css",
                                      ".shiny-output-error { visibility: hidden; }",
                                      ".shiny-output-error:before { visibility: hidden; }"
                           )
                           
                           ),
                                      )
server <- function(input, output) {
  
  #Pull csv files
#  load("Portal_Datasets.RData")
  TPM_All = read.csv("TPM_Gather_2021.csv", head=T, row.names=1) 
  BrainDEG_only = read.csv("DEGs_Brain_Gather_2021.csv", head=T, row.names=1) 
  DEGstoES = read.csv("DEGs_ESC_Gather_2021.csv", head=T, row.names=1) 

  
  #Capitalize string func
  CapStr <- function(y) {
    c <- strsplit(y, " ")[[1]]
    paste(toupper(substring(c, 1,1)), tolower(substring(c, 2)),
          sep="", collapse=" ")
  }
  
  #Setup reactive
  datasetInput <- reactive({
    switch(input$dataset,
           "DEGs between brain regions" = BrainDEG_only,
           "Transcripts/Million" = TPM_All,
           "DEGs compared to mESCs" = DEGstoES)
  })
  
###New
  output$summary <- renderTable({
    dataset <- datasetInput()
    filtered = subset(dataset, dataset$Gene %in% CapStr(input$GeneName))
    print(filtered)
})
        
} #End Server


shinyApp(ui = ui, server = server)
