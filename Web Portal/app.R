


library(DT)
library(shinythemes)
library(tidyverse)
library(DBI)
library(RSQLite)

ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage("Brain Energy Balance Atlas",
                           
                           #Abstract page
                           tabPanel("Abstract",
                                    
                                    headerPanel(HTML("<center><img src ='Image web portal_horizontal_MCDR_05 21 2020.jpg' width='80%'</center>")), ####EXTERNAL
                                    fluidRow(
                                      column(8, offset=2,
                                             hr(),
                                             h3("Gene expression atlas of energy balance brain regions"),
                                             #h4("Abstract"),
                                             
                                             div(style="text-align:justify",
                                                 p(HTML("<br>Energy balance is controlled by interconnected brain regions in the hypothalamus, brain stem, cortex and limbic system; the details of the transcript expression “signatures” of these regions can help elucidate the pathophysiology underlying obesity. RNA sequencing was conducted on P56 C57BL/6NTac male mice and E14.5 C57BL/6NTac embryos punch-biopsies in 16 obesity-relevant brain regions. Self-renewing mouse embryonic stem cells (mESCs) were included as a control. <br><br><b>This data portal is designed to serve as a resource for researchers to interrogate their genes of interest across three metrics: transcripts/million reads, differential expression between all brain regions, or differential expression compared to mESCs. </b>"))),
                                             hr(),
                                             h4("Corresponding Paper"),
                                             HTML("De Rosa MC, Glover H, Stratigopoulos G, LeDuc CA, Su Q, Shen Y, Sleeman MW, Chung WK, Leibel RL, Altarejos J, Doege CA (2021)  <b>Gene expression atlas of energy balance brain regions. </b>  <i> JCI Insight</i>; 6(16)<br>"),
                                             h4(HTML("<br>Abstract")),
                                             div(style="text-align:justify",
                                                 p(HTML("Energy balance is controlled by interconnected brain regions in the hypothalamus, brain stem, cortex and limbic system. Gene expression signatures of these regions can help elucidate the pathophysiology underlying obesity. RNA sequencing was conducted on P56 C57BL/6NTac male mice and E14.5 C57BL/6NTac embryos punch-biopsies in 16 obesity-relevant brain regions. The expression of 190 known obesity-associated genes (monogenic, rare and low-frequency coding variants, genome-wide association studies (GWAS), syndromic) were analyzed in each anatomical region. Genes associated with these genetic categories of obesity had localized expression patterns across brain regions. Known monogenic obesity causal genes were highly enriched in the arcuate nucleus of the hypothalamus and developing hypothalamus. The obesity-associated genes clustered into distinct ‘modules’ of similar expression profile and these are distinct from expression ‘modules’ formed by similar analysis with genes known to be associated with other disease phenotypes (type 1 and type 2 diabetes, autism, breast cancer) in the same energy balance-relevant brain regions. "))),
                                      )
                                    )
                                    
                                    
                           ),
                           
                           
                           
                           tabPanel("Data Portal",
                                    
                                    
                                    headerPanel(HTML("<center><img src ='Image web portal_horizontal_MCDR_05 21 2020.jpg' width='80%'</center>")),
                                    
                                    #Side panel controls
                                    fluidRow(
                                      tags$div(style = "padding: 0px 50px 0px 20px;",      
                                               column(3, offset=1,
                                                      wellPanel( 
                                                        selectInput(inputId = "dataset", label = "Choose metric", choices= c("Transcripts/Million", "DEGs between brain regions", "DEGs compared to mESCs"), multiple = F), #Drop down menu
                                                        textInput(inputId = "GeneName", label = "Official Gene Name"), #Text input
                                                        
                                                        div(style="display:inline-block",submitButton("Submit"),width=6),
                                                        div(style="display:inline-block",downloadButton('downloadData', 'Download Table')),
                                                        HTML("<p class=small><font size=1.7>Please allow a few seconds for data to load for the first search</font></p>")),
                                                      
                                                      
                                                      
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
                                                        HTML("<p class=small><font size=1.7>De Rosa MC, Glover H, Stratigopoulos G, LeDuc CA, Su Q, Shen Y, Sleeman MW, Chung WK, Leibel RL, Altarejos J, Doege CA (2021)  <b>Gene expression atlas of energy balance brain regions. </b>  <i> JCI Insight</i>; 6(16)</font></p>"),
                                                        hr(style = "border-top: 1px solid #d4d8d9;"),
                                                        h5("Contact"), 
                                                        HTML("<p class=small><font size=1.7>Please contact hg2553@cumc.columbia.edu with any questions or issues.</font></p>"),                         
                                                        hr(style = "border-top: 1px solid #d4d8d9;"),
                                                        HTML("<center><img src ='ColumbiaLogo.png' height='20px' hspace='20'></center>")))),
                                      
                                      
                                      #Main panel
                                      column(7, #offset=1,
                                             tableOutput(outputId = "summary"),
                                             plotOutput(outputId = "plotdata", width="auto"))
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
#  TPM_All = read.csv("www/TPM_Gather_2021.csv", head=T, row.names=1) 
#  BrainDEG_only = read.csv("www/DEGs_Brain_Gather_2021.csv", head=T, row.names=1) 
#  DEGstoES = read.csv("www/DEGs_ESC_Gather_2021.csv", head=T, row.names=1) 
db = dbConnect(SQLite(), "www/Portal_SQL.db")
  
  #Capitalize string func
  CapStr <- function(y) {
    c <- strsplit(y, " ")[[1]]
    paste(toupper(substring(c, 1,1)), tolower(substring(c, 2)),
          sep="", collapse=" ")
  }
  
  #Setup reactive
  datasetInput <- reactive({
    switch(input$dataset,
           "DEGs between brain regions" = "BrainDEGs",
           "Transcripts/Million" = "TPM",
           "DEGs compared to mESCs" = "ESCDEGs")
  })
  
  ##Tables  
  output$summary <- renderTable({
    dataset <- datasetInput()
    #filtered = subset(dataset, dataset$Gene %in% CapStr(input$GeneName))
    res = dbSendQuery(db, paste0('SELECT * FROM ', dataset, ' WHERE "Gene" = ?'))
    dbBind(res, list(CapStr(input$GeneName)))
    outs = dbFetch(res)  
    outs$`..1` = NULL
    outs$Value = as.numeric(outs$Value)

if(dim(outs)[1]==96){
      SubsetGene_spread = spread(outs, "Rep", "Value", "Sample")
      SubsetGene_spread[ SubsetGene_spread == "Sample" ] <- NA
      SubsetGene_spread[3:6] = SubsetGene_spread[3:6] %>% mutate_if(is.character,as.numeric)
      SubsetGene_spread$Mean = apply(SubsetGene_spread[3:6], 1, mean, na.rm=TRUE)
      SubsetGene_spread$SD = apply(SubsetGene_spread[3:6], 1, sd, na.rm=TRUE)
      SubsetGene_spread$Sample = factor(SubsetGene_spread$Sample, levels=c("THy/PHy", "F", "M", "H", "FRP", "ACA", "ACB", "VTA", "LHA", "PVH", "DMH", "VMH", "ARH", "PB", "NTS", "DVC", "ENT", "CENT2", "CUL4,5", "UVU", "CBN", "FL", "mESC B6-1", "mESC B6-2"))
      output <-SubsetGene_spread[order(SubsetGene_spread$Sample),]
      
    }else if(dim(outs)[1]==105){
      outs$Value = round(outs$Value, 2)
      SubsetGene_spread = spread(outs, "Feature" , "Value")
      SubsetGene_spread$Sample = factor(SubsetGene_spread$Sample, levels=c("THy/PHy", "M", "H", "FRP", "ACA", "ACB", "VTA", "LHA", "PVH", "DMH", "VMH", "ARH", "PB", "NTS", "DVC", "ENT", "CENT2", "CUL4,5", "UVU", "CBN", "FL"))
      output <-SubsetGene_spread[order(SubsetGene_spread$Sample),]
      output <- output[c("Sample", "Gene", "log2FC", "lfcSE", "pvalue", "padj", "baseMean")]
    }else if(nchar(CapStr(input$GeneName)) > 0){
      err = as.data.frame(c(" "))
      colnames(err) = paste("<h3 style='color: #2C3E50;'> <b>No data found for this gene name </b></h2>",  "<h4 style='color: #2C3E50;'>Ensure you are using the offical gene symbol (ie. Pomc). If this message persists, it is likely that the gene of interest was not detected during sequencing </h2>", sep= "<br>")
      output = err
    }
    print(output)
  }, na = "-", rownames = F, striped = T, width = "100%", spacing = "xs", align="l",  sanitize.text.function=identity) #End Tables
  
  ##PLOTS
  output$plotdata <- renderPlot({
    dataset <- datasetInput()
        res = dbSendQuery(db, paste0('SELECT * FROM ', dataset, ' WHERE "Gene" = ?'))
    dbBind(res, list(CapStr(input$GeneName)))
    outs = dbFetch(res)  
    outs$`..1` = NULL
    outs$Value = as.numeric(outs$Value)
    if(dim(outs)[1]==96){
      condensed = outs %>% group_by(Sample) %>%  summarize(mean_val = mean(Value, na.rm = TRUE), sd_val = sd(Value, na.rm = TRUE))
      condensed$Sample = factor(condensed$Sample, levels=c("THy/PHy", "F", "M", "H", "FRP", "ACA", "ACB", "VTA", "LHA", "PVH", "DMH", "VMH", "ARH", "PB", "NTS", "DVC", "ENT", "CENT2", "CUL4,5", "UVU", "CBN", "FL", "mESC B6-1", "mESC B6-2"))
      #condensed <-condensed[order(condensed$Sample),]
      dataplot = ggplot(condensed, aes(x=Sample, y=mean_val)) + 
        labs(y= "Transcripts/Million") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold"), 
              axis.text.y = element_text(face="bold"),
              axis.title.y = element_text(face="bold"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"), 
              panel.background = element_blank()) +
        geom_errorbar(aes(ymin=mean_val-sd_val, ymax=mean_val+sd_val), width=.2) +
        geom_hline(yintercept=0)+
        geom_line() +
        geom_bar(stat="identity", fill="#2C3E50")
      ###    
    }else{
      DEGs_reduced = subset(outs, outs$Feature %in% c("log2FC", "lfcSE"))
      DEGs_reduced_spread = spread(DEGs_reduced, "Feature" , "Value")
      DEGs_reduced_spread$Sample = factor(DEGs_reduced_spread$Sample, levels=c("THy/PHy", "M", "H", "FRP", "ACA", "ACB", "VTA", "LHA", "PVH", "DMH", "VMH", "ARH", "PB", "NTS", "DVC", "ENT", "CENT2", "CUL4,5", "UVU", "CBN", "FL"))
      
      dataplot = ggplot(DEGs_reduced_spread, aes(x=Sample, y=log2FC)) + 
        labs(y= "Log2 Fold change") +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold"),
              axis.text.y = element_text(face="bold"),
              axis.title.y = element_text(face="bold"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.background = element_blank()) +
        geom_errorbar(aes(ymin=log2FC-lfcSE, ymax=log2FC+lfcSE), width=.2) +
        geom_hline(yintercept=0)+
        geom_line() +
        geom_bar(stat="identity", fill="#2C3E50")
    }
    dataplot
  }, width = "auto")   
  
  
  
  
  #DOWNLOAD  
  output$downloadData <- downloadHandler(
    filename = "BrainEnergyBalanceData.csv",
    content = function(file) {
      write.csv({    dataset <- datasetInput()
        res = dbSendQuery(db, paste0('SELECT * FROM ', dataset, ' WHERE "Gene" = ?'))
        dbBind(res, list(CapStr(input$GeneName)))
        outs = dbFetch(res)  
        outs$`..1` = NULL
        outs$Value = as.numeric(outs$Value)
      if(dim(outs)[1]==96){
        SubsetGene_spread = spread(outs, "Rep", "Value", "Sample")
        SubsetGene_spread[ SubsetGene_spread == "Sample" ] <- NA
        SubsetGene_spread[3:6] = SubsetGene_spread[3:6] %>% mutate_if(is.character,as.numeric)
        SubsetGene_spread$Mean = apply(SubsetGene_spread[3:6], 1, mean, na.rm=TRUE)
        SubsetGene_spread$SD = apply(SubsetGene_spread[3:6], 1, sd, na.rm=TRUE)
        SubsetGene_spread$Sample = factor(SubsetGene_spread$Sample, levels=c("THy/PHy", "F", "M", "H", "FRP", "ACA", "ACB", "VTA", "LHA", "PVH", "DMH", "VMH", "ARH", "PB", "NTS", "DVC", "ENT", "CENT2", "CUL4,5", "UVU", "CBN", "FL", "mESC B6-1", "mESC B6-2"))
        output <-SubsetGene_spread[order(SubsetGene_spread$Sample),]
      }else{ #END DEGS, start TPM      
        SubsetGene_spread = spread(outs, "Feature" , "Value")
        SubsetGene_spread$Sample = factor(SubsetGene_spread$Sample, levels=c("THy/PHy", "M", "H", "FRP", "ACA", "ACB", "VTA", "LHA", "PVH", "DMH", "VMH", "ARH", "PB", "NTS", "DVC", "ENT", "CENT2", "CUL4,5", "UVU", "CBN", "FL"))
        output <-SubsetGene_spread[order(SubsetGene_spread$Sample),]
        output <- output[c("Sample", "Gene", "log2FC", "lfcSE", "pvalue", "padj", "baseMean")]
      }
      row.names(output) = output$Sample
      output$Sample = NULL
      print(output)}, file)})
  
  
  
} #End Server

shinyApp(ui = ui, server = server)
