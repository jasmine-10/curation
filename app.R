library(shiny)
library(dplyr)
library(stringr)

fulldata<-read.csv(file='fulldata.csv')
unique_molecules<-unique(fulldata$Molecule)

# User interface ----
ui <- fluidPage(
  titlePanel("BIOMARKER SIGNATURES OF SEPSIS"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.tabselected==1",helpText("Hello")),
      conditionalPanel(condition="input.tabselected==2",
                       helpText("Input the desired experimental parameters:"),
                       
                       #input omics type
                       radioButtons("omics", 
                                    label = "Molecule type:",
                                    choices = c("All", "Gene","Metabolite", "Non-coding RNA", "HERV"),
                                    selected = "All"),
                       #input platform
                       selectInput("platform",
                                   label="Platform:",
                                   choices=c("All", "10x Genomics Chromium Single Cell 3' V2","600 MHz Bruker Ultrashield Plus NMR spectrometer", "Affymetrix Human Exon 1.0 ST Array", "Affymetrix Human Gene 1.0 ST Array", "Affymetrix Human Genome U133 Plus 2.0 Array", "Affymetrix Human Genome U133A 2.0 Array", "Affymetrix Human Genome U133A Plus 2.0 Array", "Affymetrix Human Genome U219 Array", "Affymetrix Human Genome U95 V2 Array",
                                             "Agilent Human Gene Expression 4x44K V2 Microarray", "Agilent Human lncRNA Microarray V4.0", "Agilent Microarray Scanner", "Agilent SurePrint G3 human GE 8 x 60K V2 chips", "Agilent-014850 Whole Human Genome Microarray","Agilent-072363 SurePrint G3 Human GE V3 8x60K Microarray 039494", "Arraystar Human lncRNA Microarray V3.0", "Bio-Rad Bio-Plex 200 system", "Bio-Rad Human Cytokine Standard 27-Plex Assays panel",
                                             "Bruker AVANCE 600 MHz spectrometer", "Bruker AVANCE-II 600 MHz spectrometer", "Codelink 55K Human Array", "Custom Affymetrix HERV-V3 GeneChip Microarray", "Custom Affymetrix Human Transcriptome Array", "DiaSorin Liason", "FACSCanto flow cytometer", "GF Healthcare/Amersham Biosciences CodeLink UniSet Human I Bioarray", "Illumina HiSeq", "Illumina HiSeq 2000", "Illumina HiSeq 2500 sequencer",
                                             "Illumina Human Ref-8 V2.0 expression beadchip", "Illumina Human-6 V2.0 expression beadchip", "Illumina HumanHT-12 V3.0 expression beadchip", "Illumina HumanHT-12 V4.0 expression beadchip", "Illumina NextSeq 500","Illumina NovaSeq6000", "miRCURYLNA microRNA Array", "NanoString nCounter", "Nanostring nCounter Human Immunology V2", "NHICU Human 19K v1.0", "Roche Diagnostics Cobra e602", "Siemens Immulite 1000","Tecan HS Pro 4800"),
                                   selected="All",
                                   multiple=TRUE),
                       #input tissue type
                       selectInput("tissue",
                                   label="Tissue type:",
                                   choices=c("All", "A549 cells", "B cells", "Calu-3 cells", "CD14+ monocytes", "CD16+ monocytes", "CD4+ T cells", "CD8+ T cells", "Dendritic cells", "HUVEC", "Neutrophils", "NHBE cells", "NK cells", "PBMC", "Plasma", "PMN", "Serum", "Whole blood"),
                                   selected="All",
                                   multiple=TRUE),
                       #input infection source
                       selectInput("infection",
                                   label="Infection source:",
                                   choices=c("All", "Bacteria", "Burkholderia pseudomallei", "CAP", "Fecal peritontis", "E. coli", "Fungi", "Gram-negative bacteria", "Gram-positive bacteria", "LPS", "SARS-Cov-2", "Staphylococcus aureus", "UTI", "Virus"),
                                   selected="All",
                                   multiple=TRUE),
                       #input case condition
                       selectInput("case",
                                   label="Case condition:",
                                   choices=c("All", "ALI", "ARDS", "E. coli LPS", "LPS", "SARS-CoV-2", "Sepsis","Septicemic meliodosis", "SIRS"),
                                   selected="All",
                                   multiple=TRUE),
                       #input control condition
                       selectInput("control",
                                   label="Control condition:",
                                   choices=c("All", "Before infusion", "Healthy", "Mild infection", "Moderate infection", "No fever", "Non-septic", "PBS", "Seasonal respiratory infection", "Sepsis", "SIRS"),
                                   selected="All",
                                   multiple=TRUE),
                       #input age group
                       selectInput("age",
                                   label="Age group:",
                                   choices=c("All", "Neonate", "Pediatric", "Adult", "Senior"),
                                   selected="All",
                                   multiple=TRUE)
    ),
    conditionalPanel(condition="input.tabselected==3",
                     radioButtons("omics2", 
                                  label = "Molecule:",
                                  choices = c("All", "Gene","Metabolite", "Non-coding RNA", "HERV"),
                                  selected = "All"),
                     selectizeInput("molecule",
                                 label="Molecule:",
                                 options=list(maxOptions=10977),
                                 choices=c("All", as.character(unique_molecules)),
                                 selected="All",
                                 multiple=TRUE),
    )),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", value=1,helpText("Filler text here.")),
        tabPanel("By experimental conditions", value=2, helpText("Data table:"), dataTableOutput("data1")),
        tabPanel("By molecule",value=3,dataTableOutput("data2")),
        id="tabselected"
      )
    )
  
))

# Server logic ----
server <- function(input, output) {
  
  #by conditions
  #by omics
  filtered_omics<-reactive({
    if(input$omics=="All"){
      fulldata
    } else {
      fulldata %>%
        filter(Molecule_Type==input$omics)
    }
  })
  #by platform
  filtered_platform<-reactive({
    if(input$platform=="All"){
      filtered_omics()
    } else {
      filtered_omics() %>%
        filter(str_detect(Platform,input$platform)==TRUE)
    }
  })
  #by tissue
  filtered_tissue<-reactive({
    if(input$tissue=="All"){
      filtered_platform()
    } else {
      filtered_platform() %>%
        filter(str_detect(Tissue,input$tissue)==TRUE)
    }
  })
  #by infection source
  filtered_infection<-reactive({
    if(input$infection=="All"){
      filtered_tissue()
    } else {
      filtered_tissue() %>%
        filter(str_detect(Infection,input$infection)==TRUE)
    }
  })
  #by case condition
  filtered_case<-reactive({
    if(input$case=="All") {
      filtered_infection()
    } else {
      filtered_infection() %>%
        filter(str_detect(Case_Condition,input$case)==TRUE)
    }
  })
  #by control condition
  filtered_control<-reactive({
    if(input$control=="All") {
      filtered_case()
    } else {
      filtered_case() %>%
        filter(str_detect(Control_Condition,input$control)==TRUE)
    }
  })
  #by age group
  filtered_age<-reactive({
    if(input$age=="All") {
      filtered_control()
    } else {
      filtered_control() %>%
        filter(str_detect(Age_Group,input$age)==TRUE)
    }
  })
    
  
  
  #by molecule
  tab3<-reactive({
    if(input$omics2!="All"&input$molecule!="All") {
      filter(fulldata, Molecule_Type==input$omics2&Molecule==input$molecule)
    }else if (input$molecule!="All") {
      filter(fulldata, Molecule==input$molecule)
    } else if (input$omics2!="All"&input$molecule=="All"){
      filter(fulldata, Molecule_Type==input$omics2)
    } else {
      fulldata
    }
  })
  
  output$data1<-renderDataTable({
    filtered_age()
  })
  
  output$data2<-renderDataTable({
    tab3()
  })
  
}

# Run app ----
shinyApp(ui, server)