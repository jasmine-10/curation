library(shiny)
library(dplyr)

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
                                    choices = c("All", "Genes","Metabolites", "Non-coding RNA", "HERVs"),
                                    selected = "All"),
                       #input platform
                       selectInput("platform",
                                   label="Platform:",
                                   choices=c("All", "10x Genomics Chromium Single Cell 3' V2","600 MHz Bruker Ultrashield Plus NMR spectrometer", "Affymetrix Human Exon 1.0 ST Array", "Affymetrix Human Gene 1.0 ST Array", "Affymetrix Human Genome U133 Plus 2.0 Array", "Affymetrix Human Genome U133A 2.0 Array", "Affymetrix Human Genome U133A Plus 2.0 Array", "Affymetrix Human Genome U219 Array", "Affymetrix Human Genome U95 V2 Array",
                                             "Agilent Human Gene Expression 4x44K V2 Microarray", "Agilent Human lncRNA Microarray V4.0", "Agilent Microarray Scanner", "Agilent SurePrint G3 human GE 8 x 60K V2 chips", "Agilent-014850 Whole Human Genome Microarray","Agilent-072363 SurePrint G3 Human GE v3 8x60K Microarray 039494", "Arraystar Human LncRNA Microarray V3.0", "Bio-Rad Bio-Plex 200 system", "Bio-Rad Human Cytokine Standard 27-Plex Assays panel",
                                             "Bruker AVANCE 600 MHz spectrometer", "Bruker AVANCE-II 600 MHz spectrometer", "Codelink 55K Human Array", "Custom Affymetrix HERV-V3 GeneChip Microarray", "Custom Affymetrix Human Transcriptome Array", "DiaSorin Liason", "FACSCanto flow cytometer", "GF Healthcare/Amersham Biosciences CodeLink UniSet Human I Bioarray", "Illumina HiSeq", "Illumina HiSeq 2000", "Illumina HiSeq 2500 sequencer",
                                             "Illumina Human Ref-8 V2.0 expression beadchip", "Illumina Human-6 V2.0 expression beadchip", "Illumina HumanHT-12 V3.0 expression beadchip", "Illumina HumanHT-12 V4.0 expression beadchip", "Illumina NextSeq 500","Illumina NovaSeq6000", "miRCURYLNA microRNA Array", "NanoString nCounter", "Nanostring nCounter Human Immunology V2", "NHICU Human 19K v1.0", "Roche Diagnostics Cobra e602", "Siemens Immulite 1000","Tecan HS Pro 4800"),
                                   selected="All"),
                       #input tissue type
                       selectInput("tissue",
                                   label="Tissue type:",
                                   choices=c("All", "A549 cells", "B cells", "Calu-3 cells", "CD14+ monocytes", "CD16+ monocytes", "CD4+ T cells", "CD8+ T cells", "Dendritic cells", "HUVEC", "Neutrophils", "NHBE cells", "NK cells", "PBMC", "Plasma", "PMN", "Serum", "Whole blood"),
                                   selected="All"),
                       #input infection source
                       selectInput("infection",
                                   label="Infection source:",
                                   choices=c("All", "Bacteria", "Burkholderia pseudomallei", "CAP", "Fecal peritontis", "E. coli", "Fungi", "Gram-negative bacteria", "Gram-positive bacteria", "LPS", "SARS-Cov-2", "Staphylococcus aureus", "UTI", "Virus"),
                                   selected="All"),
                       #input case condition
                       selectInput("case",
                                   label="Case condition:",
                                   choices=c("All", "ALI", "ARDS", "E. coli LPS", "LPS", "SARS-CoV-2", "Sepsis","Septicemic meliodosis", "SIRS"),
                                   selected="All"),
                       #input control condition
                       selectInput("control",
                                   label="Control condition:",
                                   choices=c("All", "Before infusion", "Healthy", "Mild infection", "Moderate infection", "No fever", "Non-septic", "PBS", "Seasonal respiratory infection", "Sepsis", "SIRS"),
                                   selected="All"),
                       #input age group
                       selectInput("age",
                                   label="Age group:",
                                   choices=c("All", "Neonate", "Pediatric", "Adult", "Senior"),
                                   selected="All")
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
                                 selected="All")
    )),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", value=1,helpText("Filler text here.")),
        tabPanel("By experimental conditions", value=2, helpText("Data table:")),
        tabPanel("By molecule",value=3, dataTableOutput("data1")),
        id="tabselected"
      )
    )
  
))

# Server logic ----
server <- function(input, output) {
  
  
  
  #by molecule
  tab<-reactive({
    filter(fulldata, Molecule==input$molecule)
  })
  output$data1<-renderDataTable({
    tab()
  })

}

# Run app ----
shinyApp(ui, server)