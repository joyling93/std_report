options(shiny.maxRequestSize = 30*1024^2)
library(ggpubr)
source("qPRC2.R")
library(shiny)
library(openxlsx)
library(officer)
library(stringr)
library(dplyr)
library(flextable)
library(tidyr)
library(viridis)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  title = "standar output",
  hr(),
  fluidRow(
    column(6,wellPanel(
    selectInput('input_type','选择报告种类',
                c('cck8','qRT-PCR','reportor_gene_assay','gene_kd_oe','vector_vrius','colony_formation'
                  ,'wound_healing','transwell','cell_cycle','cell_apoptosis','Co-IP','EdU','migration'))
  ))),
  fluidRow(
    column(4,wellPanel(fileInput('file1','上传excel模板',
                                 multiple = FALSE,
                                 placeholder = 'eg: A-FW-XXX-XXX-结题报告类型-结题报告.xlsx'))),
    column(5,wellPanel(
      uiOutput('ui')
    ))
  ),
  hr(),
  h5("已上传文件列表："),
  fluidRow(column(3, verbatimTextOutput("value1",placeholder = TRUE))),
  h5("已上传图片列表："),
  fluidRow(column(3, verbatimTextOutput("value2",placeholder = TRUE))),
      downloadButton(outputId = "download", label = "在此处下载报告"),
  hr(),
  tags$a(href = "https://www.teambition.com/task/5e3bb321f931af0021568a0b",
  "点此链接下载Excel模板集", target = "_blank")
)



# Define server logic to read selected file ----
server <- function(input, output) {
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "cck8" = fileInput("file2", "目前无需额外上传",
                              multiple = TRUE,
                              placeholder = 'no more needed'),
           "qRT-PCR" = fileInput("file2", "目前无需额外上传",
                                 multiple = FALSE,
                                 placeholder = 'no more needed'),
           "reportor_gene_assay" =  fileInput("file2", "目前无需额外上传",
                                              multiple = TRUE,
                                              placeholder = 'no more needed'),
           "gene_kd_oe" = fileInput("file2", "上传 病毒感染图片、细胞PCR检测结果 和 wb实验结果(可选) 图片",
                                 multiple = TRUE,
                                 placeholder = 'eg:病毒感染图片.png,细胞PCR检测结果.png'),
           'vector_vrius'= fileInput("file2", "上传 载体图谱、靶序列测序结果 或 酶切鉴定结果 图片",
                                     multiple = TRUE,
                                     placeholder = 'eg: 载体图谱.png,靶序列测序结果.png'),
           'colony_formation'= fileInput("file2","目前无需额外上传",
                                         multiple = TRUE,
                                         placeholder = 'no more needed'),
           'wound_healing'= fileInput("file2","目前无需额外上传",
                                         multiple = TRUE,
                                         placeholder = 'no more needed'),
           'transwell'= fileInput("file2","目前无需额外上传",
                                      multiple = TRUE,
                                      placeholder = 'no more needed'),
           'cell_cycle'= fileInput("file2","目前无需额外上传",
                                  multiple = TRUE,
                                  placeholder = 'no more needed'),
           'cell_apoptosis'= fileInput("file2","目前无需额外上传",
                                       multiple = TRUE,
                                       placeholder = 'no more needed'),
           'Co-IP'= fileInput("file2","coip实验结果",
                                       multiple = TRUE,
                                       placeholder = 'coip实验结果.png'),
           'EdU'= fileInput("file2","目前无需额外上传",
                            multiple = TRUE,
                            placeholder = 'no more needed'),
           'migration'= fileInput("file2","目前无需额外上传",
                            multiple = TRUE,
                            placeholder = 'no more needed')
    )
  })
  
  output$value1 <- renderText({
    print(as.character(input$file1$name))
  })
  output$value2 <- renderText({
    print(paste(as.character(input$file2$name),collapse = '\n'))
  })
  output$download <- downloadHandler(
    filename = function(){
      y <- input$file1$name
      paste0(str_replace_all(y,'.xlsx','.docx'))},
    content = function(file){
      raw_data1 <- read.xlsx(input$file1$datapath,sheet = 1,startRow = 1)
      raw_data2 <- read.xlsx(input$file1$datapath,sheet=2,startRow = 3)
      image_path <- input$file2$datapath
      image_name <- input$file2$name
      print(trans(raw_data1,raw_data2,image_path,image_name),target = file)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
