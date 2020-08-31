options(shiny.maxRequestSize = 30*1024^2)
library(ggplot2)
source("qPRC2.R",encoding = 'UTF8')
library(shiny)
library(openxlsx)
library(officer)
library(stringr)
library(dplyr)
library(flextable)
library(tidyr)
library(viridis)
library(cowplot)
library(magick)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  title = "standar output",
  hr(),
  fluidRow(
    column(4,wellPanel(
    selectInput('input_type','选择报告种类',
                c('CCK8_MTT','qPCR','reporter_gene_assay',
                  'gene_kd_oe','vector_vrius','colony_formation',
                  'wound_healing','Transwell_invasion','cell_cycle',
                  'cell_apoptosis','Co-IP','EdU_Br',
                  'Transwell_migration','MOI预实验','WB',
                  '细胞支原体检测','细胞产品说明'))
  )),
    uiOutput('uip')
  ),
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
  hr()
)



# Define server logic to read selected file ----
server <- function(input, output) {
  output$uip <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "CCK8_MTT" = tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d943",
                           "点此链接下载Excel模板", target = "_blank"),
           "qPCR" = tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d942",
                              "点此链接下载Excel模板", target = "_blank"),
           "reporter_gene_assay" =  tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d947",
                                           "点此链接下载Excel模板", target = "_blank"),
           "gene_kd_oe" = tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d93c",
                                 "点此链接下载Excel模板", target = "_blank"),
           'vector_vrius'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d944",
                                  "点此链接下载Excel模板", target = "_blank"),
           'colony_formation'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d93d",
                                      "点此链接下载Excel模板", target = "_blank"),
           'wound_healing'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d945",
                                   "点此链接下载Excel模板", target = "_blank"),
           'Transwell_invasion'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d93f",
                               "点此链接下载Excel模板", target = "_blank"),
           'cell_cycle'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d941",
                                "点此链接下载Excel模板", target = "_blank"),
           'cell_apoptosis'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d93e",
                                    "点此链接下载Excel模板", target = "_blank"),
           'Co-IP'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d93b",
                           "点此链接下载Excel模板", target = "_blank"),
           'EdU_Br'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d946",
                         "点此链接下载Excel模板", target = "_blank"),
           'Transwell_migration'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5ed06429ab2cbf0021d5d940",
                               "点此链接下载Excel模板", target = "_blank"),
           'MOI预实验'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5f12ecef0a8eb100211f6386",
                                    "点此链接下载Excel模板", target = "_blank"),
           'WB'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5f12ecef0a8eb100211f6387",
                           "点此链接下载Excel模板", target = "_blank"),
           '细胞支原体检测'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5f12ecef0a8eb100211f6384",
                            "点此链接下载Excel模板", target = "_blank"),
           '细胞产品说明'= tags$a(href = "https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5ed0641b21c5cf0021786f8f/work/5f12ecef0a8eb100211f6385",
                                         "点此链接下载Excel模板", target = "_blank")
           )
  })
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "CCK8_MTT" = fileInput("file2", "目前无需额外上传",
                              multiple = TRUE,
                              placeholder = 'no more needed'),
           "qPCR" = fileInput("file2", "目前无需额外上传",
                                 multiple = FALSE,
                                 placeholder = 'no more needed'),
           "reporter_gene_assay" =  fileInput("file2", "目前无需额外上传",
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
           'Transwell_invasion'= fileInput("file2","目前无需额外上传",
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
           'EdU_Br'= fileInput("file2","目前无需额外上传",
                            multiple = TRUE,
                            placeholder = 'no more needed'),
           'Transwell_migration'= fileInput("file2","目前无需额外上传",
                                  multiple = TRUE,
                                  placeholder = 'no more needed'),
           
           'MOI预实验'= fileInput("file2","细胞预实验结果图片",
                              multiple = TRUE,
                              placeholder = 'K562-05-72-G-3-10X.jpg'),
           'WB'= fileInput("file2","wb结果",
                               multiple = TRUE,
                               placeholder = 'wb结果.png'),
           '细胞支原体检测'= fileInput("file2","支原体检测结果",
                                            multiple = TRUE,
                                            placeholder = '支原体检测结果.png')
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
