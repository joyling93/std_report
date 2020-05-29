library(ggpubr)
library(shiny)
library(reshape2)
library(openxlsx)
library(officer)
library(stringr)
library(dplyr)
library(flextable)
library(tidyr)
library(viridis)

dir('~/Desktop/使用教程和资料/输入模板示例/')
raw_data1 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/稳转株构建范例.xlsx',sheet=1,startRow = 1)
raw_data2 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/稳转株构建范例.xlsx',sheet=2,startRow = 3)
image_name <- dir('~/Desktop/使用教程和资料/输入图片示例/')
image_path <- paste0('~/Desktop/使用教程和资料/输入图片示例/',image_name)

############################
#                          #
#        gene_kd_oe        #
#                          #
############################

gene_kd_oe <- function(raw_data1,image_path,image_name){
  #表头定位
  raw_data1 <- raw_data1
  data_head <- raw_data1[,1]
  detail_info <- str_which(data_head, '引物信息')
  ex_group <- str_which(data_head, '实验分组')
  contract_num <- str_which(data_head, '合同号')
  qc_res <- str_which(data_head, '质检结果')
  infect_res <- str_which(data_head, '细胞筛选结果')
  pcr_res <- str_which(data_head, 'RT-qPCR检测结果')
  wb_res <- str_which(data_head, 'WB实验结果')
  conclution <- str_which(data_head, '结论')
  fontname <- "Arial"
  
  #上传图片定位
  picture_list <- list('病毒感染图片','细胞PCR检测结果','wb实验结果')
  index <- unlist(lapply(picture_list,function(x){
    str_which(image_name,x)
  }))
  image_path <- image_path[index]
  
  #实验分组
  assay_group_data <-  raw_data1[(ex_group+2):(qc_res-1),]
  colnames(assay_group_data) <- raw_data1[(ex_group+1),]
  assay_group_ft <- assay_group_data %>%
    flextable(col_keys=c('载体名称','细胞','稳转株描述'))%>%
    add_header_lines("表2.2.2  分组信息表")%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w=0.5)%>%
    font(fontname = fontname, part = "all")
  
  #引物信息
  detail_info_data <-  raw_data1[(detail_info+2):(ex_group-1),]
  colnames(detail_info_data) <- raw_data1[(detail_info+1),]
  detail_info_ft <- detail_info_data %>%
    flextable()%>%
    add_header_lines("表2.2.1  引物信息表")%>%
    align(i=1,align = 'center',part = 'head')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w=0.5)%>%
    width(j = c(1,4,5), width = 0.8)%>%
    font(fontname = fontname, part = "all")
  
  #仪器与试剂
  #仪器
  equip_name <- c('Sorvall Legend Mircro 17台式离心机','Sorvall ST 16R冷冻离心机',
                  '微量移液器','生物安全柜','EVOS荧光显微成像系统',
                  '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0mL离心管)',
                  '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱'
  )
  equip_source <- c('美国ThermoFisher公司','美国ThermoFisher公司','德国Eppendorf公司',
                    '美国ThermoFisher公司','美国ThermoFisher公司','美国ThermoFisher公司',
                    '美国Axygen公司','美国Corning公司','美国ThermoFisher公司')
  equip_table <- data.frame(equip_name,equip_source)
  equip_ft <- equip_table%>%
    flextable()%>%
    set_header_labels(equip_name='仪器名称',equip_source='生产厂家')%>%
    add_header_lines(values = '表1.1.1  主要仪器及生产商')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_w = 0.7,add_h = 0.15)%>%
    font(fontname = fontname, part = "all")
  
  #试剂列表
  regent_name <- c('质粒小量快速提取试剂盒(离心柱型)','限制性内切酶类',
                   'DNA Ligase','慢病毒包装试剂盒','EpFect Transfection Reagent',
                   'EvaGreen 2× Master Mix','DMEM高糖培养基','RPMI 1640培养基',
                   'Fetal Bovine Serum(Defined)胎牛血清')
  regent_source <- c('北京艾德莱生物科技有限公司','美国NEB公司/美国ThermoFisher公司',
                     '北京合生基因科技有限公司','北京合生基因科技有限公司','北京合生基因科技有限公司',
                     '北京合生基因科技有限公司','美国Gibco公司','美国Gibco公司','美国Gibco公司')
  regent_table <- data.frame(regent_name,regent_source)
  regent_ft <- regent_table%>%
    flextable()%>%
    set_header_labels(regent_name='试剂名称',regent_source='生产厂家')%>%
    footnote(i=1,j=1,part = 'header',value = as_paragraph('注：本部分实验涉及的其他试剂均为分析纯化学试剂。'))%>%
    add_header_lines(values = '表1.2.1  主要试剂及生产商')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_w = 0.8,add_h = 0.17)%>%
    height(height = 0.3,part = 'foot')%>%
    font(fontname = fontname, part = "all")
  
  #建立word文档
  my_doc <- read_docx('./data/template.docx')
  my_doc %>%
    body_add_par('',style = 'Normal')%>%
    body_add_par(value = data_head[contract_num+1],style  = 'Subtitle')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par("项目结题报告",style  = 'Title')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par(value = '项目名称：稳转细胞株构建服务',style  = 'Subtitle')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "仪器与试剂", style = "heading 1") %>%
    body_add_par("实验仪器",style='heading 2')  %>%
    body_add_flextable(value = equip_ft) %>%
    body_add_par("实验试剂",style='heading 2')  %>%
    body_add_flextable(value = regent_ft) %>%
    
    
    body_add_par(value = "实验方法和分组", style = "heading 1") %>%
    body_add_par("实验方法",style='heading 2')  %>%
    body_add_par('实验原理：为了探讨目的基因在特定细胞中发挥的生物学功能，本实验通过构建目的基因沉默/过表达慢病毒质粒，利用第二代慢病毒包装系统制备、浓缩慢病毒液，并感染特定细胞株。经过抗性或流式筛选获得敲低/过表达目的基因的特定细胞系。',style='Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('具体步骤（抗性筛选）：',style = 'Normal')%>%
    body_add_par('1、6 孔板铺板细胞数 6×10^5，按 MOI=15 每孔加 100μl 病毒液，第二天换液继续培养，培养第三天加 puromycin 进行抗性筛选，待对照野生型细胞全部筛死时停止药筛，即可获得稳定敲低/过表达目的基因的细胞株以及对照空载体细胞株。',style='Normal')%>%
    body_add_par('2、收集生长状态良好的目的细胞，使用 Trizol 试剂提取总 RNA，然后用反转录试剂盒反转录出 cDNA，用目的基因定量引物对进行 RT-qPCR 检测野生型、目的细胞系以及对照细胞系中的目的基因的表达情况。',style='Normal')%>%
    body_add_break(pos = "after")%>%
    body_add_par("实验分组",style='heading 2')  %>%
    body_add_flextable(value = detail_info_ft) %>%
    body_add_par("",style='Normal')%>%
    body_add_flextable(value = assay_group_ft) %>%
    body_add_break(pos = "after")
  if(length(image_path)==2){
    my_doc %>%
      body_add_par(value = "结果与讨论", style = "heading 1") %>% 
      body_add_par("细胞筛选结果", style = "heading 2")%>% 
      body_add_img(image_path[[1]],pos = "after",width = 4,height = 2,style = "heading 3")%>%
      body_add_fpar(fpar(
        '图3.1.1  病毒感染图片',
        fp_p=fp_par(text.align = 'center')))%>%
      body_add_par(paste0('结果：',raw_data1[infect_res,2]),style = 'Normal')%>%
      body_add_par("RT-qPCR检测结果", style = "heading 2")%>% 
      body_add_img(image_path[[2]],pos = "after",width = 4,height = 2,style = "heading 3")%>%
      body_add_fpar(fpar(
        '图3.1.2  细胞RT-qPCR检测结果',
        fp_p=fp_par(text.align = 'center')))%>%
      body_add_par(paste0('结果：',raw_data1[pcr_res,2]),style = 'Normal')%>%
      body_add_break(pos = "after")%>%
      
      body_add_par(value = "实验结论", style = "heading 2")%>%
      body_add_par(paste0('结果：',raw_data1[conclution,2]),style = 'Normal')
  }else{
    my_doc %>%
      body_add_par(value = "结果与讨论", style = "heading 1") %>% 
      body_add_par("细胞筛选结果", style = "heading 2")%>% 
      body_add_img(image_path[[1]],pos = "after",width = 4,height = 2,style = "heading 3")%>%
      body_add_fpar(fpar(
        '图3.1.1  病毒感染图片',
        fp_p=fp_par(text.align = 'center')))%>%
      body_add_par(paste0('结果：',raw_data1[infect_res,2]),style = 'Normal')%>%
      body_add_par("RT-qPCR检测结果", style = "heading 2")%>% 
      body_add_img(image_path[[2]],pos = "after",width = 4,height = 2,style = "heading 3")%>%
      body_add_fpar(fpar(
        '图3.1.2  细胞RT-qPCR检测结果',
        fp_p=fp_par(text.align = 'center')))%>%
      body_add_par(paste0('结果：',raw_data1[pcr_res,2]),style = 'Normal')%>%
      body_add_break(pos = "after")%>%
      body_add_img(image_path[[3]],pos = "after",width = 4,height = 2,style = "heading 3")%>%
      body_add_fpar(fpar(
        '图3.1.2  WesternBlot实验结果',
        fp_p=fp_par(text.align = 'center')))%>%
      body_add_par(paste0('结果：',raw_data1[wb_res,2]),style = 'Normal')%>%
      body_add_break(pos = "after")%>%
      
      body_add_par(value = "实验结论", style = "heading 2")%>%
      body_add_par(paste0('结果：',raw_data1[conclution,2]),style = 'Normal')
  }
  my_doc
}



