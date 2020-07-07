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
raw_data1 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/蛋白质印迹wb.xlsx',sheet=1,startRow = 1)
raw_data2 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/蛋白质印迹wb.xlsx',sheet=2,startRow = 3)
image_name <- dir('~/Desktop/使用教程和资料/输入图片示例/')
image_path <- paste0('~/Desktop/使用教程和资料/输入图片示例/','wb结果.png')

############################
#                          #
#   cell_product_man       #
#                          #
############################ 
cell_product_man <- function(raw_data1,raw_data2,image_path,image_name){
  #表头定位
  raw_data1 <- raw_data1
  raw_data2 <- raw_data2
  data_head <- raw_data1[,1]
  cell_info <- str_which(data_head, '细胞信息')
  contract_num <- str_which(data_head, '合同号')
  respon <- str_which(data_head, '负责人')
  fontname <- "Arial"
  
  ##
  cell_info_data <- raw_data1[(cell_info+2):nrow(raw_data1),]
  colnames(cell_info_data) <- raw_data1[(cell_info+1),]
  cell_info_data_ft1 <- cell_info_data[,-(8:14)]%>%
    flextable()%>%
    #footnote(i=1,j=1,part = 'header',value = as_paragraph('注：本部分实验涉及的其他试剂均为分析纯化学试剂。'))%>%
    add_header_lines(values = '表1.1  细胞信息表1')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.17)%>%
    font(fontname = fontname, part = "all")
  cell_info_data_ft2 <- cell_info_data[,-(2:7)]%>%
    flextable()%>%
    #footnote(i=1,j=1,part = 'header',value = as_paragraph('注：本部分实验涉及的其他试剂均为分析纯化学试剂。'))%>%
    add_header_lines(values = '表1.2  细胞信息表2')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.17)%>%
    font(fontname = fontname, part = "all")
  
  #建立word文档
  #读入模板
  my_doc <- read_docx('./data/cell_product_templete.docx')
  my_doc %>%
    cursor_bookmark("contract_num")%>%
    body_add_par(data_head[contract_num+1],style  = 'Subtitle')%>%
    cursor_bookmark("date")%>%
    body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
    cursor_bookmark("table1")%>%
    body_add_flextable(cell_info_data_ft1)%>%
    cursor_bookmark("table2")%>%
    body_add_flextable(cell_info_data_ft2)%>%
    body_add_img(image_path[[1]],pos = "after",width = 4,height = 2,style = "heading 3")%>%
    
  my_doc
}

  

############################
#                          #
#     cell_myco_test       #
#                          #
############################ 
cell_myco_test <- function(raw_data1,raw_data2,image_path,image_name){
  #表头定位
  raw_data1 <- raw_data1
  raw_data2 <- raw_data2
  data_head <- raw_data1[,1]
  cell_info <- str_which(data_head, '细胞信息')
  contract_num <- str_which(data_head, '合同号')
  respon <- str_which(data_head, '负责人')
  fontname <- "Arial"
  
  ##
  cell_info_data <- raw_data1[(cell_info+2):nrow(raw_data1),]
  colnames(cell_info_data) <- raw_data1[(cell_info+1),]
  cell_info_data_ft1 <- cell_info_data%>%
    flextable()%>%
    #footnote(i=1,j=1,part = 'header',value = as_paragraph('注：本部分实验涉及的其他试剂均为分析纯化学试剂。'))%>%
    add_header_lines(values = '表1.1  检测信息表')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.17)%>%
    font(fontname = fontname, part = "all")

  
  #建立word文档
  #读入模板
  my_doc <- read_docx('./data/cell_myco_templete.docx')
  my_doc %>%
    cursor_bookmark("contract_num")%>%
    body_add_par(data_head[contract_num+1],style  = 'Subtitle')%>%
    cursor_bookmark("date")%>%
    body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
    cursor_bookmark("table1")%>%
    body_add_flextable(cell_info_data_ft1)%>%
    cursor_bookmark("pic")%>%
    body_add_img(image_path[[1]],pos = "after",width = 6,height = 2,style = "pic_style")
    

  
  my_doc
}
  print(my_doc,'细胞支原体检测报告.docx')

  
  
  
  
    
  ############################
  #                          #
  #           wb             #
  #                          #
  ############################ 
wb <- function(raw_data1,raw_data2,image_path,image_name){
  #表头定位
  raw_data1 <- raw_data1
  data_head <- raw_data1[,1]
  contract_num <- str_which(data_head, '合同号')
  exp_index <- str_which(data_head, '抗体信息')
  group_index <- str_which(data_head, '实验分组')
  fontname <- "Arial"
  
  #抗体信息
  antibody_info <- raw_data1[(exp_index+2):(group_index-1),]
  colnames(antibody_info) <- raw_data1[exp_index+1,]
  antibody_info <- drop_na(antibody_info)
  antibody_ft <- antibody_info%>%
    flextable()%>%
    add_header_lines("表2.2.2 实验抗体信息")%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w = 0.2)%>%
    font(fontname = fontname, part = "all")
  
  #分组信息
  group_info <- raw_data1[(group_index+2):length(data_head),]
  colnames(group_info) <- raw_data1[group_index+1,]
  group_ft <- group_info%>%
    flextable()%>%
    add_header_lines("表2.2.1 实验分组信息")%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w = 0.2)%>%
    font(fontname = fontname, part = "all")
  
  #仪器与试剂
  #仪器
  equip_name <- c('Sorvall Legend Mircro 17台式离心机','Sorvall ST 16R冷冻离心机',
                  '微量移液器','生物安全柜','EVOS荧光显微成像系统',
                  '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0 mL离心管)',
                  '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱',
                  '凝胶成像分析系统','凝胶电泳系统')
  equip_source <- c('美国ThermoFisher公司','美国ThermoFisher公司','德国Eppendorf公司',
                    '美国ThermoFisher公司','美国ThermoFisher公司','美国ThermoFisher公司',
                    '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','北京赛智创业科技有限公司',
                    '美国BioRad公司')
  equip_table <- data.frame(equip_name,equip_source)
  equip_ft <- equip_table%>%
    flextable()%>%
    set_header_labels(equip_name='仪器名称',equip_source='生产厂家')%>%
    add_header_lines(values = '表1.1.1  主要仪器及生产商')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_w = 0.5,add_h = 0.15)%>%
    font(fontname = fontname, part = "all")
  
  
  #试剂列表
  regent_name <- c(
    'CCK-8细胞增殖及毒性检测试剂盒','DMEM高糖培养基','RPMI 1640培养基',
    '胎牛血清','NP-40裂解液',
    '蛋白酶抑制剂/磷酸酶抑制剂/EDTA','D-PBS','胰酶')
  regent_source <- c(
    '北京索莱宝科技有限公司','美国Gibco公司','美国Gibco公司',
    '美国Gibco公司','北京索莱宝科技有限公司',
    "上海碧云天生物科技有限公司","上海碧云天生物科技有限公司","上海碧云天生物科技有限公司")
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
  
  #读入模板
  my_doc <- read_docx('./data/wb_templete.docx')
  my_doc %>%
    cursor_bookmark("contract_num")%>%
    body_add_par(data_head[contract_num+1],style  = 'Subtitle')%>%
    cursor_bookmark("date")%>%
    body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
    cursor_bookmark("equipment_info")%>%
    body_add_flextable(equip_ft)%>%
    cursor_bookmark("regent_info")%>%
    body_add_flextable(regent_ft)%>%
    cursor_bookmark("exp_group")%>%
    body_add_par('',style = 'pic_style')%>%
    body_add_flextable(antibody_ft)%>%
    cursor_bookmark("exp_group")%>%
    body_add_flextable(group_ft)
  
  #结果图片插入
  #图片高度控制
  pict_height <- length(antibody_info$`抗体种类`[antibody_info$`抗体种类`=="一抗"])
  
  picture_list <- list('wb结果')
  index <- unlist(lapply(picture_list,function(x){
    str_which(image_name,x)
  }))
  image_path <- image_path[index]
  for(pic in image_path){
    cursor_bookmark(my_doc,"result")
    body_add_par(my_doc,'',style = 'pic_style')
    slip_in_img(my_doc,pic,width = 6.5,height = pict_height)
  }
  my_doc
}
  
  
  
  print(my_doc,'wb报告.docx')
  
  
