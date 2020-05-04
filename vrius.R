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
raw_data1 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/载体构建和病毒包装.xlsx',sheet=1,startRow = 1)
raw_data2 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/载体构建和病毒包装.xlsx',sheet=2,startRow = 3)

vector_vrius <- function(raw_data1,raw_data2,image_path,image_name){
  #表头定位
  raw_data1 <- raw_data1
  raw_data2 <- raw_data2
  data_head <- raw_data1[,1]
  target_info <- str_which(data_head, '靶序列信息')
  ex_type <- str_which(data_head, '实验类型')
  fun_type <- str_which(data_head, '靶序列功能')
  contract_num <- str_which(data_head, '合同号')
  primer_seq <- str_which(data_head, '测序引物信息')
  titer <- str_which(data_head, '病毒滴度')
  fontname <- "Arial"
  
  
  #产品信息速览表
  preread_ft <- raw_data2 %>%
    flextable()%>%
    add_header_lines("产品信息速览表")%>%
    bold(part = 'header')%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(i=1,size = 20,part = 'header')%>%
    fontsize(i=2,size = 12,part = 'header')%>%
    fontsize(j=-2,size = 10,part = 'body')%>%
    fontsize(j=2,size = 7.5,part = 'body')%>%
    autofit(add_h = 0,add_w = 0)%>%
    height(height = 1,part = 'body')%>%
    height(height = 0.4,part = 'header')%>%
    width(j=2,width = 2.7)%>%
    width(j=5,width = 0.9)%>%
    width(j=6,width = 0.5)%>%
    font(fontname = fontname, part = "all")
  
  #载体信息表
  target_info_data <-  raw_data1[(target_info+2):(primer_seq-1),]
  colnames(target_info_data) <- raw_data1[(target_info+1),]
  pict_height_coe <- length(target_info_data$`载体类型`)-length(target_info_data$`载体类型`[target_info_data$`载体类型`=='对照'])
  if(pict_height_coe<=0){
    pict_height_coe <- 1
  }

  
  #引物信息表
  primer_seq_data <- raw_data1[(primer_seq+2):(titer-1),]
  colnames(primer_seq_data) <- raw_data1[(primer_seq+1),]
  primer_seq_ft <- primer_seq_data %>%
    flextable(col_keys = c('载体编号','引物序列'))%>%
    add_header_lines("测序引物序列")%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w=0.5)%>%
    font(fontname = fontname, part = "all")
  
  #滴度表
  titer_data <- raw_data1[(titer+2):nrow(raw_data1),]
  colnames(titer_data) <- raw_data1[(titer+1),]
  titer_data_ft <- titer_data%>%
    flextable(col_keys = c('病毒编号','滴度','单位'))%>%
    add_header_lines(values = '病毒滴度测定结果')%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 10.5,part = 'body')%>%
    fontsize(size = 12,part = 'header')%>%
    bold(part = 'header')%>%
    autofit(add_h = 0.2,add_w=0.2)%>%
    font(fontname = fontname, part = "all")
  
  
  
  #读入模板
  my_doc <- read_docx('./data/vector&vrius_templete.docx')
  if(data_head[ex_type+1]=='载体构建'){
    if(data_head[fun_type+1]=='过表达'){
      cursor_reach(my_doc,keyword = "靶序列")
      n <- nrow(target_info_data)
      for(i in 1:n){
        body_add_par(my_doc,target_info_data[i,1],style = 'Normal')
        body_add_par(my_doc,target_info_data[i,2],style = 'Normal')
        body_add_par(my_doc,target_info_data[i,4],style = 'seq')
        body_add_par(my_doc,'',style = 'Normal')
      }
      #结果图片插入
      #图片高度控制
      #pict_height <- length(antibody_info$`抗体种类`[antibody_info$`抗体种类`=="一抗"])*2
      image_name <- dir("~/Desktop/使用教程和资料/输入图片示例",pattern = '载体图谱.*?.png')
      image_path <- c("~/Desktop/使用教程和资料/输入图片示例/载体图谱.png","~/Desktop/使用教程和资料/输入图片示例/载体图谱2.png")
      
      picture_list <- list('载体图谱')
      index <- unlist(lapply(picture_list,function(x){
        str_which(image_name,x)
      }))
      index <- sort(index,decreasing = T)
      for(pic in index){
        cursor_reach(my_doc,keyword = "载体图谱")
        body_add_par(my_doc,"",style = "pic_style")
        slip_in_img(my_doc,image_path[pic],width = 6.5,height = 6)
        slip_in_text(my_doc,image_name[pic])
      }
      
      cursor_reach(my_doc,keyword = "酶切验证")
      lapply(x,function(x){
        slip_in_img(x,width = 4.8,height = 4.2,style = 'pic_style')
      })
      cursor_reach(my_doc,keyword = "序列比对验证")
      lapply(x,function(x){
        slip_in_img(x,width = 4.8,height = 4.2,style = 'pic_style')
      })
      cursor_reach(my_doc,"测序引物序列")
      body_add_par("",style = "pic_style")
      body_add_flextable(value=primer_seq_ft)%>%
      cursor_reach(my_doc,keyword = "分子实验数据")
      lapply(x,function(x){
        slip_in_img(x,width = 4.8,height = 4.2,style = 'pic_style')
      })
      body_replace_all_text(my_doc,"\\A病毒实验数据.*?\\z","",only_at_cursor = FALSE)
    }else{
      ...
    }
  }else if(data_head[ex_type+1]=='病毒包装'){
    ...
  }else{
    ...
  }
  exists("my_doc")
  
  
  
  print(my_doc,'载体构建和病毒包装.docx')
  
  
  
  
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
    body_add_par(value = paste('项目名称:',data_head[fun_type+1],data_head[ex_type+1],sep = ''),style  = 'Subtitle')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
    body_add_break(pos = "after")%>%
    
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_flextable(value = preread_ft)%>%
    body_add_break(pos = "after")
  
  if(data_head[ex_type+1]=='载体构建'){
    body_add_par(my_doc,value = "载体构建信息", style = "heading 1")
    body_add_par(my_doc,value = "靶序列", style = "heading 2")
    n <- nrow(target_info_data)
    for(i in 1:n){
      body_add_par(my_doc,target_info_data[i,1],style = 'Normal')
      body_add_par(my_doc,target_info_data[i,2],style = 'Normal')
      body_add_par(my_doc,target_info_data[i,4],style = 'seq')
      body_add_par(my_doc,'',style = 'Normal')
    }
    my_doc%>%
      body_add_par(value = "载体图谱", style = "heading 2")%>%
      body_add_img(image_path[[1]],pos = "after",width = 4.8,height = 4.2,style = "heading 3")%>%
      body_add_fpar(fpar(
        '图 1.2.1  载体图谱',
        fp_p=fp_par(text.align = 'center')))%>%
      body_add_break(pos = "after")%>%
      body_add_par(value = "结果质控", style = "heading 1")
    
    if(data_head[fun_type+1]=='过表达'){
      my_doc%>%
        body_add_par(value = "质粒酶切验证", style = "heading 2")%>%
        body_add_img(image_path[[2]],pos = "after",width = 6.5,height = pict_height2,style = "heading 3")%>%
        body_add_fpar(fpar(
          '图 1.2.1  质粒酶切结果',
          fp_p=fp_par(text.align = 'center')))%>%
        body_add_par(value = "测序比对验证", style = "heading 2")%>%
        body_add_par(value = "测序比对结果见附件：分子实验数据。", style = "chinese_style")%>%
        body_add_break(pos = 'after')%>%
        body_add_par(value = "附录", style = "heading 1")%>%
        body_add_par(value = "测序引物序列", style = "heading 2")%>%
        body_add_flextable(value=primer_seq_ft)%>%
        body_add_par(value = "分子实验数据", style = "heading 2")%>%
        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")
      
      
    }else{
      my_doc%>%
        body_add_par(value = "测序比对验证", style = "heading 2")%>%
        body_add_img(image_path[[2]],pos = "after",width = 6.3,height = pict_height,style = "heading 3")%>%
        body_add_fpar(fpar(
          '图 2.1.1  靶序列测序结果',
          fp_p=fp_par(text.align = 'center')))%>%
        body_add_break(pos = 'after')%>%
        body_add_par(value = "附录", style = "heading 1")%>%
        body_add_par(value = "测序引物序列", style = "heading 2")%>%
        body_add_flextable(value=primer_seq_ft)%>%
        body_add_par(value = "分子实验数据", style = "heading 2")%>%
        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件等分子实验原始数据。", style = "chinese_style")
      
    }
  }else if(data_head[ex_type+1]=='病毒包装'){
    my_doc%>%
      body_add_par(value = "结果质控", style = "heading 1")%>%
      body_add_par(value = "质粒酶切验证", style = "heading 2")%>%
      body_add_img(image_path,pos = "after",width = 6.5,height = pict_height2,style = "heading 3")%>%
      body_add_par(value = "病毒滴度测定", style = "heading 2")%>%
      body_add_flextable(value=titer_data_ft)%>%
      body_add_break(pos = 'after')%>%
      
      body_add_par(value = "附录", style = "heading 1")%>%
      body_add_par(value = "病毒实验数据", style = "heading 2")%>%
      body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））", style = "chinese_style")%>%
      body_replace_all_text(paste(data_head[fun_type+1],data_head[ex_type+1],sep = ''), "病毒包装", only_at_cursor = FALSE)
    
  }else{
    body_add_par(my_doc,value = "载体构建信息", style = "heading 1")
    body_add_par(my_doc,value = "靶序列", style = "heading 2")
    n <- nrow(target_info_data)
    for(i in 1:n){
      body_add_par(my_doc,target_info_data[i,1],style = 'Normal')
      body_add_par(my_doc,target_info_data[i,2],style = 'Normal')
      body_add_par(my_doc,target_info_data[i,4],style = 'seq')
      body_add_par(my_doc,'',style = 'Normal')
    }
    my_doc%>%
      body_add_par(value = "载体图谱", style = "heading 2")%>%
      body_add_img(image_path[1],pos = "after",width = 4.8,height = 4.2,style = "heading 3")%>%
      body_add_fpar(fpar(
        '图 1.2.1  载体图谱',
        fp_p=fp_par(text.align = 'center')))%>%
      body_add_break(pos = "after")%>%
      body_add_par(value = "结果质控", style = "heading 1")
    
    if(data_head[fun_type+1]=='过表达'){
      my_doc%>%
        body_add_par(value = "质粒酶切验证", style = "heading 2")%>%
        body_add_img(image_path[[2]],pos = "after",width = 6.5,height = pict_height2,style = "heading 3")%>%
        body_add_par(value = "测序比对验证", style = "heading 2")%>%
        body_add_par(value = "测序比对结果见附件：分子实验数据。", style = "chinese_style")%>%
        body_add_par(value = "病毒滴度测定", style = "heading 2")%>%
        body_add_flextable(value=titer_data_ft)%>%
        body_add_break(pos = 'after')%>%
        
        body_add_par(value = "附录", style = "heading 1")%>%
        body_add_par(value = "测序引物序列", style = "heading 2")%>%
        body_add_flextable(value=primer_seq_ft)%>%
        body_add_par(value = "分子实验数据", style = "heading 2")%>%
        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")%>%
        body_add_par(value = "病毒实验数据", style = "heading 2")%>%
        body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））", style = "chinese_style")
      
      
    }else{
      my_doc%>%
        body_add_par(value = "测序比对验证", style = "heading 2")%>%
        body_add_img(image_path[[2]],pos = "after",width = 6.3,height = pict_height,style = "heading 3")%>%
        body_add_fpar(fpar(
          '图 2.1.1  靶序列测序结果',
          fp_p=fp_par(text.align = 'center')))%>%
        body_add_par(value = "病毒滴度测定", style = "heading 2")%>%
        body_add_flextable(value=titer_data_ft)%>%
        body_add_break(pos = 'after')%>%
        
        body_add_par(value = "附录", style = "heading 1")%>%
        body_add_par(value = "测序引物序列", style = "heading 2")%>%
        body_add_flextable(value=primer_seq_ft)%>%
        body_add_par(value = "分子实验数据", style = "heading 2")%>%
        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件等分子实验原始数据。", style = "chinese_style")%>%
        body_add_par(value = "病毒实验数据", style = "heading 2")%>%
        body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））", style = "chinese_style")
      
    }
  }
  my_doc
}




print(my_doc,'载体构建和病毒包装.docx')
