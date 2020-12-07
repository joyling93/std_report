library(ggplot2)
library(shiny)
library(reshape2)
library(openxlsx)
library(officer)
library(stringr)
library(dplyr)
library(flextable)
library(tidyr)
library(viridis)
library(cowplot)
library(magick)
dir('~/Desktop/使用教程和资料/输入模板示例/')
#raw_data1 <- read.xlsx('~/Desktop/HUVEC本底检测.xlsx',sheet=1)
#raw_data2 <- read.xlsx('~/Desktop/HUVEC本底检测.xlsx',sheet=2,startRow = 3)
raw_data1 <- read.xlsx('../D-FW-201912111140-徐燕-PC9&A549细胞系建立-RT-qPCR.xlsx',sheet=1)
raw_data2 <- read.xlsx('../D-FW-201912111140-徐燕-PC9&A549细胞系建立-RT-qPCR.xlsx',sheet=2,startRow = 3)


image_name <- dir('~/Desktop/使用教程和资料/输入图片示例/')
image_path <- paste0('~/Desktop/使用教程和资料/输入图片示例/',image_name)
setwd('/Users/zhuomingx/Desktop/Rbio/r\ scrirpt/std_report')
source('/Users/zhuomingx/Desktop/Rbio/r\ scrirpt/std_report/bin/qRTPCR.R')

vector_virus <- function(raw_data1,raw_data2,image_path,image_name){
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
  pre_read_ft <- raw_data2 %>%
    flextable()%>%
    add_header_lines("产品信息速览表")%>%
    add_header_lines(data_head[contract_num+1])%>%
    bold(part = 'header')%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(i=1:2,size = 20,part = 'header')%>%
    fontsize(i=3,size = 12,part = 'header')%>%
    fontsize(j=-2,size = 10,part = 'body')%>%
    fontsize(j=1:3,size = 7.5,part = 'body')%>%
    autofit(add_h = 0,add_w = 0)%>%
    height(height = 1,part = 'body')%>%
    height(height = 0.4,part = 'header')%>%
    width(j=1,width = 1.5)%>%
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
  
  my_doc <- read_docx(ifelse(data_head[fun_type+1]=='过表达'
                             ,'./data/vector&vrius_templete.docx'
                             ,'./data/vector&vrius_templete2.docx'))
  my_doc %>%
    cursor_bookmark("contract_num")%>%
    body_add_par(data_head[contract_num+1],style  = 'Subtitle')%>%
    cursor_bookmark("date")%>%
    body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
    cursor_bookmark("theme")%>%
    body_add_par(value = paste('项目名称:',data_head[fun_type+1],data_head[ex_type+1],sep = ''),style  = 'Subtitle')%>%
    cursor_reach("pre_read_ft")%>%
    body_remove()%>%
    body_add_par('',style = "pic_style")%>%
    body_add_flextable(pre_read_ft)
  
  if(data_head[ex_type+1]=='载体构建'){
    my_doc %>%
      cursor_reach("测序引物序列")%>%
      body_add_par("",style = "pic_style")%>%
      body_add_flextable(value=primer_seq_ft)%>%
      cursor_bookmark('seq')
    #cursor_reach(keyword = "靶序列")
    n <- nrow(target_info_data)
    for(i in 1:n){
      body_add_par(my_doc,target_info_data[i,1],style = 'Normal')
      body_add_par(my_doc,target_info_data[i,2],style = 'Normal')
      body_add_par(my_doc,target_info_data[i,4],style = 'seq')
      body_add_par(my_doc,'',style = 'Normal')
    }
    if(data_head[fun_type+1]=='过表达'){
      #确定报告结果类别
      picture_list <- list('载体图谱',"酶切鉴定结果")
      #删除多余
      my_doc %>%
        cursor_reach(keyword = "分子实验数据")%>%
        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")%>%
        cursor_reach(keyword = "病毒实验数据")%>%
        body_remove()%>%
        cursor_reach(keyword = "测序比对验证")%>%
        body_remove()%>%
        cursor_reach(keyword = "病毒滴度测定")%>%
        body_remove()
    }else{
      picture_list <- list('载体图谱',"靶序列测序结果")
      my_doc %>%
        cursor_reach(keyword = "分子实验数据")%>%
        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")%>%
        cursor_reach(keyword = "病毒实验数据")%>%
        body_remove()%>%
        cursor_reach(keyword = "质粒酶切验证")%>%
        body_remove()%>%
        cursor_reach(keyword = "病毒滴度测定")%>%
        body_remove()
    }
  }else if(data_head[ex_type+1]=='病毒包装'){
    picture_list <- list('酶切鉴定结果')
    my_doc %>%
      cursor_bookmark('seq')%>%
      #cursor_reach(keyword = "靶序列")%>%
      body_remove()%>%
      cursor_reach(keyword = "^载体构建信息$")%>%
      body_remove()%>%
      cursor_reach(keyword = "^载体$")%>%
      body_remove()%>%
      cursor_reach(keyword = "测序比对验证")%>%
      body_remove()%>%
      cursor_reach("病毒滴度测定")%>%
      body_add_par("",style = "pic_style")%>%
      body_add_flextable(value=titer_data_ft)%>%
      cursor_reach(keyword = "测序引物序列")%>%
      body_remove()%>%
      cursor_reach(keyword = "分子实验数据")%>%
      body_remove()%>%
      cursor_reach(keyword = "病毒实验数据")%>%
      body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））", style = "chinese_style")
    
  }else{
    my_doc %>%
      cursor_reach("测序引物序列")%>%
      body_add_par("",style = "pic_style")%>%
      body_add_flextable(value=primer_seq_ft)%>%
      cursor_bookmark('seq')
      #cursor_reach(keyword = "靶序列")
      n <- nrow(target_info_data)
      for(i in 1:n){
        body_add_par(my_doc,target_info_data[i,1],style = 'Normal')
        body_add_par(my_doc,target_info_data[i,2],style = 'Normal')
        body_add_par(my_doc,target_info_data[i,4],style = 'seq')
        body_add_par(my_doc,'',style = 'Normal')
      }
      if(data_head[fun_type+1]=='过表达'){
        #确定报告结果类别
        picture_list <- list('载体图谱',"酶切鉴定结果")
        my_doc %>% cursor_reach(keyword = "测序比对验证")%>%
          body_remove()
      }else{
        picture_list <- list('载体图谱',"靶序列测序结果")
        my_doc %>% cursor_reach(keyword = "质粒酶切验证")%>%
          body_remove()
      }
      my_doc %>%
        cursor_reach("病毒滴度测定")%>%
        body_add_par("",style = "pic_style")%>%
        body_add_flextable(value=titer_data_ft)%>%
        cursor_reach(keyword = "分子实验数据")%>%
        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")%>%
        cursor_reach(keyword = "病毒实验数据")%>%
        body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））", style = "chinese_style")
  }
  
  #图片高度控制
  pict_height_coe <- length(target_info_data$`载体类型`)-length(target_info_data$`载体类型`[target_info_data$`载体类型`=='对照'])
  if(pict_height_coe<=0){
    pict_height_coe <- 1
  }
  pict_height <- pict_height_coe*1.7
  pict_height2 <- pict_height_coe*2.7
  caption <- gsub('.png|.jpg',"",image_name)
  
  #图片插入
  lapply(picture_list,function(x){
    index <- str_which(image_name,x)
    index <- sort(index,decreasing = T)
    if(x=="载体图谱"){
      pwidth <- 2.6
      pheight <- 2.6
    }else if(x=="酶切鉴定结果"){
      pwidth <- 6.5
      pheight <- pict_height2
    }else if(x=="靶序列测序结果"){
      pwidth <- 6.3
      pheight <- pict_height
    }
    for(pic in index){
      cursor_reach(my_doc,keyword = x)
      body_add_par(my_doc,"",style = "pic_style")
      slip_in_img(my_doc,image_path[pic],width = pwidth,height = pheight)
      #body_add_par(my_doc,"",style = "pic_style")
      #slip_in_text(my_doc,caption[pic])
    }
  })
  body_replace_all_text(my_doc,'#.*?#','',only_at_cursor =FALSE)
  my_doc
}