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
raw_data1 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/迁移报告范例.xlsx',sheet=1,startRow = 1)
raw_data2 <- read.xlsx('~/Desktop/使用教程和资料/输入模板示例/迁移报告范例.xlsx',sheet=2,startRow = 3)
image_name <- dir('~/Desktop/使用教程和资料/输入图片示例/')
image_path <- paste0('~/Desktop/使用教程和资料/输入图片示例/',image_name)

############################
#                          #
#         migration        #
#                          #
############################ 
migration <- function(raw_data1,raw_data2){
  #表头定位
  raw_data1 <- raw_data1
  raw_data2 <- raw_data2
  data_head <- raw_data1[,1]
  ex_group <- str_which(data_head, '实验分组')
  contract_num <- str_which(data_head, '合同号')
  statistic_p <- str_which(data_head, '统计检验参数')
  paried <- str_which(data_head, '是否配对')
  statistic_test_group <- str_which(data_head, '显著性检验分组')
  fontname <- "Arial"
  
  #实验分组
  assay_group_data <-  raw_data1[(ex_group+2):(statistic_p-1),]
  colnames(assay_group_data) <- raw_data1[(ex_group+1),]
  exp_group_ft <- assay_group_data %>%
    flextable(col_keys=c('相关基因','检测细胞','细胞接种数（cell/well）','检测时间'))%>%
    add_header_lines("表2.2.2  实验参数表")%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 8,part = 'head')%>%
    fontsize(size = 6.5,part = 'body')%>%
    autofit(add_h = 0.2)%>%
    font(fontname = fontname, part = "all")
  
  #实验信息
  detail_info_ft <- assay_group_data %>%
    flextable(col_keys=c('分组名称','上小室培养条件','下小室培养条件','实验描述'))%>%
    add_header_lines("表2.2.1  实验分组信息表")%>%
    align(i=1,align = 'center',part = 'head')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 8,part = 'head')%>%
    fontsize(size = 6.5,part = 'body')%>%
    autofit(add_h = 0.2,add_w=0.3)%>%
    font(fontname = fontname, part = "all")
  
  
  #确定统计参数
  statistic_test_data <- raw_data1[str_which(raw_data1[,4],'vs'),1:5]
  compare_group <- paste(statistic_test_data[,1],statistic_test_data[,5],sep = '/',collapse = ',')
  compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
  
  #去除na和极值
  
  z <- raw_data2%>%
    gather(-c(1),key='sample',value='value')%>%
    group_by(实验分组)%>%
    mutate(value_sort=abs(value-quantile(value,0.5,na.rm = TRUE)))%>%
    arrange(实验分组,value_sort)%>%
    top_n(3,value_sort)
  
  #均值标准差表
  mean_sd_data <- z %>%
    select(c(1,3,4))%>%
    summarise(
      Mean=round(mean(value,na.rm = TRUE),digits = 2),
      SD=round(sd(value,na.rm = TRUE),digits = 2),
      QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 2)
    )
  
  sample_data <- z[,c(1,2,3)]%>%
    spread(sample,value)
  
  
  mean_sd_ft <- 
    inner_join(sample_data,mean_sd_data, by = c('实验分组'))%>%
    arrange(实验分组)%>%
    flextable() %>%
    color(i=~QC>=0.5,j=~QC, color = "red")%>%
    set_header_labels(Mean='平均值',SD='标准差')%>%
    footnote(j=~QC,part = 'header',value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
    merge_v(j=1)%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    add_header_lines("表3.1.1 迁移细胞数平均值和标准差")%>%
    align(align = 'center',part = 'all') %>%
    fontsize(size = 9,part = 'all')%>%
    autofit(add_h = 0.2) %>%
    width(j = 1, width = 1.5)%>%
    height(part = 'foot',height = 1)%>%
    font(fontname = fontname, part = "all")
  
  #显著性
  p_value <- do.call(rbind,lapply(compare_group_list,function(x){
    y <- z %>%
      filter(实验分组==x[1]|实验分组==x[2])
    if(var.test(value~实验分组,data=y)$p.value>=0.05){
      var_value=1
    }else{
      var_value=0}
    round(t.test(value~实验分组,y,var.equal =var_value)$p.value,digits = 3)
  }))
  p_value_table <- data.frame('组1'=statistic_test_data[,1],'组2'=statistic_test_data[,5],p_value )
  sig_symbol <- sapply(p_value_table$p_value,function(x){
    ifelse(x>0.05,'ns',
           ifelse(x>=0.01,'*',
                  ifelse(x>=0.001,'**',
                         ifelse(x>=0.0001,'***','****'))))
  })
  p_value_table$'sig_symbol' <- sig_symbol
  
  pvalue_ft <-  p_value_table%>%
    flextable() %>%
    add_header_lines("表3.2.1  显著性差异检验")%>%
    set_header_labels(sig_symbol='显著性')%>%
    footnote(j=3,part = 'header',
             value = as_paragraph('ns：P_value>0.05\n *：P_value≤0.05\n **：P_value≤0.01\n ***：P_value≤0.001\n ****：P_value≤0.0001\n')) %>%
    align(align = 'center',part = 'all') %>%
    fontsize(size = 9,part = 'all')%>%
    autofit(add_h = 0.2) %>%
    width(j = 1:2, width = 1.8)%>%
    width(j = 3:4, width = 0.8)%>%
    height(i=1,height = 1.7,part = 'foot')%>%
    font(fontname = fontname, part = "all")
  
  
  #统计作图
  p <- ggplot(mean_sd_data, aes(x=实验分组, y=Mean,fill =实验分组))+
    geom_bar(stat = "identity",position="dodge",width = 0.5)+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                  position=position_dodge(0.8),width=.2,color='grey')+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme(plot.title = element_text(face="bold"),
          plot.title.position = "panel",
          panel.background = element_rect(fill = NA),
          axis.title= element_text(size = 12),
          axis.text.x = element_text(size = 10,angle = 45,hjust = 1),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 10),
          legend.position="none",
    )+
    scale_fill_viridis(option = 'E',discrete = T,alpha = 0.7)+
    labs(x='',y='Number of Dots',title = "Migration Assay",
         caption = "Plot by Syngenetech")
  
  #仪器与试剂
  #仪器
  equip_name <- c('Sorvall Legend Mircro 17台式离心机',
                  '微量移液器','生物安全柜',
                  '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0mL离心管)',
                  '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱')
  equip_source <- c('美国ThermoFisher公司','德国Eppendorf公司',
                    '美国ThermoFisher公司','美国ThermoFisher公司',
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
  regent_name <- c('DMEM高糖培养基','胰酶')
  regent_source <- c('美国Gibco公司','美国Sigma公司')
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
  my_doc <- read_docx('./data/migration_templete.docx')
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
    body_add_flextable(exp_group_ft)%>%
    body_add_par("",style = "pic_style")%>%
    body_add_flextable(detail_info_ft)%>%
    cursor_bookmark("mean_sd")%>%
    body_add_flextable(mean_sd_ft)%>%
    cursor_bookmark("pvalue")%>%
    body_add_flextable(pvalue_ft)%>%
    #cursor_bookmark("raw_data")%>%
    #body_add_flextable(raw_data_ft)%>%
    #cursor_bookmark("conclusion")%>%
    #body_add_flextable(conclusion_ft)%>%
    cursor_bookmark("ggplot")%>%
    body_add_gg(p,height = 3.5)
  
  my_doc
}

print(my_doc,'载体构建和病毒包装.docx')



