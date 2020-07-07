trans <-function(raw_data1,raw_data2,image_path,image_name){
  ############################
  #                          #
  #         migration        #
  #                          #
  ############################ 
  migration <- function(raw_data1,raw_data2,image_path,image_name){
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
           caption = "Plot by syngentech")
    
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
  
  ############################
  #                          #
  #           EdU            #
  #                          #
  ############################ 
  EdU <- function(raw_data1,raw_data2,image_path,image_name){
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
      flextable()%>%
      add_header_lines("表2.2.2  分组信息表")%>%
      fix_border_issues()%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      #footnote(i=2,j=1:2,part = 'head',value = as_paragraph(c('调控元件：miRNA、mimics或转录因子','靶点：mRNA 3‘UTR、miRNA靶序列或启动子')))%>%
      fontsize(size = 7.5,part = 'all')%>%
      autofit(add_h = 0.2,add_w = 0.2)%>%
      width(j = 1, width = 1)%>%
      font(fontname = fontname, part = "all")
    
    
    #确定统计参数
    statistic_test_data <- raw_data1[str_which(raw_data1[,2],'vs'),1:3]
    compare_group <- paste(statistic_test_data[,1],statistic_test_data[,3],sep = '/',collapse = ',')
    compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
    
    #去除na和极值
    arranged_data <- raw_data2%>%
      arrange(荧光类型,分组编号,视野)
    
    ma1 <- arranged_data%>%
      filter(荧光类型=="EdU")%>%
      select(-c(1,2,3))
    
    ma2 <- arranged_data%>%
      filter(荧光类型!="EdU")%>%
      select(-c(1,2,3))
    
    z <- arranged_data%>%
      filter(荧光类型=="EdU")%>%
      select(c(2,3))%>%
      cbind(value=round(apply(ma1/ma2, 1,mean),digits = 2))
    
    
    z <- z%>%
      group_by(分组编号)%>%
      mutate(value_sort=abs(value-quantile(value,0.5,na.rm = TRUE)))%>%
      arrange(分组编号,value_sort)%>%
      top_n(3,value_sort)
    
    #均值标准差表
    mean_sd_data <- z %>%
      select(c(1,2,3))%>%
      summarise(
        Mean=round(mean(value,na.rm = TRUE),digits = 2),
        SD=round(sd(value,na.rm = TRUE),digits = 2),
        QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 2)
      )
    
    sample_data <- z[,c(1,2,3)]%>%
      spread(视野,value)
    
    mean_sd_ft <- 
      inner_join(sample_data,mean_sd_data, by = c('分组编号'))%>%
      arrange(分组编号)%>%
      flextable() %>%
      color(i=~QC>=0.5,j=~QC, color = "red")%>%
      set_header_labels(Mean='平均值',SD='标准差',`1`='视野1',`2`='视野2',`3`='视野3')%>%
      footnote(j=~QC,part = 'header',
               value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色。\n增殖细胞比例=EdU荧光强度/hoechst荧光强度'))%>%
      merge_v(j=1)%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      add_header_lines("表3.1.1 增殖细胞比例平均值和标准差")%>%
      align(align = 'center',part = 'all') %>%
      fontsize(size = 9,part = 'all')%>%
      autofit(add_h = 0.2) %>%
      width(j = 1, width = 1.5)%>%
      height(part = 'foot',height = 1)%>%
      font(fontname = fontname, part = "all")
    
    
    #显著性
    p_value <- do.call(rbind,lapply(compare_group_list,function(x){
      y <- z %>%
        filter(分组编号==x[1]|分组编号==x[2])
      if(var.test(value~分组编号,data=y)$p.value>=0.05){
        var_value=1
      }else{
        var_value=0}
      round(t.test(value~分组编号,y,var.equal =var_value)$p.value,digits = 3)
    }))
    p_value_table <- data.frame('组1'=statistic_test_data[,1],'组2'=statistic_test_data[,3],p_value )
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
      width(j = 1, width = 1.8)%>%
      height(i=1,height = 1.7,part = 'foot')%>%
      font(fontname = fontname, part = "all")
    
    mean_sd_data$分组编号 <- factor(mean_sd_data$分组编号,unique(sort(mean_sd_data$分组编号,decreasing = T)))
    
    p <- ggplot(mean_sd_data, aes(x=分组编号, y=Mean,fill =分组编号))+
      geom_bar(stat = "identity",position="dodge",width = 0.5)+
      geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                    position=position_dodge(0.5),width=.2,color='grey')+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      theme(plot.title = element_text(face="bold"),
            plot.title.position = "panel",
            plot.subtitle = element_text(color = "grey"),
            panel.background = element_rect(fill = NA),
            axis.title= element_text(size = 12),
            axis.text.x = element_text(size = 10,angle = 45,hjust = 1),
            axis.line = element_line(colour = "black"),
            axis.text.y = element_text(size = 10),
            legend.position="none"
      )+
      scale_fill_viridis(option = 'E',discrete = T,alpha = 0.7)+
      labs(x='',y='Proliferation Rate \n(Ratio of EdU/hoechst)',
           title = "EdU Cell Proliferation Assay",fill="",
           subtitle = "Ratio is calculated by EdU/hoechst",
           caption = "Plot by syngentech")
    
    #仪器与试剂
    #仪器
    equip_name <- c('Sorvall Legend Mircro 17台式离心机','Sorvall ST 16R冷冻离心机',
                    '微量移液器','生物安全柜','EVOS荧光显微成像系统',
                    '恒温二氧化碳细胞培养箱','实验室耗材I（移液枪头、1.5/2.0 mL离心管）',
                    '实验室耗材II（细胞培养皿、移液管等）','超低温冷冻冰箱',
                    'iMark Microplate Absorbance Reader','血球计数板')
    equip_source <- c('美国ThermoFisher公司','美国ThermoFisher公司','德国Eppendorf公司',
                      '美国ThermoFisher公司','美国ThermoFisher公司','美国ThermoFisher公司',
                      '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','美国Bio-Rad公司','求精')
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
    regent_name <- c('EdU 细胞增殖检测试剂盒','4%多聚甲醛固定液','Saponin通透液',
                     'DMEM高糖培养基','RPMI 1640培养基',
                     '胎牛血清')
    regent_source <- c('上海碧云天生物科技有限公司','上海碧云天生物科技有限公司','上海碧云天生物科技有限公司',
                       '美国Gibco公司','美国Gibco公司','美国Gibco公司')
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
    my_doc <- read_docx('./data/EdU_templete.docx')
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
      #body_add_flextable(detail_info_ft)%>%
      cursor_bookmark("mean_sd")%>%
      body_add_flextable(mean_sd_ft)%>%
      cursor_bookmark("pvalue")%>%
      body_add_flextable(pvalue_ft)%>%
      cursor_bookmark("raw_data")%>%
      #body_add_flextable(raw_data_ft)%>%
      #cursor_bookmark("conclusion")%>%
      #body_add_flextable(conclusion_ft)%>%
      cursor_bookmark("ggplot")%>%
      body_add_gg(p,height = 4)
    
    my_doc
  }
  ############################
  #                          #
  #          coip            #
  #                          #
  ############################ 
  CoIP <- function(raw_data1,raw_data2,image_path,image_name){
    #表头定位
    raw_data1 <- raw_data1
    data_head <- raw_data1[,1]
    contract_num <- str_which(data_head, '合同号')
    exp_index <- str_which(data_head, '抗体信息')
    group_index <- str_which(data_head, '实验分组')
    conclusion_index <- str_which(data_head, '结论')
    fontname <- "Arial"
    
    #抗体信息
    antibody_info <- raw_data1[(exp_index+2):(group_index-1),]
    colnames(antibody_info) <- raw_data1[exp_index+1,]
    antibody_info <- drop_na(antibody_info)
    antibody_ft <- antibody_info%>%
      flextable()%>%
      add_header_lines("表1.1.1 实验抗体信息")%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 7.5,part = 'all')%>%
      autofit(add_h = 0.2,add_w = 0.2)%>%
      font(fontname = fontname, part = "all")
    
    #分组信息
    group_info <- raw_data1[(group_index+2):(conclusion_index-1),]
    colnames(group_info) <- raw_data1[group_index+1,]
    group_ft <- group_info%>%
      flextable()%>%
      add_header_lines("表1.1.1 实验分组信息")%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 7.5,part = 'all')%>%
      autofit(add_h = 0.2,add_w = 0.2)%>%
      font(fontname = fontname, part = "all")
    
    #实验结论
    conclusion_info <- raw_data1[(conclusion_index+1):length(data_head),]
    conclusion_ft <- conclusion_info[,c(1,2)]%>%
      flextable()%>%
      set_header_labels(实验类型="实验分组",X2="结论")%>%
      border(border = fp_border(color="black", width = 1),part = 'all' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 7.5,part = 'all')%>%
      autofit(add_h = 0.2)%>%
      width(j=-1,width = 5)%>%
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
      '胎牛血清','NP-40裂解液','Protein A+G Agarose (Fast Flow, for IP)',
      '蛋白酶抑制剂/磷酸酶抑制剂/EDTA','D-PBS','胰酶')
    regent_source <- c(
      '北京索莱宝科技有限公司','美国Gibco公司','美国Gibco公司',
      '美国Gibco公司','北京索莱宝科技有限公司',"上海碧云天生物科技有限公司",
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
    my_doc <- read_docx('./data/Co-IP_templete.docx')
    my_doc %>%
      cursor_bookmark("contract_num")%>%
      body_add_par(data_head[contract_num+1],style  = 'Subtitle')%>%
      cursor_bookmark("date")%>%
      body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
      cursor_bookmark("equipment_info")%>%
      body_add_flextable(equip_ft)%>%
      cursor_bookmark("regent_info")%>%
      body_add_flextable(regent_ft)%>%
      cursor_bookmark("antibody_info")%>%
      body_add_flextable(antibody_ft)%>%
      cursor_bookmark("exp_group")%>%
      body_add_flextable(group_ft)%>%
      cursor_bookmark("conclusion")%>%
      body_add_flextable(conclusion_ft)
    
    #结果图片插入
    #图片高度控制
    pict_height <- length(antibody_info$`抗体种类`[antibody_info$`抗体种类`=="一抗"])*2
    
    picture_list <- list('coip结果')
    index <- unlist(lapply(picture_list,function(x){
      str_which(image_name,x)
    }))
    image_path <- image_path[index]
    for(pic in image_path){
      cursor_bookmark(my_doc,"wb_res")
      slip_in_img(my_doc,pic,width = 6.5,height = pict_height)
    }
    my_doc
  }
  
  ############################
  #                          #
  #     cell_apoptosis       #
  #                          #
  ############################ 
  cell_apoptosis <- function(raw_data1,raw_data2,image_path,image_name){
    #表头定位
    raw_data1 <- raw_data1
    raw_data2 <- raw_data2
    data_head <- raw_data1[,1]
    ex_group <- str_which(data_head, '实验分组')
    contract_num <- str_which(data_head, '合同号')
    fontname <- "Arial"
    
    #实验分组
    assay_group_data <-  raw_data1[(ex_group+2):(length(data_head)),]
    colnames(assay_group_data) <- raw_data1[(ex_group+1),]
    exp_group_ft <- assay_group_data %>%
      flextable()%>%
      add_header_lines("表2.2.2  实验参数表")%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 7.5,part = 'all')%>%
      autofit(add_h = 0.2,add_w = 0.2)%>%
      font(fontname = fontname, part = "all")
    
    #去除na和极值
    z <- raw_data2%>%
      gather(-c(1,2),key='sample',value='value')%>%
      mutate(实验分组=as.factor(实验分组),流式象限=as.factor(流式象限))%>%
      group_by(实验分组,流式象限)%>%
      mutate(value_sort=abs(value-quantile(value,0.5,na.rm = TRUE)))%>%
      arrange(实验分组,value_sort)%>%
      top_n(3,value_sort)
    
    #均值标准差表
    mean_sd_data <- z %>%
      select(c(1,2,4))%>%
      summarise(
        Mean=round(mean(value,na.rm = TRUE),digits = 2),
        SD=round(sd(value,na.rm = TRUE),digits = 2),
        QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 2)
      )
    
    sample_data <- z[,c(1,2,3,4)]%>%
      spread(sample,value)
    
    mean_sd_ft <- 
      inner_join(sample_data,mean_sd_data, by = c('实验分组','流式象限'))[,c(2,1,3,4,5,6,7,8)]%>%
      arrange(流式象限,实验分组)%>%
      flextable() %>%
      color(i=~QC>=0.5,j=~QC, color = "red")%>%
      set_header_labels(Mean='平均值',SD='标准差')%>%
      footnote(j=~QC,part = 'header',
               value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色。\nLL:活细胞；LR:早期凋亡细胞；UR:晚期凋亡细胞。'))%>%
      merge_v(j=1)%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      add_header_lines("表3.1.1 不同周期细胞比例平均值和标准差")%>%
      align(align = 'center',part = 'all') %>%
      fontsize(size = 9,part = 'all')%>%
      autofit(add_h = 0.2) %>%
      width(j = 1, width = 1.5)%>%
      height(part = 'foot',height = 1)%>%
      font(fontname = fontname, part = "all")
    
    plot_data <- mean_sd_data%>%
      arrange(实验分组,desc(流式象限))%>%
      mutate(texlabel=cumsum(Mean))
    #统计作图
    p <- ggplot(plot_data,aes(x=实验分组,y=Mean,fill=流式象限))+
      geom_bar(stat = "identity",width = 0.6)+
      geom_text(aes(y=texlabel, label=Mean), vjust=1, 
                color="black", size=2)+
      #geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
      # width=.3,color='grey')+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      theme(plot.title = element_text(face="bold"),
            plot.subtitle = element_text(color = "grey"),
            plot.title.position = "panel",
            panel.background = element_rect(fill = NA),
            axis.title= element_text(size = 12),
            axis.text.x = element_text(size = 12,angle = 45,hjust = 1),
            axis.line = element_line(colour = "black"),
            axis.text.y = element_text(size = 12),
            legend.position="right",
      )+
      scale_fill_viridis(option = 'E',discrete = T,alpha = 0.9)+
      labs(x='',y='Cell Ratio',title = "Cell Apoptosis Assay",
           #subtitle = "Apoptosis Ratio = early apoptosis + late apoptosis",
           caption = "Plot by Syngentech",fill='Class'
      )
    
    
    #仪器与试剂
    #仪器
    equip_name <- c('Sorvall Legend Mircro 17台式离心机',
                    '微量移液器','生物安全柜',
                    '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0mL离心管)',
                    '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱',
                    '流式细胞仪','酶标仪')
    equip_source <- c('美国ThermoFisher公司','德国Eppendorf公司',
                      '美国ThermoFisher公司','美国ThermoFisher公司',
                      '美国Axygen公司','美国Corning公司','美国ThermoFisher公司',
                      '美国BACKMAN公司','瑞士Tecan infinite公司')
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
    regent_name <- c('DMEM高糖培养基','胰酶','凋亡试剂盒')
    regent_source <- c('美国Gibco公司','美国Sigma公司','美国eBioscience公司')
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
    #读入模板
    my_doc <- read_docx('./data/apoptosis_templete.docx')
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
      cursor_bookmark("mean_sd")%>%
      body_add_flextable(mean_sd_ft)%>%
      #cursor_bookmark("raw_data")%>%
      #body_add_flextable(raw_data_ft)%>%
      #cursor_bookmark("conclusion")%>%
      #body_add_flextable(conclusion_ft)%>%
      cursor_bookmark("ggplot")%>%
      body_add_gg(p,height = 3.5)
    
    my_doc
  }
  
  
  ############################
  #                          #
  #        cell_cycle        #
  #                          #
  ############################ 
  cell_cycle <- function(raw_data1,raw_data2,image_path,image_name){
    #表头定位
    raw_data1 <- raw_data1
    raw_data2 <- raw_data2
    data_head <- raw_data1[,1]
    ex_group <- str_which(data_head, '实验分组')
    contract_num <- str_which(data_head, '合同号')
    statistic_test_group <- str_which(data_head, '显著性检验分组')
    fontname <- "Arial"
    
    #实验分组
    assay_group_data <-  raw_data1[(ex_group+3):statistic_test_group-1,]
    colnames(assay_group_data) <- raw_data1[(ex_group+1),]
    exp_group_ft <- assay_group_data %>%
      flextable()%>%
      add_header_lines("表2.2.2  实验参数表")%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 7.5,part = 'all')%>%
      autofit(add_h = 0.2,add_w = 0.2)%>%
      font(fontname = fontname, part = "all")
    
    #确定统计参数
    statistic_test_data <- raw_data1[str_which(raw_data1[,2],'vs'),1:3]
    compare_group <- paste(statistic_test_data[,1],statistic_test_data[,3],sep = '/',collapse = ',')
    compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
    
    
    #去除na和极值
    z <- raw_data2%>%
      gather(-c(1,2),key='sample',value='value')%>%
      group_by(实验分组,细胞周期)%>%
      mutate(value_sort=abs(value-quantile(value,0.5,na.rm = TRUE)))%>%
      arrange(实验分组,value_sort)%>%
      top_n(3,value_sort)
    
    #均值标准差表
    mean_sd_data <- z %>%
      select(c(1,2,4))%>%
      summarise(
        Mean=round(mean(value,na.rm = TRUE),digits = 2),
        SD=round(sd(value,na.rm = TRUE),digits = 2),
        QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 2)
      )
    
    sample_data <- z[,c(1,2,3,4)]%>%
      spread(sample,value)
    
    mean_sd_ft <- 
      inner_join(sample_data,mean_sd_data, by = c('实验分组','细胞周期'))[,c(2,1,3,4,5,6,7,8)]%>%
      arrange(细胞周期,实验分组)%>%
      flextable() %>%
      color(i=~QC>=0.5,j=~QC, color = "red")%>%
      set_header_labels(Mean='平均值',SD='标准差')%>%
      footnote(j=~QC,part = 'header',
               value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
      merge_v(j=1)%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      add_header_lines("表3.1.1 不同周期细胞比例平均值和标准差")%>%
      align(align = 'center',part = 'all') %>%
      fontsize(size = 9,part = 'all')%>%
      autofit(add_h = 0.2) %>%
      width(j = 1, width = 1.5)%>%
      height(part = 'foot',height = 1)%>%
      font(fontname = fontname, part = "all")
    
    #显著性
    p_value <- as.data.frame(do.call(cbind,lapply(compare_group_list,function(x){
      y <- z %>%
        filter(实验分组==x[1]|实验分组==x[2])%>%
        group_by(细胞周期)
      p_value <- unlist(lapply(split(y,y$细胞周期),function(x){
        if(var.test(value~实验分组,x)$p.value>=0.05){
          var_value=1
        }else{
          var_value=0}
        round(t.test(value~实验分组,x,var.equal =var_value)$p.value,digits = 3)
      }))
    })))
    colnames(p_value) <- unlist(lapply(compare_group_list,function(x){
      paste(x[1],'vs',x[2])
    }))
    p_value_table <-cbind('细胞周期'=rownames(p_value),p_value)  
    
    
    p_value_ft <-  p_value_table%>%
      flextable() %>%
      add_header_lines("表3.2.1  显著性差异检验")%>%
      footnote(j=-1,part = 'header',
               value = as_paragraph('ns：P_value>0.05\n *：P_value≤0.05\n **：P_value≤0.01\n ***：P_value≤0.001\n ****：P_value≤0.0001\n')) %>%
      align(align = 'center',part = 'all') %>%
      fontsize(size = 9,part = 'all')%>%
      autofit(add_h = 0.2) %>%
      width(j = 1, width = 1.8)%>%
      height(i=1,height = 1.7,part = 'foot')%>%
      font(fontname = fontname, part = "all")
    
    #统计作图
    p <- ggplot(mean_sd_data, aes(x=实验分组, y=Mean,fill =细胞周期))+
      geom_bar(stat = "identity",position="dodge",width = 0.8)+
      geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                    position=position_dodge(0.8),width=.3,color='grey')+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      theme(plot.title = element_text(face="bold"),
            plot.title.position = "panel",
            panel.background = element_rect(fill = NA),
            axis.title= element_text(size = 12),
            axis.text.x = element_text(size = 12,angle = 45,hjust = 1),
            axis.line = element_line(colour = "black"),
            axis.text.y = element_text(size = 12),
            legend.position="right",
      )+
      scale_fill_viridis(option = 'E',discrete = T,alpha = 0.7)+
      labs(x='',y='Cell Ratio',fill='Cell Cycle',title = "Cell Cycle Assay",
           caption = "Plot by Syngentech")
    
    #仪器与试剂
    #仪器
    equip_name <- c('Sorvall Legend Mircro 17台式离心机',
                    '微量移液器','生物安全柜',
                    '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0mL离心管)',
                    '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱',
                    '流式细胞仪')
    equip_source <- c('美国ThermoFisher公司','德国Eppendorf公司',
                      '美国ThermoFisher公司','美国ThermoFisher公司',
                      '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','美国BACKMAN公司')
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
    regent_name <- c('DMEM高糖培养基','胰酶','RNase A','PI')
    regent_source <- c('美国Gibco公司','美国Sigma公司','英国NEB公司','美国Sigma公司')
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
    my_doc <- read_docx('./data/cell_cycle_templete.docx')
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
      cursor_bookmark("mean_sd")%>%
      body_add_flextable(mean_sd_ft)%>%
      cursor_bookmark("raw_data")%>%
      body_add_flextable(p_value_ft)%>%
      #cursor_bookmark("conclusion")%>%
      #body_add_flextable(conclusion_ft)%>%
      cursor_bookmark("ggplot")%>%
      body_add_gg(p,height = 3.5)
    
    my_doc
  }
  ############################
  #                          #
  #        transwell         #
  #                          #
  ############################ 
  transwell <- function(raw_data1,raw_data2){
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
      autofit(add_h = 0.2,add_w=0.2)%>%
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
      add_header_lines("表3.1.1 侵袭细胞数平均值和标准差")%>%
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
      labs(x='',y='Number of Dots',title = "Transwell Assay",
           caption = "Plot by syngentech")
    
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
    regent_name <- c('DMEM高糖培养基','胰酶','Matrigel')
    regent_source <- c('美国Gibco公司','美国Sigma公司','美国Corning公司')
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
    my_doc <- read_docx('./data/transwell_templete.docx')
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

  ############################
  #                          #
  #     wound_healing        #
  #                          #
  ############################ 
  wound_healing <- function(raw_data1,raw_data2,image_path,image_name){
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
      flextable(col_keys=c('相关基因','检测细胞','细胞接种密度（cell/well）','血清浓度','检测时间'))%>%
      add_header_lines("表2.2.2  实验参数表")%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 8,part = 'head')%>%
      fontsize(size = 6.5,part = 'body')%>%
      autofit(add_h = 0.2)%>%
      font(fontname = fontname, part = "all")
    
    #实验信息
    detail_info_ft <- assay_group_data %>%
      flextable(col_keys=c('分组名称','实验描述'))%>%
      add_header_lines("表2.2.1  实验分组信息表")%>%
      align(i=1,align = 'center',part = 'head')%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 8,part = 'head')%>%
      fontsize(size = 6.5,part = 'body')%>%
      autofit(add_h = 0.2,add_w=0.5)%>%
      font(fontname = fontname, part = "all")
    
    
    #确定统计参数
    statistic_test_data <- raw_data1[str_which(raw_data1[,4],'vs'),1:5]
    compare_group <- paste(statistic_test_data[,1],statistic_test_data[,5],sep = '/',collapse = ',')
    compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
    
    
    #去除na和极值
    
    z <- raw_data2%>%
      gather(-c(1,2),key='sample',value='value')%>%
      spread(检测时间,value)
    colnames(z)[c(3,4)] <- c("pre","aft")  
    z <- z %>%
      mutate(value=round((pre-aft)/pre*100,digit=1))%>%
      group_by(实验分组)%>%
      mutate(value_sort=abs(value-quantile(value,0.5,na.rm = TRUE)))%>%
      arrange(实验分组,value_sort)%>%
      top_n(3,value_sort)
    
    
    #均值标准差表
    mean_sd_data <- z %>%
      select(c(1,2,5))%>%
      summarise(
        Mean=round(mean(value,na.rm = TRUE),digits = 2),
        SD=round(sd(value,na.rm = TRUE),digits = 2),
        QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 2)
      )
    
    sample_data <- z%>%
      select(c(1,2,5))%>%
      spread(key=sample,value=value)
    
    mean_sd_ft <- 
      inner_join(sample_data,mean_sd_data, by = '实验分组')%>%
      flextable() %>%
      color(i=~QC>=0.5,j=~QC, color = "red")%>%
      set_header_labels(Mean='平均值',SD='标准差')%>%
      footnote(j=~QC,part = 'header',value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
      merge_v(j=1)%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      add_header_lines(paste0("表3.1.2 迁移率(%)平均值和标准差"))%>%
      align(align = 'center',part = 'all') %>%
      fontsize(size = 9,part = 'all')%>%
      autofit(add_h = 0.2) %>%
      width(j=1,width = 2)%>%
      height(part = 'foot',height = 1)%>%
      font(fontname = fontname, part = "all")
    
    raw_data_ft <- z%>%
      select(c(1,2,3,4,5))%>%
      arrange(实验分组,sample)%>%
      flextable() %>%
      set_header_labels(value='迁移率%',sample="样本",pre="迁移前",aft="迁移后")%>%
      footnote(j=~value,part = 'header',value = as_paragraph('迁移率(%)=(迁移开始前划痕面积-迁移检测时划痕面积)/迁移开始前划痕面积*100'))%>%
      merge_v(j=1)%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      add_header_lines("表3.1.1 迁移面积原始数据")%>%
      align(align = 'center',part = 'all') %>%
      fontsize(size = 9,part = 'all')%>%
      autofit(add_h = 0.2) %>%
      width(j=1,width = 2)%>%
      height(part = 'foot',height = 1)%>%
      font(fontname = fontname, part = "all")
    
    #统计作图
    p <-  ggplot(mean_sd_data, aes(x=实验分组, y=Mean,fill =实验分组))+
      geom_bar(stat = "identity",position="dodge",width = 0.5)+
      geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                    position=position_dodge(0.8),width=.2,color='grey')+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      theme(plot.title = element_text(face="bold"),
            plot.title.position = "panel",
            plot.subtitle = element_text(color = "grey"),
            panel.background = element_rect(fill = NA),
            axis.title= element_text(size = 12),
            axis.text.x = element_text(size = 10,angle = 45,hjust = 1),
            axis.line = element_line(colour = "black"),
            axis.text.y = element_text(size = 10),
            legend.position="none",
      )+
      scale_fill_viridis(option = 'E',discrete = T,alpha = 0.7)+
      labs(x='',y='Wound Healing Rate(%)',title = "Wound Healing Assay",subtitle = "Wound Healing Rate(%)= Cellular Migration Area/Prcellular Migration Area*100",
           caption = "Plot by syngentech") 
    
    
    
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
    
    #仪器与试剂
    #仪器
    equip_name <- c('Sorvall Legend Mircro 17台式离心机',
                    '微量移液器','生物安全柜',
                    '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0mL离心管)',
                    '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱',
                    '血球计数板')
    equip_source <- c('美国ThermoFisher公司','德国Eppendorf公司',
                      '美国ThermoFisher公司','美国ThermoFisher公司',
                      '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','求精')
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
    my_doc <- read_docx('./data/wound_healing_templete.docx')
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
      cursor_bookmark("raw_data")%>%
      body_add_flextable(raw_data_ft)%>%
      #cursor_bookmark("conclusion")%>%
      #body_add_flextable(conclusion_ft)%>%
      cursor_bookmark("ggplot")%>%
      body_add_gg(p,height = 3.5)
    
    my_doc
  }
  
############################
#                          #
#     colony_formation     #
#                          #
############################ 
  colony_formation <- function(raw_data1,raw_data2){
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
      flextable(col_keys=c('相关基因','检测细胞','细胞接种数（cell/well）','培养体系','检测时间'))%>%
      add_header_lines("表2.2.2  实验参数表")%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 8,part = 'head')%>%
      fontsize(size = 6.5,part = 'body')%>%
      autofit(add_h = 0.2)%>%
      font(fontname = fontname, part = "all")
    
    #实验信息
    detail_info_ft <- assay_group_data %>%
      flextable(col_keys=c('分组名称','实验描述'))%>%
      add_header_lines("表2.2.1  实验分组信息表")%>%
      align(i=1,align = 'center',part = 'head')%>%
      align(align = 'center',part = 'all')%>%
      fontsize(size = 8,part = 'head')%>%
      fontsize(size = 6.5,part = 'body')%>%
      autofit(add_h = 0.2,add_w=0.5)%>%
      font(fontname = fontname, part = "all")
    
    
    #确定统计参数
    statistic_test_data <- raw_data1[str_which(raw_data1[,4],'vs'),1:5]
    compare_group <- paste(statistic_test_data[,1],statistic_test_data[,5],sep = '/',collapse = ',')
    compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
    
    #去除na和极值
    omit_extream <- function(x){
      y <- x[order(abs(x-quantile(x,0.5,na.rm = TRUE)))]
      y <- y[1:3]
    } 
    
    nor_matrix <- raw_data2[,str_which(colnames(raw_data2),'复孔')]
    z<- raw_data2%>%
      select(1)%>%
      cbind(t(apply(nor_matrix,1,omit_extream)))%>%
      gather(-1,key='sample',value='value')
    
    #均值标准差表
    mean_sd_data <- z %>% 
      group_by(实验分组)%>%
      summarise(
        Mean=mean(value,na.rm = TRUE),
        SD=round(sd(value,na.rm = TRUE),digits = 3),
        QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 3)
      )
    
    sample_data <- z%>%
      spread(sample,value)
    
    mean_sd_ft <- 
      inner_join(sample_data,mean_sd_data, by = '实验分组')%>%
      flextable() %>%
      color(i=~QC>=0.5,j=~QC, color = "red")%>%
      set_header_labels(分组名称='实验分组',Mean='平均值',SD='标准差',`1`='复孔1',`2`='复孔2',`3`='复孔3')%>%
      footnote(j=~QC,part = 'header',value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
      merge_v(j=1)%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      add_header_lines("表3.1.1 克隆形成数平均值和标准差")%>%
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
    p <-  ggplot(mean_sd_data, aes(x=实验分组, y=Mean,fill =实验分组))+
      geom_bar(stat = "identity",position="dodge",width = 0.5)+
      geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                    position=position_dodge(0.8),width=.2,color='grey')+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      theme(plot.title = element_text(face="bold"),
            plot.title.position = "panel",
            plot.subtitle = element_text(color = "grey"),
            panel.background = element_rect(fill = NA),
            axis.title= element_text(size = 12),
            axis.text.x = element_text(size = 10,angle = 45,hjust = 1),
            axis.line = element_line(colour = "black"),
            axis.text.y = element_text(size = 10),
            legend.position="none",
      )+
      scale_fill_viridis(option = 'E',discrete = T,alpha = 0.7)+
      labs(x='',y='Number of Clones',title = "Colony Formation",
           caption = "Plot by syngentech") 
    
    #仪器与试剂
    #仪器
    equip_name <- c('Sorvall Legend Mircro 17台式离心机',
                    '微量移液器','生物安全柜',
                    '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0mL离心管)',
                    '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱',
                    '血球计数板')
    equip_source <- c('美国ThermoFisher公司','德国Eppendorf公司',
                      '美国ThermoFisher公司','美国ThermoFisher公司',
                      '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','求精')
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
    regent_name <- c('DMEM高糖培养基','胰酶','多聚甲醛','结晶紫')
    regent_source <- c('美国Gibco公司','美国Sigma公司','上海碧云天生物科技有限公司','上海碧云天生物科技有限公司')
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
    my_doc <- read_docx('./data/colony_formation_templete.docx')
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
      cursor_bookmark("raw_data")%>%
      #body_add_flextable(raw_data_ft)%>%
      #cursor_bookmark("conclusion")%>%
      #body_add_flextable(conclusion_ft)%>%
      cursor_bookmark("ggplot")%>%
      body_add_gg(p,height = 3.5)
    
    my_doc
  }
  

  ############################
  #                          #
  #     vector_vrius         #
  #                          #
  ############################
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
    pre_read_ft <- raw_data2 %>%
      flextable()%>%
      add_header_lines("产品信息速览表")%>%
      bold(part = 'header')%>%
      border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
      align(align = 'center',part = 'all')%>%
      fontsize(i=1,size = 20,part = 'header')%>%
      fontsize(i=2,size = 12,part = 'header')%>%
      fontsize(j=-2,size = 10,part = 'body')%>%
      fontsize(j=c(2,3),size = 7.5,part = 'body')%>%
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
        cursor_reach(keyword = "靶序列")
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
        cursor_reach(keyword = "靶序列")%>%
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
        cursor_reach(keyword = "靶序列")
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
      merge_v(j=1)%>%
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
      body_add_par('1、6 孔板铺板细胞数 6×10^5，按 MOI=15 每孔加 100μl 病毒液，第二天换液继续培养，培养第三天加 puromycin 进行抗性筛选，待对照野生型细胞全部筛死时停止药筛，即可获得稳定沉默/过表达目的基因的细胞株以及对照空载体细胞株。',style='Normal')%>%
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
  
#############################
###          qRTPCR      ####
#############################
qRTPCR <- function(raw_data1,raw_data2,image_path,image_name){
  #表头定位
  raw_data1 <- raw_data1
  raw_data2 <- raw_data2
  data_head <- raw_data1[,1]
  detail_info <- str_which(data_head, '引物信息')
  ex_group <- str_which(data_head, '实验分组')
  contract_num <- str_which(data_head, '合同号')
  statistic_p <- str_which(data_head, '统计检验参数')
  paried <- str_which(data_head, '是否配对')
  statistic_test_group <- str_which(data_head, '显著性检验分组')
  fontname <- "Arial"
  nor_group <- str_which(data_head, '归一化组')
  
  #实验分组
  assay_group_data <-  raw_data1[(ex_group+2):(statistic_p-1),]
  colnames(assay_group_data) <- raw_data1[(ex_group+1),]
  assay_group_ft <- assay_group_data %>%
    flextable(col_keys=c('相关基因','实验分组名称','检测细胞','实验描述'))%>%
    add_header_lines("表2.2.2  分组信息表")%>%
    merge_v(j=1)%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w=0.5)%>%
    font(fontname = fontname, part = "all")
  
  #实验信息
  detail_info_data <-  raw_data1[(detail_info+2):(ex_group-1),]
  colnames(detail_info_data) <- raw_data1[(detail_info+1),]
  detail_info_ft <- detail_info_data %>%
    flextable()%>%
    add_header_lines("表2.2.1  引物信息表")%>%
    align(i=1,align = 'center',part = 'head')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w=0.2)%>%
    font(fontname = fontname, part = "all")
  
  
  #确定统计参数
  statistic_test_data <- raw_data1[str_which(raw_data1[,2],'vs'),1:3]
  compare_group <- paste(statistic_test_data[,1],statistic_test_data[,3],sep = '/',collapse = ',')
  compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
  
  #去除na和极值
  omit_extream <- function(x){
    y <- x[order(abs(x-quantile(x,0.5,na.rm = TRUE)))]
    y <- y[1:3]
  } 
  
  nor_matrix <- raw_data2[,str_which(colnames(raw_data2),'复孔')]-raw_data2[,str_which(colnames(raw_data2),'内参')]
  target_data<- raw_data2%>%
    select(1:3)%>%
    cbind(t(apply(nor_matrix,1,omit_extream)))%>%
    gather(-(1:3),key='sample',value='value1')
  
  nor_data <- target_data%>%
    group_by(相关基因)%>%
    filter(归一化组=='是')%>%
    summarise(mean=mean(value1))
  
  z <- target_data%>%
    group_by(相关基因)%>%
    mutate(value=round(2^(as.numeric(nor_data[nor_data$相关基因==unique(相关基因),2])-value1),digits = 3))%>%
    arrange(相关基因,分组名称)%>%
    select(相关基因,分组名称,sample,value)%>%
    ungroup()
  
  #均值标准差表
  mean_sd_data <- z %>% 
    group_by(相关基因,分组名称)%>%
    summarise(
      Mean=round(mean(value,na.rm = TRUE),digits = 3),
      SD=round(sd(value,na.rm = TRUE),digits = 3),
      QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 3)
    )
  
  sample_data <- z%>%
    group_by(相关基因,分组名称)%>%
    spread(sample,value)
  
  mean_sd_ft <- 
    inner_join(sample_data,mean_sd_data, by = c('分组名称',"相关基因"))%>%
    flextable() %>%
    color(i=~QC>=0.5,j=~QC, color = "red")%>%
    set_header_labels(相关基因.x='相关基因',分组名称='实验分组',Mean='平均值',SD='标准差',`1`='复孔1',`2`='复孔2',`3`='复孔3')%>%
    footnote(j=~QC,part = 'header',value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
    merge_v(j=1)%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    add_header_lines("表3.1.1 相对mRNA表达量平均值和标准差")%>%
    compose(part = 'header',i=1,value = as_paragraph('表3.1.1 相对mRNA表达量平均值和标准差(2',as_sup('-∆∆Ct'),')'))%>%
    align(align = 'center',part = 'all') %>%
    fontsize(size = 9,part = 'all')%>%
    autofit(add_h = 0.2) %>%
    width(j = 1, width = 1.5)%>%
    height(part = 'foot',height = 1)%>%
    font(fontname = fontname, part = "all")
  
  
  #显著性
  p_value <- do.call(rbind,lapply(compare_group_list,function(x){
    y <- z %>%
      filter(分组名称==x[1]|分组名称==x[2])
    if(var.test(value~分组名称,data=y)$p.value>=0.05){
      var_value=1
    }else{
      var_value=0}
    round(t.test(value~分组名称,y,var.equal =var_value)$p.value,digits = 3)
  }))
  p_value_table <- data.frame('组1'=statistic_test_data[,1],'组2'=statistic_test_data[,3],p_value )
  sig_symbol <- sapply(p_value_table$p_value,function(x){
    ifelse(x>0.05,'ns',
           ifelse(x>=0.01,'*',
                  ifelse(x>=0.001,'**',
                         ifelse(x>=0.0001,'***','****'))))
  })
  p_value_table$'sig_symbol' <- sig_symbol
  
  p_value_ft <-  p_value_table%>%
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
  p <- ggplot(mean_sd_data, aes(x=分组名称, y=Mean,fill =相关基因))+
    geom_bar(stat = "identity",position="dodge",width = 0.5)+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                  position=position_dodge(0.8),width=.2,color='grey')+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme(plot.title = element_text(face="bold"),
          plot.title.position = "panel",
          plot.subtitle = element_text(color = "grey"),
          panel.background = element_rect(fill = NA),
          axis.title= element_text(size = 12),
          axis.text.x = element_text(size = 10,angle = 45,hjust = 1),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 10),
          legend.position="right"
    )+
    scale_fill_viridis(option = 'E',discrete = T,alpha = 0.7)+
    labs(x='',y='Ralative mRNA Level',title = "qRT-PCR",subtitle = "Control gruop has been set to 1.",
         caption = "Plot by syngentech",fill="Gene")
  
  #仪器与试剂
  #仪器
  equip_name <- c('Sorvall Legend Mircro 17台式离心机','Sorvall ST 16R冷冻离心机',
                  '微量移液器','生物安全柜','EVOS荧光显微成像系统',
                  '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0mL离心管)',
                  '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱',
                  'iMark Microplate Absorbance Reader')
  equip_source <- c('美国ThermoFisher公司','美国ThermoFisher公司','德国Eppendorf公司',
                    '美国ThermoFisher公司','美国ThermoFisher公司','美国ThermoFisher公司',
                    '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','美国Bio-Rad公司')
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
  regent_name <- c('RNAiso Plus','限制性内切酶类',
                   'Top Green qPCR SuperMix','DMEM高糖培养基','RPMI 1640培养基',
                   '胎牛血清')
  regent_source <- c('宝生物工程(大连)有限公司','美国NEB公司/美国ThermoFisher公司',
                     '北京全式金生物技术有限公司','美国Gibco公司','美国Gibco公司','美国Gibco公司')
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
    body_add_par(value = '项目名称：qRT-PCR基因转录水平检测',style  = 'Subtitle')%>%
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
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "实验方法和分组", style = "heading 1") %>%
    body_add_par("实验方法",style='heading 2')  %>%
    body_add_par('实验原理：本实验采取荧光染料法进行PCR定量。该方法采用与双链DNA特异结合的荧光染料，常用的染料是SYBR green。 该染料只特异的与双链DNA结合，并且游离状态不发射荧光，只有在结合DNA双链后才发射出荧光信号。随PCR循环数增加，产物量增多，产生的荧光信号逐渐增强，实现了荧光信号与扩增产物数量级的关联。',style='Normal')%>%
    body_add_par('',style = 'Normal')%>%
    body_add_par('具体步骤：',style = 'Normal')%>%
    body_add_par('1、异硫氰酸胍/苯酚法-氯仿抽提样本RNA，并测定产物A260/A280值，估计RNA浓度。',style='Normal')%>%
    body_add_par('2、取出少量产物进行琼脂糖凝胶电泳鉴定RNA质量。质量合格的产物将用于cDNA反转录。',style='Normal')%>%
    body_add_par('3、提取的RNA产物65℃变性5min，DNase反应去除产物残留基因组DNA后，加入反转录试剂反转录cDNA。',style='Normal')%>%
    body_add_par('4、Primerbank网站（http://pga.mgh.harvard.edu/primerbank/）或primer 5.0软件设计目的基因上下游PCR引物。
                   ',style='Normal')%>%
    body_add_par('5、引物和反转录cDNA产物加入qRT-PCR反应体系，上机检测。',style='Normal')%>%
    body_add_par('6、数据统计分析，计算目的基因mRNA相对表达量。',style='Normal')%>%
    body_add_break(pos = "after")%>%
    
    
    body_add_par("实验分组",style='heading 2')  %>%
    body_add_flextable(value = detail_info_ft) %>%
    body_add_par("",style='Normal')%>%
    body_add_flextable(value = assay_group_ft) %>%
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "结果与讨论", style = "heading 1") %>% 
    body_add_par("数据平均值和标准差", style = "heading 2")%>% 
    body_add_flextable(value = mean_sd_ft)%>%
    body_add_break(pos = "after")%>%
    body_add_par("显著性差异(t-test)", style = "heading 2")%>% 
    body_add_flextable(value = p_value_ft)%>% 
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "图表绘制", style = "heading 2") %>% 
    body_add_gg(value = p ,style = "heading 3",height=6)%>%
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "实验结论", style = "heading 2")

  my_doc
}
  
  
  
  
  ###########################
  #####         cck8     ####
  ###########################
cck8 <- function(raw_data1,raw_data2){
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
  
  #确定数据区域
  assay_group_data <-  raw_data1[(ex_group+2):(statistic_p-1),]
  colnames(assay_group_data) <- raw_data1[(ex_group+1),]
  test_group <- assay_group_data$分组名称
  test_time <- assay_group_data$`检测时间点（h）`[1]
  test_0_point <- paste(unique(assay_group_data$检测0点),collapse = '/')
  assay_group_ft <- assay_group_data[,1:5] %>%
    flextable()%>%
    align(align = 'left',part = 'all')%>%
    add_header_lines("表2.2.1  CCK-8细胞增殖及毒性检测实验分组")%>%
    footnote(i=2,j=5,part = 'header',value = as_paragraph(paste('检测时间为加药后',test_0_point,'，单位：h')))%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2)%>%
    font(fontname = fontname, part = "all")
  
  #确定统计参数
  statistic_test_data <- raw_data1[str_which(raw_data1[,2],'vs'),1:3]
  compare_group <- paste(statistic_test_data[,1],statistic_test_data[,3],sep = '/',collapse = ',')
  compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
  
  #确定原始数据
  omit_extream <- function(x){
    y <- x[order(abs(x-quantile(x,0.5,na.rm = TRUE)))]
    y <- y[1:3]
  } 
  
  colnames(raw_data2) <- str_replace_all(colnames(raw_data2),'[.]',' ')
  target_data <- raw_data2%>%
    gather(colnames(raw_data2)[-(1:2)],key = 'group',value = 'value')%>%
    drop_na()%>%
    spread(sample,value)%>%
    select(-time,-group)%>%
    apply(1,omit_extream)
  
  
  data_body <- as.data.frame(t(target_data))%>%
    mutate(V1=round(V1,digits = 3),
           V2=round(V2,digits = 3),
           V3=round(V3,digits = 3))
  colnames(data_body) <- c('sample1','sample2','sample3')
  data_label <- raw_data2%>%
    gather(colnames(raw_data2)[-(1:2)],key = 'group',value = 'value')%>%
    drop_na()%>%
    filter(sample<4)%>%
    spread(sample,value)%>%
    select(time,group)
  
  clean_data <- cbind(data_label,data_body )
  
  z <- clean_data%>%
    gather('sample1','sample2','sample3',key = 'sample',value = 'value')
  
  mean_sd_data <- 
    z%>%
    group_by(time,group)%>%
    summarise(
      Mean=round(mean(value,na.rm = TRUE),digits = 3),
      SD=round(sd(value,na.rm = TRUE),digits = 3),
      QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 3)
    )%>%
    left_join(clean_data,by = c("group",'time'))%>%
    arrange(group)
  
  #原始数据和常规统计量
  mean_sd_ft <- mean_sd_data%>%
    flextable(col_keys = c('group','time','sample1','sample2','sample3','Mean','SD','QC')) %>%
    merge_v(j=1)%>%
    fix_border_issues()%>%
    color(i=~QC>=0.5,j=~QC, color = "red")%>%
    set_header_labels(group='实验分组',time='检测时间点',Mean='平均值',SD='标准差',sample1='复孔1',sample2='复孔2',sample3='复孔3')%>%
    footnote(j=~QC,part = 'header',value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    add_header_lines("表3.1.1  CCK-8细胞增殖及毒性检测原始数据（OD450）")%>%
    align(align = 'center',part = 'all') %>%
    fontsize(size = 9,part = 'all')%>%
    merge_v(j=~group,part = "body")%>%
    autofit(add_h = 0.2,add_w = 0.3) %>%
    width(j = 1, width = 1.5)%>%
    font(fontname = fontname, part = "all")
  
  #数据归一化
  nor_data <-  do.call(rbind,lapply(split(z,z$group),function(x){
    y <- x$'value'/mean(x$'value'[as.character(x$'time')==test_0_point[1]])
    cbind(x,y)
  }))
  
  #显著性
  p_value <- as.data.frame(do.call(cbind,lapply(compare_group_list,function(x){
    y <- z %>%
      filter(group==x[1]|group==x[2])%>%
      group_by(time)
    p_value <- unlist(lapply(split(y,y$time),function(x){
      if(var.test(value~group,x)$p.value>=0.05){
        var_value=1
      }else{
        var_value=0}
      round(t.test(value~group,x,var.equal =var_value)$p.value,digits = 3)
    }))
  })))
  colnames(p_value) <- unlist(lapply(compare_group_list,function(x){
    paste(x[1],'vs',x[2])
  }))
  p_value_table <-as.data.frame(t(p_value))  
  p_value_table$'ttest_group' <- rownames(p_value_table)
  
  p_value_ft <-  p_value_table%>%
    select(length(p_value_table),1:(length(p_value_table)-1))%>%
    flextable() %>%
    add_header_lines("表3.2.1  CCK-8细胞增殖及毒性检测数据显著性差异( P_value )")%>%
    footnote(i=1,part = 'header',
             value = as_paragraph('ns：P_value>0.05\n *：P_value≤0.05\n **：P_value≤0.01\n ***：P_value≤0.001\n ****：P_value≤0.0001\n')) %>%
    align(align = 'center',part = 'all') %>%
    fontsize(size = 9,part = 'all')%>%
    autofit(add_h = 0.2) %>%
    width(j = 1, width = 3)%>%
    height(i=1,height = 1.7,part = 'foot')%>%
    font(fontname = fontname, part = "all")
  
  #统计作图
  p <- ggline(z, x="time", y="value", add = "mean_sd", color = "group",palette = 'simpsons')+
    theme(plot.title = element_text(lineheight=.8, size=30, face="bold",hjust = 0.5),
          axis.title= element_text(size = 12),
          axis.text.x = element_text(size = 12,angle = 45,hjust = 1),
          axis.text.y = element_text(size = 12),
          legend.position="right",
          legend.title=element_blank(),
          legend.key=element_rect(size=0.005),
          legend.text = element_text(size = 7))+
    labs(title = '' ,x= 'Time(h)',y='Cell Viabilit')+
    scale_x_discrete(expand = c(0,0.2))
  
  p2 <- ggline(nor_data, x="time", y="y", add = "mean_sd", color = "group",palette = 'simpsons')+
    theme(plot.title = element_text(lineheight=.8, size=30, face="bold",hjust = 0.5),
          axis.title= element_text(size = 12),
          axis.text.x = element_text(size = 12,angle = 45,hjust = 1),
          axis.text.y = element_text(size = 12),
          legend.position="right",
          legend.title=element_blank(),
          legend.key=element_rect(size=0.005),
          legend.text = element_text(size = 7))+
    labs(title = '' ,x= 'Time(h)',y='Relative Cell Viability')+
    scale_x_discrete(expand = c(0,0.2))
  
  
  
  #仪器与试剂
  #仪器
  equip_name <- c('Sorvall Legend Mircro 17台式离心机','Sorvall ST 16R冷冻离心机',
                  '微量移液器','生物安全柜','EVOS荧光显微成像系统',
                  '恒温二氧化碳细胞培养箱','实验室耗材I(移液枪头、1.5/2.0 mL离心管)',
                  '实验室耗材II(细胞培养皿、移液管等)','超低温冷冻冰箱',
                  'iMark Microplate Absorbance Reader')
  equip_source <- c('美国ThermoFisher公司','美国ThermoFisher公司','德国Eppendorf公司',
                    '美国ThermoFisher公司','美国ThermoFisher公司','美国ThermoFisher公司',
                    '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','美国Bio-Rad公司')
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
  regent_name <- c('质粒小量快速提取试剂盒(离心柱型)','限制性内切酶类',
                   'CCK-8细胞增殖及毒性检测试剂盒','DMEM高糖培养基','RPMI 1640培养基',
                   '胎牛血清')
  regent_source <- c('天根生化科技(北京)有限公司','美国NEB公司/美国ThermoFisher公司',
                     '北京索莱宝科技有限公司','美国Gibco公司','美国Gibco公司','美国Gibco公司')
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
    body_add_par(value = '项目名称：CCK-8法细胞增殖活力测定',style  = 'Subtitle')%>%
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
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "实验方法和分组", style = "heading 1") %>%
    body_add_par("实验方法",style='heading 2')  %>%
    body_add_par('一、细胞活性检测:',style='Normal')%>%
    body_add_par('1、在96孔板中接种细胞悬液（100 μL/孔）。将培养板放在培养箱中预培养（37℃,5%二氧化碳)',style='Normal')%>%
    body_add_par('2、向每孔加入10 μL CCK溶液（注意不要在孔中生成气泡，它们会影响OD值读数)。',style='Normal')%>%
    body_add_par('3、将培养板在培养箱内孵育1-4小时。',style='Normal')%>%
    body_add_par('4、用酶标仪测定在450 nm处的吸光度。
               ',style='Normal')%>%
    body_add_par('5、若暂时不测定OD值，可以向每孔中加入10 μL 0.1 M的HCl溶液或者1% w/v SDS溶液，并遮盖培养板避光保存在室温条件下。24小时内测定，吸光度不会发生变化。',style='Normal')%>%
    body_add_par("",style='Normal')%>%
    body_add_par('二、细胞增殖增殖/毒性检测:',style='Normal')%>%
    body_add_par('1、在96孔板中配置100 μL的细胞悬液。将培养板在培养箱预培养24小时（37℃，5%二氧化碳）。',style='Normal')%>%
    body_add_par('2、向培养板加入10 μL不同浓度的待测物质。',style='Normal')%>%
    body_add_par('3、将培养板在培养箱孵育一段适当的时间（例如：6、12、24或48小时）。',style='Normal')%>%
    body_add_par('4、向每孔加入10 μL CCK溶液（注意不要再孔中生成气泡，它们会影响OD值的读数）。
               ',style='Normal')%>%
    body_add_par('5、将培养板在培养箱内孵育1-4小时。',style='Normal')%>%
    body_add_par('6、用酶标仪测定在450 nm处的吸光度。',style='Normal')%>%
    body_add_par('7、若暂时不测定OD值，可以向每孔中加入10 μL 0.1 M的HCl溶液或者1% w/v SDS溶液，并遮盖培养板避光保存在室温条件下。24小时内测定，吸光度不会发生变化。',style='Normal')%>%
    body_add_par('注意：如果待测物质有氧化性或还原性的话，可在加CCK之前更换新鲜培养基（除去培养基，并用培养基洗涤细胞两次，然后加入新的培养基），去掉药物影响。当然药物影响比较小的情况下，可以不更换培养基，直接扣除培养基中加入药物后的空白吸收即可。'
                 ,style='Normal')%>%
    body_add_break(pos = "after")%>%
    
    
    body_add_par("实验分组",style='heading 2')  %>%
    body_add_flextable(value = assay_group_ft) %>%
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "结果与讨论", style = "heading 1") %>% 
    body_add_par("数据平均值和标准差", style = "heading 2")%>% 
    body_add_flextable(value = mean_sd_ft)%>%
    body_add_break(pos = "after")%>%
    body_add_par("显著性差异(t-test)", style = "heading 2")%>% 
    body_add_flextable(value = p_value_ft)%>% 
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "图表绘制", style = "heading 2") %>% 
    body_add_gg(value = p,width = 6,height = 3.7,style = 'Subtitle')%>%
    body_add_fpar(fpar(
      '图3.3.1  CCK-8细胞绝对增殖/毒性检测统计图',
      fp_p=fp_par(text.align = 'center')))%>%
    body_add_gg(value = p2,width = 6,height = 3.7,style = 'Subtitle')%>%
    body_add_fpar(fpar(
      '图3.3.2  CCK-8细胞相对增殖/毒性检测统计图',
      fp_p=fp_par(text.align = 'center')))%>%
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "实验结论", style = "heading 2")
  my_doc
  }
  
  
  
#########################
#                       #
#                       #
#       双萤光素酶      #
#                       #
#                       #
#########################
dluc <- function(raw_data1,raw_data2){
  raw_data1 <- raw_data1
  raw_data2 <- raw_data2
  data_head <- raw_data1[,1]
  detail_info <- str_which(data_head, '载体信息')
  ex_group <- str_which(data_head, '实验分组')
  contract_num <- str_which(data_head, '合同号')
  statistic_p <- str_which(data_head, '统计检验参数')
  paried <- str_which(data_head, '是否配对')
  #nor_group <- str_which(data_head, '归一化组')
  statistic_test_group <- str_which(data_head, '显著性检验分组')
  fontname <- "Arial"
  
  #实验分组
  assay_group_data <-  raw_data1[(ex_group+2):(statistic_p-1),]
  colnames(assay_group_data) <- raw_data1[(ex_group+1),]
  assay_group_ft <- assay_group_data %>%
    flextable()%>%
    add_header_lines("表2.2.2  分组信息表")%>%
    merge_v(j='调控元件')%>%
    fix_border_issues()%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    align(align = 'center',part = 'all')%>%
    footnote(i=2,j=1:2,part = 'head',value = as_paragraph(c('调控元件：miRNA、mimics或转录因子','靶点：mRNA 3‘UTR、miRNA靶序列或启动子')))%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w = 0.2)%>%
    width(j = 1, width = 1)%>%
    font(fontname = fontname, part = "all")
  
  #载体信息
  detail_info_data <-  raw_data1[(detail_info+2):(ex_group-1),1:3]
  colnames(detail_info_data) <- raw_data1[(detail_info+1),1:3]
  detail_info_ft <- detail_info_data %>%
    flextable()%>%
    add_header_lines("表2.2.1  载体信息表")%>%
    align(i=1,align = 'center',part = 'head')%>%
    align(align = 'center',part = 'all')%>%
    fontsize(size = 7.5,part = 'all')%>%
    autofit(add_h = 0.2,add_w = 0.2)%>%
    font(fontname = fontname, part = "all")
  
  
  #确定统计参数
  statistic_test_data <- raw_data1[str_which(raw_data1[,2],'vs'),1:3]
  compare_group <- paste(statistic_test_data[,1],statistic_test_data[,3],sep = '/',collapse = ',')
  compare_group_list <- strsplit(strsplit(compare_group,',')[[1]],'/')
  
  #去除na和极值
  omit_extream <- function(x){
    y <- x[order(abs(x-quantile(x,0.5,na.rm = TRUE)))]
    y <- y[1:6]
  } 
  
  colnames(raw_data2) <- str_replace_all(colnames(raw_data2),'[.]',' ')
  target_data <- raw_data2%>%
    gather(-(1:3),key = 'sample',value = 'value')%>%
    drop_na()%>%
    spread('sample',value)%>%
    select(-(1:3))%>%
    apply(1,omit_extream)
  
  data_body <- as.data.frame(t(target_data))
  colnames(data_body) <- c('sample1','sample2','sample3','sample4','sample5','sample6')
  data_label <- raw_data2%>%
    gather(-(1:3),key = 'sample',value = 'value')%>%
    drop_na()%>%
    filter(sample<7)%>%
    spread(sample,value)%>%
    select((1:3))
  
  clean_data <- cbind(data_label,data_body )
  
  z <- clean_data%>%
    gather(-(1:3),key = 'sample',value = 'value')%>%
    spread(荧光类型,value)%>%
    mutate(value=round(FLUC/RLUC,digits=3))%>%
    select(-FLUC,-RLUC)
  
  #z2 <- z%>%
  #  group_by(靶点)%>%
  #  mutate(value=round(value/as.numeric(nor_data[nor_data$靶点==unique(靶点),2]),digits = 3))%>%
  #  ungroup()
  
  #均值标准差表
  #未归一化
  mean_sd_data <- z %>% 
    group_by(调控元件,靶点)%>%
    select(value)%>%
    summarise(
      Mean=round(mean(value,na.rm = TRUE),digits = 3),
      SD=round(sd(value,na.rm = TRUE),digits = 3),
      QC=round(sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),digits = 3)
    )
  
  sample_data <- z%>%
    spread(sample,value)
  
  mean_sd_ft <- 
    inner_join(sample_data,mean_sd_data, by = c("调控元件",'靶点'))%>%
    flextable() %>%
    color(i=~QC>=0.5,j=~QC, color = "red")%>%
    set_header_labels(Mean='平均值',SD='标准差',sample1='复孔1',sample2='复孔2',sample3='复孔3',sample4='复孔4',sample5='复孔5',sample6='复孔6')%>%
    footnote(j=~QC,part = 'header',value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
    border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
    add_header_lines("表3.1.1 相对萤光强度平均值和标准差(fluc/rluc)")%>%
    align(align = 'center',part = 'all') %>%
    fontsize(size = 7.5,part = 'all')%>%
    merge_v(j=1)%>%
    autofit(add_h = 0.2) %>%
    width(j = 1, width = 1)%>%
    font(fontname = fontname, part = "all")
  
  #显著性
  p_value <- as.data.frame(do.call(cbind,lapply(compare_group_list,function(x){
    y <- z %>%
      filter(调控元件==x[1]|调控元件==x[2])%>%
      group_by(靶点)
    p_value <- unlist(lapply(split(y,y$靶点),function(x){
      if(var.test(value~调控元件,x)$p.value>=0.05){
        var_value=1
      }else{
        var_value=0}
      round(t.test(value~调控元件,x,var.equal =var_value)$p.value,digits = 3)
    }))
  })))
  colnames(p_value) <- unlist(lapply(compare_group_list,function(x){
    paste(x[1],'vs',x[2])
  }))
  p_value_table <-cbind('靶点'=rownames(p_value),p_value)  
  sig_symbol <- sapply(p_value_table[,2],function(x){
    ifelse(x>0.05,'ns',
           ifelse(x>=0.01,'*',
                  ifelse(x>=0.001,'**',
                         ifelse(x>=0.0001,'***','****'))))
  })
  p_value_table$'sig_symbol' <- sig_symbol
  
  p_value_ft <-  p_value_table%>%
    flextable() %>%
    add_header_lines("表3.2.1  显著性差异检验")%>%
    set_header_labels(sig_symbol='显著性')%>%
    footnote(j=3,part = 'header',
             value = as_paragraph('ns：P_value>0.05\n *：P_value≤0.05\n **：P_value≤0.01\n ***：P_value≤0.001\n ****：P_value≤0.0001\n')) %>%
    align(align = 'center',part = 'all') %>%
    fontsize(size = 9,part = 'all')%>%
    autofit(add_h = 0.2) %>%
    width(j = 1, width = 1.8)%>%
    height(i=1,height = 1.7,part = 'foot')%>%
    font(fontname = fontname, part = "all")
  
  p <- ggplot(mean_sd_data, aes(x=靶点, y=Mean,fill =调控元件))+
    geom_bar(stat = "identity",position="dodge",width = 0.5)+
    geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                  position=position_dodge(0.5),width=.2,color='grey')+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme(plot.title = element_text(face="bold"),
          plot.title.position = "panel",
          plot.subtitle = element_text(color = "grey"),
          panel.background = element_rect(fill = NA),
          axis.title= element_text(size = 12),
          axis.text.x = element_text(size = 10,angle = 45,hjust = 1),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 10),
          legend.position="right"
    )+
    scale_fill_viridis(option = 'E',discrete = T,alpha = 0.7)+
    labs(x='',y='Ralative Luciferase Activity \n(Ratio of fLuc/rLuc)',
         title = "Dual Luciferase Reporter Assay",fill="",
         caption = "Plot by Syngentech")
  
  #仪器与试剂
  #仪器
  equip_name <- c('Sorvall Legend Mircro 17台式离心机','Sorvall ST 16R冷冻离心机',
                  '微量移液器','生物安全柜','EVOS荧光显微成像系统',
                  '恒温二氧化碳细胞培养箱','实验室耗材I（移液枪头、1.5/2.0 mL离心管）',
                  '实验室耗材II（细胞培养皿、移液管等）','超低温冷冻冰箱',
                  'iMark Microplate Absorbance Reader')
  equip_source <- c('美国ThermoFisher公司','美国ThermoFisher公司','德国Eppendorf公司',
                    '美国ThermoFisher公司','美国ThermoFisher公司','美国ThermoFisher公司',
                    '美国Axygen公司','美国Corning公司','美国ThermoFisher公司','美国Bio-Rad公司')
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
  regent_name <- c('质粒小量快速提取试剂盒(离心柱型) ','限制性内切酶类',
                   '双萤光素酶检测试剂盒','DMEM高糖培养基','RPMI 1640培养基',
                   '胎牛血清')
  regent_source <- c('天根生化科技(北京)有限公司','美国NEB公司/美国ThermoFisher公司',
                     '上海碧云天生物科技有限公司','美国Gibco公司','美国Gibco公司','美国Gibco公司')
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
    body_add_par(value = '项目名称：双萤光素酶报告基因检测',style  = 'Subtitle')%>%
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
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "实验方法和分组", style = "heading 1") %>%
    body_add_par("实验方法",style='heading 2')  %>%
    body_add_par('实验原理：萤光素酶和其底物这一生物发光体系，可以非常灵敏、高效地检测基因的表达。通常把感兴趣基因的转录调控元件或5‘启动子区克隆在Luciferase的上游，或把3’-UTR区克隆在Luciferase的下游等，构建成报告基因(Reporter Gene)质粒。然后转染细胞，用适当药物等处理细胞后裂解细胞，测定萤光素酶活性。通过萤光素酶活性的高低来判断药物处理等对目的基因的转录调控作用。Renilla Luciferase相对更多地被用作转染效率的内参，以消除细胞数量和转染效率的差异。
                   ',style='chinese_style')%>%
    body_add_par('',style = 'chinese_style')%>%
    body_add_par('具体步骤：',style = 'chinese_style')%>%
    body_add_par('1、裂解细胞：将报告基因细胞裂解液充分混匀后，加入报告基因细胞裂解液，充分裂解细胞。',style='chinese_style')%>%
    body_add_par('2、融解萤火虫萤光素酶检测试剂和海肾萤光素酶检测缓冲液，并达到室温。海肾萤光素酶检测底物(100X)置于冰浴或冰盒上备用。',style='chinese_style')%>%
    body_add_par('3、按照每个样品需100微升的量，取适量海肾萤光素酶检测缓冲液，按照1:100加入海肾萤光素酶检测底物(100X)配制成海肾萤光素酶检测工作液。',style='chinese_style')%>%
    body_add_par('4、上机检测萤光强度，测定间隔设为2秒，测定时间设为10秒。
                   ',style='chinese_style')%>%
    body_add_par('5、每个样品测定时，取样品20-100微升(如果样品量足够，请加入100微升；如果样品量不足可以适当减少用量，但同批样品的使用量宜保持一致)。
                   ',style='chinese_style')%>%
    body_add_par('6、加入100微升萤火虫萤光素酶检测试剂，用枪打匀或用其它适当方式混匀后测定RLU(Relative Light Unit)。以报告基因细胞裂解液为空白对照。
                   ',style='chinese_style')%>%
    body_add_par('7、在完成上述测定萤火虫萤光素酶步骤后，加入100微升海肾萤光素酶检测工作液，用枪打匀或用其它适当方式混匀后测定RLU。
                   ',style='chinese_style')%>%
    body_add_par('8、在以海肾萤光素酶为内参的情况下，用萤火虫萤光素酶测定得到的RLU值（fluc）除以海肾萤光素酶测定得到的RLU（rluc）值。根据得到的比值来比较不同样品间目的报告基因的激活程度。如果以萤火虫萤光素酶为内参，也可以进行类似计算。
                   ',style='chinese_style')%>%
    body_add_break(pos = "after")%>%
    
    
    body_add_par("实验分组",style='heading 2')  %>%
    body_add_flextable(value = detail_info_ft) %>%
    body_add_par("",style='Normal')%>%
    body_add_flextable(value = assay_group_ft) %>%
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "结果与讨论", style = "heading 1") %>% 
    body_add_par("数据平均值和标准差", style = "heading 2")%>% 
    body_add_flextable(value = mean_sd_ft)%>%
    body_add_par("显著性差异(t-test)", style = "heading 2")%>% 
    body_add_flextable(value = p_value_ft)%>% 
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "图表绘制", style = "heading 2") %>% 
    body_add_gg(value = p ,style = "heading 3",height = 4)%>%
    body_add_break(pos = "after")%>%
    
    body_add_par(value = "实验结论", style = "heading 2")
   my_doc
}
  
  
#########################################################################主程序
  data_head <- raw_data1[,1]
  if(data_head[1]=='CCK8'){
    my_doc <- cck8(raw_data1,raw_data2)
  }else if(data_head[1]=='Reporter Gene Assay'){
    my_doc <- dluc(raw_data1,raw_data2)
  }else if(data_head[1]=='qRTPCR'){
    my_doc <- qRTPCR(raw_data1,raw_data2,image_path,image_name)
  }else if(data_head[1]=='gene_kd_oe'){
    my_doc <- gene_kd_oe(raw_data1,image_path,image_name)
  }else if(data_head[1]=='cell_apoptosis'){
    my_doc <- cell_apoptosis(raw_data1,raw_data2,image_path,image_name)
  }else if(data_head[1]=='colony_formation'){
    my_doc <- colony_formation(raw_data1,raw_data2)
  }else if(data_head[1]=='wound_healing'){
    my_doc <- wound_healing(raw_data1,raw_data2,image_path,image_name)
  }else if(data_head[1]=='transwell'){
    my_doc <- transwell(raw_data1,raw_data2)
  }else if(data_head[1]=='cell_cycle'){
    my_doc <- cell_cycle(raw_data1,raw_data2,image_path,image_name)
  }else if(data_head[1]=='Co-IP'){
    my_doc <- CoIP(raw_data1,raw_data2,image_path,image_name)
  }else if(data_head[1]=='EdU'){
    my_doc <- EdU(raw_data1,raw_data2,image_path,image_name)
  }else if(data_head[1]=='migration'){
    my_doc <- migration(raw_data1,raw_data2,image_path,image_name)
  }
  else{
    my_doc <- vector_vrius(raw_data1,raw_data2,image_path,image_name)
  }
}

  


