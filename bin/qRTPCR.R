

qRTPCR <- function(raw_data1,raw_data2,image_path,image_name){
        #表头定位
        raw_data1 <- raw_data1
        raw_data2 <- raw_data2
        data_head <- raw_data1[,1]
        detail_info <- str_which(data_head, '引物信息')
        ex_group <- str_which(data_head, '实验分组')
        contract_num <- str_which(data_head, '合同号')
        statistic_p <- str_which(data_head, '统计检验参数')
        paired <- str_which(data_head, '是否仅检测本底')
        statistic_test_group <- str_which(data_head, '显著性检验分组')
        fontname <- "Times New Roman"
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
        
        nor_matrix <- raw_data2[,str_which(colnames(raw_data2),'复孔')]-raw_data2[,str_which(colnames(raw_data2),'内参')]
        
        target_data<- raw_data2%>%
                select(1:3)%>%
                cbind(nor_matrix)%>%
                pivot_longer(-(1:3),names_to='sample',values_to='value',values_drop_na = TRUE)%>%
                group_by(相关基因,分组名称)%>%
                mutate(value.sort=value-quantile(value,0.5))%>%
                slice_min(value.sort,n=3,with_ties = FALSE)%>%
                mutate(sample=paste0('复孔',1:3))
        
        if(raw_data1[paired,2]=='否'){
                target_data <- target_data%>%
                        group_by(相关基因)%>%
                        mutate(value.fin=2^(mean(value[归一化组%in%'是'])-value))} else {
                                target_data <- target_data%>%
                                        mutate(value.fin=value)
                        }
        
        #均值标准差表
        mean_sd_data <- target_data %>% 
                group_by(相关基因,分组名称)%>%
                summarise(
                        Mean=round(mean(value.fin,na.rm = TRUE),digits = 3),
                        SD=round(sd(value.fin,na.rm = TRUE),digits = 3),
                        QC=round(sd(value.fin,na.rm = TRUE)/mean(value.fin,na.rm = TRUE),digits = 3)
                )
        
        mean_sd_ft <- target_data%>%
                select(-value,-value.sort)%>%
                mutate(value.fin=round(value.fin,digits = 1))%>%
                arrange(分组名称,sample,.by_group=T)%>%
                pivot_wider(names_from = sample,
                            values_from=value.fin)%>%
                inner_join(mean_sd_data, by = c('分组名称',"相关基因"))%>%
                flextable() %>%
                color(i=~QC>=0.5,j=~QC, color = "red")%>%
                set_header_labels(相关基因.x='相关基因',分组名称='实验分组',Mean='平均值',SD='标准差')%>%
                footnote(j=~QC,part = 'header',value = as_paragraph('QC值为标准差/平均值，数值越大表明该组实验数据波动性越高，其中≥0.5的值会被标记为红色'))%>%
                merge_v(j=1)%>%
                border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
                add_header_lines("表3.1.1 相对mRNA表达量平均值和标准差")%>%
                align(align = 'center',part = 'all') %>%
                fontsize(size = 7,part = 'all')%>%
                autofit(add_h = 0.2) %>%
                width(j = 1, width = 1)%>%
                height(part = 'foot',height = 1)%>%
                font(fontname = fontname, part = "all")
        
        if(raw_data1[paired,2]=='否'){
                mean_sd_ft <- mean_sd_ft%>%
                        compose(part = 'header',i=1,
                        value=as_paragraph('表3.1.1 相对mRNA表达量平均值和标准差(2',as_sup('-∆∆Ct'),')'))} else {
                        mean_sd_ft <- mean_sd_ft%>%
                        compose(part = 'header',i=1,
                        value = as_paragraph('表3.1.1 相对mRNA表达量平均值和标准差(∆Ct)'))
                        }
        
        
        
        
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
                body_bookmark(id='sigtest')%>%
                
                
                body_add_par(value = "图表绘制", style = "heading 2") %>% 
                body_add_gg(value = p ,style = "heading 3",height=6)%>%
                body_add_break(pos = "after")%>%
                
                body_add_par(value = "实验结论", style = "heading 2")
        
        #显著性
        if(raw_data1[paired,2]=='否'){
          p_value <- do.call(rbind,lapply(compare_group_list,function(x){
            y <- target_data %>%
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
          my_doc%>%
            cursor_bookmark("sigtest")%>%
            body_add_par("显著性差异(t-test)", style = "heading 2")%>% 
            body_add_flextable(value = p_value_ft)%>%
            body_add_break(pos = "after")
        }
        
        my_doc
}



