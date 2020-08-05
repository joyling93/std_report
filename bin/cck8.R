
#ic50
cck8 <- function(raw_data1,raw_data2){  
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
        
        target_data <- raw_data2%>%
                pivot_longer(3:length(raw_data2),
                             names_to='group',
                             values_to='values',
                             values_drop_na=TRUE)%>%
                group_by(group,time)%>%
                mutate(values.sort=values-quantile(values,0.5))%>%
                slice_min(values.sort,n=6,with_ties = FALSE)%>%
                mutate(sample=paste0('复孔',1:3))
        
        mean_sd_data <- target_data%>%
                summarise(
                        Mean=round(mean(values,na.rm = TRUE),digits = 2),
                        SD=round(sd(values,na.rm = TRUE),digits = 2),
                        QC=round(sd(values,na.rm = TRUE)/mean(values,na.rm = TRUE),digits = 2)
                )
        
        mean_sd_ft <- 
                target_data%>%
                select(-values.sort)%>%
                pivot_wider(names_from=sample,values_from=values)%>%
                left_join(y=mean_sd_data)%>%
                select(group,everything())%>%
                flextable()%>%
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
      
        #显著性
        p_value <- as.data.frame(do.call(cbind,lapply(compare_group_list,function(x){
                y <- target_data[,c(1,3,4)]%>%
                        rename(value=values)%>%
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
        p1 <- ggplot(mean_sd_data,aes(time,Mean,color=group,fill=group))+
                geom_line()+
                geom_point(size=1.5)+
                geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                              width=.7)+
                scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
                scale_x_continuous(expand = expansion(mult = c(0,.01)))+
                theme(plot.title = element_text(face="bold"),
                      plot.title.position = "panel",
                      plot.subtitle = element_text(color = "grey"),
                      panel.background = element_rect(fill = NA),
                      legend.key = element_rect(fill = NA),
                      axis.title= element_text(size = 12),
                      axis.text.x = element_text(size = 10,hjust = 1),
                      axis.line = element_line(colour = "black"),
                      axis.text.y = element_text(size = 10),
                      legend.position="right",
                )+
                scale_color_viridis(option = 'D',discrete = T)+
                labs(title = '' ,x= 'Time(h)',y='Cell Viabilit')
        
        p2 <- target_data%>%
                group_by(group)%>%
                mutate(values=round(values/mean(values[time==0]),digits = 2))%>%
                group_by(time,group)%>%
                summarise(
                        Mean=round(mean(values,na.rm = TRUE),digits = 2),
                        SD=round(sd(values,na.rm = TRUE),digits = 2),
                        QC=round(sd(values,na.rm = TRUE)/mean(values,na.rm = TRUE),digits = 2)
                )%>%
                ggplot(aes(time,Mean,color=group,fill=group))+
                geom_line()+
                geom_point(size=1.5)+
                geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),stat = "identity",
                              width=.7)+
                scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
                scale_x_continuous(expand = expansion(mult = c(0,.01)))+
                theme(plot.title = element_text(face="bold"),
                      plot.title.position = "panel",
                      plot.subtitle = element_text(color = "grey"),
                      panel.background = element_rect(fill = NA),
                      legend.key = element_rect(fill = NA),
                      axis.title= element_text(size = 12),
                      axis.text.x = element_text(size = 10,hjust = 1),
                      axis.line = element_line(colour = "black"),
                      axis.text.y = element_text(size = 10),
                      legend.position="right",
                )+
                scale_color_viridis(option = 'D',discrete = T)+
                labs(title = '' ,x= 'Time(h)',y='Relative Cell Viability')
        p <- cowplot::plot_grid(p1,p2,ncol = 1,
                                labels=list('图3.3.1  CCK-8细胞绝对增殖/毒性检测统计图',
                                            '图3.3.2  CCK-8细胞相对增殖/毒性检测统计图'),
                                label_fontfamily ='Songti SC',
                                hjust = -0.1)
        
        ## ic50
        if(data_head=="CCK8(IC50)"){
                dose.c <-  assay_group_data%>%
                        select_if(~all(!is.na(.)))%>%
                        dplyr::select(contains("处理"))%>%
                        as_tibble()%>%
                        str_match_all(.,"[\\s，,](\\d+)")
                
                target_data <- left_join(target_data,
                                         data.frame('group'=assay_group_data[,1],
                                                    'con'=as.numeric(dose.c[[1]][,2])))%>%
                        ungroup%>%
                        arrange(con)%>%
                        mutate(values=round(values/mean(values[con%in%con[1:3]]),digits = 3)*100)
                #dot data
                ryegrass.LL.4 <- drc::drm(values~con,data = target_data,fct = drc::LL.4())
                ic50 <- round(drc::ED(ryegrass.LL.4, 50)[,1],digits = 2)
                #line data
                newdata <- expand.grid(conc=exp(seq(log(min(target_data$con)+0.01), log(max(target_data$con)), length=500)))
                pm <- predict(ryegrass.LL.4, newdata=newdata, interval="confidence") 
                newdata$p <- pm[,1]
                newdata$pmin <- pm[,2]
                newdata$pmax <- pm[,3]
                
                # plotting the curve
                p <- target_data%>%
                        group_by(con)%>%
                        summarise(y=mean(values))%>%
                        ggplot(aes(x = con, y = y)) +
                        geom_point()+ 
                        annotate("text",x=range(newdata$conc)[2],
                                 y=range(newdata$p)[2],
                                 label=paste0("IC50:",ic50),
                                 hjust=1.2,vjust=1.2,size=6)+
                        geom_ribbon(data=newdata, aes(x=conc, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
                        geom_line(data=newdata, aes(x=conc, y=p)) +
                        xlab("Drug Level") + ylab("cell viablity(%)")+
                        cowplot::theme_cowplot(12)
        }
        
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
                body_add_gg(value = p,width = 6,height = ifelse(data_head[1]=="CCK8(IC50)",3.7,7.4),style = 'Subtitle')%>%
                body_add_break(pos = "after")%>%
                
                body_add_par(value = "实验结论", style = "heading 2")
        my_doc
}