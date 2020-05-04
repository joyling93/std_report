#细胞凋亡模板
my_doc <- read_docx('./data/template.docx')
my_doc %>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("contract_num")%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par("项目结题报告",style  = 'Title')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par(value = '项目名称：细胞侵袭实验',style  = 'Subtitle')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("date")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "仪器与试剂", style = "heading 1") %>%
  body_add_par("实验仪器",style='heading 2')  %>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("equipment_info")%>%
  body_add_par("实验试剂",style='heading 2')  %>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("regent_info")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "实验方法和分组", style = "heading 1") %>%
  body_add_par("实验方法",style='heading 2')  %>%
  body_add_par('实验原理：从细胞外基质入侵是肿瘤转移的一个重要步骤，肿瘤细胞穿过重建基质膜的能力与它的体内侵袭转移能力表现出较好的相关性。侵袭实验通过在聚碳酸酯膜上涂上一层基质胶，模仿细胞外基质，上室种肿瘤细胞，下室加入 FBS 或某些特定的趋化因子，细胞欲进入下室，先要分泌基质金属蛋白酶（MMPs）将基质胶降解，方可通过聚碳酸酯膜，通过计数进入下室的细胞量测定细胞的侵袭能力。'
               ,style='chinese_style')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('具体步骤：',style = 'chinese_style')%>%
  body_add_par('1、准备感染后细胞：将处于对数生长期的各实验组细胞胰酶消化，完全培养基重悬，制成细胞悬液，计数。'
               ,style='chinese_style')%>%
  body_add_par('2、细胞接种：于 24 孔板培养板中各实验组接种 100 个细胞/孔（根据细胞生长情况确定），每个实验组设 3 个复孔。'
               ,style='chinese_style')%>%
  body_add_par('3、将接种好的细胞于培养箱中继续培养到 14 天或绝大多数单个克隆中细胞数大于 50 为止，中途每隔 3 天进行换液并观察细胞状态。'
               ,style='chinese_style')%>%
  body_add_par('4、实验终止前荧光显微镜下对细胞克隆进行拍照，PBS 洗涤细胞 1 次。'
               ,style='chinese_style')%>%
  body_add_par('5、每孔加入 1 mL 4% 多聚甲醛，固定细胞 30-60 min，PBS 洗涤细胞 1 次。'
               ,style='chinese_style')%>%
  body_add_par('6、每孔加入按照 1:20 稀释好的结晶紫水溶液 500 μL，染细胞 10 min。'
               ,style='chinese_style')%>%
  body_add_par('7、去离子水洗涤细胞数次，晾干，拍照，克隆计数。'
               ,style='chinese_style')%>%
  body_add_break(pos = "after")%>%
  
  body_add_par("实验分组",style='heading 2')  %>%
  body_add_par('',style = 'pic_style')%>%
  body_bookmark("exp_group") %>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "结果与讨论", style = "heading 1") %>% 
  body_add_par(value = "数据平均值和标准差", style = "heading 2") %>%
  body_add_par('',style = 'pic_style')%>%
  body_bookmark("mean_sd")%>%
  body_add_par('',style = 'pic_style')%>%
  body_bookmark("raw_data") %>%
  body_add_par("显著性差异(t-test)", style = "heading 2")%>%
  body_add_par('',style = 'pic_style')%>%
  body_bookmark("pvalue")%>% 
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "图表绘制", style = "heading 2")%>%
  body_add_par('',style = 'pic_style')%>%
  body_bookmark("ggplot") %>%
  
  body_add_par(value = "原始实验数据", style = "heading 2")%>%
  body_add_par(value = "《原始实验数据》文件夹内含有侵袭视野原始结果图。
                   ", style = "chinese_style")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "实验结论", style = "heading 2")%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("conclusion")



print(my_doc,target = './data/colony_formation_templete.docx')









#载体病毒模板
my_doc <- read_docx('./data/template.docx')
my_doc %>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("contract_num")%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par("项目结题报告",style  = 'Title')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par(value = '',style  = 'Subtitle')%>%
  body_bookmark('theme')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("date")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('pre_read_ft',style = 'pic_style')%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "载体构建信息", style = "heading 1")%>%
  body_add_par(value = "靶序列", style = "heading 2")%>%
  body_add_par(value = "载体图谱", style = "heading 2")%>%
  body_add_par(value = "结果质控", style = "heading 1")%>%
  body_add_par(value = "质粒酶切验证", style = "heading 2")%>%
  body_add_par(value = "测序比对验证", style = "heading 2")%>%
  body_add_par(value = "病毒滴度测定", style = "heading 2")%>%
  body_add_par(value = "附录", style = "heading 1")%>%
  body_add_par(value = "测序引物序列", style = "heading 2")%>%
  body_add_par(value = "分子实验数据", style = "heading 2")%>%
  body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")%>%
  body_add_par(value = "病毒实验数据", style = "heading 2")%>%
  body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））", style = "chinese_style")


print(my_doc,target = './data/vector&vrius_templete.docx')




