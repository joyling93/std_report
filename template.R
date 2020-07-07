#EdU
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
  body_add_par(value = '项目名称：细胞增殖实验(EdU）',style  = 'Subtitle')%>%
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
  body_add_par('实验原理：EdU(5-ethynyl-2’-deoxyuridine)，中文名为5-乙炔基-2’-脱氧尿苷，是一种新型胸苷(胸腺嘧啶脱氧核苷，thymidine)类似物，EdU可以在DNA合成过程中替代胸苷掺入到新合成的DNA中。另一方面，EdU上的乙炔基能与荧光标记的小分子叠氮化物探针(如Azide Alexa Fluor 488、Azide Alexa Fluor 555、Azide Alexa Fluor 594、Azide Alexa Fluor 647等)通过一价铜离子的催化发生共价反应，形成稳定的三唑环，该反应非常迅速，被称作点击反应(Click reaction)。通过点击反应，新合成的DNA会被相应的荧光探针所标记，从而可以使用适当的荧光检测设备检测到增殖的细胞。'
               ,style='chinese_style')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('具体步骤：',style = 'chinese_style')%>%
  body_add_par('1、在6孔板中(如有必要可以加入盖玻片)培养适当数量的细胞。细胞培养过夜并且恢复到正常状态后，进行所需的药物处理或者其它刺激处理等。'
               ,style='chinese_style')%>%
  body_add_par('2、配制2X的EdU工作液：由于EdU工作液是与培养液等体积加入到孔板中，所以需要配制成2X的工作液。推荐的EdU终浓度为10 μM (1X)，用细胞培养液1:500稀释EdU (10 mM)即可得到2X的EdU工作液(20 μM)。注意：不同细胞类型、培养液种类、细胞密度、细胞增殖速度等多方面的因素会影响EdU掺入到细胞中的量，因此初次使用时建议对EdU的使用浓度进行一定的摸索。'
               ,style='chinese_style')%>%
  body_add_par('3、将37ºC预热的2X的EdU工作液(20μM)，等体积加入6孔板中，使6孔板中的EdU终浓度变为1X。例如设计终浓度为10μM，原先6孔板中的培养基为1ml，则将1ml 2X的EdU工作液(20μM)加入到孔板中。如果培养基体积过大，可以先吸除适量的培养液，再加入等体积的2X的EdU工作液；或者可以减少工作液的体积并增加EdU的浓度，使最终培养液中的EdU浓度为10μM，例如2ml培养液中加入220微升0.1mM EdU。更换所有的培养液可能会对细胞的增殖有影响，因此不建议替换所有的培养液。
               ',style='chinese_style')%>%
  body_add_par('4、继续孵育细胞2小时。该孵育时间的长短取决于细胞生长速率，通常宜继续孵育细胞周期10%左右的时间。对于常见的哺乳动物细胞如HeLa、3T3、HEK293等，细胞周期大约在18-25小时，孵育时间宜在2小时左右。人胚胎细胞的细胞周期约30分钟，推荐的孵育时间为5分钟；酵母细胞的细胞周期约3小时，推荐的孵育时间为20分钟，增殖的神经细胞其细胞周期约5天，推荐的孵育时间为1天。孵育时间小于45分钟时，建议提高EdU的浓度；孵育时间大于20小时后，建议适当降低EdU的浓度。'
               ,style='chinese_style')%>%
  body_add_par('5、EdU标记细胞完成后，去除培养液，并加入1ml固定液(可以使用碧云天的免疫染色固定液P0098，或4%的多聚甲醛P0099)，室温固定15分钟。'
               ,style='chinese_style')%>%
  body_add_par('6、去除固定液，每孔用1ml洗涤液洗涤细胞3次，每次3-5分钟。'
               ,style='chinese_style')%>%
  body_add_par('7、去除洗涤液，每孔用1ml通透液(可以使用碧云天的免疫染色强力通透液P0097，免疫染色洗涤液P0106，或含0.3% Triton X-100的PBS)，室温孵育10-15分钟。'
               ,style='chinese_style')%>%
  body_add_par('8、去除通透液，每孔用1ml洗涤液洗涤细胞1-2次，每次3-5分钟。'
               ,style='chinese_style')%>%
  body_add_par('9、去除洗涤液，每孔用1ml通透液(可以使用碧云天的免疫染色强力通透液P0097，免疫染色洗涤液P0106，或含0.3% Triton X-100的PBS)，室温孵育10-15分钟。'
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
  body_add_par(value = "《原始实验数据》文件夹内含有显微镜视野原始结果图。
                   ", style = "chinese_style")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "实验结论", style = "heading 2")%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("conclusion")



print(my_doc,target = './data/EdU_templete.docx')









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
  body_add_par('',style = 'Normal')%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "载体构建信息", style = "heading 1")%>%
  body_add_par(value = "靶序列", style = "heading 2")%>%
  body_add_par(value = "载体", style = "heading 2")%>%
  body_add_par(value = "#载体图谱#", style = "Normal")%>%
  body_add_break(pos = "after")%>%
  body_add_par(value = "结果质控", style = "heading 1")%>%
  body_add_par(value = "质粒酶切验证", style = "heading 2")%>%
  body_add_par(value = "#酶切鉴定结果#", style = "Normal")%>%
  body_add_par(value = "测序比对验证", style = "heading 2")%>%
  body_add_par(value = "#靶序列测序结果#", style = "Normal")%>%
  body_add_par(value = "病毒滴度测定", style = "heading 2")%>%
  body_add_break(pos = "after")%>%
  body_add_par(value = "附录", style = "heading 1")%>%
  body_add_par(value = "测序引物序列", style = "heading 2")%>%
  body_add_par(value = "分子实验数据", style = "heading 2")%>%
  body_add_par(value = "病毒实验数据", style = "heading 2")
  

print(my_doc,target = './data/vector&vrius_templete.docx')



#细胞产品说明书
my_doc <- read_docx('./data/template.docx')
my_doc %>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("contract_num")%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par("细胞产品说明",style  = 'Title')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  #body_add_par(value = '项目名称：细胞增殖实验(EdU）',style  = 'Subtitle')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("date")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "产品内容", style = "heading 1") %>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("table1")%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("table2")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "注意事项", style = "heading 1") %>%
  body_add_par("冻存细胞",style='heading 2')  %>%
  body_add_par('收到细胞后，请检查产品内容，如有异常（如：缺货、无干冰等）请拍照记录。
               ',style='chinese_style')%>%
  body_add_par('请于收货后1周内复苏细胞, 建议按照“客户自用培养基：产品附送培养基=3:7/7:3/10:0”的方式进行3次传代，避免细胞因培养基成分骤变而状态异常。
               ',style='chinese_style')%>%
  body_add_par("新鲜细胞",style='heading 2')  %>%
  body_add_par('收到细胞后，请检查培养瓶是否完好，如有异常（如：缺货、漏液等）请拍照记录
               ',style='chinese_style')%>%
  body_add_par('建议按照“客户自用培养基：产品附送培养基=3:7/7:3/10:0”的方式进行3次传代，避免细胞因培养基成分骤变而状态异常。
               ',style='chinese_style')%>%
  body_add_par(value = "细胞冻存与复苏方法", style = "heading 1") %>%
  body_add_par("细胞冻存",style='heading 2')  %>%
  body_add_par('当细胞汇合度达到 70% 时，常规消化收集至15 mL离心管中，800 rpm 离心 5 min，弃掉原培养基，加入冻存液重悬。将约 1.5 mL 悬浮液缓缓加入到冻存管，标记细胞种类和冻存时间后放入冻存盒中 -80 ℃ 梯度降温。约 24 h后取出，冻存于液氮罐，记录存放位置。
               ',style='chinese_style')%>%
  body_add_par("细胞复苏",style='heading 2')  %>%
  body_add_par('设置水浴锅温度37 ℃。取冻存细胞放于37 ℃水中，迅速摇动使其快速解冻。解冻之后，将细胞转移至15 mL离心管，加入3 mL无菌PBS溶液，800 rpm离心5 min，去上清，加培养基4 mL，重悬后转移至60 mm培养皿，24 h后更换新的培养基，根据细胞密度传代或继续培养。
               ',style='chinese_style')  




print(my_doc,target = './data/cell_product_templete.docx')


#细胞支原体检测
my_doc <- read_docx('./data/template.docx')
my_doc %>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("contract_num")%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par("细胞支原体检测",style  = 'Title')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  #body_add_par(value = '项目名称：细胞增殖实验(EdU）',style  = 'Subtitle')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("date")%>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "细胞信息", style = "heading 1") %>%
  body_add_par('',style = 'Normal')%>%
  body_add_par('',style = 'Normal')%>%
  body_bookmark("table1")%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par(value = "检测结果", style = "heading 1") %>%
  body_bookmark('pic')%>%
  body_add_par('',style = 'Normal')%>%
  body_add_par(value = "注：本项目采用 Vazyme KIT 方法检测细胞培养物支原体，但是由于支原体种类繁多，检测方法多样，该结果仅供参考。
               ", style = "chinese_style") 


print(my_doc,target = './data/cell_myco_templete.docx')


#wb
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
  body_add_par(value = '项目名称：western_blot蛋白质印迹实验',style  = 'Subtitle')%>%
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
  body_add_break(pos = "after")%>%
  
  body_add_par("实验分组",style='heading 2')  %>%
  body_add_par('',style = 'pic_style')%>%
  body_bookmark("exp_group") %>%
  body_add_break(pos = "after")%>%
  
  body_add_par(value = "实验结果", style = "heading 1") %>% 
  body_bookmark("result")
print(my_doc,target = './data/wb_templete.docx')

