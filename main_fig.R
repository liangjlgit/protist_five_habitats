################fig1################################
#1a
alpha_index(t(protist_table_div), method = 'richness', base = exp(1)) %>% data.frame() %>% 
  set_names( "richness")  %>% 
  mutate(Habitats=str_replace(rownames(.),'.*Fa.*', "Farmland"))%>% mutate(Habitats=str_replace(Habitats,'.*Fo.*', "Forest")) %>%
  mutate(Habitats=str_replace(Habitats,'.*Fo.*', "Forest")) %>% mutate(Habitats=str_replace(Habitats,'.*Go.*', "Gobi Desert")) %>%
  mutate(Habitats=str_replace(Habitats,'.*Gr.*', "Grassland")) %>% mutate(Habitats=str_replace(Habitats,'.*T.*', "Mine wasteland")) %>% 
  mutate(Habitats=str_replace(Habitats,'.*\\.D.*', "Mine wasteland")) ->rich_protist
rich_protist %>% 
  ggplot(aes(x=Habitats, y=richness))+
  stat_summary(fun.data = mean_sdl,geom="errorbar",width=0.2 )+
  stat_summary(mapping=aes(fill =Habitats),fun=mean,geom = "bar",fun.args = list(mult=1)) +
  scale_fill_manual(values = c('#27c3da','#f279a2','#abac46','#acd47c','#67524d')) +
  geom_jitter(width = 0.25,fill="#ff8400", size = 3, shape = 21, alpha = 0.75) + theme_pubr(border = T, legend = "none", base_size = 15)+ylim(c(0,450))+
  theme(panel.background = element_rect(fill = "#fbfbfa"))+ylab(label = "Richness of soil protist") + 
  stat_compare_means()+ theme(axis.text.x = element_text(vjust = 0.75, angle = 30)) -> p_rich_div_protist

#1bcd
rich_protist %>% 
  ggplot(aes(x=Habitats, y=richness))+
  geom_boxplot(mapping=aes(fill =Habitats)) +
  scale_fill_manual(values = c('#27c3da','#f279a2','#abac46','#acd47c','#67524d')) +
  geom_jitter(width = 0.25,fill="#ff8400", size = 3, shape = 21, alpha = 0.75) + theme_pubr(border = T, legend = "none", base_size = 15)+ylim(c(0,250))+
  theme(panel.background = element_rect(fill = "#fbfbfa"))+ylab(label = "Richness of soil Consumer") + stat_summary(fun.y = 'mean',geom="point",shape=23,size =5 ,fill = "red")+
  stat_compare_means(method = 'anova')+ theme(axis.text.x = element_text(vjust = 0.75, angle = 30)) -> p_rich_div_Consumer

rich_protist %>% 
  ggplot(aes(x=Habitats, y=richness))+
  #stat_summary(fun.data = mean_sdl,geom="errorbar",width=0.2 )+
  geom_boxplot(mapping=aes(fill =Habitats)) +
  scale_fill_manual(values = c('#27c3da','#f279a2','#abac46','#acd47c','#67524d')) +
  geom_jitter(width = 0.25,fill="#ff8400", size = 3, shape = 21, alpha = 0.75) + theme_pubr(border = T, legend = "none", base_size = 15)+ylim(c(0,250))+
  theme(panel.background = element_rect(fill = "#fbfbfa"))+ylab(label = "Richness of soil Phototrophic") + stat_summary(fun.y = 'mean',geom="point",shape=23,size =5 ,fill = "red")+
  stat_compare_means(method = 'anova')+ theme(axis.text.x = element_text(vjust = 0.75, angle = 30)) -> p_rich_div_Phototrophic

rich_protist %>% 
  ggplot(aes(x=Habitats, y=richness))+
  #stat_summary(fun.data = mean_sdl,geom="errorbar",width=0.2 )+
  geom_boxplot(mapping=aes(fill =Habitats)) +
  scale_fill_manual(values = c('#27c3da','#f279a2','#abac46','#acd47c','#67524d')) +
  geom_jitter(width = 0.25,fill="#ff8400", size = 3, shape = 21, alpha = 0.75) + theme_pubr(border = T, legend = "none", base_size = 15)+ylim(c(0,25))+
  theme(panel.background = element_rect(fill = "#fbfbfa"))+ylab(label = "Richness of soil Parasite") + stat_summary(fun.y = 'mean',geom="point",shape=23,size =5 ,fill = "red")+
  stat_compare_means(method = 'anova')+ theme(axis.text.x = element_text(vjust = 0.75, angle = 30)) -> p_rich_div_Parasite

#1e
protist_table <- read.csv("Pr2_tax_protist.csv", header = 1,row.names = 1)
cbind(protist_table_div,protist_table[271:279]) -> protist_table2
cbind(protist_table2 %>% select(contains('.Fa'), Function) %>% mutate(Farmland = rowMeans(across(where(is.numeric)))) %>% filter(Farmland > 0) %>% select(Function, Farmland) %>% 
        group_by(Function) %>% summarise(across(where(is.numeric), sum)),
      protist_table2 %>% select(contains('.Fo'), Function) %>% mutate(Forest = rowMeans(across(where(is.numeric)))) %>% filter(Forest > 0) %>% select(Function, Forest) %>% 
        group_by(Function) %>% summarise(across(where(is.numeric), sum)),
      protist_table2 %>% select(contains('.Go'), Function) %>% mutate('Gobi desert' = rowMeans(across(where(is.numeric)))) %>% filter('Gobi desert' > 0) %>% select(Function, 'Gobi desert') %>% 
        group_by(Function) %>% summarise(across(where(is.numeric), sum)),
      protist_table2 %>% select(contains('.Gr'), Function) %>% mutate(Grassland = rowMeans(across(where(is.numeric)))) %>% filter(Grassland > 0) %>% select(Function, Grassland) %>% 
        group_by(Function) %>% summarise(across(where(is.numeric), sum)),
      protist_table2 %>% select(contains(c(".T" ,".D")), Function) %>% mutate('Mine wasteland' = rowMeans(across(where(is.numeric)))) %>% filter('Mine wasteland' > 0) %>% select(Function, 'Mine wasteland') %>% 
        group_by(Function) %>% summarise(across(where(is.numeric), sum))) %>% data.frame() %>% select(1,2,4,6,8, 10) %>% melt() -> melt_function
library(ggchicklet)
library(hrbrthemes)
melt_function$Function <-  factor(melt_function$Function, levels = c('Unknown','Consumer','Parasite','Phototrophic'))
melt_function %>% as_tibble() %>% 
  ggplot() +
  geom_chicklet(aes( y = value, x = variable, fill = Function, color=Function), position = "fill",width = 0.75, radius = grid::unit(0, "pt"))+#
  #geom_col(aes( x = value, y = variable, fill = Function, color=Function),  position = "fill") +
  scale_fill_manual(values = c( '#ef5350', '#1270ca',  '#3b9d41',   '#ff7f00'), "Key") + 
  scale_color_manual(values = c(  '#ef5350', '#1270ca'  , '#3b9d41',   '#ff7f00'), "Key")+  
  theme_pubr(border = T, legend = "top", base_size = 25)+
  ylab(label = "% Relative abundance")+
  xlab(label = NULL)

################fig2################################
rich_consumer %>% dplyr::select(1) %>% set_names("Consumer richness") %>% mutate(id=rownames(.)) -> consumer_rich
shan_consumer %>% dplyr::select(1) %>% set_names("Consumer Shannon") %>% mutate(id=rownames(.)) -> consumer_shan
rich_Phototrophic %>% dplyr::select(1) %>% set_names("Phototroph richness") %>% mutate(id=rownames(.))-> photo_rich
shan_Phototrophic %>% dplyr::select(1) %>% set_names("Phototroph Shannon") %>% mutate(id=rownames(.))-> photo_shan
rich_Parasite %>% dplyr::select(1) %>% set_names("Parasite richness") %>% mutate(id=rownames(.))-> paras_rich
shan_Parasite %>% dplyr::select(1) %>% set_names("Parasite Shannon") %>% mutate(id=rownames(.))-> paras_shan

consumer_rich %>% left_join(consumer_shan, by = 'id') %>% left_join(photo_rich, by = 'id') %>% left_join(photo_shan, by = 'id') %>% 
  left_join(paras_rich, by = 'id') %>% left_join(paras_shan, by = 'id') %>% dplyr::select(id, everything())-> three_his_alpha
three_his_alpha[is.na(three_his_alpha)]  <- 0

#library(devtools)
#install_github('Hy4m/linkET',force = TRUE) # 
library(linkET)
ls("package:linkET") # 
#mget(ls("package:linkET"), inherits = TRUE) # 
library(ggplot2)
library(dplyr)
#尾矿
protist_table %>% dplyr::select(contains(c(".T" ,".D", "Function")))  -> tmp #otu
read.table("factor-select17.csv",header=TRUE, row.names=1, sep = ",") %>% filter(grepl("T|\\.D" , rownames(.))) %>% dplyr::select(-clay, -Cu)-> env
env %>% merge(rich_fun, by.x = 0, by.y = 0) %>% merge(shan_fun,by.x = 'Row.names', by.y = 0) %>% dplyr::select(1:16,18) %>% 
  column_to_rownames('Row.names') -> tmp_env
colnames(tmp_env) <- c('Longitude','Latitude','Altitude','MAT','MAP','TP','Moisture','pH','EC','Ex_Ca','CEC','TN','TC','C/N','Richness','Shannon')
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".T" ,".D")))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".T" ,".D")))  %>%
  filter(Function=="Parasite") %>% melt() -> tm
tmp1$Parasite <- tm$value
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".T" ,".D")))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select(-2,-4,-7) %>% dplyr::select(1,2,3,4)  -> da_ta2
colnames(da_ta2)[2:3] <- c("Phototrophic",'Consumer')
tmp_env %>% merge(da_ta2, by.x=0, by.y='variable') %>% column_to_rownames(var = 'Row.names') %>% dplyr::select(1:14, 18,19,17,15,16)-> tmp_env

tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("Shannon"))-> tmp_env
#tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("ichness"))-> tmp_env
tmp_env %>% dplyr::select(1:15, 17,16, 19,20,21,18) -> tmp_env

tail_da <- read.csv("C:\\Users\\fengsw\\Desktop\\18s\\18s_ana\\PR2\\1.new_fig\\new_fig2\\tail.csv", header = 1, row.names = 1) %>% select( bac_rich, fun_rich)

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") -> tmppp
qcorrplot(correlate(tmppp, method = "spearman"), type = "full", diag = FALSE) +
  geom_square(color="white") +geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) -> pp_mi

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") %>% select( -15, -16,-17,-18,-19,-20  ) -> tmp_env




tmp %>% arrange(Function) %>% dplyr::select(-Function) -> tmp2
# tmp %>% arrange(Function) %>% filter(Function=="Consumer") %>% dim() -> a 
# tmp %>% arrange(Function) %>% filter(Function=="Phototrophic") %>% dim() -> b 
# tmp %>% arrange(Function) %>% filter(Function=="Parasite") %>% dim() -> c 
library(iCAMP)
match.name(rn.list=list(a=t(tmp2),b=tmp_env)) -> sampid.check

mantel <- mantel_test(
  seed = 12345,
  spec = sampid.check$a, env = sampid.check$b,
  spec_select = list(Protist = 1:dim(tmp2)[1]), 
  #env_select =  as.list(setNames(1:ncol(tmp_env), names(tmp_env))),
  spec_dist =  dist_func(.FUN = "vegdist", method = "bray"), # 
  env_dist = dist_func(.FUN = "vegdist", method = "euclidean"),
  mantel_fun = 'mantel', # mantel.partial
  na_omit=T,
)
mantel <- mutate(mantel, 
                 rd = cut(r, right = TRUE,# 
                          breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
                          labels = c('[0, 0.2]', '(0.2, 0.4]', '(0.4, 0.6]', '(0.6, 1]')
                 ),
                 pd = cut(p, right = FALSE,# 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                          labels = c('***', '**', '*', 'ns'),
                 )
)
mantel -> mine_mantel
#绘图
qcorrplot(correlate(tmp_env, method = "spearman"), type = "lower", diag = FALSE) +
  geom_square(color="white") +
  geom_couple(aes(colour = pd, size = rd), data = mine_mantel, 
              curvature = nice_curvature(-0.15)) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) +
  geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5)) +
  scale_colour_manual(values = c( "#419336" ,  "#605788" ,  "#CCCCCC99")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Spearman's r", order = 3)) + guides(fill='none', colour="none", size="none")-> mine_p

#fa
protist_table %>% dplyr::select(contains(c(".Fa", "Function"))) -> tmp #otu
read.table("factor-select17.csv",header=TRUE, row.names=1, sep = ",") %>% filter(grepl(".Fa" , rownames(.))) %>% dplyr::select(-clay, -Cu)-> env
env %>% merge(rich_fun, by.x = 0, by.y = 0) %>% merge(shan_fun,by.x = 'Row.names', by.y = 0) %>% dplyr::select(1:16,18) %>% 
  column_to_rownames('Row.names') -> tmp_env
colnames(tmp_env) <- c('Longitude','Latitude','Altitude','MAT','MAP','TP','Moisture','pH','EC','Ex_Ca','CEC','TN','TC','C/N','Richness','Shannon')
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fa'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".Fa")))  %>%
  filter(Function=="Parasite") %>% melt() -> tm
tmp1$Parasite <- tm$value

protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fa'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select(-2,-4,-7) %>% dplyr::select(1,2,3,4)  -> da_fa2
colnames(da_fa2)[2:3] <- c("Phototrophic",'Consumer')
tmp_env %>% merge(da_fa2, by.x=0, by.y='variable') %>% column_to_rownames(var = 'Row.names')%>% dplyr::select(1:14, 18,19, 17,15,16)-> tmp_env
tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("Shannon"))-> tmp_env
tmp_env %>% dplyr::select(1:15, 17,16, 19,20,21,18) -> tmp_env


tail_da <- read.csv("C:\\Users\\fengsw\\Desktop\\18s\\18s_ana\\PR2\\1.new_fig\\new_fig2\\fa.csv", header = 1, row.names = 1) %>% select( bac_rich, fun_rich)
tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") %>% select( -15, -16,-17,-18,-19,-20  ) -> tmp_env

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") -> tmppp
qcorrplot(correlate(tmppp, method = "spearman"), type = "full", diag = FALSE) +
  geom_square(color="white") +geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) -> pp_fa

tmp %>% arrange(Function) %>% dplyr::select(-Function) -> tmp2
library(iCAMP)
match.name(rn.list=list(a=t(tmp2),b=tmp_env)) -> sampid.check

mantel <- mantel_test(
  seed = 12345,
  spec = sampid.check$a, env = sampid.check$b,
  spec_select = list(Protist = 1:dim(tmp2)[1]), 
  #env_select =  as.list(setNames(1:ncol(tmp_env), names(tmp_env))),
  spec_dist =  dist_func(.FUN = "vegdist", method = "bray"), # 
  env_dist = dist_func(.FUN = "vegdist", method = "euclidean"),
  mantel_fun = 'mantel', # mantel.partial
  na_omit=T,
)
mantel <- mutate(mantel, 
                 rd = cut(r, right = TRUE,# 
                          breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
                          labels = c('[0, 0.2]', '(0.2, 0.4]', '(0.4, 0.6]', '(0.6, 1]')
                 ),
                 pd = cut(p, right = FALSE,# 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                          labels = c('***', '**', '*', 'ns'),
                 )
)
mantel -> fa_mantel
#
qcorrplot(correlate(tmp_env, method = "spearman"), type = "lower", diag = FALSE) +
  geom_square(color="white") +
  geom_couple(aes(colour = pd, size = rd), data = fa_mantel, 
              curvature = nice_curvature(-0.15)) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) +
  geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5)) +
  scale_colour_manual(values = c( "#419336" ,  "#605788" ,  "#CCCCCC99")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Spearman's r", order = 3)) + guides(fill='none', colour="none", size="none")-> fa_p

#fo
protist_table %>% dplyr::select(contains(c(".Fo", "Function")))  -> tmp #otu
read.table("factor-select17.csv",header=TRUE, row.names=1, sep = ",") %>% filter(grepl(".Fo" , rownames(.))) %>% dplyr::select(-clay, -Cu)-> env
env %>% merge(rich_fun, by.x = 0, by.y = 0) %>% merge(shan_fun,by.x = 'Row.names', by.y = 0) %>% dplyr::select(1:16,18) %>% 
  column_to_rownames('Row.names') -> tmp_env
colnames(tmp_env) <- c('Longitude','Latitude','Altitude','MAT','MAP','TP','Moisture','pH','EC','Ex_Ca','CEC','TN','TC','C/N','Richness','Shannon')
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fo'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".Fo")))  %>%
  filter(Function=="Parasite") %>% melt() -> tm
tmp1$Parasite <- tm$value
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fo'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select(-2,-4,-7) %>% dplyr::select(1,2,3,4)  -> da_fo2
colnames(da_fo2)[2:3] <- c("Phototrophic",'Consumer')
tmp_env %>% merge(da_fo2, by.x=0, by.y='variable') %>% column_to_rownames(var = 'Row.names') %>% dplyr::select(1:14, 18,19, 17,15,16) -> tmp_env

tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("Shannon"))-> tmp_env
#tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("ichness"))-> tmp_env
tmp_env %>% dplyr::select(1:15, 17,16, 19,20,21,18) -> tmp_env


tail_da <- read.csv("C:\\Users\\fengsw\\Desktop\\18s\\18s_ana\\PR2\\1.new_fig\\new_fig2\\fo.csv", header = 1, row.names = 1) %>% select( bac_rich, fun_rich)

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") -> tmppp
qcorrplot(correlate(tmppp, method = "spearman"), type = "full", diag = FALSE) +
  geom_square(color="white") +geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) -> pp_fo

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") %>% select( -15, -16,-17,-18,-19,-20  ) -> tmp_env


tmp %>% arrange(Function) %>% dplyr::select(-Function) -> tmp2
# tmp %>% arrange(Function) %>% filter(Function=="Consumer") %>% dim() -> a 
# tmp %>% arrange(Function) %>% filter(Function=="Phototrophic") %>% dim() -> b 
# tmp %>% arrange(Function) %>% filter(Function=="Parasite") %>% dim() -> c 
library(iCAMP)
match.name(rn.list=list(a=t(tmp2),b=tmp_env)) -> sampid.check

mantel <- mantel_test(
  seed = 12345,
  spec = sampid.check$a, env = sampid.check$b,
  spec_select = list(Protist = 1:dim(tmp2)[1]), 
  #env_select =  as.list(setNames(1:ncol(tmp_env), names(tmp_env))),
  spec_dist =  dist_func(.FUN = "vegdist", method = "bray"), # 
  env_dist = dist_func(.FUN = "vegdist", method = "euclidean"),
  mantel_fun = 'mantel', # mantel.partial：
  na_omit=T,
)
mantel <- mutate(mantel, 
                 rd = cut(r, right = TRUE,# 
                          breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
                          labels = c('[0, 0.2]', '(0.2, 0.4]', '(0.4, 0.6]', '(0.6, 1]')
                 ),
                 pd = cut(p, right = FALSE,# 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                          labels = c('***', '**', '*', 'ns'),
                 )
)
mantel -> fo_mantel
#
qcorrplot(correlate(tmp_env, method = "spearman"), type = "lower", diag = FALSE) +
  geom_square(color="white") +
  geom_couple(aes(colour = pd, size = rd), data = fo_mantel, 
              curvature = nice_curvature(-0.15)) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) +
  geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5)) +
  scale_colour_manual(values = c( "#419336" ,  "#605788" ,  "#CCCCCC99")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Spearman's r", order = 3)) + guides(fill='none', colour="none", size="none")-> fo_p


#go
protist_table %>% dplyr::select(contains(c(".Go", "Function")))  -> tmp #otu
read.table("factor-select17.csv",header=TRUE, row.names=1, sep = ",") %>% filter(grepl(".Go" , rownames(.))) %>% dplyr::select(-clay, -Cu)-> env
env %>% merge(rich_fun, by.x = 0, by.y = 0) %>% merge(shan_fun,by.x = 'Row.names', by.y = 0) %>% dplyr::select(1:16,18) %>% 
  column_to_rownames('Row.names') -> tmp_env
colnames(tmp_env) <- c('Longitude','Latitude','Altitude','MAT','MAP','TP','Moisture','pH','EC','Ex_Ca','CEC','TN','TC','C/N','Richness','Shannon')
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Go'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".Go")))  %>%
  filter(Function=="Parasite") %>% melt() -> tm
tmp1$Parasite <- tm$value
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Go'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select(-2,-4,-7) %>% dplyr::select(1,2,3,4)  -> da_go2
colnames(da_go2)[2:3] <- c("Phototrophic",'Consumer')
tmp_env %>% merge(da_go2, by.x=0, by.y='variable') %>% column_to_rownames(var = 'Row.names') %>% dplyr::select(1:14,18, 19, 17,15,16) -> tmp_env
tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("Shannon"))-> tmp_env
#tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("ichness"))-> tmp_env
tmp_env %>% dplyr::select(1:15, 17,16, 19,20,21,18) -> tmp_env


tail_da <- read.csv("C:\\Users\\fengsw\\Desktop\\18s\\18s_ana\\PR2\\1.new_fig\\new_fig2\\go.csv", header = 1, row.names = 1) %>% select( bac_rich, fun_rich)

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") -> tmppp
qcorrplot(correlate(tmppp, method = "spearman"), type = "full", diag = FALSE) +
  geom_square(color="white") +geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) -> pp_go

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") %>% select( -15, -16,-17,-18,-19,-20  ) -> tmp_env


tmp %>% arrange(Function) %>% dplyr::select(-Function) -> tmp2
# tmp %>% arrange(Function) %>% filter(Function=="Consumer") %>% dim() -> a 
# tmp %>% arrange(Function) %>% filter(Function=="Phototrophic") %>% dim() -> b 
# tmp %>% arrange(Function) %>% filter(Function=="Parasite") %>% dim() -> c 
library(iCAMP)
match.name(rn.list=list(a=t(tmp2),b=tmp_env)) -> sampid.check

mantel <- mantel_test(
  seed = 12345,
  spec = sampid.check$a, env = sampid.check$b,
  spec_select = list(Protist = 1:dim(tmp2)[1]), 
  #env_select =  as.list(setNames(1:ncol(tmp_env), names(tmp_env))),
  spec_dist =  dist_func(.FUN = "vegdist", method = "bray"), # 
  env_dist = dist_func(.FUN = "vegdist", method = "euclidean"),
  mantel_fun = 'mantel', # 
  na_omit=T,
)
mantel <- mutate(mantel, 
                 rd = cut(r, right = TRUE,# 
                          breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
                          labels = c('[0, 0.2]', '(0.2, 0.4]', '(0.4, 0.6]', '(0.6, 1]')
                 ),
                 pd = cut(p, right = FALSE,# 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                          labels = c('***', '**', '*', 'ns'),
                 )
)
mantel -> Go_mantel
#绘图
qcorrplot(correlate(tmp_env, method = "spearman"), type = "lower", diag = FALSE) +
  geom_square(color="white") +
  geom_couple(aes(colour = pd, size = rd), data = Go_mantel, 
              curvature = nice_curvature(-0.15)) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) +
  geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5)) +
  scale_colour_manual(values = c( "#419336" ,  "#605788" ,  "#CCCCCC99")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Spearman's r", order = 3))+ guides(fill='none', colour="none", size="none") -> Go_p

#gr
protist_table %>% dplyr::select(contains(c(".Gr", "Function")))  -> tmp #otu
read.table("factor-select17.csv",header=TRUE, row.names=1, sep = ",") %>% filter(grepl(".Gr" , rownames(.))) %>% dplyr::select(-clay, -Cu)-> env
env %>% merge(rich_fun, by.x = 0, by.y = 0) %>% merge(shan_fun,by.x = 'Row.names', by.y = 0) %>% dplyr::select(1:16,18) %>% 
  column_to_rownames('Row.names') -> tmp_env
colnames(tmp_env) <- c('Longitude','Latitude','Altitude','MAT','MAP','TP','Moisture','pH','EC','Ex_Ca','CEC','TN','TC','C/N','Richness','Shannon')
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Gr'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".Gr")))  %>%
  filter(Function=="Parasite") %>% melt() -> tm
tmp1$Parasite <- tm$value
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Gr'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select(-2,-4,-7) %>% dplyr::select(1,2,3,4)  -> da_gr2
colnames(da_gr2)[2:3] <- c("Phototrophic",'Consumer')
tmp_env %>% merge(da_gr2, by.x=0, by.y='variable') %>% column_to_rownames(var = 'Row.names') %>% dplyr::select(1:14, 18, 19, 17,15,16)-> tmp_env
tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("Shannon"))-> tmp_env
#tmp_env %>% merge(three_his_alpha,by.x=0, by.y="id", all.x=T ) %>% column_to_rownames('Row.names') %>% dplyr::select(!contains("ichness"))-> tmp_env
tmp_env %>% dplyr::select(1:15, 17,16, 19,20,21,18) -> tmp_env


tail_da <- read.csv("C:\\Users\\fengsw\\Desktop\\18s\\18s_ana\\PR2\\1.new_fig\\new_fig2\\gr.csv", header = 1, row.names = 1) %>% select( bac_rich, fun_rich)

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") -> tmppp
qcorrplot(correlate(tmppp, method = "spearman"), type = "full", diag = FALSE) +
  geom_square(color="white") +geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5))+
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) -> pp_gr

tmp_env %>% base::merge(tail_da, by = 0 ) %>% column_to_rownames("Row.names") %>% select( -15, -16,-17,-18,-19,-20  ) -> tmp_env


tmp %>% arrange(Function) %>% dplyr::select(-Function) -> tmp2
# tmp %>% arrange(Function) %>% filter(Function=="Consumer") %>% dim() -> a 
# tmp %>% arrange(Function) %>% filter(Function=="Phototrophic") %>% dim() -> b 
# tmp %>% arrange(Function) %>% filter(Function=="Parasite") %>% dim() -> c 
library(iCAMP)
match.name(rn.list=list(a=t(tmp2),b=tmp_env)) -> sampid.check

mantel <- mantel_test(
  seed = 12345,
  spec = sampid.check$a, env = sampid.check$b,
  spec_select = list(Protist = 1:dim(tmp2)[1]), 
  #env_select =  as.list(setNames(1:ncol(tmp_env), names(tmp_env))),
  spec_dist =  dist_func(.FUN = "vegdist", method = "bray"), # 
  env_dist = dist_func(.FUN = "vegdist", method = "euclidean"),
  mantel_fun = 'mantel', # mantel.partial：partial mantel te
  na_omit=T,
)
mantel <- mutate(mantel, 
                 rd = cut(r, right = TRUE,# 
                          breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
                          labels = c('[0, 0.2]', '(0.2, 0.4]', '(0.4, 0.6]', '(0.6, 1]')
                 ),
                 pd = cut(p, right = FALSE,# 
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                          labels = c('***', '**', '*', 'ns'),
                 )
)
mantel -> Gr_mantel
#
qcorrplot(correlate(tmp_env, method = "spearman"), type = "lower", diag = FALSE) +
  geom_square(color="white") +
  geom_couple(aes(colour = pd, size = rd), data = Gr_mantel, 
              curvature = nice_curvature(-0.15)) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlBu"))) +
  geom_mark(size = 5, only_mark = T,sig_level = c(0.05, 0.01, 0.001),  sig_thres = 0.05, colour = 'black')+
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5)) +
  scale_colour_manual(values = c( "#419336" ,  "#605788" ,  "#CCCCCC99")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "black"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Spearman's r", order = 3)) + guides(fill='none', colour="none", size="none") -> Gr_p

library(patchwork)
pp_fa + pp_fo + pp_gr + pp_go + pp_mi #18：24

##############################fig3##################################################

library(dplyr)
library(stringr)
library(randomForest)
library(rfPermute)
library(A3)
library(ggplot2)

da_18S %>% dplyr::select(-X.OTU.ID,-taxonomy) -> da_18S_div
colSums(da_18S_div) -> da_18S_div_sum
for (i in c(1:(dim(da_18S_div)[2]))){
  da_18S_div[, i] <- da_18S_div[, i]/da_18S_div_sum[i]*100 #relative abundance, 
}
da_18S_div <- log(da_18S_div+1)

da_18S_div %>% scale() %>% data.frame() ->da_18S_div_zscale #zcore re_abundance

sites_phyc2 <- read.csv("factor-select17.csv", header = T)
#cor(sites_phyc2[c(-1,-2,-3,-17)],sites_phyc2[c(-1,-2,-3,-17)]) #
sites_phyc2 %>% dplyr::select(-2,-3) %>% mutate(Habtats=str_replace(sites,".*.Fa.*", "Farmland"))%>% mutate(Habtats=str_replace(Habtats, ".*.Fo.*", "Forest"))%>% mutate(Habtats=str_replace(Habtats,".*.Go.*", "Gobi desert"))%>% 
  mutate(Habtats=str_replace(Habtats, ".*.Gr.*", "Grassland")) %>% mutate(Habtats=str_replace(Habtats,'.*T.*', "Mine wasteland")) %>% 
  mutate(Habtats=str_replace(Habtats,'.*\\.D.*', "Mine wasteland")) %>% dplyr::select(sites, MAP, MAT, Alt,Moisture, ph, EC, CEC, TN, TC, TP,Habtats) -> sites_phyc2
cluster_tree <- read.csv("tree_hab_perfer/97rami8116.clusters.txt", header = F, sep = c("\t"))
cluster_tree %>% dplyr::select(V2) %>% unlist(use.names = T) %>%  str_split(pattern = ":") -> cluster_tree_unlist 

RM_all4 <- NULL
RM_TOP1 <- NULL
for (i in 1:length(cluster_tree_unlist)){
  da_18S_div_zscale %>% filter(rownames(.) %in% cluster_tree_unlist[[i]]) %>% t() %>%  rowMeans() %>% data.frame(cluster_mean=.) %>% #filter(cluster_mean >0) %>% 
    merge(sites_phyc2, by.x = 0, by.y = "sites")  ->tmp1
  randomForest(cluster_mean~., data = tmp1[-1], importance = TRUE, ntree  = 999,na.action=na.omit) -> explain_vi 
  rfPermute(cluster_mean~., data = tmp1[-1], importance = TRUE, tree  = 999,num.cores = 10) %>% importance( type = 1) -> tmp3
  rbind(tmp3 %>% data.frame() %>% dplyr::select(1) %>% mutate(facters=rownames(.)) %>%as_tibble() %>%   t() %>% data.frame() %>%  mutate(clu=paste("cluster_",i, sep = "")),
        RM_TOP1 ) -> RM_TOP1
  
  rbind(RM_all4,cbind(cluster=i, R2=explain_vi$rsq[999]*100)) -> RM_all4
  
}

save(RM_all4, RM_TOP1,file= "C:\\Users\\fengsw\\Desktop\\环境偏好\\RM_all4_RM_TOP1.RData")
RM_all4 %>% data.frame() %>% mutate(cl="cluster") %>% tidyr::unite("cluster2",cl,cluster, sep = "_", remove = F)   %>% filter(R2 >= 20) -> RM_all5
RM_all5
#RM_all5 %>% write.csv("cluster_to_id.csv")
RM_TOP1 %>% data.frame() %>%  dplyr::select(X1,X2, clu) #

semi_cor_result <- NULL
library(ppcor)
for (i in RM_all5$cluster){
  da_18S_div_zscale %>% filter(rownames(.) %in% cluster_tree_unlist[[i]]) %>% t() %>%  rowMeans() %>% data.frame(cluster_mean=.) %>% 
    merge(sites_phyc2, by.x = 0, by.y = "sites") %>% dplyr::select(-1, -Habtats) -> tmp5 
  for (j in 2:dim(tmp5)[2]){
    spcor.test(tmp5[1], tmp5[j], tmp5[c(-1,-j)], method = "spearman") -> semi_cor
    rbind(semi_cor_result,cbind(claster=i,factors=colnames( tmp5[j]), ρ=semi_cor[1], p= semi_cor[2])) ->semi_cor_result
  }
}
semi_cor_result %>% data.frame() %>%  mutate(estimate=case_when(p.value > 0.05 ~ '0', TRUE~ as.character(estimate))) -> semi_cor_result2
semi_cor_result2$estimate  <- as.double(semi_cor_result2$estimate)
str(semi_cor_result2)
semi_cor_result2 %>% mutate(cluster = paste("cluster_", claster, sep = "")) %>% dplyr::select(5,2,3) %>% unique() %>% 
  tidyr::pivot_wider(names_from=factors , values_from=estimate) %>% data.frame()-> semi_cor_result3
rownames(semi_cor_result3) <- semi_cor_result3$cluster
#semi_cor_result3[semi_cor_result3 == 0] <- NA
semi_cor_result3[-1] -> semi_cor_result4  

########hclust
hc<-hclust(dist(semi_cor_result4, method = "euclidean"),method = "ward.D2")#
plot(hc)
library(factoextra)
library(cluster)
#
gap_stat <- clusGap(semi_cor_result4, FUN = hcut, nstart = 25, K.max = 95, B = 50)
#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
plot(hc)
rect.hclust(hc,k=51)

#
km.res <- kmeans(semi_cor_result4, 91, nstart = 25)
fviz_cluster(km.res, semi_cor_result4,labelsize = 1,show.clust.cent = F, shape = 21,pointsize = 4)+theme_bw()


groups <- cutree(hc, k=91)
groups
#
cbind(semi_cor_result4, cluster = groups) -> cluster_group
cluster_group %>% filter(cluster==1)

#############MAP
#+
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') -> tmpp
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ>0) %>% filter(factors=='MAP') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='MAP_high') -> tmp_maphight
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_map #filter(mean_abun_scale <4) %>%
#
cor.test(da_map$mean_abun_scale, da_map$MAP, method = "spearman")-> speaeman_re_map
ggplot(data = da_map, aes(y = mean_abun_scale , x = scale(MAP)))+
  geom_point(  size= 7, fill= '#253394', alpha= 0.8, color='black',shape=21)+
   theme_pubr(base_size = 20,border = F)+
  labs(x=paste('MAP' ,"(ρ = ", round(speaeman_re_map$estimate,3),", p = ",round(speaeman_re_map$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_map
#-
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ<0) %>% filter(factors=='MAP') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='MAP_low') -> tmp_maplow
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_map2 #filter(mean_abun_scale <4) %>%
#
cor.test(da_map2$mean_abun_scale, da_map2$MAP, method = "spearman")-> speaeman_re_map2
ggplot(data = da_map2, aes(y = mean_abun_scale , x = MAP))+
  geom_point(  size= 7, fill= '#9fa8da', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('MAP' ,"(ρ = ", round(speaeman_re_map2$estimate,3),", p = ",round(speaeman_re_map2$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_map2
pfe_map+pfe_map2

###MAT
#+
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ>0) %>% filter(factors=='MAT') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='Mat_high') -> tmp_mathight
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_mat #filter(mean_abun_scale <4) %>%
#
cor.test(da_mat$mean_abun_scale, da_mat$MAT, method = "spearman")-> speaeman_re_mat
ggplot(data = da_mat, aes(y = mean_abun_scale , x = scale(MAT)))+
  geom_point(  size= 7, fill= '#0fd397', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('MAT' ,"(ρ = ", round(speaeman_re_mat$estimate,3),", p = ",round(speaeman_re_mat$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_mat
#-
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ<0) %>% filter(factors=='MAT') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='MAT_low') -> tmp_matlow
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_mat2 #filter(mean_abun_scale <4) %>%
#
cor.test(da_mat2$mean_abun_scale, da_mat2$MAT, method = "spearman")-> speaeman_re_mat2
ggplot(data = da_mat2, aes(y = mean_abun_scale , x = MAT))+
  geom_point(  size= 7, fill= '#88fad6', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('MAT' ,"(ρ = ", round(speaeman_re_mat2$estimate,3),", p = ",round(speaeman_re_mat2$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_mat2
pfe_mat+pfe_mat2 # 7:15


###TP
#+
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ>0) %>% filter(factors=='TP') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='TP_high') -> tmp_tphight
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  filter(mean_abun_scale <2) %>% dplyr::select(-1, -Habtats) -> da_tp #filter(mean_abun_scale <4) %>%
#
cor.test(da_tp$mean_abun_scale, da_tp$TP, method = "pearson")-> speaeman_re_tp
ggplot(data = da_tp, aes(y = mean_abun_scale , x = scale(TP)))+
  geom_point(  size= 7, fill= '#55b8b1', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('TP' ,"(ρ = ", round(speaeman_re_tp$estimate,3),", p = ",round(speaeman_re_tp$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_tp
#-
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ<0) %>% filter(factors=='TP') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_tp2 #filter(mean_abun_scale <4) %>%
#
cor.test(da_tp2$mean_abun_scale, da_tp2$TP, method = "spearman")-> speaeman_re_tp2
ggplot(data = da_tp2, aes(y = mean_abun_scale , x = TP))+
  geom_point(  size= 7, fill= '#ffca06', alpha= 0.8, color='black',shape=21)+
   theme_pubr(base_size = 20,border = F)+
  labs(x=paste('TP' ,"(ρ = ", round(speaeman_re_tp2$estimate,3),", p = ",round(speaeman_re_tp2$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_tp2
pfe_tp+pfe_tp2 # 7:15



###Moisture
#+
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ>0) %>% filter(factors=='Moisture') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='moisture_high') -> tmp_moisturehigh
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_mo #filter(mean_abun_scale <4) %>%
#
cor.test(da_mo$mean_abun_scale, da_mo$Moisture, method = "spearman")-> speaeman_re_mo
ggplot(data = da_mo, aes(y = mean_abun_scale , x = scale(Moisture)))+
  geom_point(  size= 7, fill= '#eb6e46', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('Moisture' ,"(ρ = ", round(speaeman_re_mo$estimate,3),", p = ",round(speaeman_re_mo$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_mo
#-
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ<0) %>% filter(factors=='Moisture') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='moisture_low') -> tmp_moisturelow
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_mo2 #filter(mean_abun_scale <4) %>%
#
cor.test(da_mo2$mean_abun_scale, da_mo2$Moisture, method = "spearman")-> speaeman_re_mo2
ggplot(data = da_mo2, aes(y = mean_abun_scale , x = Moisture))+
  geom_point(  size= 7, fill= '#ffca06', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('Moisture' ,"(ρ = ", round(speaeman_re_mo2$estimate,3),", p = ",round(speaeman_re_mo2$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_mo2
pfe_mo+pfe_mo2 # 7:15

###ph
#+
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ>0) %>% filter(factors=='ph') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='ph_high') -> tmp_phhigh
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  filter(mean_abun_scale <2) %>%dplyr::select(-1, -Habtats) -> da_ph #filter(mean_abun_scale <4) %>%
#
cor.test(da_ph$mean_abun_scale, da_ph$ph, method = "spearman")-> speaeman_re_ph
ggplot(data = da_ph, aes(y = mean_abun_scale , x = scale(ph)))+
  geom_point(  size= 7, fill= '#aa00ff', alpha= 0.8, color='black',shape=21)+
   theme_pubr(base_size = 20,border = F)+
  labs(x=paste('pH' ,"(ρ = ", round(speaeman_re_ph$estimate,3),", p = ",round(speaeman_re_ph$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_ph
#-
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ<0) %>% filter(factors=='ph') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='ph_low') -> tmp_phlow
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  filter(mean_abun_scale <0.5) %>% dplyr::select(-1, -Habtats) -> da_ph2 #filter(mean_abun_scale <4) %>%
#
cor.test(da_ph2$mean_abun_scale, da_ph2$ph, method = "spearman")-> speaeman_re_ph2
ggplot(data = da_ph2, aes(y = mean_abun_scale , x = ph))+
  geom_point(  size= 7, fill= '#8393e8', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  #geom_smooth(method = 'lm', formula = y~x, se = TRUE, show.legend = FALSE, color = '#ff1744', alpha=0.2) + 
  #stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
  #  formula = y~x, parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('pH' ,"(ρ = ", round(speaeman_re_ph2$estimate,3),", p = ",round(speaeman_re_ph2$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_ph2
pfe_ph+pfe_ph2 # 7:15


###EC
#+
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ>0) %>% filter(factors=='EC') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='EC_high') -> tmp_echight
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  filter(EC <10) %>% dplyr::select(-1, -Habtats) -> da_ec #filter(mean_abun_scale <4) %>%
#
cor.test(da_ec$mean_abun_scale, da_ec$EC, method = "spearman")-> speaeman_re_ec
ggplot(data = da_ec, aes(y = mean_abun_scale , x = scale(EC)))+
  geom_point(  size= 7, fill= '#4e342e', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('EC' ,"(ρ = ", round(speaeman_re_ec$estimate,3),", p = ",round(speaeman_re_ec$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_ec
#-
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ<0) %>% filter(factors=='EC') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='EC_low') -> tmp_eclow
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  filter(EC <10) %>% dplyr::select(-1, -Habtats) -> da_ec2 #filter(mean_abun_scale <4) %>%
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='ec_low') -> tmp_eclow
#
cor.test(da_ec2$mean_abun_scale, da_ec2$EC, method = "spearman")-> speaeman_re_ec2
ggplot(data = da_ec2, aes(y = mean_abun_scale , x = EC))+
  geom_point(  size= 7, fill= '#bcaaa4', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('EC' ,"(ρ = ", round(speaeman_re_ec2$estimate,3),", p = ",round(speaeman_re_ec2$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_ec2
pfe_ec+pfe_ec2 # 7:15



###CEC
#+
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ>0) %>% filter(factors=='CEC') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='cec_high') -> tmp_cechigh
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) -> da_cec #filter(mean_abun_scale <4) %>%
#
cor.test(da_cec$mean_abun_scale, da_cec$CEC, method = "spearman")-> speaeman_re_cec
ggplot(data = da_cec, aes(y = mean_abun_scale , x = scale(CEC)))+
  geom_point(  size= 7, fill= '#ff5b9a', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('CEC' ,"(ρ = ", round(speaeman_re_cec$estimate,3),", p = ",round(speaeman_re_cec$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_cec
#-
R_p2 %>% filter(p< 0.05) %>% filter(factors!='mean_abun_scale') %>% filter(ρ< -0) %>% filter(factors=='CEC') %>% mutate(clade=str_replace(clade, "clades ", "")) %>% 
  dplyr::select(1) -> tmp6 #clades
#
cluster_group %>% filter(cluster %in% tmp6$clade) %>% rownames() %>% str_split_fixed(pattern = "_", 2) %>% data.frame() -> cluster_id_group_1 #cluster in tree
cluster_id_group_1$X2 <- as.double(cluster_id_group_1$X2)
cluster_group_asv <- NULL
for (i in cluster_id_group_1$X2){   
  append(cluster_group_asv, cluster_tree_unlist[[i]]) -> cluster_group_asv
}
cluster_group_asv %>%  data.frame(asv=.) %>% mutate(gro='CEC_low') -> tmp_ceclow
#
da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) %>% arrange(CEC) %>% dplyr::select(1) %>% head(30) %>% tail(15) -> rm_tmp

da_18S_div_zscale %>% filter(rownames(.) %in%cluster_group_asv) %>% colMeans() %>% data.frame(mean_abun_scale=.) %>%  merge(sites_phyc2, by.x = 0, by.y = "sites") %>% 
  dplyr::select(-1, -Habtats) %>% filter(!mean_abun_scale  %in% unlist(rm_tmp)) %>% filter(mean_abun_scale  > -0.02)-> da_cec2 #filter(mean_abun_scale <4) %>%
#
cor.test(da_cec2$mean_abun_scale, da_cec2$CEC, method = "spearman")-> speaeman_re_cec2
ggplot(data = da_cec2, aes(y = mean_abun_scale , x = CEC))+
  geom_point(  size= 7, fill= '#ffab91', alpha= 0.8, color='black',shape=21)+
  theme_pubr(base_size = 20,border = F)+
  labs(x=paste('CEC' ,"(ρ = ", round(speaeman_re_cec2$estimate,3),", p = ",round(speaeman_re_cec2$p.value,3),")" ,sep = ' '), y= "Cluster abundance") -> pfe_cec2
pfe_cec+pfe_cec2 # 7:15

#######################fig4###############################################
library(maptools)
library(sf)
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(rgdal)
library(ggpubr)
library(automap)
library(RColorBrewer)
library(gstat)
#fa
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fa'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fa'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho_fa1= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select( Long, Latitude,con_vs_pho_fa1 ) %>% group_by(Long, Latitude) %>% summarise(across(where(is.numeric), mean)) %>% filter(Latitude!=26.55) %>% 
  filter(Latitude!=28.21) %>% filter(Latitude!=41.11)  -> con_vs_pho_fa0
con_vs_pho_fa0 -> con_vs_pho_fa
data1<-SpatialPointsDataFrame(con_vs_pho_fa[,1:2],con_vs_pho_fa)
set.seed(11111)
aa <- autoKrige.cv(con_vs_pho_fa1~1,data1, nfold=10)
aa<-as.data.frame(aa$krige.cv_output)
cor.test(aa[,3],aa[,1],method="spearman")
plot(aa[,3],aa[,1])
#
china_map <- readOGR('D:\\china\\china-polygon.shp',stringsAsFactors=F,use_iconv = TRUE, encoding = "UTF-8")
#china_map <- readOGR('D:\\bou2_4p.shp',stringsAsFactors=F,use_iconv = TRUE,   encoding = "UTF-8")
crs(china_map) <- NA #
china_map_sf <-  st_as_sf(china_map) #
china_map_sf$name %>% unique()
china_map_sf %>% dplyr::filter(!name %in% c("台湾省", "新疆维吾尔自治区","西藏自治区", '四川省','海南省', "青海省")) -> china_map_sf_sel #
china_map_sf %>% dplyr::filter(name %in% c( "北京市", "天津市", "河北省", "山西省", "内蒙古自治区" , "辽宁省", "吉林省", "黑龙江省" ,"上海市",
                                            "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省", "湖北省", "湖南省",
                                            "广东省", "广西壮族自治区" , "重庆市",  "贵州省", "云南省","陕西省",
                                            "甘肃省",  "宁夏回族自治区"  , "香港特别行政区", "澳门特别行政区" )) -> china_map_sf_sel

china_map_sell <- as(china_map_sf_sel, "Spatial") 
fortify(china_map_sell) 
grid=makegrid(china_map,n=250000 )#
colnames(grid)<- c('Long','Latitude')
sp::coordinates(con_vs_pho_fa) = ~Long+Latitude
sp::coordinates(grid) <- ~Long+Latitude
sp::gridded(grid) <- T
province.grid=grid[china_map_sell,] #
grid<-province.grid
#
kriging_result = autoKrige(con_vs_pho_fa1~1,con_vs_pho_fa, grid)
prediction_spdf = kriging_result$krige_output
a<-coordinates(prediction_spdf)
df2fa<-as.data.frame(cbind(a,value=prediction_spdf$var1.pred))
color<-brewer.pal(11, "RdYlBu")[11:1]
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray50")+
  #geom_point(data=df2fa,aes(Long,Latitude,color=value),size=0.001)+
  geom_point(data=df2fa,aes(Long, Latitude,color=value),size =0.0001)+ #
  scale_color_gradientn(colours  = color, limits=c(0,6), breaks = c(0,2,4,6)) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.502, p<0.05)", y="Longtitud", title = 'Farmland (Consumer/Phototrophic)') -> hop1
hop1#6:8
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray50")+
  geom_point(data=con_vs_pho_fa0,aes(Long,Latitude,color=con_vs_pho_fa1),size=5)+
  scale_color_gradientn(colours  = color, limits=c(0,8)) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.502, p<0.05)", y="Longtitud", title = 'Farmland (Consumer/Phototrophic)')
#fo
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fo'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Fo'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho_fo1= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select( Long, Latitude,con_vs_pho_fo1 ) %>% group_by(Long, Latitude) %>% summarise(across(where(is.numeric), mean)) %>% filter(Latitude!=41.09) %>% 
  filter(Latitude!=29.84) %>% filter(Latitude!=0)  -> con_vs_pho_fo
data1<-SpatialPointsDataFrame(con_vs_pho_fo[,1:2],con_vs_pho_fo)
set.seed(131415)
aa <- autoKrige.cv(con_vs_pho_fo1~1,data1, nfold=10)
aa<-as.data.frame(aa$krige.cv_output)
cor.test(aa[,3],aa[,1],method="spearman")
plot(aa[,3],aa[,1])
#
china_map <- readOGR('D:\\china\\china-polygon.shp',stringsAsFactors=F,use_iconv = TRUE, encoding = "UTF-8")
#china_map <- readOGR('D:\\bou2_4p.shp',stringsAsFactors=F,use_iconv = TRUE,   encoding = "UTF-8")
crs(china_map) <- NA #
china_map_sf <-  st_as_sf(china_map) #
china_map_sf$name %>% unique()
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray")+
  geom_point(data=con_vs_pho_fo,aes(Long,Latitude,color=con_vs_pho_fo1),size=5)+
  scale_color_gradientn(colours  = color, limits=c(0,9)) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.464, p<0.05)", y="Longtitud", title = 'Farmland (Consumer/Phototrophic)')
china_map_sf %>% dplyr::filter(!name %in% c("台湾省", "新疆维吾尔自治区","西藏自治区", '海南省', "内蒙古自治区", "四川省")) -> china_map_sf_sel #
china_map_sf %>% dplyr::filter(name %in% c( "北京市", "天津市", "河北省", "山西省",  "辽宁省", "吉林省", "黑龙江省" ,"上海市",
                                            "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省", "湖北省", "湖南省",
                                            "广东省", "广西壮族自治区" , "重庆市", "贵州省", "云南省","陕西省",
                                            "甘肃省", "青海省", "宁夏回族自治区" , "香港特别行政区", "澳门特别行政区" )) -> china_map_sf_sel
china_map_sell <- as(china_map_sf_sel, "Spatial") #
fortify(china_map_sell) -> aaa
grid=makegrid(china_map,n=250000 )#
colnames(grid)<- c('Long','Latitude')
sp::coordinates(con_vs_pho_fo) = ~Long+Latitude
sp::coordinates(grid) <- ~Long+Latitude
sp::gridded(grid) <- T
province.grid=grid[china_map_sell,] #
grid<-province.grid
#
kriging_result = autoKrige(con_vs_pho_fo1~1,con_vs_pho_fo, grid)
prediction_spdf = kriging_result$krige_output
a<-coordinates(prediction_spdf)
df2fo<-as.data.frame(cbind(a,value=prediction_spdf$var1.pred))
color<-brewer.pal(11, "RdYlBu")[11:1]
ggplot()+ #
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray50")+
  #geom_point(data=df2fa,aes(Long,Latitude,color=value),size=0.001)+
  geom_point(data=df2fo,aes(Long, Latitude,color=value),size =0.0001)+
  scale_color_gradientn(colours  = color, limits=c(2,8.15), breaks = c(2,4,6, 8)) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.464, p<0.05)", y="Longtitud", title = 'Forest (Consumer/Phototrophic)') -> hop2
hop2#6:8
#gr
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Gr'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Gr'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho_gr1= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select( Long, Latitude,con_vs_pho_gr1 ) %>% group_by(Long, Latitude) %>% summarise(across(where(is.numeric), mean)) %>% filter(Latitude!=0) %>% 
  filter(Latitude!=0) %>% filter(Latitude!=0)  -> con_vs_pho_gr
data1<-SpatialPointsDataFrame(con_vs_pho_gr[,1:2],con_vs_pho_gr)
set.seed(11111)
aa <- autoKrige.cv(con_vs_pho_gr1~Long+Latitude,data1, nfold=10)
aa<-as.data.frame(aa$krige.cv_output)
cor.test(aa[,3],aa[,1],method="spearman")
plot(aa[,3],aa[,1])
#
china_map <- readOGR('D:\\china\\china-polygon.shp',stringsAsFactors=F,use_iconv = TRUE, encoding = "UTF-8")
#china_map <- readOGR('D:\\bou2_4p.shp',stringsAsFactors=F,use_iconv = TRUE,   encoding = "UTF-8")
crs(china_map) <- NA #
china_map_sf <-  st_as_sf(china_map) #
china_map_sf$name %>% unique()
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray")+
  geom_point(data=con_vs_pho_gr,aes(Long,Latitude,color=con_vs_pho_gr1),size=5)+
  scale_color_gradientn(colours  = color) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.502, p<0.05)", y="Longtitud", title = 'Farmland (Consumer/Phototrophic)')
china_map_sf %>% dplyr::filter(name %in% c("河北省", "新疆维吾尔自治区","西藏自治区",  "内蒙古自治区","山西省","四川省", "青海省","北京市" , "天津市" ,"甘肃省", "宁夏回族自治区", "陕西省")) -> china_map_sf_sel #
china_map_sell <- as(china_map_sf_sel, "Spatial") #
grid=makegrid(china_map,n=400000 )#
colnames(grid)<- c('Long','Latitude')
sp::coordinates(con_vs_pho_gr) = ~Long+Latitude
sp::coordinates(grid) <- ~Long+Latitude
sp::gridded(grid) <- T
province.grid=grid[china_map_sell,] #
grid<-province.grid
#
kriging_result = autoKrige(con_vs_pho_gr1~Long+Latitude,con_vs_pho_gr, grid)
prediction_spdf = kriging_result$krige_output
a<-coordinates(prediction_spdf)
df2gr<-as.data.frame(cbind(a,value=prediction_spdf$var1.pred))
df2gr$value %>% min()
color1<-brewer.pal(11, "RdYlBu")[11:1]
ggplot()+ #
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray50")+
  geom_point(data=df2gr,aes(Long, Latitude,color=value),size =0.0001)+
  scale_color_gradientn(colours  = color1, limits=c(-0,11.2), breaks = c(0,2.5,5,7.5,10), na.value = "#323996") +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.833, p<0.05)", y="Longtitud", title = 'Grassland (Consumer/Phototrophic)') -> hop3
hop3#6:8
#go
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Go'))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains('.Go'))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho_go1= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  filter(!variable %in%c("GS.Go5", "XJ.Go15",'XJ.Go10','QH.Go7')) %>% 
  dplyr::select( Long, Latitude,con_vs_pho_go1 ) %>% group_by(Long, Latitude) %>% summarise(across(where(is.numeric), mean)) %>% filter(Latitude!=0) %>% 
  filter(Latitude!=0) %>% filter(Latitude!=0)  -> con_vs_pho_go
data1 <- SpatialPointsDataFrame(con_vs_pho_go[,1:2],con_vs_pho_go)
set.seed(11111)
aa <- autoKrige.cv(con_vs_pho_go1~Latitude,data1, nfold=10)
aa<-as.data.frame(aa$krige.cv_output)
cor.test(aa[,3],aa[,1],method="spearman")
plot(aa[,3],aa[,1])

#
china_map <- readOGR('D:\\china\\china-polygon.shp',stringsAsFactors=F,use_iconv = TRUE, encoding = "UTF-8")
#china_map <- readOGR('D:\\bou2_4p.shp',stringsAsFactors=F,use_iconv = TRUE,   encoding = "UTF-8")
crs(china_map) <- NA #
china_map_sf <-  st_as_sf(china_map) #
china_map_sf$name %>% unique()
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray")+
  geom_point(data=con_vs_pho_go,aes(Long,Latitude,color=con_vs_pho_go1),size=5)+
  scale_color_gradientn(colours  = color) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r = 0.502, p < 0.05)", y="Longtitud", title = 'Farmland (Consumer/Phototrophic)')
china_map_sf %>% dplyr::filter(name %in% c("新疆维吾尔自治区", "青海省", "甘肃省")) -> china_map_sf_sel #
china_map_sell <- as(china_map_sf_sel, "Spatial") #
fortify(china_map_sell) -> aaa
grid=makegrid(china_map,n=400000 )#
colnames(grid)<- c('Long','Latitude')
sp::coordinates(con_vs_pho_go) = ~Long+Latitude
sp::coordinates(grid) <- ~Long+Latitude
sp::gridded(grid) <- T
province.grid=grid[china_map_sell,] #
grid<-province.grid
#
kriging_result = autoKrige(con_vs_pho_go1~Latitude,con_vs_pho_go, grid)
prediction_spdf = kriging_result$krige_output
a<-coordinates(prediction_spdf)
df2go<-as.data.frame(cbind(a,value=prediction_spdf$var1.pred))
df2go$value %>% max()
color<-brewer.pal(11, "RdYlBu")[11:1]
ggplot()+ #
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray50")+
  geom_point(data=df2go,aes(Long, Latitude,color=value),size =0.0001)+
  scale_color_gradientn(colours  = color, limits=c(-30, 120), breaks = c(0, 40, 80, 120)) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r = 1, p = 0.08)", y="Longtitud", title = 'Gobi desert (Consumer/Phototrophic)') -> hop4
hop4#6:8

#mine
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".T" ,".D")))  %>%
  filter(Function=="Consumer") %>% melt() -> tmp1
protist_Class_arrange_rela %>% group_by(Function) %>% summarise(across(where(is.numeric), sum)) %>% dplyr::select(Function, contains(c(".T" ,".D")))  %>%
  filter(Function=="Phototrophic") %>% melt() %>% merge(tmp1, by = "variable") %>% mutate(con_vs_pho_mi1= value.y/value.x) %>% merge(sites_phyc, by.x = 'variable', by.y = "sites") %>% 
  dplyr::select( Long, Latitude,con_vs_pho_mi1) %>% group_by(Long, Latitude) %>% summarise(across(where(is.numeric), mean)) %>% filter(Latitude!=37.29) %>% 
  filter(Latitude!=0) %>% filter(Latitude!=29.75) %>% filter(Latitude!=32.17)  %>% filter(con_vs_pho_mi1<300)-> con_vs_pho_mi
data1 <- SpatialPointsDataFrame(con_vs_pho_mi[,c(1:2)],con_vs_pho_mi)

set.seed(10002)
aa <- autoKrige.cv(con_vs_pho_mi1~1,data1, nfold=10,fix.values = c(500,50,NA))
aa<-as.data.frame(aa$krige.cv_output)
cor.test(aa[,3],aa[,1],method="spearman")
plot(aa[,3],aa[,1])

#
china_map <- readOGR('D:\\china\\china-polygon.shp',stringsAsFactors=F,use_iconv = TRUE, encoding = "UTF-8")
#china_map <- readOGR('D:\\bou2_4p.shp',stringsAsFactors=F,use_iconv = TRUE,   encoding = "UTF-8")
crs(china_map) <- NA #
china_map_sf <-  st_as_sf(china_map) #
china_map_sf$name %>% unique()
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray")+
  geom_point(data=con_vs_pho_mi,aes(Long,Latitude,color=con_vs_pho_mi1),size=5)+
  scale_color_gradientn(colours  = color) +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.502, p<0.05)", y="Longtitud", title = 'Farmland (Consumer/Phototrophic)')

china_map_sf %>% dplyr::filter(!name %in% c("台湾省","西藏自治区", '海南省',"")) -> china_map_sf_sel #
china_map_sf %>% dplyr::filter(name %in% c( "北京市", "天津市", "河北省", "山西省", "内蒙古自治区" , "辽宁省", "吉林省", "黑龙江省" ,"上海市",
                                            "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省", "湖北省", "湖南省",
                                            "广东省", "广西壮族自治区" , "重庆市", "四川省", "贵州省", "云南省","陕西省",
                                            "甘肃省", "青海省", "宁夏回族自治区"  , "新疆维吾尔自治区", "香港特别行政区", "澳门特别行政区" )) -> china_map_sf_sel #

china_map_sell <- as(china_map_sf_sel, "Spatial") #
fortify(china_map_sell) -> aaa

grid=makegrid(china_map,n=4000000 )#
colnames(grid)<- c('Long','Latitude')
sp::coordinates(con_vs_pho_mi) = ~Long+Latitude
sp::coordinates(grid) <- ~Long+Latitude
sp::gridded(grid) <- T
province.grid=grid[china_map_sell,] #
grid<-province.grid#
kriging_result = autoKrige(con_vs_pho_mi1~1,con_vs_pho_mi, province.grid,fix.values = c(500,50,NA))
prediction_spdf = kriging_result$krige_output
a<-coordinates(prediction_spdf)
df2m<-as.data.frame(cbind(a,value=prediction_spdf$var1.pred))
df2m$value %>% max()
color<-brewer.pal(11, "RdYlBu")[10:1]
ggplot()+ #
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group) ,fill="NA",colour="gray50")+
  geom_point(data=df2m,aes(Long, Latitude,color=value))+
  scale_color_gradientn(colours  = color, limits=c(-2.2, 150), breaks = c(0, 50, 100, 150), na.value = 'gray') +
  coord_map()+
  theme_pubr(border = T, legend = "right")+xlim(c(70, 140))+
  theme(panel.grid=element_blank())+ labs(x="Latitude (CV: r=0.429, p<0.05)", y="Longtitud", title = 'Mine wasteland (Consumer/Phototrophic)') -> hop5
hop5#6:8

library(patchwork)
hop1+hop2+hop4+hop3+hop5#12:18

#######################fig5###############################################
#5a/e/i
ggplot(com_habs_sto_each) +
  #geom_violin(aes(x = hab, y = Selection, fill = hab) )+
  #stat_summary(aes(x = hab, y = Selection, fill = hab), fun=mean, geom = "point", size=5,shape=21)+
  stat_summary(aes(x = hab, y = Selection,color=hab, fill = hab),fun.data = "mean_sdl",fun.args = list(mult = 1), shape=21,geom = "pointrange", size=2)+
  stat_compare_means(aes(x = hab, y = Selection),label.y.npc =0.5 )+
  scale_fill_manual(values = c('#656cbc','#d26272','#57ad2e','#ec844b','#6a5468')) +
  scale_color_manual(values = c('#656cbc','#d26272','#57ad2e','#ec844b','#6a5468')) +
  theme_pubr(border = F, base_size = 18,legend = "none") +labs(x=NULL)-> A
com_habs_sto_each  %>% group_by(hab) %>% summarise(mean(Selection))
compare_means(Selection ~ hab,  data = com_habs_sto_each, method = "wilcox.test")
ggplot(com_habs_sto_each) +
  stat_summary(aes(x = hab, y = Dispersal,color=hab, fill = hab),fun.data = "mean_sdl",fun.args = list(mult = 1), shape=21,geom = "pointrange", size=2)+
  stat_compare_means(aes(x = hab, y = Dispersal),label.y.npc =0.5 )+
  scale_fill_manual(values = c('#656cbc','#d26272','#57ad2e','#ec844b','#6a5468')) +
  scale_color_manual(values = c('#656cbc','#d26272','#57ad2e','#ec844b','#6a5468')) +
  theme_pubr(border = F, base_size = 18,legend = "none")+labs(x=NULL) -> B
com_habs_sto_each  %>% group_by(hab) %>% summarise(mean(Dispersal))
compare_means(Dispersal ~ hab,  data = com_habs_sto_each, method = "wilcox.test")
ggplot(com_habs_sto_each) +
  #geom_violin(aes(x = hab, y = Drift, fill = hab) )+
  #ggplot(com_habs_sto_each) +
  stat_summary(aes(x = hab, y = Drift,color=hab, fill = hab),fun.data = "mean_sdl",fun.args = list(mult = 1), shape=21,geom = "pointrange", size=2)+
  stat_compare_means(aes(x = hab, y = Drift),label.y.npc =0.5 )+
  scale_fill_manual(values = c('#656cbc','#d26272','#57ad2e','#ec844b','#6a5468')) +
  scale_color_manual(values = c('#656cbc','#d26272','#57ad2e','#ec844b','#6a5468')) +
  theme_pubr(border = F,base_size = 18, legend = "none")+labs(x=NULL) ->C
com_habs_sto_each  %>% group_by(hab) %>% summarise(mean(Drift))
compare_means(Drift ~ hab,  data = com_habs_sto_each, method = "wilcox.test")
A/B/C  # 8:10

#5b/f/j
aaa %>% ggplot(aes(x = hab, y = value, color = hab))  +
  geom_boxplot( shape = "circle", outlier.shape = NA, size=1, alpha=0.75) +
  geom_jitter( width = 0.15, alpha=0.45,  shape=1, size=3) +
  scale_color_manual(values = c(Farmland='#656cbc',Forest='#d26272',Grass='#57ad2e',Gobi='#ec844b',Minewasteland='#6a5468')) +stat_compare_means()+
  theme_pubr(base_size = 18,legend = "none") -> str_selb
bbb %>% ggplot(aes(x = hab, y = num, color = hab)) %>%  +
  geom_boxplot( shape = "circle", outlier.shape = NA, size=1, alpha=0.75) +
  geom_jitter( width = 0.15, alpha=0.45,  shape=1, size=3) +
  scale_color_manual(values = c(Farmland='#656cbc',Forest='#d26272',Grass='#57ad2e',Gobi='#ec844b',Minewasteland='#6a5468')) +stat_compare_means()+
  theme_pubr(base_size = 18,legend = "none") -> str_self
ccc %>% ggplot(aes(x = hab, y = num, color = hab)) %>%  +
  geom_boxplot( shape = "circle", outlier.shape = NA, size=1, alpha=0.75) +
  geom_jitter( width = 0.15, alpha=0.45,  shape=1, size=3) +
  scale_color_manual(values = c(Farmland='#656cbc',Forest='#d26272',Grass='#57ad2e',Gobi='#ec844b',Minewasteland='#6a5468')) +stat_compare_means()+
  theme_pubr(base_size = 18,legend = "none") -> str_selj

#5c/g/k
abun_sel %>% ggplot(aes(x = hab, y = value, color = hab))  +
  geom_boxplot( shape = "circle", outlier.shape = NA, size=1, alpha=0.75) +
  geom_jitter( width = 0.15, alpha=0.45,  shape=1, size=3) +
  scale_color_manual(values = c(Farmland='#656cbc',Forest='#d26272',Grass='#57ad2e',Gobi='#ec844b',Minewasteland='#6a5468')) +stat_compare_means()+
  theme_pubr(base_size = 18,legend = "none") -> str_selc

abun_dis %>% ggplot(aes(x = hab, y = value, color = hab))  +
  geom_boxplot( shape = "circle", outlier.shape = NA, size=1, alpha=0.75) +
  geom_jitter( width = 0.15, alpha=0.45,  shape=1, size=3) +
  scale_color_manual(values = c(Farmland='#656cbc',Forest='#d26272',Grass='#57ad2e',Gobi='#ec844b',Minewasteland='#6a5468')) +stat_compare_means()+
  theme_pubr(base_size = 18,legend = "none") -> str_selg

abun_drift %>% ggplot(aes(x = hab, y = value, color = hab))  +
  geom_boxplot( shape = "circle", outlier.shape = NA, size=1, alpha=0.75) +
  geom_jitter( width = 0.15, alpha=0.45,  shape=1, size=3) +
  scale_color_manual(values = c(Farmland='#656cbc',Forest='#d26272',Grass='#57ad2e',Gobi='#ec844b',Minewasteland='#6a5468')) +stat_compare_means()+
  theme_pubr(base_size = 18,legend = "none") -> str_selk

#5d/h/l
rbind(
  tmpfaa1 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n()) %>% mutate(hab="Farmland"),
  tmpfoo1 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Forest"),
  tmpgrr1 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Grassland"),
  tmpgoo1 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Gobi desert"),
  tmptaa1 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Mine wasteland")
) -> bbb
bbb$hab <- factor(bbb$hab, levels=c('Farmland','Forest','Grassland','Gobi desert','Mine wasteland'))
ggplot(bbb) +
  aes(x = hab, fill = Function, weight = N) +
  geom_bar(position = "fill") +
  scale_fill_manual(values  = c(Consumer = "#3685D2", Parasite = "#57AC5E", Phototrophic = "#FE9126", Unknown = "#F06D69")) +
  theme_pubr(base_size = 18,legend = "none") +labs(x=NULL, y="Function composion %")

rbind(
  tmpfaa2 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n()) %>% mutate(hab="Farmland"),
  tmpfoo2 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Forest"),
  tmpgrr2 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Grassland"),
  tmpgoo2 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Gobi desert"),
  tmptaa2 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Mine wasteland")
) -> bbb
bbb$hab <- factor(bbb$hab, levels=c('Farmland','Forest','Grassland','Gobi desert','Mine wasteland'))
ggplot(bbb) +
  aes(x = hab, fill = Function, weight = N) +
  geom_bar(position = "fill") +
  scale_fill_manual(values  = c(Consumer = "#3685D2", Parasite = "#57AC5E", Phototrophic = "#FE9126", Unknown = "#F06D69")) +
  theme_pubr(base_size = 18,legend = "none") +labs(x=NULL, y="Function composion %")

rbind(
  tmpfaa3 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n()) %>% mutate(hab="Farmland"),
  tmpfoo3 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Forest"),
  tmpgrr3 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Grassland"),
  tmpgoo3 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Gobi desert"),
  tmptaa3 %>% select(ID) %>% left_join(read.csv("bNTI_protist/id_history.csv"), by = c("ID"="id")) %>% group_by(Function) %>% summarise(N=n())%>% mutate(hab="Mine wasteland")
) -> bbb
bbb$hab <- factor(bbb$hab, levels=c('Farmland','Forest','Grassland','Gobi desert','Mine wasteland'))
ggplot(bbb) +
  aes(x = hab, fill = Function, weight = N) +
  geom_bar(position = "fill") +
  scale_fill_manual(values  = c(Consumer = "#3685D2", Parasite = "#57AC5E", Phototrophic = "#FE9126", Unknown = "#F06D69")) +
  theme_pubr(base_size = 18,legend = "none") +labs(x=NULL, y="Function composion %")

































