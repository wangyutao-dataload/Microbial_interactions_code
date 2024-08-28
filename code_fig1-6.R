##############Figure 1########################
da <- read.csv("diversity.csv") %>% select(2,3 ,4,34,35)
da
da$Ecosystem <- factor(da$Ecosystem, levels = c('Natural','Agricultural','Tailings'))
library(ggplot2)

#1b
ggplot(da) +
 aes(x = Ecosystem, y = B_richness, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base() -> B_rich
ggplot(da) +
  aes(x = Ecosystem, y = F_richness, fill = Ecosystem) +
  geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
  geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  ggthemes::theme_base() -> F_rich
B_rich/F_rich

#1c
ggplot(da) +
  aes(x = Ecosystem, y = B_biomass , fill = Ecosystem) +
  geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
  geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_hue(direction = 1) +
  ggthemes::theme_base() -> B_bio
ggplot(da) +
  aes(x = Ecosystem, y = F_biomass , fill = Ecosystem) +
  geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
  geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  ggthemes::theme_base() -> F_bio
B_bio/F_bio

#1d
da %>% ggplot()+
  geom_point(aes(y = B_richness  ,x =  B_biomass  ,fill = Ecosystem , color = Ecosystem ), size =3,alpha = 0.9 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = B_richness ,x = B_biomass) ,se=FALSE ,size = 1, color="black",
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = B_richness ,x = B_biomass,label = paste(..rr.label.., stat(p.value.label), AIC.label, sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'left', 
               label.y.npc = 'top', size = 5) +
  geom_smooth(aes(y = B_richness ,x = B_biomass ),method = 'lm', formula = y~poly(x, 2), se = F, show.legend = FALSE) +
  stat_poly_eq(aes(y = B_richness ,x = B_biomass, label = paste( ..rr.label.., stat(p.value.label),AIC.label, sep = '~`,`~')),
               formula = y~poly(x, 2), parse = TRUE, label.x.npc = 'right', label.y.npc = 'bottom', size = 5)+
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  theme_pubr(base_size = 10, border = T, base_family = "serif" )+ theme(strip.text.x = element_blank())  -> B

#1e
da %>% ggplot()+
  geom_point(aes(y = F_richness  ,x =  F_biomass  ,fill = Ecosystem , color = Ecosystem ), size =3,alpha = 0.9 , stroke = 1.5 ,shape = 21)+
  geom_smooth(aes(y = F_richness ,x = F_biomass ) ,se=FALSE ,size = 1, color="black",
              method = 'lm' , formula=y~x)+
  stat_poly_eq(aes(y = F_richness ,x = F_biomass, label = paste(..rr.label.., stat(p.value.label), AIC.label, sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'left', 
               label.y.npc = 'top', size = 5) +
  geom_smooth(aes(y = F_richness ,x = F_biomass),method = 'lm', formula = y~poly(x, 2), se = F, show.legend = FALSE) +
  stat_poly_eq(aes(y = F_richness ,x = F_biomass, label = paste( ..rr.label.., stat(p.value.label),AIC.label, sep = '~`,`~')),
               formula = y~poly(x, 2), parse = TRUE, label.x.npc = 'right', label.y.npc = 'bottom', size = 5)+
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  theme_pubr(base_size = 10, border = T, base_family = "serif" )+ theme(strip.text.x = element_blank()) -> Fu
##############Figure 2########################
da <- read.csv("net.csv") 
da
da$Ecosystem <- factor(da$Ecosystem, levels = c('Nat.','Agr.','Min.'))
library(ggplot2)

#2f
ggplot(da) +
 aes(x = Ecosystem, y = AvgK_Negative, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
#2g
ggplot(da) +
 aes(x = Ecosystem, y = ARG, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
#2h
ggplot(da) +
 aes(x = Ecosystem, y = eCIS, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
#2i
ggplot(da) +
 aes(x = Ecosystem, y = T6SS, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
#2j
ggplot(da) +
 aes(x = Ecosystem, y = AvgK_postive, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
#2k
ggplot(da) +
 aes(x = Ecosystem, y = CEA, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
#2l
ggplot(da) +
 aes(x = Ecosystem, y = NEA, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
#2m
ggplot(da) +
 aes(x = Ecosystem, y = pEA, fill = Ecosystem) +
 geom_boxplot(shape = "circle", alpha=0.5, outlier.shape = NA) +
 geom_jitter(shape = "circle", width = 0.15, aes(color = Ecosystem)) +
  scale_fill_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
  scale_color_manual(values = c('#72bf6f','#e84848','#5f97c6')) +
 ggthemes::theme_base()
 
##############Figure 3########################
#RM forest
library(randomForest)
#install.packages(c('rfPermute' ,'A3'))
library(rfPermute)
library(A3)
library(ggplot2)

rfPermute(BF_richness~., data = Na_da, na.action = na.roughfix) %>% importance( type = 1) %>% data.frame() -> Na_RF_result
Na_RF_result2 <- cbind(Na_RF_result,row.names(Na_RF_result))
Na_RF_result2$`row.names(Na_RF_result)` <- factor(Na_RF_result2$`row.names(Na_RF_result)` ,level = Na_RF_result2$`row.names(Na_RF_result)`)

Na_RF_result2 %>% mutate(sig = case_when(X.IncMSE.pval <= 0.001 ~'***' , 
                                         (X.IncMSE.pval > 0.001 & X.IncMSE.pval <= 0.01) ~'**', 
                                         (X.IncMSE.pval > 0.01 & X.IncMSE.pval <= 0.05) ~'*')) %>% merge(types, by = 0) %>% #
  ggplot() +
  aes(x = `row.names(Na_RF_result)`,fill = types , weight = X.IncMSE) +
  geom_bar(color = 'black',alpha = 0.85 ,width = 0.8) +
  # scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0,20)) +
  theme_test(base_size = 20 ) + guides(fill = FALSE)+
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), panel.background = element_rect(fill = "white")) +
  labs(x = "", y = paste("Increased in MSE (%) r2 = ", round(rr$rsq[500], digits = 3))) +
  scale_fill_manual(values = c('#b2ebf2','#ffccbc','#d1c4e9','#3b5ca5'))+
  # theme(axis.title = element_text(size = 30, vjust = 1)) + 
  #theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0)) +
  labs(x = NULL) +
  geom_text(aes(x= `row.names(Na_RF_result)`, y = X.IncMSE+1,label = sig), color = 'black' ,size =10)  -> P_Nu

P_Nu

rfPermute(BF_richness~., data = Ag_da, na.action = na.roughfix) %>% importance( type = 1) %>% data.frame() -> Ag_RF_result
Ag_RF_result2 <- cbind(Ag_RF_result,row.names(Ag_RF_result))
Ag_RF_result2$`row.names(Ag_RF_result)` <- factor(Ag_RF_result2$`row.names(Ag_RF_result)` ,level = Ag_RF_result2$`row.names(Ag_RF_result)`)

Ag_RF_result2 %>% mutate(sig = case_when(X.IncMSE.pval <= 0.001 ~'***' , 
                                         (X.IncMSE.pval > 0.001 & X.IncMSE.pval <= 0.01) ~'**', 
                                         (X.IncMSE.pval > 0.01 & X.IncMSE.pval <= 0.05) ~'*')) %>% merge(types, by = 0) %>% #
  ggplot() +
  aes(x = `row.names(Ag_RF_result)`,fill = types , weight = X.IncMSE) +
  geom_bar(color = 'black',alpha = 0.85 ,width = 0.8) +
  # scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0,20)) +
  theme_test(base_size = 20 ) +  guides(fill = FALSE)+
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), panel.background = element_rect(fill = "white")) +
  labs(x = "", y = paste("Increased in MSE (%) r2 = ", round(rr$rsq[500], digits = 3))) +
  scale_fill_manual(values = c('#b2ebf2','#ffccbc','#d1c4e9','#3b5ca5'))+
  # theme(axis.title = element_text(size = 30, vjust = 1)) + 
  #theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0)) +
  labs(x = NULL) +
  geom_text(aes(x= `row.names(Ag_RF_result)`, y = X.IncMSE+1,label = sig), color = 'black' ,size =10)   -> P_Ag
P_Ag

rfPermute(BF_richness~., data = Ta_da, na.action = na.roughfix) %>% importance( type = 1) %>% data.frame() -> Ta_RF_result
Ta_RF_result2 <- cbind(Ta_RF_result,row.names(Ta_RF_result))
Ta_RF_result2$`row.names(Ta_RF_result)` <- factor(Ta_RF_result2$`row.names(Ta_RF_result)` ,level = Ta_RF_result2$`row.names(Ta_RF_result)`)

Ta_RF_result2 %>% mutate(sig = case_when(X.IncMSE.pval <= 0.001 ~'***' , 
                                         (X.IncMSE.pval > 0.001 & X.IncMSE.pval <= 0.01) ~'**', 
                                         (X.IncMSE.pval > 0.01 & X.IncMSE.pval <= 0.05) ~'*')) %>% merge(types, by = 0) %>% #
  ggplot() +
  aes(x = `row.names(Ta_RF_result)`,fill = types , weight = X.IncMSE) +
  geom_bar(color = 'black',alpha = 0.85 ,width = 0.8) +
  # scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0,20)) +
  theme_test(base_size = 20 ) + 
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), panel.background = element_rect(fill = "white")) +
  labs(x = "", y = paste("Increased in MSE (%) r2 = ", round(rr$rsq[500], digits = 3))) +
  scale_fill_manual(values = c('#b2ebf2','#ffccbc','#d1c4e9','#3b5ca5'))+ guides(fill = FALSE)+
  # theme(axis.title = element_text(size = 30, vjust = 1)) + 
  #theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0)) +
  labs(x = NULL) +
  geom_text(aes(x= `row.names(Ta_RF_result)`, y = X.IncMSE+1,label = sig), color = 'black' ,size =10)   -> P_Ta
P_Ta

#point
library(ggpmisc)

Na_da %>% ggplot( aes(y = BF_richness , x = ARG_abundance/1000))+
  geom_point(  size= 5, fill= '#548aae', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "ARG (×103)"), y = "Microbial richness") -> fig3h
Ag_da %>% ggplot( aes(y = BF_richness , x = ARG_abundance/1000))+
  geom_point(  size= 5, fill= '#eebb44', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "ARG (×103)"), y = "Microbial richness")-> fig3i
Ta_da %>% ggplot( aes(y = BF_richness , x = ARG_abundance/1000))+
  geom_point(  size= 5, fill= '#b26a6e', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "ARG (×103)"), y = "Microbial richness") -> fig3j


Na_da %>% ggplot( aes(y = BF_richness , x = T6SS_abundance*1000))+
  geom_point(  size= 5, fill= '#548aae', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "T6SS (×10-3)"), y = "Microbial richness") -> fig3hh
Ag_da %>% ggplot( aes(y = BF_richness , x = T6SS_abundance*1000))+
  geom_point(  size= 5, fill= '#eebb44', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "T6SS (×10-3)"), y = "Microbial richness")-> fig3ii
Ta_da %>% ggplot( aes(y = BF_richness , x = T6SS_abundance*1000))+
  geom_point(  size= 5, fill= '#b26a6e', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "T6SS (×10-3)"), y = "Microbial richness") -> fig3jj

fac %>% filter(Ecosystem == "Natural") %>% select(-1) -> Na_da
fac %>% filter(Ecosystem == "Agricultural") %>% select(-1) -> Ag_da

Na_da %>% ggplot( aes(y = BF_richness , x = eCIS_abundance/1000))+
  geom_point(  size= 5, fill= '#548aae', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "eCIS (×103)"), y = "Microbial richness") -> fig3hhh

Ag_da %>% ggplot( aes(y = BF_richness , x = eCIS_abundance/1000))+
  geom_point(  size= 5, fill= '#eebb44', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "eCIS (×103)"), y = "Microbial richness")-> fig3iii

Ta_da %>% ggplot( aes(y = BF_richness , x = eCIS_abundance/1000))+
  geom_point(  size= 5, fill= '#b26a6e', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "eCIS (×103)"), y = "Microbial richness") -> fig3jjj

P_Nu+fig3h+fig3hh+fig3hhh+plot_layout(ncol = 4,widths =  c( 6, 5,5,5))+
P_Ag+fig3i+fig3ii+fig3iii+plot_layout(ncol = 4,widths =  c( 6, 5,5,5))+
P_Ta+fig3j+fig3jj+fig3jjj+plot_layout(ncol = 4,widths =  c( 6, 5,5,5)) 


##############Figure 4########################
#RM forest
library(dplyr)
library(vegan)
library(ggprism)
library(ggpubr)
library(reshape2)
library(ggplot2)

rfPermute(BF_biomass~., data = Na_da, na.action = na.roughfix) %>% importance( type = 1) %>% data.frame() -> Na_RF_result
Na_RF_result2 <- cbind(Na_RF_result,row.names(Na_RF_result))
Na_RF_result2$`row.names(Na_RF_result)` <- factor(Na_RF_result2$`row.names(Na_RF_result)` ,level = Na_RF_result2$`row.names(Na_RF_result)`)

Na_RF_result2 %>% mutate(sig = case_when(X.IncMSE.pval <= 0.001 ~'***' , 
                                         (X.IncMSE.pval > 0.001 & X.IncMSE.pval <= 0.01) ~'**', 
                                         (X.IncMSE.pval > 0.01 & X.IncMSE.pval <= 0.05) ~'*')) %>% merge(types, by = 0) %>% #
  ggplot() +
  aes(x = `row.names(Na_RF_result)`,fill = types , weight = X.IncMSE) +
  geom_bar(color = 'black',alpha = 0.85 ,width = 0.8) +
  # scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0,20)) +
  theme_test(base_size = 20 ) + guides(fill = FALSE)+
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), panel.background = element_rect(fill = "white")) +
  labs(x = "", y = paste("Increase in MSE (%) r2 = ", round(rr$rsq[500], digits = 3))) +
  scale_fill_manual(values = c('#f0f4c3','#cbefef','#d0d5f4','#068415'))+ guides(fill = FALSE)+
  # theme(axis.title = element_text(size = 30, vjust = 1)) + 
  #theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0)) +
  labs(x = NULL) +
  geom_text(aes(x= `row.names(Na_RF_result)`, y = X.IncMSE+1,label = sig), color = 'black' ,size =10) -> P_Nu
P_Nu

rfPermute(BF_biomass~., data = Ag_da, na.action = na.roughfix) %>% importance( type = 1) %>% data.frame() -> Ag_RF_result
Ag_RF_result2 <- cbind(Ag_RF_result,row.names(Ag_RF_result))
Ag_RF_result2$`row.names(Ag_RF_result)` <- factor(Ag_RF_result2$`row.names(Ag_RF_result)` ,level = Ag_RF_result2$`row.names(Ag_RF_result)`)

Ag_RF_result2 %>% mutate(sig = case_when(X.IncMSE.pval <= 0.001 ~'***' , 
                                         (X.IncMSE.pval > 0.001 & X.IncMSE.pval <= 0.01) ~'**', 
                                         (X.IncMSE.pval > 0.01 & X.IncMSE.pval <= 0.05) ~'*')) %>% merge(types, by = 0) %>% #
  ggplot() +
  aes(x = `row.names(Ag_RF_result)`,fill = types , weight = X.IncMSE) +
  geom_bar(color = 'black',alpha = 0.85 ,width = 0.8) +
  # scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0,15)) +
  theme_test(base_size = 20 ) +  guides(fill = FALSE)+
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), panel.background = element_rect(fill = "white")) +
  labs(x = "", y = paste("Increase in MSE (%) r2 = ", round(rr$rsq[500], digits = 3))) +
  scale_fill_manual(values = c('#f0f4c3','#cbefef','#d0d5f4','#068415'))+ guides(fill = FALSE)+
  # theme(axis.title = element_text(size = 30, vjust = 1)) + 
  #theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0)) +
  labs(x = NULL) +
  geom_text(aes(x= `row.names(Ag_RF_result)`, y = X.IncMSE+1,label = sig), color = 'black' ,size =10)  -> P_Ag
P_Ag

rfPermute(BF_biomass~., data = Ta_da, na.action = na.roughfix) %>% importance( type = 1) %>% data.frame() -> Ta_RF_result
Ta_RF_result2 <- cbind(Ta_RF_result,row.names(Ta_RF_result))
Ta_RF_result2$`row.names(Ta_RF_result)` <- factor(Ta_RF_result2$`row.names(Ta_RF_result)` ,level = Ta_RF_result2$`row.names(Ta_RF_result)`)

Ta_RF_result2 %>% mutate(sig = case_when(X.IncMSE.pval <= 0.001 ~'***' , 
                                         (X.IncMSE.pval > 0.001 & X.IncMSE.pval <= 0.01) ~'**', 
                                         (X.IncMSE.pval > 0.01 & X.IncMSE.pval <= 0.05) ~'*')) %>% merge(types, by = 0) %>% #
  ggplot() +
  aes(x = `row.names(Ta_RF_result)`,fill = types , weight = X.IncMSE) +
  geom_bar(color = 'black',alpha = 0.85 ,width = 0.8) +
  # scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0,15)) +
  theme_test(base_size = 20 ) + 
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), panel.background = element_rect(fill = "white")) +
  labs(x = "", y = paste("Increase in MSE (%) r2 = ", round(rr$rsq[500], digits = 3))) +
  scale_fill_manual(values = c('#f0f4c3','#cbefef','#d0d5f4','#068415'))+ guides(fill = FALSE)+
  # theme(axis.title = element_text(size = 30, vjust = 1)) + 
  #theme(axis.title = element_text(vjust = 1), axis.text = element_text(hjust = 1), axis.text.x = element_text(vjust = 1,hjust = 1 ,angle = 30), axis.text.y = element_text(vjust = 0)) +
  labs(x = NULL) +
  geom_text(aes(x= `row.names(Ta_RF_result)`, y = X.IncMSE+1,label = sig), color = 'black' ,size =10)   -> P_Ta
P_Ta

#points
fac2 %>% filter(Ecosystem == "Natural") %>% select(-1) -> Na_da
fac2 %>% filter(Ecosystem == "Agricultural") %>% select(-1) -> Ag_da
fac2 %>% filter(Ecosystem == "Tailings") %>% select(-1) -> Ta_da
library(ggpmisc)

Na_da %>% ggplot( aes(y = BF_biomass , x = ARG_abundance/1000))+
  geom_point(  size= 5, fill= '#548aae', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "ARG (×103)"), y = "Microbial biomass") -> fig3h
Ag_da %>% ggplot( aes(y = BF_biomass , x = ARG_abundance/1000))+
  geom_point(  size= 5, fill= '#eebb44', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "ARG (×103)"), y = "Microbial biomass")-> fig3i
Ta_da %>% ggplot( aes(y = BF_biomass , x = ARG_abundance/1000))+
  geom_point(  size= 5, fill= '#b26a6e', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "ARG (×103)"), y = "Microbial biomass") -> fig3j


Na_da %>% ggplot( aes(y = BF_biomass , x = T6SS_abundance*1000))+
  geom_point(  size= 5, fill= '#548aae', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "T6SS (×10-3)"), y = "Microbial biomass") -> fig3hh
Ag_da %>% ggplot( aes(y = BF_biomass , x = T6SS_abundance*1000))+
  geom_point(  size= 5, fill= '#eebb44', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "T6SS (×10-3)"), y = "Microbial biomass")-> fig3ii
Ta_da %>% ggplot( aes(y = BF_biomass , x = T6SS_abundance*1000))+
  geom_point(  size= 5, fill= '#b26a6e', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "T6SS (×10-3)"), y = "Microbial biomass") -> fig3jj

fac %>% filter(Ecosystem == "Natural") %>% select(-1) -> Na_da
fac %>% filter(Ecosystem == "Agricultural") %>% select(-1) -> Ag_da

Na_da %>% ggplot( aes(y = BF_biomass , x = eCIS_abundance/1000))+
  geom_point(  size= 5, fill= '#548aae', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "eCIS (×103)"), y = "Microbial biomass") -> fig3hhh

Ag_da %>% ggplot( aes(y = BF_biomass , x = eCIS_abundance/1000))+
  geom_point(  size= 5, fill= '#eebb44', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "eCIS (×103)"), y = "Microbial biomass")-> fig3iii

Ta_da %>% ggplot( aes(y = BF_biomass , x = eCIS_abundance/1000))+
  geom_point(  size= 5, fill= '#b26a6e', alpha= 0.8, color='black',shape=21)+
  #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
  #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE, color = 'red', alpha=0.2) + 
  stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'top', size = 8)+
  theme_pubr(base_size = 20,border = T )+
  labs(x=paste( "eCIS (×103)"), y = "Microbial biomass") -> fig3jjj

P_Nu+P_Ag+P_Ta+plot_layout(ncol = 3,widths =  c( 6, 5,5))+
  fig3h+fig3i+fig3j+plot_layout(ncol = 3,widths =  c( 6, 5,5))+
  fig3hh+fig3ii+fig3jj+plot_layout(ncol = 3,widths =  c( 6, 5,5)) +
  fig3hhh+fig3iii+fig3jjj+plot_layout(ncol = 3,widths =  c( 6, 5,5)) 
  
##############Figure 5########################
#5a
setwd("E:\\data")
library(tidyverse)
library(Hmisc)
library(ggpubr)
library(corrplot)
library(reshape2)
da <- read.csv("data1.csv", header = 1)
da %>% dplyr::select(1,2, 26:37) %>% melt( ) -> da_melt

library(ggplot2)

da$Ecosystem  <- factor(da$Ecosystem, levels =c('Natural','Agricultural','Tailings'))

ggplot(da_melt) +
 aes(x = Ecosystem, fill = variable, weight = value) +
 geom_bar() +
 scale_fill_manual(values = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#feb462','#b3de69','#fdcde5','#d9d9d9','#bc80bd','#ccebc5','#ffee6f')) +
 theme_pubr(base_size = 10, border = T)+ylim(0,1.5)+labs(x=NULL, y="Relativa abundance (%)") -> p1
p1
da_melt %>% mutate(gr=str_replace(variable, "CBB_cycle|rTCA_cycle", "C")) %>%  
                     mutate(gr=str_replace(gr, "Nitrogen_fixation|Dissimilatory_nitrate_reduction|Assimilatory_nitrate_reduction|Denitrification|Nitrification|Anammox", "N")) %>% 
                   mutate(gr=str_replace(gr, "Inorganic_P_solublization|Organic_P_mineralization|P_transportation|P_regulation", "P")) -> DAAA

compare_means(value~Ecosystem, data = DAAA %>% filter(gr=="P"))

#5b
cbind_func_fact %>% mutate(Carbonfixation=CBB_cycle +rTCA_cycle,
                           Nitrogenmetabolism=Nitrogen_fixation+Anammox+Dissimilatory_nitrate_reduction+Assimilatory_nitrate_reduction+Denitrification+Nitrification,
                           Phosphoruscycling=Inorganic_P_solublization+ Organic_P_mineralization+P_transportation+ P_regulation) %>% 
  select(-22:-33) -> cbind_func_fact_CPN
results_r2 <- NULL
results <- NULL
for (i in colnames(cbind_func_fact_CPN)[22:24]) {
  set.seed(862)
  randomForest(as.formula(paste0(i, "~ ARG.abundance + eCIS.abundance + T6SS.abundance + 
                              C.EA_MBC + N.EA_MBC + P.EA_MBC + Bacterial.richness + 
                              Fungal.richness + Bacterial.biomass + Fungal.biomass + 
                              NDVI + Longitude + Latitude + Elevation + MAT + MAP + pH +
                              Moisture + Total.C + Total.N + Total.P")),data = cbind_func_fact_CPN, importance = TRUE, ntree = 500, na.action=na.omit) -> r2
  
  set.seed(862)
  rfPermute(as.formula(paste0(i, "~ ARG.abundance + eCIS.abundance + T6SS.abundance + 
                              C.EA_MBC + N.EA_MBC + P.EA_MBC + Bacterial.richness + 
                              Fungal.richness + Bacterial.biomass + Fungal.biomass + 
                              NDVI + Longitude + Latitude + Elevation + MAT + MAP + pH +
                              Moisture + Total.C + Total.N + Total.P")), 
            data = cbind_func_fact_CPN, importance = TRUE, ntree = 500, na.action = na.omit) %>% importance(type = 1)  -> var_each
  
  results <- rbind(var_each %>% data.frame() %>% select(1,2) %>% mutate(X.IncMSE=case_when( X.IncMSE.pval> 0.05~0, TRUE~ X.IncMSE)) %>% mutate(fac=i), results)
  results_r2 <- rbind(r2$rsq[500] %>% data.frame(R2=.) %>% mutate(facter=i),  results_r2)
}
results_r2
results %>% mutate(var = rownames(.)) %>% mutate(var=str_replace(var, "\\.", "_")) %>%  mutate(var=str_replace(var, "[0-9]+$", "")) %>% filter(X.IncMSE > 0)-> results2
results2$fac <- factor(results2$fac, levels = unique(results2$fac))


spearman_Nu <- rcorr(as.matrix(Natural_da %>% select(-1,-2)), as.matrix(cbind_func_fact_CPN[22:24]), type = 'spearman')
spearman_Nu$r %>% melt() %>% filter(Var1 %in% colnames(cbind_func_fact_CPN[22:24]) & Var2 %in% colnames(Natural_da)) %>% set_names('Var1','Var2','r') -> spearman_Nu_r
spearman_Nu$P %>% melt() %>% filter(Var1 %in% colnames(cbind_func_fact_CPN[22:24]) & Var2 %in% colnames(Natural_da)) %>% set_names('Var1','Var2','p') %>% 
  mutate(sig = case_when(  
    p <= 0.001 ~ "***",   
    p > 0.001 & p <= 0.01 ~ "**",   
    p > 0.01 & p <= 0.05 ~ "*",  
    TRUE ~ ""  # 
  )) -> spearman_Nu_p

ggplot() +
  geom_tile(data = spearman_Nu_r, aes(x = Var1, y = Var2, fill = r)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black', angle =90, hjust = 1, vjust = 0, size = 10), 
        axis.text.y = element_text(color = 'black',size = 10), axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))  +theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  labs(y = '', x = '', fill = 'Correlation')


spearman_Nu_r %>% mutate(Var2=str_replace(Var2, "\\.", "_")) -> spearman_Nu_r2
spearman_Nu_p %>% mutate(Var2=str_replace(Var2, "\\.", "_")) -> spearman_Nu_p

spearman_Nu_r2$Var1 <- factor(spearman_Nu_r2$Var1, levels = unique(spearman_Nu_r2$Var1))
spearman_Nu_r2$Var2 <- factor(spearman_Nu_r2$Var2, levels = rev(unique(spearman_Nu_r2$Var2)))
results2
ggplot() + 
  geom_tile(data = spearman_Nu_r2, aes(x = Var1, y = Var2, fill = r)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  geom_point(data = results2, aes(x = fac, y=var, size=X.IncMSE), shape = 1)+
  geom_text(data = spearman_Nu_p, aes(x = Var1, y = Var2, label=sig), size = 8)+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black', angle =90, hjust = 1, vjust = 0, size = 10), 
        axis.text.y = element_text(color = 'black',size = 10), axis.ticks = element_line(color = 'black')) +scale_size_continuous(range   = c(3, 8), limits = c(0, 25))+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  labs(y = '', x = '', fill = 'Correlation') -> p_N2
p_N2

library(ggplot2)
results_r2$facter <- factor(results_r2$facter, levels = rev(results_r2$facter))
ggplot(results_r2, aes(x = facter, fill = facter, weight = R2*100)) +
  geom_bar( color="black") +
  scale_fill_manual(values = c('#bc80bd','#8dd3c7', '#ffee6f')) +
  ggpubr::theme_pubr(base_size = 10, border = T, legend = "none")+
  labs(x = NULL, y = "Explained \nvariation (%)",  fill = NULL)+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+scale_y_continuous(expand = c(0,0))+ylim(c(0,100)) -> p_N1
#5c
cbind_func_fact %>% mutate(Carbonfixation=CBB_cycle +rTCA_cycle,
                           Nitrogenmetabolism=Nitrogen_fixation+Anammox+Dissimilatory_nitrate_reduction+Assimilatory_nitrate_reduction+Denitrification+Nitrification,
                           Phosphoruscycling=Inorganic_P_solublization+ Organic_P_mineralization+P_transportation+ P_regulation) %>% 
  select(-22:-33) -> cbind_func_fact_CPN
results_r2 <- NULL
results <- NULL
for (i in colnames(cbind_func_fact_CPN)[22:24]) {
  set.seed(862)
  randomForest(as.formula(paste0(i, "~ ARG.abundance + eCIS.abundance + T6SS.abundance + 
                              C.EA_MBC + N.EA_MBC + P.EA_MBC + Bacterial.richness + 
                              Fungal.richness + Bacterial.biomass + Fungal.biomass + 
                              NDVI + Longitude + Latitude + Elevation + MAT + MAP + pH +
                              Moisture + Total.C + Total.N + Total.P")),data = cbind_func_fact_CPN, importance = TRUE, ntree = 500, na.action=na.omit) -> r2
  
  set.seed(862)
  rfPermute(as.formula(paste0(i, "~ ARG.abundance + eCIS.abundance + T6SS.abundance + 
                              C.EA_MBC + N.EA_MBC + P.EA_MBC + Bacterial.richness + 
                              Fungal.richness + Bacterial.biomass + Fungal.biomass + 
                              NDVI + Longitude + Latitude + Elevation + MAT + MAP + pH +
                              Moisture + Total.C + Total.N + Total.P")), 
            data = cbind_func_fact_CPN, importance = TRUE, ntree = 500, na.action = na.omit) %>% importance(type = 1)  -> var_each
  
  results <- rbind(var_each %>% data.frame() %>% select(1,2) %>% mutate(X.IncMSE=case_when( X.IncMSE.pval> 0.05~0, TRUE~ X.IncMSE)) %>% mutate(fac=i), results)
  results_r2 <- rbind(r2$rsq[500] %>% data.frame(R2=.) %>% mutate(facter=i),  results_r2)
}
results_r2
results %>% mutate(var = rownames(.)) %>% mutate(var=str_replace(var, "\\.", "_")) %>%  mutate(var=str_replace(var, "[0-9]+$", "")) %>% filter(X.IncMSE > 0)-> results2
results2$fac <- factor(results2$fac, levels = unique(results2$fac))


spearman_Nu <- rcorr(as.matrix(Agricultural_da %>% select(-1,-2)), as.matrix(cbind_func_fact_CPN[22:24]), type = 'spearman')
spearman_Nu$r %>% melt() %>% filter(Var1 %in% colnames(cbind_func_fact_CPN[22:24]) & Var2 %in% colnames(Agricultural_da)) %>% set_names('Var1','Var2','r') -> spearman_Nu_r
spearman_Nu$P %>% melt() %>% filter(Var1 %in% colnames(cbind_func_fact_CPN[22:24]) & Var2 %in% colnames(Agricultural_da)) %>% set_names('Var1','Var2','p')%>% 
  mutate(sig = case_when(  
    p <= 0.001 ~ "***",   
    p > 0.001 & p <= 0.01 ~ "**",   
    p > 0.01 & p <= 0.05 ~ "*",  
    TRUE ~ ""  #   
  )) -> spearman_Nu_p

ggplot() +
  geom_tile(data = spearman_Nu_r, aes(x = Var1, y = Var2, fill = r)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black', angle =90, hjust = 1, vjust = 0, size = 10), 
        axis.text.y = element_text(color = 'black',size = 10), axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))  +theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  labs(y = '', x = '', fill = 'Correlation')


spearman_Nu_r %>% mutate(Var2=str_replace(Var2, "\\.", "_")) -> spearman_Nu_r2
spearman_Nu_p %>% mutate(Var2=str_replace(Var2, "\\.", "_")) -> spearman_Nu_p

spearman_Nu_r2$Var1 <- factor(spearman_Nu_r2$Var1, levels = unique(spearman_Nu_r2$Var1))
spearman_Nu_r2$Var2 <- factor(spearman_Nu_r2$Var2, levels = rev(unique(spearman_Nu_r2$Var2)))
results2
ggplot() + 
  geom_tile(data = spearman_Nu_r2, aes(x = Var1, y = Var2, fill = r)) +
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  geom_text(data = spearman_Nu_p, aes(x = Var1, y = Var2, label=sig),size = 8)+
  geom_point(data = results2, aes(x = fac, y=var, size=X.IncMSE), shape = 1)+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black', angle =90, hjust = 1, vjust = 0, size = 10), 
        axis.text.y = element_text(color = 'black',size = 10), axis.ticks = element_line(color = 'black')) +scale_size_continuous(range   = c(3, 8), limits = c(0, 25))+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  labs(y = '', x = '', fill = 'Correlation') -> p_A2
p_A2

library(ggplot2)
results_r2$facter <- factor(results_r2$facter, levels = rev(results_r2$facter))
ggplot(results_r2, aes(x = facter, fill = facter, weight = R2*100)) +
  geom_bar( color="black") +
  scale_fill_manual(values = c('#bc80bd','#8dd3c7', '#ffee6f')) +
  ggpubr::theme_pubr(base_size = 10, border = T, legend = "none")+
  labs(x = NULL, y = "Explained \nvariation (%)",  fill = NULL)+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+scale_y_continuous(expand = c(0,0))+ylim(c(0,100)) -> p_A1

#5d
 size=X.IncMSE), shape = 1)+
  geom_text(data = spearman_Nu_p, aes(x = Var1, y = Var2, label=sig),size = 8)+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black', angle =90, hjust = 1, vjust = 0, size = 10), 
        axis.text.y = element_text(color = 'black',size = 10), axis.ticks = element_line(color = 'black')) +scale_size_continuous(range   = c(3, 8), limits = c(0, 25))+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  labs(y = '', x = '', fill = 'Correlation') -> p_T2
p_T2

library(ggplot2)
results_r2$facter <- factor(results_r2$facter, levels = rev(results_r2$facter))
ggplot(results_r2, aes(x = facter, fill = facter, weight = R2*100)) +
  geom_bar( color="black") +
  scale_fill_manual(values = c('#bc80bd','#8dd3c7', '#ffee6f')) +
  ggpubr::theme_pubr(base_size = 10, border = T, legend = "none")+
  labs(x = NULL, y = "Explained \nvariation (%)",  fill = NULL)+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+scale_y_continuous(expand = c(0,0))+ylim(c(0,100)) -> p_T1

##############Figure 6########################

library(ggpubr)
library(patchwork)
palette=c("#8dd3c7","#ffffb3","#bebada","#fb8072","#ffed6f","#80b1d3","#fdb462","#b3de69",
           "#fccde5","#bc80bd","#ccebc5","#d9d9d9")
Phylum <- read.csv(file="gene_abun_phylum.csv")
Phylum$Phylum <- factor(Phylum$Phylum, levels = c("Acidobacteria","Actinobacteria","Bacteroidetes",
                                                  "Chloroflexi","Cyanobacteria","Firmicutes",
                                                  "Gemmatimonadetes","Nitrospirae","Planctomycetes",
                                                  "Proteobacteria","Verrucomicrobia","Others"))
Phylum$Na1_ABG<-paste0(round(Phylum$Na_ABG*100,2),"%")
Phylum$Fa1_ABG<-paste0(round(Phylum$Fa_ABG*100,2),"%")
Phylum$T1_ABG<-paste0(round(Phylum$T_ABG*100,2),"%")

p1 <- ggdonutchart(Phylum, "Na_ABG", label = "Na1_ABG",palette = palette,
                   fill = "Phylum", color = "white",lab.pos="out")+
  theme(legend.position = "none",legend.text = element_text(size=8))
p1 
p2 <- ggpie(Phylum, "Na_ABG", label = "Na1_ABG",palette = palette,
            fill = "Phylum", color = "white")+
  theme(legend.position = "right",legend.text = element_text(size=8))
p2 
p1+p2

p1 <- ggdonutchart(Phylum, "Fa_ABG", label = "Fa1_ABG",palette = palette,
                   fill = "Phylum", color = "white",lab.pos="out")+
  theme(legend.position = "none",legend.text = element_text(size=8))
p1 
p2 <- ggpie(Phylum, "Fa_ABG", label = "Fa1_ABG",palette = palette,
            fill = "Phylum", color = "white")+
  theme(legend.position = "right",legend.text = element_text(size=8))
p2 

p1+p2

p1 <- ggdonutchart(Phylum, "T_ABG", label = "T1_ABG",palette = palette,
                   fill = "Phylum", color = "white",lab.pos="out")+
  theme(legend.position = "none",legend.text = element_text(size=8))
p1
p2 <- ggpie(Phylum, "T_ABG", label = "T1_ABG",palette = palette,
            fill = "Phylum", color = "white")+
  theme(legend.position = "right",legend.text = element_text(size=8))
p2 
p1+p2

ratio <- read.csv("gene_ratio_phylum.csv",row.names = 1)
library(pheatmap)
bk <- c(seq(0,2, by=0.001))
pheatmap(ratio, fontsize_number=20, 
         cluster_rows = FALSE, cluster_cols = FALSE, 
         angle_col = 90, 
         #border_color = "white", 
         #gaps_row = c(2),
         #gaps_col = c(4),
         fontsize_col = 12,fontsize_row = 12,
         cellheight = 30, cellwidth = 30,
         color = c(colorRampPalette(colors = c("#2c7bb6","#f7f7f7"))(length(bk)/2),
                   colorRampPalette(colors = c("#f7f7f7","#d7191c"))(length(bk)/2)),
         legend_breaks = seq(0,2,0.5),breaks = bk)


#6k-6o
da <- read.csv("data_phylum.csv", header=1, row.names=1) 
da
library(psych)
library(qgraph)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(ggsci)
da %>% dim()

da %>% select(2:17,1) -> da
da$ARG <- da$ARG/1000

pp<- list()
i=1
col<-pal_igv(palette = "default")(23)#
for (aa in colnames(da)){
  p1 <-  ggplot(data = da, aes_string(x = aa , y = tail(colnames(da),1)))+
    geom_point(  size= 4, fill= col[i], alpha= 0.8, color='black',shape=21)+
    #  stat_cor(aes(x = get(aa) , y = get(tail(colnames(da),1)) , label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson', 
    #   label.x.npc = 'left', label.y.npc = 'top', size = 6)+
    geom_smooth(method = 'lm', formula = y~x, se = TRUE, show.legend = FALSE, color = '#ff1744', alpha=0.2) + 
    stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = '~`,`~')),
                 formula = y~x, parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 8)+
    theme_bw(base_size = 25)+
    labs(x= paste(aa), y= tail(colnames(da),1)  )+
    theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))  
  print(p1)
  pp[[i]] <- p1  
  i=i+1
}

library(patchwork)
pp[[1]]+pp[[7]]+pp[[8]]+pp[[6]]+pp[[9]]
  








