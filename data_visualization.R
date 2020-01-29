mae_1h_ahead_fs1 <- c()
mae_1h_ahead_fs2 <- c()
mae_1h_ahead_fs3 <- c()
mae_1h_ahead_fs4 <- c()
mae_1h_ahead_fs5 <- c()
mae_1h_ahead_fs6 <- c()
mae_1h_ahead_fs7 <- c()
mae_1h_ahead_fs8 <- c()

mae_1h_ahead_fs1nn <- c()
mae_1h_ahead_fs2nn <- c()
mae_1h_ahead_fs3nn <- c()
mae_1h_ahead_fs4nn <- c()
mae_1h_ahead_fs8nn <- c()


for (i in 1:10){
  mae1fs1 <- run_featureset_two[[i]]$mean_mae[9]
  mae1fs2 <- run_featureset_four[[i]]$mean_mae[9]
  mae1fs3 <- run_featureset_six[[i]]$mean_mae[9]
  mae1fs4 <- run_featureset_eight[[i]]$mean_mae[9]
  mae1fs5 <- run_featureset_eleven[[i]]$mean_mae[9]
  mae1fs6 <- run_featureset_twelve[[i]]$mean_mae[9]
  mae1fs7 <- run_featureset_thirteen[[i]]$mean_mae[9]
  mae1fs8 <- run_featureset_ten[[i]]$mean_mae[9]
  ##nn
  mae1fs1nn <- run_featureset_two_nn[[i]]$mean_mae[9]
  mae1fs2nn <- run_featureset_four_nn[[i]]$mean_mae[9]
  mae1fs3nn <- run_featureset_six_nn[[i]]$mean_mae[9]
  mae1fs4nn <- run_featureset_eight_nn[[i]]$mean_mae[9]
  mae1fs8nn <- run_featureset_ten_nn[[i]]$mean_mae[9]
  
  mae_1h_ahead_fs1 <- c(mae_1h_ahead_fs1,mae1fs1)
  mae_1h_ahead_fs2 <- c(mae_1h_ahead_fs2,mae1fs2)
  mae_1h_ahead_fs3 <- c(mae_1h_ahead_fs3,mae1fs3)
  mae_1h_ahead_fs4 <- c(mae_1h_ahead_fs4,mae1fs4)
  mae_1h_ahead_fs5 <- c(mae_1h_ahead_fs5,mae1fs5)
  mae_1h_ahead_fs6 <- c(mae_1h_ahead_fs6,mae1fs6)
  mae_1h_ahead_fs7 <- c(mae_1h_ahead_fs7,mae1fs7)
  mae_1h_ahead_fs8 <- c(mae_1h_ahead_fs8,mae1fs8)
  ##nn
  mae_1h_ahead_fs1nn <- c(mae_1h_ahead_fs1nn,mae1fs1nn)
  mae_1h_ahead_fs2nn <- c(mae_1h_ahead_fs2nn,mae1fs2nn)
  mae_1h_ahead_fs3nn <- c(mae_1h_ahead_fs3nn,mae1fs3nn)
  mae_1h_ahead_fs4nn <- c(mae_1h_ahead_fs4nn,mae1fs4nn)
  mae_1h_ahead_fs8nn <- c(mae_1h_ahead_fs8nn,mae1fs8nn)
  
}

rf_plot_traindays_9h <- cbind.data.frame(c(1:10),mae_1h_ahead_fs1,mae_1h_ahead_fs2,
                                      mae_1h_ahead_fs3,mae_1h_ahead_fs4,
                                      mae_1h_ahead_fs5,mae_1h_ahead_fs6,mae_1h_ahead_fs7,
                                      mae_1h_ahead_fs8)
names(rf_plot_traindays_9h) <- c('train_days','1','2','3','4','5','6','7','8')

rf_plot_traindays_9h <- melt(rf_plot_traindays_9h, id='train_days')


nn_plot_traindays_9h <- cbind.data.frame(c(1:10),mae_1h_ahead_fs1nn,
                                      mae_1h_ahead_fs2nn,mae_1h_ahead_fs3nn,
                                      mae_1h_ahead_fs4nn,mae_1h_ahead_fs8nn)

names(nn_plot_traindays_9h) <- c('train_days','1',
                              '2','3',
                              '4','8')

nn_plot_traindays_9h <- melt(nn_plot_traindays_9h, id='train_days')

#######################################################################
###1h ahead prediction

####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_1h <- ggplot(rf_plot_traindays_1h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.065,0.075),breaks=seq(0.065,0.075,0.0025),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  annotation_custom(text_annotation_a,xmin=0,xmax=0,ymin=0,ymax=0)+
  labs(colour = "Feature set")



####ANN
nn_plot_ktraindays_1h <- ggplot(nn_plot_traindays_1h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.075,0.155),breaks=seq(0.075,0.155,0.02),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_1h

##combine both plots
plot_grid(
  rf_plot_ktraindays_1h, nn_plot_ktraindays_1h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

####################################
###2h ahead prediction

####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_2h <- ggplot(rf_plot_traindays_2h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.09,0.11),breaks=seq(0.09,0.11,0.005),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  annotation_custom(text_annotation_a,xmin=0,xmax=0,ymin=0,ymax=0)+
  labs(colour = "Feature set")

rf_plot_ktraindays_2h


####ANN
nn_plot_ktraindays_2h <- ggplot(nn_plot_traindays_2h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.11,0.21),breaks=seq(0.11,0.21,0.02),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_2h

##combine both plots
plot_grid(
  rf_plot_ktraindays_2h, nn_plot_ktraindays_2h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

####################################
###3h ahead prediction

####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_3h <- ggplot(rf_plot_traindays_3h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.1,0.13),breaks=seq(0.1,0.13,0.005),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  annotation_custom(text_annotation_a,xmin=0,xmax=0,ymin=0,ymax=0)+
  labs(colour = "Feature set")

rf_plot_ktraindays_3h


####ANN
nn_plot_ktraindays_3h <- ggplot(nn_plot_traindays_3h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.13,0.19),breaks=seq(0.13,0.19,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_3h

##combine both plots
plot_grid(
  rf_plot_ktraindays_3h, nn_plot_ktraindays_3h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

####################################
###4h ahead prediction

####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_4h <- ggplot(rf_plot_traindays_4h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.105,0.145),breaks=seq(0.105,0.145,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  annotation_custom(text_annotation_a,xmin=0,xmax=0,ymin=0,ymax=0)+
  labs(colour = "Feature set")

rf_plot_ktraindays_4h


####ANN
nn_plot_ktraindays_4h <- ggplot(nn_plot_traindays_4h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.140,0.2),breaks=seq(0.140,0.2,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_4h

##combine both plots
plot_grid(
  rf_plot_ktraindays_4h, nn_plot_ktraindays_4h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


######################
###5h ahead prediction

####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_5h <- ggplot(rf_plot_traindays_5h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.105,0.155),breaks=seq(0.105,0.155,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

rf_plot_ktraindays_5h


####ANN
nn_plot_ktraindays_5h <- ggplot(nn_plot_traindays_5h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.145,0.215),breaks=seq(0.145,0.215,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_5h


##combine both plots
plot_grid(
  rf_plot_ktraindays_5h, nn_plot_ktraindays_5h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

######################
###6h ahead prediction


####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_6h <- ggplot(rf_plot_traindays_6h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.11,0.16),breaks=seq(0.1,0.16,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

rf_plot_ktraindays_6h


####ANN
nn_plot_ktraindays_6h <- ggplot(nn_plot_traindays_6h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.155,0.195),breaks=seq(0.155,0.195,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_6h


##combine both plots
plot_grid(
  rf_plot_ktraindays_6h, nn_plot_ktraindays_6h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


######################
###7h ahead prediction


####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_7h <- ggplot(rf_plot_traindays_7h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.11,0.17),breaks=seq(0.11,0.17,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

rf_plot_ktraindays_7h


####ANN
nn_plot_ktraindays_7h <- ggplot(nn_plot_traindays_7h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.155,0.195),breaks=seq(0.155,0.195,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_7h


##combine both plots
plot_grid(
  rf_plot_ktraindays_7h, nn_plot_ktraindays_7h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

######################
###8h ahead prediction

####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_8h <- ggplot(rf_plot_traindays_8h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.11,0.17),breaks=seq(0.11,0.17,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

rf_plot_ktraindays_8h


####ANN
nn_plot_ktraindays_8h <- ggplot(nn_plot_traindays_8h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.155,0.205),breaks=seq(0.155,0.205,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_8h


##combine both plots
plot_grid(
  rf_plot_ktraindays_8h, nn_plot_ktraindays_8h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

######################
###9h ahead prediction


####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_9h <- ggplot(rf_plot_traindays_9h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.11,0.17),breaks=seq(0.11,0.17,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

rf_plot_ktraindays_9h


####ANN
nn_plot_ktraindays_9h <- ggplot(nn_plot_traindays_9h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.16,0.2),breaks=seq(0.16,0.2,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_9h


##combine both plots
plot_grid(
  rf_plot_ktraindays_9h, nn_plot_ktraindays_9h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

######################
###10h ahead prediction


####RF
##MAE of different feature sets as a function of traing days
rf_plot_ktraindays_10h <- ggplot(rf_plot_traindays_10h[-c(41:70),], 
                                aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.11,0.17),breaks=seq(0.11,0.17,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.12),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

rf_plot_ktraindays_10h


####ANN
nn_plot_ktraindays_10h <- ggplot(nn_plot_traindays_10h, aes(train_days,value))+
  geom_line(aes(colour=variable),size=.7)+
  theme_bw()+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Training dataset size (days)", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) +
  scale_y_continuous(limits=c(0.155,0.205),breaks=seq(0.155,0.205,0.01),expand = c(0,0))+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9),
        legend.text=element_text(size=8),
        legend.key.size = unit(.4,"cm"),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.88,0.88),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour = "Feature set")

nn_plot_ktraindays_10h


##combine both plots
plot_grid(
  rf_plot_ktraindays_10h, nn_plot_ktraindays_10h,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


#############################################################################
##Plot performance of all Feature sets (average over all training days) as
##a function of prediction steps

##RF

##fetch values from table_values_error
rf_plot_3 <- melt(mean_mae_rf,id='horizon')


rf_plot_pred_hor <- ggplot(rf_plot_3, aes(horizon,as.numeric(as.character(value))))+
  theme_bw()+
  geom_line(aes(colour=variable),size=.7)+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  labs(x = "Prediction steps", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(0.06,0.18),breaks=seq(0.06,0.18,0.03),expand = c(0,0))+
  theme(legend.key.size = unit(.4,"cm")) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.82,0.078),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')


###ANN

nn_plot_3 <- melt(mean_mae_nn,id='horizon')

nn_plot_pred_hor <- ggplot(nn_plot_3, aes(horizon,value))+
  theme_bw()+
  geom_line(aes(colour=variable),size=0.7)+
  scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
  labs(x = "Prediction steps", y = "MAE") +
  scale_x_continuous(breaks = seq(1, 10, 1),expand=c(0,0)) + 
  scale_y_continuous(limits=c(0.09,0.19),breaks=seq(0.09,0.19,0.02),expand = c(0,0)) + 
  theme(legend.key.size = unit(.4,"cm")) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.9,0.135),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')



plot_grid(
  rf_plot_pred_hor, nn_plot_pred_hor,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


###############################################################################
##Feature importance

namesimp <- c('TOD','Betweenness','Closeness','Alpha','Public Transport',
              'Touristic','Business','Residential','Office',
              'Industrial')
#namesimp <- c(names(run_featureset_ten[[1]][6]$imp_incmse[[1]][1:10]))
importances <- cbind.data.frame(namesimp,stringsAsFactors =F)


for (i in 1:10){
  imps <- unname(run_featureset_ten[[1]][6]$imp_incmse[[i]][1:10])
  importances <- cbind(importances,imps)
}
colnames(importances) <- c("feature",as.character(1:10))
mean_importance <- rowMeans(importances[,-1])
importances <- cbind(importances,mean_importance)
importances <- importances[,c(1,12)]

importances[nrow(importances) + 1,] <- list("t-1",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[1]][11]))
importances[nrow(importances) + 1,] <- list("t-2",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[2]][11]))
importances[nrow(importances) + 1,] <- list("t-3",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[3]][11]))
importances[nrow(importances) + 1,] <- list("t-4",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[4]][11]))
importances[nrow(importances) + 1,] <- list("t-5",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[5]][11]))
importances[nrow(importances) + 1,] <- list("t-6",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[6]][11]))
importances[nrow(importances) + 1,] <- list("t-7",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[7]][11]))
importances[nrow(importances) + 1,] <- list("t-8",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[8]][11]))
importances[nrow(importances) + 1,] <- list("t-9",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[9]][11]))
importances[nrow(importances) + 1,] <- list("t-10",
                        unname(run_featureset_ten[[1]][6]$imp_incmse[[10]][11]))

##mean value of past OCCR
mean(importances[1:20,2])

importances <- rbind(importances,c('Historical Occupancy',mean(importances[1:20,2])))


##importance of historical Occupancy rates
plot_importance_occr <- ggplot(data = importances[c(11:20),], aes(x = feature,
                                      y = as.numeric(mean_importance))) +
  theme_bw()+
  geom_bar(stat = "identity", width = .4,fill='#3182bd') +
  labs(x = "OCCR", y = "%IncMSE") +
  scale_x_discrete(limits=c("t-1","t-2","t-3","t-4","t-5","t-6","t-7","t-8",
                            "t-9","t-10"))+
  scale_y_continuous(limits=c(0.0,0.1),breaks=seq(0,0.1,0.01),expand = c(0,0))+
  theme(axis.text.x = element_text(size=10,angle = 45,hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.title = element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=10),
        axis.title.x = element_blank(),
        plot.margin = unit(c(.50,0.1,.5,.5), "cm"))

plot_importance_occr


#####


plot_hist_tod <- ggplot(data = importances[c(21,1),], aes(x = feature,
                                        y = as.numeric(mean_importance))) +
  theme_bw()+
  geom_bar(stat = "identity", width = .4,fill='#3182bd') +
  labs(x = "", y = "%IncMSE",title='Historical &\nTemporal') +
  scale_y_continuous(limits=c(0.0,0.07),breaks=seq(0,0.06,0.01),expand = c(0,0))+
  scale_x_discrete(labels=c("Historical Occupancy" = "Historical\nOccupancy",
                            "TOD"="TOD"))+
  annotate("rect", xmin = -Inf, 
           xmax = Inf, ymin = 0.064, ymax = 0.07,
           alpha = 1,size=0,color='black',fill="#A9A9A9")+
  theme(axis.text.x = element_text(size=10,angle = 45,hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.title = element_text(vjust=-12,hjust=.5,size = 11,color = 'white'),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=10),
        axis.title.x.bottom=element_text(vjust=18,hjust=.5),
        axis.title.x = element_text(size=10,colour="grey30"),
        plot.margin = unit(c(.50,0.1,.5,.5), "cm"))


plot_importance_centrality <- ggplot(data = importances[c(2:4),], aes(x = feature,
                                                           y = as.numeric(mean_importance))) +
  theme_bw()+
  geom_bar(stat = "identity", width = .4,fill='#3182bd') +
  labs(x = "", y = "",title='Centrality') +
  scale_y_continuous(limits=c(0.0,0.07),breaks=seq(0,0.06,0.01),expand = c(0,0))+
  annotate("rect", xmin = -Inf, 
           xmax = Inf, ymin = 0.064, ymax = 0.07,
           alpha = 1,size=0,color='black',fill="#A9A9A9")+
  theme(axis.text.x = element_text(size=10,angle = 45,hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.title = element_text(vjust=-12,hjust=.5,size=11,color='white'),
        plot.margin = unit(c(.50,0.1,.5,-.5), "cm"))


plot_importance_poi <- ggplot(data = importances[c(5:7),], aes(x = feature,
              y = as.numeric(mean_importance))) +
  theme_bw()+
  geom_bar(stat = "identity", width = .4,fill='#3182bd') +
  labs(x = "", y = "",title='POI') +
  scale_y_continuous(limits=c(0.0,0.07),breaks=seq(0,0.06,0.01),expand = c(0,0))+
  annotate("rect", xmin = -Inf, 
           xmax = Inf, ymin = 0.064, ymax = 0.07,
           alpha = 1,size=0,color='black',fill="#A9A9A9")+
  theme(axis.text.x = element_text(size=10,angle = 45,hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.title = element_text(vjust=-12,hjust=.5,size=11,color='white'),
        plot.margin = unit(c(.50,0.1,.5,-.5), "cm"))


plot_importance_landuse <- ggplot(data = importances[c(8:10),], 
                                  aes(x = feature,
                                 y = as.numeric(mean_importance))) +
  theme_bw()+
  geom_bar(stat = "identity", width = .4,fill='#3182bd') +
  labs(x = "", y = "",title='Land Use') +
  scale_y_continuous(limits=c(0.0,0.07),breaks=seq(0,0.06,0.01),expand = c(0,0))+
  annotate("rect", xmin = -Inf, 
           xmax = Inf, ymin = 0.064, ymax = 0.07,
           alpha = 1,size=0,color='black',fill="#A9A9A9")+
  theme(axis.text.x = element_text(size=10,angle = 45,hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.title = element_text(vjust=-12,hjust=.5,size=11,color='white'),
        plot.margin = unit(c(.50,0.5,.5,-0.5), "cm"))


###arrange four plots into one
ggarrange(
  plot_hist_tod, plot_importance_centrality,plot_importance_poi,
  plot_importance_landuse,ncol=5,
  common.legend = TRUE, legend = "none",align='h',
  widths = c(1.07,1,1,1),
  font.label = list(size = 8, color = "black"), hjust = -2.3,vjust=-.5)


#################################################################################
###boxplots for districts performance

##fetch from table_values_error

###plot error per district (RF & NN)
##1h ahead prediction fs1
plot_dist_error_1hfs1 <- ggplot(street_seg_error_rfnn, aes(x=PM_DISTRICT_NAME, 
                                                           y=mae1hfs1, fill=algorithm)) + 
  geom_boxplot(width=.5,position = position_dodge(.7),outlier.size=.8)+
  theme_bw()+
  scale_fill_manual(values=c('#4daf4a','#984ea3'))+
  scale_y_continuous(limits=c(0.025,0.2),breaks=seq(0,0.2,0.025),expand = c(0,0))+
  labs(x = "", y = "MAE",fill='Algorithm')+
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.position = c(0.89,0.895),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.text=element_text(size=8))

###plot error per district (RF & NN)
##1h ahead prediction fs8
plot_dist_error_1hfs8 <- ggplot(street_seg_error_rfnn, aes(x=PM_DISTRICT_NAME, 
                                                           y=mae1hfs8, fill=algorithm)) + 
  geom_boxplot(width=.5,position = position_dodge(.7),outlier.size=.8)+
  theme_bw()+
  scale_fill_manual(values=c('#4daf4a','#984ea3'))+
  scale_y_continuous(limits=c(0.025,0.2),breaks=seq(0,0.2,0.025),expand = c(0,0))+
  labs(x = "", y = "MAE",fill='Algorithm')+
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.position = c(0.89,0.895),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.text=element_text(size=8))

##combine both plots

plot_grid(
  plot_dist_error_1hfs1, plot_dist_error_1hfs8,align = "hv", ncol = 2,
  labels = c('(a) Feature set 1', '(b) Feature set 8'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -3.5,label_fontface = 'plain'
)


##################################################

###plot error per district (RF & NN)
##5h ahead prediction fs1
plot_dist_error_5hfs1 <- ggplot(street_seg_error_rfnn, aes(x=PM_DISTRICT_NAME, y=mae5hfs1, fill=algorithm)) + 
  geom_boxplot(width=.5,position = position_dodge(.7),outlier.size=.8)+
  theme_bw()+
  scale_fill_manual(values=c('#4daf4a','#984ea3'))+
  scale_y_continuous(limits=c(0.05,0.4),breaks=seq(0.05,0.4,0.05),expand = c(0,0))+
  labs(x = "", y = "MAE",fill='Algorithm')+
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.position = c(0.89,0.895),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.text=element_text(size=8))


###plot error per district (RF & NN)
##5h ahead prediction fs8
plot_dist_error_5hfs8 <- ggplot(street_seg_error_rfnn, aes(x=PM_DISTRICT_NAME, y=mae5hfs8, fill=algorithm)) + 
  geom_boxplot(width=.5,position = position_dodge(.7),outlier.size=.8)+
  theme_bw()+
  scale_fill_manual(values=c('#4daf4a','#984ea3'))+
  scale_y_continuous(limits=c(0.05,0.4),breaks=seq(0.05,0.4,0.05),expand = c(0,0))+
  labs(x = "", y = "MAE",fill='Algorithm')+
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.position = c(0.89,0.895),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.text=element_text(size=8))

##combine both plots

plot_grid(
  plot_dist_error_5hfs1, plot_dist_error_5hfs8,align = "hv", ncol = 2,
  labels = c('(a) Feature set 1', '(b) Feature set 8'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -3.5,label_fontface = 'plain'
)

################################

##improvement per district (boxplot)
melted_street_seg_error_rf <- melt(street_seg_error_rf[,c('PM_DISTRICT_NAME',
                                                          'mae1h_per_diff_rf',
                                                          'mae5h_per_diff_rf')],
                                   id='PM_DISTRICT_NAME')


plot_dist_improvement <- ggplot(melted_street_seg_error_rf, 
                                aes(x=PM_DISTRICT_NAME, y=value, fill=variable)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.3)+
  geom_boxplot(width=.5,position = position_dodge(.6),outlier.size=.8)+
  theme_bw()+
  scale_fill_manual(values=c('#9ecae1','#3182bd'),labels=c(1,5))+
  scale_y_continuous(limits=c(-20,55),breaks=seq(-20,55,15),expand = c(0,0))+
  labs(x = "", y = "Performance improvement (% MAE)",fill='Prediction horizon (steps)')+
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1),
        legend.position = c(0.65,0.90),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.text=element_text(size=8))


###############################
##fetch from table_values_error

###improvement as function of prediction horizon
###RF
melted_change_table_rf <- melt(change_table_rf,id='id')
percent_change_horizon_rf <- ggplot(melted_change_table_rf, aes(as.numeric(variable),value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(2,26),breaks=seq(2,26,4),expand = c(0,0)) + 
  labs(x = "Prediction horizon (steps)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.080),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

###improvement as function of prediction horizon
###ANN
melted_change_table_nn <- melt(change_table_nn,id='id')
percent_change_horizon_nn <- ggplot(melted_change_table_nn, aes(as.numeric(variable),value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-8,8),breaks=seq(-8,8,2),expand = c(0,0)) + 
  labs(x = "Prediction horizon (steps)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.066),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')


plot_grid(
  percent_change_horizon_rf, percent_change_horizon_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


###############################################################
###improvement as function of training dataset size

#RF
#plot percentage improvements ### 1 hours ahead prediction
melted_change_table_1hp_rf <- melt(change_table_1hp_rf,id='id')
percent_change_train_1h_rf <- ggplot(melted_change_table_1hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(2,6),breaks=seq(2,6,0.5),expand = c(0,0)) + 
  labs(x = "Training input (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 1 hours ahead prediction
melted_change_table_1hp_nn <- melt(change_table_1hp_nn,id='id')
percent_change_train_1h_nn <- ggplot(melted_change_table_1hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-22,14),breaks=seq(-22,14,4),expand = c(0,0)) + 
  labs(x = "Training input (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_1h_rf, percent_change_train_1h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


############

#RF
#plot percentage improvements ### 2 hours ahead prediction
melted_change_table_2hp_rf <- melt(change_table_2hp_rf,id='id')
percent_change_train_2h_rf <- ggplot(melted_change_table_2hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(4,12),breaks=seq(4,12,1),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 2 hours ahead prediction
melted_change_table_2hp_nn <- melt(change_table_2hp_nn,id='id')
percent_change_train_2h_nn <- ggplot(melted_change_table_2hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-15,10),breaks=seq(-15,10,5),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_2h_rf, percent_change_train_2h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


############

#RF
#plot percentage improvements ### 3 hours ahead prediction
melted_change_table_3hp_rf <- melt(change_table_3hp_rf,id='id')
percent_change_train_3h_rf <- ggplot(melted_change_table_3hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(6,16),breaks=seq(6,16,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 3 hours ahead prediction
melted_change_table_3hp_nn <- melt(change_table_3hp_nn,id='id')
percent_change_train_3h_nn <- ggplot(melted_change_table_3hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-12,12),breaks=seq(-12,12,4),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_3h_rf, percent_change_train_3h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


############

#RF
#plot percentage improvements ### 4 hours ahead prediction
melted_change_table_4hp_rf <- melt(change_table_4hp_rf,id='id')
percent_change_train_4h_rf <- ggplot(melted_change_table_4hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(8,20),breaks=seq(8,20,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.80,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 4 hours ahead prediction
melted_change_table_4hp_nn <- melt(change_table_4hp_nn,id='id')
percent_change_train_4h_nn <- ggplot(melted_change_table_4hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-12,12),breaks=seq(-12,12,4),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_4h_rf, percent_change_train_4h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


############

#RF
#plot percentage improvements ###5 hours ahead prediction
melted_change_table_5hp_rf <- melt(change_table_5hp_rf,id='id')
percent_change_train_5h_rf <- ggplot(melted_change_table_5hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(8,22),breaks=seq(8,22,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

percent_change_train_5h_rf

##ANN
#plot percentage improvements  ### 5 hours ahead prediction
melted_change_table_5hp_nn <- melt(change_table_5hp_nn,id='id')
percent_change_train_5h_nn <- ggplot(melted_change_table_5hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-6,18),breaks=seq(-6,18,4),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_5h_rf, percent_change_train_5h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

##############

#RF
#plot percentage improvements ### 6 hours ahead prediction
melted_change_table_6hp_rf <- melt(change_table_6hp_rf,id='id')
percent_change_train_6h_rf <- ggplot(melted_change_table_6hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(10,25),breaks=seq(10,25,2.5),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 6 hours ahead prediction
melted_change_table_6hp_nn <- melt(change_table_6hp_nn,id='id')
percent_change_train_6h_nn <- ggplot(melted_change_table_6hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-8,12),breaks=seq(-8,12,4),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_6h_rf, percent_change_train_6h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


############

#RF
#plot percentage improvements ### 7 hours ahead prediction
melted_change_table_7hp_rf <- melt(change_table_7hp_rf,id='id')
percent_change_train_7h_rf <- ggplot(melted_change_table_7hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(10,26),breaks=seq(10,26,4),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.80,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 7 hours ahead prediction
melted_change_table_7hp_nn <- melt(change_table_7hp_nn,id='id')
percent_change_train_7h_nn <- ggplot(melted_change_table_7hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-6,10),breaks=seq(-6,10,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_7h_rf, percent_change_train_7h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


############

#RF
#plot percentage improvements ### 8 hours ahead prediction
melted_change_table_8hp_rf <- melt(change_table_8hp_rf,id='id')
percent_change_train_8h_rf <- ggplot(melted_change_table_8hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(12,28),breaks=seq(12,28,4),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.80,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 8 hours ahead prediction
melted_change_table_8hp_nn <- melt(change_table_8hp_nn,id='id')
percent_change_train_8h_nn <- ggplot(melted_change_table_8hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-4,10),breaks=seq(-4,10,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_8h_rf, percent_change_train_8h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


############

#RF
#plot percentage improvements ### 9 hours ahead prediction
melted_change_table_9hp_rf <- melt(change_table_9hp_rf,id='id')
percent_change_train_9h_rf <- ggplot(melted_change_table_9hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(12,28),breaks=seq(12,28,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.80,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 9 hours ahead prediction
melted_change_table_9hp_nn <- melt(change_table_9hp_nn,id='id')
percent_change_train_9h_nn <- ggplot(melted_change_table_9hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-4,10),breaks=seq(-4,10,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)" , y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.8,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_9h_rf, percent_change_train_9h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)

############

#RF
#plot percentage improvements ### 10 hours ahead prediction
melted_change_table_10hp_rf <- melt(change_table_10hp_rf,id='id')
percent_change_train_10h_rf <- ggplot(melted_change_table_10hp_rf, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000',
                               '#ff00ff','#a52a2a','#12e7e7','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(14,30),breaks=seq(14,30,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.80,0.924),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

##ANN
#plot percentage improvements ### 10 hours ahead prediction
melted_change_table_10hp_nn <- melt(change_table_10hp_nn,id='id')
percent_change_train_10h_nn <- ggplot(melted_change_table_10hp_nn, aes(as.numeric(variable),
                                                                     value,group=id))+
  theme_bw()+
  geom_line(aes(colour=id),size=.7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black",size=.4)+
  scale_colour_manual(values=c('#0000FF','#ff0000','#008000','#252525'))+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0,0)) + 
  scale_y_continuous(limits=c(-8,10),breaks=seq(-8,10,2),expand = c(0,0)) + 
  labs(x = "Training dataset size (days)", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.80,0.939),legend.direction = 'horizontal',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.4,"cm"))+
  guides(colour = guide_legend(title.position = "top"))+
  labs(colour='Feature set')

plot_grid(
  percent_change_train_10h_rf, percent_change_train_10h_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)


###########################################################################
#histogram 1h, 5h, and 10h change for each feature set (RF)

change_table_rf_hist <- change_table_rf[,c('id','1','5','10')]
melted_change_table_rf_hist <- melt(change_table_rf_hist,id='id')

percent_change_hist_rf <- ggplot(melted_change_table_rf_hist, aes(fill=variable, 
                                                                  y=value, x=id)) + 
  geom_bar(width=0.7, position = position_dodge(width = 0.8), stat="identity")+
  theme_bw()+
  scale_fill_manual(values=c('#6baed6','#3182bd','#08519c'))+
  scale_x_discrete(breaks=c('2','3','4','5','6','7','8')) + 
  scale_y_continuous(limits=c(0,28),breaks=seq(0,28,4),expand = c(0,0)) + 
  labs(x = "Feature set", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.2,0.89),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.6,"cm"),
        axis.ticks.x=element_blank())+
  guides(fill = guide_legend(title.position = "top"))+
  labs(fill='Prediction horizon (steps)')


#####################
#histogram 1h, 5h, and 10h change for each feature set (ANN)

change_table_nn_hist <- change_table_nn[,c('id','1','5','10')]
melted_change_table_nn_hist <- melt(change_table_nn_hist,id='id')

percent_change_hist_nn <- ggplot(melted_change_table_nn_hist, aes(fill=variable, y=value, x=id)) + 
  geom_bar(width=0.6, position = position_dodge(width = 0.7), stat="identity")+
  theme_bw()+
  scale_fill_manual(values=c('#6baed6','#3182bd','#08519c'))+
  scale_x_discrete(breaks=c('2','3','4','8')) + 
  scale_y_continuous(limits=c(-8,8),breaks=seq(-8,8,4),expand = c(0,0)) + 
  labs(x = "Feature set", y = "Performance improvement (% MAE)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.box.background = element_rect(colour = "black"),
        legend.position = c(0.2,0.89),legend.direction = 'vertical',
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.key.size = unit(.6,"cm"),
        axis.ticks.x=element_blank())+
  guides(fill = guide_legend(title.position = "top"))+
  labs(fill='Prediction horizon (steps)')




plot_grid(
  percent_change_hist_rf, percent_change_hist_nn,align = "hv", ncol = 2,
  labels = c('(a) RF', '(b) ANN'),label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -2.5,label_fontface = 'plain'
)



