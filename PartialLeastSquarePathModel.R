#each personality includes their original score, not averaged, e.g. ext has three.

library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr)
library(readr)
library(readxl)
library(plspm)
library(plsdepot) #for pca correleation circle.
library(reshape)
library(mctest) #check multicollineary VIF
library(corrplot)
library(RColorBrewer)

##############################
#load data 
##############################

merged_bigfive_socialcapital_notaverged <- read_csv("E:/Data/merged_bigfive_socialcapital_notaverged.csv")

df1 <- merged_bigfive_socialcapital_notaverged %>% dplyr::select(-X1)

##########################
#barplot for each question of each personality
##########################
#questions of extraversion
ext1 <- 'I feel comfortable around people.'
ext2 <- 'I start conversations.'
ext3 <- 'I am talkative.'

#put questions in one vector
ext_q <- c(ext1,ext2,ext3)

#setting graphical parameters
op <- par(mfrow = c(2,2),mar = c(2.5,3.2,2,0.8))
#bar chart of each question of extraversion
for (i in 2:(length(ext_q) + 1)) {
  distribution = table(df[,i])/nrow(df1)
  barplot(distribution,
          boarder = NA,
          col = brewer.pal(8,'Blues')[2:8],
          axes = FALSE,
          main = ext_q[i - 1],
          cex.main = 1)
  axis(side = 2,las = 2)
  box('figure',col = 'gray70')
}


par(op)

###################################
#boxplot for each personality
###################################
#ext
ext <- df1 %>% dplyr::select(d_id,ext1,ext2,ext3)
ext.long <- gather(ext,'extraversion','score',ext1:ext3)
ext.boxplot <- ggplot(ext.long, aes(extraversion,score)) +
  geom_boxplot(aes(fill = extraversion),outlier.colour = 'red') +
  scale_fill_manual(values = c('#FFCCCC','#fff699','#c0ff99')) +
  ggtitle('Extraversion Personality Boxplot')

ext.boxplot


###############################
#prepare data for the model
###############################
df2 <- df1
df2$n.ext1 <- 7 - df2$ext1
df2$n.ext2 <- 7 - df2$ext2
df2$n.ext3 <- 7 - df2$ext3
df2$n.con1 <- 7 - df2$con1
df2$n.con2 <- 7 - df2$con2
df2$n.con3 <- 7 - df2$con3

df <- df2 %>% dplyr::select(d_id,
                            agr1,agr2,
                            n.con1,n.con2,n.con3,
                            #neu1,neu2,neu3,
                            ope1,ope2,
                            #brsc1,brsc4,
                            obosc1,obosc2,obosc3,
                            msc1,msc2,msc3,
                            #sound,
                            #vibrate,
                            shop,
                            #phot,phot.f,
                            ente,ente.f,
                            musi,
                            #weat.f,
                            wechat,wechat.f,
                            #QQ.f,
                            zhifubao.f,
                            #setting,
                            #soci.f,
                            sound)
df <- as.data.frame(df)

corrplot(cor(df[,-1]),type = 'lower')


###################################################
#pls pm mode1 1
###################################################

########################
#inner model matrix
########################

#rows of the inner model matrix. 0 means no affect, 1 means affect. column j affect row i.
#PAGR <- c(0,0) #personality agreement
PCON <- c(0,0,0,0,0,0,0,0,0,0) #personality consioutiouness
#PNEU <- c(0,0,0,0,0,0,0,0,0,0) #personality neu
POPE <- c(0,0,0,0,0,0,0,0,0,0) #personality openness
#SCOBR <- c(1,0,0,0,0,0) # social capital online bridege
WEC <- c(1,1,0,0,0,0,0,0,0,0) #wechat
ENT <- c(1,1,0,0,0,0,0,0,0,0) # entertaiment
MUS <- c(1,1,0,0,0,0,0,0,0,0) # music
#WEA <- c(0,1,0,0,0,0,0,0,0,0) #weather
#PHO <- c(1,0,0,0,0,0) #photo & vedio
SHO <- c(0,1,0,0,0,0,0,0,0,0) #shopping
#QQF <- c(0,0,0,0,0,0,0,0,0,0,0,0) #QQ.freq
ZFF <- c(0,1,0,0,0,0,0,0,0,0) #Zhifubao freq
#SEF <- c(0,0,0,0,0,0,0,0,0,0,0,0) #setting freq
#SNF <- c(0,0,0,0,0,0,0,0,0,0,0) #social network freq.
NSOU <- c(0,1,0,0,0,0,0,0,0,0) #notification sound
#NVIB <- c(1,0,0,0,0,0) #notificaton-vibrate
SCMAN <- c(0,0,1,1,1,1,1,1,0,0) #social capital mantainane
SCOBR <- c(0,0,1,1,1,1,1,1,0,0) #colnmae is obosc. social capital online bonding
#SCBON <- c(0,0,0,0,0,1,1,1,0,0,0,0) #colnmae is brsc. 



#path matrix created by row binding
p_path <- rbind(#PAGR,
  PCON,
  #PNEU,
  POPE,
  #SCOBR,
  WEC,
  ENT,
  MUS,
  #WEA,
  #PHO,
  SHO,
  #QQF,
  ZFF,
  #SEF,
  #SNF,
  NSOU,
  SCMAN,
  #SCBR,
  SCOBR)
#SCBON)
#NVIB)

#add column names
colnames(p_path) <- row.names(p_path)

#plot the path matrix
innerplot(p_path)

#################
#outer model: blocks and modes.
#################

#define list of indicators, what variables are associated with what latent variables
p_blocks <- list(#c('agr1','agr2'),
  c('n.con1','n.con2','n.con3'),
  #c('neu1','neu2','neu3'),
  c('ope1','ope2'),
  c('wechat.f'),
  c('ente','ente.f'),
  c('musi'),
  #c('weat.f'),
  #c('phot','phot.f'),
  c('shop'),
  #c('QQ.f'),
  c('zhifubao.f'),
  #c('setting'),
  #c('soci.f'),
  c('sound'),
  c('msc1','msc2','msc3'),
  #c('brsc1','brsc2','brsc3','brsc4'),
  c('obosc1','obosc2','obosc3') #name is wrong, it is bridge sc.
  #c('brsc1','brsc4')
  #c('vibrate')
)
#all latent variables are measured in a reflective way (mark as letter 'A', formative marked as 'B')
p_modes <- c('A',
             'A',
             'A',
             'A',
             'A',
             'A',
             'A',
             'A',
             'A',
             'A'
)

'A'#reflective mode (A) and formative mode (B).

#run plspm
p_pls <- plspm(df,
               p_path,
               p_blocks, 
               modes = p_modes,
               #scaling = p_scale,
               scheme = 'path',
               #scaled = TRUE,
               boot.val = TRUE, 
               br = 100)
p_pls
summary(p_pls)

##########################################
#assess PLS_PM model
#########################################

######################
#1.check the outer model first
######################
#ONLY apply to reflective mode (A)
#c.alpha >0.7 , DG.rho >0.7. (d.rho is better indicator than c.alpha)
#eig.lst should be much larger than 1, eig.2nd should be smaller than 1.
p_pls$unidim

#plot the loadings
plot(p_pls, what = 'loadings')

#plot the weight
plot(p_pls, what = 'weighting')


#path coeffient, here the pr(>|t|) is the p value, which are used to check the significant. 
#bootstrap confidence interval should be used to check these values.
p_pls$path_coefs

##there are some nice plot about it from the book. 
#matrix of path coefficients
paths <- p_pls$path_coefs

#matrix with values based on path coeffs
arrow_lwd <- 10 * round(paths,2)

#arrows of different sizes reflecting the values of the path coeffs
plot(p_pls, arr.pos = 0.35, arr.lwd = arrow_lwd)

#check outer model
#(for reflective mode,loading should >0.7.)
#(for formative mode: 
#1. the weighting should be checked. It should be >1.96, 
#but if it is not, it is fine. 
#2.Check the multicollinearity VIF<10, then consider elimate the indicator. 
#3.Check the loading, it should be signification (95% confidence interval should not include 0.)
p_pls$outer_model

#plot the outermodel above with nice ggplot for reflective model
#barchart of loading

sub.outermodel <- p_pls$outer_model %>% filter(block %in% reflectivemode)

ggplot(data = sub.outermodel,
       aes(x = name, y = loading, fill = block)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  #threshold line (to peek acceptable loading above 0.7)
  geom_hline(yintercept = 0.7, color = 'gray50') +
  #add titel
  ggtitle('Barchart of loading for outer model') +
  theme(axis.text.x = element_text(angle = 90))

#check cross loading for reflective mode.
#(traitor indicator:A given loading in one of these sections must be
#greater than any other loading in its row)
p_pls$crossloadings

sub.crossloadings <- p_pls$crossloadings %>% filter(block %in% reflectivemode) %>% dplyr::select(-PCON,-POPE)

#reshape crosslaoding data.frame for ggplot
xloads <- melt(sub.crossloadings, id.vars = c('name','block'), variable_name = 'LV')

#bar-charts of crossloadings by block for reflective mode.(check the diagosal one is always higher than others)
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  #add horizontal reference lines
  geom_hline(yintercept = 0, color = 'gray75') +
  geom_hline(yintercept = 0.5, color = 'gray70', linetype = 2) +
  #indicate the use of bar-charts
  geom_bar(stat = 'identity',position = 'dodge') +
  #panel display(e.g. faceting)
  facet_wrap(block ~ LV) +
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  #add title
  ggtitle('Crossloadings of reflective indicators')


#######################
#2.check the inner model(p value, R square,gof,reduancy)
######################
#inner model,similar with regression mode, p value(Pr(>|t|)) can be checked here.
p_pls$inner_model

#R square (R2 >0.5)
##HIgh redundancy means high ability to predict.
#AVE: how much of the block variability is reproducible by the latent variable.
p_pls$inner_summary

#GOF
p_pls$gof

#7.total effect
p_pls$effects
#select active rows
good_rows <- c(3:4,7:7,10:11)

#active effect in matrix format
path_effs <- as.matrix(p_pls$effects[good_rows,2:3])

#add rownames to path effs
rownames(path_effs) <- p_pls$effects[good_rows,1]
path_effs

#setting margin size
op <- par(mar = c(8,3,1,0.5))

#barplots of total effects(direct + indirect)
barplot(t(path_effs),
        border = NA,
        col = c('#9E9AC8','#DADAEB'),
        las = 2,
        cex.names = 0.8,
        legend = c('Direct','Indirect'),
        args.legend = list(x = 'top', 
                           ncol = 2, 
                           border = NA,
                           bty = 'n',
                           title = 'Effects'))
#resetting default margins
par(op)


#validation using bootstrapping.check the bootstrap result.
p_pls$boot

#GOF
p_pls$gof

#4.latent variable scores. Can be reused for further analysis, rescaled.
summary(p_pls$scores)

#plot histgram
op <- par(mfrow = c(2,3),mar = c(4,5,2,0.5))

#for each score
for (j in 1:(ncol(summary(p_pls$scores)) - 1)) {
  #histogram with probability density
  hist(p_pls$scores[,j],
       freq = FALSE,
       xlab = '',
       border = '#6A51A3',
       col = '#DADAEB',
       main = colnames(p_pls$scores)[j])
  #add axes
  axis(side = 1, col = 'gray70',col.axis = 'gray70')
  axis(side = 2, col = 'gray70',col.axis = 'gray70')
}
par(op)

#rescale the scores
Scores <- rescale(p_pls)

#summary
summary(Scores)

#plot histgram
op <- par(mfrow = c(2,3),mar = c(4,5,2,1.5),bty = 'n')

#for each score
for (j in 1:ncol(summary(Scores))) {
  #histogram with probability density
  hist(Scores[,j],
       axes = FALSE,
       xlim = c(1,7),
       ylim = c(0,125),
       xlab = '',
       border = '#6A51A3',
       col = '#DADAEB',
       main = colnames(Scores)[j])
  #add horizontal axes
  axis(side = 1, col = 'gray70',col.axis = 'gray70')
  #add vertical axis
  axis(side = 2, col = 'gray70',col.axis = 'gray70',las = 2)
}
par(op)

#scatterplot 
pairs(Scores,
      pch = 19,
      cex = 0.7,
      col = '#999bff',
      cex.axis = 0.8,
      col.axis = 'gray70',
      gap = 0.5)


###################################
#choose only n.con as personality(formative mode)
###################################
#select the variables which have association
df2 <- mergedBigFiveSocialCapital %>% dplyr::select(d_id,n.con,prod,util,ente,musi,QQ,zhifubao,QQ.f,zhifubao.f,setting.f,silence,sound,vibrate)
df2 <- as.data.frame(df2)


################################
#reflective mode (A) and formative mode (B).
################################
reflectivemode <- c('freq','usage','noti')

corrplot(cor(df2[,-1]))