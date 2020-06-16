setwd("~/Documents/R/CRT_MEP_SICI")

library(dplyr)
library(ggplot2)

#作図テーマ
theme_set(theme_classic(base_size = 18, base_family = "Helvetica"))

# データの読み込み
x <- read.csv("CRT_MEP_SICI_grand.txt", header = TRUE)
head(x)

#Trialcondをラベル付け
x$Sub_ID <- factor(x$Sub_ID)
x$TrialCond <- factor(x$TrialCond, levels=c(0,1,2), labels=c("rest", "go", 'nogo'))
x$sppp <- factor(x$sppp, levels=c(1,2), labels=c("sp", 'pp'))
#x$TMSdelay <- factor(x$TMSdelay)

# 試行平均
df <- x %>% group_by(Sub_ID, sppp, TrialCond, TMSdelay) %>%
  summarize(ampAPB.mean = mean(APBamp, na.rm=TRUE), ampAPB.sd = sd(APBamp, na.rm=TRUE),
            ampADM.mean = mean(ADMamp, na.rm=TRUE), ampADM.sd = sd(ADMamp, na.rm=TRUE),
            ampAPB.rest.mean = mean(APBamp_rest, na.rm=TRUE), ampAPB.rest.sd = sd(APBamp_rest, na.rm=TRUE),
            ampADM.rest.mean = mean(ADMamp_rest, na.rm=TRUE), ampADM.rest.sd = sd(ADMamp_rest, na.rm=TRUE),
            ampAPB.z = mean(APBamp_z, na.rm=TRUE), 
            ampADM.z = mean(ADMamp_z, na.rm=TRUE))
head(df)

# TMS delay除外
df <- df %>% filter(TMSdelay != 200)

# 各被験者平均反応時間
meanRT.sub <- x %>% group_by(Sub_ID) %>% summarize(meanRT_sub = mean(RT, na.rm=TRUE))
meanRTsub.faster <- meanRT.sub %>% arrange(meanRT_sub)

# 被験者数を取得
lab.sub <- levels(df$Sub_ID)
n.sub <- length(lab.sub)

g <- ggplot()

# 各被験者解析APB(raw)
pdf("~/Documents/R/CRT_MEP_SICI/sub_ampAPB.pdf")

for(i in 1:n.sub) {
data.sub <- filter(df, Sub_ID==lab.sub[i])

g[[i]] <- ggplot(data.sub,
                 aes(x = TMSdelay,
                     y = ampAPB.mean,
                     group = interaction(TrialCond,sppp),
                     colour = TrialCond,
                     linetype = sppp,
                     fill = sppp))
g[[i]] <- g[[i]] + 
  geom_line() + 
  geom_point(aes(colour = TrialCond, shape = TrialCond, fill = sppp), size = 4) + 
  geom_errorbar(aes(ymin = ampAPB.mean - ampAPB.sd,
                    ymax = ampAPB.mean + ampAPB.sd,
                    width = 3)) +
  geom_vline(xintercept=meanRT.sub$meanRT_sub[[i]], linetype="dashed", colour="gray")

g[[i]] <- g[[i]] + 
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"),
        legend.position = c(0, 1), legend.justification = c(0, 1)) + 
        scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-100,320))  +
        labs(x="TMS delay (ms)", y="Mean amplitude (mV)", title=paste("ampAPB_sub",lab.sub[i], sep = "" ))
plot(g[[i]])
}
dev.off()


# 各被験者解析APB(per rest)
pdf("~/Documents/R/CRT_MEP_SICI/sub_ampAPB_rest.pdf")
for(i in 1:n.sub) {
  data.sub.rest <- df %>% filter(Sub_ID==lab.sub[i],TrialCond=='go'|TrialCond=='nogo')
  
  g[[i]] <- ggplot(data.sub.rest,
                   aes(x = TMSdelay,
                       y = ampAPB.rest.mean,
                       group = interaction(TrialCond,sppp),
                       colour = TrialCond,
                       linetype = sppp))
  g[[i]] <- g[[i]] + 
    geom_line() + 
    geom_point(aes(colour = TrialCond, shape = TrialCond, fill = sppp), size = 4) + 
    geom_errorbar(aes(ymin = ampAPB.rest.mean - ampAPB.rest.sd,
                      ymax = ampAPB.rest.mean + ampAPB.rest.sd,
                      width = 3)) +
    geom_vline(xintercept=meanRT.sub$meanRT_sub[[i]], linetype="dashed", colour="gray") +
    geom_hline(yintercept=1, linetype="solid", colour="gray")
    
  g[[i]] <- g[[i]] + 
    theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"),
        legend.position = c(0, 1), legend.justification = c(0, 1)) + 
        scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-100,320))  +
        labs(x="TMS delay (ms)", y="Amplitude (%rest)", title=paste("ampAPB_sub",lab.sub[i], sep = "" ))
  
  plot(g[[i]])
}
dev.off()


# 各被験者解析APB(z-score)
pdf("~/Documents/R/CRT_MEP_SICI/sub_ampAPB_z.pdf")
for(i in 1:n.sub) {
  data.sub.z <- filter(df, Sub_ID==lab.sub[i])
  
  g[[i]] <- ggplot(data.sub.z,
                   aes(x = TMSdelay,
                       y = ampAPB.z,
                       group = interaction(TrialCond,sppp),
                       colour = TrialCond,
                       linetype = sppp))
  g[[i]] <- g[[i]] + 
    geom_line() + 
    geom_point(aes(colour = TrialCond, shape = TrialCond, fill = sppp), size = 4) + 
    geom_vline(xintercept=meanRT.sub$meanRT_sub[[i]], linetype="dashed", colour="gray")
  
  g[[i]] <- g[[i]] + 
    theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
                           axis.ticks=element_line(colour = "black"),
          legend.position = c(0, 1), legend.justification = c(0, 1)) + 
          scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-100,320))  +
          scale_y_continuous(breaks=seq(-0.5,2,by=0.5),limits=c(-1,2.4)) +
    labs(x="TMS delay (ms)", y="Amplitude (z-score)", title=paste("ampAPB_sub",lab.sub[i], sep = "" ))
  
  plot(g[[i]])
}
dev.off()

# 各被験者解析APB(z-score)
pdf("~/Documents/R/CRT_MEP_SICI/sub_ampAPB_z2.pdf")
g <- ggplot( df, aes( x = TMSdelay, y = ampAPB.z, group = interaction(Sub_ID , TrialCond), colour = TrialCond ) ) 
g <- g + geom_line()
g <- g + 
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"),
        legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  labs(x="TMS delay (ms)", y="Amplitude (z-score)", title=paste("ampAPB_sub",lab.sub[i], sep = "" ))
plot(g)
dev.off()


# grandave(作図用)
ga_df <- df %>% 
  group_by(sppp, TrialCond, TMSdelay) %>% 
  summarize(ampAPB.grandave.mean = mean(ampAPB.rest.mean, na.rm=TRUE), 
            ampAPB.grandave.sd = sd(ampAPB.rest.mean, na.rm=TRUE),
            ampAPB.z.grandave.mean= mean(ampAPB.z, na.rm=TRUE),
            ampAPB.z.grandave.sd= sd(ampAPB.z, na.rm=TRUE),
            ampADM.grandave.mean = mean(ampADM.rest.mean, na.rm=TRUE), 
            ampADM.grandave.sd = sd(ampADM.rest.mean, na.rm=TRUE),
            ampADM.z.grandave.mean= mean(ampADM.z, na.rm=TRUE),
            ampADM.z.grandave.sd= sd(ampADM.z, na.rm=TRUE))
head(ga_df)

SICI_df <- df %>% select(Sub_ID, sppp, TrialCond, TMSdelay, ampAPB.mean, ampADM.mean) %>% gather(emgloc, amplitude, -Sub_ID, -TrialCond, -TMSdelay, -sppp) %>% 
  spread(sppp, amplitude) %>% mutate(spppratio = pp/sp) %>% select(TMSdelay, emgloc, spppratio) %>% spread(emgloc, spppratio)
SICI_ga_df <- SICI_df %>% group_by(TrialCond, TMSdelay) %>%
  summarize(siciAPB.grand.mean = mean(ampAPB.mean, na.rm=TRUE),
            siciAPB.grand.sd = sd(ampAPB.mean, na.rm=TRUE),
            siciADM.grand.mean = mean(ampADM.mean, na.rm=TRUE),
            siciADM.grand.sd = sd(ampADM.mean, na.rm=TRUE))
SICI_ga_df_rest <- SICI_ga_df %>% filter(TrialCond == "rest")



# grandave作図（/rest）
pdf("~/Documents/R/CRT_MEP_SICI/grand_amp_rest.pdf")

ga_df_trial <- ga_df %>% filter(TrialCond != "rest")
ga_df_rest <- ga_df %>% filter(TrialCond == "rest")

# APB
g <- ggplot(
  ga_df_trial,
  aes(
    x = TMSdelay,
    y = ampAPB.grandave.mean,
    group = interaction(TrialCond,sppp),
    colour = TrialCond,
    linetype = sppp))

g <- g + 
  geom_vline(xintercept = 0, linetype="dashed", colour="gray") +
  geom_line() + geom_point(aes(colour = TrialCond, shape = TrialCond, fill = sppp), size = 4)
g <- g + geom_errorbar(
  aes(
    ymin = ampAPB.grandave.mean - ampAPB.grandave.sd/sqrt(n.sub),
    ymax = ampAPB.grandave.mean + ampAPB.grandave.sd/sqrt(n.sub),
    width = 3)) + 
  geom_hline(yintercept=1, linetype="solid", colour="gray")

g <- g + 
  scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-100,180)) + 
  scale_y_continuous(breaks=seq(0,10,by=2),limits=c(-1,10)) +
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
          axis.ticks=element_line(colour = "black"),
          legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  labs(x = "TMS delay (ms)", y = "Mean amplitude (%rest)", title="ampAPB (%rest±SEM)" )
plot(g)

# ADM
g <- ggplot(
  ga_df_trial,
  aes(
    x = TMSdelay,
    y = ampADM.grandave.mean,
    group = interaction(TrialCond,sppp),
    colour = TrialCond,
    linetype = sppp))

g <- g + 
  geom_vline(xintercept = 0, linetype="dashed", colour="gray") +
  geom_line() + geom_point(aes(colour = TrialCond, shape = TrialCond, fill = sppp), size = 4)
g <- g + geom_errorbar(
  aes(
    ymin = ampADM.grandave.mean - ampADM.grandave.sd/sqrt(n.sub),
    ymax = ampADM.grandave.mean + ampADM.grandave.sd/sqrt(n.sub),
    width = 3)) + 
  geom_hline(yintercept=1, linetype="solid", colour="gray")

g <- g + 
  scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-100,180)) + 
  scale_y_continuous(breaks=seq(0,10,by=2),limits=c(-1,10)) +
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"),
        legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  labs(x = "TMS delay (ms)", y = "Mean amplitude (%rest)", title="ampADM (%rest±SEM)" )
plot(g)
dev.off()


# grandave作図（z-score）
pdf("~/Documents/R/CRT_MEP_SICI/grand_amp_z.pdf")
# APB
g <- ggplot(
  ga_df_trial,
  aes(
    x = TMSdelay,
    y = ampAPB.z.grandave.mean,
    group = interaction(TrialCond,sppp),
    colour = TrialCond,
    linetype = sppp, shape = TrialCond))
g <- g +
  geom_vline(xintercept = 0, linetype="dashed", colour="gray") +
  geom_line(size = 1) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(
    ymin = ampAPB.z.grandave.mean - ampAPB.z.grandave.sd/sqrt(n.sub),
    ymax = ampAPB.z.grandave.mean + ampAPB.z.grandave.sd/sqrt(n.sub),
    width = 3))
g <- g + 
  scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-50,180)) + 
  scale_y_continuous(breaks=seq(-0.5,1.5,by=0.5),limits=c(-0.6,1.6)) +
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"), 
        legend.position = c(0, 1), legend.justification = c(0, 1)) +
  labs(x = "TMS delay (ms)", y = "MEP amplitude (z-score)", title="ampAPB (z-score±SEM)" )
plot(g)

# ADM
g <- ggplot(
  ga_df_trial,
  aes(
    x = TMSdelay,
    y = ampADM.z.grandave.mean,
    group = interaction(TrialCond,sppp),
    colour = TrialCond,
    fill = sppp,
    linetype = sppp))
g <- g +
  geom_line() + 
  geom_vline(xintercept = 0, linetype="dashed", colour="gray") +
  geom_point(aes(colour = TrialCond, shape = TrialCond, fill = sppp), size = 4) + 
  geom_errorbar(aes(
    ymin = ampADM.z.grandave.mean - ampADM.z.grandave.sd/sqrt(n.sub),
    ymax = ampADM.z.grandave.mean + ampADM.z.grandave.sd/sqrt(n.sub),
    width = 3))
g <- g + 
  scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-50,180)) + 
  scale_y_continuous(breaks=seq(-0.5,1.5,by=0.5),limits=c(-0.6,1.6)) +
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"),
        legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  labs(x = "TMS delay (ms)", y = "MEP amplitude (z-score)", title="ampADM (z-score±SEM)" )
plot(g)
dev.off()


# grandave作図（SICI）
pdf("~/Documents/R/CRT_MEP_SICI/grand_sici.pdf")
SICI_ga_df <- SICI_ga_df %>% filter(TrialCond !=  "rest")
# APB
g <- ggplot(
  SICI_ga_df,
  aes(
    x = TMSdelay,
    y = siciAPB.grand.mean,
    group = TrialCond,
    colour = TrialCond
    ))
g <- g +
  geom_hline(yintercept = 1, color = 'black') +
  geom_vline(xintercept = 0, linetype="dashed", colour="gray") +
  geom_line(size = 1.2, position = position_dodge(10)) + 
  geom_point(aes(colour = TrialCond, shape = TrialCond), size = 4, position = position_dodge(10)) + 
  geom_errorbar(aes(
    ymin = siciAPB.grand.mean - siciAPB.grand.sd/sqrt(n.sub),
    ymax = siciAPB.grand.mean + siciAPB.grand.sd/sqrt(n.sub),
    width = 3), position = position_dodge(10)) +
  geom_hline(yintercept = SICI_ga_df_rest$siciAPB.grand.mean, color = 'gray') + 
  annotate("rect", xmin=-Inf,xmax=Inf,
            ymin=SICI_ga_df_rest$siciAPB.grand.mean - SICI_ga_df_rest$siciAPB.grand.sd/sqrt(n.sub),
            ymax=SICI_ga_df_rest$siciAPB.grand.mean + SICI_ga_df_rest$siciAPB.grand.sd/sqrt(n.sub),
            fill="gray",alpha=0.2)

g <- g + 
  scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-50,170)) + 
  scale_y_continuous(breaks=seq(0.5,1.5,by=0.5),limits=c(0.5,1.5)) +
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"),
        legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  labs(x = "TMS delay (ms)", y = "PP/SP ratio", title="siciAPB" )
plot(g)

# ADM
g <- ggplot(
  SICI_ga_df,
  aes(
    x = TMSdelay,
    y = siciADM.grand.mean,
    group = TrialCond,
    colour = TrialCond
  ))
g <- g +
  geom_hline(yintercept = 1, color = 'black') +
  geom_vline(xintercept = 0, linetype="dashed", colour="gray") +
  geom_line(size = 1.2, position = position_dodge(10)) + 
  geom_point(aes(colour = TrialCond, shape = TrialCond), size = 4, position = position_dodge(10)) + 
  geom_errorbar(aes(
    ymin = siciADM.grand.mean - siciADM.grand.sd/sqrt(n.sub),
    ymax = siciADM.grand.mean + siciADM.grand.sd/sqrt(n.sub),
    width = 3), position = position_dodge(10)) +
  geom_hline(yintercept = SICI_ga_df_rest$siciADM.grand.mean, color = 'gray') + 
  annotate("rect", xmin=-Inf,xmax=Inf,
           ymin=SICI_ga_df_rest$siciADM.grand.mean - SICI_ga_df_rest$siciADM.grand.sd/sqrt(n.sub),
           ymax=SICI_ga_df_rest$siciADM.grand.mean + SICI_ga_df_rest$siciADM.grand.sd/sqrt(n.sub),
           fill="gray",alpha=0.2)

g <- g + 
  scale_x_continuous(breaks=seq(-40,160,by=40),limits=c(-50,170)) + 
  scale_y_continuous(breaks=seq(0.5,1.5,by=0.5),limits=c(0.5,1.5)) +
  theme(axis.line=element_line(linetype = 'solid', colour = "black", size=1),
        axis.ticks=element_line(colour = "black"),
        legend.position = c(0, 1), legend.justification = c(0, 1)) + 
  labs(x = "TMS delay (ms)", y = "PP/SP ratio", title="siciADM" )
plot(g)
dev.off()



#heatmap#
df.go <- df %>% filter(TrialCond=='go')
df.nogo <- df %>% filter(TrialCond=='nogo')
df.go.nogo.subt <- transform(df.go, ampAPB.z.subt = df.go$ampAPB.z-df.nogo$ampAPB.z ) 

h<-ggplot(df.go,aes(TMSdelay,Sub_ID)) +
  geom_tile(aes(fill=ampAPB.z)) +
  scale_fill_gradient2(limits=c(-2, 2), midpoint=0, low = "blue", mid = "white", high = "red") + 
  scale_y_discrete(limits=c(paste(meanRTsub.faster$Sub_ID))) +
  geom_point(data = meanRT.sub, aes(meanRT_sub, Sub_ID), size = 4, shape=1) + 
  scale_x_continuous(breaks=seq(60,400,by=40),limits=c(50,400)) + 
  theme(legend.title=element_blank())
plot(h)


h<-ggplot(df.nogo,aes(TMSdelay,Sub_ID)) +
  geom_tile(aes(fill=ampAPB.z)) +
  scale_fill_gradient2(limits=c(-2, 2), midpoint=0, low = "blue", mid = "white", high = "red") + 
  scale_y_discrete(limits=c(paste(meanRTsub.faster$Sub_ID))) +
  geom_point(data = meanRT.sub, aes(meanRT_sub, Sub_ID), size = 4, shape=1) + 
  scale_x_continuous(breaks=seq(60,400,by=40),limits=c(50,400)) + 
  theme(legend.title=element_blank())
plot(h)

h<-ggplot(df.go.nogo.subt,aes(TMSdelay,Sub_ID)) +
  geom_tile(aes(fill=ampAPB.z.subt)) +
  scale_fill_gradient2(midpoint=0.5, low = "white", mid = "white", high = "red") + 
  scale_y_discrete(limits=c(paste(meanRTsub.faster$Sub_ID))) +
  geom_point(data = meanRT.sub, aes(meanRT_sub, Sub_ID), size = 4, shape=1) + 
  scale_x_continuous(breaks=seq(60,400,by=40),limits=c(50,400)) + 
  theme(legend.title=element_blank())
plot(h)

dev.off()
