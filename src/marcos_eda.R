library(ggplot2)
library(data.table)
library(dplyr)
library(MASS)
library(RColorBrewer)
library(Hmisc)
library(ggpubr)
library(tidyverse)
library(here)

# let R understand the path to the git dir
indir <- here()

# load the paths specified in function paths
source(file.path(indir, 'functions/paths.R'))

# load the data
load(path.pairs.hh.tsi)

#Divide in 5 years age bands 
age_limits <- c(15, 20, 25, 30, 35, 40, 45, 50)
age_labels <- paste0(age_limits, '-', age_limits + 5)
pairs_tsi$AGE_BAND.RECIPIENT<-cut(
    pairs_tsi$AGE_INFECTION.RECIPIENT,
    breaks=age_limits, 
    labels=age_labels[1:(length(age_limits) - 1)], 
    include.lowest=TRUE)

#Exclude respondants in 'neuro' community
pairs_tsi <- pairs_tsi[COMM.SOURCE!='neuro' & COMM.RECIPIENT!='neuro',]

#Divide in 2 datasets for transmissions within and out of HH
pairs_tsi_same_hh<-pairs_tsi[same_hh==1,]
pairs_tsi_diff_hh<-pairs_tsi[same_hh==0,]


################################### BY RECIPIENT COMMUNITY AND HOUSEHOLD FACTOR ###########################

# AB: it's clearer to perform operations here. 
# less prone to bugs, more readable, and can focus on graphics only in ggplot snippet
p1_data <- pairs_tsi[ , {
    N.in.inland <- sum(COMM.RECIPIENT == 'inland')
    binom.confs <- (.N*binconf(N.in.inland, .N)) |> as.list()
    names(binom.confs) <- c('BC.center', 'BC.min', 'BC.max')
    binom.confs
}, by=same_hh]

p1_new <- ggplot(pairs_tsi, aes(x = as.logical(same_hh),fill = COMM.RECIPIENT)) +
    geom_bar()+
    geom_errorbar(data=p1_data, 
        aes( ymin=BC.min,ymax=BC.max, fill=NA), 
    width=0.4, colour="black", alpha=0.9, linewidth=1.3) +
    labs(x='transmission type',
        y='number of infections',
        fill='recipient community') +
    scale_x_discrete(labels=c("out of hh","within hh")) 


p2<-ggplot(data = pairs_tsi, aes(x = as.logical(same_hh),fill = COMM.RECIPIENT)) +
  geom_bar(position="fill") +
  labs(x='Transmission type',
    y='Proportion of infections',
    fill='Recipient community') +
  scale_x_discrete(labels=c("Out of HH","Within HH")) +
  geom_errorbar( aes(x='FALSE', ymin=binconf(nrow(pairs_tsi_diff_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi_diff_hh))[2], ymax=binconf(nrow(pairs_tsi_diff_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi_diff_hh))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='TRUE', ymin=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi_same_hh))[2], ymax=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi_same_hh))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)

p<-ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
p2<-annotate_figure(p, top="Colored by recipient community")
annotate_figure(p2, top="Infections within and out of household")

table(as.logical(pairs_tsi$same_hh),pairs_tsi$COMM.RECIPIENT)
binconf(216,610)*610
binconf(175,610)*610
binconf(117,610)*610
binconf(102,610)*610


p3<-ggplot(data = pairs_tsi, aes(x = COMM.RECIPIENT,fill = as.logical(same_hh))) +
  geom_bar()+
  labs(x='Recipient community',
       y='Number of infections',
       fill='Transmission type') +
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  geom_errorbar( aes(x='fishing', ymin=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='fishing',]),nrow(pairs_tsi[COMM.RECIPIENT=='fishing',]))[2]*nrow(pairs_tsi[COMM.RECIPIENT=='fishing',]), ymax=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='fishing',]),nrow(pairs_tsi[COMM.RECIPIENT=='fishing',]))[3]*nrow(pairs_tsi[COMM.RECIPIENT=='fishing',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='inland', ymin=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi[COMM.RECIPIENT=='inland',]))[2]*nrow(pairs_tsi[COMM.RECIPIENT=='inland',]), ymax=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi[COMM.RECIPIENT=='inland',]))[3]*nrow(pairs_tsi[COMM.RECIPIENT=='inland',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)
  
  

p4<-ggplot(data = pairs_tsi, aes(x = COMM.RECIPIENT,fill = as.logical(same_hh))) +
  geom_bar(position='fill')+
  labs(x='Recipient community',
       y='Proportion of infections',
       fill='Transmission type') +
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  geom_errorbar( aes(x='fishing', ymin=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='fishing',]),nrow(pairs_tsi[COMM.RECIPIENT=='fishing',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='fishing',]),nrow(pairs_tsi[COMM.RECIPIENT=='fishing',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='inland', ymin=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi[COMM.RECIPIENT=='inland',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[COMM.RECIPIENT=='inland',]),nrow(pairs_tsi[COMM.RECIPIENT=='inland',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)

p<-ggarrange(p3, p4, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
p2<-annotate_figure(p, top="Colored by transmission type")
annotate_figure(p2, top="Infections by recipient community")



################################### BY SEX AND HOUSEHOLD FACTOR ###########################
p1<-ggplot(data = pairs_tsi, aes(x = SEX.SOURCE,fill = as.logical(same_hh))) +
  geom_bar()+
  labs(x='Sex of the source',
    y='Number of infections',
    fill='Transmission type') +
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  geom_errorbar( aes(x='F', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[2]*nrow(pairs_tsi[SEX.SOURCE=='F',]), ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[3]*nrow(pairs_tsi[SEX.SOURCE=='F',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='M', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[2]*nrow(pairs_tsi[SEX.SOURCE=='M',]), ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[3]*nrow(pairs_tsi[SEX.SOURCE=='M',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)

p2<-ggplot(data = pairs_tsi, aes(x = SEX.SOURCE,fill = as.logical(same_hh))) +
  geom_bar(position = "fill")+
  labs(x='Sex of the source',
       y='Number of infections',
       fill='Transmission type') +
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  geom_errorbar( aes(x='F', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='M', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)

p<-ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
p2<-annotate_figure(p, top="Colored by transmission type")
annotate_figure(p2, top="Infections by sex of the source")



#### Other way around:
p1<-ggplot(data = pairs_tsi, aes(x = as.logical(same_hh),fill = SEX.SOURCE)) +
  geom_bar()+
  labs(x='Transmission type',
       y='Number of infections',
       fill='Sex of the source') +
  scale_x_discrete(labels=c("Out of HH","Within HH")) 
  #geom_errorbar( aes(x='F', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[2]*nrow(pairs_tsi[SEX.SOURCE=='F',]), ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[3]*nrow(pairs_tsi[SEX.SOURCE=='F',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  #geom_errorbar( aes(x='M', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[2]*nrow(pairs_tsi[SEX.SOURCE=='M',]), ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[3]*nrow(pairs_tsi[SEX.SOURCE=='M',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)

p2<-ggplot(data = pairs_tsi, aes(x = SEX.SOURCE,fill = as.logical(same_hh))) +
  geom_bar(position="fill") +
  labs(x='Transmission type',
       y='Number of infections',
       fill='Sex of the source') +
  scale_x_discrete(labels=c("Out of HH","Within HH")) 
  #geom_errorbar( aes(x='F', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi[SEX.SOURCE=='F',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  #geom_errorbar( aes(x='M', ymin=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi[SEX.SOURCE=='M',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)

p<-ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
p2<-annotate_figure(p, top="Colored by sex of the source")
annotate_figure(p2, top="Infections by transmission type")

#################### BY AGE BAND ######################

p1<-ggplot(data = pairs_tsi[is.na(AGE_BAND.RECIPIENT)==FALSE & SEX.RECIPIENT=='F',], aes(fill = as.logical(same_hh), x = COMM.RECIPIENT)) +
  geom_bar()+
  labs(fill='Transmission type',
       y='Number of infections',
       x='Recipient community')+
  facet_grid(.~ AGE_BAND.RECIPIENT, scales="free_y") +
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  scale_x_discrete(labels=c('F','I')) +
  theme(strip.text = element_blank()) +
  theme(legend.position='bottom')

p2<-ggplot(data = pairs_tsi[is.na(AGE_BAND.RECIPIENT)==FALSE & SEX.RECIPIENT=='M',], aes(fill = as.logical(same_hh), x = COMM.RECIPIENT)) +
  geom_bar()+
  labs(fill='Transmission type',
       y='Number of infections',
       x='Recipient community')+
  facet_grid(.~ AGE_BAND.RECIPIENT, scales="free_y") +
  scale_x_discrete(labels=c('F','I')) +
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  theme(legend.position='bottom')

p<-ggarrange(p1, p2, ncol=1, nrow=2, common.legend = TRUE, legend="right")
p2<-annotate_figure(p, top="Grouped by transmission type and recipient age at infection")
annotate_figure(p2, top="Infections by recipient community")

####################   BY ROUND   #####################
tmp<-pairs_tsi
tmp[is.na(ROUND.M)==TRUE,]$M
tmp[is.na(ROUND.M)==TRUE & year(M)<=2003,]$ROUND.M<-'before R10'
tmp[is.na(ROUND.M)==TRUE & year(M)>=2018,]$ROUND.M<-'after R18'
pairs_tsi<-tmp
pairs_tsi$ROUND.M<-factor(pairs_tsi$ROUND.M,levels=c('before R10','R010','R011','R012','R013','R014','R015','R016','R017','R018','after R18'))
pairs_tsi_same_hh<-pairs_tsi[same_hh==1,]
pairs_tsi_diff_hh<-pairs_tsi[same_hh==0,]

p2<-ggplot(data = pairs_tsi, aes(x = ROUND.M ,fill = as.logical(same_hh))) +
  geom_bar(position="fill")+
  labs(x='Round',
       y='Proportion of infections',
       fill='Same HH') +
  geom_errorbar( aes(x='before R10', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='before R10',]),nrow(pairs_tsi[ROUND.M=='before R10',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='before R10',]),nrow(pairs_tsi[ROUND.M=='before R10',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R010', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R010',]),nrow(pairs_tsi[ROUND.M=='R010',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R010',]),nrow(pairs_tsi[ROUND.M=='R010',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R011', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R011',]),nrow(pairs_tsi[ROUND.M=='R011',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R011',]),nrow(pairs_tsi[ROUND.M=='R011',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R012', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R012',]),nrow(pairs_tsi[ROUND.M=='R012',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R012',]),nrow(pairs_tsi[ROUND.M=='R012',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R013', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R013',]),nrow(pairs_tsi[ROUND.M=='R013',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R013',]),nrow(pairs_tsi[ROUND.M=='R013',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R014', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R014',]),nrow(pairs_tsi[ROUND.M=='R014',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R014',]),nrow(pairs_tsi[ROUND.M=='R014',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R015', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R015',]),nrow(pairs_tsi[ROUND.M=='R015',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R015',]),nrow(pairs_tsi[ROUND.M=='R015',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R016', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R016',]),nrow(pairs_tsi[ROUND.M=='R016',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R016',]),nrow(pairs_tsi[ROUND.M=='R016',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R017', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R017',]),nrow(pairs_tsi[ROUND.M=='R017',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R017',]),nrow(pairs_tsi[ROUND.M=='R017',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R018', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R018',]),nrow(pairs_tsi[ROUND.M=='R018',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R018',]),nrow(pairs_tsi[ROUND.M=='R018',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='after R18', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='after R18',]),nrow(pairs_tsi[ROUND.M=='after R18',]))[2], ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='after R18',]),nrow(pairs_tsi[ROUND.M=='after R18',]))[3]), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  scale_x_discrete(labels=c("before","R10",'R11','R12','R13','R14','R15','R16','R17','R18','after R18'))

       
p1<-ggplot(data = pairs_tsi, aes(x = ROUND.M,fill = as.logical(same_hh))) +
  geom_bar()+
  labs(x='Round',
       y='Number of infections',
       fill='Transmission type')+
  geom_errorbar( aes(x='before R10', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='before R10',]),nrow(pairs_tsi[ROUND.M=='before R10',]))[2]*nrow(pairs_tsi[ROUND.M=='before R10',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='before R10',]),nrow(pairs_tsi[ROUND.M=='before R10',]))[3]*nrow(pairs_tsi[ROUND.M=='before R10',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R010', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R010',]),nrow(pairs_tsi[ROUND.M=='R010',]))[2]*nrow(pairs_tsi[ROUND.M=='R010',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R010',]),nrow(pairs_tsi[ROUND.M=='R010',]))[3]*nrow(pairs_tsi[ROUND.M=='R010',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R011', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R011',]),nrow(pairs_tsi[ROUND.M=='R011',]))[2]*nrow(pairs_tsi[ROUND.M=='R011',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R011',]),nrow(pairs_tsi[ROUND.M=='R011',]))[3]*nrow(pairs_tsi[ROUND.M=='R011',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R012', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R012',]),nrow(pairs_tsi[ROUND.M=='R012',]))[2]*nrow(pairs_tsi[ROUND.M=='R012',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R012',]),nrow(pairs_tsi[ROUND.M=='R012',]))[3]*nrow(pairs_tsi[ROUND.M=='R012',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R013', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R013',]),nrow(pairs_tsi[ROUND.M=='R013',]))[2]*nrow(pairs_tsi[ROUND.M=='R013',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R013',]),nrow(pairs_tsi[ROUND.M=='R013',]))[3]*nrow(pairs_tsi[ROUND.M=='R013',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R014', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R014',]),nrow(pairs_tsi[ROUND.M=='R014',]))[2]*nrow(pairs_tsi[ROUND.M=='R014',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R014',]),nrow(pairs_tsi[ROUND.M=='R014',]))[3]*nrow(pairs_tsi[ROUND.M=='R014',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R015', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R015',]),nrow(pairs_tsi[ROUND.M=='R015',]))[2]*nrow(pairs_tsi[ROUND.M=='R015',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R015',]),nrow(pairs_tsi[ROUND.M=='R015',]))[3]*nrow(pairs_tsi[ROUND.M=='R015',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R016', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R016',]),nrow(pairs_tsi[ROUND.M=='R016',]))[2]*nrow(pairs_tsi[ROUND.M=='R016',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R016',]),nrow(pairs_tsi[ROUND.M=='R016',]))[3]*nrow(pairs_tsi[ROUND.M=='R016',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R017', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R017',]),nrow(pairs_tsi[ROUND.M=='R017',]))[2]*nrow(pairs_tsi[ROUND.M=='R017',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R017',]),nrow(pairs_tsi[ROUND.M=='R017',]))[3]*nrow(pairs_tsi[ROUND.M=='R017',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='R018', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R018',]),nrow(pairs_tsi[ROUND.M=='R018',]))[2]*nrow(pairs_tsi[ROUND.M=='R018',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='R018',]),nrow(pairs_tsi[ROUND.M=='R018',]))[3]*nrow(pairs_tsi[ROUND.M=='R018',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  geom_errorbar( aes(x='after R18', ymin=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='after R18',]),nrow(pairs_tsi[ROUND.M=='after R18',]))[2]*nrow(pairs_tsi[ROUND.M=='after R18',]), ymax=binconf(nrow(pairs_tsi_same_hh[ROUND.M=='after R18',]),nrow(pairs_tsi[ROUND.M=='after R18',]))[3]*nrow(pairs_tsi[ROUND.M=='after R18',])), width=0.4, colour="black", alpha=0.9, linewidth=1.3)+
  scale_fill_discrete(labels=c("Out of HH","Within HH")) +
  scale_x_discrete(labels=c("before","R10",'R11','R12','R13','R14','R15','R16','R17','R18','after R18'))
  
ggarrange(p1, p2, ncol=1, nrow=2, common.legend = TRUE, legend="right")

######################### BY AGE OF THE SOURCE AND RECIPIENT #####################################à 
p1<-ggplot(data = pairs_tsi[SEX.SOURCE=='M',], aes(x = AGE_INFECTION.RECIPIENT,y=AGE_TRANSMISSION.SOURCE,color=as.logical(same_hh) )) +
  geom_point(size=1)+
  geom_abline(intercept=0,slope=1,color='black')+
  geom_smooth(data=pairs_tsi_diff_hh[SEX.SOURCE=='M',],color='red',size=1,se=FALSE)+
  geom_smooth(data=pairs_tsi_same_hh[SEX.SOURCE=='M',],color='blue',size=1,se=FALSE)+
  xlim(15,50)+
  ylim(15,50)+
  labs(title='                    M to F',
       x='Age at transmission of the recipient',
       y='Age at transmission of the source',
       color='Transmission type')+
  scale_color_manual(values=c("red", "blue"),labels=c('Out of household','Within household'))

p2<-ggplot(data = pairs_tsi[SEX.SOURCE=='F',], aes(x = AGE_INFECTION.RECIPIENT,y=AGE_TRANSMISSION.SOURCE,color=as.logical(same_hh) )) +
  geom_point(size=1)+
  geom_abline(intercept=0,slope=1,color='black')+
  geom_smooth(data=pairs_tsi_diff_hh[SEX.SOURCE=='F',],color='red',size=1,se=FALSE)+
  geom_smooth(data=pairs_tsi_same_hh[SEX.SOURCE=='F',],color='blue',size=1,se=FALSE)+
  xlim(15,50)+
  ylim(15,50)+
  labs(title='                    F to M',
       x='Age at transmission of the recipient',
       y='Age at transmission of the source',
       color='Transmission type')+
  scale_color_manual(values=c("red", "blue"),labels=c('Out of household','Within household'))
p3<-ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(p3, top="Infections by age")


################   SOURCE AGE   ###################
library(plyr)
med.fac = ddply(pairs_tsi, .(SEX.SOURCE, same_hh), function(.d)
  data.frame(x=median(.d$AGE_TRANSMISSION.SOURCE)))

p1<-ggplot(data = pairs_tsi, aes(x = AGE_TRANSMISSION.SOURCE,color=as.logical(same_hh))) +
  geom_density(alpha=.2) +
  scale_color_manual(values=c("red", "blue"))+
  labs(title='Histogram of the age at transmission of sources',
       x='Age at transmission',
       color='Transmission type') +
  scale_color_manual(values=c("red", "blue"),labels=c('Out of household','Within household')) +
  facet_grid(.~ SEX.SOURCE, scales="free_y") +
  geom_vline(data=med.fac[c(2,4),], aes(xintercept=x),color="blue", linetype="dashed", size=.5)+
  geom_vline(data=med.fac[c(1,3),], aes(xintercept=x),color="red", linetype="dashed", size=.5)+
  theme(legend.position='bottom')

#Double check:
median(pairs_tsi_diff_hh[SEX.SOURCE=='M',]$AGE_TRANSMISSION.SOURCE)
median(pairs_tsi_same_hh[SEX.SOURCE=='M',]$AGE_TRANSMISSION.SOURCE)
median(pairs_tsi_diff_hh[SEX.SOURCE=='F',]$AGE_TRANSMISSION.SOURCE)
median(pairs_tsi_same_hh[SEX.SOURCE=='F',]$AGE_TRANSMISSION.SOURCE)

################   RECIPIENT AGE   ###################
med.fac = ddply(pairs_tsi, .(SEX.RECIPIENT, same_hh), function(.d)
  data.frame(x=median(.d$AGE_INFECTION.RECIPIENT)))
p2<-ggplot(data = pairs_tsi, aes(x = AGE_INFECTION.RECIPIENT,color=as.logical(same_hh))) +
  geom_density(alpha=.2)+
  scale_color_manual(values=c("red", "blue"))+
  labs(title='Histogram of the age at transmission of recipients',
       x='Age at transmission ',
       color='Transmission type') +
  scale_color_manual(values=c("red", "blue"),labels=c('Out of household','Within household')) +
  facet_grid(.~ SEX.RECIPIENT, scales="free_y") +
  geom_vline(data=med.fac[c(2,4),], aes(xintercept=x),color="blue", linetype="dashed", size=.5)+
  geom_vline(data=med.fac[c(1,3),], aes(xintercept=x),color="red", linetype="dashed", size=.5)+
  theme(legend.position='bottom')

#Double check:
median(pairs_tsi_diff_hh[SEX.RECIPIENT=='M',]$AGE_INFECTION.RECIPIENT)
median(pairs_tsi_same_hh[SEX.RECIPIENT=='M',]$AGE_INFECTION.RECIPIENT)
median(pairs_tsi_diff_hh[SEX.RECIPIENT=='F',]$AGE_INFECTION.RECIPIENT)
median(pairs_tsi_same_hh[SEX.RECIPIENT=='F',]$AGE_INFECTION.RECIPIENT)

ggarrange(p1, p2, ncol=1, nrow=2, common.legend = TRUE, legend="right")

###############  2-D KERNEL DENISTY ESTIMATION: ############################
library(ks)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
set.seed(1001)

#DIFFERENT HH, M to F
d <- matrix(c(pairs_tsi[as.logical(same_hh)==0 & SEX.SOURCE=='M', ]$AGE_INFECTION.RECIPIENT, pairs_tsi[as.logical(same_hh)==0 & SEX.SOURCE=='M',]$AGE_TRANSMISSION.SOURCE),ncol=2) %>% 
  magrittr::set_colnames(c("x", "y")) %>% 
  as_tibble()

kd<-ks::kde(d, compute.cont=TRUE, h=0.2)
get_contour <- function(kd_out=kd, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%", "50%" ,"80%", "90%"), ~get_contour(kd, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
  mutate(z = c(kd$estimate %>% t))

p1<-ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
  geom_path(aes(x, y, group = prob), 
            data=dat_out, colour = I("black")) +
  geom_text(aes(label = prob), data = 
              filter(dat_out, (prob%in% c("10%") & n_val==100 | prob%in% c("20%") & n_val==80) | prob%in% c("50%") & n_val==60 | prob%in% c("80%") & n_val==40 | prob%in% c("90%") & n_val==20),
            colour = I("black"), size =I(3))+
  xlim(15,50)+
  ylim(15,50)+
  geom_abline(intercept=0,slope=1,color='black')+
  scale_fill_gradient(low = "white", high = "red") +
  labs(x='Age of recipient at transmission ',
       y='Age of source at transmission') +
  theme_bw() +
  theme(legend.position = "none")


#SAME HH, M to F
d <- matrix(c(pairs_tsi[as.logical(same_hh)==1 & SEX.SOURCE=='M', ]$AGE_INFECTION.RECIPIENT, pairs_tsi[as.logical(same_hh)==1 & SEX.SOURCE=='M',]$AGE_TRANSMISSION.SOURCE),ncol=2) %>% 
  magrittr::set_colnames(c("x", "y")) %>% 
  as_tibble()

kd<-ks::kde(d, compute.cont=TRUE, h=0.2)
get_contour <- function(kd_out=kd, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%","50%","80%", "90%"), ~get_contour(kd, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
  mutate(z = c(kd$estimate %>% t))

p2<-ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
  geom_path(aes(x, y, group = prob), 
            data=dat_out, colour = I("black")) +
  geom_text(aes(label = prob), data = 
              filter(dat_out, (prob%in% c("10%") & n_val==100 | prob%in% c("20%") & n_val==80) | prob%in% c("50%") & n_val==60 | prob%in% c("80%") & n_val==40 | prob%in% c("90%") & n_val==20),
            colour = I("black"), size =I(3))+
  xlim(15,50)+
  ylim(15,50)+
  geom_abline(intercept=0,slope=1,color='black')+
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x='Age of recipient at transmission ',
       y='Age of source at transmission') +
  theme_bw() +
  theme(legend.position = "none")
p3<-ggarrange(p1,p2,ncol=2,nrow=1)
annotate_figure(p3,bottom=('Red: Out of household \nBlue: Within household'), top='Contour plots for infections M to F')

#DIFFERENT HH, F to M
d <- matrix(c(pairs_tsi[as.logical(same_hh)==0 & SEX.SOURCE=='F', ]$AGE_INFECTION.RECIPIENT, pairs_tsi[as.logical(same_hh)==0 & SEX.SOURCE=='F',]$AGE_TRANSMISSION.SOURCE),ncol=2) %>% 
  magrittr::set_colnames(c("x", "y")) %>% 
  as_tibble()

kd<-ks::kde(d, compute.cont=TRUE, h=0.2)
get_contour <- function(kd_out=kd, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%","50%","80%", "90%"), ~get_contour(kd, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
  mutate(z = c(kd$estimate %>% t))

p1<-ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
  geom_path(aes(x, y, group = prob), 
            data=dat_out, colour = I("black")) +
  geom_text(aes(label = prob), data = 
              filter(dat_out, (prob%in% c("10%") & n_val==100 | prob%in% c("20%") & n_val==80) | prob%in% c("50%") & n_val==60 | prob%in% c("80%") & n_val==40 | prob%in% c("90%") & n_val==20),
            colour = I("black"), size =I(3))+
  xlim(15,50)+
  ylim(15,50)+
  geom_abline(intercept=0,slope=1,color='black')+
  scale_fill_gradient(low = "white", high = "red") +
  labs(x='Age of recipient at transmission ',
       y='Age of source at transmission') +
  theme_bw() +
  theme(legend.position = "none")

#SAME HH, F to M
d <- matrix(c(pairs_tsi[as.logical(same_hh)==1 & SEX.SOURCE=='F', ]$AGE_INFECTION.RECIPIENT, pairs_tsi[as.logical(same_hh)==1 & SEX.SOURCE=='F',]$AGE_TRANSMISSION.SOURCE),ncol=2) %>% 
  magrittr::set_colnames(c("x", "y")) %>% 
  as_tibble()

kd<-ks::kde(d, compute.cont=TRUE, h=0.2)
get_contour <- function(kd_out=kd, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%","50%","80%", "90%"), ~get_contour(kd, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
  mutate(z = c(kd$estimate %>% t))

p2<-ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
  geom_path(aes(x, y, group = prob), 
            data=dat_out, colour = I("black")) +
  geom_text(aes(label = prob), data = 
              filter(dat_out, (prob%in% c("10%") & n_val==100 | prob%in% c("20%") & n_val==80) | prob%in% c("50%") & n_val==60 | prob%in% c("80%") & n_val==40 | prob%in% c("90%") & n_val==20),
            colour = I("black"), size =I(3))+
  xlim(15,50)+
  ylim(15,50)+
  geom_abline(intercept=0,slope=1,color='black')+
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x='Age of recipient at transmission ',
       y='Age of source at transmission') +
  theme_bw() +
  theme(legend.position = "none")
p3<-ggarrange(p1,p2,ncol=2,nrow=1)
annotate_figure(p3,bottom=('Red: Out of household \nBlue: Within household'), top='Contour plots for infections F to M')

#For the table
nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',])
nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',])
nrow(pairs_tsi_diff_hh[SEX.SOURCE=='M',])
nrow(pairs_tsi_diff_hh[SEX.SOURCE=='F',])
binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi))*nrow(pairs_tsi)
binconf(nrow(pairs_tsi_same_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi))*nrow(pairs_tsi)
binconf(nrow(pairs_tsi_diff_hh[SEX.SOURCE=='M',]),nrow(pairs_tsi))*nrow(pairs_tsi)
binconf(nrow(pairs_tsi_diff_hh[SEX.SOURCE=='F',]),nrow(pairs_tsi))*nrow(pairs_tsi)
