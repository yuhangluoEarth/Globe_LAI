library(ggplot2)
library(dplyr)
library(reshape2)
library(MatchIt)
library(readxl)
library(foreign)
library(openxlsx)
library(raveio)
###############
setwd('...\\Matching')
data.Globe.Point <- read.csv("Matching_Data.csv")
################
rownames(data.Globe.Point) <- data.Globe.Point$Order_reshape
data.Globe.Point[is.na(data.Globe.Point)] <-0
data.Globe.Point <- mutate(data.Globe.Point,change = ifelse((LUCC_Gain==0)&(LUCC_Loss==0),0,1))
setwd('...\\Matching')
############MATCH
m.out <- matchit(change ~ prec_yr_slope + temp_yr_slope + elevation + slope + highway_dis + population_dis + lai_mean + maxlc_Avva+Forest_Age,  #+LUCC_mean
                 data = data.Globe.Point,replace = TRUE,exact = c("Eco_ID","Country_ID","maxlc_Avlo"),
                 method = "nearest",ratio=3,caliper=0.25)
#                 method = "nearest",ratio=1,caliper=NULL)

m.data.i <- data.frame(m.out$match.matrix)
m.data.i <- transform(m.data.i,Order_reshape = rownames(m.data.i))
######################
######################
write.csv(m.data.i,"Matching_ID.csv")
######################
######################
m_out <- summary(m.out, interactions = F)
write.xlsx(data.frame(rownames(m_out$sum.all),m_out$sum.all),"summary_matchit_all.xlsx")
write.xlsx(data.frame(rownames(m_out$sum.matched),m_out$sum.matched),"summary_matchit_matched.xlsx")
write.xlsx(data.frame(rownames(m_out$reduction),m_out$reduction),"summary_matchit_reduction.xlsx")


pdf(file = 'histogram.pdf',height = 7, width = 8)
plot(m.out, type = "histogram")
dev.off()
pdf(file = 'love_plot2.pdf',height = 5, width = 6)
plot(summary(m.out, interactions = F),
     var.order = "unmatched")
dev.off()