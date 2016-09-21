setwd("C:/Users/harin/Documents/Starr/data")
data = read.csv("StarrBOP_SDB_Data_Extract_2012.txt", sep="|")
data1 = read.csv("StarrBOP_SDB_Data_Extract_2013.txt", sep="|")
data2 = read.csv("StarrBOP_SDB_Data_Extract_2014.txt", sep="|")
data3 = read.csv("StarrBOP_SDB_Data_Extract_2015.txt", sep="|")
data4 = read.csv("StarrBOP_SDB_Data_Extract_2016.txt", sep="|")
a=rbind(data,data1)
a=rbind(a,data2)
a=rbind(a,data3)
a=rbind(a,data4)


# call source file with all required packages.
library(Hmisc)         # to describe the data
library(ggmap)
library(plotly)
library('tidyr')      # for reshaping data
library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(choroplethr)


dim(a) #[1] 1674987     139

describe(a)
#Number of Polcies made effective each year
#Number of Polices expired each year

a$PEff_year = year(as.Date(a$Policy_Effective_Date))
a$PExp_year = year(as.Date(a$Policy_Expiration_Date))
a$pbo_year = year(as.Date(a$Policy_Bound_Date ))
a$pac_year =  year(as.Date(a$Policy_Cancelled_Date ))

save(a,file="pickle.rda")
load("pickle.rda")

pol_eff_num = a[,c('Policy_Number','PEff_year')] %>% group_by(PEff_year) %>% summarise(Num =n())
pol_exp_num = a[,c('Policy_Number','PExp_year')] %>% group_by(PExp_year) %>% summarise(Num =n( ))
pol_bo_num = a[,c('Policy_Number','pbo_year')] %>% group_by(pbo_year) %>% summarise(Num =n( ))
pol_can_num = a[,c('Policy_Number','pac_year')] %>% group_by(pac_year) %>% summarise(Num =n( ))
pol_can_num = pol_can_num[pol_can_num$pac_year>=2012,]

pdf("Year1.pdf")
p1=ggplot(data=pol_eff_num, aes(x=PEff_year , y = Num , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Effective Policies Each Year")
p2=ggplot(data=pol_exp_num, aes(x=PExp_year , y = Num , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Exipred Policies Each Year")
p3=ggplot(data=pol_bo_num, aes(x=pbo_year , y = Num , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Bound Policies Each Year")
p4=ggplot(data=pol_can_num, aes(x=pac_year , y = Num , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Cancelled Policies Each Year")
grid.arrange(p1,p2,p3,p4 ,ncol = 2, top = "Trend Captured")
dev.off()
######## ------ 2012 Month wise Data --------#########
a$Mon_eff = month(as.Date(a$Policy_Effective_Date))
data_12_Eff  = a[a$PEff_year== 2012,c('Policy_Number','PEff_year', 'Policy_Effective_Date','Mon_eff')]
d12_Eff_Mon =  data_12_Eff %>% group_by(Mon_eff) %>% summarise(Month_wise_Count = n())

a$Mon_exp = month(as.Date(a$Policy_Expiration_Date))
data_12_Exp  = a[a$PExp_year== 2012,c('Policy_Number','PExp_year','Mon_exp')]
d12_Exp_Mon =  data_12_Exp %>% group_by(Mon_exp) %>% summarise(Month_wise_Count = n())

a$Mon_bou = month(as.Date(a$Policy_Bound_Date))
data_12_bou  = a[a$pbo_year== 2012,c('Policy_Number','pbo_year','Mon_bou')]
d12_bou_Mon =  data_12_bou %>% group_by(Mon_bou) %>% summarise(Month_wise_Count = n())

a$Mon_can = month(as.Date(a$Policy_Cancelled_Date))
data_12_can  = a[a$PEff_year== 2012,c('pac_year','pac_year','Mon_can')]
d12_can_Mon =  data_12_can %>% group_by(Mon_can) %>% summarise(Month_wise_Count = n())

p5=ggplot(data=d12_Eff_Mon, aes(x=Mon_eff , y = Month_wise_Count , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Effective Policies Each Month in 2012")
#p6=ggplot(data=d12_Exp_Mon, aes(x=Mon_exp , y = Month_wise_Count , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Exipred Policies Each Month in 2012")
p7=ggplot(data=d12_bou_Mon, aes(x=Mon_bou , y = Month_wise_Count , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Bound Policies Each Month in 2012")
p8=ggplot(data=d12_can_Mon, aes(x=Mon_can , y = Month_wise_Count , colour="blue")) + geom_line(size=1.5,stat="identity")+ ggtitle("Cancelled Policies Each Month in 2012")
pdf("Year2.pdf")
grid.arrange(p5,p7,p8 ,ncol = 2, top = "Trend Captured")
dev.off()
######## ------ 2013 Month wise Data --------#########
a$Mon_eff = month(as.Date(a$Policy_Effective_Date))
data_13_Eff  = a[a$PEff_year== 2013,c('Policy_Number','PEff_year', 'Policy_Effective_Date','Mon_eff')]
d13_Eff_Mon =  data_13_Eff %>% group_by(Mon_eff) %>% summarise(Month_wise_Count = n())

a$Mon_exp = month(as.Date(a$Policy_Expiration_Date))
data_13_Exp  = a[a$PExp_year== 2013,c('Policy_Number','PExp_year','Mon_exp')]
d13_Exp_Mon =  data_13_Exp %>% group_by(Mon_exp) %>% summarise(Month_wise_Count = n())

a$Mon_bou = month(as.Date(a$Policy_Bound_Date))
data_13_bou  = a[a$pbo_year== 2013,c('Policy_Number','pbo_year','Mon_bou')]
d13_bou_Mon =  data_13_bou %>% group_by(Mon_bou) %>% summarise(Month_wise_Count = n())

a$Mon_can = month(as.Date(a$Policy_Cancelled_Date))
data_13_can  = a[a$PEff_year== 2013,c('pac_year','pac_year','Mon_can')]
d13_can_Mon =  data_13_can %>% group_by(Mon_can) %>% summarise(Month_wise_Count = n())
pdf("2013M.pdf")
p9=ggplot(data=d13_Eff_Mon, aes(x=Mon_eff , y = Month_wise_Count , colour="blue")) + geom_line(color='orange',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Effective Policies Each Month in 2013")
p10=ggplot(data=d13_Exp_Mon, aes(x=Mon_exp , y = Month_wise_Count , colour="blue")) + geom_line(color='orange',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Exipred Policies Each Month in 2013")
p11=ggplot(data=d13_bou_Mon, aes(x=Mon_bou , y = Month_wise_Count , colour="blue")) + geom_line(color='orange',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Bound Policies Each Month in 2013")
p12=ggplot(data=d13_can_Mon, aes(x=Mon_can , y = Month_wise_Count , colour="blue")) + geom_line(color='orange',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Cancelled Policies Each Month in 2013")
grid.arrange(p9,p10,p11,p12 ,ncol = 2, top = "Monthly Trend Captured for 2013")
dev.off()

######## ------ 2014 Month wise Data --------#########
data_14_Eff  = a[a$PEff_year== 2014,c('Policy_Number','PEff_year', 'Policy_Effective_Date','Mon_eff')]
d14_Eff_Mon =  data_14_Eff %>% group_by(Mon_eff) %>% summarise(Month_wise_Count = n())

data_14_Exp  = a[a$PExp_year== 2014,c('Policy_Number','PExp_year','Mon_exp')]
d14_Exp_Mon =  data_14_Exp %>% group_by(Mon_exp) %>% summarise(Month_wise_Count = n())

data_14_bou  = a[a$pbo_year== 2014,c('Policy_Number','pbo_year','Mon_bou')]
d14_bou_Mon =  data_14_bou %>% group_by(Mon_bou) %>% summarise(Month_wise_Count = n())

data_14_can  = a[a$PEff_year== 2014,c('pac_year','pac_year','Mon_can')]
d14_can_Mon =  data_14_can %>% group_by(Mon_can) %>% summarise(Month_wise_Count = n())
pdf("2014M.pdf")
p13=ggplot(data=d14_Eff_Mon, aes(x=Mon_eff , y = Month_wise_Count , colour="blue")) + geom_line(color='green',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Effective Policies Each Month in 2014")
p14=ggplot(data=d14_Exp_Mon, aes(x=Mon_exp , y = Month_wise_Count , colour="blue")) + geom_line(color='green',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Exipred Policies Each Month in 2014")
p15=ggplot(data=d14_bou_Mon, aes(x=Mon_bou , y = Month_wise_Count , colour="blue")) + geom_line(color='green',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Bound Policies Each Month in 2014")
p16=ggplot(data=d14_can_Mon, aes(x=Mon_can , y = Month_wise_Count , colour="blue")) + geom_line(color='green',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ ggtitle("Cancelled Policies Each Month in 2014")
grid.arrange(p13,p14,p15,p16 ,ncol = 2, top = "Monthly Trend Captured for 2014")
dev.off()
######## ------ 2015 Month wise Data --------#########
data_15_Eff  = a[a$PEff_year== 2015,c('Policy_Number','PEff_year', 'Policy_Effective_Date','Mon_eff')]
d15_Eff_Mon =  data_15_Eff %>% group_by(Mon_eff) %>% summarise(Month_wise_Count = n())

data_15_Exp  = a[a$PExp_year== 2015,c('Policy_Number','PExp_year','Mon_exp')]
d15_Exp_Mon =  data_15_Exp %>% group_by(Mon_exp) %>% summarise(Month_wise_Count = n())

data_15_bou  = a[a$pbo_year== 2015,c('Policy_Number','pbo_year','Mon_bou')]
d15_bou_Mon =  data_15_bou %>% group_by(Mon_bou) %>% summarise(Month_wise_Count = n())

data_15_can  = a[a$PEff_year== 2015,c('pac_year','pac_year','Mon_can')]
d15_can_Mon =  data_15_can %>% group_by(Mon_can) %>% summarise(Month_wise_Count = n())
pdf("2015M.pdf")
p17=ggplot(data=d15_Eff_Mon, aes(x=Mon_eff , y = Month_wise_Count , colour="blue")) + geom_line(color='purple',size=1.5,stat="identity")+  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12))+ggtitle("Effective Policies Each Month in 2015")
p18=ggplot(data=d15_Exp_Mon, aes(x=Mon_exp , y = Month_wise_Count , colour="blue")) + geom_line(color='purple',size=1.5,stat="identity")+ scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +ggtitle("Exipred Policies Each Month in 2015")
p19=ggplot(data=d15_bou_Mon, aes(x=Mon_bou , y = Month_wise_Count , colour="blue")) + geom_line(color='purple',size=1.5,stat="identity")+ scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +ggtitle("Bound Policies Each Month in 2015")
p20=ggplot(data=d15_can_Mon, aes(x=Mon_can , y = Month_wise_Count , colour="blue")) + geom_line(color='purple',size=1.5,stat="identity")+ scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +ggtitle("Cancelled Policies Each Month in 2015")
grid.arrange(p17,p18,p19,p20 ,ncol = 2, top = "Monthly Trend Captured for 2015")
dev.off()
######## ------ 2016 Month wise Data --------#########
data_16_Eff  = a[a$PEff_year== 2016,c('Policy_Number','PEff_year', 'Policy_Effective_Date','Mon_eff')]
d16_Eff_Mon =  d16_Eff_Mon %>% group_by(Mon_eff) %>% summarise(Month_wise_Count = n())

data_16_Exp  = a[a$PExp_year== 2016,c('Policy_Number','PExp_year','Mon_exp')]
d16_Exp_Mon =  data_16_Exp %>% group_by(Mon_exp) %>% summarise(Month_wise_Count = n())

data_16_bou  = a[a$pbo_year== 2016,c('Policy_Number','pbo_year','Mon_bou')]
d16_bou_Mon =  data_16_bou %>% group_by(Mon_bou) %>% summarise(Month_wise_Count = n())

data_16_can  = a[a$PEff_year== 2016,c('pac_year','pac_year','Mon_can')]
d16_can_Mon =  data_16_can %>% group_by(Mon_can) %>% summarise(Month_wise_Count = n())

pdf("2016M.pdf")
p17=ggplot(data=d16_Eff_Mon, aes(x=Mon_eff , y = Month_wise_Count , colour="blue")) + geom_line(color='steelblue',size=1.6,stat="identity")+ scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) + ggtitle("Effective Policies Each Month in 2016")
p18=ggplot(data=d16_Exp_Mon, aes(x=Mon_exp , y = Month_wise_Count , colour="blue")) + geom_line(color='steelblue',size=1.6,stat="identity")+ scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +ggtitle("Exipred Policies Each Month in 2016")
p19=ggplot(data=d16_bou_Mon, aes(x=Mon_bou , y = Month_wise_Count , colour="blue")) + geom_line(color='steelblue',size=1.6,stat="identity")+ scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) + ggtitle("Bound Policies Each Month in 2016")
p20=ggplot(data=d16_can_Mon, aes(x=Mon_can , y = Month_wise_Count , colour="blue")) + geom_line(color='steelblue',size=1.6,stat="identity")+ scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +ggtitle("Cancelled Policies Each Month in 2016")
grid.arrange(p17,p18,p19,p20 ,ncol = 2, top = "Monthly Trend Captured for 2016")
dev.off()
############ Policy Type Analysis ##############
unique(factor(a$Policy_Type) )
Pol_type_data =  a[,c('PEff_year', 'Policy_Type')]
Pol_type_data_1 =  Pol_type_data %>% group_by(PEff_year,Policy_Type) %>% summarise(No._of_each_Pol_type_effective = n())
Pol_type_data_1 = Pol_type_data_1[order(Pol_type_data_1$No._of_each_Pol_type_effective),]
pdf("PType.pdf")
ggplot(data=Pol_type_data_1, aes(x= PEff_year, y=No._of_each_Pol_type_effective))+ 
geom_bar(aes(fill = Policy_Type), stat="identity") + ggtitle("Year wise Trend of Primary and Umbrella ")+geom_text(aes(label = No._of_each_Pol_type_effective ), size = 4)
dev.off()

########## Coverage_Type_Code  ################
unique(a$Coverage_Type_Code) #Levels: BOP 105 109 CYC GLB
BOP = a[a$Coverage_Type_Code== "BOP",c("Risk_Location_State_Code","Risk_Location_Postal_Zip_Code")]
BOP = BOP %>% group_by(Risk_Location_State_Code) %>% summarise(n =n())
D105 = a[a$Coverage_Type_Code== "105",c("Risk_Location_State_Code","Risk_Location_Postal_Zip_Code")]
D105 = D105 %>% group_by(Risk_Location_State_Code) %>% summarise(n =n())
D109 = a[a$Coverage_Type_Code== "109",c("Risk_Location_State_Code","Risk_Location_Postal_Zip_Code")]
D109 = D109 %>% group_by(Risk_Location_State_Code) %>% summarise(n =n())
CYC = a[a$Coverage_Type_Code== "CYC",c("Risk_Location_State_Code","Risk_Location_Postal_Zip_Code")]
CYC = CYC %>% group_by(Risk_Location_State_Code) %>% summarise(n =n())
GLB = a[a$Coverage_Type_Code== "GLB",c("Risk_Location_State_Code","Risk_Location_Postal_Zip_Code")]
GLB = GLB %>% group_by(Risk_Location_State_Code) %>% summarise(n =n())
pdf("Maps.pdf")
state_choropleth(BOP ,title      = "US states with BOP policy density", legend  = "Number of Policies")
state_choropleth(CYC ,title      = "US states with CYC policy density",  num_colors = 2, legend  = "Number of Policies" )
state_choropleth(GLB ,title      = "US states with GLB policy density",   legend  = "Number of Policies" )
dev.off()



