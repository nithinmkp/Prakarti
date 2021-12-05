#Packages
packages<-c("tidyverse","readxl","rio","RColorBrewer","formulaic","scales")
#sapply(packages,install.packages,character.only=T) #Uncomment this line if you dont have any of these packages installed
sapply(packages,library,character.only=T)

#folder to store all plots
plot_dir<-"ggplots"
if (!dir.exists(plot_dir)) {dir.create(plot_dir)}

#Use functions
source("functions.R")

#read data
sheets<-import_list("KDF.xlsx")
names(sheets)<-c("GDP_PC","GDP_PC_gr","RI_PC")
list2env(sheets,envir = globalenv())

#some wrangling on sheet3- Not in standard format
names(RI_PC)<-RI_PC[2,]
RI_PC<-RI_PC %>% slice(-c(1:2))
RI_PC<-RI_PC %>% pivot_longer(-`Country Name`,names_to = "Year",
                       values_to = "RI_PC") %>% 
  select(Country=`Country Name`,everything())

#GDP_PC_gr wrangling
GDP_PC_gr<-GDP_PC_gr %>% 
  mutate(`Growth Rate of GDP per capita`=`Growth Rate of GDP per capita`/100,
         Year=as.factor(Year))



#list of dataframes
df<-list(GDP_PC,GDP_PC,RI_PC,RI_PC,GDP_PC_gr,GDP_PC_gr)

#converting columns
df<-map(df,convert_fn,col_ind=c(1,2),fn=as.factor)
df<-map(df,convert_fn,col_ind=3,fn=as.numeric)

#plots
k<-tibble(catvar=rep(c("Year","Country"),times=3),
       leg_title=rep(c("Period","Country"),times=3),
       legend.pos.h=c(0.29,0.33,0.29,0.73,0.29,0.274),
       leg_row=c(3,4,3,5,3,5),
       df=df,
       xvar=rep(c("Log GDP Per Capita","RI_PC","Growth Rate of GDP per capita"),each=2),
       gr=as.logical(rep(c("FALSE","FALSE","TRUE"),each=2)),
       xlab=rep(c("Log GDP Per Capita","Relative Income Per capita",
              "Growth Rate of GDP per capita"),each=2),
       legend.pos.t=c(rep(1,2),0.11,rep(1,3))) %>% 
  mutate(plot=pmap(.,kd_fn))
  
  
  
k %>% 
  mutate(filename = paste0("ggplots/", xlab, leg_title,".png")) %>% 
    select(plot,filename) %>% 
  pwalk(ggsave,width = 22.5,
        height = 29.50,
        units = "cm",
        dpi = 300)



