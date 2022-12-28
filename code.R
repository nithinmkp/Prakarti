#Packages
source("functions.R")
packages<-c("tidyverse","readxl","MetBrewer","scales","rio","RColorBrewer","fs")
package_fn(packages)

#folder to store all plots
dir_create("ggplots")

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
df_names<-names(Filter(is.data.frame,as.list(.GlobalEnv)))
df<-map(df_names,~replicate(2,get(.x),simplify = F)) %>% flatten() %>% 
  set_names(rep(df_names,each=2)) 

#converting columns
df<-map(df,convert_fn,col_ind=c(1,2),fn=as.factor)
df<-map(df,convert_fn,col_ind=3,fn=as.numeric)
list2env(df[c(1,3,5)],envir = globalenv())

#variables to plot
catvar<-df[1:3] %>% map(~.x %>% select_if(is.factor) %>% names()) %>% unlist()
xvar<-df %>% map(~.x %>% select_if(is.numeric) %>% names()) %>% unlist()

#plots
tibble(catvar=catvar,
       legend.pos.h=c(0.33,0.29,0.73,0.29,0.274,0.29),
       leg_row=c(4,3,5,3,5,3),
       df=df,
       xvar=xvar,
       xlab=rep(c("Log GDP Per Capita","Relative Income Per capita",
                  "Growth Rate of GDP per capita"),each=2),
       legend.pos.t=c(rep(1,2),1,0.11,rep(1,2))) |> 
  mutate(leg_title =case_when(catvar=="Year"~"Period",
                              TRUE~catvar),
         gr=case_when(xvar=="Growth Rate of GDP per capita"~TRUE,
                      TRUE~FALSE))%>% 
  mutate(plot=pmap(.,kd_fn,pal_name="Hokusai1"))%>% 
  mutate(filename = paste0("ggplots/", xlab, leg_title,".png")) %>% 
  select(plot,filename) %>% 
  pwalk(ggsave,width = 22.5,
        height = 29.50,
        units = "cm",
        dpi = 300)
  
