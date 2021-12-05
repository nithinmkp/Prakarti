#Packages
packages<-c("tidyverse","readxl","rio","RColorBrewer","formulaic","scales")
#sapply(packages,install.packages,character.only=T) #Uncomment this line if you dont have any of these packages installed
sapply(packages,library,character.only=T)

#folder to store all plots
dir.create("ggplots")

#read data
sheets<-import_list("KDF.xlsx")
names(sheets)<-c("GDP_PC","GDP_PC_gr","RI_PC")
list2env(sheets,envir = globalenv())

#KD plot function
kd_fn<- function(df,xvar,catvar,alph=0.2,siz=1.5,xlab,ylab="Density",
                 title=NA,leg_title,gr=F,legend.pos.h=0.29,
                 legend.pos.t=1,ylab_acur=2,leg_row=3){
  ncolors<-df %>% select({{catvar}}) %>% n_distinct()
  my_colors<-colorRampPalette(brewer.pal(8,"Set2"))(ncolors)
  p1<-df %>% ggplot(aes(x=.data[[xvar]]))+
    geom_density(aes(color=.data[[catvar]]),
                 alpha=alph,
                 size=siz,
                 key_glyph="smooth")+
    theme_bw()+
    theme(legend.direction  ="vertical",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="darkblue"),
          legend.text = element_text(size = 9),
          legend.title.align = 0.5,
          legend.justification = "top",
          legend.position = c(legend.pos.h,legend.pos.t))+
    guides(col=guide_legend(nrow=leg_row, byrow=T))+
    labs(x=xlab,y=ylab,
         color=leg_title)+
    scale_color_manual(values = my_colors)
  if(gr==T){
    p1+scale_x_continuous(labels = label_percent(accuracy = ylab_acur))
  }else{
    p1
  }
}


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



#converting columns
convert_fn<-function(df, col_ind,fn) {
  df <- df %>% mutate(across(.cols = col_ind, .fns = fn))
}

df<-list(GDP_PC,GDP_PC,RI_PC,RI_PC,GDP_PC_gr,GDP_PC_gr)
df<-map(df,convert_fn,col_ind=c(1,2),fn=as.factor)
df<-map(df,convert_fn,col_ind=3,fn=as.numeric)
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



