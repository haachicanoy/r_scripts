######ANALISIS DE TENDENCIAS PARA EL VALLE######
#Written by: Lizeth Llanos
#Date: 04/02/2016
library(ggplot2)
library(trend)
library(ggthemes)

setwd("C:\\Users\\lllanos\\Desktop\\Info cenicaña")
files<-list.files("C:\\Users\\lllanos\\Desktop\\Info cenicaña",pattern= ".csv")
data<-lapply(files,read.csv,header=T)

dir.create("Graficos")

ylabs=c("Temperatura máxima (°C)","Temperatura mínima (°C)","Precipitación (mm)")
all_models=list()

v=5
for(j in 1:length(files)){

  x=data[[j]]
  main.x=substring(files[j],1,nchar(files[j])-4)
  x$month=factor(month.name[x$month],levels=month.name)
    models.res=rbind()

    for(i in 1:12){
      by_month=x[x$month==month.name[i],]
      by_month=ts(start = min(by_month$year),end = max(by_month$year),by_month[,v])
      sen.res=sens.slope(by_month)
      
      t <- (1:length(by_month))
      s.pred <- sen.res$intercept + sen.res$b.sen * t
      x[x$month==month.name[i],"predict"]<- s.pred
      
      models.res$intercep[i]=sen.res$intercept
      models.res$slope[i]=sen.res$b.sen
      models.res$slope_lo[i]=sen.res$b.sen.lo
      models.res$slope_up[i]=sen.res$b.sen.up
    
      x[x$month==month.name[i],"trend"]<-ifelse(models.res$slope_lo[i]<=0 & models.res$slope_up[i]>=0,"Sin tendencia", "Tendencia")
      
    }


  all_models[[j]]=as.data.frame(models.res)

  
  ggplot(x,aes_string(x=names(x)[2],y=names(x)[v]))+geom_line()+geom_line(aes(y=predict,colour=trend),size=1)+
  facet_wrap(~month)+ theme_bw()+scale_x_continuous(name = NULL,seq(min(x$year),max(x$year),2),seq(min(x$year),max(x$year),2))+
  theme(axis.text.x = element_text(angle = 90,size=10),plot.title = element_text(lineheight=.8, face="bold"))+
  ylab(ylabs[v+3])+ggtitle(paste(main.x))
  
  ggsave(paste0("Graficos/",main.x,".png"),scale = 1.5)

}


final=do.call("rbind",all_models)
final$mes=rep(month.abb,length(files))
final$mes=factor(final$mes,levels=month.abb)

final$estacion=rep(substring(files,1,nchar(files)-12),each=12)
final$trend=ifelse(final$slope_lo<=0 & final$slope_up>=0,"Sin tendencia", "Tendencia")
  
write.csv(final,paste0("Graficos/resultados",names(x)[v],".csv"))

ggplot(final, aes(x = mes, y = slope)) +
  geom_point(size = 4,aes(colour=trend))+ 
  geom_errorbar(aes(ymax = slope_up, ymin = slope_lo))+facet_wrap(~estacion,scales = "free_x")+theme_bw()
ggsave("Graficos/interval.png",scale = 1.5)


