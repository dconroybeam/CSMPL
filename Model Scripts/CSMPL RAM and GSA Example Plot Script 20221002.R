######Packages######
library(ggplot2)
library(reshape2)
library(igraph)
library(magick)
library(readr)



######Load Data#######

set.seed(02092020)

#Load in the data
alldata<-read.csv("Human Data/Processed Data/CAREER1 PROCESSED Data 01172020 104258.csv")

data<-alldata[alldata$CIN %in% sample(unique(alldata$CIN),15,replace=F),]

#Break into females and males
females<-data[data$sex==0,]
males<-data[data$sex==1,]

females<-females[order(females$CIN),]
males<-males[order(males$CIN),]

#Save figures?
sf<-1





######Functions######


#Attraction calculation#
mvcalc<-function(ideal,traits){
  #ideal is ideal preferences
  #traits is actual trait values

  #Calculates the Euclidean distance between the agent's preferences and each mate's traits
  #Transforms this distance such that positive values are more attractive by multiplying by -1 and adding 1 plus the maximum possible Euclidean distance
  mv<-apply(traits,1,function(x) 10*(-1*(dist(rbind(ideal,x)))+sqrt(10^2*16))/sqrt(10^2*16))

  
  return(mv)
  
}



#Improved Sampling#
#A sample function that will return only one value if asked to sample from a scalar
resample <- function(x,...){if(length(x)==1) x else sample(x,...)} 





#Mate Choice#
matechoice<-function(females,males,choiceType){
  
  ######Computing Attraction######
  
  #Males
  #Calculate how attracted each male is to each female
  maleattmatrix<-t(apply(males,1,function(q) mvcalc(as.numeric(q[235:250]),females[,c(345:360)])))
  
  colnames(maleattmatrix)<-females$PIN
  rownames(maleattmatrix)<-males$PIN
  
  
  #Females
  #Calculate how attracted each female is to each male
  femaleattmatrix<-t(apply(females,1,function(q) mvcalc(as.numeric(q[235:250]),males[,c(345:360)])))
  
  colnames(femaleattmatrix)<-males$PIN
  rownames(femaleattmatrix)<-females$PIN
  
  
  
  
  
  ######Mate Choice######
  
  if(choiceType=="ram"){
    #A matrix for storing how much investment each agent sends to each potential mate
    #Start with how attracted each agent is to each potential mate
    maleinvestprops<-maleattmatrix
    femaleinvestprops<-femaleattmatrix
    
    maleinvestprops<-t(apply(maleinvestprops,1,function(x) x*(10/sum(x))))
    femaleinvestprops<-t(apply(femaleinvestprops,1,function(x) x*(10/sum(x))))
    
    #Loop through 100 timesteps for courtship
    for(c in 1:50){
      #How much investment each male agent received from each female agent in the previous timestep
      maleinvestpropr<-t(femaleinvestprops)
      
      #How much investment each female agent received from each male agent in the previous timestep
      femaleinvestpropr<-t(maleinvestprops)
      
      #Calculate how much investment each agent will send in this round.
      #Investment after the first round is proportional to the mutual investment in the previous round
      maleinvestprops<-(maleinvestprops*maleinvestpropr)^.75
      femaleinvestprops<-(femaleinvestprops*femaleinvestpropr)^.75
      
      maleinvestprops<-t(apply(maleinvestprops,1,function(x) x*(10/sum(x))))
      femaleinvestprops<-t(apply(femaleinvestprops,1,function(x) x*(10/sum(x))))
      
      #Some models can produce NaNs. This just converts these to 0
      maleinvestprops[is.nan(maleinvestprops)]<-0
      femaleinvestprops[is.nan(femaleinvestprops)]<-0
      
      maleinvestprops2<-cbind(males$PIN,maleinvestprops)
      femaleinvestprops2<-cbind(females$PIN,femaleinvestprops)
      
      maleinvestprops2<-data.frame(maleinvestprops2)
      femaleinvestprops2<-data.frame(femaleinvestprops2)
      
      colnames(maleinvestprops2)<-c("PIN",females$PIN)
      colnames(femaleinvestprops2)<-c("PIN",males$PIN)
      
      #Melt this data so that the first two columns represent the investment giver and receiver
      #The third column represents investment sent
      maleinvestprops2<-melt(maleinvestprops2,id.vars=c("PIN"))
      femaleinvestprops2<-melt(femaleinvestprops2,id.vars=c("PIN"))
      
      #Combine these dataframes
      investmatrix<-rbind(maleinvestprops2,femaleinvestprops2)
      
      #Label the columns
      colnames(investmatrix)<-c("SPIN","MPIN","weight")
      
      #Store the sex of ach agent
      sexes<-rbind(males[,3:4],females[,3:4])
      
      #Convert PIN to a numeric
      investmatrix$MPIN<-as.numeric(levels(investmatrix$MPIN))[investmatrix$MPIN]
      
      #Create the graph
      g<-graph_from_data_frame(investmatrix,directed=T,vertices=sexes)
      
      #Make the weights of the edges proportional to the amount of investment
      E(g)$width<-E(g)$weight/3
      
      #Remove any edges that represent non-investment
      g<-delete.edges(g,which(E(g)$weight==0))
      
      #Set the color of the nodes based on agent sex
      V(g)$color<-ifelse(V(g)$sex==1,"orange","lightgreen")
      
      V(g)$label.cex<-.5
      
      if(c==1){
        l<-layout_with_graphopt(g,charge=.02,spring.constant=.33)
      } else{
        l<-layout_with_graphopt(g,charge=.02,start=lc,spring.constant=.10)
      }
      
      lc<-l
      
      plotname<-paste0("plot",sprintf("%03d",c),".png")
      
      png(file=paste0("Figures/RAM/",plotname),width=6,height=6,units="in",res=300)
      
      par(mar=c(0,0,0,0)+.1)
      # plot(g,layout=l,edge.arrow.size=0,edge.color="dodgerblue3",vertex.color="white",vertex.frame.color="dodgerblue3",vertex.label.color="dodgerblue3",vertex.label.font=1, vertex.size=10)
      
      plot(g,layout=l,edge.arrow.size=0,edge.color="dodgerblue3",vertex.frame.color="dodgerblue3",vertex.label.color="dodgerblue3", vertex.size=10)
      
      dev.off()
      
      #Determine which male each female has chosen
      fchoice<-data.frame("female"=1:nrow(females))
      fchoice$choice<-apply(femaleinvestprops,1,function(x) resample(which.max(x),1))
    }
    
    acc<-mean(fchoice[,1]==fchoice[,2])
    
  }
  
  if(choiceType=="gsa"){
    
    ######Gale Shapley Algorithm######
    
    #Redo MV matrices based on aggregate prefs rather than individuals
    maleattmatrix<-matrix(colMeans(maleattmatrix),nrow(maleattmatrix),ncol(maleattmatrix),byrow=T)
    
    colnames(maleattmatrix)<-females$PIN
    rownames(maleattmatrix)<-males$PIN
    
    femaleattmatrix<-matrix(colMeans(femaleattmatrix),nrow(femaleattmatrix),nrow(femaleattmatrix),byrow=T)
    
    colnames(femaleattmatrix)<-males$PIN
    rownames(femaleattmatrix)<-females$PIN
    
    #Reorder females by MV for plotting
    females<-females[order(colMeans(maleattmatrix),decreasing=T),]
    
    #Reorder MV matrices
    femaleattmatrix<-femaleattmatrix[order(colMeans(maleattmatrix),decreasing=T),]
    maleattmatrix<-maleattmatrix[,order(colMeans(maleattmatrix),decreasing=T)]
    
    #Exclude 1 couple for now
    excl<-sample(1:nrow(females),1)
    
    maleattmatrix2<-maleattmatrix[-excl,-excl]
    femaleattmatrix2<-femaleattmatrix[-excl,-excl]
    
    #Place the excluded couple at the end
    maleattmatrix<-maleattmatrix[c((1:nrow(maleattmatrix))[-excl],excl),c((1:nrow(maleattmatrix))[-excl],excl)]
    femaleattmatrix<-femaleattmatrix[c((1:nrow(femaleattmatrix))[-excl],excl),c((1:nrow(femaleattmatrix))[-excl],excl)]
    
    #Reorder again
    females<-females[order(colMeans(maleattmatrix),decreasing=T),]
    males<-males[c((1:nrow(males))[-excl],excl),]
    
    males2<-males[-nrow(males),]
    females2<-females[-nrow(females),]
    
    #Create a dataframe for plotting all couples
    plotdata<-data.frame("mv"=c(colMeans(maleattmatrix),colMeans(femaleattmatrix)),
                         "sex"=rep(c("F","M"),each=nrow(femaleattmatrix)),
                         "x"=rep(1:nrow(femaleattmatrix),2),
                         "y"=rep(c(3,1),each=nrow(femaleattmatrix))
                         )
    
    #Create a dataframe that excludes the last couple
    plotdata2<-plotdata[-c(nrow(females),2*nrow(females)),]
    
    #Place the last couple at the end
    plotdata[nrow(maleattmatrix),3:4]<-c(nrow(females),3)
    plotdata[2*nrow(maleattmatrix),3:4]<-c(nrow(males),1)
    
    #Create a temporary plot just to get colors for agents
    p<-qplot(x,y,color=as.factor(mv),data=plotdata2)+scale_color_discrete()
    p<-ggplot_build(p)
    cols<-unique(p$data[[1]])$colour
    plotdata2$cols<-cols
    
    #Transfer these colors to plotdata, assign the held out couple black so they stand out
    plotdata$cols<-c(plotdata2$cols[1:(nrow(maleattmatrix)-1)],"black",
                     plotdata2$cols[-(1:(nrow(maleattmatrix)-1))],"black")
    
    #Create temporary place vectors
    plotdata$x2<-plotdata$x
    plotdata$y2<-plotdata$y
    plotdata2$x2<-plotdata2$x
    plotdata2$y2<-plotdata2$y

    
    ###GSA With 1 Couple Excluded###
    
    #A vector of male agents' partners
    mpartner<-matrix(0,nrow(males2),1)
    
    #A vector of female agents' partners
    fpartner<-matrix(0,nrow(females2),1)
    
    #A matrix of females have already rejected each male
    mrejected<-matrix(0,nrow(males2),nrow(females2))
        
    
    #Create a variable to keep track of GSA steps
    step<-0
    
    #Create initial plot
    p<-qplot(x2,y2,color=I(plotdata2$cols),data=plotdata2,size=I(6.5))+
      geom_text(aes(x2,y2,label=1:nrow(plotdata2)),color=I("white"),fontface="bold")+
      theme_void()+theme(legend.position="none",
                         panel.background=element_rect(fill="white"))
    
    ggsave(paste0("Figures/GSA Excluded/",step,".png"),p,width=1800,height=1800,units="px")
    
    #Loops as long as some male is still unpaired
    while(sum(mpartner==0,na.rm=T)>0){
      
      step<-step+1
      
      #Selects the first unpaired male
      focalmale<-which(mpartner==0)[1]
      
      #Pulls out his attractions
      focalmaleatts<-maleattmatrix2[focalmale,]
      
      #Determines which females have already rejected him
      focalmalerejections<-mrejected[focalmale,]
      
      #If there is still a female left who has not rejected the focal male...
      if(sum(focalmalerejections)<nrow(females2)){
        
        #Sets the attractiveness of each female who has rejected the focal male to 0
        focalmaleatts[focalmalerejections==1]<-0
        
        #Determines which of the remaining females he is most attracted to
        femalechosen<-which(focalmaleatts==max(focalmaleatts))[1]
        
        #If she is single...
        if(fpartner[femalechosen]==0){
          #She provisionally gsapairs with him
          fpartner[femalechosen]<-focalmale
          
          #And he provisionally gsapairs with her
          mpartner[focalmale]<-femalechosen
          
          #If the chosen female is not single, she compares the attractiveness of the focal male to her current partner...
        } else if(femaleattmatrix2[femalechosen,focalmale]>femaleattmatrix2[femalechosen,fpartner[femalechosen]]){
          #If the focal male is more attractive to the chosen female than her current partner...
          
          #The chosen female rejects her current partner
          mrejected[fpartner[femalechosen],femalechosen]<-1
          
          #And he becomes single
          mpartner[fpartner[femalechosen]]<-0
          
          #And the focal male and the chosen female provisionally pair
          mpartner[focalmale]<-femalechosen
          fpartner[femalechosen]<-focalmale
        } else{
          #If the focal male is not more attractive to the chosen female than her current partner, the chosen female rejects the focal male
          mrejected[focalmale,femalechosen]<-1
        }
      } else{
        #If there is no female who has yet to reject the male, have him abandon search
        mpartner[focalmale]<-NA
      }
      
      plotdata2$x2[plotdata2$sex=="M"]<-sapply(1:length(mpartner),function(x)
        ifelse(mpartner[x]==0,plotdata2$x[plotdata2$sex=="M"][x],plotdata2$x[plotdata2$sex=="F"][mpartner[x]]))
        
      plotdata2$y2[plotdata2$sex=="M"]<-sapply(1:length(mpartner),function(x)
        ifelse(mpartner[x]==0,plotdata2$y[plotdata2$sex=="M"][x],plotdata2$y[plotdata2$sex=="F"][mpartner[x]]-.5))
      
      p<-qplot(x2,y2,color=I(plotdata2$cols),data=plotdata2,size=I(6.5))+
        geom_text(aes(x2,y2,label=1:nrow(plotdata2)),color=I("white"),fontface="bold")+
        theme_void()+theme(legend.position="none",
                           panel.background=element_rect(fill="white"))
      
      ggsave(paste0("Figures/GSA Excluded/",step,".png"),p,width=1800,height=1800,units="px")
      
    }
    
    
    
    ###GSA With that One Couple Re-Included
    
    #A vector of male agents' partners
    mpartner<-matrix(0,nrow(males),1)
    
    #A vector of female agents' partners
    fpartner<-matrix(0,nrow(females),1)
    
    #A matrix of females have already rejected each male
    mrejected<-matrix(0,nrow(males),nrow(females))
    
    
    #Create a variable to keep track of GSA steps
    step<-0
    
    #Create initial plot
    p<-qplot(x2,y2,color=I(plotdata$cols),data=plotdata,size=I(6.5))+
      geom_text(aes(x2,y2,label=1:nrow(plotdata)),color=I("white"),fontface="bold")+
      theme_void()+theme(legend.position="none",
                         panel.background=element_rect(fill="white"))
    
    ggsave(paste0("Figures/GSA Full/",step,".png"),p,width=1800,height=1800,units="px")
    
    #Loops as long as some male is still unpaired
    while(sum(mpartner==0,na.rm=T)>0){
      
      step<-step+1
      
      #Selects the first unpaired male
      focalmale<-which(mpartner==0)[1]
      
      #Pulls out his attractions
      focalmaleatts<-maleattmatrix[focalmale,]
      
      #Determines which females have already rejected him
      focalmalerejections<-mrejected[focalmale,]
      
      #If there is still a female left who has not rejected the focal male...
      if(sum(focalmalerejections)<nrow(females)){
        
        #Sets the attractiveness of each female who has rejected the focal male to 0
        focalmaleatts[focalmalerejections==1]<-0
        
        #Determines which of the remaining females he is most attracted to
        femalechosen<-which(focalmaleatts==max(focalmaleatts))[1]
        
        #If she is single...
        if(fpartner[femalechosen]==0){
          #She provisionally gsapairs with him
          fpartner[femalechosen]<-focalmale
          
          #And he provisionally gsapairs with her
          mpartner[focalmale]<-femalechosen
          
          #If the chosen female is not single, she compares the attractiveness of the focal male to her current partner...
        } else if(femaleattmatrix[femalechosen,focalmale]>femaleattmatrix[femalechosen,fpartner[femalechosen]]){
          #If the focal male is more attractive to the chosen female than her current partner...
          
          #The chosen female rejects her current partner
          mrejected[fpartner[femalechosen],femalechosen]<-1
          
          #And he becomes single
          mpartner[fpartner[femalechosen]]<-0
          
          #And the focal male and the chosen female provisionally pair
          mpartner[focalmale]<-femalechosen
          fpartner[femalechosen]<-focalmale
        } else{
          #If the focal male is not more attractive to the chosen female than her current partner, the chosen female rejects the focal male
          mrejected[focalmale,femalechosen]<-1
        }
      } else{
        #If there is no female who has yet to reject the male, have him abandon search
        mpartner[focalmale]<-NA
      }
      
      plotdata$x2[plotdata$sex=="M"]<-sapply(1:length(mpartner),function(x)
        ifelse(mpartner[x]==0,plotdata$x[plotdata$sex=="M"][x],plotdata$x[plotdata$sex=="F"][mpartner[x]]))
      
      plotdata$y2[plotdata$sex=="M"]<-sapply(1:length(mpartner),function(x)
        ifelse(mpartner[x]==0,plotdata$y[plotdata$sex=="M"][x],plotdata$y[plotdata$sex=="F"][mpartner[x]]-.5))
      
      p<-qplot(x2,y2,color=I(plotdata$cols),data=plotdata,size=I(6.5))+
        geom_text(aes(x2,y2,label=1:nrow(plotdata)),color=I("white"),fontface="bold")+
        theme_void()+theme(legend.position="none",
                           panel.background=element_rect(fill="white"))
      
      ggsave(paste0("Figures/GSA Full/",step,".png"),p,width=1800,height=1800,units="px")
      
    }
    
    
    mpartner<-mpartner[is.na(mpartner)==F]
    
    acc<-mean(mpartner==(1:length(mpartner)))
    
    
  }
  
  
  
  
  return(acc)
  
}



gsaacc<-100*matechoice(females,males,"gsa")
ramacc<-100*matechoice(females,males,"ram")


if(sf==1){
  imgs<-list.files("Figures/GSA Excluded/",full.names=T)
  imgs<-imgs[order(parse_number(imgs))]
  imgs<-c(imgs,rep(imgs[length(imgs)],10))
  img_list<-lapply(imgs,image_read)
  img_joined<-image_join(img_list)
  img_animated<-image_animate(img_joined,fps=2)
  image_write(image=img_animated,path="Figures/GSA Excluded.gif")
  
  file.remove(list.files(path="Figures/GSA Excluded/",pattern=".png",full.names=T))
  
  
  imgs<-list.files("Figures/GSA Full/",full.names=T)
  imgs<-imgs[order(parse_number(imgs))]
  imgs<-c(imgs,rep(imgs[length(imgs)],10))
  img_list<-lapply(imgs,image_read)
  img_joined<-image_join(img_list)
  img_animated<-image_animate(img_joined,fps=2)
  image_write(image=img_animated,path="Figures/GSA Full.gif")
  
  file.remove(list.files(path="Figures/GSA Full/",pattern=".png",full.names=T))
  
  
  imgs<-list.files("Figures/RAM/",full.names=T)
  img_list<-lapply(imgs,image_read)
  img_joined<-image_join(img_list)
  img_animated<-image_animate(img_joined,fps=2)
  image_write(image=img_animated,path="Figures/RAM.gif")
  
  file.remove(list.files(path="Figures/RAM/",pattern=".png",full.names=T))
}
