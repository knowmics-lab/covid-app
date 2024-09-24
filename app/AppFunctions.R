#Number of colors: 42 (25 + 12 brewer Set1 + 5 brewer Set3)
#brewer.pal(12,"Set3")
#brewer.pal(5,"Set1")
plot.colors <- c("coral", "chartreuse3" ,"cyan","darkgoldenrod2" ,"mediumpurple1", "palevioletred", "violetred1","forestgreen","darkolivegreen3",
                 "skyblue1", "darkseagreen1", "dodgerblue", "burlywood4", "yellow", "grey60","tan1", "darkorchid4", "tomato", "plum1",
                 "darkkhaki", "brown1", "dodgerblue4", "mediumvioletred", "darksalmon", "darkslategray4",
                 "#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5", "#D9D9D9", "#BC80BD",
                 "#CCEBC5","#FFED6F","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00")
names(plot.colors) <- c("19A", "19B" ,"20A", "20B", "20C", "20D", "20E(EU1)" , "20F", "20G", 
                        "20H(Beta,V2)", "20I(Alpha,V1)", "20J(Gamma,V3)", "21A(Delta)","21B(Kappa)", 
                        "21C(Epsilon)", "21D(Eta)", "21E(Theta)", "21F(Iota)" , "21G(Lambda)", "21H(Mu)",
                        "21I(Delta)",  "21J(Delta)", "21K(Omicron)", "21L(Omicron)", "21M(Omicron)",
                        "22A(Omicron)","22B(Omicron)","22C(Omicron)","22D(Omicron)","22E(Omicron)","22F(Omicron)",
                        "23A(Omicron)","23B(Omicron)","23C(Omicron)","23D(Omicron)","23E(Omicron)","23F(Omicron)",
                        "23G(Omicron)","23H(Omicron)","23I(Omicron)","24A(Omicron)","24B(Omicron)") 

compute.prevalences <- function(clade.prevalences,start.interval,end.interval)
{
  # Compute percentages
  sub.clade.prevalences <- clade.prevalences[,c(1,start.interval:end.interval)]
  plot.data <- pivot_longer(data= sub.clade.prevalences,
                            cols = colnames(sub.clade.prevalences[,-1]),
                            values_to = "Frequency",
                            names_to = "Time")
  plot.data$Percentage <- apply(plot.data,1,function(vec){ 
    as.numeric(vec[3]) / sum(plot.data$Frequency[plot.data$Time==vec[2]]) *100 
  })
  plot.data$Percentage <- round(plot.data$Percentage,3)
  #Remove clades that are not present in that interval of time
  clade.freqs <- as.data.frame(plot.data  %>%
                                 group_by(Clade) %>%
                                 summarise(n = sum(Frequency)))
  present.clades <- clade.freqs[clade.freqs$n>0,"Clade"]
  plot.data <- as.data.frame(plot.data[plot.data$Clade %in% present.clades,])
  return(plot.data)
}

compute.clade.frequent.mut <- function(clade.mutation.rates,clade.prevalences,clade.set,start.interval,end.interval)
{
  if(start.interval==end.interval) {
    sub.mut.rates <- clade.mutation.rates[clade.mutation.rates$Clade %in% clade.set,c(1,2,3),drop=F]
    sub.clade.prevalences <- clade.prevalences[clade.prevalences$Clade %in% clade.set,c(1,2),drop=F]
  } else  {
    sub.mut.rates <- clade.mutation.rates[clade.mutation.rates$Clade %in% clade.set,c(1,2,start.interval:end.interval),drop=F]
    sub.clade.prevalences <- clade.prevalences[clade.prevalences$Clade %in% clade.set,c(1,(start.interval-1):(end.interval-1)),drop=F]
  }
  sub.mut.rates$Frequency <- rowSums(sub.mut.rates[,3:ncol(sub.mut.rates),drop=F])
  sub.mut.rates <- sub.mut.rates[sub.mut.rates$Frequency>0,]
  sub.clade.prevalences$Total <- rowSums(sub.clade.prevalences[,2:ncol(sub.clade.prevalences),drop=F])
  final.data <- merge(sub.mut.rates,sub.clade.prevalences[,c("Clade","Total")])
  final.data$Frequency <- round(final.data$Frequency/final.data$Total*100,3)
  final.data <- final.data[order(final.data$Clade,-final.data$Frequency),]
  final.data <- final.data[,c("Clade","Mutation","Frequency")]
  return(final.data)
}

compute.mut.frequencies <- function(mutation.rates,mutation.set,info)
{
  clade.rates <- mutation.rates[mutation.rates$Mutation %in% mutation.set,grepl("Mutation|Rate",names(mutation.rates)),drop=F]
  list.clades <- unique(unlist(strsplit(names(clade.rates)[-1],"_"))[seq(1,(ncol(clade.rates)-1)*2,2)])
  plot.data <- data.frame(Mutation=rep(clade.rates$Mutation,each=(ncol(clade.rates)-1)/2),
                          Clade=rep(list.clades,length(mutation.set)),
                          Frequency=as.numeric(t(clade.rates[,grepl(info,names(clade.rates))])))
  plot.data <- plot.data[plot.data$Frequency>0,]
  plot.data <- plot.data[order(plot.data$Mutation,plot.data$Clade),]
  return(plot.data)
}

make.prevalence.plot <- function(plot.data)
{
  x.lab <- unique(plot.data$Time)
  plot.data$Time <- factor(plot.data$Time, levels=x.lab)
  plot.data$Month <- as.numeric(plot.data$Time)
  #plot.data$Clade <- factor(plot.data$Clade)
  prevalence.plot <- ggplot(plot.data, aes(x=Month, y=Percentage, fill=Clade, label=Time)) +
    geom_area(stat="identity",linewidth=0.2, color="black") +
    scale_x_continuous(breaks=1:length(x.lab),labels=x.lab) +
    scale_fill_manual(values=plot.colors[unique(plot.data$Clade)]) +
    labs(x="Month",y="Prevalence (%)") +
    theme(axis.text.x=element_text(size=12, angle=60, color="black", vjust = 0.5),
          axis.text.y=element_text(size=12, color="black"),
          axis.title.x=element_text(size=17, face="bold", margin=margin(t=20)),
          axis.title.y=element_text(size=17, face="bold"),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_line(color="black", linewidth = 0.5),
          axis.ticks.length=unit(.25, "cm"),
          legend.text.align = 0)
  return(prevalence.plot)
}

make.clade.frequent.mut.plot <- function(plot.data,topk)
{
  plot.list <- list()
  clade.list <- unique(plot.data$Clade)
  for(clade in clade.list)
  {
    clade.data <- plot.data[plot.data$Clade==clade,]
    clade.data <- clade.data[1:min(topk,nrow(clade.data)),]
    clade.data$Mutation <- factor(clade.data$Mutation,levels = clade.data$Mutation)
    sub.plot <- ggplot(clade.data, aes(x=Mutation, y=Frequency,
                                     text=paste0(Mutation,"\n<b>Frequency:</b> ",Frequency,"%"))) +
      ggtitle(clade) + 
      geom_bar(stat="identity", fill="dodgerblue") +
      labs(x="Mutation",y="Frequency (%)") +
      theme(plot.title=element_text(size=18, face="bold", hjust=0.5),
            axis.text.x=element_text(size=12, angle=60, color="black", vjust = 0.5),
            axis.text.y=element_text(size=12, color="black"),
            axis.title.x=element_text(size=17, face="bold", margin=margin(t=20)),
            axis.title.y=element_text(size=17, face="bold"),
            legend.text = element_text(size=12),
            #legend.title = element_blank(),
            axis.line.x = element_line(color="black", linewidth = 0.5),
            axis.line.y = element_line(color="black", linewidth = 0.5),
            axis.ticks.length=unit(.25, "cm"),
            legend.position="none")
    plot.list[[clade]] <- sub.plot
  }
  return(plot.list)
}

make.temporal.plot <- function(mutation.rates, mutation.set)
{
  temporal.rates <- mutation.rates[mutation.rates$Mutation %in% mutation.set,!grepl("Rate|error",names(mutation.rates)),drop=F]
  error.rates <- mutation.rates[mutation.rates$Mutation %in% mutation.set,grepl("error",names(mutation.rates)),drop=F]
  if(ncol(error.rates)==0) {
    final.data <- data.frame(mutation=rep(temporal.rates$Mutation,each=ncol(temporal.rates)-1),
             month=rep(names(temporal.rates)[-1],length(mutation.set)),
             rate=as.numeric(t(temporal.rates[,-1])))
  } else {
    final.data <- data.frame(mutation=rep(temporal.rates$Mutation,each=ncol(temporal.rates)-1),
             month=rep(names(temporal.rates)[-1],length(mutation.set)),
             rate=as.numeric(t(temporal.rates[,-1])),
             error=as.numeric(t(error.rates)))
  }
  
  final.data$month <- factor(final.data$month,levels=unique(final.data$month))
  
  if(ncol(error.rates)==0) {
    temporal.plot <- ggplot(final.data, aes(x=month, y=rate, color=mutation, group=mutation, 
                                            text=paste0(month,"\n<b>Mutation:</b> ",mutation,"\n","<b>Rate:</b> ",rate,"%\n","<b>Error:</b> Not Available")))
  } else {
    temporal.plot <- ggplot(final.data, aes(x=month, y=rate, color=mutation, group=mutation, 
                                            text=paste0(month,"\n<b>Mutation:</b> ",mutation,"\n","<b>Rate:</b> ",rate,"%\n","<b>Error:</b> ",ifelse(is.na(error),"Not Available",paste0(error,"%"))))) +
                     geom_errorbar(aes(ymin = rate-error, ymax = rate+error), width=1.5)
  }
  
  temporal.plot <- temporal.plot + 
    geom_line(linewidth=0.75) + 
    geom_point(size=1.5) +
    #scale_y_continuous(breaks=seq(0,100,10)) +
    scale_color_discrete(name="") + 
    labs(x="Month",y="Mutation rate (%)") +
    theme(axis.text.x=element_text(size=12, angle=60, color="black", vjust = 0.5),
          axis.text.y=element_text(size=12, color="black"),
          axis.title.x=element_text(size=17, face="bold", margin=margin(t=20)),
          axis.title.y=element_text(size=17, face="bold"),
          legend.text = element_text(size=12),
          #legend.title = element_blank(),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_line(color="black", linewidth = 0.5),
          axis.ticks.length=unit(.25, "cm"),
          legend.text.align = 0)
  
  return(temporal.plot)
}

make.mut.frequency.plot <- function(plot.data)
{
  plot.list <- list()
  mutation.list <- unique(plot.data$Mutation)
  for(mutation in mutation.list)
  {
    sub.plot.data <- plot.data[plot.data$Mutation==mutation,]
    #sub.plot.data <- sub.plot.data[1:min(topk,nrow(sub.plot.data)),]
    sub.plot.data$Clade <- factor(sub.plot.data$Clade,levels=sub.plot.data$Clade)
    clade.colors <- plot.colors[as.character(sub.plot.data$Clade)]
    sub.plot <- ggplot(sub.plot.data, aes(x=Clade, y=Frequency,
                                          text=paste0(Clade,"\n<b>Frequency:</b> ",Frequency,"%"))) +
      ggtitle(mutation) + 
      geom_bar(stat="identity", fill=clade.colors) +
      #geom_text(aes(label = Frequency), vjust = -0.75, size=3) +
      labs(x="Clade",y="Frequency (%)") +
      theme(plot.title=element_text(size=18, face="bold", hjust=0.5),
            axis.text.x=element_text(size=12, angle=60, color="black", vjust = 0.5),
            axis.text.y=element_text(size=12, color="black"),
            axis.title.x=element_text(size=17, face="bold", margin=margin(t=20)),
            axis.title.y=element_text(size=17, face="bold"),
            legend.text = element_text(size=12),
            #legend.title = element_blank(),
            axis.line.x = element_line(color="black", linewidth = 0.5),
            axis.line.y = element_line(color="black", linewidth = 0.5),
            axis.ticks.length=unit(.25, "cm"),
            legend.position="none")
    plot.list[[mutation]] <- sub.plot
  }
  return(plot.list)
}

make.correlation.interactive.plot <- function(pair.rates,corr.plot.opt)
{
  mut.names <- pair.rates$Mutation
  if("Use clades" %in% corr.plot.opt)
  {
    pair.rates <- pair.rates[,grepl("_relRate",names(pair.rates))]
    names(pair.rates) <- gsub("_relRate","",names(pair.rates))
  }
  else
    pair.rates <- pair.rates[,!grepl("Rate|Mutation",names(pair.rates))]
  pair.rates <- pair.rates[,colSums(pair.rates)>0,drop=F]
  temporal.pair.rates <- data.frame(time=names(pair.rates),mut1=as.numeric(pair.rates[1,]),mut2=as.numeric(pair.rates[2,]))
  mut.rate.plot <- ggplot(temporal.pair.rates, aes(x=mut1, y=mut2, 
                    text=paste0(time,"\n<b>Rate ",mut.names[1],"</b>: ",mut1,"%\n<b>Rate ",mut.names[2],"</b>: ",mut2,"%"),
                    group=1)) + 
    geom_point(size=2, position=position_jitter(h=0.01,w=0.01)) +
    labs(x=paste0("Mutation rate ",mut.names[1]," (%)"),y=paste0("Mutation rate ",mut.names[2]," (%)")) +
    theme(axis.text.x=element_text(size=12, color="black"),
          axis.text.y=element_text(size=12, color="black"),
          axis.title.x=element_text(size=17, face="bold",margin=margin(t=20)),
          axis.title.y=element_text(size=17, face="bold",margin=margin(r=20)),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_line(color="black", linewidth = 0.5),
          axis.ticks.length=unit(.25, "cm"),
          legend.text.align = 0)
  if("Show regression line" %in% corr.plot.opt)
    mut.rate.plot <- mut.rate.plot + geom_smooth(formula = y ~ x,method=glm)
  mut.rate.plot <- ggplotly(mut.rate.plot, tooltip = "text", height=600) %>% config(displayModeBar = FALSE) %>%
    layout(hoverlabel = list(font=list(size=17)))
  return(mut.rate.plot)
}

make.correlation.plot <- function(pair.rates,corr.plot.opt)
{
  mut.names <- pair.rates$Mutation
  if("Use clades" %in% corr.plot.opt)
  {
    pair.rates <- pair.rates[,grepl("_relRate",names(pair.rates))]
    names(pair.rates) <- gsub("_relRate","",names(pair.rates))
  }
  else
    pair.rates <- pair.rates[,!grepl("Rate|Mutation",names(pair.rates))]
  pair.rates <- pair.rates[,colSums(pair.rates)>0,drop=F]
  temporal.pair.rates <- data.frame(time=names(pair.rates),mut1=as.numeric(pair.rates[1,]),mut2=as.numeric(pair.rates[2,]))
  mut.rate.plot <- ggplot(temporal.pair.rates, aes(x=mut1, y=mut2)) + 
    geom_point(size=2, position=position_jitter(h=0.01,w=0.01)) +
    geom_text_repel(label=temporal.pair.rates$time, max.overlaps = 100, segment.color = 'transparent', size=4) +
    labs(x=paste0("Mutation rate ",mut.names[1]," (%)"),y=paste0("Mutation rate ",mut.names[2]," (%)")) + theme_bw() +
    theme(axis.text.x=element_text(size=15, color="black"),
          axis.text.y=element_text(size=15, color="black"),
          axis.title.x=element_text(size=20, face="bold",margin=margin(t=20)),
          axis.title.y=element_text(size=20, face="bold",margin=margin(r=20)),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_line(color="black", linewidth = 0.5),
          axis.ticks.length=unit(.25, "cm"),
          legend.text.align = 0)
  if("Show regression line" %in% corr.plot.opt)
    mut.rate.plot <- mut.rate.plot + geom_smooth(formula = y ~ x,method="glm")
  return(mut.rate.plot)
}

make.heatmap.plot <- function(mut1, mut2, counts)
{
  plot.data <- data.frame(m1=rep(c("Absent","Present"),2),m2=rep(c("Absent","Present"),each=2),value=counts)
  plot.data$m1 <- factor(plot.data$m1)
  plot.data$m2 <- factor(plot.data$m2,levels = c("Present","Absent"))
  final.plot <- ggplot(plot.data, aes(x = m1, y = m2)) +
    #ggtitle("Sample counts") +
    geom_tile(aes(fill = value),color="black") +
    geom_text(aes(label = paste0("Samples: ",value)), color = "red", size=5) + 
    scale_fill_gradient(high = "black", low = "white") +
    theme(legend.position = "none", 
          #panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank()) +
    labs(x=mut1,y=mut2) +
    theme(
      #plot.title = element_text(size=17, face="bold", hjust = 0.5),
      axis.text.x=element_text(size=12, color="black"),
      axis.text.y=element_text(size=12, color="black"),
      axis.title.x=element_text(size=17, face="bold"),
      axis.title.y=element_text(size=17, face="bold"))
  final.plot <- ggplotly(final.plot, tooltip = "none") %>% config(displayModeBar = FALSE)
  return(final.plot)
}

build.country.icon <- function(country) {
  shiny::HTML(paste(
    tags$img(src=paste0("https://alpha.dmi.unict.it/~gmicale/Icons/",country,".png"), width=30, height=22),
    country
  ))
}

metadata <- data.frame(do.call(rbind, strsplit(gsub(".rds","",list.files("Data/Correlations")),"_")))
names(metadata) <- c("Country","Region")
global.clade.prevalences <- readRDS("Data/CladePrevalences/All_All.rds")
global.clade.mutation.rates <- readRDS("Data/CladeMutationRates/All_All.rds")
global.mutation.rates <- readRDS("Data/Rates/All_All.rds")
global.clade.corr <- readRDS("Data/Correlations/All_All.rds")
