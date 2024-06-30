make.temporal.plot <- function(mutation.rates, mutation.set)
{
  temporal.rates <- mutation.rates[mutation.rates$Mutation %in% mutation.set,!grepl("Rate",names(mutation.rates)),drop=F]
  final.data <- data.frame(mutation=rep(temporal.rates$Mutation,each=ncol(temporal.rates)-1),
             month=rep(names(temporal.rates)[-1],length(mutation.set)),
             rate=as.numeric(t(temporal.rates[,-1])))
  final.data$month <- factor(final.data$month,levels=unique(final.data$month))
  
  temporal.plot <- ggplot(final.data, aes(x=month, y=rate, color=mutation, group=mutation, 
                                          text=paste0(month,"\n<b>Mutation:</b> ",mutation,"\n","<b>Rate:</b> ",rate,"%"))) + 
    geom_line(linewidth=1) + geom_point(size=2) +
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

make.clade.interactive.plot <- function(mutation.rates,mutation.set)
{
  clade.rates <- mutation.rates[mutation.rates$Mutation %in% mutation.set,grepl("Mutation|Rate",names(mutation.rates)),drop=F]
  list.clades <- unique(unlist(strsplit(names(clade.rates)[-1],"_"))[seq(1,(ncol(clade.rates)-1)*2,2)])
  pie.data <- data.frame(mutation=rep(clade.rates$Mutation,each=(ncol(clade.rates)-1)/2),
             clade=rep(list.clades,length(mutation.set)),
             cladeRate=as.numeric(t(clade.rates[,grepl("_relRate",names(clade.rates))])),
             absRate=as.numeric(t(clade.rates[,grepl("_absRate",names(clade.rates))])))
  pie.data <- pie.data[pie.data$absRate>=0.1,]
  #Added 15 colors (12 colors of Set3 palette + 3 colors of Set1 palette in RColorBrewer)
  plot.colors <- c("coral", "chartreuse3" ,"cyan","darkgoldenrod2" ,"mediumpurple1", "palevioletred", "violetred1","forestgreen","darkolivegreen3",
             "skyblue1", "darkseagreen1", "dodgerblue", "burlywood4", "yellow", "grey60","tan1", "darkorchid4", "tomato", "plum1",
             "darkkhaki", "brown1", "dodgerblue4", "mediumvioletred", "darksalmon", "darkslategray4",
             "#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5", "#D9D9D9", "#BC80BD",
             "#CCEBC5","#FFED6F","#E41A1C","#377EB8","#4DAF4A")
  names(plot.colors) <- c("19A", "19B" ,"20A", "20B", "20C", "20D", "20E(EU1)" , "20F", "20G", 
                          "20H(Beta,V2)", "20I(Alpha,V1)", "20J(Gamma,V3)", "21A(Delta)","21B(Kappa)", 
                          "21C(Epsilon)", "21D(Eta)", "21E(Theta)", "21F(Iota)" , "21G(Lambda)", "21H(Mu)",
                          "21I(Delta)",  "21J(Delta)", "21K(Omicron)", "21L(Omicron)", "21M(Omicron)",
                          "22A(Omicron)","22B(Omicron)","22C(Omicron)","22D(Omicron)","22F(Omicron)",
                          "23A(Omicron)","23B(Omicron)","23C(Omicron)","23D(Omicron)","23F(Omicron)",
                          "23G(Omicron)","23H(Omicron)","23I(Omicron)","24A(Omicron)","24B(Omicron)") 
  pie.plot.list <- list()
  for(mutation in mutation.set)
  {
    mutation.pie.data <- pie.data[pie.data$mutation==mutation,]
    mutation.pie.data$csum <- rev(cumsum(rev(mutation.pie.data$absRate)))
    if(nrow(mutation.pie.data)>1) {
      mutation.pie.data$pos <- mutation.pie.data$absRate/2 + c(mutation.pie.data$csum[2:nrow(mutation.pie.data)],0)
    } else {
      mutation.pie.data$pos <- mutation.pie.data$absRate/2
    }
    mutation.pie.colors <- plot.colors[as.character(mutation.pie.data$clade)]
    mutation.pie.plot <- plot_ly(mutation.pie.data, labels = ~clade, values = ~absRate, 
            marker = list(colors = mutation.pie.colors, line = list(color = 'white', width = 2)), 
            sort=F, type = 'pie', textinfo="none",
            text = ~paste('<b>',clade,'</b>','\nClade Frequency: ', absRate, '%\nClade Mutation Rate: ',cladeRate,"%"), 
            hoverinfo = 'text') %>%
    layout(title=list(text=paste0("<b>",mutation,"</b>"),font=list(size=20)), 
           legend = list(font = list(size = 16)),hoverlabel = list(font = list(size = 16)),
           margin=list(l = 20, r = 20,b = 20, t = 70)) %>%
    config(displayModeBar = FALSE)
    pie.plot.list[[mutation]] <- mutation.pie.plot
  }
  return(pie.plot.list)
}

make.clade.plot <- function(mutation.rates,mutation.set)
{
  clade.rates <- mutation.rates[mutation.rates$Mutation %in% mutation.set,grepl("Mutation|Rate",names(mutation.rates)),drop=F]
  list.clades <- unique(unlist(strsplit(names(clade.rates)[-1],"_"))[seq(1,(ncol(clade.rates)-1)*2,2)])
  pie.data <- data.frame(mutation=rep(clade.rates$Mutation,each=(ncol(clade.rates)-1)/2),
                         clade=rep(list.clades,length(mutation.set)),
                         cladeRate=as.numeric(t(clade.rates[,grepl("_relRate",names(clade.rates))])),
                         absRate=as.numeric(t(clade.rates[,grepl("_absRate",names(clade.rates))])))
  pie.data <- pie.data[pie.data$absRate>=0.1,]
  plot.colors <- c("coral", "chartreuse3" ,"cyan","darkgoldenrod2" ,"mediumpurple1", "palevioletred", "violetred1","forestgreen","darkolivegreen3",
                   "skyblue1", "darkseagreen1", "dodgerblue", "burlywood4", "yellow", "grey60","tan1", "darkorchid4", "tomato", "plum1",
                   "darkkhaki", "brown1", "dodgerblue4", "mediumvioletred", "darksalmon", "darkslategray4",
                   "#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5", "#D9D9D9", "#BC80BD",
                   "#CCEBC5","#FFED6F","#E41A1C","#377EB8","#4DAF4A")
  names(plot.colors) <- c("19A", "19B" ,"20A", "20B", "20C", "20D", "20E(EU1)" , "20F", "20G", 
                          "20H(Beta,V2)", "20I(Alpha,V1)", "20J(Gamma,V3)", "21A(Delta)","21B(Kappa)", 
                          "21C(Epsilon)", "21D(Eta)", "21E(Theta)", "21F(Iota)" , "21G(Lambda)", "21H(Mu)",
                          "21I(Delta)",  "21J(Delta)", "21K(Omicron)", "21L(Omicron)", "21M(Omicron)",
                          "22A(Omicron)","22B(Omicron)","22C(Omicron)","22D(Omicron)","22F(Omicron)",
                          "23A(Omicron)","23B(Omicron)","23C(Omicron)","23D(Omicron)","23F(Omicron)",
                          "23G(Omicron)","23H(Omicron)","23I(Omicron)","24A(Omicron)","24B(Omicron)")
  pie.plot.list <- list()
  for(mutation in mutation.set)
  {
    mutation.pie.data <- pie.data[pie.data$mutation==mutation,]
    mutation.pie.data$csum <- rev(cumsum(rev(mutation.pie.data$absRate)))
    if(nrow(mutation.pie.data)>1) {
      mutation.pie.data$pos <- mutation.pie.data$absRate/2 + c(mutation.pie.data$csum[2:nrow(mutation.pie.data)],0)
    } else {
      mutation.pie.data$pos <- mutation.pie.data$absRate/2
    }
    mutation.pie.colors <- plot.colors[as.character(mutation.pie.data$clade)]
    
    mutation.pie.plot <- ggplot(mutation.pie.data, aes(x="", y=absRate, fill=clade)) +
      geom_bar(stat="identity", width=1, color="black") +
      coord_polar("y", start=0) +
      scale_fill_manual(values = mutation.pie.colors) +
      geom_label_repel(data = mutation.pie.data,
                        aes(y = pos, label = paste0("CF: ",absRate,"%, CMR: ",cladeRate,"%")),
                        size = 3, nudge_x = 1, show.legend = FALSE) +
      theme_void() +
      theme(legend.text=element_text(size=15), legend.title=element_blank(),
            legend.position="right", legend.spacing.y = unit(0.2, 'cm')) +
      guides(fill = guide_legend(byrow = F)) +
      ggtitle(label=paste0("\n",mutation)) +
      theme(plot.title = element_text(size=17,hjust = 0.5,face="bold"), plot.margin=unit(c(0,0,0,0), "cm"))
    #mutation.pie.plot
    pie.plot.list[[mutation]] <- mutation.pie.plot
  }
  if(length(pie.plot.list)==4) {
    nrow <- 2
    ncol <- 2
  } else {
  nrow <- ceiling(length(pie.plot.list)/3)
  ncol <- min(length(pie.plot.list),3)
  }
  final.plot <- ggarrange(plotlist = pie.plot.list, nrow = nrow, ncol = ncol, legend="right")
  #final.plot <- annotate_figure(final.plot,top=text_grob("AR: Absolute Rate, CR: Clade Rate", face="bold", size=13))
  final.plot
  return(final.plot)
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
global.mutation.rates <- readRDS("Data/Rates/All_All.rds")
global.clade.corr <- readRDS("Data/Correlations/All_All.rds")
