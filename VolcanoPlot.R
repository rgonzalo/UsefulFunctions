#data = toptable with features as rownames (Intereseting columns should be labeled "logFC", "Pvalue", "Adj.Pval")
#logFC = column depicting fold change between the comparisons (i.e. logFC, estimate, ...)
#limitFC = Numerical value indicating absolute threshold for logFC
#useAdjP = logical value indicating if use Adjusted P value for feature selection
#limitPvalue = numerical value indicating threshold for pvalue or adjusted pvalue. 
#colors = if you want to change the colors.
#title1 = title of the plot indicating comparison involved
#interactive =  if you want to use ggplotly to make the graphic interactive


require(ggplot2)
require(plotly)

volcano.color <- function (data, 
                           limitFC, 
                           useAdjP = TRUE, 
                           limitPvalue, 
                           colors2 = c("blue", "red", "grey"), 
                           title1,
                           interactive = TRUE) {
  if(useAdjP){
    data2vol <- data
    data2vol$diffexpressed <- "NO"
    data2vol$diffexpressed[data2vol$logFC >= limitFC & data2vol$Adj.Pval < limitPvalue] <- "UP"
    data2vol$diffexpressed[data2vol$logFC <= -limitFC & data2vol$Adj.Pval < limitPvalue] <- "DOWN"
    mycolors <- colors2
    names(mycolors) <- c("DOWN", "UP", "NO")
    # add labels
    data2vol$delabel <- NA
    data2vol$delabel[data2vol$diffexpressed != "NO"] <- rownames(data2vol)[data2vol$diffexpressed != "NO"]
    #plot
    p1 <- ggplot(data = data2vol, aes(x = logFC,  y = -log10(Adj.Pval), col = diffexpressed, label = delabel)) + 
      geom_point() +
      scale_colour_manual(values = mycolors) +
      theme_minimal() +
      ggtitle(paste0(title1, " \nAdjPvalue < ", limitPvalue, " & abs(logFC) >= ", limitFC)) +
      labs(x = "LogFC") +
      theme(axis.text.x = element_text(angle = 0,  size = 6), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  } else {
    data2vol <- data
    data2vol$diffexpressed <- "NO"
    data2vol$diffexpressed[data2vol$logFC >= limitFC & data2vol$Pvalue < limitPvalue] <- "UP"
    data2vol$diffexpressed[data2vol$logFC <= -limitFC & data2vol$Pvalue < limitPvalue] <- "DOWN"
    mycolors <- colors2
    names(mycolors) <- c("DOWN", "UP", "NO")
    # add labels
    data2vol$delabel <- NA
    data2vol$delabel[data2vol$diffexpressed != "NO"] <- rownames(data2vol)[data2vol$diffexpressed != "NO"]
    #plot
    p1 <- ggplot(data = data2vol, aes(x = logFC,  y = -log10(Pvalue), col = diffexpressed, label = delabel)) + 
      geom_point() +
      scale_colour_manual(values = mycolors) +
      theme_minimal() +
      ggtitle(paste0(title1, " \nPvalue < ", limitPvalue, " & abs(logFC) >= ", limitFC)) +
      labs(x = "LogFC") +
      theme(axis.text.x = element_text(angle = 0,  size = 6), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  }
  
  if(interactive){
    ggplotly(p1)
  } else {
    p1
  }
}



