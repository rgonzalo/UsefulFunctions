#data1 = toptable1 with features as rownames (Intereseting columns should be labeled "logFC", "Pvalue", "Adj.Pval")
#data2 = toptable2 with features as rownames (Intereseting columns should be labeled "logFC", "Pvalue", "Adj.Pval")
#upreg = Logical value indicating if upregulated or downregulated features (default "upregulated")
#limitFC = Numerical value indicating absolute threshold for logFC
#useAdjP = logical value indicating if use Adjusted P value for feature selection (default = TRUE)
#limitPvalue = numerical value indicating threshold for pvalue or adjusted pvalue. 
#comp1 = Name of comparison 1
#comp2 = Name of comparison 2
#colores =  vector with two colors


require(ggvenn)

vennplot_2 <- function (data1,
                        data2,
                        upreg = TRUE,
                        useAdjP = TRUE,
                        limitFC = 0,
                        limitPvalue = 0.05,
                        comp1 = "Comparison1",
                        comp2 = "Comparison2",
                        colores = c("#0073C2FF", "#EFC000FF")) {
  #initializing vectors
  common <- c()
  dif1 <- c()
  dif2 <- c()
  if(upreg){
    if(useAdjP){
      comp.name1 <- paste0(comp1, ".UP")
      comp.name2 <- paste0(comp2, ".UP")
      df1.up <- subset(data1, logFC > limitFC & Adj.Pval < limitPvalue)
      df2.up <- subset(data2, logFC > limitFC & Adj.Pval < limitPvalue)
      list.df.UP <- list(list1 = rownames(df1.up), list2 = rownames(df2.up))
      names(list.df.UP) <- c(comp.name1, comp.name2)
      #Draw the plot
      print(ggvenn(list.df.UP,
             fill_color = colores,
             stroke_size = 0.5,
             set_name_color = "black",
             set_name_size = 4,
             text_color = "black",
             text_size = 4,
             show_percentage = FALSE))
      
      #Search for elements, print and store
      dif1 <- setdiff(rownames(df1.up), rownames(df2.up))
      dif2 <- setdiff(rownames(df2.up), rownames(df1.up))
      common <- intersect(rownames(df1.up), rownames(df2.up))
      
      print("Features in common")
      print(common)
      print(paste0("Features specific of: ", comp.name1))
      print(dif1)
      print(paste0("Features specific of: ", comp.name2))
      print(dif2)
      common <<- common
      dif1 <<- dif1
      dif2 <<- dif2
      } else {
        comp.name1 <- paste0(comp1, ".UP")
        comp.name2 <- paste0(comp2, ".UP")
        df1.up <- subset(data1, logFC > limitFC & Pvalue < limitPvalue)
        df2.up <- subset(data2, logFC > limitFC & Pvalue < limitPvalue)
        list.df.UP <- list(list1 = rownames(df1.up), list2 = rownames(df2.up))
        names(list.df.UP) <- c(comp.name1, comp.name2)
        #Draw the plot and print
        print(ggvenn(list.df.UP,
               fill_color = colores,
               stroke_size = 0.5,
               set_name_color = "black",
               set_name_size = 4,
               text_color = "black",
               text_size = 4,
               show_percentage = FALSE))
        
        dif1 <- setdiff(rownames(df1.up), rownames(df2.up))
        dif2 <- setdiff(rownames(df2.up), rownames(df1.up))
        common <- intersect(rownames(df1.up), rownames(df2.up))
        
        print("Features in common")
        print(common)
        print(paste0("Features specific of: ", comp.name1))
        print(dif1)
        print(paste0("Features specific of: ", comp.name2))
        print(dif2)
        common <<- common
        dif1 <<- dif1
        dif2 <<- dif2
      }
    } else {
      if(useAdjP) {
        comp.name1 <- paste0(comp1, ".DOWN")
        comp.name2 <- paste0(comp2, ".DOWN")
        df1.down <- subset(data1, logFC < -limitFC & Adj.Pval < limitPvalue)
        df2.down <- subset(data2, logFC < -limitFC & Adj.Pval < limitPvalue)
        list.df.down <- list(list1 = rownames(df1.down), list2 = rownames(df2.down))
        names(list.df.down) <- c(comp.name1, comp.name2)
        #Draw the plot
        print(ggvenn(list.df.down,
               fill_color = colores,
               stroke_size = 0.5,
               set_name_color = "black",
               set_name_size = 4,
               text_color = "black",
               text_size = 4,
               show_percentage = FALSE))
        #Search for elements and print
        dif1 <- setdiff(rownames(df1.down), rownames(df2.down))
        dif2 <- setdiff(rownames(df2.down), rownames(df1.down))
        common <- intersect(rownames(df1.down), rownames(df2.down))
        
        print("Features in common")
        print(common)
        print(paste0("Features specific of: ", comp.name1))
        print(dif1)
        print(paste0("Features specific of: ", comp.name2))
        print(dif2)
        common <<- common
        dif1 <<- dif1
        dif2 <<- dif2
        
      } else {
        comp.name1 <- paste0(comp1, ".DOWN")
        comp.name2 <- paste0(comp2, ".DOWN")
        df1.down <- subset(data1, logFC < -limitFC & Pvalue < limitPvalue)
        df2.down <- subset(data2, logFC < -limitFC & Pvalue < limitPvalue)
        list.df.down <- list(list1 = rownames(df1.down), list2 = rownames(df2.down))
        names(list.df.down) <- c(comp.name1, comp.name2)
        #Draw the plot
        print(ggvenn(list.df.down,
               fill_color = colores,
               stroke_size = 0.5,
               set_name_color = "black",
               set_name_size = 4,
               text_color = "black",
               text_size = 4,
               show_percentage = FALSE))
        #Search for elements and print
        dif1 <- setdiff(rownames(df1.down), rownames(df2.down))
        dif2 <- setdiff(rownames(df2.down), rownames(df1.down))
        common <- intersect(rownames(df1.down), rownames(df2.down))
        
        print("Features in common")
        print(common)
        #return(common)
        print(paste0("Features specific of: ", comp.name1))
        print(dif1)
        print(paste0("Features specific of: ", comp.name2))
        print(dif2)
        common <<- common
        dif1 <<- dif1
        dif2 <<- dif2
      }
    }
}