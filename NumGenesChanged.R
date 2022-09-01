
# topTab: Typical "limma" toptable with colnames as "logFC", "Adj.Pval" and "Pvalue"
##### adj0: p.valor adjustat pel qual filtrar (de normal, 0.01)
##### adj1: segon p.valor adjustat pel qual filtrar (de normal, 0.05)
##### adj2: tercer p.valor adjustat pel qual filtrar (de normal, 0.15)
##### adj3: tercer p.valor adjustat pel qual filtrar (de normal, 0.25)
##### P1: p.valor pel qual filtrar (de normal, 0.01)
##### P2: segon p.valor pel qual filtrar (de normal, 0.05)
################################################################################

genesSelectable <- function (topTab, adj0 = 0.01, adj1 = 0.05, adj2 = 0.15, P1 = 0.01, P2 = 0.05)
{
  upBelowAdj0 <- nrow(subset(topTab, Adj.Pval < adj0 & logFC > 0))
  downBelowAdj0 <- nrow(subset(topTab, Adj.Pval < adj0 & logFC < 0))
  
  upBelowAdj1 <- nrow(subset(topTab, Adj.Pval < adj1 & logFC > 0))
  downBelowAdj1 <- nrow(subset(topTab, Adj.Pval < adj1 & logFC < 0))
  
  upBelowAdj2 <- nrow(subset(topTab, Adj.Pval < adj2 & logFC > 0))
  downBelowAdj2 <- nrow(subset(topTab, Adj.Pval < adj2 & logFC < 0))
  
  upBelowP1 <- nrow(subset(topTab, Pvalue < P1 & logFC > 0))
  downBelowP1 <- nrow(subset(topTab, Pvalue < P1 & logFC < 0))
  
  upBelowP2 <- nrow(subset(topTab, Pvalue < P2 & logFC > 0))
  downBelowP2 <- nrow(subset(topTab, Pvalue < P2 & logFC < 0))
  
  return(c(upRegAdj0.01 = upBelowAdj0, downRegAdj0.01 = downBelowAdj0,
           upRegAdj0.05 = upBelowAdj1, downRegAdj0.05 = downBelowAdj1,
           upRegAdj0.15 = upBelowAdj2, downRegAdj0.15 = downBelowAdj2,
           upRegP0.01 = upBelowP1, downRegP0.01 = downBelowP1,
           upRegP0.05 = upBelowP2, downRegP0.05 = downBelowP2))   
}
