#8,266 SARS-CoV-2 Genomic Assemblies from Asymptomatic Carriers in Japan#
####figure3.R####
#!/usr/bin/env R
library(NipponMap) #version 0.2
library(tidyverse) #version 2.0.0
library(viridis) #version 0.6.5
library(tagcloud) #version 0.6

#working directory
if (!interactive()) {
  setwd(dirname(normalizePath(sys.frame(1)$ofile)))
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(script_path)) {
    setwd(dirname(script_path))
  }
}

mapdata <- read_csv("./data/fig3_data.csv")

#color
cols<- smoothPalette(log10(mapdata$n+1), 
                     palfunc=colorRampPalette(c("white", viridis(6)[1])))

#legend
max_n <- max(mapdata$n)
lgd = rep(NA, max_n)
lgd[c(1,max_n+1)] = c(0,max_n)

#plot
pdf("./output/fig3.pdf", width = 6.7, height = 5.5)
JapanPrefMap(cols)
legend("bottom",
       legend = lgd,
       fill = colorRampPalette(colors = c("white", viridis(6)[1]))(max_n+1),
       border = NA, y.intersp = 0.0005, cex = 1, text.font = 0.8)
dev.off()
