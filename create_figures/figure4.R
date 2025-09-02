#8,266 SARS-CoV-2 Genomic Assemblies from Asymptomatic Carriers in Japan#
####figure4.R####
#!/usr/bin/env R
library(tidyverse) #version 2.0.0
library(viridis) #version 0.6.5

#working directory
if (!interactive()) {
  setwd(dirname(normalizePath(sys.frame(1)$ofile)))
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(script_path)) {
    setwd(dirname(script_path))
  }
}

seq_len <- read_csv("./data/fig4_data.csv")
p <- ggplot(data = seq_len, aes(x = seq_length, fill = category)) +
  geom_histogram(bins = 350, position = "stack", alpha = 0.9) +
  labs(x = "sequence length", y = "Frequency", title = "Histogram of sequence length (n=8,266)") +
  scale_x_continuous(breaks = seq(20000, 30000, by = 1000)) + 
  scale_y_continuous(breaks = seq(0, 180, by = 30), limits = c(0, 180)) +
  scale_fill_manual(breaks = c("Category 1", "Category 2"),
                    values = c(viridis(6)[1], viridis(6)[4])) +
  geom_vline(xintercept = 29000, linetype = "dashed", color = "black", linewidth = 0.8) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size=20)) +
  guides(fill = guide_legend(title = NULL)) +
  theme_bw()

ggsave("./output/fig4.pdf", p, width = 10, height = 5, dpi=300)
