#8,266 SARS-CoV-2 Genomic Assemblies from Asymptomatic Carriers in Japan#
####figure1_2.R####
#!/usr/bin/env R
library(tidyverse) #version 2.0.0
library(colorspace) #version 2.1-1
library(cowplot) #version 1.1.3
library(scales) #version 1.3.0

#working directory
if (!interactive()) {
  setwd(dirname(normalizePath(sys.frame(1)$ofile)))
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(script_path)) {
    setwd(dirname(script_path))
  }
}

scientific_10 <- function(x) {
  index_zero <- which(x == 0)
  label <- scientific_format()(x)
  label <- str_replace(label, "e", " %*% 10^")
  label <- str_replace(label, "\\^\\+", "\\^")
  label[index_zero] <- "0"
  parse(text=label)
}

####figure 1a####
#Newly confirmed COVID−19 cases in Japan
daily_infect <- read_csv("./data/fig1a_data.csv")
daily_infect <- rename(daily_infect, "cases"="Daily infected cases in Japan")
p1a <- ggplot(daily_infect, aes(x = Date, y = cases)) +
  geom_area() +
  scale_x_date(limits = c(as.Date("2020-07-01"), as.Date("2023-01-16")), 
               date_breaks = "2 months", labels = scales::label_date_short()) +
  scale_y_continuous(label=scientific_10) +
  labs(title = "Newly confirmed COVID-19 cases in Japan", x = "date", y = "cases") +
  theme_bw() +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

p1a_legend <- get_legend(
  ggplot(daily_infect, aes(x = Date, y = cases)) +
    geom_area()
)

####figure 1b####
#gisaid#
fig1b_data <- read_csv("./data/fig1b_data.csv")

fig1b_data2 <- fig1b_data %>%
  mutate(
    lineages = recode(lineage,
                      "B.1.1.284" = "B.1.1.284", 
                      "B.1.1.214" = "B.1.1.214",
                      "R.1" = "R.1",
                      "B.1" = "B.1.x",
                      "B.1.1" = "B.1.x",
                      "B.1.1.7" = "Alpha (B.1.1.7)",
                      .default = scorpio_call))

fig1b_data3 <- fig1b_data2 %>%
  mutate(
    lineages = case_when(
      .$lineages == "B.1.1.318-like" ~ "B.1.x",
      .$lineages == "B.1.1.284" ~ "B.1.1.284",
      .$lineages == "B.1.1.214" ~ "B.1.1.214",
      .$lineages == "R.1" ~ "R.1",
      .$lineages == "B.1.x" ~ "B.1.x",
      .$lineages == "Alpha (B.1.1.7)" ~ "Alpha (B.1.1.7)",
      .$lineages == "Alpha (B.1.1.7-like)" ~ "Alpha (B.1.1.7)",
      .$lineages == "Beta (B.1.351-like)" ~ "B.1.x",
      .$lineages == "Delta (B.1.617.2-like)" ~ "Delta (B.1.617.2)",
      .$lineages == "Delta (B.1.617.2-like) +K417N" ~ "Delta (B.1.617.2)",
      .$lineages == "Delta (AY.4-like)" ~ "Delta (AY.4.x)",
      .$lineages == "Delta (AY.4.2-like)" ~ "Delta (AY.4.x)",
      .$lineages == "Omicron (BA.1-like)" ~ "Omicron (BA.1.x)",
      .$lineages == "Omicron (BA.2-like)" ~ "Omicron (BA.2.x)",
      .$lineages == "Omicron (BA.4-like)" ~ "Omicron (BA.4.x)",
      .$lineages == "Omicron (BA.5-like)" ~ "Omicron (BA.5.x)",
      .$lineages == "Omicron (XBB-like)" ~ "Omicron (XBB.x)",
      .$lineages == "Omicron (XBB.1-like)" ~ "Omicron (XBB.x)",
      .$lineages == "Omicron (XBB.1.5-like)" ~ "Omicron (XBB.x)",
      .$lineages == "Omicron (XE-like)" ~ "Omicron (others)",
      .$lineages == "Omicron (Unassigned)" ~ "Omicron (others)",
      .$lineages == "Probable Omicron (Unassigned)" ~ "Omicron (others)"
    ))

#NA to others
fig1b_data3 <- fig1b_data3 %>% 
  replace_na(list(lineages = "others"))
#convert "date" to "year-month"
fig1b_data3 <- fig1b_data3 %>%
  mutate(
    yearmon = format(as.Date(fig1b_data3$`Collection date`), "%Y-%m")
  )

#convert "year-month" to "year-month-01"
fig1b_data3$yearmon <- paste0(fig1b_data3$yearmon, "-1")
fig1b_data3$yearmon <- as.Date(fig1b_data3$yearmon, format = "%Y-%m-%d")

#color
own_col <- c("midnightblue", "royalblue", "purple", divergingx_hcl(10, "Spectral"), "gray")
order <- c(
  "B.1.1.284",
  "B.1.1.214",
  "R.1",
  "B.1.x",
  "Alpha (B.1.1.7)",
  "Delta (B.1.617.2)",
  "Delta (AY.4.x)",
  "Omicron (BA.1.x)",
  "Omicron (BA.2.x)",
  "Omicron (BA.4.x)",
  "Omicron (BA.5.x)",
  "Omicron (XBB.x)",
  "Omicron (others)",
  "others"
)

p1b <- ggplot(fig1b_data3, aes(x = yearmon, fill = lineages)) +
  geom_bar(color = "black") +
  scale_fill_manual(
    breaks = order,
    values = own_col) +
    scale_x_date(limits = c(as.Date("2020-07-01"), max(as.Date("2023-01-16"))),
                 date_breaks = "2 months", labels = scales::label_date_short()) +
  scale_y_continuous(label=scientific_10) +
  labs(title = "GISAID, Japan, n=539,504", x = "date", y = "sequences") +
  theme_bw() +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
        plot.title = element_text(size = 20)) +
  theme(legend.position = "none")

p1b_legend <- get_legend(
  ggplot(fig1b_data3, aes(x=yearmon, fill=lineages)) +
    geom_bar(color = "black") +
    theme(legend.text = element_text(size = 16),
          legend.title = element_text(size = 20)) +
    scale_fill_manual(
      breaks = order,
      values = own_col))

####figure 1c####
fig1c_data <- read_csv("./data/fig1c_data.csv")
fig1c_data2 <- fig1c_data %>%
  mutate(
    lineages =  recode(lineage,
                       "B.1.1.284" = "B.1.1.284", 
                       "B.1.1.214" = "B.1.1.214",
                       "R.1" = "R.1",
                       "B.1" = "B.1.x",
                       "B.1.1" = "B.1.x",
                       "B.1.1.7" = "Alpha (B.1.1.7)",
                       .default = scorpio_call))

fig1c_data3 <- fig1c_data2 %>%
  mutate(
    lineages = case_when(
      .$lineages == "B.1.1.318-like" ~ "B.1.x",
      .$lineages == "B.1.1.284" ~ "B.1.1.284",
      .$lineages == "B.1.1.214" ~ "B.1.1.214",
      .$lineages == "R.1" ~ "R.1",
      .$lineages == "B.1.x" ~ "B.1.x",
      .$lineages == "Alpha (B.1.1.7)" ~ "Alpha (B.1.1.7)",
      .$lineages == "Alpha (B.1.1.7-like)" ~ "Alpha (B.1.1.7)",
      .$lineages == "Beta (B.1.351-like)" ~ "B.1.x",
      .$lineages == "Delta (B.1.617.2-like)" ~ "Delta (B.1.617.2)",
      .$lineages == "Delta (B.1.617.2-like) +K417N" ~ "Delta (B.1.617.2)",
      .$lineages == "Delta (AY.4-like)" ~ "Delta (AY.4.x)",
      .$lineages == "Delta (AY.4.2-like)" ~ "Delta (AY.4.x)",
      .$lineages == "Omicron (BA.1-like)" ~ "Omicron (BA.1.x)",
      .$lineages == "Omicron (BA.2-like)" ~ "Omicron (BA.2.x)",
      .$lineages == "Omicron (BA.4-like)" ~ "Omicron (BA.4.x)",
      .$lineages == "Omicron (BA.5-like)" ~ "Omicron (BA.5.x)",
      .$lineages == "Omicron (XBB-like)" ~ "Omicron (XBB.x)",
      .$lineages == "Omicron (XBB.1-like)" ~ "Omicron (XBB.x)",
      .$lineages == "Omicron (XBB.1.5-like)" ~ "Omicron (XBB.x)",
      .$lineages == "Omicron (XE-like)" ~ "Omicron (others)",
      .$lineages == "Omicron (Unassigned)" ~ "Omicron (others)",
      .$lineages == "Probable Omicron (Unassigned)" ~ "Omicron (others)"
    ))
#convert "date" to "year-month"
fig1c_data3 <- fig1c_data3 %>%
  mutate(
    yearmon = format(as.Date(fig1c_data3$Date), "%Y-%m"))

#convert "year-month" to "year-month-01"
fig1c_data3$yearmon <- paste0(fig1c_data3$yearmon, "-1")
fig1c_data3$yearmon <- as.Date(fig1c_data3$yearmon, format = "%Y-%m-%d") 

p1c <- ggplot(fig1c_data3, aes(x = yearmon, fill = lineages)) +
  geom_bar(color = "black", size = 0.1) +
  scale_fill_manual(
    breaks = order,
    values = own_col) +
    scale_x_date(limits = c(as.Date("2020-07-01"), max(as.Date("2023-01-16"))), 
               date_breaks = "2 months", labels = scales::label_date_short()) +
  scale_y_continuous(label=scientific_10) +
  labs(title = "Asymptomatic, SBCVIC, Japan, n=2,917", x = "date", y = "sequences") +
  theme_bw() +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
        plot.title = element_text(size = 20)) +
  theme(legend.position = "none")

p1c_legend <- get_legend(
  ggplot(fig1c_data3, aes(x = yearmon, fill = lineages)) +
  geom_bar(color = "black", size = 0.1) +
    theme(legend.text = element_text(size = 16),
          legend.title = element_text(size = 20)) +
    scale_fill_manual(
    breaks = order,
    values = own_col))

A <- plot_grid(p1a, p1a_legend, nrow = 1, rel_widths = c(7, 1))
B <- plot_grid(p1b, p1b_legend, nrow = 1, rel_widths = c(7, 1))
C <- plot_grid(p1c, p1c_legend, nrow = 1, rel_widths = c(7, 1))

fig1 <- plot_grid(A, B, C, ncol = 1, labels = c("A", "B", "C"),
                  label_size = 25,
                  rel_heights = c(0.4, 0.8, 0.8))

ggsave("./output/fig1.pdf", fig1, width = 20, height = 20, dpi=300)

####figure 2####
fig2_data <- read_csv("./data/fig2_data.csv")
a = factor("tested")
b = factor("positive")

p_sb <- ggplot() +
  geom_area(data = fig2_data, aes(x = Date, y = Samples, fill = a),
            linewidth = 1, alpha = 0.5) +
  geom_line(data = fig2_data, aes(x = Date, y = Positive.samples/0.01, color = b), 
            linewidth = 1.2, alpha = 0.3) +
  scale_x_date(limits = c(as.Date("2020-07-01"), as.Date("2023-01-16")),
               date_breaks = "2 months", labels = scales::label_date_short()) +
  scale_y_continuous(label=scientific_10,
                     sec.axis = sec_axis(~. *0.01, name = "positive cases", labels = derive())) +
  labs(title = "Number of Tested and Positive cases of SARS-CoV-2 in SBCVIC", x = "date", y = "tested cases") +
  theme_bw() +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.position = "top", legend.text = element_text(size = 20),
        legend.title=element_blank()) + 
  scale_color_manual(values = c("blue")) +
  scale_fill_manual(values = c("gray20"))

ggsave("./output/fig2.pdf", p_sb, width = 20, height = 5, dpi=300)
