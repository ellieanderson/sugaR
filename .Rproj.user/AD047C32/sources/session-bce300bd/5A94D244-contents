library(tidyverse)
real <- read_csv("/Users/ellieanderson/Library/CloudStorage/Box-Box/Duke Biostats Summer Internship_Schuster_Anderson_2023/Data/raw_mice_assay.csv") %>%
  mutate(group = substring(`Sample File Name`, 1, 2))

dat1 <- real %>%
  group_by(group, Allele) %>%
  summarize(avg_size = mean(Size, na.rm = TRUE),
            sd_size = sd(Size, na.rm = TRUE),
            avg_height = mean(Height, na.rm = TRUE),
            sd_height = sd(Height, na.rm = TRUE),
            avg_area = mean(Area, na.rm = TRUE),
            sd_area = sd(Area, na.rm = TRUE)) %>%
  pivot_longer(c(avg_size, sd_size, avg_height, sd_height, avg_area, sd_area),
               names_to = c(".value", "column"), names_sep = "_")

dat1 %<>%
  group_by(column) %>%
  mutate(sd = replace_na(sd, mean(sd, na.rm = T)))

dat1$n <- 4

dat2 <- dat1 %>%
  mutate(new1 = abs(rnorm(n = n, mean = avg, sd = sd)),
         new2 = abs(rnorm(n = n, mean = avg, sd = sd)),
         new3 = abs(rnorm(n = n, mean = avg, sd = sd)),
         new4 = abs(rnorm(n = n, mean = avg, sd = sd))) %>%
  pivot_longer(new1:new4) %>%
  select(-c(name, n)) %>%
  pivot_wider(id_cols = c("group", "Allele"),
              id_expand = TRUE,
              names_from = "column") %>%
  unnest(cols = c(size, height, area)) %>%
  group_by(group, Allele) %>%
  mutate(id = case_when(group == "AE" ~ sample(1:4, 4),
                        group == "Ag" ~ sample(5:8, 4),
                        group == "B6" ~ sample(9:12, 4),
                        group == "GH" ~ sample(13:16, 4),
                        group == "Hu" ~ sample(17:20, 4),
                        group == "Mo" ~ sample(21:24, 4),
                        group == "QM" ~ sample(25:28, 4),
                        group == "TA" ~ sample(29:32, 4))) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    # adjust individual areas to show that there are 2 of some components
    adj_area = if_else(str_detect(Allele, "2"), area*2, area),
    # Calculate total area for each sample
    total_area = sum(area),
    # Calculate adjusted area for the species w 2 of same component
    adj_tot_area = sum(adj_area),
    # Calculate relative abundance of each allele in each sample (%)
    rel_abundance = if_else(str_detect(Allele, "2"),
                            (adj_area/adj_tot_area)*100,
                            (area/total_area)*100)) %>%
  # get rid of grouping
  ungroup() %>%
  # determine what glycans are included in each allele
  mutate(G = case_when(str_detect(Allele, "G") &
                         !str_detect(Allele, "G0") &
                         !str_detect(Allele, "G2") ~ 1,
                       TRUE ~ 0),
         G2 = if_else(str_detect(Allele, "G2"), 1, 0),
         F = if_else(str_detect(Allele, "F"), 1, 0),
         S = case_when(str_detect(Allele, "S") &
                         !str_detect(Allele, "S2") ~ 1,
                       TRUE ~ 0),
         S2 = if_else(str_detect(Allele, "S2"), 1, 0),
         B = if_else(str_detect(Allele, "B"), 1, 0),
         A = if_else(str_detect(Allele, "A"), 1, 0)) %>%
  rename("Area" = area)



dat3 <- dat2 %>%
  get_percs("G", "id") %>%
  get_percs("G2", "id") %>%
  get_percs("S", "id") %>%
  get_tots("G", "id", "Allele") %>%
  get_tots("S", "id", "Allele")


ggplot(dat3, aes(x = group, y = G_perc)) +
  geom_boxplot()
