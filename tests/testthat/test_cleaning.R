here::here()
# source(paste0("/Users/ellieanderson/Downloads/Summer Internship/",
#        "anderson_ellie_prolonged_viral_shedding/scripts/functions.R"))

library(covr)
library(testthat)
library(tidyverse)
# library(vdiffr)
library(sugaR)
local_edition(3)

# GET_PERCS
# data
Area <- 20
total_area <- 100
G <- 1
G_perc <- 54.3
sample <- "A"

p <- cbind(sample, Area, total_area, G) %>%
  as.data.frame %>%
  mutate(Area = as.numeric(Area),
         total_area = as.numeric(total_area))

# test
test_that("get_percs is accuate", {

  p1 <- get_percs(p, "G", "sample") %>% ungroup()

  expected <- p %>%
    mutate("G_perc" = 20) %>% as_tibble()

  expect_equal(p1, expected, ignore_attr = FALSE)
})

# GET_TOTS🥔
# data
sample <- "B3"
Allele <- "G2S2"
Area <- 97704
adj_area <- 195498
total_area <- 887484
adj_tot_area <- 1443249

potato <- cbind(sample, Allele, Area, adj_area, total_area, adj_tot_area) %>%
  as.data.frame() %>%
  mutate(Area = as.numeric(Area),
         adj_area = as.numeric(adj_area),
         total_area = as.numeric(total_area),
         adj_tot_area = as.numeric(adj_tot_area)) %>%
  as.tibble()

# test
test_that("get_tots is accurate",{
  tot <- get_tots(potato, "G", "sample", "Allele") %>%
    ungroup()

  expected <- potato %>%
    mutate(tot_G = (195498/1443249)*100)

  expect_equal(tot, expected, ignore_attr = TRUE)
})

