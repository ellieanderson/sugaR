here::here()
# source(paste0("/Users/ellieanderson/Downloads/Summer Internship/",
#        "anderson_ellie_prolonged_viral_shedding/scripts/functions.R"))

library(covr)
library(testthat)
library(tidyverse)
library(vdiffr)
library(sugaR)
local_edition(3)

###########################
# TODO:
# check_groups() ✓
# group_test()
# pair_test()
# analyze() - especially the significant bars
# create_time_data() ✓
# plot_time_data()
# plot_overall()
# get_percs() ✓
# get_tots() ✓
# for plots: vdiffr
###########################

# file_coverage("scripts/functions.R", "tests/testing.R")

# GROUP_TEST
# data
x <- c("ABC", "ABC", "ABC",
       "DEF", "DEF", "DEF",
       "GHI", "GHI", "GHI", "GHI", "GHI", "GHI")
y <- c(13, 16, 21, 63, 89, 50, 30, 42, 58, 80, 60, 20)
group <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
id <- seq(1:12)

test_dat <- cbind.data.frame(x, y, group, id)

# test
test_that("group_test Kruskal-Wallis test works without filtering",{

  actual <- group_test(test_dat, x = "x", y = "y", id_col = "id")[[1]]
  expected <- kruskal.test(formula = y ~ x, data = test_dat)

  expect_equal(actual, expected, ignore_attr = TRUE)
  })

test_that("group_test Kruskal-Wallis test works WITH filtering", {

  actual <- group_test(test_dat,
                       x = "group", y = "y",
                       id_col = "id",
                       filter_col = "x",
                       filter_str = "GHI")[[1]]

  expected <- test_dat %>%
    filter(x == "GHI") %>%
    kruskal.test(formula = y ~ group, data = .)

  expect_equal(actual, expected, ignore_attr = TRUE)

})

test_that("group_test Kruskal-Wallis test works WITH filtering", {

  actual <- group_test(test_dat,
                       x = "group", y = "y",
                       id_col = "id",
                       filter_col = "x",
                       filter_str = "GHI")[[1]]

  expected <- test_dat %>%
    filter(x == "GHI") %>%
    kruskal.test(formula = y ~ group, data = .)

  expect_equal(actual, expected, ignore_attr = TRUE)

})

## plot to svg and return file contant as character
# plot_image <- function(expr) {
#   file <- tempfile(fileext=".svg")
#   on.exit(unlink(file))
#   svg(file)
#   expr
#   dev.off()
#   readLines(file)
# }

## the IDs differ at each `svg` call, that's why we simply remove them
ignore_svg_id <- function(lines) {
  gsub(pattern = "(xlink:href|id)=\"#?([a-z0-9]+)-?(?<![0-9])[0-9]+\"",
       replacement = "\\1=\"\\2\"", x = lines, perl = TRUE)
}

## compare svg character vs reference
expect_image_equal <- function(object, expected, ...) {
  stopifnot(is.character(expected) && file.exists(expected))
  expect_equal(ignore_svg_id(plot_image(object)),
               ignore_svg_id(readLines(expected)), ...)
}

## create reference image
create_reference_image <- function(expr, file) {
  svg(file)
  expr
  dev.off()
}

test_that("group_test plot is working without filtering",{
  p <- ggplot(test_dat,
              aes(x = as.factor(group),
                  y = y,
                  group = as.factor(group))) +
    geom_boxplot() +
    theme_bw() +
    labs(title = "Plot", x = "Group")

  actual <- group_test(test_dat,
                       x = "group", y = "y",
                       id_col = "id")[[2]]

  create_reference_image(p, "reference.svg")

  expect_image_equal(actual, "reference.svg")
  })

test_that("group_test plot is working with filtering",{
  p <- ggplot(test_dat %>% filter(group == "GHI"),
              aes(x = as.factor(group),
                  y = y,
                  group = as.factor(group))) +
    geom_boxplot() +
    theme_bw() +
    labs(title = "Plot", x = "Group")

  actual <- group_test(test_dat,
                       x = "group", y = "y",
                       id_col = "id",
                       filter_col = "x",
                       filter_str = "GHI")[[2]]

  create_reference_image(p, "reference.svg")

  expect_image_equal(actual, "reference.svg")
})


# PAIR TEST
# data
group <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
y <- c(rnorm(6, mean = 5, sd = 2), rnorm(6, mean = 25, sd = 5))
id <- seq(1:12)
x <- c("ABC", "ABC", "DEF", "DEF", "DEF", "DEF",
      "DEF", "DEF", "DEF", "DEF", "GHI", "GHI")
pair <- cbind(group, y, id, x) %>% as.data.frame()

# test
test_that("pair_test works without filtering", {
  actual <- pair_test(pair,
              values = "y",
              groups = "group",
              p_value = .05,
              id_col = "id")$p.values[1,2]

  expected <- wilcox.test(as.numeric(y) ~ group, data = pair)$p.value

  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("pair_test works with filtering", {
  actual <- pair_test(pair,
                      values = "y",
                      groups = "group",
                      filter_str = "DEF",
                      filter_col = "x",
                      p_value = .05,
                      id_col = "id")$p.values[1,2]

  expected <- pair %>%
    filter(x == "DEF") %>%
    {wilcox.test(formula = as.numeric(.$y) ~ .$group)$p.value}

  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("pair_test finds significant comparisons", {
  actual <- pair_test(pair,
                      values = "y",
                      groups = "group",
                      p_value = .05,
                      id_col = "id")$significant

  expected <- list(paste("1,2"))

  expect_equal(actual, expected, ignore_attr = TRUE)
})

# CHECK_GROUPS
# data
id <- seq(1:12)
group <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
check1 <- cbind(id, group) %>% as.data.frame()

test_that("checking groups is working", {
  actual <- check_groups(check1, group, id)

  expected <- check1
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("analyze is working", {
  x <- analyze(test_dat, "group", "x", "y", .05, "id")

  expect_equal(x, list())
})

test_that("analyze is working", {
  x <- analyze(test_dat, "group", "x", "y", .05, "id")

  expect_equal(x, list())
})

test_that("check_groups works in analyze", {

  expect_output(analyze(test_dat, "group", "x", "y", .05, "id"),
                {cat("c(\"ABC\", \"DEF\") removed due to insufficient sample size")
                cat("Not enough groups to run comparison")
                cat("[1] \"No comparisons run\"")}
  )
})

test_that("analyze works when given an adequate dataframe", {

  glycan <- rep("G_perc", 20)
  perc <- c(24.1, 21.2, 31.3, 26.8, 29.2, 34.3, 27.3,
            26.3, 20.0, 34.2, 34, 24.1, 28.5, 10.9, 20.0,
            31, 35.4, 27.7, 30.9, 33)
  group <- c(3, 3, 4, 3, 4, 4, 3, 3, 3, 4, 4, 3,
             3, 4, 3, 3, 3, 4, 4, 4)
  id <- seq(1:20)

  test_dat <- cbind.data.frame(id, group, glycan, perc) %>% as_tibble

  p <- analyze(test_dat, "glycan", "group", "perc", .05, "id")$Plot %>% suppressWarnings()

  w <- ggplot(test_dat, aes(x = as.factor(group), y = perc, group = as.factor(group))) +
    geom_boxplot() +
    geom_signif(comparisons = list(c("3", "4")),
                map_signif_level = TRUE)

  suppressWarnings(expect_equal(layer_data(p, 2), layer_data(w, 2), ignore_attr = TRUE))
  })

