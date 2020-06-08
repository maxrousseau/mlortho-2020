## author: maxime rousseau
## date: May 29, 2020

## load libraries
library(tidyverse)
library(irr)

## Functions ----------------------------------------

day_rater_cols <- function(table, day, rater) {
    new_table <- table %>%
        add_column(Day = day, Rater = rater)
    return(new_table)
}

merge_tbl <- function(table) {
    t1 <- day_rater_cols(table[[1]], 1, 1)
    t2 <- day_rater_cols(table[[2]], 2, 1)
    t3 <- day_rater_cols(table[[3]], 1, 2)
    t4 <- day_rater_cols(table[[4]], 2, 2)

    return(bind_rows(list(t1, t2, t3, t4)))
}

euc_dist <- function(TABLE, L1, L2){

    ldmk <- TABLE %>%
        select(starts_with(L1),
               starts_with(L2))
    
    x1 <- ldmk[1]
    y1 <- ldmk[2]
    x2 <- ldmk[3]
    y2 <- ldmk[4]

    dist <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

    return(dist)
}

compute_metrics <- function(TABLE){

    ## perform computations
    mn_gb <- euc_dist(TABLE, "Gn", "N")
    mn_sn <- euc_dist(TABLE, "Gn", "Sn")
    mn_lm <- euc_dist(TABLE, "Gn", "Lm")
    um_sn <- euc_dist(TABLE, "Um", "Sn")
    lv_lm <- euc_dist(TABLE, "Lv", "Lm")
    um_uv <- euc_dist(TABLE, "Um", "Uv")

    lower_face <- (mn_sn / mn_gb) * 100
    max_st <- (um_sn / mn_sn) * 100
    mand_st <- (mn_lm / mn_sn) * 100
    l_lip <- (lv_lm / mn_lm) * 100
    u_lip <- (um_uv / um_sn) * 100

    ## create tibble
    vars <- (c("lower_face",
               "max_st",
               "mand_st",
               "l_lip",
               "u_lip")
    )

    metric_tbl <- bind_cols(lower_face,
                            max_st,
                            mand_st,
                            l_lip,
                            u_lip)
    names(metric_tbl) <- vars

    return(metric_tbl)
   
}

## calibration computations
intra_icc <- function(TABLE, RATER, METRIC) {
    t1 <- subset(TABLE,
                 Rater == RATER & Day == 1
                 )[, METRIC]
    t2 <- subset(TABLE,
                 Rater == RATER & Day == 2
                 )[, METRIC]

    test_tbl <- bind_cols(t1, t2)

    return(icc(test_tbl, model = "twoway",
               type = "agreement",
               unit = "average"))
}

inter_icc <- function(TABLE, METRIC) {
    t1 <- subset(TABLE,
                 Rater == 1 & Day == 1
                 )[, METRIC]
    t2 <- subset(TABLE,
                 Rater == 2 & Day == 1
                 )[, METRIC]
    
    test_tbl <- bind_cols(t1, t2)

    return(icc(test_tbl, model = "twoway",
               type = "agreement",
               unit = "average"))
}

compute_icc <- function(TABLE){
    l_metrics <- c("lower_face",
                   "max_st",
                   "mand_st",
                   "l_lip",
                   "u_lip")
    raters <- c(1, 2)
    table <- TABLE

    results <- tibble(
        metrics = character()
        )
    
    for (metric in l_metrics) {
        intra_1 <- intra_icc(TABLE = table,
                             RATER = 1,
                             METRIC = metric)
        intra_2 <- intra_icc(TABLE = table,
                             RATER = 2,
                             METRIC = metric)
        inter <- inter_icc(TABLE = table,
                           METRIC = metric)

        values <- tibble(metrics = metric,
                         intra_1_icc = intra_1$value,
                         intra_1_ub = intra_1$ubound,
                         intra_1_lb = intra_1$lbound,
                         intra_2_icc = intra_2$value,
                         intra_2_ub = intra_2$ubound,
                         intra_2_lb = intra_2$lbound,
                         inter_icc = inter$value,
                         inter_ub = inter$ubound,
                         inter_lb = inter$lbound)
        results <- bind_rows(results, values)
    }
    
    return(results)
}

hyp_test <- function (X, Y) {

    metrics <- names(X)
    res_tbl <- tibble()

    for (metric in metrics) {
        
        res_tbl <- t.test(select(X, metric),
                          select(Y, metric)) %>%
            .$p.value %>%
            tibble(metric = metric,
                   auto_mu = mean(pull(X, metric)),
                   auto_sd = sd(pull(X, metric)),
                   man_mu = mean(pull(Y, metric)),
                   man_sd = sd(pull(Y, metric)),
                   p_val = .
                   ) %>%
            bind_rows(res_tbl)
    }

    return(res_tbl)
    
}

## Calibration ----------------------------------------

## get files
path_db <- normalizePath("../db")
files <- unlist(dir(path_db))

## load rater calibration data
cal_tbl <- files[c(1:4)] %>%
    file.path(path_db, .) %>%
    map(read_csv)

## merge calibration data into a single table
cal_tbl <- merge_tbl(cal_tbl)

## compute ratio and bind to calibration data
full_cal_tbl <- compute_metrics(cal_tbl) %>%
    bind_cols(cal_tbl, .)

## compute inter and intra rater ICC
ICC_results <- compute_icc(full_cal_tbl)


## Hypothesis testing -----------------------------

## load the manual and automated data
exp_tbl <- files[c(5:8)] %>%
    file.path(path_db, .) %>%
    map(read_csv)

pfla_ldmk <- as.character(c(9, 58, 67, 63, 52, 34, 28))
pfla_lbl <- c("Id",
              "Gn-X", "Gn-Y",
              "Lv-X", "Lv-Y",
              "Lm-X", "Lm-Y",
              "Um-X", "Um-Y",
              "Uv-X", "Uv-Y",
              "Sn-X", "Sn-Y",
              "N-X", "N-Y"
              )

exp_mn <- bind_rows(exp_tbl[[1]], exp_tbl[[2]]) %>%
    rename(Id = 'File No.')

exp_at <- bind_rows(exp_tbl[[3]], exp_tbl[[4]]) %>%
    select(X1, starts_with(pfla_ldmk))
names(exp_at) <- pfla_lbl

exp_at <- select(exp_mn, Age, Gender, Id) %>%
    left_join(exp_at, by = 'Id')

manual_metrics <- compute_metrics(exp_mn)
auto_metrics <- compute_metrics(exp_at)

## hyp_results <- hyp_test(auto_metrics, manual_metrics)
## get confidence intervals



## Plot results ------------------------------------------


table0 <- full_cal_tbl %>% filter(Day == 1) %>% select(`File No.`, Rater, lower_face,
                                                       max_st, mand_st, u_lip,
                                                       l_lip)
table1 <- table0 %>% pivot_longer(c(`lower_face`,
                                    `max_st`,
                                    `mand_st`,
                                    `l_lip`,
                                    `u_lip`),
                                  names_to = "metrics",
                                  values_to = "results")

inter_plot_df <- table1 %>% pivot_wider(names_from = "Rater", values_from = "results") %>%
    rename(Id = 'File No.', Rater_1 = `1`, Rater_2 = `2`)

## inter rater metrics plot
ggplot(data = inter_plot_df) +
    geom_point(mapping = aes(x = Rater_1,
                             y = Rater_2,
                             color = metrics)) +
    geom_abline()
ggsave("inter_rater.tiff", width = 7, height = 7)

table2 <- full_cal_tbl %>%
    select(`File No.`, Day, Rater, lower_face,
           max_st, mand_st, u_lip, l_lip)

table3 <- table2 %>%
    pivot_longer(c(`lower_face`,
                   `max_st`,
                   `mand_st`,
                   `l_lip`,
                   `u_lip`),
                 names_to = "metrics",
                 values_to = "results")

intra_plot_df <- table3 %>%
    pivot_wider(names_from = "Day", values_from = "results") %>%
    
    rename(Id = 'File No.', Day_1 = `1`, Day_2 = `2`) %>%
    mutate(Rater = as.character(Rater))


## intra rater metrics plot
ggplot(data = intra_plot_df) +
    geom_point(mapping = aes(x = Day_1,
                             y = Day_2,
                             color = metrics,
                             shape = Rater)) +
    geom_abline()
ggsave("intra_rater.tiff", width = 7, height = 7)

## bland altman analysis ----------------------------------------
diff_df <- auto_metrics - manual_metrics
avg_df <- (auto_metrics + manual_metrics) / 2
mean_diff <- map_dbl(diff_df, mean)
perc_diff <- ((diff_df / avg_df) * 100) %>%
    map_dbl(mean)
ci_diff <- map_dbl(diff_df, function(x) sd(x) * 1.96)
llimit_agreement <- mean_diff - ci_diff
ulimit_agreement <- mean_diff + ci_diff
metric_names <- c('lower_face',
                  'max_st',
                  'mand_st',
                  'l_lip',
                  'u_lip')
diff_plot_tbl <- as_tibble(diff_df) %>% pivot_longer(metric_names,
                                                     names_to = "metrics",
                                                     values_to = "difference")
avg_plot_tbl <- as_tibble(avg_df) %>% pivot_longer(metric_names,
                                                   names_to = "metrics",
                                                   values_to = "average")
ba_plot_tbl <- bind_cols(diff_plot_tbl, avg_plot_tbl)
## bland-altman analysis results
ba_lines <- tibble(metrics = metric_names, mean_diff, ulimit_agreement, llimit_agreement)

## fix axis make more pretty
ggplot(data = ba_plot_tbl) +
    geom_point(mapping = aes(x = average,
                              y = difference,
                             color = metrics)) +
    geom_hline(data = ba_lines, mapping = aes(yintercept = mean_diff,
                                              color = metrics)) +
    geom_hline(data = ba_lines, mapping = aes(yintercept = ulimit_agreement,
                                              color= metrics),
               linetype = "dotted") +
    geom_hline(data = ba_lines, mapping = aes(yintercept = llimit_agreement,
                                              color= metrics),
               linetype = "dotted") +
    facet_wrap(~ metrics, nrow=3)
ggsave("ba_analysis.tiff", width = 7, height = 7)

summary_df <- function(df, fcn, name){
    map(df, fcn) %>%
    as_tibble %>%
    pivot_longer(metric_names,
                 names_to = "metric",
                 values_to = name)
}


ba_analysis_tabl <- summary_df(manual_metrics, mean, "manual_mu") %>%
    right_join(summary_df(manual_metrics, sd, "manual_sd"), by = "metric") %>%
    right_join(summary_df(auto_metrics, mean, "auto_mu"), by = "metric") %>%
    right_join(summary_df(auto_metrics, sd, "auto_sd"), by = "metric") %>%
    bind_cols(tibble(mean_diff)) %>%
    bind_cols(tibble(ulimit_agreement)) %>%
    bind_cols(tibble(llimit_agreement)) %>%
    bind_cols(tibble(perc_diff))


## EXPORT results ----------------------------------------
results_table <- ICC_results %>% rename(metric = "metrics") %>%
    left_join(ba_analysis_tabl, by = "metric")

write_csv(results_table, "./results.csv")

## save plots to tiff file format

## TODO: write main function
## -> clean up graphs
## -> final output should be a simple table (Ok)
## -> rewrite with functionals (pipes, map, etc.)


