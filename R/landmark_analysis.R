## author: maxime rousseau
## date: May 29, 2020


## load libraries
library("tidyverse", "irr")

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
    mn_gb <- euc_dist(TABLE, "Mn", "Gb")
    mn_sn <- euc_dist(TABLE, "Mn", "Sn")
    mn_lm <- euc_dist(TABLE, "Mn", "Lm")
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
               type = "agreement"))
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
               type = "agreement"))
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
              "Mn-X", "Mn-Y",
              "Lv-X", "Lv-Y",
              "Lm-X", "Lm-Y",
              "Um-X", "Um-Y",
              "Uv-X", "Uv-Y",
              "Sn-X", "Sn-Y",
              "Gb-X", "Gb-Y"
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

hyp_results <- hyp_test(auto_metrics, manual_metrics)

## Male vs Female ----------------------------------------

f_metrics <- compute_metrics(filter(exp_at,
                                    Gender == "F"
                                    ))
m_metrics <- compute_metrics(filter(exp_at,
                                    Gender == "M"
                                    ))
mf_hyp_res <- hyp_test(f_metrics, m_metrics) ##NSF, discard

## Export results ----------------------------------------
write_csv(ICC_results, "./icc_results.csv")
write_csv(hyp_results, "./hyp_results.csv")

## TODO: write main function
## -> final output should be a simple table
## -> rewrite with functionals (pipes, map, etc.)
## -> read: https://r4ds.had.co.nz/program-intro.html

