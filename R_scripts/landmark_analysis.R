## author: maxime rousseau
## date: May 29, 2020

## load libraries
library("tidyverse", "irr")

## set paths for data
path_db <- normalizePath("../db")
files <- unlist(dir(path_db))

## load rater calibration data
## NOTE: refactor for efficiency code quality (i.e. map/funcprog)
cal_files <- files[c(1:4)]
cal_tbl <- file.path(path_db, cal_files) %>%
    map(read_csv)

day_rater_cols <- function(table, day, rater) {
    new_table <- table %>%
        add_column(Day = day, Rater = rater)
    return(new_table)
}

merge_tbl <- function(table) {
    t1 <- day_rater_cols(table[[1]],
                         1,
                         1)
    t2 <- day_rater_cols(table[[2]],
                         2,
                         1)
    t3 <- day_rater_cols(table[[3]],
                         1,
                         2)
    t4 <- day_rater_cols(table[[4]],
                         2,
                         2)
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

# NOTE: fix case headers of tables
compute_metrics <- function(TABLE){

    mn_gb <- euc_dist(TABLE, "Mn", "Gb")
    mn_sn <- euc_dist(TABLE, "Mn", "Sn")
    mn_lm <- euc_dist(TABLE, "Mn", "Lm")
    um_sn <- euc_dist(TABLE, "Um", "Lm")
    lv_lm <- euc_dist(TABLE, "Lv", "Lm")
    um_uv <- euc_dist(TABLE, "Um", "Uv")
# rename table
    lower_face <- mn_sn / mn_gb %>%
        rename(lower_face = colnames(.)[1])
                         
    max_st <- um_sn / mn_sn
    mand_st <- mn_lm / mn_sn
    l_lip <- lv_lm / mn_lm
    u_lip <- um_uv / um_sn

    ## create tibble
    
    return(lower_face)
   
}

## calibration computations
intra_icc <- function(TABLE, RATER, METRIC) {
    t1 <- subset(TABLE,
                 Rater == RATER & Day == 1)[, METRIC]
    t2 <- subset(TABLE,
                 Rater == RATER & Day == 2)[, METRIC]
    test_tbl <- bind_cols(t1, t2)
    return(icc(test_tbl))
}

inter_icc <- function(TABLE, METRIC) {
    t1 <- subset(TABLE,
                 Rater == 1 & Day == 1
                 )[, METRIC]
    t2 <- subset(TABLE,
                 Rater == 2 & Day == 1
                 )[, METRIC]
    test_tbl <- bind_cols(t1, t2)
    return(icc(test_tbl))
}

compute_icc <- function(){
}

cal_tbl <- merge_tbl(cal_tbl)
