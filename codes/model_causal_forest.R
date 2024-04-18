##%######################################################%##
#                                                          #
####      Causal Forests - Orchestra - @lslbastos       ####
#                                                          #
##%######################################################%##


# library -----------------------------------------------------------------
library(tidyverse)
# library(tidylog)
library(grf)


# Input data --------------------------------------------------------------

df_icu <- 
    read_csv("input/df_icu_age18_surv.csv") %>% 
    mutate(
        eff_group = case_when(
            smr < median(smr) & sru < median(sru) ~ "most_efficient",
            smr > median(smr) & sru > median(sru) ~ "least_efficient",
            smr < median(smr) & sru > median(sru) ~ "overachieving",
            smr > median(smr) & sru < median(sru) ~ "underachieving"
            )
    ) %>% 
    mutate(
        eff_group_out = if_else(eff_group == "most_efficient", 0, 1)
    ) %>% 
    mutate(
        # outlier_3sd = case_when(
        #     between(( asr - mean(asr) ) / sd(asr), -3, 3) ~ 0,
        #     TRUE ~ 1
        # ),
        outlier_2sd = case_when(
            between(( asr - mean(asr) ) / sd(asr), -2, 2) ~ 0,
            TRUE ~ 1
        )
    ) 
    # %>%
    # filter(!outlier_2sd)

# write_csv(df_icu, "input/df_icu_orchestra.csv")
    
# 
# df_gps <- read_csv("../input/df_icu_red_gps.csv") %>%
#     mutate_at(c("has_med_res_cc", "is_icu_spec", "bc_md_247"), as.integer)
# %>%
# mutate(
#     outlier = ifelse(asr < quantile(asr, probs = 0.25) - IQR(asr) |
#                          asr > quantile(asr, probs = 0.75) + IQR(asr),
#                      1, 0),
#     outlier_md = ifelse(avg_nurse_10bed < quantile(avg_nurse_10bed, probs = 0.25) - IQR(avg_nurse_10bed) |
#                             avg_nurse_10bed > quantile(avg_nurse_10bed, probs = 0.75) + IQR(avg_nurse_10bed),
#                      1, 0)
# )
# %>%
# filter(outlier != 1, outlier_md != 1)


# Data Analysis -----------------------------------------------------------


# GPS distribution among treatment vars
# plot_gps_distribution <- df_gps %>% 
#     select(starts_with("gps"), starts_with("ps")) %>% 
#     pivot_longer(cols = everything(), values_to = "gps", names_to = "var") %>% 
#     mutate(var = str_remove(var, "gps_")) %>% 
#     ggplot() +
#     geom_histogram(aes(gps)) +
#     theme_bw() +
#     facet_grid(. ~ var, scales = "free")
# 
# 
# plot_asr_treat_vars <- 
#     df_gps %>% 
#     select(!contains("ps"), -c(orchestra_code, smr, sru)) %>% 
#     pivot_longer(cols = -asr, values_to = "val", names_to = "var") %>% 
#     ggplot() +
#     geom_point(aes(x = val, y = asr)) +
#     geom_smooth(aes(x = val, y = asr)) +
#     facet_wrap(. ~ var, scales = "free", nrow = 3, ncol = 3) +
#     theme_bw()
# 
# 
# 
# ggsave("../output/plot_asr_treat_vars.png", plot_asr_treat_vars, 
#        units = "in", width = 12, height = 5, dpi = 800)
# 


df_gps <- df_icu

# Causal forest -----------------------------------------------------------
df_conf_matrix <- read_csv("input/conf_matrix_orchIII_w_residency.csv") %>% 
    pivot_longer(-exposure, names_to = "confounder", values_to = "is_confounder") %>% 
    filter(is_confounder == 1)

v_exposure <- unique(df_conf_matrix$exposure)

# v_cat_variables <- c("has_med_res_cc", "is_icu_spec", "bc_md_247")
v_cat_variables <- c("is_icu_spec", "bc_md_247")

ls_te_result <- list()

ls_ite_pred <- list()

set.seed(2^31 - 1)
for (v in v_exposure) {
    print(v)
    v_sel_confounders <- df_conf_matrix %>% 
        filter(exposure == v) %>% 
        pull(confounder)
    
    conf_vars <- v_sel_confounders
    treat_var <- v
    # outcome <- c("asr")
    outcome <- c("eff_group_out")    
    # if (v %in% v_cat_variables) {
    #     conf_gps <- paste0("ps_", treat_var)
    #     
    # } else {
    #     conf_gps <- paste0("gps_", treat_var)
    # }
    
    # Causal forest settings    
    X <- df_gps %>% dplyr::select(all_of(conf_vars))
    W <- df_gps %>% dplyr::select(all_of(treat_var)) %>% pull()
    Y <- df_gps %>% dplyr::select(all_of(outcome))  %>% pull()
    # GPS <- df_gps %>% select(all_of(conf_gps)) %>% pull()
    
    model_crf <- causal_forest(X = X, Y = Y, W = W,
                               # tune.parameters = "all",
                               # tune.num.draws = 10000
                               # W.hat = GPS,
                               # honesty = FALSE,
                               # honesty.fraction = 0.7,
                               # honesty.prune.leaves = FALSE
                               tune.parameters = c("honesty.fraction",
                                                   "honesty.prune.leaves")
                               # tune.parameters = c("sample.fraction",
                               #                     "mtry", "min.node.size",
                               #                     "honesty.fraction",
                               #                     "honesty.prune.leaves",
                               #                     "alpha",
                               #                     "imbalance.penalty"),
                               # tune.num.trees = 2000,
                               # tune.num.reps = 500,
                               # num.trees = 10000
                               )
    # print(v)
    # print(test_calibration(model_crf))
    
    if (v %in% v_cat_variables) {
        # set.seed(2^31 - 1)
        avg_te <- average_treatment_effect(model_crf, num.trees.for.weights = 1000)
        
    } else {
        # set.seed(2^31 - 1)
        avg_te <- average_treatment_effect(model_crf, num.trees.for.weights = 1000)
    }
    
    ls_ite_pred[[v]] <-
        df_pred_ite <- 
        predict(model_crf, estimate.variance = TRUE) %>% 
        as_tibble() %>% 
        mutate(exposure = v, 
               ite_se = sqrt(variance.estimates),
               conf_low = (predictions - 1.96 * ite_se),
               conf_high = (predictions + 1.96 * ite_se)) %>% 
        arrange(predictions) %>% 
        mutate(i = 1:n()) %>% 
        mutate(
            sign = if_else(conf_low <= 0 & conf_high >= 0, 0, 1)
        )
    
    
    
    # (df_pred_ite %>% 
    #     ggplot() +
    #     geom_point(aes(x = i, y = predictions)) +
    #     geom_errorbar(aes(x = i, y = predictions, 
    #                       ymin = (predictions - 1.96 * ite_se), 
    #                       ymax = (predictions + 1.96 * ite_se))) +
    #     labs(
    #         x = "Unit",
    #         y = "ITE",
    #         title = v
    #     ) +
    #     theme_bw())

    
    
    
    ls_te_result[[which(v_exposure %in% v)]] <- 
        tibble(
            exposure = treat_var,
            confounders = paste0(conf_vars, collapse = "|"),
            te = round(avg_te[1], 4),
            se = round(avg_te[2], 4),
            ) %>% 
        mutate(
            ci_low =  avg_te[1] - 1.96 * avg_te[2],
            ci_high = avg_te[1] + 1.96 * avg_te[2]
        ) %>% 
        mutate(
            te = case_when(
                exposure == "hosp_bed" ~ round(100 * te, 5), 
                TRUE ~ round(te, 5)
            ),
            ci_low = case_when(
                exposure == "hosp_bed" ~ round(100 * ci_low, 5), 
                TRUE ~ round(ci_low, 5)
            ),
            ci_high = case_when(
                exposure == "hosp_bed" ~ round(100 * ci_high, 5), 
                TRUE ~ round(ci_high, 5)
            )
        ) %>% 
        mutate(
            te_ci_95 = paste0(round(te, 4), 
                              " [", round(ci_low, 4),
                              ", " , round(ci_high, 4), "]")
        ) %>% 
        mutate(
            sign = case_when(
                ci_low <= 0 & ci_high >= 0 ~ 0,
                TRUE ~ 1
                )
        )
    }


df_te_result <- 
    bind_rows(ls_te_result) %>% 
    select(exposure, ci_low, ci_high, te_ci_95, sign) 
    # %>% 
    # filter(exposure != "has_med_res_cc")



## Individual Treament Effect (ITE)
df_ite_result <- 
    ls_ite_pred %>% 
    bind_rows() %>% 
    # filter(exposure != "has_med_res_cc") %>% 
    mutate(
        predictions = if_else(exposure == "hosp_bed", predictions * 100, predictions),
        conf_low  = if_else(exposure == "hosp_bed", conf_low  * 100, conf_low ),
        conf_high = if_else(exposure == "hosp_bed", conf_high * 100, conf_high)
    ) %>% 
    mutate(
        exposure = factor(exposure, 
                          levels = c("hosp_bed",
                                     "icu_hosp_bed",
                                     "has_med_res_cc",
                                     "is_icu_spec",
                                     "avg_md_10bed",
                                     "avg_nurse_10bed",
                                     "bc_md_247"),
                          labels = c("Number of hospital beds (x100)",
                                     "ICU beds per hospital beds ratio",
                                     "Presence of training program in critical care",
                                     "Specialized ICU",
                                     "Average Physicians per 10 beds",
                                     "Average Nurses per 10 beds",
                                     "Presence of a full-time board-certified intensivist")
                          )
        )

# df_pred_ite <- 
#     predict(model_crf, estimate.variance = TRUE) %>% 
#     as_tibble() %>% 
#     mutate(exposure = v, 
#            ite_se = sqrt(variance.estimates),
#            conf_low = (predictions - 1.96 * ite_se),
#            conf_high = (predictions + 1.96 * ite_se)) %>% 
#     arrange(predictions) %>% 
#     mutate(i = 1:n()) %>% 
#     mutate(
#         sign = if_else(conf_low <= 0 & conf_high >= 0, 0, 1)
#     )






# writexl::write_xlsx(df_te_result %>%
#                         select(exposure, ci_low, ci_high, te_ci_95, sign)
#                     , "output/results_orchestra_org/df_CRF_2SD.xlsx")


writexl::write_xlsx(df_te_result %>%
                        select(exposure, ci_low, ci_high, te_ci_95, sign)
                    , "output/results_orchestra_org/df_CRF_ALL.xlsx")

df_ite_result %>% 
    group_by(exposure) %>% 
    summarise(
        n_yes = sum(sign), 
        prop_yes = mean(sign)) 


plot_ite_crf <-     
    df_ite_result %>% 
    ggplot() +
    geom_point(aes(x = i, y = predictions, color = factor(sign)), size = 0.2) +
    geom_errorbar(aes(x = i, y = predictions, 
                      ymin = conf_low, 
                      ymax = conf_high, color = factor(sign)), size = 0.2) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(1, 128, 10)) +
    scale_color_manual(values = c("0" = "black", "1" = "blue"), guide = "none") +
    facet_wrap(. ~ exposure) +
    labs(
        x = "",
        y = "Conditional Average Treatment Effect (CATE)",
        # title = v
    ) +
    theme_bw()


# ggsave("output/plot_ite_crf_coloured_2SD.png", plot_ite_crf, 
#        dpi = 800, units = "in", width = 9, height = 5)
ggsave("output/plot_ite_crf_coloured_ALL.pdf", plot_ite_crf,
       dpi = 800, units = "in", width = 9, height = 5)


