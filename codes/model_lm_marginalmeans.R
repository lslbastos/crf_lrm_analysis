##%######################################################%##
#                                                          #
####      Causal Forests - Orchestra - @lslbastos       ####
#                                                          #
##%######################################################%##


# library -----------------------------------------------------------------
library(tidyverse)
# library(tidylog)
library(marginaleffects)


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
    ) %>%
    filter(!outlier_2sd)





### Marginal means estimates
model_hosp_bed <- 
    lm(asr ~
           hosp_bed * has_med_res_cc +
           hosp_bed * avg_md_10bed +
           hosp_bed * avg_nurse_10bed
       , 
       data = df_icu %>%
           mutate(has_med_res_cc = factor(has_med_res_cc),
                  is_icu_spec = factor(is_icu_spec),
                  bc_md_247 = factor(bc_md_247),
                  )
       )


model_icu_hosp_bed <- 
    lm(asr ~
           icu_hosp_bed * avg_md_10bed +
           icu_hosp_bed * icu_occupancy_rate
       , 
       data = df_icu %>%
           mutate(has_med_res_cc = factor(has_med_res_cc),
                  is_icu_spec = factor(is_icu_spec),
                  bc_md_247 = factor(bc_md_247),
           )
    )




model_has_med_res_cc <- 
    lm(asr ~
           has_med_res_cc * hosp_bed +
           has_med_res_cc * is_icu_spec +
           has_med_res_cc * avg_md_10bed +
           has_med_res_cc * bc_md_247 +
           has_med_res_cc * avg_nurse_10bed +
           has_med_res_cc * adm_bed 
       , 
       data = df_icu %>%
           mutate(has_med_res_cc = factor(has_med_res_cc),
                  is_icu_spec = factor(is_icu_spec),
                  bc_md_247 = factor(bc_md_247),
           )
    )



model_is_icu_spec <- 
    lm(asr ~
           is_icu_spec * hosp_bed +
           is_icu_spec * has_med_res_cc
       , 
       data = df_icu %>%
           mutate(has_med_res_cc = factor(has_med_res_cc),
                  is_icu_spec = factor(is_icu_spec),
                  bc_md_247 = factor(bc_md_247),
           )
    )


model_avg_md_10bed <- 
    lm(asr ~
           avg_md_10bed * hosp_bed +
           avg_md_10bed * icu_hosp_bed +
           avg_md_10bed * is_icu_spec +
           avg_md_10bed * avg_nurse_10bed
       , 
       data = df_icu %>%
           mutate(has_med_res_cc = factor(has_med_res_cc),
                  is_icu_spec = factor(is_icu_spec),
                  bc_md_247 = factor(bc_md_247),
           )
    )


model_avg_nurse_10bed <- 
    lm(asr ~
           avg_nurse_10bed * hosp_bed +
           avg_nurse_10bed * icu_hosp_bed +
           avg_nurse_10bed * has_med_res_cc +
           avg_nurse_10bed * is_icu_spec +
           avg_nurse_10bed * avg_md_10bed +
           avg_nurse_10bed * bc_md_247 +
           avg_nurse_10bed * icu_occupancy_rate
           
       , 
       data = df_icu %>%
           mutate(has_med_res_cc = factor(has_med_res_cc),
                  is_icu_spec = factor(is_icu_spec),
                  bc_md_247 = factor(bc_md_247),
           )
    )



model_bc_md_247 <- 
    lm(asr ~
           bc_md_247 * hosp_bed +
           bc_md_247 * is_icu_spec +
           bc_md_247 * avg_nurse_10bed
       , 
       data = df_icu %>%
           mutate(has_med_res_cc = factor(has_med_res_cc),
                  is_icu_spec = factor(is_icu_spec),
                  bc_md_247 = factor(bc_md_247),
           )
    )



tidy_mmeans <- function(model, var) {
    
    model_means <- marginaleffects(model, 
                    variables = var) %>% 
        tidy()
    
    return(model_means)
    
}




df_model_mmeans <- 
    bind_rows(
        tidy_mmeans(model_hosp_bed, "hosp_bed"),
        tidy_mmeans(model_icu_hosp_bed, "icu_hosp_bed"),
        tidy_mmeans(model_has_med_res_cc, "has_med_res_cc"),
        tidy_mmeans(model_is_icu_spec, "is_icu_spec"),
        tidy_mmeans(model_avg_md_10bed, "avg_md_10bed"),
        tidy_mmeans(model_avg_nurse_10bed, "avg_nurse_10bed"),
        tidy_mmeans(model_bc_md_247, "bc_md_247")
    ) %>% 
    select(term, estimate, conf.low, conf.high) %>% 
    mutate(
        estimate = case_when(
            term == "hosp_bed" ~ round(100 * estimate, 5), 
            TRUE ~ round(estimate, 5)
            ),
        conf.low = case_when(
            term == "hosp_bed" ~ round(100 * conf.low, 5), 
            TRUE ~ round(conf.low, 5)
            ),
        conf.high = case_when(
            term == "hosp_bed" ~ round(100 * conf.high, 5), 
            TRUE ~ round(conf.high, 5)
        )
    ) %>% 
    mutate(
        estimate_ci95 = paste0(round(estimate, 4), 
               " [", round(conf.low, 4),
               ", " , round(conf.high, 4), "]")
    )



 writexl::write_xlsx(df_model_mmeans %>%
                         select(term, estimate, conf.low, conf.high, estimate_ci95)
                     , "output/results_orchestra_org/df_LM_EMMEANS_ALL.xlsx")
#writexl::write_xlsx(df_model_mmeans %>%
#                        select(term, estimate, conf.low, conf.high, estimate_ci95)
#                    , "output/results_orchestra_org/df_LM_EMMEANS_2SD.xlsx")

