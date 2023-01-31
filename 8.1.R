# 8.1 Main models shown in text
library(stargazer)
full <- read_rds("Data/Final_Data/full_data.rds")

# Custom Functions --------------------------------------------------------
tidy.mira <-  function(mipo){
  pool <-  pool(mipo$glm)
  print(glue::glue("{pool$pooled |> slice(1) |> pull(m)} models achieved succesfully"))
  
  tidy  <-
    pool |>
    summary(conf.int = T, exp = T) %>%
    select(
      term,
      OR = estimate,
      p = p.value,
      se = std.error,
      low = `2.5 %`,
      high = `97.5 %`
    ) |> 
    mutate(term = as.character(term))
  
  class(tidy) = 'data.frame'
  
  return(tidy)
}
glance.mira <- function(mipo){
  glance  = 
    mipo %>% 
    mutate(
      predictions = map(glm, predict, type = "response"),
      roc.i = map2(glm, predictions, function(value, predictions) {
        pROC::roc(value$y, predictions, quiet = TRUE)$auc
      }),
      se.roc.i = map(roc.i, function(roc.i) {
        sqrt(pROC::var(roc.i))
      }),
      rsq.i = map(glm, psfmi::rsq_nagel),
      sc_brier.i = map2(glm, predictions, function(x, y) {
        psfmi::scaled_brier(x$y, y)
      })
    ) %>%
    select(-predictions)
  
  rsq.n = (unlist(glance$rsq.i) %>% atanh %>% mean %>% tanh)
  
  glance = glance %>% 
    ungroup() |> 
    summarise(roc_res = psfmi::pool_auc(roc.i, se.roc.i, nimp = 25, log_auc = TRUE),
              sc_brier = mean(unlist(sc_brier.i))) %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rbind(c("r2",rsq.n)) %>% 
    as_tibble() |> 
    rename(stat = rowname, value = V1) |> 
    mutate(value = as.numeric(value)) |> 
    spread(stat,value) |> 
    mutate(nobs = mipo$glm[[1]] |> broom::glance() |> pull(nobs))
  class(glance) = 'data.frame'
  return(glance)
}
run_base <- function(formula){
  model = glm(formula, data = full |> slice_sample(n = 1000), family = 'binomial')
  return((model))
}
run_imputed <- function(formula){
  mira = imp |> 
    group_by(.imp) |>
    nest() |> 
    mutate(glm = map(data,function(x){
      glm(formula,data = x, family = binomial)
    })) |> 
    select(glm)
  return(mira)
}
tidyglance <- function(mira){
  tidy <- mira |> tidy.mira()
  glance <- mira |> glance.mira()
  return(cbind(tidy,glance))
}
run_imp_pipeline <- function(formula){run_imputed(formula) |> tidyglance()}
stargazer_mice_or <-function(base_model,imputed_glm,odd.ratio = F,no.space = T,ci = F,omit.stat = c("aic", "n", "ll"),...) {
    if(!ci){ci_custom = NULL}
    
    if (!("list" %in% class(base_model)))
      model <- list(base_model)
    
    or = imputed_glm |> map("OR")
    ci_custom = imputed_glm |> map(function(x) {x |> select(c('low', 'high'))})
    se = imputed_glm |> map("se")
    p = imputed_glm |> map("p")
    
    auc = map(imputed_glm,function(x){slice(x,1)}) |> map_dbl("roc_res.C-statistic") |> Ben::numformat(3)
    nobs = map(imputed_glm,function(x){slice(x,1)}) |> map_dbl("nobs") |> papaja::print_num(digits = 0)
    brier = map(imputed_glm,function(x){slice(x,1)}) |> map_dbl("sc_brier")|> Ben::numformat(3)
    r2 = map(imputed_glm,function(x){slice(x,1)}) |> map_dbl("r2")|> Ben::numformat(3)
    
    
    add.lines = list(c("N",nobs),
                     c("AUC",auc),
                     c("R2",r2),
                     c("Scaled Brier", brier))
    
    stargazer::stargazer(
      base_model,
      coef = or,
      p = p,
      se = se,
      ci = ci,
      # ci.custom = ci_custom,
      table.layout = "-#-t-sa-n",
      add.lines = add.lines,
      no.space = no.space,
      star.cutoffs = c(.05, .01, .001),
      omit.stat = omit.stat,...
    )
  }

# Incremental Predictive Validity -----------------------------------------
if(rerun){
# Bivariate
read_rds("Data/Final_Data/full_data.rds") |> 
  select(all_of(v_bert), grad6) |> 
  Ben::HARcor()

library(mice)
imp <- read_rds('Data/Final_Data/imp.rds')
imp <- complete(imp, 'long')

# Just bert
f1 <- formula('grad6 ~ I(scale(prosocial))+I(scale(leadership))+I(scale(teamwork))+I(scale(mastery))+I(scale(perseverance))+I(scale(selfconcordance))+I(scale(goal))')
# Just demo
f2 <- formula('grad6 ~ +race+parentdegree+female+parentmarried+ell+title1hs')
# Just OSA
f3 <- formula('grad6 ~ I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))')
# Just stdtest
f4 <- formula('grad6 ~ I(scale(stdtest))')
# al but bert
f5 <- formula('grad6 ~ I(scale(stdtest))+I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))+race+parentdegree+female+parentmarried+ell+title1hs')
# all
f6 <- formula('grad6 ~ I(scale(prosocial))+I(scale(leadership))+I(scale(teamwork))+I(scale(mastery))+I(scale(perseverance))+I(scale(selfconcordance))+I(scale(goal))+
I(scale(stdtest))+I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))+race+parentdegree+female+parentmarried+ell+title1hs')

full <- mice::complete(imp)
auc = function(model){pROC::ci.auc(model$y, model$fitted.values) %>% as_tibble() %>% mutate(name = c("lo","auc","hi")) %>% spread(name,value)}
brier = function(model){psfmi::scaled_brier(model$y, model$fitted.values)}

models = c(f1,f2,f3,f4,f5,f6) |> 
  enframe() |> 
  mutate(base_glm = map(value, run_base),
         imputed_glm = map(value,run_imp_pipeline))

write_rds(models, "Output/imputed_regression_results.rds")
}
# Checkpoint --------------------------------------------------------------
models <- read_rds("Output/imputed_regression_results.rds")

models |> unnest(imputed_glm) |> 
  ggplot(aes(term,OR, ymin= low, ymax =high))+
  geom_point()+
  geom_errorbar()+
  facet_wrap(~name)+
  coord_flip()

(models$imputed_glm[6][[1]])[1:8,] |> 
  as_tibble() |> 
  mutate(p = round(p, 4))



stargazer_mice_or(models$base_glm, imputed_glm = models$imputed_glm, odd.ratio = T,type = "html", ci = F,  out = "Output/glms_main.html")
stargazer_mice_or(models$base_glm[c(1,6)], imputed_glm = models$imputed_glm[c(1,6)], odd.ratio = T,type = "latex", ci = F,  out = "Output/glms_main.tex")


