# 8.3 RC: Grades
library(stargazer)
full <- read_rds("Data/Final_Data/full_data.rds")
ids <- full |> filter(!is.na(gpa)) |> pull(id)

if(rerun){
library(mice)
imp <- read_rds('Data/Final_Data/imp.rds')
imp <- complete(imp, 'long')
imp <- imp |> filter(id %in% ids)



# Just bert
f1 <- formula('grad6 ~ I(scale(prosocial))+I(scale(leadership))+I(scale(teamwork))+I(scale(mastery))+I(scale(perseverance))+I(scale(selfconcordance))+I(scale(goal))')
# Just demo
f2 <- formula('grad6 ~ +race+parentdegree+female+parentmarried+ell+title1hs')
# Just OSA
f3 <- formula('grad6 ~ I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))')
# Just stdtest
f4 <- formula('grad6 ~ I(scale(stdtest)) + I(scale(gpa))')
# al but bert
f5 <- formula('grad6 ~ I(scale(stdtest))+ I(scale(gpa))+I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))+race+parentdegree+female+parentmarried+ell+title1hs')
# all
f6 <- formula('grad6 ~ I(scale(prosocial))+I(scale(leadership))+I(scale(teamwork))+I(scale(mastery))+I(scale(perseverance))+I(scale(selfconcordance))+I(scale(goal))+
I(scale(stdtest))+ I(scale(gpa))+I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))+race+parentdegree+female+parentmarried+ell+title1hs')

full <- mice::complete(imp)
auc = function(model){pROC::ci.auc(model$y, model$fitted.values) %>% as_tibble() %>% mutate(name = c("lo","auc","hi")) %>% spread(name,value)}
brier = function(model){psfmi::scaled_brier(model$y, model$fitted.values)}

models_gpa = c(f1,f2,f3,f4,f5,f6) |> 
  enframe() |> 
  mutate(base_glm = map(value, run_base),
         imputed_glm = map(value,run_imp_pipeline))

write_rds(models_gpa, "Output/imputed_regression_results_gpa.rds")
}
# Checkpoint --------------------------------------------------------------
models_gpa <- read_rds("Output/imputed_regression_results_gpa.rds")

models_gpa |> unnest(imputed_glm) |> 
  ggplot(aes(term,OR, ymin= low, ymax =high))+
  geom_point()+
  geom_errorbar()+
  facet_wrap(~name)+
  coord_flip()

stargazer_mice_or <-
  function(base_model,
           imputed_glm,
           odd.ratio = F,
           no.space = T,
           omit.stat = c("aic", "n", "ll"),
           ...) {
    
    if (!("list" %in% class(base_model)))
      model <- list(base_model)
    
    or = imputed_glm |> map("OR")
    ci = imputed_glm |> map(function(x) {x |> select(c('low', 'high'))})
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
      table.layout = "-#-t-sa-n",
      # ci.custom = ci,
      add.lines = add.lines,
      no.space = no.space,
      omit.stat = omit.stat,...
    )
  }


stargazer_mice_or(models_gpa$base_glm, imputed_glm = models_gpa$imputed_glm, odd.ratio = T,type = "html", ci = F,star.cutoffs = c(.05, .01, .001), out = "Output/glms_gpa.html")
