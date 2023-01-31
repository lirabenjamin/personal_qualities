# 8.4 RC: IGR
library(stargazer)
if(rerun){
full <- read_rds("Data/Final_Data/full_data.rds")

library(mice)
imp <- read_rds('Data/Final_Data/imp.rds')
imp <- complete(imp, 'long')


# Just bert
f1 <- formula('grad6 ~ I(scale(prosocial))+I(scale(leadership))+I(scale(teamwork))+I(scale(mastery))+I(scale(perseverance))+I(scale(selfconcordance))+I(scale(goal))')
# Just demo
f2 <- formula('grad6 ~ +race+parentdegree+female+parentmarried+ell+title1hs+I(scale(grad6rates))')
# Just OSA
f3 <- formula('grad6 ~ I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))')
# Just stdtest
f4 <- formula('grad6 ~ I(scale(stdtest))')
# al but bert
f5 <- formula('grad6 ~ I(scale(stdtest))+I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))+race+parentdegree+female+parentmarried+ell+title1hs+I(scale(grad6rates))')
# all
f6 <- formula('grad6 ~ I(scale(prosocial))+I(scale(leadership))+I(scale(teamwork))+I(scale(mastery))+I(scale(perseverance))+I(scale(selfconcordance))+I(scale(goal))+
I(scale(stdtest))+I(scale(OSAn))+I(scale(OSAt))+I(scale(OSAsport))+race+parentdegree+female+parentmarried+ell+title1hs+I(scale(grad6rates))')

full <- mice::complete(imp)
auc = function(model){pROC::ci.auc(model$y, model$fitted.values) %>% as_tibble() %>% mutate(name = c("lo","auc","hi")) %>% spread(name,value)}
brier = function(model){psfmi::scaled_brier(model$y, model$fitted.values)}

models_igr = c(f1,f2,f3,f4,f5,f6) |> 
  enframe() |> 
  mutate(base_glm = map(value, run_base),
         imputed_glm = map(value,run_imp_pipeline))

write_rds(models_igr, "Output/imputed_regression_results_igr.rds")
}
# Checkpoint --------------------------------------------------------------
models_igr <- read_rds("Output/imputed_regression_results_igr.rds")

models_igr |> unnest(imputed_glm) |> 
  ggplot(aes(term,OR, ymin= low, ymax =high))+
  geom_point()+
  geom_errorbar()+
  facet_wrap(~name)+
  coord_flip()


stargazer_mice_or(models_igr$base_glm, imputed_glm = models_igr$imputed_glm, odd.ratio = T,type = "html", ci = F,out = "Output/glms_igr.html")
