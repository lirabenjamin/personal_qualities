# 8.1 Models in Development Sample
library(stargazer)
if(rerun){
dev <- read_rds("Data/Final_Data/development.rds")
# Rename varibales so that models pick up human ratings rather than model scores
dev <- dev |> rename_with(function(x)str_remove(x, "_hr"))

# Formulas
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

auc = function(model){pROC::ci.auc(model$y, model$fitted.values) %>% as_tibble() %>% mutate(name = c("lo","auc","hi")) %>% spread(name,value)}
run_model0 = function(formula){glm(formula, 'binomial',dev)}
run_model = function(formula){
  model = glm(formula, 'binomial',dev)
  tidy = broom::tidy(model,conf.int = T, exp = T) |>  
    mutate(se = std.error*estimate) |> 
    select(
    term,
    OR = estimate,
    se,
    p = p.value,
    low = conf.low,
    high = conf.high
  )
  glance = broom::glance(model)
  r2 = psfmi::rsq_nagel(model)
  auc = auc(model)
  glance = glance |> mutate(r2  = r2) |> cbind(auc)
  result = cbind(tidy,glance)
  return(result)
    }

models_dev = c(f1,f2,f3,f4,f5,f6) |> 
  enframe() |> 
  mutate(model0 = map(value,run_model0),
           glm = map(value, run_model))

write_rds(models_dev, "Output/imputed_regression_results_dev.rds")
}
# Checkpoint --------------------------------------------------------------
models_dev <- read_rds("Output/imputed_regression_results_dev.rds")

stargazer_or <-
  function(model0,
           base_model,
           odd.ratio = T,
           no.space = T,
           ci = F,
           star.cutoffs = c(0.05, 0.01, 0.001),
           table.layout = "-#-t-sa-n",
           omit.stat = c("aic", "n", "ll"),
           ...) {
    
    if (!("list" %in% class(model0)))
      model <- list(model0)
    
    or = base_model |> map("OR")
    se = base_model |> map("se")
    ci_custom = base_model |> map(function(x) {x |> select(c('low', 'high'))})
    p = base_model |> map("p")
    
    auc = map(base_model,function(x){slice(x,1)}) |> map_dbl("auc") |> Ben::numformat(3)
    nobs = map(base_model,function(x){slice(x,1)}) |> map_dbl("nobs") |> papaja::print_num(digits = 0)
    r2 = map(base_model,function(x){slice(x,1)}) |> map_dbl("r2")|> Ben::numformat(3)
    
    
    add.lines = list(c("N",nobs),
                     c("AUC",auc),
                     c("R2",r2))
    if(!ci){ci_custom = NULL}
    stargazer(model0,
      coef = or,
      p = p,
      se = se,
      ci.custom = ci_custom,
      add.lines = add.lines,
      no.space = no.space,
      star.cutoffs = star.cutoffs,
      table.layout = table.layout,
      omit.stat = omit.stat,...
    )
  }


stargazer_or(models_dev$model0,models_dev$glm, odd.ratio = T,type = "html", ci = F, out = "Output/glms_dev.html")

