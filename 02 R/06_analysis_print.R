rm(list = ls())

pc.wd = "Q:\\Research\\Data\\14_Orbian_transaction\\02 RData"
github.dir = "Q:\\Research\\Projects\\01 DynamicDiscountingRF (DynDis)"
artwork.dir = paste(github.dir,"\\01 Latex\\Figures\\",sep="")
models.dir = "Q:\\Research\\Data\\14_Orbian_transaction\\03 Models"

inArtDir <- function(file.name) return(paste(artwork.dir,file.name,sep=""))
inModelsDir <- function(file.name) return(paste(models.dir,file.name,sep=""))

if(dir.exists(file.path(pc.wd, "."))) setwd(pc.wd)

library(dplyr)
library(modelsummary)
library(survival)
library(ggplot2)

source( "./02 R/00_loadFonts.R" %>% paste(github.dir ,., sep=""))
# models

load("LABELS.rds" %>% inModelsDir())

load(file   = "main.models.rds" %>% inModelsDir())

load(file   = "size.models.rds" %>% inModelsDir())

load(file   = "main.models.biz.rds" %>% inModelsDir())

load(file   = "size.models.biz.rds" %>% inModelsDir())

load(file   = "aft.main.models.rds" %>% inModelsDir())




# corresponding data

load("dynDis.data.biz.rData" %>% inModelsDir())
load("dynDis.data.rData" %>% inModelsDir())

#################
#
# Adapt MSUMMARY for my latex style
#
#################

msummary.tex <- function(m_list,SEQUENCE.MODEL,TERM.AFTER = LABEL.days.until.quarter.end,EXTRA.TERMS = NULL){
  
  term.sequence = broom::tidy(SEQUENCE.MODEL)$term
  if(!is.null(EXTRA.TERMS)){
    term.sequence = c(term.sequence,EXTRA.TERMS)
  }
  elements.to.move <- term.sequence[grepl("\\\\times", term.sequence)]
  term.sequence <- term.sequence[!term.sequence %in% elements.to.move]
  index.to.insert <- which(term.sequence == TERM.AFTER) + 1
  term.sequence <- append(term.sequence, elements.to.move, after = index.to.insert - 1)
  #print(term.sequence)
  
  first = T
  MSUMMARY <- msummary(m_list, stars=TRUE,output="data.frame",coef_map = term.sequence) %>%
      mutate(across(where(is.numeric), ~ sprintf("%.3f", .)))

  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("approval.days.until.month.end", LABEL.approval.days.until.month.end, x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("approval.days.until.quarter.end", LABEL.approval.days.until.quarter.end, x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("days.until.month.end", LABEL.days.until.month.end, x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("days.until.quarter.end",LABEL.days.until.quarter.end, x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Num.decisions", "Num. decisions", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Num.invoices", "Num. invoices", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub(".fixed.", " fixed ", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\+", "$^{+}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Rsq", "$R^2$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("Num..obs.", "$N$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*\\*", "$^{xxx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*", "$^{xx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*", "$^{x}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xxx\\}", "{***}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xx\\}", "{**}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{x\\}", "{*}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("TRUE", " (1=yes)", x)}))

  

  N.models <- dim(MSUMMARY)[2]-3
  N.all.rows <- dim(MSUMMARY)[1]
  N.rows <- sum(MSUMMARY$part=="estimates")
  out = ""
  
  for(i in 1:N.models)
    #out = paste(out,"&& \\muc{1.5cm}{(",i,")}", sep="")
    out = paste(out,"&& \\suc{(",i,")}", sep="")
  
  out = paste(out,"\\\\\n",sep="")
  
  for(i in 1:N.models)
    out = paste(out,"\\cline{",2*i+1,"-",2*i+1,"}", sep="")
  
  out = paste(out,"\n",sep="")
  
  for(i in 1:N.rows){
    if(grepl("2011",MSUMMARY[i,"term"])&&first){
      out = paste(out,"adoption year\\\\\n")
      first=F
    }
    if(MSUMMARY[i,"statistic"]=="estimate"){
      #if(MSUMMARY[i,"statistic"]=="modelsummary_tmp1"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        out = paste(out,"&&", MSUMMARY[i,j], sep="")
      }
      out = paste(out,"\\\\\n", sep="")
    }
    if(MSUMMARY[i,"statistic"]=="std.error"){
      for(j in 4:(N.models+3)){
        out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
      }
      out = paste(out,"\\\\\n", sep="")
    }
  }
  out = paste(out,"\\midrule\n", sep="")
  for(i in 1:N.all.rows){
    if(MSUMMARY[i,"part"]=="gof"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        if(MSUMMARY[i,"term"]=="$N$"){
          out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
        }
        else{
          out = paste(out,"&&", MSUMMARY[i,j], sep="")
        }
      }
      out = paste(out,"\\\\\n", sep="")
    }
  }

  cat(out)
  clipr::write_clip(out)
}

#################
#
# CoxPH for MSUMMARY
#
#################

CoxPH.to.list <- function(COXPH) {
  N.decisions <- COXPH %>% model.frame(.) %>% nrow()
  N.invoices  <- COXPH %>% nobs(.) %>% as.numeric()
  AIC         <- COXPH %>% AIC(.) %>% round(3)  # Explicit rounding to 3 digits
  rmse        <- get_gof(COXPH)["rmse"] %>% round(3)  # Explicit rounding to 3 digits
  
  # Fixed effects remain as strings
  supplier.industry.fixed.effect <- "\\suc{(included)}"
  supplier.country.fixed.effect <- "\\suc{(included)}"
  buyer.fixed.effect <- "\\suc{(included)}"
  year.fixed.effect <- "\\suc{(included)}"
  
  # Process the model estimates
  fitted.df <- COXPH %>%
    broom::tidy(.) %>%
    subset(!grepl("BID", term)) %>%
    subset(!grepl("year", term)) %>%
    subset(!grepl("supplier.industry", term)) %>%
    subset(!grepl("supplier.country", term)) %>%
    mutate(term = stringr::str_replace_all(term, "supplier.industry", "")) %>%
    mutate(term = stringr::str_replace_all(term, "&", "\\\\&")) %>%
    mutate(term = stringr::str_replace_all(term, "supplier.country", "")) %>%
    mutate(term = stringr::str_replace_all(term, "year", "")) %>%
    mutate(term = stringr::str_replace_all(term, "[.]", " ")) %>%
    mutate(term = stringr::str_replace_all(term, ":", "$\\\\times$")) %>%
    mutate(term = stringr::str_replace_all(term, "approval days until month end", LABEL.approval.days.until.month.end)) %>%
    mutate(term = stringr::str_replace_all(term, "approval days until quarter end", LABEL.approval.days.until.quarter.end)) %>%
    mutate(term = stringr::str_replace_all(term, "days until quarter end", LABEL.days.until.quarter.end)) %>%
    mutate(term = stringr::str_replace_all(term, "days until month end", LABEL.days.until.month.end)) %>%
    dplyr::select(-c("std.error", "statistic"))
  
  # Ensure numeric fields are explicitly rounded
  fit.df <- data.frame(
    supplier.industry.fixed.effect,
    supplier.country.fixed.effect,
    buyer.fixed.effect,
    year.fixed.effect,
    N.decisions = N.decisions,
    N.invoices = N.invoices,
    AIC = round(AIC, 3),
    rmse = round(rmse, 3),
    stringsAsFactors = FALSE  # Avoid coercing to factors
  )
  
  out.list <- list(estimates = fitted.df, fit = fit.df)
  class(out.list) <- c("custom_coxph", class(out.list))
  return(out.list)
}


#################
#
# CoxPH for MSUMMARY
#
#################

flexsurv.to.list <- function(FLEXSURV){
  N.decisions = FLEXSURV %>% model.frame(.) %>% nrow
  N.invoices  = FLEXSURV %>% nobs(.) %>% as.numeric
  AIC         = FLEXSURV %>% AIC(.)
  rmse        = get_gof(FLEXSURV)["rmse"]
  buyer.fixed.effect = "(included)"
  year.fixed.effect = "(included)"
  
  
  fitted.df <- FLEXSURV %>%
    broom::tidy(.) %>%
    subset(!grepl("BID",term))%>%
    subset(!grepl("year",term))%>%
    mutate(term = stringr::str_replace_all(term, "supplier.industry", ""))%>%
    mutate(term = stringr::str_replace_all(term, "&", "\\\\&"))%>%
    mutate(term = stringr::str_replace_all(term, "supplier.country", ""))%>%
    mutate(term = stringr::str_replace_all(term, "year", ""))%>%
    mutate(term = stringr::str_replace_all(term, "[.]", " "))%>%
    mutate(term = stringr::str_replace_all(term, ":", "$\\\\times$")) %>%
    mutate(term = stringr::str_replace_all(term, "days until quarter end", "days to next quarter end"))%>%
    dplyr::select(-c("statistic")) %>% 
    subset(term != "shape" & term != "scale")
  
  
  fit.df <- data.frame(buyer.fixed.effect,year.fixed.effect,N.decisions, N.invoices,AIC,rmse) 
  out.list <- list(estimates = fitted.df, fit = fit.df)
  
  class(out.list) <- c("custom_aft", class(out.list))
  return(out.list)
}

tidy.custom_coxph <- function(in.list, ...) {
  # Extract estimates
  x <- in.list$estimates
  
  # Extract fixed effects and format as rows
  fixed.effects <- data.frame(
    term = c(      
      "supplier industry fixed effect",
      "supplier country fixed effect",
      "buyer fixed effect",
      "year fixed effect"
    ),
    estimate = c(
      in.list$fit$supplier.industry.fixed.effect,
      in.list$fit$supplier.country.fixed.effect,
      in.list$fit$buyer.fixed.effect,
      in.list$fit$year.fixed.effect
    ),
    std.error = NA,
    p.value = NA
  )
  
  # Combine fixed effects with the model estimates
  combined <- rbind(
    data.frame(
      term = x$term,
      estimate = sprintf("%.3f",x$estimate),     
      std.error = round(x$robust.se,3),   
      p.value = x$p.value        
    ),
    fixed.effects
  )
  
  return(combined)
}

glance.custom_coxph <-function(in.list,...){
  x <- in.list$fit
  data.frame(
   # Buyer.fixed.effect=x$buyer.fixed.effect %>% paste("\\suc{",.,"}",sep=""),
   # Year.fixed.effect=x$year.fixed.effect%>% paste("\\suc{",.,"}",sep=""),
    Num.decisions=x$N.decisions%>% paste("\\suc{",.,"}",sep=""),
    Num.invoices=x$N.invoices%>% paste("\\suc{",.,"}",sep=""),
    AIC=round(x$AIC,2)%>% paste("\\suc{",.,"}",sep=""),
   # RMSE=round(x$rmse,3)%>% paste("\\suc{",.,"}",sep=""),
    model="\\suc{Cox PH}"
  )
}
  
tidy.custom_aft <- function(in.list, ...){
  x <- in.list$estimates
  data.frame(
    term =  x$term,
    estimate = x$estimate,
    std.error = x$std.error,
    p.value = x$p.value
  )
}

glance.custom_aft <-function(in.list,...){
  x <- in.list$fit
  data.frame(
    Num.decisions=x$N.decisions%>% paste("\\suc{",.,"}",sep=""),
    Num.invoices=x$N.invoices%>% paste("\\suc{",.,"}",sep=""),
    AIC=round(x$AIC,2)%>% paste("\\suc{",.,"}",sep=""),
    RMSE=round(x$rmse,3)%>% paste("\\suc{",.,"}",sep=""),
    model="\\suc{AFT}"
  )
}

#################
#
# Main
#
#################

list(coxph.controls.fm = coxph.controls.fm %>% CoxPH.to.list(),
     coxph.eom.fm = coxph.eom.fm %>% CoxPH.to.list(),
     coxph.eoq.fm = coxph.eoq.fm %>% CoxPH.to.list(),
     coxph.all.dates.fm = coxph.all.dates.fm %>% CoxPH.to.list(),
     coxph.interaction.eom.fm = coxph.interaction.eom.fm %>% CoxPH.to.list(),
     coxph.interaction.eoq.fm = coxph.interaction.eoq.fm %>% CoxPH.to.list(),
     coxph.interaction.fm = coxph.interaction.fm %>% CoxPH.to.list()
) %>% msummary.tex(.,SEQUENCE.MODEL= coxph.interaction.fm %>% CoxPH.to.list())



# function for likelihood ratio test with LaTeX output
lr_test_latex <- function(model1, model2) {
  LR_statistic <- 2 * (model2$loglik[2] - model1$loglik[2])
  df <- length(coef(model2)) - length(coef(model1))
  p_value <- pchisq(LR_statistic, df, lower.tail = FALSE)
  
  # Construct the LaTeX output string
  latex_output <- paste0("$\\left(\\chi^2 = ", round(LR_statistic, 2), 
                         "; \\text{df} = ", df, 
                         "; p ", ifelse(p_value < 0.001, "< 0.001", paste0("= ", formatC(p_value, format = "f", digits = 4))), "\\right)$")
  
  print(latex_output)
  clipr::write_clip(latex_output)
}

# usage
lr_test_latex(coxph.controls.fm, coxph.eom.fm)
lr_test_latex(coxph.controls.fm, coxph.eoq.fm)
lr_test_latex(coxph.eom.fm, coxph.all.dates.fm)
lr_test_latex(coxph.eoq.fm, coxph.all.dates.fm)




#################
#
# Size split
#
#################

list(coxph.eom.large.suppliers.fm = coxph.eom.large.suppliers.fm %>% CoxPH.to.list(),
     coxph.eoq.large.suppliers.fm = coxph.eoq.large.suppliers.fm %>% CoxPH.to.list(),
     coxph.all.dates.large.suppliers.fm = coxph.all.dates.large.suppliers.fm %>% CoxPH.to.list(),
     coxph.eom.small.suppliers.fm = coxph.eom.small.suppliers.fm %>% CoxPH.to.list(),
     coxph.eoq.small.suppliers.fm = coxph.eoq.small.suppliers.fm %>% CoxPH.to.list(),
     coxph.all.dates.small.suppliers.fm = coxph.all.dates.small.suppliers.fm %>% CoxPH.to.list()
) %>% msummary.tex(.,SEQUENCE.MODEL= coxph.all.dates.small.suppliers.fm %>% CoxPH.to.list()) #, EXTRA.TERMS=c("days until quarter end","days until next random event")




#################
#
# Only business dates main models
#
#################


list(coxph.controls.biz.fm = coxph.controls.biz.fm %>% CoxPH.to.list(),
     coxph.eom.biz.fm = coxph.eom.biz.fm %>% CoxPH.to.list(),
     coxph.eoq.biz.fm = coxph.eoq.biz.fm %>% CoxPH.to.list(),
     coxph.all.dates.biz.fm = coxph.all.dates.biz.fm %>% CoxPH.to.list(),
     coxph.interaction.eom.biz.fm = coxph.interaction.eom.biz.fm %>% CoxPH.to.list(),
     coxph.interaction.eoq.biz.fm = coxph.interaction.eoq.biz.fm %>% CoxPH.to.list(),
     coxph.interaction.biz.fm = coxph.interaction.biz.fm %>% CoxPH.to.list()
) %>% msummary.tex(.,SEQUENCE.MODEL= coxph.interaction.biz.fm %>% CoxPH.to.list())


#################
#
# Only business dates size split
#
#################


list(coxph.eom.large.suppliers.biz.fm = coxph.eom.large.suppliers.biz.fm %>% CoxPH.to.list(),
     coxph.eoq.large.suppliers.biz.fm = coxph.eoq.large.suppliers.biz.fm %>% CoxPH.to.list(),
     coxph.all.dates.large.suppliers.biz.fm = coxph.all.dates.large.suppliers.biz.fm %>% CoxPH.to.list(),
     coxph.eom.small.suppliers.biz.fm = coxph.eom.small.suppliers.biz.fm %>% CoxPH.to.list(),
     coxph.eoq.small.suppliers.biz.fm = coxph.eoq.small.suppliers.biz.fm %>% CoxPH.to.list(),
     coxph.all.dates.small.suppliers.biz.fm = coxph.all.dates.small.suppliers.biz.fm %>% CoxPH.to.list()
) %>% msummary.tex(.,SEQUENCE.MODEL= coxph.all.dates.small.suppliers.biz.fm %>% CoxPH.to.list()) #, EXTRA.TERMS=c("days until quarter end","days until next random event")




#################
#
# AFT
#
#################
   
list(aft.controls.fm = aft.controls.fm %>% flexsurv.to.list(),
     aft.payday.fm = aft.payday.fm %>% flexsurv.to.list(),
     aft.eom.fm = aft.eom.fm %>% flexsurv.to.list(),
     aft.eoq.fm = aft.eoq.fm %>% flexsurv.to.list(),
     aft.all.dates.fm = aft.all.dates.fm %>% flexsurv.to.list(),
     aft.only.effects.fm = aft.only.effects.fm %>% flexsurv.to.list()
) %>% msummary.tex(.,SEQUENCE.MODEL= aft.all.dates.fm %>% flexsurv.to.list())




#################
#
# Print counter factual, prepare data first
#
#################


se <- function(x) sd(x)/sqrt(length(x))
unscale <- function(x) x * attr(x, "scaled:scale") + attr(x, "scaled:center")

# Combine the data to create one comprehensive data frame with all relevant variables
ggplot.df <- transactions.analysis.df %>% 
  select(c(TID, approval.date, discount.date, approval.days.until.quarter.end, invoice.value)) %>% 
  mutate(invoice.value = invoice.value %>% unscale %>% exp) %>%
  cbind(., predict(aft.control.fm, newdata = transactions.analysis.df)) %>%
  dplyr::mutate(predicted.discount.date = approval.date + .pred_time %>% ceiling) %>%
  select(-".pred_time") %>%
  dplyr::mutate(actual.discount.same.quarter = lubridate::quarter(approval.date) == lubridate::quarter(discount.date) & 
                  lubridate::year(approval.date) == lubridate::year(discount.date)) %>%
  dplyr::mutate(predicted.discount.same.quarter = lubridate::quarter(approval.date) == lubridate::quarter(predicted.discount.date) & 
                  lubridate::year(approval.date) == lubridate::year(predicted.discount.date)) %>%
  dplyr::mutate(actual.value.discount.same.quarter = actual.discount.same.quarter * invoice.value,
                predicted.value.discount.same.quarter = predicted.discount.same.quarter * invoice.value) %>%
  mutate(approval.days.until.quarter.end = approval.days.until.quarter.end %>% unscale) %>%
  mutate(approval.days.until.quarter.end.category = case_when(
    approval.days.until.quarter.end >= 0 & approval.days.until.quarter.end <= 30 ~ "0-30",
    approval.days.until.quarter.end >= 31 & approval.days.until.quarter.end <= 60 ~ "31-60",
    approval.days.until.quarter.end >= 61 & approval.days.until.quarter.end <= 91 ~ "61-91",
    TRUE ~ NA_character_
  )) %>%
  group_by(TID) %>%
  slice(1) %>%
  ungroup() %>% 
  group_by(approval.days.until.quarter.end.category) %>% 
  dplyr::summarise(
    actual.count = sum(actual.discount.same.quarter, na.rm = TRUE),
    counter.factual.count = sum(predicted.discount.same.quarter, na.rm = TRUE),
    actual.value = sum(actual.value.discount.same.quarter, na.rm = TRUE) / 1E6,
    counter.factual.value = sum(predicted.value.discount.same.quarter, na.rm = TRUE) / 1E6,
    n = n()
  ) %>%
  dplyr::rename("days.to.EOQ" = approval.days.until.quarter.end.category)



#################
#
# some stats for the paper
#
#################

ggplot.df %>% mutate(diff.abs = actual.count -  counter.factual.count,
                     diff.rel = diff.abs/counter.factual.count)

ggplot.df %>% mutate(diff.abs = actual.value -  counter.factual.value,
                     diff.rel = diff.abs/counter.factual.value) %>%
  dplyr::summarise(total.value.actual =  sum(actual.value),
                   total.value.counter.factual =  sum(counter.factual.value),
                   diff.abs = total.value.actual - total.value.counter.factual,
                   diff.rel = diff.abs / total.value.counter.factual)
#################
#
# (a) count
#
#################

ggplot.long.df <- ggplot.df %>%
  tidyr::pivot_longer(cols = c("actual.count", "counter.factual.count"), names_to = "type", values_to = "value") %>%
  select(c(days.to.EOQ, type, value)) %>%
  dplyr::rename("data" = "type") %>%
  mutate(data = ifelse(data == "counter.factual.count", "counterfactual", "actual")) %>%
  mutate(data = factor(data, levels = c("counterfactual", "actual")))


# Create the plot with error bars and specified y-axis limits using coord_cartesian
(p<-ggplot(ggplot.long.df, aes(x = days.to.EOQ, y = value, fill = data)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    scale_fill_manual(values = c("#DDDDDD", "#3D3D3D")) +
    coord_cartesian(ylim = c(0, 20000)) +  # Restricts the displayed y-axis range
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    ggtitle("") +
    xlab("days invoice approved before quarter end") +
    ylab("number of invoices discounted\nbefore quarter end") +
    theme(plot.title = element_text(size = 26, face = "bold"),
          legend.position = "top",
          text = element_text(size = 36, family = "latex"),
          axis.text.x = element_text(hjust = 0.5, size = 30), 
          axis.text.y = element_text(size = 30)))


# Save the plot as a PDF with the specified dimensions
ggsave("counter.factual.count.pdf" %>% inArtDir, plot = p, width = 30, height = 20, units = "cm")


#################
#
# (b) value
#
#################

ggplot.long.value.df <- ggplot.df %>%
  tidyr::pivot_longer(cols = c("actual.value", "counter.factual.value"), names_to = "type", values_to = "value") %>%
  select(c(days.to.EOQ, type, value)) %>%
  dplyr::rename("data" = "type") %>%
  mutate(data = ifelse(data == "counter.factual.value", "counterfactual value", "actual value")) %>%
  mutate(data = factor(data, levels = c("counterfactual value", "actual value")))


# Create the plot with error bars and specified y-axis limits using coord_cartesian
(p<-ggplot(ggplot.long.value.df, aes(x = days.to.EOQ, y = value, fill = data)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
    scale_fill_manual(values = c("#DDDDDD", "#3D3D3D")) +
    #   coord_cartesian(ylim = c(15000, 20000)) +  # Restricts the displayed y-axis range
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    ggtitle("") +
    xlab("days invoice approved before quarter end") +
    ylab("value of invoices discounted\nbefore quarter end (mn USD)") +
    theme(plot.title = element_text(size = 26, face = "bold"),
          legend.position = "top",
          text = element_text(size = 36, family = "latex"),
          axis.text.x = element_text(hjust = 0.5, size = 30), 
          axis.text.y = element_text(size = 30)))


# Save the plot as a PDF with the specified dimensions
ggsave("counter.factual.value.pdf" %>% inArtDir, plot = p, width = 30, height = 20, units = "cm")

