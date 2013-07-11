
#library(devtools)
#setwd("/home/tgr/riv")
#load_all()

#dev_help("iv.str")
#dev_help("iv.str.mult")
#dev_help("iv.str.replace.woe")

# outiv <- iv.str.mult(german_data,"gbbin")
# x <- iv.str.replace.woe(german_data,outiv)
# str(x)
# 
# model <- glm(data=x,formula=gbbin~ca_status_woe+credit_history_woe+purpose_woe+savings_woe+present_employment_since_woe+
#                installment_rate_income_woe+other_debtors_woe+property_woe+other_installment_woe+housing_woe+
#                job_woe+telephone_woe+foreign_worker_woe+mob+credit_amount,family="binomial")
# summary(model)
# #plot(model)
# 
# out <- x
# out["prediction"] <-  predict(model,type="resp")
# str(out)
# ggplot(data=out) + geom_histogram(aes(x=prediction),binwidth=.01) + facet_grid(~gbbin)
# ggplot(data=out) + geom_density(aes(x=prediction),binwidth=.01) + facet_grid(~gbbin)
# ggplot(out, aes(x=prediction, fill=as.factor(gbbin))) + geom_density(alpha=.3)
