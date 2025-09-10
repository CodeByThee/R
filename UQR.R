library(readr) 
library(dplyr)
library(ggplot2) 
library(Hmisc) 
library(corrplot) 
library(car) 
library(lmtest) 
library(performance) 
library(broom)
library(corrplot)


#Loading the Dataset
unidata<-read.csv("/Users/theekshanamadushanka/Desktop/Advanced Business Intelligence/ABI-CIS6008-MAR-2025-Dataset/Question-(a)/UQR.csv")


#Printing the Dataset
print(unidata)

#Checking for null Values
is.null(unidata)

#Getting information about the dataset structure
str(unidata)

#Getting a Overview of the Dataset
glimpse(unidata)

#Summary of the dataset
summary(unidata)


#Selecting numeric variables
num_vars <- unidata |> dplyr::select(where(is.numeric)) |> dplyr::select(-`University.Ranking.Score`)

#Running Shapiro–Wilk tests
sw <- lapply(num_vars, function(x) shapiro.test(sample(x, min(5000, length(x)))))

#Collecting results
data.frame(
  variable = names(sw),
  W = sapply(sw, \(s) unname(s$statistic)),
  p_value = sapply(sw, \(s) unname(s$p.value))
) |> arrange(p_value)

#Q–Q plots
for (v in names(num_vars)) {
  print(ggplot(unidata, aes(sample = .data[[v]])) + stat_qq() + stat_qq_line() +
          labs(title = paste("Q–Q Plot:", v)))
}

#Extracting the Target Variable
target <- unidata$`University.Ranking.Score`

#Correlation Coefficients (Pearson and Spearman)
pearson_vals  <- sapply(num_vars, \(x) cor(x, target, use="complete.obs", method="pearson"))
spearman_vals <- sapply(num_vars, \(x) cor(x, target, use="complete.obs", method="spearman"))

#Tabulation of Results
corr_tbl <- data.frame(variable = names(num_vars),
                       pearson = pearson_vals,
                       spearman = spearman_vals) |>
  arrange(desc(abs(pearson)))
corr_tbl

#Correlation Heatmap Across All Numeric Variables
num_all <- unidata |> dplyr::select(where(is.numeric))
cm <- Hmisc::rcorr(as.matrix(num_all))
corrplot(cm$r, type="upper", tl.cex=.7, addCoef.col="black", number.cex=.5)


#Selection of the Top Predictors
top_vars <- head(corr_tbl$variable, 4)

#Scatterplots with Regression Lines
for (v in top_vars) {
  print(ggplot(unidata, aes_string(x=v, y="`University.Ranking.Score`")) +
          geom_point(alpha=.5) + geom_smooth(method="lm", se=TRUE) +
          labs(title=paste("Ranking Score vs", v)))
}


#Simple Linear Regression (SLR) with the Best Predictor
best_pred <- top_vars[1]
slr <- lm(as.formula(paste("`University.Ranking.Score` ~ `", best_pred, "`", sep="")), data=unidata)
summary(slr)
par(mfrow=c(2,2)); plot(slr); par(mfrow=c(1,1))
bptest(slr)
car::vif(slr)
performance::check_model(slr)

#Multiple Linear Regression (MLR) with Policy-Relevant Predictors
mlr <- lm(`University.Ranking.Score` ~ `Faculty.Salary..Avg..` +
            `Research.Funding..Million.USD.` + `Graduation.Rate....` +
            `Student.Faculty.Ratio` + `Employment.Rate....` +
            `Tuition.Fees..USD.` + `Student.Enrollment`, data=unidata)
summary(mlr)
car::vif(mlr); bptest(mlr); performance::check_model(mlr)

#Stepwise Model Refinement (AIC-based Selection)
full <- mlr; null <- lm(`University Ranking Score` ~ 1, data=uqr)
step_model <- step(null, scope=formula(full), direction="both", trace=0)
summary(step_model)
broom::tidy(step_model, conf.int=TRUE); broom::glance(step_model)





