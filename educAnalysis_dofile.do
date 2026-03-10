/*
Author: Binya Benson Abe Boro
Dataset: School_spending.dta
						
Research Question: Does public spending on education improve pupil's performance?
*/

clear all
* This clears the workspace

cd "/Users/benson/Desktop/econ_analysis/education/educ_finance_analysis"
* This sets the working directory

log using "results/educ_finance", text replace
* creates a logfile and saves it in the results subfolder

use "data/school_spending", clear
* loads the dataset: "school_spending.dta"


******Inspecting and cleaning the dataset******

describe // check for general overview of the dataset
* The dataset has 420 observations and 11 variables. Some variable labels are in German.

mdesc // check for missing data
*no missing observations in the datase


***** changing the variable labels to English****

label variable dist_cod "District-ID"
label variable read_scr "Average test scores reading"
label variable math_scr "Average test scores math"
label variable testscr "Average test scores general (read_scr+math_scr)/2)"
label variable meal_pct "Share of pupils with food vouchers (in %)"
label variable comp_stu "Number of computers per pupil"
label variable expn_stu "Expenditures per pupil"
label variable avginc "Average income (in 1000 US$)"
label variable el_pct "Share of non native English speakers (in %)"
label variable str "Student Teacher Ration (in %)"
label variable computer "Number of computers"


save "data/school_spending_clean", replace // saves the cleaned data in data subfolder



******Descriptive statistics *******

ssc install estout // install estout

estpost summarize testscr expn_stu meal_pct comp_stu avginc el_pct
esttab using results/Table1.rtf, ///
cells("mean sd min max count") ///
title("Table 1: Summary Statistics") ///
label replace
* This produces summary statistics and saves the output as Table1 in the results subfolder.

*** graphical analysis of key variables: testscr (average test scores) and expn_stu (expenditure per pupil)****

*** creating a histogram of the distribution of average test scores***
histogram testscr, frequency normal ///
 title ("Histogram of the Distribution of Pupil's Average Test Scores") ///
 xtitle ("Average Test Scores") ///
 ytitle ("Frequency") ///
 start(605) ///
 width (5) ///
 graphregion(color(white)) ///
 name (FigureI, replace)
 graph export "results/FigureI.png", replace
 * Saves the histogram as Figure1 in the results subfolder

*** creating a histogram of the distribution of expenditure per pupil***
histogram expn_stu, frequency normal ///
 start(3926) ///
 title ("Histogram of the Distribution of School Expenditure Per Pupil") ///
 xtitle ("Expenditure Per Pupil") ///
 ytitle ("Frequency") ///
 graphregion(color(white)) ///
 name (FigureII, replace)
 graph export "results/FigureII.png", replace
 * saves the histogram as FigureII in the results subfolder.
 
 
 *** Analyzing the relationship between average test scores and expenditure per pupils***

correlate testscr expn_stu // obtain correlation coefficient
 local corr = r(rho) // extracts the correlation coefficient to later include in the scatter plot
 
 *** creating a twoway scatter plot***
 twoway ///
 (scatter testscr expn_stu, ///
 mcolor(navy) ///
 msize(small) ///
 msymbol(circle)) ///
 (lfit testscr expn_stu, ///
 lcolor(red) ///
 lwidth(medium)), ///
 title("Scatter Plot of Pupil's Test Scores and Expenditure Per Pupil") ///
 subtitle("Correlation = `: display %4.3f `corr''") ///
 xtitle("Expenditure Per Pupil") ///
 ytitle("Average Test Score", margin(medium)) ///
 legend(order(1 "Observed Values" 2 "Fitted Line")) ///
 graphregion(color(white)) ///
 name(FigureIII, replace)
 
 graph export "results/FigureIII.png", replace // exports the scatter plot as FigureIII.
 
 
 
 *** OLS Estimation of the effect of public spending on pupil's performance****
/* 
Baseline modal:

dependent variable: Average Test Scores, measured by testscr
Independent variable: Expenditure per pupils, measured by expn_stu

Covariates: 
 - share of pupils with food vouchers, meal_pct
 - number of computers per pupil, comp_stu
 - share of non-native English speakers, el_pct
*/

*** 
generate log_expn = log(expn_stu) // log transforming expenditure per pupil (expn_stu)

label variable log_expn "log (expenditure per pupil)" // labeling log_expn

*** regression analysis

eststo modelI: regress testscr log_expn // bivariate regression analysis
* This also stores the estimates as modelI. The coefficient is positive, conforming to that of the scatter plot of the two variables.

eststo modelII: regress testscr log_expn el_pct comp_stu avginc meal_pct // multivariate analysis, including all covariates.

***creating a publication-ready regression table consisting of model I and II above***

esttab modelI modelII using "results/baseline_reg.rtf", replace ///
b(3) se(3) ///
compress ///
nogaps ///
varwidth(20) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(F p r2 r2_a N, fmt(3 3 3 3 0) ///
labels("F_Statistic" "Prop > F" "R_Squared" "Adjusted R_squared" "Observations")) ///
mtitles("Bivariate Regression" "Multivariate Regression") ///
title("Table II: OLS Regression of Average Test Scores on Expenditure Per Pupil") ///
label


***Comparing different model specifications****

/*
Model I is the baseline multivariate model
Model II is a semi-log model including log transformed average income
*/

eststo clear // clears previouly stored model estimates
 
histogram avginc // histogram of average income.
* The distribution is not symmetric.

 generate log_avginc = log(avginc) // log transform average income. 
* generates a log transformation of average income
label variable log_avginc "log (average income)" // label log_avginc


eststo modelI: regress testscr log_expn el_pct comp_stu avginc meal_pct // estimating baseline model 1.

eststo modelII: regress testscr log_expn log_avginc el_pct comp_stu meal_pct // estimating semi-log model II, with log transformed average income

*** comparing model I and II, using their AIC, BIC, and adjusted R-squared.

esttab modelI modelII using "results/model_testing_reg.rtf", replace ///
b(3) se(3) ///
compress ///
nogaps ///
varwidth(20) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(r2_a aic bic N, fmt(3 3 3 0) ///
labels("Adjusted R_squared" "AIC" "BIC" "Observations")) ///
mtitles("Baseline Model" "log-linear Model") ///
title("Table III: Testing and Comparing Different Model Specifications") ///
label
**The baseline model has higher adjusted R_squared, lower AIC and BIC as compared to Model II.


***Testing and Correcting for Heteroskedasticity

eststo clear // clears stored model estimates

eststo m1: regress testscr log_expn el_pct comp_stu avginc meal_pct // this is the baseline model without robust standard errors

estat imtest, white // white test for heteroskedasticity in the baseline model
** Fail to reject homoskedasticity (Prob > chi2 = 0.7335 > 0.005). 

return list // checking which scalar contains the p-value of the white test.

scalar white_p = r(p) // storing the white test's p-value to later extract it and include in the regressiion table below.

est restore m1 // re-activates baseline model m1, to make inclusion of the white test p-value possibl
estadd scalar white_p = white_p // adds the white test p-value into the baseline model, m1


***estimating the baseline model with robust standard errors

eststo m2: regress testscr log_expn el_pct comp_stu avginc meal_pct, robust
est restore m2
estadd scalar white_p = white_p // adds the white test p-value into the baseline model with robust standard errors, m2


*** creating a publication-ready regression table of the baseline model with and without robust standard errors, and including the white test p-value in it.

esttab m1 m2 using "results/robust.rtf", replace ///
nogaps ///
se star(* 0.10 ** 0.05 *** 0.01) ///
stats(N r2 white_p, fmt(0 3 3) ///
labels("Observations" "R_Squared" "White Test p_value")) ///
mtitles("OLS" "OLS with Robust SE") ///
title("Table IV: OLS Regression with Robust Standard Errors") ///
label


*** graphical detection of heteroskedasticity in the baseline model

regress testscr log_expn el_pct comp_stu avginc meal_pct

predict resid_hat, resid // predict residuals

predict y_hat, xb // predict fitted values

*** creating a scatter plot of residuals and fitted values***
twoway ///
(scatter resid_hat y_hat, ///
mcolor(navy) msize(small)) ///
(lowess resid_hat y_hat, ///
lcolor(red) lwidth(medium)), ///
yline(0, lpattern(dash) lcolor(black)) ///
title("Scatter Plot of Residuals and Fitted Values") ///
xtitle("Fitted Values") ///
ytitle("Residuals") ///
legend(order(1 "Residuals" 2 "Lowess smooth")) ///
graphregion(color(white)) ///
name(FigureIV, replace)
* exporting the scatter plot as MS Word
 graph export "results/FigureIV.png", replace

 
*** checking functional form specification using the RESET test
* null hypothesis, Ho: the baseline model has no omitted variables/ is correctly specified

regress testscr log_expn el_pct comp_stu avginc meal_pct

estat ovtest // REESET test for model specification
* Fail to reject the null hypothesis (Prob > F = 0.7762 > 0.05). The baseline model has no omitteed variables.

log close
