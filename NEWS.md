# creditmodel1.1.2
**In this version I have:**
* Fixted additional issues for the *last* version released on CRAN: 
   + Error in finalizer(env) :
   + Inconsistent 'best_score' values between the closure state: 0.742427 and the xgb.attr: 0.742427
* Fixed some potential bugs in `xgb_filter`,`feature_select_wrapper`,`split_bins`, `ks_table_plot`,`ks_psi_plot`,`ks_value`.
* Add a new function `pred_score` for predicting new data using trained models.
* Provide new functions `perf_table`,`roc_plot`,`ks_plot`,`lift_plot`,`psi_plot` for model validation drawings.
* Provide new functions `partial_dependence_plot`,`get_partial_dependence_plots` for generating partial dependence plot.
* Provide new functions `cohort_analysis`,`cohort_table`,`cohort_plot`  for cohort analysis and visualization.


# creditmodel1.1.1
**In this version I have:**
* Fixed some potential bugs in `get_names`, `digits_num`

# creditmodel1.1.0

**In this version I have:**

* Add a function `data_exploration`  for data exploration.
* Fixed some potential bugs in `missing_proc`, `outliers_proc` ,`get_names` 
* In `lasso_filter`,  `AUC`&`K-S` is added to select the best lambda. In this way, not only can the set of variables that makes the AUC or K-S maximized be selected, but also the multicollinearity (which is difficult to eliminate by AIC in stepwise regression),  can be minimized. That means instead of stepwise regression, the optimal combination of variables can be selected by lasso to solve the regression problem.
* Visualize the number of variables, `K-S` or `AUC` values corresponding to different lambda.
* Provide new functions``auc_value`  \ `ks_value`,  which can calculate Kolmogorov-Smirnov (K-S)  & AUC of multiple model results quickly.

