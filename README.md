# regression_competition

Repo for submission to STAT 301-3 Regression Competition

# file organization

Folders `set_1`, `set_2`, and `set_3` contain model setup files and tuning scripts for three sets of recipes for the kaggle competition. Each folder also contains a `kaggle_submission` folder that contains the predictions that were submitted to the kaggle competition. The random forest models from sets 1 and 3 were the final submissions for the kaggle competition. These scripts are collated in the `final_models` folder.

# final models

The final models for the Kaggle competition can be found under the `final_models` folder. To run these scripts, please follow these instructions:

-   To run each model, open the respective model setup file (`final_model_1` or `final_model_3`).

-   Run these scripts upto and including the "save necessary objects for tuning" step (approximately at line 68-72).

-   Next, move to the random forest tuning scripts for each model (`rf_tuning_1` or `rf_tuning_3` respectively).

-   Run these RF tuning scripts as individual jobs. When the jobs have successfully completed, move to the model setup file one last time.

-   Run the remaining code in the `final_model_1` or `final_model_3` scripts, starting at approximately line 72.

-   The predictions should be saved in the `kaggle_submission` folder that is inside the `final_models` folder.
