# Information about CogPsych RuggedLearning data

The raw RuggedLearning data has been formatted and anonymised using the `prepare_data.R` script, which generates `cogpsych_data_anon.Rdata`.

This file contains three data frames:

- **data** : trial-by-trial information from study sessions using RuggedLearning (about 225,000 observations from 3800 learning sessions), along with reconstructed model parameters

- **exam.item** : each student's exam performance on items that were part of the study items, along with information about how these items were studied before the exam

- **grades** : each student's final grade for the course

Note: this data set only includes anonymised data from students who gave informed consent for their data to be used for research purposes.