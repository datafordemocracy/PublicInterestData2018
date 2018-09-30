# Public Interest Data Lab (DS 5559)

The Public Interest Data Lab is intended to provide data science experience to students in service of the public interest. We have three goals:

* Make progress on a project that advances the public interest or social good, in service of a client's goals. And have a project to point to at the end, an example of your work.
* Practice working with real data to answer real questions. This includes finding data, exploring and cleaning data, analyzing and modeling data, and visualizing and communicating data.
* Develop experience working on a data science team, including processes for working collaboratively, openly, inclusively, and reproducibly. 

## Spring 2018 Project

Our client for this spring is the City of Charlottesville's Department of Social Services who wish to understand whether there is racial disproportionality or racially disparate outcomes in local child welfare services, and if so, to what extent and at what stages. There are a number of ways we might investigate this question, depending on the nature of the data the DSS records as well as other data we might use to supplement, merge with, or contextualize the data. 

# Organizational Structure

## Repo Layout

  + [code](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code) contains all of the exploratory data analysis, data cleaning scripts, and final data analysis and visualizations generated for the report.
    + [code/dataanalysis](https://github.com/datafordemocracy/PublicInterestData2018/tree/master/code/dataanalysis) contains all of the analysis and visualizations produced for the report.
    + [code/dataprep](https://github.com/datafordemocracy/PublicInterestData2018/tree/master/code/dataprep) contains finalized cleaning scripts that create useful data object for the analysis scripts.
    + [code/exploratory](https://github.com/datafordemocracy/PublicInterestData2018/tree/master/code/exploratory) contains exploratory data cleaning and data analysis scripts created by lab members in the early weeks of the class. 
  + [images](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/images) contains git fetch demos IS THIS IMPORTANT?
  + [pages](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/pages) contains web pages for the project site, including our data confidentiality agreement, code review guidelines, course expectations, a collective problem statement, and the course schedule.
  + [resources](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/resources) contains materials from class discussions, including our research questions, analysis plan, literature & code review guidelines, a draft data confidentiality agreement, commentary on links in the data, draft problem statements, a discussion of the `race` variable, and individual weekly updates. 

## Data & Scripts

  + **Referral data**: Clients referred to Charlottesville DSS from July 1, 2014 to June 30, 2017 (n=2706). Includes date of referral; age, race, ethnicity, gender, and census tract of referred children; nature of alleged abuse or neglect; relation of individual making the referral to the referred child; whether the case was accepted, the response priority, whether the case was assessed or investigated, the finding of the investigation and finding date. Data in the `referral` dataframe.
    + [Cleaning script](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataprep/prep_postreferral.R) in `prep_postreferral.R`.
    + [Analysis](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/analysis_postreferral.R) in `analysis_postreferral.R`.
    + [Figures and visualizations of disproportionality](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/figures_postreferral.R) in `figures_postreferral.R`.
    + [Figures and visualizations of outcome predictions](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/figure_predictions.R) in `figures_predictions.R`.
    
  + **Ongoing client data**: Active clients under CPS care between July 1, 2014 and June 30, 2017 (n=375). Includes race, ethnicity, age, and gender of child; date the child’s involvement with CPS began and child’s age at that time; and the number of face-to-face interactions between DSS and the child. Data in the `active` dataframe.
    + [Analysis](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/analysis_postreferral.R) in `analysis_postreferral.R`.
    + [Cleaning script](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataprep/prep_postreferral.R) in `prep_postreferral.R`.
    + [Figures and visualizations of disproportionality](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/figures_postreferral.R) in `figures_postreferral.R`.
    + [Figures and visualizations of outcome predictions](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/figure_predictions.R) in `figures_predictions.R`.
    
  + **Foster care data**: Foster care clients entering care between July 1, 2014 to June 30, 2017 (n=118). Includes race, age, and gender of child; age child entered custody, date child entered custody, date child existed custody, and reason for exit; child’s current/final placement type and the number of face-to-face interactions between DSS and the child. Data in the `fc` data frame.
    + [Cleaning script](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataprep/prep_fostercare.R) in `prep_fostercare.R`.
    + [Analysis](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/analysis_fostercare.R) in `analysis_fostercare.R`.
    + [Figures and visualizations of disproportionality](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/figures_fostercare.R) in `figures_fostercare.R`.
    + [Figures and visualizations of outcome predictions](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/figure_fostercarepredictions.R) in `figures_fostercarepredictions.R`.
    
  + **Foster care placement history data**: Placement history of foster care clients entering care from July 1, 2014 to June 30, 2017. Includes date of entry for each new placement, type of placement, date of exist for each placement, and reason for exit. Data in the `fcph` data frame.
    + [Cleaning script](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataprep/prep_fostercare.R) in `prep_fostercare.R`.
    + [Analysis](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/analysis_fostercare.R) in `analysis_fostercare.R`.
    
  + **Charlottesville demographic data** from the 2012-2016 5-year Sex by Age ACS estimates for Cville, ADD MORE HERE
    + [Data fetching and cleaning script](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataprep/prep_basedisp.R) in `prep_basedisp.R`.
    + [Analysis](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/analysis_basedisp.R) in `analysis_basedisp`.



