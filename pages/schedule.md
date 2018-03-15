# Resources

We'll be using Garrett Grolemund's and Hadley Wickham's freely available [R for Data Science](http://r4ds.had.co.nz/) as an R resource and [A research synthesis on child welfare disproportionality and disparities](https://www.cssp.org/publications/child-welfare/alliance/Disparities-and-Disproportionality-in-Child-Welfare_An-Analysis-of-the-Research-December-2011.pdf) produced by the Center for the Study of Social Policy and others as a subject resource. Additional materials are either freely available and linked here or provided on our collab page. Most of the assigned reading will occur in the first few weeks.

The UVA Library and other research support partners across grounds offer free workshops and training opportunities that might be of interest to some, especially those new to tools like R. You can find a list of the [Libray offerings here](http://data.library.virginia.edu/training/), and a full list of workshops by [partner organizations here](http://cadre.virginia.edu/service-detail/education) (the workshops in the health sciences library and school of medicine are open to all, but tend to fill up quickly).

# Schedule and Reading

## January 19, 2018
Since we only have 10 weeks together, we'll be diving in on day one with discussions about working collaboriatively and for a client, ethical considerations and data security, and some background on our research question. 

* What does it mean to use data in the public interest? Read the GovLab's [Five principles for applying data science for social good](http://thegovlab.org/five-principles-for-applying-data-science-for-social-good/)
* How will we work collaboratively and with a client? Click through DataKind's Labs Blueprient [Module 4: Six Components of Successful Lab Projects](http://www.datakind.org/blog/introducing-our-new-labs-blueprint)
* What are some ethical considerations in using adminsitrative data? Read Stiles and Boothroyd's [Ethical Use of Administrative Data for Research Purposes](https://www.aisp.upenn.edu/wp-content/uploads/2015/09/0033_12_SP2_Ethical_Admin_Data_001.pdf) (2015), focusing on pages 4-11 and 22-29.
* What are the broad outlines of our/our client's questions and concerns? Scan the Children's Bureau Issue Brief (November 2016) on [Racial Disproportionality and Disparity in Child Welfare](https://www.childwelfare.gov/pubPDFs/racial_disproportionality.pdf)
* How do we address heightened sensititives and local interest on the project? Watch this story aired in Charlottesville's local CBS 19 News on January 5, 2018 - [Data shows minority children disproportionately in foster care](http://www.newsplex.com/content/news/Data-shows-minority-children-disproportionately-in-foster-care-468157653.html). Really, you're going to want to spend 3 minutes watching this!

## January 26, 2018
Our clients will be joining us this day as we delve into the prior research around racial disproportionality and disparity in child welfare systems. We'll start by collectively presenting the key points from the research synthesis. Everyone is responsible for the first section of the research synthesis, and I'll begin by choosing individuals to summarize definitions and frameworks to ensure we're starting with the same understanding. To practice presenting and communicating research, teams of two (or three, if needed) lab members will have seven minutes to brief the class and clients on a section of the report, followed by three minutes for follow up questions. Our clients will give us some additional local context and we'll have a chance to start asking them some questions.

[A research synthesis on child welfare disproportionality and disparities](https://www.cssp.org/publications/child-welfare/alliance/Disparities-and-Disproportionality-in-Child-Welfare_An-Analysis-of-the-Research-December-2011.pdf). Center for the Study of Social Policy, December 2011. pp. 7-70
* Section 0 (everybody): pp. 7-22
* Section 1: pp. 23-28, 43-46
* Section 2: pp. 29-42 
* ~~Section 3: 47-49, 62-63, 64-70~~
* Section 4: 50-61 

For next week, review the data dictionary for the data provided by DSS. Each lab member should email me by 10am Friday (2/2) one to three research questions she or he believes can fruitfully be answered by the DSS data, or this data supplemented by other additional data (e.g., census-based data); in the same email, include additional questions you have about the data (no need to include questions I've already raised on the document). This will replace the weekly update scheduled to begin February 2.

## February 2, 2018
Lab members will have access to the data after our 1/26 meeting and can start exploring the data right away. In the first part of class, we'll go over the submitted questions, consider possibilities for supplementing the DSS data, and pin down a series of research questions we will pursue. We'll parcel out additional tasks to individual or teams of lab members as needed -- e.g., ensuring the data lends itself to a proposed question, verifying the availability of supplemental data, creating a schema for capturing and presenting the work. We'll spend the second half of class going over some concepts of working in R and working with (potentially different aspects of) the data in groups.
* [R4DS Chapters 1,2,4,5,6,8,15,18](http://r4ds.had.co.nz/introduction.html)

Each group is cleaning, exploring one of data sets for next week, and purusing one supplemental task: looking into useful census data and acquiring, looking into the Cville Open Data Portal and acquiring data if useful, seeking to clarify remaining questions about the data with DSS.

## February 9, 2018
After reviewing last week's work, we'll draft a formal problem statement based on last week's discussion and initial investigations into the data and availability of supplementary data. We'll devise a more formal research plan, splitting up into teams to tackle different pieces of the research plan. We'll spend some time going over some data wrangling concepts.
* [R4DS Chapters 3,7,9,10,11,12,13,14,16](http://r4ds.had.co.nz/wrangle-intro.html)

Also, there's a workshop on [using Census data Wednesday, February 7 from 2-3](http://cal.lib.virginia.edu/event/3793601) in the Brown Science & Engineering Library!

**Today's Agenda**

Time (ish) | Task/Goal
--- | ---
12:00 | [Code review](codereview.html), each group will review another group's initial code and provide suggestions for improvement
12:20 | Report progress on supplemental tasks (no more than 10 minutes each) - exploring/recommending census data, exploring/recommending cville open data portal data, and clarifying questions about the data.
12:45 | Problem statement -- in groups, draft initial problem statement following the [DataKind template from week 1](https://drive.google.com/file/d/0Bxn_Q60v2F7wM01GcU9rbXVXRjQ/view), slides 16-29
1:00 | Reporting/refining a common problem statement
1:15 | Wrangling questions in R -- what do we need to know? E.g., aggregating, merging/joining, graphing, dates/times? 
1:40 | Groups review initial review of code and implement improvements.

## February 16, 2018
We'll definitely spend time time at the beginning of class sharing progress and providing feedback, and demonstrating key data wrangling concepts. And we'll finalize the problem statement and draft some analysis plans.
  * [R4DS Chapters 22-28](http://r4ds.had.co.nz/model-intro.html)

Also, there's a potentially relevant research talk today for those interested: [Beyond Reading, Writing and Arithmetic: The Role of Teachers and Schools in Reporting Child Maltreatment](https://curry.virginia.edu/faculty-research/join-us/curry-education-research-lectureship-series), Maria Fitzpatrick, Associate Professor, Cornell University, 11-12:30, Holloway Hall (Rm 116), Bavaro Hall

**Today's Agenda**

Time (ish) | Task/Goal
--- | ---
12:00 | Collective code review, each group will have 10 minutes to walk us through their code to read in, examine, format, and clean up a data set. We'll ask questions and make suggestions, and use examples from each other's code to demonstrate more general functions and processes. We'll also finalize some choices on how we'll incorporate race and ethnicity consistently.
12:40 | [Problem Statement](problemstatement.html), we'll refine a merged problem statement based on those we began last week, and use this to start outlining a sequence of analysis for each question.
1:20 | Start analyzing, we'll break up into teams to tackle different pieces/questions and each group will outline a research and analysis strategy for their part of the analysis.

## February 23, 2018 and beyond
We'll spend the first half updating one another on our progress, and talking about modeling choices and strategies. And we'll talk about collaborative writing in [ShareLaTeX!](https://www.sharelatex.com/).

**Today's Agenda**

Time (ish) | Task/Goal
--- | ---
12:00 | Sharing of and feedback on group progress reports (one for each enumerated portion of the [problem statement](/resources/problemstatement.md)), including implications for how "race" is coded in analysis.
1:00 | Review of [regression models](http://r4ds.had.co.nz/model-basics.html) and the [ShareLaTeX!](https://www.sharelatex.com/) document.
1:30 | Analysis work. Each group will ultimately generate (1) an R script that reads in, formats and wrangles, and cleans the data, starting with the code of our previous groups, but adapting as needed, and saved as `prep_yourchoice.R` and (2) an R sript that implements the analysis used in the report and saved as `analysis_yourchoice.R`. You may also want a script for exploration of the data that won't necessarily become part of the analysis, e.g., `exploratory_yourchoice.R`. 

## March 2, 2018
Updates on progress, sharing the article review load, and working in groups. I want to be sure to reserve at least 20 minutes to talk with each group in more detail while the other group digs in on analysis, articles, adding to the document, etc.

**Today's Agenda**

Time (ish) | Task/Goal
--- | ---
12:00 | Sharing of and feedback on group progress (one for each enumerated portion of the [problem statement](/resources/problemstatement.md)).
1:00 | Divvy up the articles
1:10 | Confab with group 1 while group 2 works
1:30 | Confab with group 2 while group 1 works
1:50 | More on annotation of article (it's unlikely we'll have any time remaining, but hope springs eternal!)


## March 16, 2018
Updates on progress and working in groups, including time to talk with each group separately.
