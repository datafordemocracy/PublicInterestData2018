## 2018-02-16
* MC: quick review of each group's code, more exploration of data, combine/revise problem statement for discussion and further revision, set Friday's agenda, read some literature on geographic context cited in the research synthesis.
* MW: This week I edited my R script and responded to the comments. I'm not sure whether the comments were made by you or by other students, but they were very helpful for clarifying and potential errors I made. Things that I might have found clear after looking extensively at the data might not be obvious to an outsider, so in the future I need to provide answers to these questions in my analysis. I also looked at the referral data and did the code review/added comments.
* HS: This week my group updated the code book with the additional information from Jenny. We also reviewed our code and incorporated the comments we received. This included adding more comments, grouping by case id and checking for outliers. One difficult part of the week was figuring out how to look for outliers in the data. We did a couple of things, but aren't sure if that is what to look for. I also created a version of the referral code with comments. 
* CM: This week I got some more census and ACS data by block group, updated my script with a binary indicator of ongoing cases, a duration variable with a dummy data retrieval date (not sure when that was), and explored some new visualization! N
* NP: Since last week in class our group realized that data visualization will be very important,  I am exploring new ways right now. Also, I am doing research about children’s psychology. I want to know whether we should keep exploring the age and gender effects. 
* AW: I was able to go through the explore_referraldata and my group's ongoing_clients data searched up functions that I didn't understand and didn't have a full grasp of. I now have more background on how the data is structured and better understand what should be done.
* MW: This week I looked more into potential tests we can do to analyze the data. There is this spreadsheet-based application called Oracle Crystal Ball that can be used to find the "sensitivity" of various variables and the impact of their potential values on the ultimate output variable. This could be used if later one we want to run a regression analysis on how various factors impact the likelihood of a child with being screened in. There are also other analyses on the probability and sensitivity of various variables on an outcome, called the Monte Carlo and Tornado Analysis. I will be attending a session on using Crystal Ball this afternoon so I will have a better idea of how to use it afterward. 

## 2018-02-09
* MC: updated explore_referraldata.R -- added comments, more cleaning, more exploration; set Friday's agenda, added code review questions


### Group 1 

* AW: My group and I explored the data for the Ongoing Clients, we were able to clean the data for it to be in a clearer format. I was also able to walk through our exploration R file from class and try to understand the steps. 
* HS: We explored the data set On_Going reports and renamed variables to make them clearer. We also factored categorical data points. We set up a meeting with Jenny Jones at DSS to explore our questions further.


### Group 2

* MW: I worked on the R Script for the Foster Care dataset. Since we don't have any questions to investigate yet, I just looked for differences between white and non-white kids in the system.
* NP: I used R to analyze the foster care dataset. I focused on the age variable. Here are some interesting findings:
  * First, generally, children whose age is around five have the highest chance to be put in the Legal Custody.
  * Second, girls and boys who are in the Legal Custody have different age distributions. For children whose age is below 8, boys are more likely to get into the Legal Custody. However, after 8, there are more girls getting into the Legal Custody.
  * Third, children from different races have different age distribution. For instance, African American children are more likely getting into the Legal Custody around age 1,5 and 15. However, white children are more likely to get into the Legal Custody at 16 and 17 years old.
* NP: This week I worked on looking through the Charlottesville open data. I identified 4 data sets that may be useful to us. The census tracts have geographical and demographic information that is relevant to our project. The crime data may be used to assess the risk that a child may face in a geographical area. Finally, the real estate assessment may give us some indication of income. 
* BE: NP uploaded the local, geographical data we thought would be useful from the Charlottesville Open Data Portal. The sets might relate to some of our lab's questions, to the census, or to foster children's exposure to social services or probability of harm. The open data chosen includes US Census Block Group Data, US Census Block Area, Crime Data, and Real Estate Current Assessments. Additionally, I had signed up for the "Help! I need to use the Census" workshop this week but had to miss that. 

### Group 3

* MW: I met with my group and we looked through the initial Social Explorer Tract Level ACS Census data. Specifically, I pulled out data on Charlottesville's 2016 Total Population, Sex, Sex by Age, Race, Poverty Status in for Children
 Under 18, Poverty Status based on Race, Means of Transportation to Work for Workers 16 Years and Over, and Travel Time to Work for Workers 16 years and Over.
* CM: I really enjoyed learning R and exploring the data this week. I’m new to R but I was able to build out a cleaning script, and that was the one my group decided to use so I was proud of that! I hit a number of roadblocks in trying to figure out how to group the placement data in a way it could be merged with the other data because placements are listed separated, but with extensive Googling and reading, I figured it out. 
* BA: We've cleaned up the placements data and created some new variables that we thought might be helpful. But, there are few questions in the that we wanted to run by you in orded to make changes to the data set. Secondly, going to the census workshop was of great help. But, after listening to Jenn talk about how the data is collected, I wouldn't recommend we use ACS data afterall. We would have to look if the 95% confidence intervals of the data aren't overlapping for all the tracts if we want to use them. Or we can't just prove that the data for the tracts are accurate or if there is a significant difference in the the tract data.
