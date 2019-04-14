# BKpctcrime
An analysis of crime complaints in Prospect Park and Fort Greene Park compared to surrounding precincts in Brooklyn

How Safe Are Our City Parks Today?
-----------------------------------
If we want to understand which parks in New York City have the fewest crime complaints, and thereby identify which park is (perhaps) safest, we can tabulate the park stats and come up with a hypothetical winner on the safety scale. But is it safer in the park than in its surrounding precinct(s)? Can we measure whether a Brooklyn neighborhood park is safer or less safe than the neighborhood where it is situated? Is there a difference? The environments certainly feel very different, but what does the data say?

Using NYC Open Data for Police Complaints we may analyze crime trends for Prospect Park and Fort Greene Park in Brooklyn alongside crime trends for the precincts directly surrounding these two neighborhood parks. Police Complaint Data is open to the public for all reported incidents from 2006 to the present. Beginning in 2014, the NYPD began recording parks complaints as a separate variable in their complaint data. This parks column includes large parks, playgrounds, and all green spaces in the city. The parks are still recorded as part of their dedicated precincts but can be separated for analysis by park name. 

The Project
------------
Time Frame: 2014-2018
Location: Brooklyn, NY
Goal: Calculate crime complaint rates in Prospect Park and Fort Greene Park, as well as their adjacent Brooklyn precincts:
70, 71, 72, 77, 78, 88

Using square area and 2010 census block data, create population density measures for each precinct. The parks have estimated visitor populations of 10,000,000 (Prospect Park) and 1,000,000 (Fort Greene Park). Dividing these totals by 365, we will have an estimate of daily population to use in calculations.

Using R:
---------
Filter crime data (NYPD Complaint Data (Historic)merged with NYPD Complaint Data (Current)) for years 2014-2018, filter by the precincts in question, and select only the relevant variables for this research question: Date, Time, Precinct Number, Crime Level, Description, and Park Name.

Gather block data from 2010 Census (American FactFinder). Using CSV key of block:precinct key-values, sum the total population per precinct.

QGIS may be used with precinct shapefiles to calculate square area for the precincts. Acres are the measurement used in this study.

Create scatterplots for each precinct and park showing all crimes reported from 2014-2018, separated by crime level (Felony, Misdemeanor, Violation)

Create sums of each crime level for each year: this potentially shows a trend of more crimes over time or fewer crimes over time for each area

Create crime rate for each precinct, based on population. Number of crime complaints per 1000 people, over time.

Using month and time variables, we may also create risk profiles for each park: which months have historically had the fewest/most complaints, and which hours of the day have historically had the fewest/most complaints.

Show the top 5 crime complaints by description for each precinct and park. What crimes are actually being reported?


