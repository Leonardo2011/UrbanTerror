Hertie School of Governance

MPP-E1180: Introduction to Collaborative Social Science Data Analysis

Fall 2014

Instructor: Christopher Gandrud


#URBAN TERROR


Lukas Bretzinger, Cameron Reed, Sascha Schuster
===========


**short run: to do**
 *  reorganize repo: only main scripts in the main folder, everything else goes into subfolders, data is zipped (Scripts: 1 - Data Gathering, Cleaning, Merging.R, 1.a - Global Terrorism Database.R, 1.b - Country Data.R, 1.c - City Data.R, 2. Data Preparation for Analysis, 3. Analysis (bring all regressions in one script) 4. Visuals
 *  The "2. Data Preparation for Analysis" would create two csv files in the Cache folder, one with country means for fixed effects and one with all observations (PreGTD)
 *  replace data.frame with data.table operations if possible to save time an make the script run again on machines with less than 24GB RAM. 
 *  bring some "small scripts" back into the main ones, even if they become larger
- improve readability of the main scripts, rename generic Variables with something telling
 *  replace gsub operations in the GTD City cleaning with a counrty sensitive process to reduce false positives (done with the example of lima

**long run: to do** 
 *  learn stats and how to interpret regression results correctely (not kidding guys)
 *  run much more tests: non-country-relative variables. different regressions, glm, exponential growth etc. Adding one variable after another. test for other indicators than urbanization, what about cell phone / internet penetration. 
 *  Model with treatment on country level: Drones vs. no drones. 
 *  Model with treatment on country level: Coastal Megacity vs. no Coastal Megacity. Gravitation?  
 * single out the effect of various institutions coding and account for it, if possible, to run analysis back til 1970.

 * Identify the best analytical tools and methods to study group behaviour (we want to identify both if there are clear trends, and if certain events have an impact, so some sort of polynomial regression and visualisation would be a potentially usefull first step)


**nice to haves** 
 *  use geonames to find missing values
 *  find exact urban center growth for larger cities to replace our estimates. 
 *  get into spatial analysis, as c.traxler suggested at the thesis cluster meeting 


===========


**1 - Data Gathering, Cleaning, Merging.R**	is the main script for merging data sets. The result is the "preGTD" saved as csv in the Terror Data Folder. It **combines the scipts 1.a, 1.b., 1.c**. 

**1.a - Global Terrorism Database.R** is a script to select variables, calculate some new ones and and clean the cities in the GTD to they later match with our datasets on cities, The GTD is the Global Terrorism Database (GTD) Copyright © 2009-2014 National Consortium for the Study of Terrorism and Responses to Terrorism.

**1.b - Country Data.R**	is a script to bring the GTD country coding to World Bank / UN standards for country coding and erases all historic codings (East Germany, Soviet Union, Yugoslavia...). Here, variables of war are introduced from the correlatesofwar.org project. 

**1.c - City Data.R**	is a long script merging 3 different data sets on world cities into one, testing its belonging to one of 500 of the Worlds Urban centers, and collecting a ton of geographical data on these 100k + places.

**Analysis**  is still scattert.

**Anything else** are temporary scripts and subscripts in "SmallScripts"




note: the only R related projects working with the GTD und using gitHub have been found here
http://rpubs.com/rljohn/stat545a-2013-hw05_johnston-reb
https://github.com/daattali/statsTerrorismProject (http://rpubs.com/daattali/statsTerrorismProject)
