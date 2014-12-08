Hertie School of Governance
MPP-E1180: Introduction to Collaborative Social Science Data Analysis
Fall 2014
Instructor: Christopher Gandrud


#URBAN TERROR
Lukas B Cameron R Sascha S##########
===========

Lukas Bretzinger, Cameron Reed, Sascha Schuster



**1 - Data Gathering, Cleaning, Merging.R**	is the main script for merging data sets. The result is the "preGTD" saved as csv in the Terror Data Folder. It **combines the scipts 1.a, 1.b., 1.c**. 

**1.a - Global Terrorism Database.R ** is a script to select variables, calculate some new ones and and clean the cities in the GTD to they later match with our datasets on cities, The GTD is the Global Terrorism Database (GTD) Copyright © 2009-2014 National Consortium for the Study of Terrorism and Responses to Terrorism.

**1.b - Country Data.R**	is a script to bring the GTD country coding to World Bank / UN standards for country coding and erases all historic codings (East Germany, Soviet Union, Yugoslavia...). Here, variables of war are introduced from the correlatesofwar.org project. 

**1.c - City Data.R**	is a long script merging 3 different data sets on world cities into one, testing its belonging to one of 500 of the Worlds Urban centers, and collecting a ton of geographical data on these 100k + places.

**Analysis**  is still scattert.

**Anything else** are temporary scripts and subscripts in "SmallScripts"




note: the only R related projects working with the GTD have been found here
http://rpubs.com/rljohn/stat545a-2013-hw05_johnston-reb
https://github.com/daattali/statsTerrorismProject (http://rpubs.com/daattali/statsTerrorismProject)
