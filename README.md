# SSMU-Data-Challenge-I

Welcome to Data Challenge I! You and your teammates will have from 11 am to 5:30 pm on Saturday, October 17th, to explore the data and submit a presentation on your analysis.

This Git repository contains this `README` file and the dataset in a `CSV` format. If you are a Duke student, we recommend you use Duke's Container Manager to access your preferred IDE.

## How to Participate

1. Fork the repository emailed to you by susan.li@duke.edu
2. Load the data from `motor_vehicle_collisions.zip`. Click the file, click “View Raw,” and download it.
3. Come up with a question to answer with data analysis. Analyze the dataset and prepare a presentation of your findings.
4. Submit your files to your team’s Google Drive folder. Submission materials should include: your code notebook file, presentation media, and citations to all outside resources.
5. Present! **You must fit your presentation within a 5-minute timeframe**. Please do not push changes to this repository; it is read-only for participants.
The analysis period will run from ~11:30 am to 4:30 pm. You and your team may stay in Physics 150 or leave during the analysis period (to get food, analyze in another room, etc.). You and your team must return to Physics 150 by 4:30 pm sharp to submit your presentation materials.

## Data Dictionary

**About**

You will be working with NYC motor vehicle collision data. The Motor Vehicle Collisions data tables contain information from all police-reported motor vehicle collisions in NYC. Each row represents a crash event. The police report (MV104-AN) is required to be filled out for collisions where someone is injured or killed, or where there is at least $1000 worth of damage. The dataset includes 2,269,187 records across 29 columns.

Data Source: City of New York

Last Updated: July 31, 2026

Link: https://catalog.data.gov/dataset/motor-vehicle-collisions-crashes
We provided a zip file with the CSV for your team's convenience, but you can also visit the website to download the other available files.

**Data Variables**

| Column | Data type | Description |
| --- | --- | --- |
| CRASH DATE | Date/time | Date the crash occurred. |
| CRASH TIME | Text | Time the crash occurred. |
| BOROUGH | Text | NYC borough where the crash occurred. |
| ZIP CODE | Text | ZIP code where the crash occurred. |
| LATITUDE | Number | Latitude in decimal degrees, using WGS 1984 (EPSG:4326). |
| LONGITUDE | Number | Longitude in decimal degrees, using WGS 1984 (EPSG:4326). |
| LOCATION | Point | Combined latitude and longitude. |
| ON STREET NAME | Text | Street where the crash occurred. |
| CROSS STREET NAME | Text | Nearest cross street to the crash. |
| OFF STREET NAME | Text | Street address, if known. |
| NUMBER OF PERSONS INJURED | Number | Total number of people injured. |
| NUMBER OF PERSONS KILLED | Number | Total number of people killed. |
| NUMBER OF PEDESTRIANS INJURED | Number | Number of pedestrians injured. |
| NUMBER OF PEDESTRIANS KILLED | Number | Number of pedestrians killed. |
| NUMBER OF CYCLIST INJURED | Number | Number of cyclists injured. |
| NUMBER OF CYCLIST KILLED | Number | Number of cyclists killed. |
| NUMBER OF MOTORIST INJURED | Number | Number of vehicle occupants injured. |
| NUMBER OF MOTORIST KILLED | Number | Number of vehicle occupants killed. |
| CONTRIBUTING FACTOR VEHICLE 1 | Text | Reported contributing factor for vehicle 1. |
| CONTRIBUTING FACTOR VEHICLE 2 | Text | Reported contributing factor for vehicle 2. |
| CONTRIBUTING FACTOR VEHICLE 3 | Text | Reported contributing factor for vehicle 3. |
| CONTRIBUTING FACTOR VEHICLE 4 | Text | Reported contributing factor for vehicle 4. |
| CONTRIBUTING FACTOR VEHICLE 5 | Text | Reported contributing factor for vehicle 5. |
| COLLISION_ID | Number | Unique crash identifier; primary key of the crash table. |
| VEHICLE TYPE CODE 1 | Text | Type of vehicle 1. |
| VEHICLE TYPE CODE 2 | Text | Type of vehicle 2. |
| VEHICLE TYPE CODE 3 | Text | Type of vehicle 3. |
| VEHICLE TYPE CODE 4 | Text | Type of vehicle 4. |
| VEHICLE TYPE CODE 5 | Text | Type of vehicle 5. |
