Note: 
This dataset includes all the monitoring period, however, in the paper we seperate the clear sky days and cloudy days
Clear Sky Days:"2022-08-24","2022-08-29","2022-09-02","2022-09-03","2022-09-07","2022-09-08",
                "2022-09-09","2022-09-10","2022-09-11","2022-09-15","2022-09-24","2022-09-30","2022-10-03"
Cloudy Days: "2022-08-26","2022-08-27","2022-09-20","2022-09-21","2022-09-22","2022-09-25",
              "2022-09-26","2022-09-28", "2022-10-01","2022-10-06"
For the mixed effect model, we use 10am-16pm to represent the daytime, 10pm-4am to represent the nighttime


Variables:
YMDH - Year-Month-Day-Hour	
Temperature - Absolute Air Temperature	
Date.Time - POSIXct	
Humidity - Relative Humidity	
Location - Sample Location  1: Open Grass Area  2: Discrete Tree Cluster  3: Aggregate Tree Cluster  4: Reference Site	
ID - Park ID	
Name - 	The sensor's ID: park+location
hour - 	Hour of the day
Tw -Wet-bulb Temperature	
T_Dif -	Air Temperature relative to the reference site
H_Dif -	Relative Humidity relative to the reference site
Tw_Dif -Wet-bulb Temperature relative to the reference site

