#For 15 minute Data:
Select * from mt123electricity;

#For Hourly Data:
SELECT Date(RecordDateTime) as date_actual, IFNULL(LAG(EXTRACT(Hour From RecordDateTime), 1) OVER (ORDER BY RecordDateTime), 0) as final_lag_hour, mt123electricity.Value as val 
FROM `MT123electricity`;

#For Daily Data:
SELECT Date(RecordDateTime) as date_actual, IFNULL(LAG((Date(RecordDateTime)), 1) OVER (ORDER BY RecordDateTime), 1) as final_lag_day, (mt123electricity.Value) as val 
FROM `MT123electricity`;
