SELECT date(started_at) AS Ymd, count(*) AS Count 
FROM cleaned_bike_df_2
WHERE start_station_name IS NOT NULL
GROUP BY Ymd
ORDER by  Ymd ;