SELECT strftime('%H', started_at) AS Hour, count(*) AS Count 
FROM cleaned_bike_df_2
WHERE start_station_name IS NOT NULL
GROUP BY Hour
ORDER by Hour ;