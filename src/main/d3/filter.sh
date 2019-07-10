cat simple_views_all.csv |  cut -d ',' -f1,3,8,9,10 | sort -k2,2 -t ',' > simple_3r_70.csv
