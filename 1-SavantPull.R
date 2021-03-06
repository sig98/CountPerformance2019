#This script to pull Baseball Savant data was written by Robert Frey, his video on how to do this is here: https://www.youtube.com/watch?v=swJr4u-HYr0&t=400s
install.packages('devtools')
library(devtools)
install_github("BillPetti/baseballr")

install.packages('tidyverse')
library(tidyverse)

date328407 = baseballr::scrape_statcast_savant(start_date = '2019-03-28',
                                               end_date = '2019-04-07', player_type = 'batter')

date408414 = baseballr::scrape_statcast_savant(start_date = '2019-04-08',
                                               end_date = '2019-04-14', player_type = 'batter')

date415421 = baseballr::scrape_statcast_savant(start_date = '2019-04-15',
                                               end_date = '2019-04-21', player_type = 'batter')

date422428 = baseballr::scrape_statcast_savant(start_date = '2019-04-22',
                                               end_date = '2019-04-28', player_type = 'batter')

date429505 = baseballr::scrape_statcast_savant(start_date = '2019-04-29',
                                               end_date = '2019-05-05', player_type = 'batter')

date506512 = baseballr::scrape_statcast_savant(start_date = '2019-05-06',
                                               end_date = '2019-05-12', player_type = 'batter')

date513519 = baseballr::scrape_statcast_savant(start_date = '2019-05-13',
                                               end_date = '2019-05-19', player_type = 'batter')

date520526 = baseballr::scrape_statcast_savant(start_date = '2019-05-20',
                                               end_date = '2019-05-26', player_type = 'batter')

date527602 = baseballr::scrape_statcast_savant(start_date = '2019-05-27',
                                               end_date = '2019-06-02', player_type = 'batter')

date603609 = baseballr::scrape_statcast_savant(start_date = '2019-06-03',
                                               end_date = '2019-06-09', player_type = 'batter')

date610616 = baseballr::scrape_statcast_savant(start_date = '2019-06-10',
                                               end_date = '2019-06-16', player_type = 'batter')

date617623 = baseballr::scrape_statcast_savant(start_date = '2019-06-17',
                                               end_date = '2019-06-23', player_type = 'batter')

date624630 = baseballr::scrape_statcast_savant(start_date = '2019-06-24',
                                               end_date = '2019-06-30', player_type = 'batter')

date701707 = baseballr::scrape_statcast_savant(start_date = '2019-07-01',
                                               end_date = '2019-07-07', player_type = 'batter')

date708714 = baseballr::scrape_statcast_savant(start_date = '2019-07-08',
                                               end_date = '2019-07-14', player_type = 'batter')

date715721 = baseballr::scrape_statcast_savant(start_date = '2019-07-15',
                                               end_date = '2019-07-21', player_type = 'batter')

date722728 = baseballr::scrape_statcast_savant(start_date = '2019-07-22',
                                               end_date = '2019-07-28', player_type = 'batter')

date729804 = baseballr::scrape_statcast_savant(start_date = '2019-07-29',
                                               end_date = '2019-08-04', player_type = 'batter')

date805811 = baseballr::scrape_statcast_savant(start_date = '2019-08-05',
                                               end_date = '2019-08-11', player_type = 'batter')

date812818 = baseballr::scrape_statcast_savant(start_date = '2019-08-12',
                                               end_date = '2019-08-18', player_type = 'batter')

date819825 = baseballr::scrape_statcast_savant(start_date = '2019-08-19',
                                               end_date = '2019-08-25', player_type = 'batter')

date826901 = baseballr::scrape_statcast_savant(start_date = '2019-08-26',
                                               end_date = '2019-09-01', player_type = 'batter')

date902908 = baseballr::scrape_statcast_savant(start_date = '2019-09-02',
                                               end_date = '2019-09-08', player_type = 'batter')

date909915 = baseballr::scrape_statcast_savant(start_date = '2019-09-09',
                                               end_date = '2019-09-15', player_type = 'batter')

date916922 = baseballr::scrape_statcast_savant(start_date = '2019-09-16',
                                               end_date = '2019-09-22', player_type = 'batter')

date923929 = baseballr::scrape_statcast_savant(start_date = '2019-09-23',
                                               end_date = '2019-09-29', player_type = 'batter')

#combine all data into one data frame
SavantData19 = rbind(date328407, date408414, date415421, date422428, date429505,
                     date506512, date513519, date520526, date527602, date603609,
                     date610616, date617623, date624630, date701707, date708714,
                     date715721, date722728, date729804, date805811, date812818,
                     date819825, date826901, date902908, date909915, date916922,
                     date923929)
SavantTest = SavantData19[sample(nrow(SavantData19),10),]

write.csv(SavantData19, "SavantHittingData19.csv", row.names = F)
