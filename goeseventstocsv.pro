;+
; NAME:
;   GOESEventsToCSV
;
; PURPOSE:
;   Retrieve and output GOES event data as a CSV file
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   DATE     [string]: The date for the day you want to retrieve data for. Default is today. 
;   SAVE_LOC [string]: Path to the directory to store the CSV file
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   CSV file with GOES events: Day, time, peak, duration, class, and location in lat, lon
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft and GOES database
;
; EXAMPLE:
;   GOESEventsToCSV, START_DATE = '15-feb-2011', END_DATE = '16-feb-2011'
;
; MODIFICATION HISTORY:
; 2013-09-03: James Paul Mason: Wrote script. 
; 2018-06-25: James Paul Mason: Figured out that the first reported location is longitude and second is latitude. Made csv titles accordingly explicit. 
;-
PRO GOESEventsToCSV, START_DATE = start_date, END_DATE = end_date, SAVE_LOCATION = saveloc

IF ~keyword_set(START_DATE) THEN start_date = systim()
IF ~keyword_set(END_DATE) THEN end_date = systim()
IF ~keyword_set(SAVE_LOCATION) THEN saveloc = './'

; Get the GOES data
rd_gev, start_date, end_date, goesEvents

; Open csv file
close,1 & openw, 1, saveloc + 'GOESEvents' + start_date + '.csv'
printf, 1, 'Day, Time, Peak, Duration, Class, Longitude[º], Latitude[º]'

FOR i = 0, n_elements(goesEvents.time) - 1 DO $
  printf, 1, goesEvents[i].day, ',', goesEvents[i].time, ',', goesEvents[i].peak, ',', goesEvents[i].duration, ',', string(goesEvents[i].st$class), ',', goesEvents[i].location[0], ',', goesEvents[i].location[1]

close, 1

END