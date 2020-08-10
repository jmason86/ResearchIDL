;+
; NAME:
;   GOES1mDataToCSV
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
;   MAKE_IDL_SAVESET: Set this to make an IDL saveset instead of a csv file
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
;   GOES1mDataToCSV, START_DATE = '7-mar-2012 00:00:00.000', END_DATE = '8-mar-2012 00:00:00.000'
;
; MODIFICATION HISTORY:
;     2013/09/03: James Paul Mason: Wrote script
;     2016/06/22: James Paul Mason: Added MAKE_IDL_SAVESET keyword and functionality
;-
PRO GOES1mDataToCSV, START_DATE = start_date, END_DATE = end_date, SAVE_LOCATION = saveloc, MAKE_IDL_SAVESET = MAKE_IDL_SAVESET

IF ~keyword_set(START_DATE) THEN start_date = systim()
IF ~keyword_set(END_DATE) THEN end_date = systim()
IF ~keyword_set(SAVE_LOCATION) THEN saveloc = './'
  
; Get the GOES data
rd_gxd, start_date, end_date, goesData, /ONE_MINUTE

IF keyword_set(MAKE_IDL_SAVESET) THEN BEGIN
  time = goesData.time
  day = goesData.day
  flux = goesData.lo
  ; not storing high channel (high energy?) because Allison doesn't need it
  save, time, day, flux, FILENAME = saveloc + 'GOES_xray_EVE_era.sav'
ENDIF ELSE BEGIN

  ; Open csv file and write the data
  close,1 & openw, 1, saveloc + 'GOESData' + str_replace(start_date, '/', '-') + '.csv'
  printf, 1, 'Day, Time, LowChannel, HiChannel' ; TODO: Add units
  FOR i = 0, n_elements(goesData.time) - 1 DO printf, 1, goesData[i].day, ',', goesData[i].time, ',', goesData[i].lo, ',', goesData[i].hi
  close, 1
ENDELSE 
END