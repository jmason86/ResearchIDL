;+
; NAME:
;   GoesProtonDataToSaveset
;
; PURPOSE:
;   Retrieve and output GOES proton data as a CSV file
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   DATE     [string]: The date for the day you want to retrieve data for. Format should be 'd-mon-yyyy' e.g., '1-feb-2010'
;   SAVE_LOC [string]: Path to the directory to store the CSV file
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   IDL saveset with goes proton data, including time stamps
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft and GOES proton database
;
; EXAMPLE:
;   GoesProtonDataToSaveset, START_DATE = '1-feb-2010', END_DATE = '30-may-2014'
;
; MODIFICATION HISTORY:
;   2016/06/22: James Paul Mason: Wrote script
;-
PRO GoesProtonDataToSaveset, START_DATE = start_date, END_DATE = end_date, SAVE_LOCATION = saveloc

; Defaults
IF start_date EQ !NULL THEN start_date = '1-feb-2010'
IF end_date EQ !NULL THEN end_date = '30-may-2014' ; Roughly the era of SDO/EVE data
IF saveloc EQ !NULL THEN saveloc = './'

; Get the GOES proton data
rd_goesp_ascii, start_date, end_date, goesp

date = goesp.date
mjd = goesp.mjd
secondOfDay = goesp.sec
time = goesp.time
protonCounts = goesp.p

; Save data 
save, date, mjd, secondOfDay, time, protonCounts, FILENAME = saveloc + 'GOES_proton_data.sav'

END