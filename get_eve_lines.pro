;+
; NAME:
;   get_eve_lines
;
; PURPOSE:
;   Get the EVE extract emission lines data product across the input time, merged into a single file.
;   Also replaces bad data flag (-1) with NAN. 
;   Also includes julian date and ISO8601 format date.
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   start_date [string]:            ISO8601 format date (yyyy-mm-dd).  
;                                   Default is the start of mission data, 2010-05-01.
;   end_date [string]:              ISO8601 format date (yyyy-mm-dd).
;                                   Default is the current date, provided by JPMsystime(/UTC). 
;   n_samples_to_average [integer]: The number of samples to average. Each sample is 10 seconds long, so setting this equal to 6 mean 1 minute cadence.
;                                   Default is 6.
;   saveloc [string]:               The path to save the new file to. 
;   
; KEYWORD PARAMETERS:
;   VERBOSE: Set to print out the precisions
;
; OUTPUTS:
;   Saves file to disk in saveloc. 
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires JPMsystime(). 
;
; EXAMPLE:
;   get_eve_lines, start_date='2010-05-01', end_date=JPMsystime(/UTC), number
;-
PRO get_eve_lines, $
    start_date=start_date, end_date=end_date, n_samples_to_average=n_samples_to_average, $
    VERBOSE=VERBOSE

; Defaults
IF start_date EQ !NULL THEN start_date = '2010-05-01'
IF end_date EQ !NULL THEN end_date = strmid(JPMsystime(/UTC), 0, 10)
IF n_samples_to_average EQ !NULL THEN n_samples_to_average = 6 ; 60 second average
IF saveloc EQ !NULL THEN saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Data/SDO/EVE/'

start_date_yd = JPMjd2yyyydoy(JPMiso2jd(start_date))
end_date_yd = JPMjd2yyyydoy(JPMiso2jd(end_date))

eve_lines = eve_merge_evl(start_date_yd, end_date_yd, meta=evemeta, n_average=n_samples_to_average)
save, eve_lines, filename=saveloc + 'eve_lines_' + start_date + '_to_' + end_date + '.sav', /COMPRESS

  
END