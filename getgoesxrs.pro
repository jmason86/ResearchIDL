;+
; NAME:
;   GetGoesXrs
;
; PURPOSE:
;   Grab the GOES/XRS data at 1m cadence
;
; INPUTS:
;   startDateIso [string]: The start date to search in ISO format e.g., '2010-05-01T00:00:00Z'
;   endDateIso [string]:   The end date to search in format e.g., '2014-05-26T00:00:00Z'
;
; OPTIONAL INPUTS:
;   filePathAndName [string]: The path and name to store the IDL saveset output.
;                             Default is '~/Dropbox/Research/Data/GOES/Selectxrs.sav'.
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Results in IDL saveset containing the GOES XRS data in a structure that's been enhanced with human and JD event times
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
;   GetGoesXrs, '2010-05-01T00:00:00Z', '2014-05-26T00:00:00Z'
;-
PRO GetGoesXrs, startDateIso, endDateIso, $
                filePathAndName=filePathAndName

; Defaults
IF filePathAndName EQ !NULL THEN filePathAndName = '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/events/GoesXrs.sav'

; Convert ISO time to backwards rd_gev time
startDateddmmyy = JPMyyyymmdd2dd_mon_yy(startDateIso)
endDateddmmyy = JPMyyyymmdd2dd_mon_yy(endDateIso)

goesEventPath = '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/xrs/'
IF getenv('DIR_GEN_GEV') NE goesEventPath THEN setenv, 'DIR_GEN_GEV=' + goesEventPath
rd_gxd, startDateddmmyy, endDateddmmyy, xrs, /ONE_MINUTE, /GOES15

IF ~isa(xrs, 'struct') THEN BEGIN
  message, /INFO, JPMsystime() + ' No GOES/XRS data found for input times ' + startDateIso + ' to ' + endDateIso
  return
ENDIF

; Convert from dumb anytim MJD structure to JD and human time
goesTime = anytim(xrs, /MJD) ; Time is returned in milliseconds of day
goesTimeJd = goesTime.MJD + 2400000.5 + goesTime.TIME / 8.64D7
goesTimeIso = JPMjd2iso(goesTimeJd)
goesTimeHuman = JPMjd2iso(goesTimeJd, /NO_T_OR_Z)

; Add convenient times to structure
xrs = JPMAddTagsToStructure(xrs, 'TimeJd', 'double')
xrs.TimeJd = goesTimeJd
xrs = JPMAddTagsToStructure(xrs, 'TimeIso', 'string')
xrs.TimeIso = goesTimeIso
xrs = JPMAddTagsToStructure(xrs, 'TimeHuman', 'string')
xrs.TimeHuman = goesTimeHuman

save, xrs, filename=filePathAndName

END