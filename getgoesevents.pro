;+
; NAME:
;   GetGoesEvents
;
; PURPOSE:
;   Grab the GOES flare events
;
; INPUTS:
;   startDateIso [string]: The start date to search in ISO format e.g., '2010-05-01T00:00:00Z'
;   endDateIso [string]:   The end date to search in format e.g., '2014-05-26T00:00:00Z'
;
; OPTIONAL INPUTS:
;   minFlareClass [string]:   Set this to the minimum flare class you want to accept, e.g., 'C'
;   filePathAndName [string]: The path and name to store the IDL saveset output. 
;                             Default is '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/SelectGoesEvents.sav'.
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Results in IDL saveset containing the GOES event data in a structure that's been enhanced with human and JD event times
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
;   GetGoesEvents, '2010-05-01T00:00:00Z', '2014-05-26T00:00:00Z', minFlareClass = 'C'
;
; MODIFICATION HISTORY:
;   2016-10-08: James Paul Mason: Wrote script.
;   2016-10-26: James Paul Mason: Changed input time format from dd-mm-yy to iso and now use JPMiso2ddmmyy to convert
;   2016-11-29: James Paul Mason: Updated to store normal times into the structure itself rather than as separate variables
;-
PRO GetGoesEvents, startDateIso, endDateIso, $ 
                   minFlareClass = minFlareClass, filePathAndName = filePathAndName

; Defaults
IF filePathAndName EQ !NULL THEN filePathAndName = '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/events/SelectGoesEvents.sav'

; Convert ISO time to backwards rd_gev time
startDateddmmyy = JPMyyyymmdd2dd_mon_yy(startDateIso)
endDateddmmyy = JPMyyyymmdd2dd_mon_yy(endDateIso)

goesEventPath = '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/events'
IF getenv('DIR_GEN_GEV') NE goesEventPath THEN setenv, 'DIR_GEN_GEV=' + goesEventPath
rd_gev, startDateddmmyy, endDateddmmyy, goesEvents

IF ~isa(goesEvents, 'struct') THEN BEGIN
  message, /INFO, JPMsystime() + ' No GOES events found for input times ' + startDateIso + ' to ' + endDateIso
  return
ENDIF

; Get event start times from GOES event list and convert from dumb anytim MJD structure to JD and human time
goesEventStartTimes = anytim(goesEvents, /MJD) ; Time is returned in milliseconds of day
goesEventStartTimesJd = goesEventStartTimes.MJD + 2400000.5 + goesEventStartTimes.TIME / 8.64D7
goesEventStartTimesHuman = JPMjd2iso(goesEventStartTimesJd, /NO_T_OR_Z)

; Get the GOES peak times as well
goesEventPeakTimesJd = goesEventStartTimesJd + goesEvents.peak / 86400. ; peak variable is the number of seconds since event start that the peak occurred
goesEventPeakTimesHuman = JPMjd2iso(goesEventPeakTimesJd, /NO_T_OR_Z)

; Add convenient times to structure
goesEvents = JPMAddTagsToStructure(goesEvents, 'eventStartTimeJd', 'double')
goesEvents.eventStartTimeJd = goesEventStartTimesJd
goesEvents = JPMAddTagsToStructure(goesEvents, 'eventStartTimeHuman', 'string')
goesEvents.eventStartTimeHuman = goesEventStartTimesHuman
goesEvents = JPMAddTagsToStructure(goesEvents, 'eventPeakTimeJd', 'double')
goesEvents.eventPeakTimeJd = goesEventPeakTimesJd
goesEvents = JPMAddTagsToStructure(goesEvents, 'eventPeakTimeHuman', 'string')
goesEvents.eventPeakTimeHuman = goesEventPeakTimesHuman

; Convert GOES class to GOES flux
goesFlux = !NULL
FOR goesEventIndex = 0, n_elements(goesEvents) - 1 DO BEGIN
  goesClass = string(goesEvents[goesEventIndex].ST$CLASS)
  goesClassLinearScale = float(strmid(goesClass, 1))
  IF strmid(goesClass, 0, 1) EQ 'A' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-8] ELSE $
  IF strmid(goesClass, 0, 1) EQ 'B' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-7] ELSE $
  IF strmid(goesClass, 0, 1) EQ 'C' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-6] ELSE $
  IF strmid(goesClass, 0, 1) EQ 'M' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-5] ELSE $
  IF strmid(goesClass, 0, 1) EQ 'X' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-4] $
  ELSE goesFlux = [goesFlux, !VALUES.F_NAN]
ENDFOR

; Add GOES flux to structure
goesEvents = JPMAddTagsToStructure(goesEvents, 'flux', 'float')
goesEvents.flux = goesFlux

; Restrict to select flares
IF minFlareClass NE !NULL THEN selectFlares = goesEvents[where(string(goesEvents.ST$CLASS) GE 'C')] ELSE selectFlares = goesEvents
numFlares = n_elements(selectFlares)

save, goesEvents, selectFlares, numFlares, $ 
      FILENAME = filePathAndName

END