;+
; NAME:
;   GetMinxssMegsbFlareData
;
; PURPOSE:
;   Locate and store data for the biggest flare observed by both MinXSS-1 and SDO/EVE/MEGS-B to date
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   UPDATE_GOES_EVENTS:    Set this to run the ssw routine to update the database
;   UPDATE_EVE_DATA:       Set this to update the EVE data to the present date 
;   PLOT_FLARE_OCCURRENCE: Set this to generate a histogram of the flare occurrence duing the MinXSS era
;
; OUTPUTS:
;   IDL saveset with MEGS-B and MinXSS-1 data in separate variables
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires EVE SSW package, MinXSS IDL code package, access to both sets of data, and access to 
;   the GOES event list (don't need GOES SSW if already ran GetGoesEvents.pro to generate GOES_events_MinXSS_era.sav)
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2016-10-26: James Paul Mason: Wrote script.
;   2016-11-16: James Paul Mason: Added UPDATE_EVE_DATA keyword
;-
PRO GetMinxssMegsbFlareData, UPDATE_GOES_EVENTS = UPDATE_GOES_EVENTS, UPDATE_EVE_DATA = UPDATE_EVE_DATA, PLOT_FLARE_OCCURRENCE = PLOT_FLARE_OCCURRENCE

; Setup
datalocEve = '/Users/' + getenv('username') + '/Dropbox/Research/Data/EVE/
datalocGoes = '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/'
datalocMinxss = getenv('minxss_data') + '/fm1/level1/'
datalocAll3 = '/Users/' + getenv('username') + '/Dropbox/Research/Data/EVE-GOES-MinXSS/'

; Grab the list of GOES flare events
IF keyword_set(UPDATE_GOES_EVENTS) THEN sswdb_upgrade, /spawn
IF file_test(datalocGoes + 'GOES_events_MinXSS_era.sav') THEN restore, datalocGoes + 'GOES_events_MinXSS_era.sav' ELSE BEGIN
  GetGoesEvents, '2016-05-16T00:00:00Z', JPMsystime(/ISO), filePathAndName = datalocGoes + 'GOES_events_MinXSS_era.sav'
ENDELSE

; Guess at flare end times and add to structure
eventEndTimesJd = goesEvents.eventPeakTimeJd + 1./24. ; 1 hour after peak
goesEvents = JPMAddTagsToStructure(goesEvents, 'eventEndTimeJd', 'double')
goesEvents.eventEndTimeJd = eventEndTimesJd
goesEvents = JPMAddTagsToStructure(goesEvents, 'eventEndTimeHuman', 'string')
goesEvents.eventEndTimeHuman = JPMjd2iso(eventEndTimesJd, /NO_T_OR_Z)

; Convert GOES class to GOES flux
goesFlux = !NULL
FOR goesEventIndex = 0, n_elements(goesEvents) - 1 DO BEGIN
  goesClass = string(goesEvents[goesEventIndex].ST$CLASS)
  goesClassLinearScale = float(strmid(goesClass, 1))
  IF strmid(goesClass, 0, 1) EQ 'B' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-7]
  IF strmid(goesClass, 0, 1) EQ 'C' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-6]
  IF strmid(goesClass, 0, 1) EQ 'M' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-5]
  IF strmid(goesClass, 0, 1) EQ 'X' THEN goesFlux = [goesFlux, goesClassLinearScale * 1E-4]
ENDFOR

; Optionally create flare occurrence histogram
IF keyword_set(PLOT_FLARE_OCCURRENCE) THEN BEGIN
  hist = histogram(goesFlux, NBINS = 1000, LOCATIONS = fluxBins) 
  p1 = barplot(fluxBins, hist, /HISTOGRAM, $ 
               TITLE = 'Histogram of Flares During MinXSS Era (until ' + JPMsystime() + ')', $
               XTITLE = 'GOES Class', XRANGE = [1E-7, 1E-3], XTICKNAME = ['B', 'C', 'M', 'X', ''], $
               YTITLE = '#')
  t = text(0.8, 0.8, 'n = ' + JPMPrintNumber(n_elements(goesFlux), /NO_DECIMALS))
ENDIF
STOP
; Restore MinXSS data
restore, datalocMinxss + 'minxss1_l1_mission_length.sav'

; Get EVE data
IF keyword_set(UPDATE_EVE_DATA) THEN BEGIN
  yyyydoyStart = JPMjd2yyyydoy(JPMyyyymmddhhmmss2jd('2016-05-16T00:00:00Z'))
  yyyydoyEnd = JPMjd2yyyydoy(JPMyyyymmddhhmmss2jd(JPMsystime(/UTC)))
  eveLines = eve_merge_evl(yyyydoyStart, yyyydoyEnd, meta = evemeta, N_AVERAGE = 6) ; 1 minute cadence data
  eveSpectra = eve_merge_evs(yyyydoyStart, yyyydoyEnd, meta = evemeta, N_AVERAGE = 6) ; 1 minute cadence data
ENDIF ELSE BEGIN
  restore, datalocEve + 'eve_lines_20160516-20161116 MinXSS-1 Mission.sav'
ENDELSE

; Convert EVE TAI time to JD and human to be consistent with my standard workflow
eveLines = JPMAddTagsToStructure(eveLines, 'timeJd', 'double')
eveLines.timeJd = JPMtai2jd(eveLines.tai)
eveLines = JPMAddTagsToStructure(eveLines, 'timeHuman', 'string')
eveLines.timeHuman = JPMjd2iso(eveLines.timeJd, /NO_T_OR_Z)
eveSpectra = JPMAddTagsToStructure(eveSpectra, 'timeJd', 'double')
eveSpectra.timeJd = JPMtai2jd(eveSpectra.tai)
eveSpectra = JPMAddTagsToStructure(eveSpectra, 'timeHuman', 'string')
eveSpectra.timeHuman = JPMjd2iso(eveSpectra.timeJd, /NO_T_OR_Z)

; Include the wavelengths in the EVE spectra because why the hell aren't they alredy? 
eveSpectra = JPMAddTagsToStructure(eveSpectra, 'wavelength', 'fltarr', numElements = 5200)
eveSpectra.wavelength = wavelength

;
; Identify the biggest flare observed by both MinXSS-1 and MEGS-B
; 

keepSearching = 1
WHILE keepSearching DO BEGIN
  
  ; Identify the next biggest flare 
  flareFlux = max(goesEvents.flux, flareGoesIndex)
  flareClass = strtrim(goesEvents[flareGoesIndex].st$class, 2)
  
  ; Isolate MinXSS and EVE data during the flare
  minxssFlareIndices = where(minxsslevel1.time.jd GE goesEvents[flareGoesIndex].eventStartTimeJd AND $ 
                             minxsslevel1.time.jd LE goesEvents[flareGoesIndex].eventEndTimeJd, minxssSpectraCount)
  eveFlareIndices = where(eveLines.timeJd GE goesEvents[flareGoesIndex].eventStartTimeJd AND $ 
                          eveLines.timeJd LE goesEvents[flareGoesIndex].eventEndTimeJd, eveSpectraCount)

  ; Determine if MinXSS and EVE have at least 10 spectra during the flare
  IF minxssSpectraCount GE 10 AND eveSpectraCount GE 10 THEN BEGIN
    keepSearching = 0
  ENDIF ELSE BEGIN
    goesFlux[flareGoesIndex] = 0.
  ENDELSE
  
ENDWHILE

; Isolate just the data of interest
goesEvent = goesEvents[flareGoesIndex]
minxss = minxsslevel1[minxssFlareIndices]
eveLines = eveLines[eveFlareIndices]
eveSpectra = eveSpectra[eveFlareIndices]

; Save the GOES, MinXSS, and EVE data for the biggest flare to an IDL saveset
save, goesEvent, minxss, eveLines, eveSpectra, FILENAME = datalocAll3 + 'FlareAnalysis.sav', /COMPRESS

END