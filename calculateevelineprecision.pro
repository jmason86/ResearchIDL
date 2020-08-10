;+
; NAME:
;   CalculateEVELinePrecision
;
; PURPOSE:
;   Compute precisions for all emission lines in EVE. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   numberOfSamplesToAverage [float]: The number of 10 second integrations to average. Default is 6 (60 seconds). 
;
; KEYWORD PARAMETERS:
;   RELOAD_EVE_DATA: Set this to force the code to grab data from the EVE_DATA server. The data will be saved 
;                    to disk as an IDL saveset. When keyword is not set, that saveset is restored. 
;   VERBOSE:         Set to print out the precisions
;
; OUTPUTS:
;   Returns array of precisions. 
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires JPMPrintNumber
;
; EXAMPLE:
;   precisions = CalculateEVELinePrecision(/VERBOSE)
;
; MODIFICATION HISTORY:
;   2016-10-05: James Paul Mason: Wrote script, largely based on CalculateEveLinePrecision.pro but now for all emission lines in EVE
;-
FUNCTION CalculateEVELinePrecision, numberOfSamplesToAverage = numberOfSamplesToAverage, RELOAD_EVE_DATA = RELOAD_EVE_DATA, VERBOSE = VERBOSE

; Defaults
IF ~keyword_set(numberOfSamplesToAverage) THEN numberOfSamplesToAverage = 6. ; 60 second average

; Setup
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/EVE Precision/'

; Get data for a quiet period - beginning of the below day is very quiet in 171
IF keyword_set(RELOAD_EVE_DATA) THEN BEGIN 
  eveLinesRaw = eve_merge_evl(2013028, 2013028, meta = evemeta)
  eveLines = evelinesRaw.line_irradiance
  sod = eveLinesRaw.sod
  
  ; Remove bad data 
  badDataIndices = where(eveLines EQ -1)
  eveLines[badDataIndices] = !VALUES.F_NAN
  save, sod, eveLines, FILENAME = saveloc + 'EVE Line Data.sav'
ENDIF ELSE restore, saveloc + 'EVE Line Data.sav'

; Grab time and end of one hour index
endIndexRaw = closest(3600, sod)

; Restrict EVE data to just that hour
eveLines = eveLines[*, 0:endIndexRaw]

precisionOverTime = eveLines & precisionOverTime[*, *] = !VALUES.F_NAN
FOR timeIndex = 0, n_elements(eveLines[0, *]) / numberOfSamplesToAverage - 1 DO BEGIN
  FOR lineIndex = 0, n_elements(eveLines[*, 0]) - 1 DO BEGIN
    currentTimeIndexRange = [timeIndex * numberOfSamplesToAverage: timeIndex * numberOfSamplesToAverage + numberOfSamplesToAverage - 1]
    precisionOverTime[lineIndex, timeIndex] = stddev(eveLines[lineIndex, currentTimeIndexRange], /NAN) / sqrt(numberOfSamplesToAverage) / mean(eveLines[lineIndex, currentTimeIndexRange], /NAN)
  ENDFOR
ENDFOR

; Take average of normalized precisions over the hour long period and put in % units
precisionPercent = mean(precisionOverTime, DIMENSION = 2, /NAN) * 100.
precisions = precisionPercent / 100.

return, precisions

END