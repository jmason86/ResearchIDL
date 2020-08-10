;+
; NAME:
;   CalculateEVEFeLinePrecision
;
; PURPOSE:
;   Compute precisions for important lines in EVE. 
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
;   2015/02/18: James Paul Mason: Wrote script.
;   2016/04/26: James Paul Mason: Modifications based on arguments from Amir Caspi. No longer getting standard deviation of mean, 
;                                 instead just getting standard deviaiton. 
;   2017-10-06: James Paul Mason: Changed how eveLines is handled: now just an array rather than structure (but reload_eve_data not yet updated)
;-
FUNCTION CalculateEVEFeLinePrecision, numberOfSamplesToAverage = numberOfSamplesToAverage, RELOAD_EVE_DATA = RELOAD_EVE_DATA, VERBOSE = VERBOSE

; Defaults
IF ~keyword_set(numberOfSamplesToAverage) THEN numberOfSamplesToAverage = 6. ; 60 second average

; Setup
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/EVE Precision/'

; Get data for a quiet period - beginning of the below day is very quiet in 171
IF keyword_set(RELOAD_EVE_DATA) THEN BEGIN 
  eveLines = eve_merge_evl(2013028, 2013028, meta = evemeta)
  eveLinesAverage = eve_merge_evl(2013028, 2013028, meta = evemeta, n_average = numberOfSamplesToAverage)
  save, eveLines, FILENAME = saveloc + 'EVE Line Data.sav'
ENDIF ELSE restore, saveloc + 'EVE Line Data.sav'

endIndexRaw = closest(3600, sod)

eve94 = eveLines[0, 0:endIndexRaw]
eve132 = eveLines[2, 0:endIndexRaw]
eve171 = eveLines[3, 0:endIndexRaw]
eve177 = eveLines[4, 0:endIndexRaw]
eve180 = eveLines[5, 0:endIndexRaw]
eve195 = eveLines[6, 0:endIndexRaw]
eve202 = eveLines[7, 0:endIndexRaw]
eve211 = eveLines[8, 0:endIndexRaw]
eve284 = eveLines[10, 0:endIndexRaw]
eve335 = eveLines[12, 0:endIndexRaw]

precisionOverTime94 = !NULL
precisionOverTime132 = !NULL
precisionOverTime171 = !NULL
precisionOverTime177 = !NULL
precisionOverTime180 = !NULL
precisionOverTime195 = !NULL
precisionOverTime202 = !NULL
precisionOverTime211 = !NULL
precisionOverTime284 = !NULL
precisionOverTime335 = !NULL
FOR i = 0, n_elements(eve94) / numberOfSamplesToAverage - 1 DO BEGIN
  currentIndexRange = [i * numberOfSamplesToAverage: i * numberOfSamplesToAverage + numberOfSamplesToAverage - 1]
  precisionOverTime94 = [precisionOverTime94, stddev(eve94[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve94[currentIndexRange])]
  precisionOverTime132 = [precisionOverTime132, stddev(eve132[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve132[currentIndexRange])]
  precisionOverTime171 = [precisionOverTime171, stddev(eve171[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve171[currentIndexRange])]
  precisionOverTime177 = [precisionOverTime177, stddev(eve177[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve177[currentIndexRange])]
  precisionOverTime180 = [precisionOverTime180, stddev(eve180[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve180[currentIndexRange])]
  precisionOverTime195 = [precisionOverTime195, stddev(eve195[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve195[currentIndexRange])]
  precisionOverTime202 = [precisionOverTime202, stddev(eve202[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve202[currentIndexRange])]
  precisionOverTime211 = [precisionOverTime211, stddev(eve211[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve211[currentIndexRange])]
  precisionOverTime284 = [precisionOverTime284, stddev(eve284[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve284[currentIndexRange])]
  precisionOverTime335 = [precisionOverTime335, stddev(eve335[currentIndexRange]) / sqrt(numberOfSamplesToAverage) / mean(eve335[currentIndexRange])]
ENDFOR

; Take average of normalized precisions over the hour long period and put in % units
precision94Percent = mean(precisionOverTime94) * 100.
precision132Percent = mean(precisionOverTime132) * 100.
precision171Percent = mean(precisionOverTime171) * 100.
precision177Percent = mean(precisionOverTime177) * 100.
precision180Percent = mean(precisionOverTime180) * 100.
precision195Percent = mean(precisionOverTime195) * 100.
precision202Percent = mean(precisionOverTime202) * 100.
precision211Percent = mean(precisionOverTime211) * 100.
precision284Percent = mean(precisionOverTime284) * 100.
precision335Percent = mean(precisionOverTime335) * 100.

; Compute precision - Method suggested by Tom
;precision94 = stdev(eve94[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision132 = stdev(eve132[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision171 = stdev(eve171[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision177 = stdev(eve177[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision180 = stdev(eve180[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision195 = stdev(eve195[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision202 = stdev(eve202[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision211 = stdev(eve211[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision284 = stdev(eve284[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;precision335 = stdev(eve335[0:endIndex]) / sqrt(numberOfSamplesToAverage)
;
;; Convert to percentage
;precision94Percent = precision94 / eve94[0:endIndex] * 100. 
;precision132Percent = precision132 / eve132[0:endIndex] * 100.
;precision171Percent = precision171 / eve171[0:endIndex] * 100.
;precision177Percent = precision177 / eve177[0:endIndex] * 100.
;precision180Percent = precision180 / eve180[0:endIndex] * 100.
;precision195Percent = precision195 / eve195[0:endIndex] * 100.
;precision202Percent = precision202 / eve202[0:endIndex] * 100.
;precision211Percent = precision211 / eve211[0:endIndex] * 100.
;precision284Percent = precision284 / eve284[0:endIndex] * 100.
;precision335Percent = precision335 / eve335[0:endIndex] * 100.

; Output
IF keyword_set(VERBOSE) THEN BEGIN
  print, 'Average 94 Å precision = ' + JPMPrintNumber(precision94Percent) + '%'
  print, 'Average 132 Å precision = ' + JPMPrintNumber(precision132Percent) + '%'
  print, 'Average 171 Å precision = ' + JPMPrintNumber(precision171Percent) + '%'
  print, 'Average 177 Å precision = ' + JPMPrintNumber(precision177Percent) + '%'
  print, 'Average 180 Å precision = ' + JPMPrintNumber(precision180Percent) + '%'
  print, 'Average 195 Å precision = ' + JPMPrintNumber(precision195Percent) + '%'
  print, 'Average 202 Å precision = ' + JPMPrintNumber(precision202Percent) + '%'
  print, 'Average 211 Å precision = ' + JPMPrintNumber(precision211Percent) + '%'
  print, 'Average 284 Å precision = ' + JPMPrintNumber(precision284Percent) + '%'
  print, 'Average 335 Å precision = ' + JPMPrintNumber(precision335Percent) + '%'
ENDIF

precisions = reform([[precision94Percent], [precision132Percent], [precision171Percent], $
                    [precision177Percent], [precision180Percent], [precision195Percent], $
                    [precision202Percent], [precision211Percent], [precision284Percent], [precision335Percent]] / 100.)

return, precisions

END