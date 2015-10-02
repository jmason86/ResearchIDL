;+
; NAME:
;   CalculateEVELinePrecision
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
;   VERBOSE: Set to print out the precisions
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
;-
FUNCTION CalculateEVELinePrecision, numberOfSamplesToAverage = numberOfSamplesToAverage, VERBOSE = VERBOSE

IF ~keyword_set(numberOfSamplesToAverage) THEN numberOfSamplesToAverage = 6. ; 60 second average

; Get data for a quiet period
eveLines = eve_merge_evl(2013028, 2013028, meta = evemeta, n_average = numberOfSamplesToAverage) ; Beginning of day is very quiet in 171
sod  = eveLines.sod
endIndex = closest(3600, sod)

eve94 = eveLines.line_irradiance[0]
eve132 = eveLines.line_irradiance[2]
eve171 = eveLines.line_irradiance[3]
eve177 = eveLines.line_irradiance[4]
eve180 = eveLines.line_irradiance[5]
eve195 = eveLines.line_irradiance[6]
eve202 = eveLines.line_irradiance[7]
eve211 = eveLines.line_irradiance[8]
eve284 = eveLines.line_irradiance[10]
eve335 = eveLines.line_irradiance[12]

; Compute precision
precision94 = stdev(eve94[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision132 = stdev(eve132[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision171 = stdev(eve171[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision177 = stdev(eve177[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision180 = stdev(eve180[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision195 = stdev(eve195[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision202 = stdev(eve202[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision211 = stdev(eve211[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision284 = stdev(eve284[0:endIndex]) / sqrt(numberOfSamplesToAverage)
precision335 = stdev(eve335[0:endIndex]) / sqrt(numberOfSamplesToAverage)

; Convert to percentage
precision94Percent = precision94 / eve94[0:endIndex] * 100. 
precision132Percent = precision132 / eve132[0:endIndex] * 100.
precision171Percent = precision171 / eve171[0:endIndex] * 100.
precision177Percent = precision177 / eve177[0:endIndex] * 100.
precision180Percent = precision180 / eve180[0:endIndex] * 100.
precision195Percent = precision195 / eve195[0:endIndex] * 100.
precision202Percent = precision202 / eve202[0:endIndex] * 100.
precision211Percent = precision211 / eve211[0:endIndex] * 100.
precision284Percent = precision284 / eve284[0:endIndex] * 100.
precision335Percent = precision335 / eve335[0:endIndex] * 100.

; Output
IF keyword_set(VERBOSE) THEN BEGIN
  print, 'Average 94 Å precision = ' + JPMPrintNumber(mean(precision94Percent)) + '%'
  print, 'Average 132 Å precision = ' + JPMPrintNumber(mean(precision132Percent)) + '%'
  print, 'Average 171 Å precision = ' + JPMPrintNumber(mean(precision171Percent)) + '%'
  print, 'Average 177 Å precision = ' + JPMPrintNumber(mean(precision177Percent)) + '%'
  print, 'Average 180 Å precision = ' + JPMPrintNumber(mean(precision180Percent)) + '%'
  print, 'Average 195 Å precision = ' + JPMPrintNumber(mean(precision195Percent)) + '%'
  print, 'Average 202 Å precision = ' + JPMPrintNumber(mean(precision202Percent)) + '%'
  print, 'Average 211 Å precision = ' + JPMPrintNumber(mean(precision211Percent)) + '%'
  print, 'Average 284 Å precision = ' + JPMPrintNumber(mean(precision284Percent)) + '%'
  print, 'Average 335 Å precision = ' + JPMPrintNumber(mean(precision335Percent)) + '%'
ENDIF

precisions = reform([[mean(precision94Percent)], [mean(precision132Percent)], [mean(precision171Percent)], $
                    [mean(precision177Percent)], [mean(precision180Percent)], [mean(precision195Percent)], $
                    [mean(precision202Percent)], [mean(precision211Percent)], [mean(precision284Percent)], [mean(precision335Percent)]] / 100.)

return, precisions

END