;+
; NAME:
;   GetEVEInPercentChange
;
; PURPOSE:
;   Retrieve EVE line data and convert it to %change from the start time (always midnight because of the way eve_merge_evl works as of 2013/09/25) or a user selected time.
;
; INPUTS:
;   
;
; OPTIONAL INPUTS:
;   REFERENCE_TIME [integer]: Second of day to use as reference. Nearest point in EVE timeseries will be used. 
;   NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE [integer]: Determines the average of EVE data
;   
; KEYWORD PARAMETERS:
;   CHOOSE_REFERENCE_TIME: Enable to show a plot of 171 for help manually selecting a reference time
;
; OUTPUTS:
;   percentChangeOut [fltarr]: All of the EVE extracted lines converted to percentage units referenced to the REFERENCE_TIME
;   sodOut [dblarr]: The second of day for the EVE extracted lines
;
; OPTIONAL OUTPUTS:
;
;
; RESTRICTIONS:
;   SolarSoft EVE package
;
; EXAMPLE:
;   GetEVEInPercentChange, 2011041, 2011041, REFERENCE_TIME = 17100, evePercentChange, sod
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/09/25
;-
PRO GetEVEInPercentChange, startYYYYDOY, endYYYYDOY, REFERENCE_TIME = reference_time, NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE = number_of_10s_integrations_to_average, CHOOSE_REFERENCE_TIME = choose_reference_time, $
                           percentChangeOut, sodOut

IF ~keyword_set(NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE) THEN number_of_10s_integrations_to_average = 6

; Load EVE data, 60 second average
eveData = eve_merge_evl(startYYYYDOY, endYYYYDOY, N_AVERAGE = number_of_10s_integrations_to_average, META = eveMeta)
eveLines = eveData.line_irradiance

; Remove bad EVE data
eveLines[where(eveLines EQ -1)] = !VALUES.F_NAN

; If CHOOSE_REFERENCE_TIME is enabled, make a plot of 171 so the user can note what they want to use as the reference time
IF keyword_set(CHOOSE_REFERENCE_TIME) THEN BEGIN
  printfull, startYYYYDOY
  p = plot(evedata.sod, evelines[3, *])
  ;p.window.UVALUE={x0:0, y0:0, buttonDown:0L}
  ;p.window.MOUSE_DOWN_HANDLER='JPMMouseDownHandler' ; TODO: Ends up in window coordinates, not data units
  p2 = plot([reference_time+3600, reference_time+3600], p.yrange, 'r2', /OVERPLOT)
  p3 = plot([reference_time, reference_time], p.yrange, 'b2', /OVERPLOT)
  STOP
  p.close
ENDIF

; If reference time provided, determine what index it corresponds to
IF ~keyword_set(REFERENCE_TIME) THEN referenceIndex = 0 ELSE BEGIN
  sod = eveData.sod
  referenceIndex = closest(reference_time, sod, /DECIDE)
ENDELSE

; Convert to percent change
percentChange = eveLines & percentChange[*, *] = !VALUES.F_NAN
FOR i = 0, n_elements(eveLines[*, 0]) - 1 DO $
   percentChange[i, *] = perdiff(eveLines[i, referenceIndex], reform(eveLines[i, *]))

; Output values
percentChangeOut = percentChange
sodOut = eveData.sod
jdOut = anytim2jd(eveData.tai)

END

FUNCTION JPMMouseDownHandler, oWin, x, y, iButton, KeyMods, nClicks
  print, x, y
  return, 0
END