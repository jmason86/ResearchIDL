;+
; NAME:
;   DeterminePreflareIrradianceUnitTest
;
; PURPOSE:
;   Unit test for DeterminePreflareIrradiance
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Console message about success or failure
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Obviously requires the DeterminePreflareIrradiance code and dependencies therein
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2016-11-08: James Paul Mason: Wrote script.
;-
PRO DeterminePreflareIrradianceUnitTest

failCounter = 0

;;
; Case 1
;;

; Generate bogus input data
lightCurve = findgen(1000)
timeJd = timegen(1000, units = 'minutes', step_size = 1, start = systime(/julian) - 0.2)
goesEventStartTimeJd = timegen(1, start = systime(/julian) - 0.05) & goesEventStartTimeJd = goesEventStartTimeJd[0]
preflareTimesLast24HoursJd = [[goesEventStartTimeJd - 0.113, goesEventStartTimeJd - 0.091], $ 
                              [goesEventStartTimeJd - 0.065, goesEventStartTimeJd - 0.043]]
preflareIrradiancesLast24Hours = [27., 30.]
preflareUncertaintiesLast24Hours = [5., 6.]

; Run code
preflareIrradiance = DeterminePreflareIrradiance(lightCurve, timeJd, goesEventStartTimeJd, $ 
                                                 preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, preflareUncertaintiesLast24Hours, $ 
                                                 preflareTimeRangeJdOut = preflareTimeRangeJd, preflareIndicesOut = preflareIndices, $ 
                                                 preflareIrradianceUncertaintyOut = preflareIrradianceUncertainty, /VERBOSE)

; Check that output matches expectations
IF preflareIrradiance EQ 27. THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 1 success for pre-flare irradiance determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 1 failure for pre-flare irradiance determination. Should be ' + JPMPrintNumber(preflareIrradiancesLast24Hours) + $ 
                                 ' but returned ' + JPMPrintNumber(preflareIrradiance)
ENDELSE
IF preflareTimeRangeJd[0] EQ preflareTimesLast24HoursJd[0, 0] AND preflareTimeRangeJd[1] EQ preflareTimesLast24HoursJd[1, 0] THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 1 success for pre-flare time range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 1 failure for pre-flare time range determination. Should be: '
  print, preflareTimesLast24HoursJd[*, 0]
  print, ' but returned: '
  print, preflareTimeRangeJd
ENDELSE
IF preflareIndices[0] EQ 53 AND preflareIndices[1] EQ 85 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 1 success for pre-flare index range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 1 failure for pre-flare index range determination. Should be [53, 85]' + $
                                 ' but returned: '
  print, preflareIndices
ENDELSE
IF preflareIrradianceUncertainty EQ 5. THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 1 success for pre-flare uncertainty determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 1 failure for pre-flare uncertainty determination. Should be 5 ' + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradianceUncertainty)
ENDELSE

;;
; Case 2
;;

; Generate bogus input data
lightCurve = findgen(1000)
timeJd = timegen(1000, units = 'minutes', step_size = 1, start = systime(/julian) - 0.2)
goesEventStartTimeJd = timegen(1, start = systime(/julian) - 0.05) & goesEventStartTimeJd = goesEventStartTimeJd[0]
preflareTimesLast24HoursJd = [[goesEventStartTimeJd - 0.999, goesEventStartTimeJd - 0.972], $
                              [goesEventStartTimeJd - 0.113, goesEventStartTimeJd - 0.091], $
                              [goesEventStartTimeJd - 0.065, goesEventStartTimeJd - 0.043]]
preflareIrradiancesLast24Hours = [11., 27., 30.]
preflareUncertaintiesLast24Hours = [2., 13., 50.]

; Run code
preflareIrradiance = DeterminePreflareIrradiance(lightCurve, timeJd, goesEventStartTimeJd, $ 
                                                 preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, preflareUncertaintiesLast24Hours, $ 
                                                 preflareTimeRangeJdOut = preflareTimeRangeJd, preflareIndicesOut = preflareIndices, $
                                                 preflareIrradianceUncertaintyOut = preflareIrradianceUncertainty, /VERBOSE)

; Check that output matches expectations
IF preflareIrradiance EQ 27. THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 2 success for pre-flare irradiance determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 2 failure for pre-flare irradiance determination. Should be ' + JPMPrintNumber(preflareIrradiancesLast24Hours) + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradiance)
ENDELSE
IF preflareTimeRangeJd[0] EQ preflareTimesLast24HoursJd[0, 1] AND preflareTimeRangeJd[1] EQ preflareTimesLast24HoursJd[1, 1] THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 2 success for pre-flare time range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 2 failure for pre-flare time range determination. Should be: '
  print, preflareTimesLast24HoursJd[*, 1]
  print, ' but returned: '
  print, preflareTimeRangeJd
ENDELSE
IF preflareIndices[0] EQ 53 AND preflareIndices[1] EQ 85 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 2 success for pre-flare index range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 2 failure for pre-flare index range determination. Should be [53, 85]' + $
                                 ' but returned: '
  print, preflareIndices
ENDELSE
IF preflareIrradianceUncertainty EQ 13. THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 2 success for pre-flare uncertainty determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 2 failure for pre-flare uncertainty determination. Should be 5 ' + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradianceUncertainty)
ENDELSE

;;
; Case 3
;;

; Generate bogus input data
lightCurve = findgen(1000)
timeJd = timegen(1000, units = 'minutes', step_size = 1, start = systime(/julian) - 0.2)
goesEventStartTimeJd = timegen(1, start = systime(/julian) - 0.05) & goesEventStartTimeJd = goesEventStartTimeJd[0]
preflareTimesLast24HoursJd = [[goesEventStartTimeJd - 0.999, goesEventStartTimeJd - 0.972], $
                              [goesEventStartTimeJd - 0.113, goesEventStartTimeJd - 0.091], $
                              [goesEventStartTimeJd - 0.065, goesEventStartTimeJd - 0.043]]
preflareIrradiancesLast24Hours = [27., 27., 30.]
preflareUncertaintiesLast24Hours = [2., 13., 50.]

; Run code
preflareIrradiance = DeterminePreflareIrradiance(lightCurve, timeJd, goesEventStartTimeJd, $ 
                                                 preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, preflareUncertaintiesLast24Hours, $ 
                                                 preflareTimeRangeJdOut = preflareTimeRangeJd, preflareIndicesOut = preflareIndices,  $ 
                                                 preflareIrradianceUncertaintyOut = preflareIrradianceUncertainty, /VERBOSE)

; Check that output matches expectations
IF finite(preflareIrradiance) EQ 0 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 3 success for pre-flare irradiance determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 3 failure for pre-flare irradiance determination. Should be NAN' + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradiance)
ENDELSE
IF finite(preflareTimeRangeJd) EQ 0 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 3 success for pre-flare time range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 3 failure for pre-flare time range determination. Should be: '
  print, preflareTimesLast24HoursJd[*, 1]
  print, ' but returned: '
  print, preflareTimeRangeJd
ENDELSE
IF preflareIndices EQ !NULL THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 3 success for pre-flare index range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 3 failure for pre-flare index range determination. Should be [53, 85]' + $
                                 ' but returned: '
  print, preflareIndices
ENDELSE
IF finite(preflareIrradianceUncertainty) EQ 0 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 3 success for pre-flare uncertainty determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 3 failure for pre-flare uncertainty determination. Should be 5 ' + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradianceUncertainty)
ENDELSE

;;
; Case 4
;;

; Generate bogus input data
lightCurve = findgen(1000)
timeJd = timegen(1000, units = 'minutes', step_size = 1, start = systime(/julian) - 0.25)
goesEventStartTimeJd = timegen(1, start = systime(/julian) - 0.05) & goesEventStartTimeJd = goesEventStartTimeJd[0]
preflareTimesLast24HoursJd = !NULL
preflareIrradiancesLast24Hours = !NULL
preflareUncertaintiesLast24Hours = !NULL

; Run code
preflareIrradiance = DeterminePreflareIrradiance(lightCurve, timeJd, goesEventStartTimeJd, $
                                                 preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, preflareUncertaintiesLast24Hours, $
                                                 preflareTimeRangeJdOut = preflareTimeRangeJd, preflareIndicesOut = preflareIndices,  $
                                                 preflareIrradianceUncertaintyOut = preflareIrradianceUncertainty, /VERBOSE)

; Check that output matches expectations
IF finite(preflareIrradiance) EQ 0 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 4 success for pre-flare irradiance determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 4 failure for pre-flare irradiance determination. Should be NAN' + $
    ' but returned ' + JPMPrintNumber(preflareIrradiance)
ENDELSE
IF finite(preflareTimeRangeJd) EQ 0 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 4 success for pre-flare time range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 4 failure for pre-flare time range determination. Should be: '
  print, preflareTimesLast24HoursJd[*, 1]
  print, ' but returned: '
  print, preflareTimeRangeJd
ENDELSE
IF preflareIndices EQ !NULL THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 4 success for pre-flare index range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 4 failure for pre-flare index range determination. Should be [53, 85]' + $
                                 ' but returned: '
  print, preflareIndices
ENDELSE
IF finite(preflareIrradianceUncertainty) EQ 0 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 4 success for pre-flare uncertainty determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 4 failure for pre-flare uncertainty determination. Should be 5 ' + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradianceUncertainty)
ENDELSE

;;
; Case 5
;;

; Generate bogus input data
lightCurve = findgen(1000)
timeJd = timegen(1000, units = 'minutes', step_size = 1, start = systime(/julian) - 0.25)
goesEventStartTimeJd = timegen(1, start = systime(/julian) - 0.05) & goesEventStartTimeJd = goesEventStartTimeJd[0]
preflareTimesLast24HoursJd = !NULL
preflareIrradiancesLast24Hours = !NULL
preflareUncertaintiesLast24Hours = !NULL

; Run code
preflareIrradiance = DeterminePreflareIrradiance(lightCurve, timeJd, goesEventStartTimeJd, $
                                                 preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, preflareUncertaintiesLast24Hours, $
                                                 preflareWindowStdDevThreshold = 55., $
                                                 preflareTimeRangeJdOut = preflareTimeRangeJd, preflareIndicesOut = preflareIndices,  $
                                                 preflareIrradianceUncertaintyOut = preflareIrradianceUncertainty, /VERBOSE)

; Check that output matches expectations
IF preflareIrradiance EQ 198 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 5 success for pre-flare irradiance determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 5 failure for pre-flare irradiance determination. Should be NAN' + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradiance)
ENDELSE
IF systime(/julian) - preflareTimeRangeJd[0] LT 0.2 AND systime(/julian) - preflareTimeRangeJd[1] LT 0.1 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 5 success for pre-flare time range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 5 failure for pre-flare time range determination. Should be: '
  print, preflareTimesLast24HoursJd[*, 1]
  print, ' but returned: '
  print, preflareTimeRangeJd
ENDELSE
IF preflareIndices[0] EQ 108 AND preflareIndices[1] EQ 287 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 5 success for pre-flare index range determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 5 failure for pre-flare index range determination. Should be [53, 85]' + $
                                 ' but returned: '
  print, preflareIndices
ENDELSE
IF preflareIrradianceUncertainty EQ 4.8546586 THEN BEGIN
  message, /INFO, JPMsystime() + ' Case 5 success for pre-flare uncertainty determination.'
ENDIF ELSE BEGIN
  failCounter++
  message, /INFO, JPMsystime() + ' Case 5 failure for pre-flare uncertainty determination. Should be 5 ' + $
                                 ' but returned ' + JPMPrintNumber(preflareIrradianceUncertainty)
ENDELSE

;;
; Close out
;;

IF failCounter EQ 0 THEN BEGIN
  message, /INFO, JPMsystime() + ' Unit test fully successful.'
ENDIF ELSE BEGIN
  message, /INFO, JPMsystime() + ' Unit test resulted in ' + JPMPrintNumber(failCounter, /NO_DECIMALS) + ' failures.'
ENDELSE

END