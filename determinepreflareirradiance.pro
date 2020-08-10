;+
; NAME:
;   DeterminePreflareIrradiance
;
; PURPOSE:
;   Best estimate of the pre-flare irradiance in a light curve from SDO/EVE
;
; INPUTS:
;   lightCurve [fltarr]:                       Irradiance for a single emission line as a function of time. Must extend at least thresholdTimePriorFlareMinutes back. 
;   timeJd [dblarr]:                           Time corresponding to the lightCurve points in julian date format. Must extend at least thresholdTimePriorFlareMinutes back.
;   lineName [string]:                         The name of the emission line corresponding to light curve, e.g., 'Fe IX 171'
;   goesEventStartTimeJd [double]:             The time that the GOES flare event has listed for the start time
;   preflareTimesLast24HoursJd [dblarr]:       [2, N] The list of all prior times that flares occurred up to 24 hours ago where a pre-flare irradiance was calculated. 
;                                              Element [0, N] is the start time used for the calculation and element [1, N] is the end time used. 
;   preflareIrradiancesLast24Hours [fltarr]:   The irradiances corresponding to preflareTimesLast24HoursJd
;   preflareUncertaintiesLast24Hours [fltarr]: The uncertainties corresponding to preflareIrradiancesLast24Hours
;   
; OPTIONAL INPUTS:
;   thresholdTimePriorFlareMinutes [float]:                  How long before the current event does the last one need to have happened for this event to be 
;                                                            considered independent. If the last one was too recent, will use that events preflare irradiance. 
;                                                            Default is 240 (4 hours). 
;   preflareWindowStdDevThreshold [float]:                   The maximum allowed standard deviation in the pre-flare irradiance calculation window. 
;                                                            Default is 0.02. TODO: Figure out if this is a sensible number. 
;   preflareSubWindowMedianDifferenceThreshold [float]:      The maximum allowed % (in fractional terms) difference in medians between sub-windows
;   preflareSubWindowsMedianMeanDifferenceThreshold [float]: The maximum allowed difference between the median and mean within a sub-window. 
;   
; KEYWORD PARAMETERS:
;   VERBOSE: Set this to print processing messages to console
;
; OUTPUTS:
;   preflareIrradiance [float]: The best estimate for the pre-flare irradiance. This is a mean value over This is the return value of the function. 
;
; OPTIONAL OUTPUTS:
;   preflareTimeRangeJdOut [dblarr]:          2 eleemnt vector with the range of times used to compute the pre-flare irradiance.
;   preflareIndicesOut [intarr]:              The indices in the lightcurve that correspond to preflareTimeRangeJdOut.
;   preflareIrradianceUncertaintyOut [float]: The uncertainty corresponding to the preflareIrradiance
;   
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   preflareIrradiance = DeterminePreflareIrradiance(eveLines.line_irradiance[i], eveTimeJd, goesEventStartTimeJd, preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, $
;                                                    preflareTimeRangeJdOut = preflareTimeRangeJd, preflareIndicesOut = preflareIndices)
;
; MODIFICATION HISTORY:
;   2016-10-27: James Paul Mason: Wrote script.
;   2017-03-02: James Paul Mason: Added lineName required input
;   2017-03-30: James Paul Mason: Changed logic from checking standard deviation to checking median, standard deviation, and mean in sub-windows. 
;                                 As part of that, added preflareSubWindowMedianDifferenceThreshold and 
;                                 preflareSubWindowsMedianMeanDifferenceThreshold optional inputs. 
;-
FUNCTION DeterminePreflareIrradiance, lightCurve, timeJd, lineName, goesEventStartTimeJd, preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, preflareUncertaintiesLast24Hours, $
                                      thresholdTimePriorFlareMinutes = thresholdTimePriorFlareMinutes, preflareWindowStdDevThreshold = preflareWindowStdDevThreshold, $
                                      preflareSubWindowMedianDifferenceThreshold = preflareSubWindowMedianDifferenceThreshold, $
                                      preflareSubWindowsMedianMeanDifferenceThreshold = preflareSubWindowsMedianMeanDifferenceThreshold, $ 
                                      preflareTimeRangeJdOut = preflareTimeRangeJdOut, preflareIndicesOut = preflareIndicesOut, $
                                      preflareIrradianceUncertaintyOut = preflareUncertaintyUncertaintyOut, $
                                      VERBOSE = VERBOSE
; Input checks
IF preflareTimesLast24HoursJd.IsEmpty() EQ 0 THEN BEGIN
  IF size(preflareTimesLast24HoursJd[0], /DIMENSIONS) NE 2 THEN BEGIN
    message, /INFO, JPMsystime() + ' preflareTimesLast24HoursJd input should be a [2, N] array'
    STOP
  ENDIF
ENDIF

; Defaults
IF thresholdTimePriorFlareMinutes EQ !NULL THEN thresholdTimePriorFlareMinutes = 240.
IF preflareWindowStdDevThreshold EQ !NULL THEN preflareWindowStdDevThreshold = 0.02 ; TODO: Figure out if this is a sensible number
IF preflareSubWindowMedianDifferenceThreshold EQ !NULL THEN preflareSubWindowMedianDifferenceThreshold = 0.02 ; [% in fractional terms] TODO: Figure out if this is a sensible number 
IF preflareSubWindowsMedianMeanDifferenceThreshold EQ !NULL THEN preflareSubWindowsMedianMeanDifferenceThreshold = 0.20 ; [% in fractional terms] TODO: Figure out if this is a sensible number
IF preflareTimesLast24HoursJd.IsEmpty() THEN isEventInLast24Hours = 0 ELSE isEventInLast24Hours = 1

; Setup
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Analysis/Coronal Dimming/Automatic Dimming Database/Processing Plots/Pre-flare/'

;;
; Handle case where prior events occurred recently
;; 

IF isEventInLast24Hours THEN BEGIN 
  
  ; Compute the mean time of the prior events 
  meanTimeEventsLast24HoursJd = mean(preflareTimesLast24HoursJd, DIMENSION = 1)
  
  eventTimesMinutesPriorToPresentEvent = (goesEventStartTimeJd - meanTimeEventsLast24HoursJd) * 24. * 60.
  nearbyEventIndices = where(eventTimesMinutesPriorToPresentEvent LT thresholdTimePriorFlareMinutes, numberNearbyEvents)
  IF numberNearbyEvents NE 0 THEN BEGIN
    isPriorEvent = 1
      
    ; If there's no event prior to the window (e.g., within 4 hours) within the last 24 hours of events, 
    ; then can assume that the irradiance of the earliest event in window is good
    ; else there is -- see embedded conditionals in that case
    IF nearbyEventIndices[0] EQ 0 THEN BEGIN
      presentPreflareTimeJd = preflareTimesLast24HoursJd[*, nearbyEventIndices[0]]
      presentPreflareIrradiance = preflareIrradiancesLast24Hours[nearbyEventIndices[0]]
      presentPreflareUncertainty = preflareUncertaintiesLast24Hours[nearbyEventIndices[0]]
      IF keyword_set(VERBOSE) THEN $
        message, /INFO, JPMsystime() + ' Event at ' + JPMjd2iso(goesEventStartTimeJd, /NO_T_OR_Z) + $ 
                        ' has other events within ' + strtrim(thresholdTimePriorFlareMinutes, 2) + ' minutes.' + $ 
                        ' Using same pre-flare irradiance.'
    ENDIF ELSE BEGIN  
      ; If the two bounding events have identical irradiances then set presentPreflareIrradiance to NAN 
      ; else set presentPreflareIrradiance to the irradiance of the earliest event in the window
      IF preflareIrradiancesLast24Hours[nearbyEventIndices[0]] EQ preflareIrradiancesLast24Hours[nearbyEventIndices[0] - 1] THEN BEGIN
        presentPreflareTimeJd = !VALUES.D_NAN
        presentPreflareIrradiance = !VALUES.F_NAN
        presentPreflareUncertainty = !VALUES.F_NAN
        IF keyword_set(VERBOSE) THEN $ 
          message, /INFO, JPMsystime() + ' Event at ' + JPMjd2iso(goesEventStartTimeJd, /NO_T_OR_Z) + $
                          ' has other events within ' + strtrim(thresholdTimePriorFlareMinutes, 2) + ' mintues' + $ 
                          ' but pre-flare irradiance is stale (NAN).' 
      ENDIF ELSE BEGIN
        presentPreflareTimeJd = preflareTimesLast24HoursJd[*, nearbyEventIndices[0]]
        presentPreflareIrradiance = preflareIrradiancesLast24Hours[nearbyEventIndices[0]]
        presentPreflareUncertainty = preflareUncertaintiesLast24Hours[nearbyEventIndices[0]]
        IF keyword_set(VERBOSE) THEN $ 
          message, /INFO, JPMsystime() + ' Event at ' + JPMjd2iso(goesEventStartTimeJd, /NO_T_OR_Z) + $
                          ' has other events within ' + strtrim(thresholdTimePriorFlareMinutes, 2) + ' minutes.' + $ 
                          ' Using same pre-flare irradiance.'
      ENDELSE ; The events bounding the search window (e.g., 4 hours) were not identical
    ENDELSE ; There was an event < 24 hours ago but > threshold hours ago (e.g, 4 hours ago)
  ENDIF
  
  ; Don't need the rest of the code since pre-flare irradiance has now been determined
  preflareTimeRangeJdOut = presentPreflareTimeJd
  IF finite(presentPreflareIrradiance) EQ 0 THEN preflareIndicesOut = [-1, -1] ELSE $
                                                 preflareIndicesOut = [closest(presentPreflareTimeJd[0], timeJd, /LOWER), closest(presentPreflareTimeJd[1], timeJd, /UPPER)]
  preflareUncertaintyUncertaintyOut = presentPreflareUncertainty
  return, presentPreflareIrradiance
ENDIF ; isEventInLast24Hours = 1

;;
; Pre-flare determination method when no interference from prior events
;;

; Restrict the irradiance to just times within thresholdTimePriorFlareMinutes and goesEventStartTimeJd
medianWindowJd = [goesEventStartTimeJd - (thresholdTimePriorFlareMinutes / 1440.), goesEventStartTimeJd]
medianWindowIndices = where(timeJd GT medianWindowJd[0] AND timeJd LT medianWindowJd[1])
IF medianWindowIndices EQ [-1] THEN BEGIN
  presentPreflareTimeJd = !VALUES.D_NAN
  presentPreflareIrradiance = !VALUES.F_NAN
  presentPreflareUncertainty = !VALUES.F_NAN
  
  IF keyword_set(VERBOSE) THEN BEGIN
    message, /INFO, JPMsystime() + ': No irradiance measurements for pre-flare time window' + $
                                   JPMjd2iso(medianWindowJd[0], /NO_T_OR_Z) + ' to ' + JPMjd2iso(medianWindowJd[1], /NO_T_OR_Z)
  ENDIF
ENDIF ELSE BEGIN
  noGoodPreflareYet = 1
  WHILE noGoodPreflareYet DO BEGIN
  
  
  lightCurvePreflareWindow = lightCurve[medianWindowIndices]
  
  ; Break into 3 sub-windows
  n_elementsInSubWindow = n_elements(lightCurvePreflareWindow) / 3
  median1 = median(lightCurvePreflareWindow[0 : n_elementsInSubWindow])
  median2 = median(lightCurvePreflareWindow[n_elementsInSubWindow + 1: 2 * n_elementsInSubWindow])
  median3 = median(lightCurvePreflareWindow[2 * n_elementsInSubWindow + 1 : -1])
  
  
  ENDWHILE ; noGoodPreflareYet
  
  ; Check the standard deviation of lightCurveMedianWindow and iteritavely reduce the window size until the standard deviation falls below preflareWindowStdDevThreshold
  presentStdDev = stddev(lightCurveMedianWindow)
  preflareWindowMinutes = thresholdTimePriorFlareMinutes
  WHILE presentStdDev GT preflareWindowStdDevThreshold AND preflareWindowMinutes GT 0 DO BEGIN
    preflareWindowMinutes -= 60.
    medianWindowIndices = where(timeJd GT goesEventStartTimeJd - (preflareWindowMinutes / 1440.) AND timeJd LT goesEventStartTimeJd)
    lightCurveMedianWindow = lightCurve[medianWindowIndices]
    presentStdDev = stddev(lightCurveMedianWindow)
  ENDWHILE

  ; If the lightCurveMedianWindow standard deviation never fell below the threshold, then no good pre-flare irradiance can be computed. Set to NAN.
  ; Else calculate the median of lightCurveMedianWindow and its uncertainty
  IF preflareWindowMinutes EQ 0 THEN BEGIN
    presentPreflareTimeJd = [!VALUES.D_NAN, !VALUES.D_NAN]
    presentPreflareIrradiance = !VALUES.F_NAN
    presentPreflareUncertainty = !VALUES.F_NAN

    IF keyword_set(VERBOSE) THEN BEGIN
      message, /INFO, JPMsystime() + ' No good pre-flare window could be found for event at ' + JPMjd2iso(goesEventStartTimeJd, /NO_T_OR_Z)
    ENDIF
  ENDIF ELSE BEGIN
    presentPreflareTimeJd = [timeJd[medianWindowIndices[0]], timeJd[medianWindowIndices[-1]]]
    presentPreflareIrradiance = median(lightCurveMedianWindow)
    presentPreflareUncertainty = 1.25 * stddev(lightCurveMedianWindow) / sqrt(n_elements(lightCurveMedianWindow))
  ENDELSE
ENDELSE ; found a median window in irradiance data

;;
; Produce and save annotated plot for sanity checks
;;

relativeTimeHours = (timeJd - goesEventStartTimeJd) * 24.
fixedYRangeAroundMedian = 0.15 * presentPreflareIrradiance ; ± this value for full range

IF ~isEventInLast24Hours THEN BEGIN
  preflareSelectedTimesIndices = where(timeJd GE presentPreflareTimeJd[0] AND timeJd LE presentPreflareTimeJd[1])
  ; New pre-flare irradiance determination 
  p1 = plot(relativeTimeHours, lightCurve * 1e6, '2', /BUFFER, $ 
            TITLE = 'Event ' + JPMjd2iso(goesEventStartTimeJd, /NO_T_OR_Z) + ' ' + lineName + ' Å', $
            XTITLE = 'Time relative to GOES event [hours]', $
            YTITLE = 'Irradiance [$\mu$W m$^{-1}$ nm$^{-1}$]', YRANGE = [presentPreflareIrradiance - fixedYRangeAroundMedian, presentPreflareIrradiance + fixedYRangeAroundMedian] * 1e6, $
            NAME = 'EVE irradiance')
  p2 = plot(relativeTimeHours[preflareSelectedTimesIndices], lightCurve[preflareSelectedTimesIndices] * 1e6, '6', COLOR = 'dodger blue', TRANSPARENCY = 70, /OVERPLOT, $
            NAME = 'Used as pre-flare')
  p3 = plot(p1.xrange, [presentPreflareIrradiance, presentPreflareIrradiance] * 1e6, '2--', COLOR = 'dark orange', /OVERPLOT, $
            NAME = 'Median')
  l1 = legend(TARGET = [p1, p2, p3], POSITION = [0.89, 0.85])
  t1 = text(0.88, 0.66, 'Pre-flare Irradiance = ' + JPMPrintNumber(presentPreflareIrradiance, /SCIENTIFIC_NOTATION), ALIGNMENT = 1, COLOR = 'dark orange')
  IF presentStdDev EQ !NULL THEN BEGIN
    t2 = text(0.88, 0.63, 'Values propagated from prior event', ALIGNMENT = 1)
  ENDIF ELSE BEGIN
    t2 = text(0.88, 0.63, '$\sigma / \mu$ = ' + JPMPrintNumber(presentStdDev / presentPreflareIrradiance, /SCIENTIFIC_NOTATION), ALIGNMENT = 1, COLOR = 'dodger blue')
  ENDELSE
  p1.save, saveloc + 'Event ' + 'at ' + JPMjd2iso(goesEventStartTimeJd, /NO_T_OR_Z) + ' ' + lineName + ' Å.png'
ENDIF ELSE BEGIN
  ; Propagated pre-flare irradiance determination
  p4 = plot(relativeTimeHours, lightCurve * 1e6, '2', $
            TITLE = 'Event ' + JPMjd2iso(goesEventStartTimeJd, /NO_T_OR_Z) + ' ' + lineName + ' Å', $
            XTITLE = 'Time relative to GOES event [hours]', $
            YTITLE = 'Irradiance [$\mu$W m$^{-1}$ nm$^{-1}$]', $
            NAME = 'EVE irradiance')
  p5 = plot(p1.xrange, [presentPreflareIrradiance, presentPreflareIrradiance] * 1e6, $
            NAME = 'Median')
ENDELSE

;;
; Setup return values 
;;

preflareTimeRangeJdOut = presentPreflareTimeJd
IF finite(presentPreflareIrradiance) EQ 0 THEN preflareIndicesOut = [-1, -1] ELSE $
                                               preflareIndicesOut = [closest(presentPreflareTimeJd[0], timeJd, /LOWER), closest(presentPreflareTimeJd[1], timeJd, /UPPER)]
preflareUncertaintyUncertaintyOut = presentPreflareUncertainty

return, presentPreflareIrradiance

END