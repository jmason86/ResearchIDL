;+
; NAME:
;   FitCoronalDimmingLightCurve
;
; PURPOSE:
;   Compare various fits to EVE dimming light curves.
;
; INPUTS:
;   None. 
;
; OPTIONAL INPUTS:
;   eventNumber [int]: The event number to run for. Can alternatively edit hardcode to set eventNumber. 
;
; KEYWORD PARAMETERS:
;   NO_PLOTS:                       Set this to skip the generation of new plots
;   SKIP_BAD_EVENTS:                Set this to skip events that are effective or literal duplicates, or flagged as horrible in EVE
;   MANUAL_SELECT_PARAMETERIZATION: Set this to not buffer the plot and stop after its displayed so that the times to use for depth and slope calculations can be determined. 
;
; OUTPUTS:
;   Plot showing EVE data with error bars (hard coded uncertainty) and various fits to the data with their reduced chiSquared values
;
; OPTIONAL OUTPUTS:
;   bestFitOut [string]:   A string with the name of the best fit by reduced chi squared. Can be either Exponential, Parabola, 5th Order Polynomial, or Linear
;   bestFitChiOut [float]: The value of the best fit reduced chi squared
;
; RESTRICTIONS:
;   Requires JPMPrintNumber.pro and closest.pro
;   EveCoreDimmingCorrection must already have been run. Intended for use with events in the 2-2 week study period. 
;
; EXAMPLE:
;   FitCoronalDimmingLightCurve, eventNumber = 12, /SKIP_BAD_EVENTS
;
; MODIFICATION HISTORY:
;   2014/12/31: James Paul Mason: Wrote procedure
;   2015/02/12: James Paul Mason: Changed procedure to restore data from the Corrected/Event#/WarmCorrection save files. Added hard-coded time ranges for each event.
;   2015/06/01: James Paul Mason: Added MANUAL_SELECT_PARAMETERIZATION keyword and corresponding code, including the hard-code results. 
;-
PRO FitCoronalDimmingLightCurve, eventNumber = eventNumber, $ 
                                 NO_PLOTS = NO_PLOTS, SKIP_BAD_EVENTS = SKIP_BAD_EVENTS, MANUAL_SELECT_PARAMETERIZATION = MANUAL_SELECT_PARAMETERIZATION, $
                                 bestFitOut = bestFit, bestFitChiOut = bestFitChi

; Hard-code input
IF ~keyword_set(eventNumber) THEN eventNumber = 28

IF keyword_set(SKIP_BAD_EVENTS) THEN $
  IF eventNumber EQ 15 OR eventNumber EQ 20 OR eventNumber EQ 23 OR eventNumber EQ 24 OR eventNumber EQ 25 OR eventNumber EQ 27 OR eventNumber EQ 29 OR eventNumber EQ 30 OR eventNumber EQ 32 THEN return

; Setup
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/'
savename = 'Event' + JPMPrintNumber(eventNumber) + ' 171 Å Fit'

; Restore corrected dimming data
eventName = 'Event' + JPMPrintNumber(eventNumber)
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/' + eventName + '/Warm correction/EVEScaledIrradiances.sav'
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/' + eventName + '/Warm correction/ExtrapolatedPreFlareTrend.sav'

; Restore measurement errors
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/' + eventName + '/Warm correction/UncertaintiesCorrectedEVEDimmingCurves.sav'

; Convert time
sod = (eveTimeJD - floor(eveTimeJD[0]) - 0.5) * 86400.

; Identify the indices of 171, 177, 180, 195, and 202 all corrected by 284 - Only using 171 for now
indicesOfInterest = [3, 8, 13, 18, 23]
intensity = correctedEVEDimmingCurves[*, indicesOfInterest[0]] ; 171 Å corrected by 284 Å
intensityError = uncertaintiesCorrectedEVEDimmingCurves[*, indicesOfInterest[0]] ; 171 Å corrected by 284 Å
IF eventNumber EQ 27 THEN BEGIN 
  intensity = correctedEVEDimmingCurves[*, 2] ; 171 Å corrected by 211 Å
  intensityError = uncertaintiesCorrectedEVEDimmingCurves[*, 2]
ENDIF
IF eventNumber EQ 28 THEN BEGIN 
  intensity = correctedEVEDimmingCurves[*, 0] ; 171 Å corrected by 94 Å
  intensityError = uncertaintiesCorrectedEVEDimmingCurves[*, 0]
ENDIF
; Ignore NANs if they exist
intensityError = intensityError[where(finite(intensity))]
sod = sod[where(finite(intensity))]
intensity = intensity[where(finite(intensity))]

; Determine the indices for the sod range (only required once)
;p = plot(sod, intensity) 
;STOP

; Hard-code input
CASE eventNumber OF ; sodSubsetRangeIndices is for zooming in on just the dimming instead of trying to fit the whole day
   1: BEGIN ; Event 1 2011041_24730
    sodSubsetRangeIndices = where(sod GE 25812 AND sod LE 4E4) 
    depthTimeSod = 8.57 * 3600
    slopeTimesSod = [7.503 * 3600, 8.07 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
   END
   2: BEGIN ; Event 2 2011041_44170
    sodSubsetRangeIndices = where(sod GE 51624 AND sod LE 54060)
    depthTimeSod = 14.8 * 3600
    slopeTimesSod = [14.37 * 3600, 14.6 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
   END
   3: BEGIN ; Event 3 2011042_21010
    sodSubsetRangeIndices = where(sod GE 23650 AND sod LE 32890)
    depthTimeSod = 8.303 * 3600
    slopeTimesSod = [6.97 * 3600, 7.837 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
   END
   4: BEGIN ; Event 4 2011042_45490
    sodSubsetRangeIndices = where(sod GE 50040 AND sod LE 56350)
    depthTimeSod = 15.27 * 3600
    slopeTimesSod = [14.3 * 3600, 14.9 * 3600]
    fitToUseForDepth = '4th Order Poly'
    fitToUseForSlope = '3rd Order Poly'
   END
   5: BEGIN  ; Event 5 2011042_72250
    sodSubsetRangeIndices = where(sod GE 75612 AND sod LE 81144)
    depthTimeSod = 21.9 * 3600
    slopeTimesSod = [21.27 * 3600, 21.7 * 3600]
    fitToUseForDepth = '4th Order Poly'
    fitToUseForSlope = '5th Order Poly'
   END
   6: BEGIN ; Event 6 2011043_12490
    sodSubsetRangeIndices = where(sod GE 11530 AND sod LE 19690)
    depthTimeSod = 5.037 * 3600
    slopeTimesSod = [3.57 * 3600, 4.703 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
   END
   7: BEGIN ; Event 7 2011044_52450
    sodSubsetRangeIndices = where(sod GE 61940 AND sod LE 68420)
    depthTimeSod = 18.87 * 3600
    slopeTimesSod = [17.5 * 3600, 18.54 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
   END
   8: BEGIN ; Event 8 2011045_56530
    sodSubsetRangeIndices = where(sod GE 42850 AND sod LE 72250)
    depthTimeSod = 19.17 * 3600
    slopeTimesSod = [13.17 * 3600, 16.74 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
   END
   9: BEGIN ; Event 9 2011046_1693
    sodSubsetRangeIndices = where(sod GE 1332  AND sod LE 11412)
    depthTimeSod = 2.937 * 3600
    slopeTimesSod = [0.9702 * 3600, 2.704 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '3rd Order Poly'
  END
  10: BEGIN ; Event 10 20111047_50530
    sodSubsetRangeIndices = where(sod GE 52450 AND sod LE 63730)
    depthTimeSod = 17.1 * 3600
    slopeTimesSod = [14.87 * 3600, 15.5 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
  END
  11: BEGIN ; Event 11 2011048_613
    sodSubsetRangeIndices = where(sod GE 0     AND sod LE 9010)
    depthTimeSod = 1.704 * 3600
    slopeTimesSod = [0.437 * 3600, 1.37 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  12: BEGIN ; Event 12 2011049_38530
    sodSubsetRangeIndices = where(sod GE 39600 AND sod LE 48492)
    depthTimeSod = 12.97 * 3600
    slopeTimesSod = [11.64 * 3600, 12.4 * 3600]
    fitToUseForDepth = '4th Order Poly'
    fitToUseForSlope = '4th Order Poly'
  END
  13: BEGIN ; Event 13 2011049_60850
    sodSubsetRangeIndices = where(sod GE 60250 AND sod LE 81370)
    depthTimeSod = 22.24 * 3600
    slopeTimesSod = [16.87 * 3600, 21.87 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  14: BEGIN ; Event 14 2011055_26770
    sodSubsetRangeIndices = where(sod GE 25330 AND sod LE 36850)
    depthTimeSod = 8.835 * 3600
    slopeTimesSod = [7.568 * 3600, 8.368 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  15: sodSubsetRangeIndices = where(sod GE 25330 AND sod LE 38170) ; Event 15 2011055_24490 - Effectively duplicate of Event 14
  16: BEGIN ; Event 16 2011214_16210
    sodSubsetRangeIndices = where(sod GE 25200 AND sod LE 39250)
    depthTimeSod = 10.6 * 3600
    slopeTimesSod = [7.668 * 3600, 10.17 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '3rd Order Poly'
  END
  17: BEGIN ; Event 17 2011214_40570
    sodSubsetRangeIndices = where(sod GE 41650 AND sod LE 47770)
    depthTimeSod = 12.47 * 3600
    slopeTimesSod = [11.87 * 3600, 12.27 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  18: BEGIN ; Event 18 2011215_46690
    sodSubsetRangeIndices = where(sod GE 48370 AND sod LE 56290)
    depthTimeSod = 15.1 * 3600
    slopeTimesSod = [13.73 * 3600, 14.5 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
  END
  19: BEGIN ; Event 19 2011216_9246
    sodSubsetRangeIndices = where(sod GE 10210 AND sod LE 26170)
    depthTimeSod = 6.068 * 3600
    slopeTimesSod = [3.535 * 3600, 4.768 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  20: sodSubsetRangeIndices = where(sod GE 10210 AND sod LE 26170) ; Event 20 2011216_9246 - Literally duplicate of Event 19
  21: BEGIN ; Event 21 2011217_22330
    sodSubsetRangeIndices = where(sod GE 25570 AND sod LE 33966)
    depthTimeSod = 9.202 * 3600
    slopeTimesSod = [7.568 * 3600, 8.935 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  22: BEGIN ; Event 22 2011218_36130
    sodSubsetRangeIndices = where(sod GE 39130 AND sod LE 48970)
    depthTimeSod = 12.87 * 3600
    slopeTimesSod = [11.04 * 3600, 12.27 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
  END
  23: sodSubsetRangeIndices = where(sod GE 57730 AND sod LE 62410) ; Event 23 2011218_60370 - Horrible event
  24: sodSubsetRangeIndices = where(sod GE 68050 AND sod LE 69850) ; Event 24 2011218_69010 - Horrible event
  25: BEGIN ; Event 25 2011218_76810
    sodSubsetRangeIndices = where(sod GE 75850 AND sod LE 86400) 
    depthTimeSod = 23.8 * 3600
    slopeTimesSod = [21.8 * 3600, 23.74 * 3600]
    fitToUseForDepth = '4th Order Poly'
    fitToUseForSlope = '4th Order Poly'
  END
  26: BEGIN ; Event 26 2011219_5406
    sodSubsetRangeIndices = where(sod GE 11044 AND sod LE 16567)
    depthTimeSod = 4.302 * 3600
    slopeTimesSod = [3.202 * 3600, 3.868 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
  END
  27: sodSubsetRangeIndices = where(sod GE 8647  AND sod LE 14530) ; Event 27 2011220_966 - Correction using 211 Å instead of 284 Å
  28: BEGIN ; Event 28 2011220_20250
    sodSubsetRangeIndices = where(sod GE 20250 AND sod LE 36972)
    depthTimeSod = 10.2 * 3600
    slopeTimesSod = [6.2 * 3600, 8.9 * 3600]
    fitToUseForDepth = 'Parabola'
    fitToUseForSlope = 'Parabola'
  END
  29: sodSubsetRangeIndices = where(sod GE 67090 AND sod LE 75010) ; Event 29 2011220_62050 - Correction looks like it went awry
  30: sodSubsetRangeIndices = where(sod GE 61450 AND sod LE 75490) ; Event 30 2011220_62050 - Effectively duplicate of Event 29
  31: BEGIN ; Event 31 2011221_22570
    sodSubsetRangeIndices = where(sod GE 27250 AND sod LE 34450)
    depthTimeSod = 9.102 * 3600
    slopeTimesSod = [7.935 * 3600, 8.569 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  32: sodSubsetRangeIndices = where(sod GE 26770 AND sod LE 34450) ; Event 32 2011221_22570 - Literally duplicate of Event 31
  33: BEGIN ; Event 33 2011221_35530
    sodSubsetRangeIndices = where(sod GE 36250 AND sod LE 52330)
    depthTimeSod = 13.14 * 3600
    slopeTimesSod = [10.67 * 3600, 12.27 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  34: BEGIN ; Event 34 2011223_32410
    sodSubsetRangeIndices = where(sod GE 35050 AND sod LE 43930)
    depthTimeSod = 10.77 * 3600
    slopeTimesSod = [10.04 * 3600, 10.6 * 3600]
    fitToUseForDepth = '4th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
  35: BEGIN ; Event 35 2011224_25690
    sodSubsetRangeIndices = where(sod GE 20170 AND sod LE 35528)
    depthTimeSod = 9.369 * 3600
    slopeTimesSod = [6.902 * 3600, 8.835 * 3600]
    fitToUseForDepth = '3rd Order Poly'
    fitToUseForSlope = '3rd Order Poly'
  END
  36: BEGIN ; Event 36 2011224_36490
    sodSubsetRangeIndices = where(sod GE 23530 AND sod LE 41290)
    depthTimeSod = 11.37 * 3600
    slopeTimesSod = [6.935 * 3600, 10.84 * 3600]
    fitToUseForDepth = '5th Order Poly'
    fitToUseForSlope = '5th Order Poly'
  END
ENDCASE

;sodSubsetRangeIndices = where(sod GE 0.75E4 AND sod LE 2E4) ; Test fit data
sod = sod[sodSubsetRangeIndices]
intensity = intensity[sodSubsetRangeIndices]
intensityError = intensityError[sodSubsetRangeIndices]

; Shift time for fitting exponential - needs to start at time = 0 
;shiftedTime = sod - sod[0]

; Exponential Fit
; FIXME: If using this fit need to do two things: 
;        1. Figure out how to input uncertainties
;        2. Make a two segment exponential: one piece for prior-during flare, another for the dimming
;fitParameters = expfit(shiftedTime, intensity, resultantSigma, expChiSq)
;a = fitParameters[0]
;b = fitParameters[1]
;c = fitParameters[2]
;fitCurve = a * exp(b * shiftedTime) + c
;expTroughIntensity = min(fitCurve, troughIndex)
;expTroughSod = sod[troughIndex]

; 2nd Order Poly Fit (Parabola)
parabolaParameters = poly_fit(sod, intensity, 2, parabolaCurve, CHISQ = parabolaChi, MEASURE_ERRORS = intensityError, YBAND = parabolaError)
parabolaReducedChi = parabolaChi / (n_elements(sod) - 2.)
parabolaSlope = deriv(sod / 3600, parabolaCurve)
troughIndex = closest(0., parabolaSlope) 
parabolaTroughSod = sod[troughIndex]
parabolaTroughIntensity = parabolaCurve[troughIndex]

; 3rd Order Poly Fit
poly3Parameters = poly_fit(sod, intensity, 3, poly3Curve, CHISQ = poly3Chi, MEASURE_ERRORS = intensityError, YBAND = poly3Error)
poly3ReducedChi = poly3Chi / (n_elements(sod) - 5. - 1.)
poly3Slope = deriv(sod / 3600, poly3Curve)
troughIndex = closest(0., poly3Slope)
poly3TroughSod = sod[troughIndex]
poly3TroughIntensity = poly3Curve[troughIndex]

; 4th Order Poly Fit
poly4Parameters = poly_fit(sod, intensity, 4, poly4Curve, CHISQ = poly4Chi, MEASURE_ERRORS = intensityError, YBAND = poly4Error)
poly4ReducedChi = poly4Chi / (n_elements(sod) - 5. - 1.)
poly4Slope = deriv(sod / 3600, poly4Curve)
troughIndex = closest(0., poly4Slope)
poly4TroughSod = sod[troughIndex]
poly4TroughIntensity = poly4Curve[troughIndex]

; 5th Order Poly Fit
poly5Parameters = poly_fit(sod, intensity, 5, poly5Curve, CHISQ = poly5Chi, MEASURE_ERRORS = intensityError, YBAND = poly5Error, STATUS = status)
poly5ReducedChi = poly5Chi / (n_elements(sod) - 5. - 1.)
poly5Slope = deriv(sod / 3600, poly5Curve)
troughIndex = closest(0., poly5Slope)
poly5TroughSod = sod[troughIndex]
poly5TroughIntensity = poly5Curve[troughIndex]

; Determine which reduced chi squared was the best fit and prepare to make it bold in plot and make the arrow point there
allReducedChis = [parabolaReducedChi, poly3ReducedChi, poly4ReducedChi, poly5ReducedChi]
bestChiIndex = closest(1.0, allReducedChis)
bestChiBoolArray = intarr(n_elements(allReducedChis))
FOR i = 0, n_elements(allReducedChis) - 1 DO BEGIN
  IF i EQ bestChiIndex THEN bestChiBoolArray[i] = 1 ELSE $
                            bestChiBoolArray[i] = 0
ENDFOR
;CASE bestChiIndex OF
;  0: BEGIN
;    depthTimeSod = parabolaTroughSod
;    troughIntensity = parabolaTroughIntensity
;    arrowColor = 'red'
;    bestFit = 'Parabola'
;    bestFitChi = parabolaReducedChi
;    fitToUseForDepth = 'Parabola'
;    fitToUseForSlope = 'Parabola'
;  END
;  1: BEGIN
;    depthTimeSod = poly3TroughSod
;    troughIntensity = poly3TroughIntensity
;    arrowColor = 'green'
;    bestFit = '3rd Order Polynomial'
;    bestFitChi = poly3ReducedChi
;    fitToUseForDepth = '3rd Order Poly'
;    fitToUseForSlope = '3rd Order Poly'
;  END
;  2: BEGIN
;    depthTimeSod = poly4TroughSod
;    troughIntensity = poly4TroughIntensity
;    arrowColor = 'blue'
;    bestFit = '4th Order Polynomial'
;    bestFitChi = poly4ReducedChi
;    fitToUseForDepth = '4th Order Poly'
;    fitToUseForSlope = '4th Order Poly'
;  END
;  3: BEGIN
;    depthTimeSod = poly5TroughSod
;    troughIntensity = poly5TroughIntensity
;    arrowColor = 'orange'
;    bestFit = '5th Order Polynomial'
;    bestFitChi = poly5ReducedChi
;    fitToUseForDepth = '5th Order Poly'
;    fitToUseForSlope = '5th Order Poly'
;  END
;ENDCASE

; Ignore the above section and use the manually selected values to determine dimming parameterization, including which fit is best

IF fitToUseForDepth EQ 'Parabola' THEN BEGIN
  depthIntensity = parabolaCurve[closest(depthTimeSod, sod, /UPPER)]
  
  ; Extrapolate pre-flare trend
  extrapolatedPreFlareTrendCorrected = correctedPreFlareTrendFits[1, 3] * depthTimeSod + correctedPreFlareTrendFits[0, 3] ; y = ax + b
  extrapolatedPreFlareTrend = fitParameters171[1] * depthTimeSod + fitParameters171[0] ; y = ax + b
  depthIntensityExtrapolatedCorrected = depthIntensity - extrapolatedPreFlareTrendCorrected
  depthIntensityExtrapolated = depthIntensity - extrapolatedPreFlareTrend
  arrowColor = 'red'
ENDIF
IF fitToUseForSlope EQ 'Parabola' THEN BEGIN
  slopePointLeft = parabolaCurve[closest(slopeTimesSod[0], sod, /UPPER)]
  slopePointRight = parabolaCurve[closest(slopeTimesSod[1], sod, /UPPER)]
  slopeArray = parabolaSlope[closest(slopeTimesSod[0], sod, /UPPER) : closest(slopeTimesSod[1], sod, /UPPER)]
  slope = mean(slopeArray)
  slopeStandardDeviation = stddev(slopeArray)
  circleColor = 'red'
ENDIF

IF fitToUseForDepth EQ '3rd Order Poly' THEN BEGIN
  depthIntensity = poly3Curve[closest(depthTimeSod, sod, /UPPER)]
  
  ; Extrapolate pre-flare trend
  extrapolatedPreFlareTrendCorrected = correctedPreFlareTrendFits[1, 3] * depthTimeSod + correctedPreFlareTrendFits[0, 3] ; y = ax + b
  extrapolatedPreFlareTrend = fitParameters171[1] * depthTimeSod + fitParameters171[0] ; y = ax + b
  depthIntensityExtrapolatedCorrected = depthIntensity - extrapolatedPreFlareTrendCorrected
  depthIntensityExtrapolated = depthIntensity - extrapolatedPreFlareTrend
  arrowColor = 'green'
ENDIF
IF fitToUseForSlope EQ '3rd Order Poly' THEN BEGIN
  slopePointLeft = poly3Curve[closest(slopeTimesSod[0], sod, /UPPER)]
  slopePointRight = poly3Curve[closest(slopeTimesSod[1], sod, /UPPER)]
  slopeArray = poly3Slope[closest(slopeTimesSod[0], sod, /UPPER) : closest(slopeTimesSod[1], sod, /UPPER)]
  slope = mean(slopeArray)
  slopeStandardDeviation = stddev(slopeArray)
  circleColor = 'green'
ENDIF

IF fitToUseForDepth EQ '4th Order Poly' THEN BEGIN
  depthIntensity = poly4Curve[closest(depthTimeSod, sod, /UPPER)]
  
  ; Extrapolate pre-flare trend
  extrapolatedPreFlareTrendCorrected = correctedPreFlareTrendFits[1, 3] * depthTimeSod + correctedPreFlareTrendFits[0, 3] ; y = ax + b
  extrapolatedPreFlareTrend = fitParameters171[1] * depthTimeSod + fitParameters171[0] ; y = ax + b
  depthIntensityExtrapolatedCorrected = depthIntensity - extrapolatedPreFlareTrendCorrected
  depthIntensityExtrapolated = depthIntensity - extrapolatedPreFlareTrend
  arrowColor = 'blue'
ENDIF
IF fitToUseForSlope EQ '4th Order Poly' THEN BEGIN
  slopePointLeft = poly4Curve[closest(slopeTimesSod[0], sod, /UPPER)]
  slopePointRight = poly4Curve[closest(slopeTimesSod[1], sod, /UPPER)]
  slopeArray = poly4Slope[closest(slopeTimesSod[0], sod, /UPPER) : closest(slopeTimesSod[1], sod, /UPPER)]
  slope = mean(slopeArray)
  slopeStandardDeviation = stddev(slopeArray)
  circleColor = 'blue'
ENDIF

IF fitToUseForDepth EQ '5th Order Poly' THEN BEGIN
  depthIntensity = poly5Curve[closest(depthTimeSod, sod, /UPPER)]
  
  ; Extrapolate pre-flare trend
  extrapolatedPreFlareTrendCorrected = correctedPreFlareTrendFits[1, 3] * depthTimeSod + correctedPreFlareTrendFits[0, 3] ; y = ax + b
  extrapolatedPreFlareTrend = fitParameters171[1] * depthTimeSod + fitParameters171[0] ; y = ax + b
  depthIntensityExtrapolatedCorrected = depthIntensity - extrapolatedPreFlareTrendCorrected
  depthIntensityExtrapolated = depthIntensity - extrapolatedPreFlareTrend
  arrowColor = 'orange'
ENDIF
IF fitToUseForSlope EQ '5th Order Poly' THEN BEGIN
  slopePointLeft = poly5Curve[closest(slopeTimesSod[0], sod, /UPPER)]
  slopePointRight = poly5Curve[closest(slopeTimesSod[1], sod, /UPPER)]
  slopeArray = poly5Slope[closest(slopeTimesSod[0], sod, /UPPER) : closest(slopeTimesSod[1], sod, /UPPER)]
  slope = mean(slopeArray)
  slopeStandardDeviation = stddev(slopeArray)
  circleColor = 'orange'
ENDIF

IF ~keyword_set(NO_PLOTS) THEN BEGIN
  IF keyword_set(MANUAL_SELECT_PARAMETERIZATION) THEN useBuffer = 0 ELSE useBuffer = 1
  
  ; Error plot
  p1 = errorplot(sod/3600., intensity, intensityError, '2', BUFFER = useBuffer, $
                 TITLE = 'Event #' + JPMPrintNumber(eventNumber) + ' - ' + yyyymmdd + ' ' + hhmmss, $
                 XTITLE = 'Time [Hour]', $
                 YTITLE = 'Pre-flare Relative Intensity [%]', $
                 NAME = 'EVE')
  p2 = errorplot(sod/3600., parabolaCurve, parabolaError, 'r2', /OVERPLOT, ERRORBAR_COLOR = 'r', $
            YRANGE = p1.YRANGE, $
            NAME = 'Parabola')
  p3 = errorplot(sod/3600., poly3Curve, poly3Error, 'g2', /OVERPLOT, ERRORBAR_COLOR = 'g', $
            YRANGE = p1.YRANGE, $
            NAME = '3rd Order Poly')
  p4 = errorplot(sod/3600., poly4Curve, poly4Error, 'b2', /OVERPLOT, ERRORBAR_COLOR = 'b', $
            YRANGE = p1.YRANGE, $
            NAME = '4th Order Poly')
  p5 = errorplot(sod/3600., poly5Curve, poly5Error, COLOR = 'orange', '2', /OVERPLOT, ERRORBAR_COLOR = 'orange', $
            YRANGE = p1.YRANGE, $
            NAME = '5th Order Poly')
  a = arrow([depthTimeSod / 3600, depthTimeSod / 3600], [0., depthIntensity], /DATA, COLOR = arrowColor, THICK = 2)
  s = symbol(slopeTimesSod / 3600, [slopePointLeft, slopePointRight], /DATA, /SYM_FILLED, SYM_COLOR = circleColor, SYM_SIZE = 2, 'circle')
  t1 = text(0.90, 0.82, 'EVE 2 min ave', ALIGNMENT = 1)
  t2 = text(0.90, 0.78, 'Parabola $\chi^2$: '       + JPMPrintNumber(parabolaReducedChi), ALIGNMENT = 1, FONT_COLOR = 'red',    FONT_STYLE = bestChiBoolArray[0])
  t3 = text(0.90, 0.74, '3rd Order Poly $\chi^2$: ' + JPMPrintNumber(poly3ReducedChi),    ALIGNMENT = 1, FONT_COLOR = 'green',  FONT_STYLE = bestChiBoolArray[1])
  t4 = text(0.90, 0.70, '4th Order Poly $\chi^2$: ' + JPMPrintNumber(poly4ReducedChi),    ALIGNMENT = 1, FONT_COLOR = 'blue',   FONT_STYLE = bestChiBoolArray[2])
  t5 = text(0.90, 0.66, '5th Order Poly $\chi^2$: ' + JPMPrintNumber(poly5ReducedChi),    ALIGNMENT = 1, FONT_COLOR = 'orange', FONT_STYLE = bestChiBoolArray[3])
  t6 = text(0.15, 0.20, 'Dimming Depth = ' + JPMPrintNumber(depthIntensity) + '%' + ', ' + JPMPrintNumber(depthIntensityExtrapolatedCorrected) + '%, ' + $
                                             JPMPrintNumber(depthIntensityExtrapolated) + '%', FONT_COLOR = arrowColor, FONT_STYLE = 1)
  t7 = text(0.15, 0.16, 'Dimming Slope = ' + JPMPrintNumber(slope) + ' ± ' + JPMPrintNumber(slopeStandardDeviation) + '%/hour', FONT_COLOR = circleColor, FONT_STYLE = 1)  
  p1.save, saveloc + savename + '.png'
  save, FILENAME = saveloc + savename + '.sav'
ENDIF

END