;+
; NAME:
;   DimmingInEVEAIAPlot
;
; PURPOSE:
;   Messy code to create plots for ongoing analysis. 
;
; INPUTS:
;   
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;   FOR_DEAN: Set this to create a custom plot that Dean requested that shows 171 Å corrected by 284 Å
;             EVE 171, EVE 284 scaled and timeshifted, EVE 171 corrected, and AIA region dimming
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
; RESTRICTIONS:
;
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;  2013/07/29: James Paul Mason: Wrote code. 
;  2015/07/15: James Paul Mason: Added FOR_DEAN keyword to make a custom plot that Dean Pesnell requested. 
;-
PRO DimmingInEVEAIAPlot, FOR_DEAN = FOR_DEAN

; EDIT HERE
NoAIA = 0
doLightCurvesPlot = 1
; END EDITS

; Setup
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/2010219_07AUG_1824_M1.0/'
;saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/2011083_24MAR_1207_M1.0/'
IF NoAIA EQ 0 THEN restore, saveloc + 'LightCurveData.sav' ; AIA light curves
restore, saveloc + 'Warm correction/EVEScaledIrradiances.sav'
restore, saveloc + 'Warm correction/EVELines.sav'
IF keyword_set(FOR_DEAN) THEN restore, saveloc + 'Warm correction/EVEScaledBrightCurves.sav'

; No AIA data yet
IF NoAIA EQ 1 THEN BEGIN
  timeRange = minmax(eveTimeJD)
  timeStartIndex = closest(timeRange[0], eveTimeJD, /LOWER)
  eve171By211Minimum = min(correctedevedimmingcurves[*, 2], eveCorrectedMinIndex)
  eveCorrectedMinTime = eveTimeJD[eveCorrectedMinIndex]
  GOTO, SKIPAIA
ENDIF

; Get time range
timeRange = minmax(jd171) ;& timeRange[0] = timeRange[0] + 0.04 ; Scoot the zero time over to just prior to dimming
slopeCalculationStartPoint = 2455416.243056 ; 2010/08/07 17:50 UT
timeStartIndex = closest(timeRange[0], jd171, /LOWER)

; Get AIA totals for 171Å, 193Å and 211Å and convert to percent change
aia171 = total171
aia193 = total193
aia211 = total211
aiaper171 = perdiff(aia171, aia171[timeStartIndex])
aiaper193 = perdiff(aia193, aia193[timeStartIndex])
aiaper211 = perdiff(aia211, aia211[timeStartIndex])

; Compute total dimming from AIA (See AnalyzeAIADimmingRegions.pro)
aiaDimmingTotal171 = total(cutouts171[*, 0:1], 2) + total(cutouts171[*, 4:*], 2)
aiaDimmingTotal193 = total(cutouts193[*, 0:1], 2) + total(cutouts193[*, 4:*], 2)
aiaDimmingTotal211 = total(cutouts211[*, 0:1], 2) + total(cutouts211[*, 4:*], 2)
perDimmingTotal171 = perdiff(aiaDimmingTotal171, initial171Total, /RELATIVE)
perDimmingTotal193 = perdiff(aiaDimmingTotal193, initial193Total, /RELATIVE)
perDimmingTotal211 = perdiff(aiaDimmingTotal211, initial211Total, /RELATIVE)

; Store core dimming from AIA (See AnalyzeAIADimmingRegions.pro)
perCoreDimming171 = perdiff(cutouts171[*, 0], initial171Total, /RELATIVE)
perCoreDimming193 = perdiff(cutouts193[*, 0], initial193Total, /RELATIVE)
perCoreDimming211 = perdiff(cutouts211[*, 0], initial211Total, /RELATIVE)
;minAIACoreDimming171 = min(perCoreDimming171, minAIACoreDimming171Index)
;minAIACoreDimming171Time = jd171[minAIACoreDimming171Index]
minAIACoreDimming171Time = 2455416.3286266202d ; August 7, 2010 event
;minAIACoreDimming171Time = 2455777.7916666d ; August 4, 2011 event
minAIACoreDimming171 = perCoreDimming171[closest(minAIACoreDimming171Time, jd171, /LOWER)]
minAIACoreDimming193 = perCoreDimming193[closest(minAIACoreDimming171Time, jd193, /LOWER)]
minAIACoreDimming211 = perCoreDimming211[closest(minAIACoreDimming171Time, jd211, /LOWER)]
minAIAGlobalDimming171 = aiaper171[closest(minAIACoreDimming171Time, jd171, /LOWER)]
minAIAGlobalDimming193 = aiaper193[closest(minAIACoreDimming171Time, jd171, /LOWER)]
minAIAGlobalDimming211 = aiaper211[closest(minAIACoreDimming171Time, jd171, /LOWER)]

; Compute depth and slope for AIA global and core dimming
aiaCoreDimmingDepth171 = abs(minAIACoreDimming171)
aiaCoreDimmingDepth193 = abs(minAIACoreDimming193)
aiaCoreDimmingDepth211 = abs(minAIACoreDimming211)
aiaGlobalDimmingDepth171 = abs(minAIAGlobalDimming171)
aiaGlobalDimmingDepth193 = abs(minAIAGlobalDimming193)
aiaGlobalDimmingDepth211 = abs(minAIAGlobalDimming211)
aiaCoreDimmingSlope171 = abs(aiaCoreDimmingDepth171 / ((minAIACoreDimming171Time - slopeCalculationStartPoint[0]) * 24.)) ; Converted to /sec from /day
aiaCoreDimmingSlope193 = abs(aiaCoreDimmingDepth193 / ((minAIACoreDimming171Time - slopeCalculationStartPoint[0]) * 24.)) ; Converted to /sec from /day
aiaCoreDimmingSlope211 = abs(aiaCoreDimmingDepth211 / ((minAIACoreDimming171Time - slopeCalculationStartPoint[0]) * 24.)) ; Converted to /sec from /day
aiaGlobalDimmingSlope171 = abs(aiaGlobalDimmingDepth171 / ((minAIACoreDimming171Time - slopeCalculationStartPoint[0]) * 24.)) ; Converted to /sec from /day
aiaGlobalDimmingSlope193 = abs(aiaGlobalDimmingDepth193 / ((minAIACoreDimming171Time - slopeCalculationStartPoint[0]) * 24.)) ; Converted to /sec from /day
aiaGlobalDimmingSlope211 = abs(aiaGlobalDimmingDepth211 / ((minAIACoreDimming171Time - slopeCalculationStartPoint[0]) * 24.)) ; Converted to /sec from /day

; Output AIA depth and slopes
print, 'AIA % Depths: Core dimming 171, 193, 211; Global dimming 171, 193, 211: ', strtrim(aiaCoreDimmingDepth171, 2), ', ', strtrim(aiaCoreDimmingDepth193, 2), ', ', strtrim(aiaCoreDimmingDepth211, 2), ', ', strtrim(aiaGlobalDimmingDepth171, 2), ', ', strtrim(aiaGlobalDimmingDepth193, 2), ', ', strtrim(aiaGlobalDimmingDepth211, 2)
print, 'AIA Slopes: Core dimming 171, 193, 211; Global dimming 171, 193, 211: ', strtrim(aiaCoreDimmingSlope171, 2), ', ', strtrim(aiaCoreDimmingSlope193, 2), ', ', strtrim(aiaCoreDimmingSlope211, 2), ', ', strtrim(aiaGlobalDimmingSlope171, 2), ', ', strtrim(aiaGlobalDimmingSlope193, 2), ', ', strtrim(aiaGlobalDimmingSlope211, 2)

; Define minimum time
eveCorrectedMinIndex = closest(minAIACoreDimming171Time, eveTimeJD, /LOWER)
eveCorrectedMinTime = eveTimeJD[eveCorrectedMinIndex]

IF doLightCurvesPlot EQ 1 THEN BEGIN
; -= PLOT DIMMING LIGHT CURVES -= ;
fontSize = 24 ; Plot font size - 24 for publication, 16 is default

; Loop through all the different EVE corrections and dimming lines
FOR dimIndex = 0, n_elements(dimByBrightNames) - 1 DO BEGIN
  ; Current corrected EVE data
  correctedEVE = correctedEVEDimmingCurves[*, dimIndex] 
  
  ; Fix EVE data so that the % change is computed from the same 0 point as all other lines
  eveTimeJDStartIndex = closest(timeRange[0], eveTimeJD, /LOWER)
  differenceToRemoveDimLines = correctedEVE[eveTimeJDStartIndex] - 0.
  correctedEVE = correctedEVE - differenceToRemoveDimLines
  FOR i = 0, 4 DO BEGIN
    differenceToRemoveDimLines = dimmingCurves[eveTimeJDStartIndex, i] - 0.
    dimmingCurves[*, i] = dimmingCurves[*, i] - differenceToRemoveDimLines
    differenceToRemoveBrightLines = brighteningCurves[eveTimeJDStartIndex, i] - 0.
    brighteningCurves[*, i] = brighteningCurves[*, i] - differenceToRemoveBrightLines
  ENDFOR
  
  ; Select the relevant dim curve ['171Å', '177Å', '180Å', '195Å', '202Å']
  currentDimmingLine = strmid(dimByBrightNames[dimIndex], 0, 3)
  currentBrighteningLine = strmid(dimByBrightNames[dimIndex], 4, 3, /REVERSE_OFFSET)
  IF currentDimmingLine EQ '171' THEN relevantDimCurve = 0
  IF currentDimmingLine EQ '177' THEN relevantDimCurve = 1
  IF currentDimmingLine EQ '180' THEN relevantDimCurve = 2
  IF currentDimmingLine EQ '195' THEN relevantDimCurve = 3
  IF currentDimmingLine EQ '202' THEN relevantDimCurve = 4
  
  ; Get EVE/AIA raw and band product if looking at 171 data 
  IF currentDimmingLine EQ '171' THEN BEGIN
    eveAIABand = eveLines.band_irradiance[2]
    eveAIABand[where(eveAIABand EQ -1)] = !VALUES.F_NAN
    perEVEAIABand = perdiff(eveAIABand, eveAIABand[eveTimeJDStartIndex])
  ENDIF
  
  ; Get EVE/AIA band product if looking at 195 data
  IF currentDimmingLine EQ '195' THEN BEGIN
    eveAIABand = eveLines.band_irradiance[3]
    eveAIABand[where(eveAIABand EQ -1)] = !VALUES.F_NAN
    perEVEAIABand = perdiff(eveAIABand, eveAIABand[eveTimeJDStartIndex])
  ENDIF
  
  ; Apply scaling for correction to 284 Å
  IF keyword_set(FOR_DEAN) THEN BEGIN
    indexCorrespondingto120Minutes = 120 * 60 / (6 * 10) ; Assumes 1 minute average data (6 10-second samples)
    eventPeakJD = JPMyyyyDoy2jd(yyyyDoy) - 0.5 + float(eventPeakSOD)/86400.
    eventPeakIndex = closest(eventPeakJD, eveTimeJD)
    dimPeak = max(dimmingCurves[((eventPeakIndex - indexCorrespondingto120Minutes) > 0):((eventPeakIndex + indexCorrespondingto120Minutes) < n_elements(eveTimeSOD)-1), 3], dimMaxIndex)
    brightPeak = max(brighteningCurves[((eventPeakIndex - indexCorrespondingto120Minutes) > 0):((eventPeakIndex + indexCorrespondingto120Minutes) < n_elements(eveTimeSOD)-1), 3], brightMaxIndex)
    scaleFactor = dimPeak / brightPeak

    timeShiftByIndex = dimMaxIndex - brightMaxIndex
    scaledBrightCurve = shift(brighteningCurves[*, 3], timeShiftByIndex) * scaleFactor
  ENDIF
  
  ; Compute depth and slope for EVE corrected
  eveCorrectedDepth = abs(correctedEVE[eveCorrectedMinIndex])
  eveUncorrectedDepth = abs(dimmingCurves[eveCorrectedMinIndex, relevantDimCurve])
  eveCorrectedSlope = abs(eveCorrectedDepth / ((eveCorrectedMinTime - slopeCalculationStartPoint) * 24.)) ; Converted to /sec from /day
  eveUncorrectedSlope = abs(eveUncorrectedDepth / ((eveCorrectedMinTime - slopeCalculationStartPoint) * 24.)) ; Converted to /sec from /day
  
  ; Output EVE depth and slopes
  IF currentBrighteningLine EQ '284' THEN BEGIN 
    print, 'EVE % Depths: Corrected ' + dimByBrightNames[dimIndex] + '; Uncorrected: ', strtrim(eveCorrectedDepth, 2), ', ', strtrim(eveUncorrectedDepth, 2)
    print, 'EVE Slopes: Corrected ' + dimByBrightNames[dimIndex] + '; Uncorrected: ', strtrim(eveCorrectedSlope, 2), ', ', strtrim(eveUncorrectedSlope, 2)
  ENDIF
  
  ; Plot when AIA data exists (171 or 193)
  IF currentDimmingLine EQ '171' THEN BEGIN
    p1 = plot(jd171, aiaper171, '2', COLOR = 'magenta', /BUFFER, $
              TITLE  = dimByBrightNames[dimIndex], FONT_SIZE = fontSize, $
              XTITLE = 'Time [UTC Hours]', XTICKUNITS = 'Hours', XRANGE = timeRange, $
              YTITLE = '% Change', YRANGE = [-6, 6], $
              NAME = 'AIA Total')
    p2 = plot(eveTimeJD, perEVEAIABand, 'g2', /OVERPLOT,  $
              NAME = 'EVE AIA Band')
    p3 = plot(eveTimeJD, dimmingCurves[*, relevantDimCurve], COLOR = 'orange', '2', /OVERPLOT, $
              NAME = 'EVE')
    p4 = plot(eveTimeJD, correctedEVE, COLOR = 'blue', '2', /OVERPLOT, $
              NAME = 'Corrected EVE')
    p5 = plot(jd171, perCoreDimming171, 'r2', /OVERPLOT, $
              NAME = 'AIA Region 1')
    p6 = plot(jd171, perDimmingTotal171, COLOR = 'black', '2', /OVERPLOT, $
              NAME = 'AIA Region Total')
    p7 = plot(p1.XRANGE, [0, 0], '--', /OVERPLOT)
    a1 = arrow([eveCorrectedMinTime, eveCorrectedMinTime], [-eveCorrectedDepth, 0], COLOR = 'blue', ARROW_STYLE = 3, THICK = 2, /DATA)
    a2 = arrow([minAIACoreDimming171Time, minAIACoreDimming171Time], [minAIACoreDimming171, 0], COLOR = 'red', ARROW_STYLE = 3, THICK = 2, /DATA)
    ;a3 = arrow([timeRange[0], eveCorrectedMinTime], [0, -eveCorrectedDepth], COLOR = 'blue', ARROW_STYLE = 3, THICK = 2, /DATA)
    ;a4 = arrow([timeRange[0], minAIACoreDimming171Time], [0, minAIACoreDimming171], COLOR = 'red', ARROW_STYLE = 3, THICK = 2, /DATA)
    t1 = text(0.24, 0.26, 'AIA Region 1 Depth  = ' + strmid(strtrim(aiaCoreDimmingDepth171, 2), 0, 4) + ' %, Slope = ' + string(aiaCoreDimmingSlope171, format = '(F0.2)') + ' %/hour')
    t2 = text(0.24, 0.22, 'EVE Dimming Depth = ' + strmid(strtrim(eveCorrectedDepth, 2), 0, 4) + ' %, Slope = ' + string(eveCorrectedSlope, format = '(F0.2)') + ' %/hour')
    ;t1 = text(0.23, 0.26, 'AIA Region 1 Dimming Depth    = 2.94%, Slope = 2.22%/hour')
    ;t2 = text(0.23, 0.22, 'Corrected EVE Dimming Depth = 2.94%, Slope = 2.29%/hour')
    leg = legend(TARGET = [p1, p2, p3, p4, p5, p6], POSITION = [0.92, 0.87])
    
    IF keyword_set(FOR_DEAN) AND dimIndex EQ 3 THEN BEGIN ; only do this for 171 by 284
      p1 = plot(eveTimeJD, dimmingCurves[*, 3], COLOR = 'black', '2', $
                TITLE  = '171 Å Corrected by 284 Å', FONT_SIZE = fontSize, $
                XTITLE = 'Time [UTC Hours]', XTICKUNITS = 'Hours', XRANGE = timeRange, $
                YTITLE = '% Change', YRANGE = [-6, 6], $
                NAME = 'EVE 171 Å')
      p2 = plot(eveTimeJD, scaledBrightCurve, COLOR = 'green', '2', /OVERPLOT, $
                NAME = 'EVE 284 Å Scaled & Shifted')
      p3 = plot(eveTimeJD, correctedEVE, COLOR = 'blue', '2', /OVERPLOT, $
                NAME = 'EVE Corrected')
      p4 = plot(jd171, perCoreDimming171, 'r2', /OVERPLOT, $
                NAME = 'AIA 171 Å Region')
      p7 = plot(p1.XRANGE, [0, 0], '--', /OVERPLOT)
      t1 = text(0.24, 0.26, 'AIA Region Depth      = ' + strmid(strtrim(aiaCoreDimmingDepth171, 2), 0, 4) + ' %, Slope = ' + string(aiaCoreDimmingSlope171, format = '(F0.2)') + ' %/hour')
      t2 = text(0.24, 0.22, 'EVE Dimming Depth = ' + strmid(strtrim(eveCorrectedDepth, 2), 0, 4) + ' %, Slope = ' + string(eveCorrectedSlope, format = '(F0.2)') + ' %/hour')
      leg = legend(TARGET = [p1, p2, p3, p4], POSITION = [0.92, 0.87])
      t3 = text(0.77, 0.7355, '(171 -        )', TARGET = [leg])
      t4 = text(0.84, 0.7355, '284', TARGET = [leg], COLOR = 'green')
      STOP
    ENDIF
    
  ENDIF ELSE IF currentDimmingLine EQ '195' THEN BEGIN 
    p1 = plot(jd193, aiaper193, '2', COLOR = 'magenta', /BUFFER, $
              TITLE  = dimByBrightNames[dimIndex], FONT_SIZE = fontSize, $
              XTITLE = 'Time [UTC Hours]', XTICKUNITS = 'Hours', XRANGE = timeRange, $
              YTITLE = '% Change', YRANGE = [-6, 6], $
              NAME = 'AIA Total')
    p2 = plot(eveTimeJD, perEVEAIABand, 'g2', /OVERPLOT,  $
              NAME = 'EVE AIA Band')
    p3 = plot(eveTimeJD, dimmingCurves[*, relevantDimCurve], COLOR = 'orange', '2', /OVERPLOT, $
              NAME = 'EVE')
    p4 = plot(eveTimeJD, correctedEVE, COLOR = 'blue', '2', /OVERPLOT, $
              NAME = 'Corrected EVE')
    p5 = plot(jd193, perCoreDimming193, 'r2', /OVERPLOT, $
              NAME = 'AIA Region 1')
    p6 = plot(jd193, perDimmingTotal193, COLOR = 'black', '2', /OVERPLOT, $
              NAME = 'AIA Dimming Total')
    p7 = plot(p1.XRANGE, [0, 0], '--', /OVERPLOT)
    a1 = arrow([eveCorrectedMinTime, eveCorrectedMinTime], [-eveCorrectedDepth, 0], COLOR = 'blue', ARROW_STYLE = 3, THICK = 2, /DATA)
    a2 = arrow([minAIACoreDimming171Time, minAIACoreDimming171Time], [minAIACoreDimming193, 0], COLOR = 'red', ARROW_STYLE = 3, THICK = 2, /DATA)
    ;a3 = arrow([timeRange[0], eveCorrectedMinTime], [0, -eveCorrectedDepth], COLOR = 'blue', ARROW_STYLE = 3, THICK = 2, /DATA)
    ;a4 = arrow([timeRange[0], minAIACoreDimming171Time], [0, minAIACoreDimming193], COLOR = 'red', ARROW_STYLE = 3, THICK = 2, /DATA)    
    t1 = text(0.24, 0.26, 'AIA Region 1 Depth  = ' + strmid(strtrim(aiaCoreDimmingDepth193, 2), 0, 4) + ' %, Slope = ' + string(aiaCoreDimmingSlope193, format = '(F0.2)') + ' %/hour')
    t2 = text(0.24, 0.22, 'EVE Dimming Depth = ' + strmid(strtrim(eveCorrectedDepth, 2), 0, 4) + ' %, Slope = ' + string(eveCorrectedSlope, format = '(F0.2)') + ' %/hour')
    ;t1 = text(0.23, 0.26, 'AIA Region 1 Dimming Depth    = 2.49%, Slope = 1.88%/hour')
    ;t2 = text(0.23, 0.22, 'Corrected EVE Dimming Depth = 2.09%, Slope = 1.62%/hour')
    ;leg = legend(TARGET = [p1, p2, p3, p4, p5, p6], POSITION = [0.92, 0.88])
  ENDIF ELSE BEGIN ; Plot when AIA data doesn't exist (177, 180, 202)
    p1 = plot(eveTimeJD, dimmingCurves[*, relevantDimCurve], COLOR = 'orange', '2', /BUFFER, $
              TITLE  = dimByBrightNames[dimIndex], $
              XTITLE = 'Time [UTC Hours]', XTICKUNITS = 'Hours', XRANGE = timeRange, $
              YTITLE = 'Percent Change', $
              NAME = 'EVE')
    p2 = plot(eveTimeJD, correctedEVE, COLOR = 'blue', '2', /OVERPLOT, $      
              NAME = 'Corrected EVE')
    p3 = plot(p1.XRANGE, [0, 0], '--', /OVERPLOT)
    a1 = arrow([eveCorrectedMinTime, eveCorrectedMinTime], [-eveCorrectedDepth, 0], COLOR = 'blue', ARROW_STYLE = 3, THICK = 2, /DATA)
    ;a3 = arrow([timeRange[0], eveCorrectedMinTime], [0, -eveCorrectedDepth], COLOR = 'blue', ARROW_STYLE = 3, THICK = 2, /DATA)
    t2 = text(0.15, 0.16, 'EVE Dimming Depth = ' + strmid(strtrim(eveCorrectedDepth, 2), 0, 4) + ' %, Slope = ' + string(eveCorrectedSlope, format = '(F0.2)') + ' %/hour')
    leg = legend(TARGET = [p1, p2], POSITION = [0.92, 0.88])
  ENDELSE
  
  p1.Save, saveloc + 'EVECorrectedVsAIADimming_' + dimByBrightNames[dimIndex] + '.eps'
ENDFOR

; -= END PLOT DIMMING LIGHT CURVES -= ;
ENDIF ; doLightCurvesPlot

SKIPAIA:
; -= PLOT %DEPTH VERSUS WAVELENGTH =- ;

; Create arrays for %depth vs wavelength plot
dimmingWavelengths = [171, 177, 180, 195, 202]
correctedEVEPercentDepths = fltarr(5)
uncorrectedEVEPercentDepths = fltarr(5)

; Loop through the 5 dimming wavelengths
k = 0
FOR wavelengthIndex = 0, 24, 5 DO BEGIN ; Start at 2 to use the 211Å correction, 4 to use the 335Å correction
  ; Select the relevant dim curve ['171Å', '177Å', '180Å', '195Å', '202Å'] and Manually select each dimming lines best correction line (171 by 211, 177 by 284, 180 by 335, 195 by 335, 202 by 132) 
  currentDimmingLine = strmid(dimByBrightNames[wavelengthIndex], 0, 3)
  IF currentDimmingLine EQ '171' THEN BEGIN 
    relevantDimCurve = 0
    correctedEVE = correctedEVEDimmingCurves[*, 2]
  ENDIF  
  IF currentDimmingLine EQ '177' THEN BEGIN 
    relevantDimCurve = 1
    correctedEVE = correctedEVEDimmingCurves[*, 8]
  ENDIF  
  IF currentDimmingLine EQ '180' THEN BEGIN 
    relevantDimCurve = 2
    correctedEVE = correctedEVEDimmingCurves[*, 14]
  ENDIF  
  IF currentDimmingLine EQ '195' THEN BEGIN
    relevantDimCurve = 3
    correctedEVE = correctedEVEDimmingCurves[*, 19]
  ENDIF
  IF currentDimmingLine EQ '202' THEN BEGIN
    relevantDimCurve = 4
    correctedEVE = correctedEVEDimmingCurves[*, 21]
  ENDIF

  ; Fix EVE data so that the % change is computed from the same 0 point as all other lines
  eveTimeJDStartIndex = closest(timeRange[0], eveTimeJD, /LOWER)
  differenceToRemoveDimLines = correctedEVE[eveTimeJDStartIndex] - 0.
  correctedEVE = correctedEVE - differenceToRemoveDimLines
  FOR i = 0, 4 DO BEGIN
    differenceToRemoveDimLines = dimmingCurves[eveTimeJDStartIndex, i] - 0.
    dimmingCurves[*, i] = dimmingCurves[*, i] - differenceToRemoveDimLines
    differenceToRemoveBrightLines = brighteningCurves[eveTimeJDStartIndex, i] - 0.
    brighteningCurves[*, i] = brighteningCurves[*, i] - differenceToRemoveBrightLines
  ENDFOR
  
  ; Store depths
  correctedEVEPercentDepths[k] = abs(correctedEVE[eveCorrectedMinIndex])
  uncorrectedEVEPercentDepths[k] = abs(dimmingCurves[eveCorrectedMinIndex, relevantDimCurve])
  
  k++
ENDFOR

; Create %depth versus wavelength plot
p1 = scatterplot(dimmingWavelengths, correctedEVEPercentDepths, SYMBOL = 'Square', /SYM_FILLED, SYM_THICK = 3, SYM_COLOR = 'Blue', /BUFFER, $
                 TITLE = 'Dimming lines corrected by best mat', $
                 XTITLE = '$\lambda$ [Å]', $
                 YTITLE = '% Depth', $
                 NAME = 'Corrected EVE')
p2 = scatterplot(dimmingWavelengths, uncorrectedEVEPercentDepths, SYMBOL = 'X', SYM_THICK = 3, SYM_COLOR = 'Orange', /OVERPLOT, $
                 NAME = 'Uncorrected EVE')
l1 = legend(TARGET = [p1, p2], POSITION = [0.92, 0.88])

p1.Save, saveloc + 'PercentDepthVersusEVEWavelength.png'
; -= END PLOT %DEPTH VERSUS WAVELENGTH =- ;

END