;+
; NAME:
;   PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass
;
; PURPOSE:
;   Create two plots showing slope vs speed and depth vs mass
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   FIT_3D_ONLY:        Set this to only analyze the CME data that was derived with 3D methods
;                       Needs to be run anytime parameters change to generate the saveset that 
;                       will be restored for creating the published plot, which includes a red-dashed
;                       line for the 3D line fit. 
;   FIT_HIGH_MASS_ONLY: Same idea as above except for fitting CME masses above 1E15 g
;   FIT_LOW_MASS_ONLY:  Same idea as above except for fitting CME masses below 1E15 g
;   PUBLISH_STYLE:      Set to increase font and window size of plots
;   
; OUTPUTS:
;   PNG and EPS versions of plot in 2 directories:
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/
;   2. Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/EPSs/ and PNGs/
;   Also produces a .sav of everything in
;   Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/IDLSavesets/
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2015/10/05: James Paul Mason: Wrote script.
;   2015/10/09: James Paul Mason: Significant changes to expand linfit uncertainty grey polygon out by sigma of fits. 
;                                 Also flipped x and y axes but am not going to go to the trouble of changing related variable names. 
;   2015/12/08: James Paul Mason: Updated to handle the new upper/lower limits for mass/velocity agreed upon by CME co-authors
;   2015/12/21: James Paul Mason: Can handle fitting 3D only, high or low mass only, and should now be ready for final round of revisions by co-authors
;   2015/12/29: James Paul Mason: MAJOR revelation: code from ssw called fitexy can accept uncertainty inputs in both x and y and return a single sigma for the fit
;   2016/01/07: James Paul Mason: Cleaned up code given usage of fitexy -- deleted all the polyfit complicated stuff. Saved all that old stuff in a copy of this code with beforefitexy at the end
;-
PRO PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass, FIT_3D_ONLY = FIT_3D_ONLY, FIT_HIGH_MASS_ONLY = FIT_HIGH_MASS_ONLY, FIT_LOW_MASS_ONLY = FIT_LOW_MASS_ONLY, PUBLISH_STYLE = PUBLISH_STYLE

; Defaults
IF keyword_set(PUBLISH_STYLE) THEN BEGIN
  windowDimension = [800, 700]
  fontSize = 18
  plotTitle = ['', '']
ENDIF ELSE BEGIN
  windowDimension = [600, 500]
  fontSize = 12
  plotTitle = ['CME Speeds vs Dimming Slopes', 'CME Masses vs Dimming Depths']
ENDELSE


; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/20160115 Mason 2-2 Week Period/Preparation/Figures/'

; Hard-code for plot annotated 3D-derived CME parameters
cmeParametersFrom3DEvents = [7, 9, 10, 31, 32, 35]
cmeParametersFrom3DIndices = cmeParametersFrom3DEvents - 1
cmeParametersFrom2014Paper = [37] ; 37 is the index, 38 is the event

; Load the the parameterized values
readcol, saveloc1 + 'Two Two Week Period Event List Export 20151221.csv', eventNumber, speeds, speedErrors, speedLowerLimit, speedUpperLimit, $
                                                                          masses, massErrors, massLowerLimit, massUpperLimit, $
                                                                          depths, depthErrors, slopes, slopeErrors, SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
                                                                          format = 'f, f, f, f, f, f, f, f, f, f, f, f, f'
;readcol, saveloc1 + 'Two Two Week Period Event List Export 20151207.csv', eventNumber, daveJunk, speeds, speedErrors, speedLowerLimit, speedUpperLimit, $
;                                                                          masses, massErrors, massLowerLimit, massUpperLimit, $
;                                                                          depths, depthErrors, slopes, slopeErrors, SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
;                                                                          format = 'f, a, f, f, f, f, f, f, f, f, f, f, f, f'
;readcol, saveloc1 + 'Two Two Week Period Event List Export With CME Span 20151207.csv', eventNumber, daveJunk, speeds, speedErrors, speedLowerLimit, speedUpperLimit, $
;                                                                                        masses, massErrors, massLowerLimit, massUpperLimit, $
;                                                                                        depths, depthErrors, slopes, slopeErrors, cmeSpan, $
;                                                                                        SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
;                                                                                        format = 'f, a, f, f, f, f, f, f, f, f, f, f, f, f, f'
;readcol, saveloc1 + 'Two Two Week Period Event List Export With Flare Type 20151207.csv', eventNumber, daveJunk, speeds, speedErrors, speedLowerLimit, speedUpperLimit, $
;                                                                                          masses, massErrors, massLowerLimit, massUpperLimit, $
;                                                                                          depths, depthErrors, slopes, slopeErrors, flareType, $
;                                                                                          SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
;                                                                                          format = 'f, a, f, f, f, f, f, f, f, f, f, f, f, f, a'
;readcol, saveloc1 + 'Two Two Week Period Event List Export With Flare Class 20151207.csv', eventNumber, daveJunk, speeds, speedErrors, speedLowerLimit, speedUpperLimit, $
;                                                                                          masses, massErrors, massLowerLimit, massUpperLimit, $
;                                                                                          depths, depthErrors, slopes, slopeErrors, flareClass, $
;                                                                                          SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
;                                                                                          format = 'f, a, f, f, f, f, f, f, f, f, f, f, f, f, a'
                                                                     
; Use only 3D data
IF keyword_set(FIT_3D_ONLY) THEN BEGIN
  speeds = speeds[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  speedErrors = speedErrors[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  speedLowerLimit = speedLowerLimit[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  speedUpperLimit = speedUpperLimit[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  masses = masses[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  massErrors = massErrors[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  massLowerLimit = massLowerLimit[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  massUpperLimit = massUpperLimit[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  depths = depths[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  depthErrors = depthErrors[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  slopes = slopes[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  slopeErrors = slopeErrors[[cmeParametersFrom3DIndices, cmeParametersFrom2014Paper]]
  
  ; Fit looks horrible so Tom suggested adding a (0, 0) ± 0 point
  ; Can't use actual 0 though because that's the no-data flag until next step of code
  speeds = [1E-3, speeds]
  speedErrors = [1E-3, speedErrors]
  masses = [1E-3, masses]
  massErrors = [min(massErrors), massErrors] ; < 8.5 causes a poly_fit error so just use minimum error
  depths = [1E-3, depths]
  depthErrors = [1E-3, depthErrors]
  slopes = [1E-3, slopes]
  slopeErrors = [1E-3, slopeErrors]
ENDIF ELSE restore, saveloc1 + 'Correlation Fits 3D CMEs Only.sav'

IF keyword_set(FIT_HIGH_MASS_ONLY) THEN masses[where(masses LT 1E15)] = 0
IF keyword_set(FIT_LOW_MASS_ONLY) THEN BEGIN
  masses = [1E-3, masses]
  massErrors = [min(massErrors[where(massErrors GT 1E5)]), massErrors]
  massLowerLimit = [min(massLowerLimit[where(massLowerLimit GT 1E5)]), massLowerLimit] ; Lowest lower limit (0 causes crash)
  massUpperLimit = [massLowerLimit[0] + 4.3E13, massUpperLimit] ; Makes for the smallest range
  depths = [1E-3, depths]
  depthErrors = [1E-3, depthErrors]
  masses[where(masses GE 1E15)] = 0
ENDIF

; The /NAN keyword for readcol doesn't do what its supposed to (0 values -> NAN) so have to do it manually
IF where(speeds EQ 0) NE [-1] THEN speeds[where(speeds EQ 0)] = !VALUES.F_NAN
IF where(speedErrors EQ 0 AND speedUpperLimit EQ 0) NE [-1] THEN speedErrors[where(speedErrors EQ 0 AND speedUpperLimit EQ 0)] = !VALUES.F_NAN
IF where(masses EQ 0) NE [-1] THEN masses[where(masses EQ 0)] = !VALUES.F_NAN
IF where(massErrors EQ 0 AND massUpperLimit EQ 0) NE [-1] THEN massErrors[where(massErrors EQ 0 AND massUpperLimit EQ 0)] = !VALUES.F_NAN
IF where(depths EQ 0) NE [-1] THEN depths[where(depths EQ 0)] = !VALUES.F_NAN
IF where(depthErrors EQ 0) NE [-1] THEN depthErrors[where(depthErrors EQ 0)] = !VALUES.F_NAN
IF where(slopes EQ 0) NE [-1] THEN slopes[where(slopes EQ 0)] = !VALUES.F_NAN
IF where(slopeErrors EQ 0) NE [-1] THEN slopeErrors[where(slopeErrors EQ 0)] = !VALUES.F_NAN

goodSpeedAndSlopeIndices = where(finite(speeds) AND finite(speedErrors) AND finite(slopes) AND finite(slopeErrors), numberOfGoodSpeedAndSlopes)
goodDepthAndMassIndices = where(finite(depths)  AND finite(depthErrors) AND finite(masses) AND finite(massErrors), numberOfGoodDepthAndMasses)

; Hold onto the full 38 events in separate variables for plotting separately
speedsAllEvents = speeds
speedErrorsAllEvents = speedErrors
massesAllEvents = masses
massErrorsAllEvents = massErrors
depthsAllEvents = depths
depthErrorsAllEvents = depthErrors
slopesAllEvents = slopes
slopeErrorsAllEvents = slopeErrors

; Throw away all the NaNs
speeds = speeds[goodSpeedAndSlopeIndices]
speedErrors = speedErrors[goodSpeedAndSlopeIndices]
speedLowerLimit = speedLowerLimit[goodSpeedAndSlopeIndices]
speedUpperLimit = speedUpperLimit[goodSpeedAndSlopeIndices]
masses = masses[goodDepthAndMassIndices]
massErrors = massErrors[goodDepthAndMassIndices]
massLowerLimit = massLowerLimit[goodDepthAndMassIndices]
massUpperLimit = massUpperLimit[goodDepthAndMassIndices]
depths = depths[goodDepthAndMassIndices]
depthErrors = depthErrors[goodDepthAndMassIndices]
slopes = slopes[goodSpeedAndSlopeIndices]
slopeErrors = slopeErrors[goodSpeedAndSlopeIndices]

; For Dave's 1-viewpoint CME parameters that have lower-upper limits, compute midpoint and corresponding uncertainty to result in same range
singleViewPointCmeIndices = where(speedUpperLimit NE 0)
IF singleViewPointCmeIndices NE [-1] THEN $
FOR i = 0, n_elements(singleViewPointCmeIndices) - 1 DO BEGIN
  speeds[singleViewPointCmeIndices[i]] = mean([speedLowerLimit[singleViewPointCmeIndices[i]], speedUpperLimit[singleViewPointCmeIndices[i]]])
  speedErrors[singleViewPointCmeIndices[i]] = speedUpperLimit[singleViewPointCmeIndices[i]] - speeds[singleViewPointCmeIndices[i]]
ENDFOR
singleViewPointCmeIndices = where(massUpperLimit NE 0)
IF singleViewPointCmeIndices NE [-1] THEN $
FOR i = 0, n_elements(singleViewPointCmeIndices) - 1 DO BEGIN
  masses[singleViewPointCmeIndices[i]] = mean([massLowerLimit[singleViewPointCmeIndices[i]], massUpperLimit[singleViewPointCmeIndices[i]]])
  massErrors[singleViewPointCmeIndices[i]] = massUpperLimit[singleViewPointCmeIndices[i]] - masses[singleViewPointCmeIndices[i]]
ENDFOR

; Linear fit dimming-CME parameters with uncertainties in both axes and get single 1sigma on fit back
FITEXY, slopes, speeds, speedSlopeFitYIntercept, speedSlopeFitSlope, X_SIG = slopeErrors, Y_SIG = speedErrors, speedSlopeFitSigma
FITEXY, depths, masses, massDepthFitYIntercept,  massDepthFitSlope,  X_SIG = depthErrors, Y_SIG = massErrors,  massDepthFitSigma 

print, 'speed/slope fit sigma: ' + strtrim(speedSlopeFitSigma, 2)
print, 'mass/depth fit sigma: ' + strtrim(massDepthFitSigma, 2)

; Ready x values
xRangeSpeedSlope = JPMrange(0, 5., npts = 2500)
xRangeMassDepth  = JPMrange(0, 6, npts = 2500)

; Extrapolate lines y=b+ax for SpeedSlope
yLineSpeedSlope = speedSlopeFitYIntercept + speedSlopeFitSlope * xRangeSpeedSlope
yLineSpeedSlope_PlusSigma  = (speedSlopeFitYIntercept + 3 * speedSlopeFitSigma[0]) + (speedSlopeFitSlope + 3 * speedSlopeFitSigma[1]) * xRangeSpeedSlope
yLineSpeedSlope_MinusSigma = (speedSlopeFitYIntercept - 3 * speedSlopeFitSigma[0]) + (speedSlopeFitSlope - 3 * speedSlopeFitSigma[1]) * xRangeSpeedSlope

; Extrapolate lines y=b+ax for MassDepth
yLineMassDepth  = massDepthFitYIntercept + massDepthFitSlope * xRangeMassDepth
yLineMassDepth_PlusSigma  = (massDepthFitYIntercept + 3 * massDepthFitSigma[0]) + (massDepthFitSlope + 3 * massDepthFitSigma[1]) * xRangeMassDepth
yLineMassDepth_MinusSigma = (massDepthFitYIntercept - 3 * massDepthFitSigma[0]) + (massDepthFitSlope - 3 * massDepthFitSigma[1]) * xRangeMassDepth

; Convert lines to x, y corners for polygon box - SpeedSlope
polygonXSpeedSlope = [xRangeSpeedSlope[closest(0, yLineSpeedSlope_PlusSigma)], $
                      xRangeSpeedSlope[closest(2500., yLineSpeedSlope_PlusSigma)], $
                      xRangeSpeedSlope[closest(2500., yLineSpeedSlope_MinusSigma)], $
                      xRangeSpeedSlope[closest(0, yLineSpeedSlope_MinusSigma)]]
polygonYSpeedSlope = [yLineSpeedSlope_PlusSigma[closest(0, yLineSpeedSlope_PlusSigma)], $
                      yLineSpeedSlope_PlusSigma[closest(2500., yLineSpeedSlope_PlusSigma)], $
                      yLineSpeedSlope_MinusSigma[closest(2500., yLineSpeedSlope_MinusSigma)], $
                      yLineSpeedSlope_MinusSigma[closest(0, yLineSpeedSlope_MinusSigma)]]

; Convert lines to x, y corners for polygon box - MassDepth
polygonXMassDepth = [xRangeMassDepth[closest(0, yLineMassDepth_PlusSigma)], $
                     xRangeMassDepth[closest(1E16, yLineMassDepth_PlusSigma)], $
                     xRangeMassDepth[closest(1E16, yLineMassDepth_MinusSigma)], $
                     xrangeMassDepth[closest(0, yLineMassDepth_MinusSigma)], $
                     0]
polygonYMassDepth = [yLineMassDepth_PlusSigma[closest(0, yLineMassDepth_PlusSigma)], $
                     yLineMassDepth_PlusSigma[closest(1E16, yLineMassDepth_PlusSigma)], $
                     yLineMassDepth_MinusSigma[closest(1E16, yLineMassDepth_MinusSigma)], $
                     yLineMassDepth_MinusSigma[closest(0, yLineMassDepth_MinusSigma)], $
                     0]

; Ad hoc fix for 3D speedSlope polygon
IF ~keyword_set(FIT_3D_ONLY) THEN BEGIN
  polygonXSpeedSlope3d = [polygonXSpeedSlope3d[0:1], 5., polygonXSpeedSlope3d[2:3]]
  polygonYSpeedSlope3d = [polygonYSpeedSlope3d[0:1], 2500., polygonYSpeedSlope3d[2:3]]
ENDIF

; Slope vs Speed plot
p1 = errorplot(slopes, speeds, slopeErrors, speedErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, TITLE = plotTitle[0], $
                 XTITLE = 'Dimming Slope [$% hour^{-1}$]', XRANGE = [0, 5], $
                 YTITLE = 'CME Speed [$km s^{-1}$]', $
                 DIMENSIONS = windowDimension, FONT_SIZE = fontSize)
p1a = errorplot(slopesAllEvents[cmeParametersFrom3DIndices], speedsAllEvents[cmeParametersFrom3DIndices], slopeErrorsAllEvents[cmeParametersFrom3DIndices], speedErrorsAllEvents[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, /OVERPLOT)
p1b = errorplot(slopesAllEvents[cmeParametersFrom2014Paper], speedsAllEvents[cmeParametersFrom2014Paper], slopeErrorsAllEvents[cmeParametersFrom2014Paper], speedErrorsAllEvents[cmeParametersFrom2014Paper], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'blue', SYM_SIZE = 1, /OVERPLOT)
IF ~keyword_set(FIT_3D_ONLY) THEN p1c3d = polygon(polygonXSpeedSlope3D, polygonYSpeedSlope3D, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'red', FILL_TRANSPARENCY = 80, TARGET = [p1])
p1c = polygon(polygonXSpeedSlope, polygonYSpeedSlope, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', FILL_TRANSPARENCY = 80, TARGET = [p1])
p1d = plot(xRangeSpeedSlope, yLineSpeedSlope,  '--', /OVERPLOT, YRANGE = p1.yrange)
IF ~keyword_set(FIT_3D_ONLY) THEN p1d3d = plot(xRangeSpeedSlope, yLineSpeedSlope3D, 'r--', /OVERPLOT, YRANGE = p1.yrange)
;p1fp = plot(xRangeSpeedSlope, yLineSpeedSlope_PlusSigma, 'r', /OVERPLOT)
;p1fm = plot(xRangeSpeedSlope, yLineSpeedSlope_MinusSigma, 'b', /OVERPLOT)
IF ~keyword_set(FIT_3D_ONLY) THEN t13d = text(0.60, 0.22, 'y = 5.13*$10^{2}$x - 5.26*$10^{-1}$', COLOR = 'red', FONT_SIZE = fontSize - 2)
;IF ~keyword_set(FIT_3D_ONLY) THEN t13d = text(0.58, 0.17, 'y = ' + strtrim(speedSlopeFitSlope3d, 2) + 'x + ' + strtrim(speedSlopeFitYIntercept3d, 2), COLOR = 'red')
t1 = text(0.60, 0.17, 'y = 7.54*$10^{2}$x - 3.09*$10^{2}$', FONT_SIZE = fontSize - 2)
;t1   = text(0.58, 0.20, 'y = ' + strtrim(speedSlopeFitSlope, 2) + 'x + ' + strtrim(speedSlopeFitYIntercept, 2))

; Depth vs Mass plot
p2 = errorplot(depths, masses, depthErrors, massErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, TITLE = plotTitle[1], $
                 XTITLE = 'Dimming Depth [%]', XRANGE = [0, 6], $
                 YTITLE = 'CME Mass [g]', YRANGE = [1E13, 1E16], $
                 DIMENSIONS = windowDimension, FONT_SIZE = fontSize)
p2a = errorplot(depthsAllEvents[cmeParametersFrom3DIndices], massesAllEvents[cmeParametersFrom3DIndices], depthErrorsAllEvents[cmeParametersFrom3DIndices], massErrorsAllEvents[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, /OVERPLOT)
p2b = errorplot(depthsAllEvents[cmeParametersFrom2014Paper], massesAllEvents[cmeParametersFrom2014Paper], depthErrorsAllEvents[cmeParametersFrom2014Paper], massErrorsAllEvents[cmeParametersFrom2014Paper], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'blue', SYM_SIZE = 1, /OVERPLOT)
IF ~keyword_set(FIT_3D_ONLY) THEN p2c3d = polygon(polygonXMassDepth3D, polygonYMassDepth3D, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'red', FILL_TRANSPARENCY = 80, TARGET = [p2])
p2c = polygon(polygonXMassDepth, polygonYMassDepth, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', FILL_TRANSPARENCY = 80, TARGET = [p2])
p2d = plot(xRangeMassDepth, yLineMassDepth, '--', /OVERPLOT)
IF ~keyword_set(FIT_3D_ONLY) THEN p2d3d = plot(xRangeMassDepth, yLineMassDepth3D, 'r--', /OVERPLOT)
IF keyword_set(FIT_LOW_MASS_ONLY) THEN p2.yrange = [1E13, 1E15]
;p2fp = plot(xRangeMassDepth, yLineMassDepth_PlusSigma, 'r', /OVERPLOT)
;p2fm = plot(xRangeMassDepth, yLineMassDepth_MinusSigma, 'b', /OVERPLOT)
IF ~keyword_set(FIT_3D_ONLY) THEN t23d = text(0.55, 0.32, 'y = 1.26*$10^{15}$x + 3.16*$10^{14}$', COLOR = 'red', FONT_SIZE = fontSize - 2)
;IF ~keyword_set(FIT_3D_ONLY) THEN t23d = text(0.58, 0.23, 'y = ' + strtrim(massDepthFitSlope3d, 2) + 'x + ' + strtrim(massDepthFitYIntercept3d, 2), COLOR = 'red')
t2 = text(0.55, 0.27, 'y = 1.90*$10^{14}$x  -  8.04*$10^{13}$', FONT_SIZE = fontSize - 2)
;t2   = text(0.55, 0.26, 'y = ' + strtrim(massDepthFitSlope, 2) + 'x + ' + strtrim(massDepthFitYIntercept, 2))

; Label extra information next to each point
;FOR i = 0, n_elements(eventNumber) - 1 DO BEGIN
;  IF eventNumber[i] NE 0 THEN t3 = text(slopesAllEvents[i], speedsAllEvents[i], /DATA, JPMPrintNumber(eventNumber[i], /NO_DECIMALS), TARGET = [p1])
;  IF eventNumber[i] NE 0 THEN t4 = text(depthsAllEvents[i], massesAllEvents[i], /DATA, JPMPrintNumber(eventNumber[i], /NO_DECIMALS), TARGET = [p2])
;ENDFOR
;FOR i = 0, n_elements(cmeSpan) - 1 DO BEGIN
;  IF cmeSpan[i] NE 0 THEN t3 = text(slopesAllEvents[i], speedsAllEvents[i] + 100, /DATA, JPMPrintNumber(cmeSpan[i], /NO_DECIMALS), TARGET = [p1])
;  IF cmeSpan[i] NE 0 THEN t4 = text(depthsAllEvents[i], massesAllEvents[i] + 2E14, /DATA, JPMPrintNumber(cmeSpan[i], /NO_DECIMALS), TARGET = [p2])
;ENDFOR
;FOR i = 0, n_elements(flareType) - 1 DO BEGIN
;  IF flareType[i] NE 0 THEN t3 = text(slopesAllEvents[i], speedsAllEvents[i] + 100, /DATA, strtrim(flareType[i], 2), TARGET = [p1])
;  IF flareType[i] NE 0 THEN t4 = text(depthsAllEvents[i], massesAllEvents[i] + 2E14, /DATA, strtrim(flareType[i], 2), TARGET = [p2])
;ENDFOR
;FOR i = 0, n_elements(flareClass) - 1 DO BEGIN
;  IF flareClass[i] NE '' THEN t3 = text(slopesAllEvents[i], speedsAllEvents[i] + 100, /DATA, strtrim(flareClass[i], 2), TARGET = [p1])
;  IF flareClass[i] NE '' THEN t4 = text(depthsAllEvents[i], massesAllEvents[i] + 2E14, /DATA, strtrim(flareClass[i], 2), TARGET = [p2])
;ENDFOR

; This code can be called by PlotDimmingPaper2Figure5DepthVsHighOrLowMass for plotting just high or low mass, so return the plot object to that code
IF keyword_set(FIT_HIGH_MASS_ONLY) THEN BEGIN
  p1.close
  p2d3d.delete
  p2c3d.delete
  t23d.delete
  print, 'High mass only fit parameters: '
  print, 'y = ' + strtrim(massDepthFitSlope, 2) + 'x + ' + strtrim(massDepthFitYIntercept, 2)
  t2.string = 'y = 8.04*$10^{14}$x + 1.57*$10^{15}$'
  t2.position = [0.58, 0.2]
  fit_high_mass_only = p2
  return ; Don't want to save plots from this code, so skip out 
ENDIF
IF keyword_set(FIT_LOW_MASS_ONLY) THEN BEGIN
  p1.close
  p2d3d.delete
  p2c3d.delete
  t23d.delete
  print, 'Low mass only fit parameters: '
  print, 'y = ' + strtrim(massDepthFitSlope, 2) + 'x + ' + strtrim(massDepthFitYIntercept, 2)
  t2.string = 'y = 4.45*$10^{13}$x + 6.49*$10^{13}$'
  t2.position = [0.58, 0.2]
  fit_low_mass_only = p2
  return ; Don't want to save plots from this code, so skip out
ENDIF

p1.save, saveloc1 + 'Slope Vs Speed.png'
p1.save, saveloc2 + 'PNGs/SlopeVsSpeed.png'
p1.save, saveloc2 + 'EPSs/SlopeVsSpeed.eps', /CMYK
p2.save, saveloc1 + 'Depth Vs Mass.png'
p2.save, saveloc2 + 'PNGs/DepthVsMass.png'
p2.save, saveloc2 + 'EPSs/DepthVsMass.eps', /CMYK
IF keyword_set(FIT_3D_ONLY) THEN BEGIN
  polygonXSpeedSlope3D = polygonXSpeedSlope
  polygonYSpeedSlope3D = polygonYSpeedSlope
  yLineSpeedSlope3D = yLineSpeedSlope
  polygonXMassDepth3D = polygonXMassDepth
  polygonYMassDepth3D = polygonYMassDepth
  yLineMassDepth3D = yLineMassDepth
  speedSlopeFitSlope3D = speedSlopeFitSlope
  speedSlopeFitYIntercept3D = speedSlopeFitYIntercept
  massDepthFitSlope3D = massDepthFitSlope
  massDepthFitYIntercept3D = massDepthFitYIntercept
  save, polygonXSpeedSlope3D, polygonYSpeedSlope3D, yLineSpeedSlope3D, polygonXMassDepth3D, polygonYMassDepth3D, yLineMassDepth3D, $
        speedSlopeFitSlope3D, speedSlopeFitYIntercept3D, massDepthFitSlope3D, massDepthFitYIntercept3D,  FILENAME = saveloc1 + 'Correlation Fits 3D CMEs Only.sav', /COMPRESS
ENDIF
save, FILENAME = saveloc2 + 'IDLSavesets/Figure4Saveset.sav', /COMPRESS

END