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
;   FIT_3D_ONLY:                Set this to only analyze the CME data that was derived with 3D methods
;                               Needs to be run anytime parameters change to generate the saveset that 
;                               will be restored for creating the published plot, which includes a red-dashed
;                               line for the 3D line fit. 
;   FIT_HIGH_MASS_ONLY:         Same idea as above except for fitting CME masses above 1E15 g
;   FIT_LOW_MASS_ONLY:          Same idea as above except for fitting CME masses below 1E15 g
;   PUBLISH_STYLE:              Set to increase font and window size of plots
;   DARK_BACKGROUND:            Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;   USE_PHYSICAL_RELATIONSHIPS: Set this to plot and fit with slope/depth and sqrt(depth) instead of just slope and depth. 
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
;   2015-10-05: James Paul Mason: Wrote script.
;   2015-10-09: James Paul Mason: Significant changes to expand linfit uncertainty grey polygon out by sigma of fits. 
;                                 Also flipped x and y axes but am not going to go to the trouble of changing related variable names. 
;   2015-12/08: James Paul Mason: Updated to handle the new upper/lower limits for mass/velocity agreed upon by CME co-authors
;   2015-12-21: James Paul Mason: Can handle fitting 3D only, high or low mass only, and should now be ready for final round of revisions by co-authors
;   2015-12-29: James Paul Mason: MAJOR revelation: code from ssw called fitexy can accept uncertainty inputs in both x and y and return a single sigma for the fit
;   2016-01-07: James Paul Mason: Cleaned up code given usage of fitexy -- deleted all the polyfit complicated stuff. Saved all that old stuff in a copy of this code with beforefitexy at the end
;   2016-04-29: James Paul Mason: Edits to change relationships from slope vs speed and depth vs mass to slop/depth vs speed and sqrt(depth) vs mass. Added 
;                                 new keyword USE_PHYSICAL_RELATIONSHIPS to do this and still maintain ability to make old plots. 
;   2017-02-27: James Paul Mason: Added some new simplified plots that get exported by default
;-
PRO PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass, FIT_3D_ONLY = FIT_3D_ONLY, FIT_HIGH_MASS_ONLY = FIT_HIGH_MASS_ONLY, FIT_LOW_MASS_ONLY = FIT_LOW_MASS_ONLY, $ 
                                                        PUBLISH_STYLE = PUBLISH_STYLE, DARK_BACKGROUND = DARK_BACKGROUND, USE_PHYSICAL_RELATIONSHIPS = USE_PHYSICAL_RELATIONSHIPS

; Defaults
IF keyword_set(PUBLISH_STYLE) THEN BEGIN
  windowDimension = [800, 700]
  fontSize = 18
ENDIF ELSE BEGIN
  windowDimension = [600, 500]
  fontSize = 12
ENDELSE
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  barFillColor = 'Azure'
  blueDarkOrLight = 'deep sky blue'
  backgroundColor = 'slate grey' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  barFillColor = 'dark slate grey'
  blueDarkOrLight = 'blue'
  backgroundColor = 'white'
ENDELSE

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/20160115 Mason 2-2 Week Period/Preparation/Figures/'

; Hard-code for known axis ranges
IF keyword_set(USE_PHYSICAL_RELATIONSHIPS) THEN BEGIN
  slopeXrange = [0, 0.001]
  depthXrange = [0, 3]
ENDIF ELSE BEGIN
  slopeXrange = [0, 0.0015]
  depthXrange = [0, 6]
ENDELSE

; Hard-code for plot annotated 3D-derived CME parameters
cmeParametersFrom3DEvents = [7, 9, 10, 31, 32, 35]
cmeParametersFrom3DIndices = cmeParametersFrom3DEvents - 1
cmeParametersFrom2014Paper = [37] ; 37 is the index, 38 is the event

; Load the the parameterized values
readcol, saveloc1 + 'Two Two Week Period Event List Export 20160427.csv', eventNumber, speeds, speedErrors, speedLowerLimit, speedUpperLimit, $
                                                                          masses, massErrors, massLowerLimit, massUpperLimit, $
                                                                          depths, depthErrors, slopes, slopeErrors, SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
                                                                          format = 'i, d, d, d, d, d, d, d, d, d, d, d, d'
;readcol, saveloc1 + 'Two Two Week Period Event List Export 20161221.csv', eventNumber, speeds, speedErrors, speedLowerLimit, speedUpperLimit, $
;                                                                          masses, massErrors, massLowerLimit, massUpperLimit, $
;                                                                          depths, depthErrors, slopes, slopeErrors, SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
;                                                                          format = 'f, f, f, f, f, f, f, f, f, f, f, f, f'
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

; mass = k * sqrt(depth) based on Aschanden et al., 2009, 706: 376
depthsSqrt = double(sqrt(depths))
depthSqrtErrors = depthErrors / 2.

; speed = k * slope/depth based on derivation by Tom Woods on 2016/04/25-27
slopesOverDepths = slopes / depths
slopeOverDepthErrors = sqrt((slopeErrors / slopes)^2 + (2 * depthErrors / depths)^2) * slopes / depths

; The /NAN keyword for readcol doesn't do what its supposed to (0 values -> NAN) so have to do it manually
IF where(speeds EQ 0) NE [-1] THEN speeds[where(speeds EQ 0)] = !VALUES.F_NAN
IF where(speedErrors EQ 0 AND speedUpperLimit EQ 0) NE [-1] THEN speedErrors[where(speedErrors EQ 0 AND speedUpperLimit EQ 0)] = !VALUES.F_NAN
IF where(masses EQ 0) NE [-1] THEN masses[where(masses EQ 0)] = !VALUES.F_NAN
IF where(massErrors EQ 0 AND massUpperLimit EQ 0) NE [-1] THEN massErrors[where(massErrors EQ 0 AND massUpperLimit EQ 0)] = !VALUES.F_NAN
IF where(depths EQ 0) NE [-1] THEN depths[where(depths EQ 0)] = !VALUES.F_NAN
IF where(depthErrors EQ 0) NE [-1] THEN depthErrors[where(depthErrors EQ 0)] = !VALUES.F_NAN
IF where(depthsSqrt EQ 0) NE [-1] THEN depthsSqrt[where(depthsSqrt EQ 0)] = !VALUES.F_NAN
IF where(depthSqrtErrors EQ 0) NE [-1] THEN depthSqrtErrors[where(depthSqrtErrors EQ 0)] = !VALUES.F_NAN
IF where(slopes EQ 0) NE [-1] THEN slopes[where(slopes EQ 0)] = !VALUES.F_NAN
IF where(slopeErrors EQ 0) NE [-1] THEN slopeErrors[where(slopeErrors EQ 0)] = !VALUES.F_NAN
IF where(slopesOverDepths EQ 0) NE [-1] THEN slopesOverDepths[where(slopesOverDepths EQ 0)] = !VALUES.F_NAN
IF where(slopeOverDepthErrors EQ 0) NE [-1] THEN slopeOverDepthErrors[where(slopeOverDepthErrors EQ 0)] = !VALUES.F_NAN

goodSpeedAndSlopeIndices = where(finite(speeds) AND finite(speedErrors) AND finite(slopesOverDepths) AND finite(slopeOverDepthErrors), numberOfGoodSpeedAndSlopes)
goodDepthAndMassIndices = where(finite(depths)  AND finite(depthErrors) AND finite(masses) AND finite(massErrors), numberOfGoodDepthAndMasses)

; Hold onto the full 38 events in separate variables for plotting separately
speedsAllEvents = speeds
speedErrorsAllEvents = speedErrors
massesAllEvents = masses
massErrorsAllEvents = massErrors
depthsAllEvents = depths
depthErrorsAllEvents = depthErrors
depthsSqrtAllEvents = depthsSqrt
depthSqrtErrorsAllEvents = depthSqrtErrors
slopesAllEvents = slopes
slopeErrorsAllEvents = slopeErrors
slopesOverDepthsAllEvents = slopesOverDepths
slopeOverDepthErrorsAllEvents = slopeOverDepthErrors

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
depthsSqrt = depthsSqrt[goodDepthAndMassIndices]
depthSqrtErrors = depthSqrtErrors[goodDepthAndMassIndices]
slopes = slopes[goodSpeedAndSlopeIndices]
slopeErrors = slopeErrors[goodSpeedAndSlopeIndices]
slopesOverDepths = slopesOverDepths[goodSpeedAndSlopeIndices]
slopeOverDepthErrors = slopeOverDepthErrors[goodSpeedAndSlopeIndices]

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
IF keyword_set(USE_PHYSICAL_RELATIONSHIPS) THEN BEGIN
  FITEXY, slopesOverDepths, speeds, speedSlopeFitYIntercept, speedSlopeFitSlope, X_SIG = slopeOverDepthErrors, Y_SIG = speedErrors, speedSlopeFitSigma, TOLERANCE = 1d-7
  FITEXY, depthsSqrt, masses, massDepthFitYIntercept, massDepthFitSlope, X_SIG = depthSqrtErrors, Y_SIG = massErrors, massDepthFitSigma 
  speedSlopeXtitle = 'Dimming Slope / Depth [s$^{-1}$]'
  depthMassXtitle = '(Dimming Depth [%])$^{1/2}$'
ENDIF ELSE BEGIN
  IF keyword_set(FIT_3D_ONLY) THEN BEGIN
    ; For the non-physical, better correlation directly between speed and slope, need to add a fake (0, 0) point
    slopes = [0, slopes]
    speeds = [0, speeds]
    slopeErrors = [min(slopeErrors), slopeErrors]
    speedErrors = [min(speedErrors), speedErrors]
  ENDIF
  
  FITEXY, slopes, speeds, speedSlopeFitYIntercept, speedSlopeFitSlope, X_SIG = slopeErrors, Y_SIG = speedErrors, speedSlopeFitSigma
  FITEXY, depths, masses, massDepthFitYIntercept,  massDepthFitSlope,  X_SIG = depthErrors, Y_SIG = massErrors,  massDepthFitSigma
  speedSlopeXtitle = 'Dimming Slope [% s$^{-1}$]'
  depthMassXtitle = 'Dimming Depth [%]'
ENDELSE

; Ready x values
xRangeSpeedSlope = JPMrange(0, slopeXrange[1], npts = 1000)
xRangeMassDepth  = JPMrange(0, depthXRange[1], npts = 1000)

; Extrapolate lines y=b+ax for SpeedSlope
yLineSpeedSlope = speedSlopeFitYIntercept + speedSlopeFitSlope * xRangeSpeedSlope
yLineSpeedSlope_PlusSigma  = (speedSlopeFitYIntercept +  speedSlopeFitSigma[0]) + (speedSlopeFitSlope +  speedSlopeFitSigma[1]) * xRangeSpeedSlope
yLineSpeedSlope_MinusSigma = (speedSlopeFitYIntercept -   speedSlopeFitSigma[0]) + (speedSlopeFitSlope -  speedSlopeFitSigma[1]) * xRangeSpeedSlope

; Extrapolate lines y=b+ax for MassDepth
yLineMassDepth  = massDepthFitYIntercept + massDepthFitSlope * xRangeMassDepth
yLineMassDepth_PlusSigma  = (massDepthFitYIntercept +  massDepthFitSigma[0]) + (massDepthFitSlope +  massDepthFitSigma[1]) * xRangeMassDepth
yLineMassDepth_MinusSigma = (massDepthFitYIntercept -  massDepthFitSigma[0]) + (massDepthFitSlope -  massDepthFitSigma[1]) * xRangeMassDepth

; Convert lines to x, y corners for polygon box - SpeedSlope
polygonXSpeedSlope = [xRangeSpeedSlope[closest(0, yLineSpeedSlope_PlusSigma)], $
                      xRangeSpeedSlope[closest(2500., yLineSpeedSlope_PlusSigma)], $
                      slopeXrange[1], $
                      xRangeSpeedSlope[closest(2500., yLineSpeedSlope_MinusSigma)], $
                      xRangeSpeedSlope[closest(0, yLineSpeedSlope_MinusSigma)], $
                      0]
polygonYSpeedSlope = [yLineSpeedSlope_PlusSigma[closest(0, yLineSpeedSlope_PlusSigma)], $
                      yLineSpeedSlope_PlusSigma[closest(2500., yLineSpeedSlope_PlusSigma)], $
                      2500., $
                      yLineSpeedSlope_MinusSigma[closest(2500., yLineSpeedSlope_MinusSigma)], $
                      yLineSpeedSlope_MinusSigma[closest(0, yLineSpeedSlope_MinusSigma)], $
                      0]

; Convert lines to x, y corners for polygon box - MassDepth
polygonXMassDepth = [xRangeMassDepth[closest(0, yLineMassDepth_PlusSigma)], $
                     xRangeMassDepth[closest(1E16, yLineMassDepth_PlusSigma)], $
                     depthXRange[1], $
                     xRangeMassDepth[closest(1E16, yLineMassDepth_MinusSigma)], $
                     xrangeMassDepth[closest(0, yLineMassDepth_MinusSigma)], $
                     0]
polygonYMassDepth = [yLineMassDepth_PlusSigma[closest(0, yLineMassDepth_PlusSigma)], $
                     yLineMassDepth_PlusSigma[closest(1E16, yLineMassDepth_PlusSigma)], $
                     1e16, $
                     yLineMassDepth_MinusSigma[closest(1E16, yLineMassDepth_MinusSigma)], $
                     yLineMassDepth_MinusSigma[closest(0, yLineMassDepth_MinusSigma)], $
                     0]

IF keyword_set(USE_PHYSICAL_RELATIONSHIPS) THEN BEGIN
  slopesToPlot = slopesOverDepths
  slopeErrorsToPlot = slopeOverDepthErrors
  slopesAllEventsToPlot = slopesOverDepthsAllEvents
  slopeErrorsAllEventsToPlot = slopeoverDepthErrorsAllEvents
  
  depthsToPlot = depthsSqrt
  depthsErrorsToPlot = depthSqrtErrors
  depthsAllEventsToPlot = depthsSqrtAllEvents
  depthErrorsAllEventsToPlot = depthSqrtErrorsAllEvents
ENDIF ELSE BEGIN
  slopesToPlot = slopes
  slopeErrorsToPlot = slopeErrors
  slopesAllEventsToPlot = slopesAllEvents
  slopeErrorsAllEventsToPlot = slopeErrorsAllEvents
  
  depthsToPlot = depths
  depthsErrorsToPlot = depthErrors
  depthsAllEventsToPlot = depthsAllEvents
  depthErrorsAllEventsToPlot = depthErrorsAllEvents
ENDELSE

; Slope vs Speed plot
w = window(BACKGROUND_COLOR = backgroundColor)
p1 = errorplot(slopesToPlot, speeds, slopeErrorsToPlot, speedErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, ERRORBAR_COLOR = foregroundBlackOrWhite, SYM_SIZE = 1, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
               XTITLE = speedSlopeXtitle, XCOLOR = foregroundBlackOrWhite, $
               YTITLE = 'CME Speed [$km s^{-1}$]', YCOLOR = foregroundBlackOrWhite, $
               DIMENSIONS = windowDimension, FONT_SIZE = fontSize)
p1a = errorplot(slopesAllEventsToPlot[cmeParametersFrom3DIndices], speedsAllEvents[cmeParametersFrom3DIndices], slopeErrorsAllEventsToPlot[cmeParametersFrom3DIndices], speedErrorsAllEvents[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'red', ERRORBAR_COLOR = 'red', SYM_SIZE = 1, /OVERPLOT)
p1b = errorplot(slopesAllEventsToPlot[cmeParametersFrom2014Paper], speedsAllEvents[cmeParametersFrom2014Paper], slopeErrorsAllEventsToPlot[cmeParametersFrom2014Paper], speedErrorsAllEvents[cmeParametersFrom2014Paper], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = blueDarkOrLight, ERRORBAR_COLOR = blueDarkOrLight, SYM_SIZE = 1, /OVERPLOT)

; Only want fits to be shown for the direct slope-speed relationship since it's crap for the slope/depth-speed relationship
IF ~keyword_set(USE_PHYSICAL_RELATIONSHIPS) THEN BEGIN
  IF ~keyword_set(FIT_3D_ONLY) THEN p1c3d = polygon(polygonXSpeedSlope3D, polygonYSpeedSlope3D, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'red', FILL_TRANSPARENCY = 80, TARGET = [p1])
  p1c = polygon(polygonXSpeedSlope, polygonYSpeedSlope, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', COLOR = foregroundBlackOrWhite, FILL_TRANSPARENCY = 80, TARGET = [p1])
  p1d = plot(xRangeSpeedSlope, yLineSpeedSlope,  '--', /OVERPLOT, COLOR = foregroundBlackOrWhite, YRANGE = p1.yrange)
  IF ~keyword_set(FIT_3D_ONLY) THEN p1d3d = plot(xRangeSpeedSlope, yLineSpeedSlope3D, 'r--', /OVERPLOT, YRANGE = p1.yrange)
  ;p1fp = plot(xRangeSpeedSlope, yLineSpeedSlope_PlusSigma, 'r', /OVERPLOT)
  ;p1fm = plot(xRangeSpeedSlope, yLineSpeedSlope_MinusSigma, 'b', /OVERPLOT)
  IF ~keyword_set(FIT_3D_ONLY) THEN t13d = text(0.53, 0.18, 'y = ' + JPMPrintNumber(speedSlopeFitSlope3d, /SCIENTIFIC_NOTATION) + 'x ' + JPMPrintNumber(speedSlopeFitYIntercept3d, /SCIENTIFIC_NOTATION), FONT_SIZE = fontSize, COLOR = 'red')
  t1 = text(0.53, 0.23, 'y = ' + JPMPrintNumber(speedSlopeFitSlope, /SCIENTIFIC_NOTATION) + 'x ' + JPMPrintNumber(speedSlopeFitYIntercept, /SCIENTIFIC_NOTATION), FONT_SIZE = fontSize, COLOR = foregroundBlackOrWhite)

  IF keyword_set(PUBLISH_STYLE) THEN BEGIN
    p1.xmajor = 4
    t1.position = [0.22, 0.75]
    t13d.position = [0.22, 0.80]
  ENDIF
  p1.xrange = slopeXRange
ENDIF

; Depth vs Mass plot
w2 = window(BACKGROUND_COLOR = backgroundColor)
p2 = errorplot(depthsToPlot, masses, depthsErrorsToPlot, massErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, ERRORBAR_COLOR = foregroundBlackOrWhite, SYM_SIZE = 1, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
                 XTITLE = depthMassXtitle, XRANGE = depthXRange, XCOLOR = foregroundBlackOrWhite, $
                 YTITLE = 'CME Mass [g]', YRANGE = [0, 1e16], YCOLOR = foregroundBlackOrWhite, $
                 DIMENSIONS = windowDimension, FONT_SIZE = fontSize)
p2a = errorplot(depthsAllEventsToPlot[cmeParametersFrom3DIndices], massesAllEvents[cmeParametersFrom3DIndices], depthErrorsAllEventsToPlot[cmeParametersFrom3DIndices], massErrorsAllEvents[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'red', ERRORBAR_COLOR = 'red', SYM_SIZE = 1, /OVERPLOT)
p2b = errorplot(depthsAllEventsToPlot[cmeParametersFrom2014Paper], massesAllEvents[cmeParametersFrom2014Paper], depthErrorsAllEventsToPlot[cmeParametersFrom2014Paper], massErrorsAllEvents[cmeParametersFrom2014Paper], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = blueDarkOrLight, ERRORBAR_COLOR = blueDarkOrLight, SYM_SIZE = 1, /OVERPLOT)
IF ~keyword_set(FIT_3D_ONLY) THEN p2c3d = polygon(polygonXMassDepth3D, polygonYMassDepth3D, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'red', FILL_TRANSPARENCY = 80, TARGET = [p2])
p2c = polygon(polygonXMassDepth, polygonYMassDepth, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', COLOR = foregroundBlackOrWhite, FILL_TRANSPARENCY = 80, TARGET = [p2])
p2d = plot(xRangeMassDepth, yLineMassDepth, '--', /OVERPLOT, COLOR = foregroundBlackOrWhite)
IF ~keyword_set(FIT_3D_ONLY) THEN p2d3d = plot(xRangeMassDepth, yLineMassDepth3D, 'r--', /OVERPLOT)
IF keyword_set(FIT_LOW_MASS_ONLY) THEN p2.yrange = [1E13, 1E15]
;p2fp = plot(xRangeMassDepth, yLineMassDepth_PlusSigma, 'r', /OVERPLOT)
;p2fm = plot(xRangeMassDepth, yLineMassDepth_MinusSigma, 'b', /OVERPLOT)
t2 = text(0.16, 0.8, 'y = ' + JPMPrintNumber(massDepthFitSlope, /SCIENTIFIC_NOTATION) + 'x ' + JPMPrintNumber(massDepthFitYIntercept, /SCIENTIFIC_NOTATION), FONT_SIZE = fontSize, COLOR = foregroundBlackOrWhite)
IF ~keyword_set(FIT_3D_ONLY) THEN t23d = text(0.16, 0.75, 'y = ' + JPMPrintNumber(massDepthFitSlope3d, /SCIENTIFIC_NOTATION) + 'x +' + JPMPrintNumber(massDepthFitYIntercept3d, /SCIENTIFIC_NOTATION), FONT_SIZE = fontSize, COLOR = 'red')
IF keyword_set(PUBLISH_STYLE) THEN BEGIN
  t2.position = [0.5, 0.27]
  t23d.position = [0.35, 0.25]
ENDIF


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
;FOR i = 0, n_elements(flareClass) - 1 DO BEGINkill
;  IF flareClass[i] NE '' THEN t3 = text(slopesAllEvents[i], speedsAllEvents[i] + 100, /DATA, strtrim(flareClass[i], 2), TARGET = [p1])
;  IF flareClass[i] NE '' THEN t4 = text(depthsAllEvents[i], massesAllEvents[i] + 2E14, /DATA, strtrim(flareClass[i], 2), TARGET = [p2])
;ENDFOR

; This code can be called by PlotDimmingPaper2Figure5DepthVsHighOrLowMass for plotting just high or low mass, so return the plot object to that code
IF keyword_set(FIT_HIGH_MASS_ONLY) THEN BEGIN
  p1.close
  t2.string = 'y = ' + JPMPrintNumber(massDepthFitSlope, /SCIENTIFIC_NOTATION) + 'x ' + JPMPrintNumber(massDepthFitYIntercept, /SCIENTIFIC_NOTATION)
  t2.position = [0.16, 0.82]
  IF keyword_set(PUBLISH_STYLE) THEN BEGIN
    t2.position = [0.22, 0.75]
    t23d.position = [0.22, 0.80]
  ENDIF
  fit_high_mass_only = p2
  STOP
  return ; Don't want to save plots from this code, so skip out 
ENDIF
IF keyword_set(FIT_LOW_MASS_ONLY) THEN BEGIN
  p1.close
  t2.string = 'y = ' + JPMPrintNumber(massDepthFitSlope, /SCIENTIFIC_NOTATION) + ' x' + JPMPrintNumber(massDepthFitYIntercept, /SCIENTIFIC_NOTATION)
  t2.position = [0.16, 0.82]
  IF keyword_set(PUBLISH_STYLE) THEN t2.position = [0.5, 0.2]
  fit_low_mass_only = p2
  return ; Don't want to save plots from this code, so skip out
ENDIF

; Simplified speed vs. slope plot for sharing the concept ; 2017-02-27
averageSlopeSpeedSlope = 2.36e6
slopeRange = JPMrange(0, .0016, npts = 10)
bestFitSpeed = averageSlopeSpeedSlope * slopeRange
w = window(BACKGROUND_COLOR = backgroundColor)
p3 = errorplot(slopesToPlot, speeds, slopeErrorsToPlot, speedErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, ERRORBAR_COLOR = foregroundBlackOrWhite, SYM_SIZE = 1, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
               XTITLE = speedSlopeXtitle, XCOLOR = foregroundBlackOrWhite, $
               YTITLE = 'CME Speed [$km s^{-1}$]', YCOLOR = foregroundBlackOrWhite, $
               DIMENSIONS = windowDimension, FONT_SIZE = fontSize)
p3b = plot(slopeRange, bestFitSpeed, '2--', /OVERPLOT, COLOR = foregroundBlackOrWhite, YRANGE = p3.yrange)
t3 = text(0.6, 0.8, 'y = ' + JPMPrintNumber(averageSlopeSpeedSlope, /SCIENTIFIC_NOTATION) + ' x', FONT_SIZE = fontSize, FONT_COLOR = foregroundBlackOrWhite)

; Simplified mass vs. depth plot for sharing the concept ; 2017-02-27
averageSlopeMassDepth = 2.59e15
sqrtDepthRange = JPMrange(0, 3., npts = 10)
bestFitMass = averageSlopeMassDepth * sqrtDepthRange
w = window(BACKGROUND_COLOR = backgroundColor)
p4 = errorplot(depthsToPlot, masses, depthsErrorsToPlot, massErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, ERRORBAR_COLOR = foregroundBlackOrWhite, SYM_SIZE = 1, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
               XTITLE = depthMassXtitle, XRANGE = depthXRange, XCOLOR = foregroundBlackOrWhite, $
               YTITLE = 'CME Mass [g]', YRANGE = [0, 1e16], YCOLOR = foregroundBlackOrWhite, $
               DIMENSIONS = windowDimension, FONT_SIZE = fontSize)
p4d = plot(sqrtDepthRange, bestFitMass, '2--', /OVERPLOT, COLOR = foregroundBlackOrWhite)
t4 = text(0.6, 0.8, 'y = ' + JPMPrintNumber(averageSlopeMassDepth, /SCIENTIFIC_NOTATION) + ' x', FONT_SIZE = fontSize, FONT_COLOR = foregroundBlackOrWhite)

IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  ;p1.save, '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/Correlations1.png', /TRANSPARENT
  ;p2.save, '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/Correlations2.png', /TRANSPARENT
  p3.save, './SimpleCorrelationSpeedVsSlope.png', /TRANSPARENT
  p4.save, './SimpleCorrelationMassVsDepth.png', /TRANSPARENT
ENDIF ELSE BEGIN
  IF keyword_set(USE_PHYSICAL_RELATIONSHIPS) THEN BEGIN
    slopeType = 'SlopeOverDepth'
    depthType = 'DepthSqrt'
  ENDIF ELSE BEGIN
    slopeType = 'Slope'
    depthType = 'Depth'
  ENDELSE
  ;p1.save, saveloc1 + slopeType + ' Vs Speed.png'
  ;p1.save, saveloc2 + 'PNGs/' + slopeType + 'VsSpeed.png'
  ;p1.save, saveloc2 + 'EPSs/' + slopeType + 'VsSpeed.eps', /CMYK
  ;p2.save, saveloc1 + depthType + ' Vs Mass.png'
  ;p2.save, saveloc2 + 'PNGs/' + depthType + 'VsMass.png'
  ;p2.save, saveloc2 + 'EPSs/' + depthType + 'VsMass.eps', /CMYK
  p3.save, './SimpleCorrelationSpeedVsSlope.png', /TRANSPARENT
  p4.save, './SimpleCorrelationMassVsDepth.png', /TRANSPARENT
  
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
    ;save, polygonXSpeedSlope3D, polygonYSpeedSlope3D, yLineSpeedSlope3D, polygonXMassDepth3D, polygonYMassDepth3D, yLineMassDepth3D, $
    ;      speedSlopeFitSlope3D, speedSlopeFitYIntercept3D, massDepthFitSlope3D, massDepthFitYIntercept3D,  FILENAME = saveloc1 + 'Correlation Fits 3D CMEs Only.sav', /COMPRESS
  ENDIF
  ;save, FILENAME = saveloc2 + 'IDLSavesets/Figure4Saveset.sav', /COMPRESS
ENDELSE

END