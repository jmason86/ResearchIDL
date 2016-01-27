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
;   FIT_3D_ONLY: Set this to only analyze the CME data that was derived with 3D methods
;                Needs to be run anytime parameters change to generate the saveset that 
;                will be restored for creating the published plot, which includes a red-dashed
;                line for the 3D line fit. 
;   FIT_HIGH_MASS_ONLY: Same idea as above except for fitting CME masses above 1E15 g
;   FIT_LOW_MASS_ONLY: Same idea as above except for fitting CME masses below 1E15 g
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
;-
PRO PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass, FIT_3D_ONLY = FIT_3D_ONLY, FIT_HIGH_MASS_ONLY = FIT_HIGH_MASS_ONLY, FIT_LOW_MASS_ONLY = FIT_LOW_MASS_ONLY

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

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

; Compute linear fits that account for uncertainties in both axes (resulting in two different lines)
fitSpeedSlope_SpeedError = poly_fit(slopes, speeds, 1, MEASURE_ERRORS = speedErrors, $
                                    SIGMA = sigmaSpeedSlope_SpeedError, CHISQ = chiSpeedSlope_SpeedError)
fitSpeedSlope_SlopeError = poly_fit(speeds, slopes, 1, MEASURE_ERRORS = slopeErrors, $
                                    SIGMA = sigmaSpeedSlope_SlopeError, CHISQ = chiSpeedSlope_SlopeError)
fitMassDepth_MassError = poly_fit(depths, masses, 1, MEASURE_ERRORS = massErrors, $
                                  SIGMA = sigmaMassDepth_MassError, CHISQ = chiMassDepth_MassError)
fitMassDepth_DepthError = poly_fit(masses, depths, 1, MEASURE_ERRORS = depthErrors, $
                                   SIGMA = sigmaMassDepth_DepthError, CHISQ = chiMassDepth_DepthError) 

FITEXY, slopes, speeds, outputYIntercept, outputSlope, X_SIG = slopeErrors, Y_SIG = speedErrors, outputFitSigma, fitChi, outputQ
;FITEXY, speeds, slopes, outputYIntercept, outputSlope, X_SIG = speedErrors, Y_SIG = slopeErrors, outputFitSigma, fitChi, outputQ
FITEXY, depths, masses, outputYIntercept2, outputSlope2, X_SIG = depthErrors, Y_SIG = massErrors, outputFitSigma2 

STOP

; Compute weights for weighted mean
speedWeight = 1/sigmaSpeedSlope_SpeedError
slopeWeight = 1/sigmaSpeedSlope_SlopeError
massWeight = 1/sigmaMassDepth_MassError
depthWeight = 1/sigmaMassDepth_DepthError

; Mean sigma of fit
;-sigmaSpeedSlope_SlopeError[0] / sigmaSpeedSlope_SlopeError[1]
sigma_SpeedSlope_SlopeErrorInvertedSlope =  abs(sigmaSpeedSlope_SlopeError[1] / fitSpeedSlope_SlopeError[1]^2.)
sigma_SpeedSlope_SlopeErrorInvertedYIntercept = sqrt( (fitSpeedSlope_SlopeError[0] / fitSpeedSlope_SlopeError[0])^2 + $
                                                      (fitSpeedSlope_SlopeError[0] * sigmaSpeedSlope_SlopeError[0] / fitSpeedSlope_SlopeError[1]^2)^2 )
sigma_SpeedSlope_SlopeErrorInverted = [sigma_SpeedSlope_SlopeErrorInvertedYIntercept, sigma_SpeedSlope_SlopeErrorInvertedSlope] 



;sigma_MassDepth_DepthErrorInverted = [-sigmaMassDepth_DepthError[0] / sigmaMassDepth_DepthError[1], (1. / sigmaMassDepth_DepthError[1]) / sigma_MassDepth_DepthError[1]]
print, 'Mean speed/slope sigma slope: ' + strtrim(mean([sigmaSpeedSlope_SpeedError[1], sigma_SpeedSlope_SlopeErrorInverted[1]]), 2)
print, 'Mean speed/slope sigma y-intercept: ' + strtrim(mean([sigmaSpeedSlope_SpeedError[0], sigma_SpeedSlope_SlopeErrorInverted[0]]), 2)
;print, 'Mean mass/depth sigma slope: ' + strtrim(mean([sigmaMassDepth_MassError[1], sigma_MassDepth_DepthErrorInverted[1]]), 2)
;print, 'Mean mass/depth sigma y-intercept: ' + strtrim(mean([sigmaMassDepth_MassError[0], sigma_MassDepth_DepthErrorInverted[0]]), 2)

; Store terms for legibility 
fitSpeedSlope_SpeedErrorInverted = [-fitSpeedSlope_SpeedError[0] / fitSpeedSlope_SpeedError[1], 1. / fitSpeedSlope_SpeedError[1]]
fitSpeedSlope_SlopeErrorInverted = [-fitSpeedSlope_SlopeError[0] / fitSpeedSlope_SlopeError[1], 1. / fitSpeedSlope_SlopeError[1]]
fitMassDepth_MassErrorInverted   = [-fitMassDepth_MassError[0]   / fitMassDepth_MassError[1],   1. / fitMassDepth_MassError[1]]
fitMassDepth_DepthErrorInverted  = [-fitMassDepth_DepthError[0]  / fitMassDepth_DepthError[1],  1. / fitMassDepth_DepthError[1]]

; Store weighted terms for legibility
fitSpeedSlope_SlopeErrorWeighted = fitSpeedSlope_SlopeError * slopeWeight
fitSpeedSlope_SpeedErrorWeighted = [fitSpeedSlope_SpeedErrorInverted[0] * speedWeight[1], fitSpeedSlope_SpeedErrorInverted[1] * speedWeight[0]]
fitMassDepth_DepthErrorWeighted = fitMassDepth_DepthError * depthWeight
fitMassDepth_MassErrorWeighted = [fitMassDepth_MassErrorInverted[0] * massWeight[1], fitMassDepth_MassErrorInverted[1] * massWeight[0]]

; Weighted mean linear fits
;meanSpeedSlope = [(fitSpeedSlope_SlopeErrorWeighted[0] + fitSpeedSlope_SpeedErrorWeighted[0]) / (slopeWeight[0] + speedWeight[0]), $
;                  (fitSpeedSlope_SlopeErrorWeighted[1] + fitSpeedSlope_SpeedErrorWeighted[1]) / (slopeWeight[1] + speedWeight[1])]
;meanDepthMass =   [(fitMassDepth_DepthErrorWeighted[0] + fitMassDepth_MassErrorWeighted[0])   / (depthWeight[0] + massWeight[0]), $
;                   (fitMassDepth_DepthErrorWeighted[1] + fitMassDepth_MassErrorWeighted[1])   / (depthWeight[1] + massweight[1])]

; Mean lienar fits
meanSpeedSlope = [(fitSpeedSlope_SpeedError[0] + fitSpeedSlope_SlopeErrorInverted[0]) / 2., $
                  (fitSpeedSlope_SpeedError[1] + fitSpeedSlope_SlopeErrorInverted[1]) / 2.]
meanDepthMass =  [(fitMassDepth_MassError[0]   + fitMassDepth_DepthErrorInverted[0])  / 2., $
                  (fitMassDepth_MassError[1]   + fitMassDepth_DepthErrorInverted[1])  / 2.]
STOP
; Ready x values
xRangeSpeedSlope = JPMrange(0, 5., npts = 2500)
xRangeMassDepth = JPMrange(0, 6, npts = 2500)

; Extrapolate lines y=b+ax - SpeedSlope
yLineSpeedSlope_SpeedError_PlusSigma  = (fitSpeedSlope_SpeedError[0] + sigmaSpeedSlope_SpeedError[0]) + (fitSpeedSlope_SpeedError[1] + sigmaSpeedSlope_SpeedError[1]) * xRangeSpeedSlope
yLineSpeedSlope_SpeedError_MinusSigma = (fitSpeedSlope_SpeedError[0] - sigmaSpeedSlope_SpeedError[0]) + (fitSpeedSlope_SpeedError[1] - sigmaSpeedSlope_SpeedError[1]) * xRangeSpeedSlope
yLineSpeedSlope_SlopeError_PlusSigma  = (-(fitSpeedSlope_SlopeError[0] + sigmaSpeedSlope_SlopeError[0]) / (fitSpeedSlope_SlopeError[1] + sigmaSpeedSlope_SlopeError[1])) $ 
                                      + (1/(fitSpeedSlope_SlopeError[1] + sigmaSpeedSlope_SlopeError[1])) * xRangeSpeedSlope
yLineSpeedSlope_SlopeError_MinusSigma = (-(fitSpeedSlope_SlopeError[0] - sigmaSpeedSlope_SlopeError[0]) / (fitSpeedSlope_SlopeError[1] - sigmaSpeedSlope_SlopeError[1])) $
                                      + (1/(fitSpeedSlope_SlopeError[1] - sigmaSpeedSlope_SlopeError[1])) * xRangeSpeedSlope
yLineSpeedSlope_Mean = meanSpeedSlope[0] + meanSpeedSlope[1] * xRangeSpeedSlope
yLineSpeedSlope_Mean_Forced0Intercept = 0.0 + meanSpeedSlope[1] * xRangeSpeedSlope

; Extrapolate lines y=b+ax - MassDepth
yLineMassDepth_MassError_PlusSigma = (fitMassDepth_MassError[0] + sigmaMassDepth_MassError[0]) + (fitMassDepth_MassError[1] + sigmaMassDepth_MassError[1]) * xRangeMassDepth
yLineMassDepth_MassError_MinusSigma = (fitMassDepth_MassError[0] - sigmaMassDepth_MassError[0]) + (fitMassDepth_MassError[1] - sigmaMassDepth_MassError[1]) * xRangeMassDepth
yLineMassDepth_DepthError_PlusSigma = (-(fitMassDepth_DepthError[0] + sigmaMassDepth_DepthError[0]) / (fitMassDepth_DepthError[1] + sigmaMassDepth_DepthError[1])) $
                                    + (1/(fitMassDepth_DepthError[1] + sigmaMassDepth_DepthError[1])) * xRangeMassDepth
yLineMassDepth_DepthError_MinusSigma = (-(fitMassDepth_DepthError[0] - sigmaMassDepth_DepthError[0]) / (fitMassDepth_DepthError[1] - sigmaMassDepth_DepthError[1])) $
                                    + (1/(fitMassDepth_DepthError[1] - sigmaMassDepth_DepthError[1])) * xRangeMassDepth
yLineMassDepth_Mean = meanDepthMass[0] + meanDepthMass[1] * xRangeMassDepth
yLineMassDepth_Mean_Forced0Incercept = 0.0 + meanDepthMass[1] * xRangeMassDepth

; Convert lines to x, y corners for polygon box - SpeedSlope
polygonXSpeedSlope = [xRangeSpeedSlope[closest(0, yLineSpeedSlope_SpeedError_PlusSigma)], $
                      xRangeSpeedSlope[closest(2500., yLineSpeedSlope_SlopeError_MinusSigma)], $
                      5., $
                      xRangeSpeedSlope[closest(2500., yLineSpeedSlope_SpeedError_MinusSigma)], $
                      xRangeSpeedSlope[closest(0, yLineSpeedSlope_SlopeError_PlusSigma)], $
                      0]
polygonYSpeedSlope = [yLineSpeedSlope_SpeedError_PlusSigma[closest(0, yLineSpeedSlope_SpeedError_PlusSigma)], $
                      yLineSpeedSlope_SlopeError_MinusSigma[closest(2500., yLineSpeedSlope_SlopeError_MinusSigma)], $
                      2500., $
                      yLineSpeedSlope_SpeedError_MinusSigma[closest(2500., yLineSpeedSlope_SpeedError_MinusSigma)], $
                      yLineSpeedSlope_SlopeError_PlusSigma[closest(0, yLineSpeedSlope_SlopeError_PlusSigma)], $
                      0]

; Convert lines to x, y corners for polygon box - MassDepth
polygonCornersXMassDepth = [xRangeMassDepth[closest(0, yLineMassDepth_MassError_PlusSigma)], $
                            xRangeMassDepth[closest(1E16, yLineMassDepth_DepthError_MinusSigma)], $
                            6., $
                            xRangeMassDepth[closest(1E16, yLineMassDepth_MassError_MinusSigma)], $
                            xrangeMassDepth[closest(0, yLineMassDepth_DepthError_PlusSigma)], $
                            0]
polygonCornersYMassDepth = [yLineMassDepth_MassError_PlusSigma[closest(0, yLineMassDepth_MassError_PlusSigma)], $
                            yLineMassDepth_DepthError_MinusSigma[closest(1E16, yLineMassDepth_DepthError_MinusSigma)], $
                            1E16, $
                            yLineMassDepth_MassError_MinusSigma[closest(1E16, yLineMassDepth_MassError_MinusSigma)], $
                            yLineMassDepth_DepthError_PlusSigma[closest(0, yLineMassDepth_DepthError_PlusSigma)], $
                            0]
;polygonXMassDepth = [JPMrange(polygonCornersXMassDepth[0], polygonCornersXMassDepth[1], npts = 2500), JPMrange(polygonCornersXMassDepth[2], polygonCornersXMassDepth[3], npts = 2500)]
;polygonYMassDepth = [JPMrange(polygonCornersYMassDepth[0], polygonCornersYMassDepth[1], npts = 2500), JPMrange(polygonCornersYMassDepth[2], polygonCornersYMassDepth[3], npts = 2500)]

; Slope vs Speed plot
p1 = errorplot(slopes, speeds, slopeErrors, speedErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, TITLE = 'CME Speeds vs Dimming Slopes', $
                 XTITLE = 'Dimming Slope [$% hour^{-1}$]', XRANGE = [0, 5], $
                 YTITLE = 'CME Speed [$km s^{-1}$]')
p1a = errorplot(slopesAllEvents[cmeParametersFrom3DIndices], speedsAllEvents[cmeParametersFrom3DIndices], slopeErrorsAllEvents[cmeParametersFrom3DIndices], speedErrorsAllEvents[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, /OVERPLOT)
p1b = errorplot(slopesAllEvents[cmeParametersFrom2014Paper], speedsAllEvents[cmeParametersFrom2014Paper], slopeErrorsAllEvents[cmeParametersFrom2014Paper], speedErrorsAllEvents[cmeParametersFrom2014Paper], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'blue', SYM_SIZE = 1, /OVERPLOT)
p1c = polygon(polygonXSpeedSlope, polygonYSpeedSlope, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', FILL_TRANSPARENCY = 80, TARGET = [p1])
p1d = plot(xRangeSpeedSlope, yLineSpeedSlope_Mean,  '--', /OVERPLOT, YRANGE = p1.yrange)
IF ~keyword_set(FIT_3D_ONLY) THEN p1d3d = plot(xRangeSpeedSlope, yLineSpeedSlope_Mean3D, 'r--', /OVERPLOT, YRANGE = p1.yrange)
;p1e = plot(xRangeSpeedSlope, yLineSpeedSlope_Mean_Forced0Intercept, 'g--', /OVERPLOT)
;p1fp = plot(xRangeSpeedSlope, yLineSpeedSlope_SpeedError_PlusSigma, 'r', /OVERPLOT)
;p1fm = plot(xRangeSpeedSlope, yLineSpeedSlope_SpeedError_MinusSigma, 'r--', /OVERPLOT)
;p1gp = plot(xRangeSpeedSlope, yLineSpeedSlope_SlopeError_PlusSigma, 'b', /OVERPLOT)
;p1gm = plot(xRangeSpeedSlope, yLineSpeedSlope_SlopeError_MinusSigma, 'b--', /OVERPLOT)
t1 = text(0.60, 0.20, 'y = 4.74*$10^{2}$x - 1.31*$10^{2}$')
;t1   = text(0.58, 0.20, 'y = ' + strtrim(meanSpeedSlope[1], 2) + 'x + ' + strtrim(meanSpeedSlope[0], 2))
IF ~keyword_set(FIT_3D_ONLY) THEN t13d = text(0.60, 0.17, 'y = 4.12*$10^{2}$x - 4.18*$10^{-1}$', COLOR = 'red')
;IF ~keyword_set(FIT_3D_ONLY) THEN t13d = text(0.58, 0.17, 'y = ' + strtrim(meanSpeedSlope3d[1], 2) + 'x + ' + strtrim(meanSpeedSlope3d[0], 2), COLOR = 'red')


; Depth vs Mass plot
p2 = errorplot(depths, masses, depthErrors, massErrors, LINESTYLE = 'none', SYMBOL = 'none', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, TITLE = 'CME Masses vs Dimming Depths', $
                 XTITLE = 'Dimming Depth [%]', XRANGE = [0, 6], $
                 YTITLE = 'CME Mass [g]', YRANGE = [1E13, 1E16])
p2a = errorplot(depthsAllEvents[cmeParametersFrom3DIndices], massesAllEvents[cmeParametersFrom3DIndices], depthErrorsAllEvents[cmeParametersFrom3DIndices], massErrorsAllEvents[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'red', SYM_SIZE = 1, /OVERPLOT)
p2b = errorplot(depthsAllEvents[cmeParametersFrom2014Paper], massesAllEvents[cmeParametersFrom2014Paper], depthErrorsAllEvents[cmeParametersFrom2014Paper], massErrorsAllEvents[cmeParametersFrom2014Paper], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_COLOR = 'blue', SYM_SIZE = 1, /OVERPLOT)
p2c = polygon(polygonCornersXMassDepth, polygonCornersYMassDepth, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', FILL_TRANSPARENCY = 80, TARGET = [p2])
p2d = plot(xRangeMassDepth, yLineMassDepth_Mean, '--', /OVERPLOT)
IF ~keyword_set(FIT_3D_ONLY) THEN p2d3d = plot(xRangeMassDepth, yLineMassDepth_Mean3D, 'r--', /OVERPLOT)
IF keyword_set(FIT_LOW_MASS_ONLY) THEN p2.yrange = [1E13, 1E15]
;p2e = plot(xRangeMassDepth, yLineMassDepth_Mean_Forced0Incercept, 'g--', /OVERPLOT)
;p2fp = plot(xRangeMassDepth, yLineMassDepth_MassError_PlusSigma, 'r', /OVERPLOT)
;p2fm = plot(xRangeMassDepth, yLineMassDepth_MassError_MinusSigma, 'r--', /OVERPLOT)
;p2gp = plot(xRangeMassDepth, yLineMassDepth_DepthError_PlusSigma, 'b', /OVERPLOT)
;p2gm = plot(xRangeMassDepth, yLineMassDepth_DepthError_MinusSigma, 'b--', /OVERPLOT)
t2 = text(0.60, 0.21, 'y = 1.45*$10^{15}$x  -  1.81*$10^{15}$')
;t2   = text(0.58, 0.20, 'y = ' + strtrim(meanDepthMass[1], 2) + 'x + ' + strtrim(meanDepthMass[0], 2))
IF ~keyword_set(FIT_3D_ONLY) THEN t23d = text(0.60, 0.18, 'y = 1.75*$10^{15}$x + 1.87*$10^{14}$', COLOR = 'red')
;IF ~keyword_set(FIT_3D_ONLY) THEN t23d = text(0.58, 0.17, 'y = ' + strtrim(meanDepthMass3d[1], 2) + 'x + ' + strtrim(meanDepthMass3d[0], 2), COLOR = 'red')

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
  t23d.delete
  t2.string = 'y = 2.10*$10^{15}$x - 2.16*$10^{15}$'
  fit_high_mass_only = p2
  return ; Don't want to save plots from this code, so skip out 
ENDIF
IF keyword_set(FIT_LOW_MASS_ONLY) THEN BEGIN
  p1.close
  p2d3d.delete
  t23d.delete
  t2.string = 'y = 1.85*$10^{14}$x + 4.88*$10^{13}$'
  fit_low_mass_only = p2
  return ; Don't want to save plots from this code, so skip out
ENDIF

p1.save, saveloc1 + 'Slope Vs Speed.png'
p1.save, saveloc2 + 'PNGs/SlopeVsSpeed.png'
p1.save, saveloc2 + 'EPSs/SlopeVsSpeed.eps'
p2.save, saveloc1 + 'Depth Vs Mass.png'
p2.save, saveloc2 + 'PNGs/DepthVsMass.png'
p2.save, saveloc2 + 'EPSs/DepthVsMass.eps'
IF keyword_set(FIT_3D_ONLY) THEN BEGIN
  polygonXSpeedSlope3D = polygonXSpeedSlope
  polygonYSpeedSlope3D = polygonYSpeedSlope
  yLineSpeedSlope_Mean3D = yLineSpeedSlope_Mean
  polygonCornersXMassDepth3D = polygonCornersXMassDepth
  polygonCornersYMassDepth3D = polygonCornersYMassDepth
  yLineMassDepth_Mean3D = yLineMassDepth_Mean
  meanSpeedSlope3D = meanSpeedSlope
  meanDepthMass3D = meanDepthMass
  save, polygonXSpeedSlope3D, polygonYSpeedSlope3D, yLineSpeedSlope_Mean3D, polygonCornersXMassDepth3D, polygonCornersYMassDepth3D, yLineMassDepth_Mean3D, meanSpeedSlope3D, meanDepthMass3D, FILENAME = saveloc1 + 'Correlation Fits 3D CMEs Only.sav', /COMPRESS
ENDIF
save, FILENAME = saveloc2 + 'IDLSavesets/Figure4Saveset.sav', /COMPRESS

END