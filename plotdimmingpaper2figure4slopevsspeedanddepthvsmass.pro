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
;   None
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
;-
PRO PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

; Hard-code for plot annotated 3D-derived CME parameters
cmeParametersFrom3DIndices = [3, 5, 12, 14] 

; Load the the parameterized values
readcol, saveloc1 + 'Two Two Week Period Event List Export 20151017.csv', eventNumber, daveJunk, speeds, speedErrors, masses, massErrors, depths, depthErrors, $
                                                                          slopes, slopeErrors, SKIPLINE = 1, /PRESERVE_NULL, /SILENT, $
                                                                          format = 'f, a, f, f, f, f, f, f, f, f'
                                                                          
; The /NAN keyword for readcol doesn't do what its supposed to (0 values -> NAN) so have to do it manually
speeds[where(speeds EQ 0)] = !VALUES.F_NAN
speedErrors[where(speedErrors EQ 0)] = !VALUES.F_NAN
masses[where(masses EQ 0)] = !VALUES.F_NAN
massErrors[where(massErrors EQ 0)] = !VALUES.F_NAN
depths[where(depths EQ 0)] = !VALUES.F_NAN
depthErrors[where(depthErrors EQ 0)] = !VALUES.F_NAN
slopes[where(slopes EQ 0)] = !VALUES.F_NAN
slopeErrors[where(slopeErrors EQ 0)] = !VALUES.F_NAN

goodSpeedAndSlopeIndices = where(finite(speeds) AND finite(slopes), numberOfGoodSpeedAndSlopes)
goodDepthAndMassIndices = where(finite(depths) AND finite(masses), numberOfGoodDepthAndMasses)

; Through away all the NaNs
speeds = speeds[goodSpeedAndSlopeIndices]
speedErrors = speedErrors[goodSpeedAndSlopeIndices]
masses = masses[goodDepthAndMassIndices]
massErrors = massErrors[goodDepthAndMassIndices]
depths = depths[goodDepthAndMassIndices]
depthErrors = depthErrors[goodDepthAndMassIndices]
slopes = slopes[goodSpeedAndSlopeIndices]
slopeErrors = slopeErrors[goodSpeedAndSlopeIndices]

; Hard-code constant value uncertainties for CME parameters
massErrorsConstant = fltarr(numberOfGoodDepthAndMasses) & FOR i = 0, numberOfGoodDepthAndMasses - 1 DO massErrorsConstant[i] = 4.56E14
speedErrorsConstant = fltarr(numberOfGoodSpeedAndSlopes) & FOR i = 0, numberOfGoodSpeedAndSlopes - 1 DO speedErrorsConstant[i] = 119.

; Compute linear fits that account for uncertainties in both axes (resulting in two different lines)
fitSpeedSlope_SpeedError = poly_fit(slopes, speeds, 1, MEASURE_ERRORS = speedErrorsConstant, $
                                    SIGMA = sigmaSpeedSlope_SpeedError, CHISQ = chiSpeedSlope_SpeedError)
fitSpeedSlope_SlopeError = poly_fit(speeds, slopes, 1, MEASURE_ERRORS = slopeErrors, $
                                    SIGMA = sigmaSpeedSlope_SlopeError, CHISQ = chiSpeedSlope_SlopeError)
fitMassDepth_MassError = poly_fit(depths, masses, 1, MEASURE_ERRORS = massErrorsConstant, $
                                  SIGMA = sigmaMassDepth_MassError, CHISQ = chiMassDepth_MassError)
fitMassDepth_DepthError = poly_fit(masses, depths, 1, MEASURE_ERRORS = depthErrors, $
                                   SIGMA = sigmaMassDepth_DepthError, CHISQ = chiMassDepth_DepthError) 

; Compute weights for weighted mean
speedWeight = 1/sigmaSpeedSlope_SpeedError
slopeWeight = 1/sigmaSpeedSlope_SlopeError
massWeight = 1/sigmaMassDepth_MassError
depthWeight = 1/sigmaMassDepth_DepthError

; Store terms for legibility 
fitSpeedSlope_SpeedErrorInverted = [-fitSpeedSlope_SpeedError[0] / fitSpeedSlope_SpeedError[1], 1. / fitSpeedSlope_SpeedError[1]]
fitMassDepth_MassErrorInverted   = [-fitMassDepth_MassError[0]   / fitMassDepth_MassError[1],   1. / fitMassDepth_MassError[1]]

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
meanSpeedSlope = [(fitSpeedSlope_SlopeError[0] + fitSpeedSlope_SpeedErrorInverted[0]) / 2., $
                  (fitSpeedSlope_SlopeError[1] + fitSpeedSlope_SpeedErrorInverted[1]) / 2.]
meanDepthMass =   [(fitMassDepth_DepthError[0] + fitMassDepth_MassErrorInverted[0])   / 2., $
                   (fitMassDepth_DepthError[1] + fitMassDepth_MassErrorInverted[1])   / 2.]

; Ready x values for mass-depth
xRangeMassDepth = range(1E13, 1E16, npts = 2000)

; Extrapolate lines y=b+ax - SpeedSlope
yLineSpeedSlope_SlopeError_PlusSigma  = (fitSpeedSlope_SlopeError[0] + sigmaSpeedSlope_SlopeError[0]) + (fitSpeedSlope_SlopeError[1] + sigmaSpeedSlope_SlopeError[1]) * findgen(2000)
yLineSpeedSlope_SlopeError_MinusSigma = (fitSpeedSlope_SlopeError[0] - sigmaSpeedSlope_SlopeError[0]) + (fitSpeedSlope_SlopeError[1] - sigmaSpeedSlope_SlopeError[1]) * findgen(2000)
yLineSpeedSlope_SpeedError_PlusSigma  = (-(fitSpeedSlope_SpeedError[0] + sigmaSpeedSlope_SpeedError[0]) / (fitSpeedSlope_SpeedError[1] + sigmaSpeedSlope_SpeedError[1])) $ 
                                      + (1/(fitSpeedSlope_SpeedError[1] + sigmaSpeedSlope_SpeedError[1])) * findgen(2000)
yLineSpeedSlope_SpeedError_MinusSigma = (-(fitSpeedSlope_SpeedError[0] - sigmaSpeedSlope_SpeedError[0]) / (fitSpeedSlope_SpeedError[1] - sigmaSpeedSlope_SpeedError[1])) $
                                     + (1/(fitSpeedSlope_SpeedError[1] - sigmaSpeedSlope_SpeedError[1])) * findgen(2000)
yLineSpeedSlope_Mean = meanSpeedSlope[0] + meanSpeedSlope[1] * findgen(2000)
yLineSpeedSlope_Mean_Forced0Intercept = 0.0 + meanSpeedSlope[1] * findgen(2000)

; Extrapolate lines y=b+ax - MassDepth
yLineMassDepth_DepthError_PlusSigma = (fitMassDepth_DepthError[0] + sigmaMassDepth_DepthError[0]) + (fitMassDepth_DepthError[1] + sigmaMassDepth_DepthError[1]) * xRangeMassDepth
yLineMassDepth_DepthError_MinusSigma = (fitMassDepth_DepthError[0] - sigmaMassDepth_DepthError[0]) + (fitMassDepth_DepthError[1] - sigmaMassDepth_DepthError[1]) * xRangeMassDepth
yLineMassDepth_MassError_PlusSigma = (-(fitMassDepth_MassError[0] + sigmaMassDepth_MassError[0]) / (fitMassDepth_MassError[1] + sigmaMassDepth_MassError[1])) $
                                    + (1/(fitMassDepth_MassError[1] + sigmaMassDepth_MassError[1])) * xRangeMassDepth
yLineMassDepth_MassError_MinusSigma = (-(fitMassDepth_MassError[0] - sigmaMassDepth_MassError[0]) / (fitMassDepth_MassError[1] - sigmaMassDepth_MassError[1])) $
                                    + (1/(fitMassDepth_MassError[1] - sigmaMassDepth_MassError[1])) * xRangeMassDepth
yLineMassDepth_Mean = meanDepthMass[0] + meanDepthMass[1] * xRangeMassDepth
yLineMassDepth_Mean_Forced0Incercept = 0.0 + meanDepthMass[1] * xRangeMassDepth

; Convert lines to x, y corners for polygon box - SpeedSlope
polygonXSpeedSlope = [closest(0, yLineSpeedSlope_SpeedError_PlusSigma), closest(5, yLineSpeedSlope_SlopeError_MinusSigma), 2000., closest(5, yLineSpeedSlope_SpeedError_MinusSigma), closest(0, yLineSpeedSlope_SlopeError_PlusSigma), 0]
polygonYSpeedSlope = [yLineSpeedSlope_SpeedError_PlusSigma[polygonXSpeedSlope[0]], yLineSpeedSlope_SlopeError_MinusSigma[polygonXSpeedSlope[1]], 5.0, $
                      yLineSpeedSlope_SpeedError_MinusSigma[polygonXSpeedSlope[3]], yLineSpeedSlope_SlopeError_PlusSigma[polygonXSpeedSlope[4]], 0]

; Convert lines to x, y corners for polygon box - MassDepth
polygonCornersXMassDepth = [xRangeMassDepth[where(yLineMassDepth_MassError_PlusSigma EQ min(yLineMassDepth_MassError_PlusSigma))], $
                            xRangeMassDepth[where(yLineMassDepth_DepthError_MinusSigma EQ max(yLineMassDepth_DepthError_MinusSigma))], $
                            xRangeMassDepth[where(yLineMassDepth_MassError_MinusSigma EQ max(yLineMassDepth_MassError_MinusSigma))], $
                            xrangeMassDepth[where(yLineMassDepth_DepthError_PlusSigma EQ min(yLineMassDepth_DepthError_PlusSigma))]]
polygonCornersYMassDepth = [min(yLineMassDepth_MassError_PlusSigma), max(yLineMassDepth_DepthError_MinusSigma), max(yLineMassDepth_MassError_MinusSigma), min(yLineMassDepth_DepthError_PlusSigma)]
polygonXMassDepth = [range(polygonCornersXMassDepth[0], polygonCornersXMassDepth[1], npts = 2000), range(polygonCornersXMassDepth[2], polygonCornersXMassDepth[3], npts = 2000)]
polygonYMassDepth = [range(polygonCornersYMassDepth[0], polygonCornersYMassDepth[1], npts = 2000), range(polygonCornersYMassDepth[2], polygonCornersYMassDepth[3], npts = 2000)]

; Slope vs Speed plot
p1 = errorplot(slopes, speeds, slopeErrors, speedErrorsConstant, LINESTYLE = 'none', SYMBOL = 'circle', SYM_SIZE = 1, TITLE = 'CME Speeds vs Dimming Slopes', $
                 XTITLE = 'Dimming Slope [$% hour^{-1}$]', XRANGE = [0, 5], $
                 YTITLE = 'CME Speed [$km s^{-1}$]')
p1a = errorplot(slopes[cmeParametersFrom3DIndices], speeds[cmeParametersFrom3DIndices], slopeErrors[cmeParametersFrom3DIndices], speedErrorsConstant[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_SIZE = 1, /OVERPLOT)
p1b = polygon(polygonYSpeedSlope, polygonXSpeedSlope, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', FILL_TRANSPARENCY = 80, TARGET = [p1])
p1c = plot(yLineSpeedSlope_Mean, findgen(2000),  '--', /OVERPLOT)
;p1c = plot(yLineSpeedSlope_Mean_Forced0Intercept, findgen(2000), 'g--', /OVERPLOT)
;p1dp = plot(yLineSpeedSlope_SpeedError_PlusSigma, findgen(2000), 'r', /OVERPLOT)
;p1dm = plot(yLineSpeedSlope_SpeedError_MinusSigma, findgen(2000), 'r--', /OVERPLOT)
;p1ep = plot(yLineSpeedSlope_SlopeError_PlusSigma, findgen(2000), 'b', /OVERPLOT)
;p1em = plot(yLineSpeedSlope_SlopeError_MinusSigma, findgen(2000), 'b--', /OVERPLOT)
t1 = text(0.16, 0.8, 'y = 2.61*$10^{-3}$x + ' + JPMPrintNumber(meanSpeedSlope[0]))
;t1 = text(0.16, 0.8, 'y = nn*$10^{-nn}$x + 0.0')

; Depth vs Mass plot
p2 = errorplot(depths, masses, depthErrors, massErrorsConstant, LINESTYLE = 'none', SYMBOL = 'circle', SYM_SIZE = 1, TITLE = 'CME Masses vs Dimming Depths', $
                 XTITLE = 'Dimming Depth [%]', XRANGE = [0, 6], $
                 YTITLE = 'CME Mass [g]', YRANGE = [1E13, 1E16], /YLOG)
p2a = errorplot(depths[cmeParametersFrom3DIndices], masses[cmeParametersFrom3DIndices], depthErrors[cmeParametersFrom3DIndices], massErrorsConstant[cmeParametersFrom3DIndices], $
                LINESTYLE = 'none', SYMBOL = 'circle', /SYM_FILLED, SYM_SIZE = 1, /OVERPLOT)
p2b = polygon(polygonYMassDepth, polygonXMassDepth, /DATA, /FILL_BACKGROUND, FILL_COLOR = 'grey', FILL_TRANSPARENCY = 80, TARGET = [p2])
p2c = plot(yLineMassDepth_Mean, xRangeMassDepth, '--', /OVERPLOT)
;p2c = plot(yLineMassDepth_Mean_Forced0Incercept, xRangeMassDepth, 'g--', /OVERPLOT)
;p2dp = plot(yLineMassDepth_MassError_PlusSigma, xRangeMassDepth, 'r', /OVERPLOT)
;p2dm = plot(yLineMassDepth_MassError_MinusSigma, xRangeMassDepth, 'r--', /OVERPLOT)
;p2ep = plot(yLineMassDepth_DepthError_PlusSigma, xRangeMassDepth, 'b', /OVERPLOT)
;p2em = plot(yLineMassDepth_DepthError_MinusSigma, xRangeMassDepth, 'b--', /OVERPLOT)
t2 = text(0.16, 0.8, 'y = 6.47*$10^{-16}$x + ' + JPMPrintNumber(meanDepthMass[0]))
;t2 = text(0.16, 0.8, 'y = nn*$10^{-nn}$x + 0.0')

p1.save, saveloc1 + 'Slope Vs Speed.png'
p1.save, saveloc2 + 'PNGs/SlopeVsSpeed.png'
p1.save, saveloc2 + 'EPSs/SlopeVsSpeed.eps'
p2.save, saveloc1 + 'Depth Vs Mass.png'
p2.save, saveloc2 + 'PNGs/DepthVsMass.png'
p2.save, saveloc2 + 'EPSs/DepthVsMass.eps'
save, FILENAME = saveloc2 + 'IDLSavesets/Figure4Saveset.sav', /COMPRESS

END