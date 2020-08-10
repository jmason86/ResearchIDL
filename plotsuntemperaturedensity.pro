;+
; NAME:
;   PlotSunTemperatureDensity
;
; PURPOSE:
;   Dissertation plot. 
;   Create a plot showing the temperature and density of the sun from the core to the corona. 
;   Had to use a datathief equivalent (called GraphClick.app) to get these data from different existing plots. 
;   Atmosphere came from http://history.nasa.gov/SP-402/p2.htm
;   Inner sun temperature came from The Cambridge Encyclopedia of the Sun on page 56, Figure 3.1. 
;   Inner sun density came from Dalsgard et al. 1996. The current state of solar modeling. Science, 272, 1286 - 1292.
;   http://solarscience.msfc.nasa.gov/interior.shtml
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   DARK_BACKGROUND: Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;
; OUTPUTS:
;   Plot of temperature and density versus distance from center
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
;   2016/02/25: James Paul Mason: Wrote script.
;-
PRO PlotSunTemperatureDensity, DARK_BACKGROUND = DARK_BACKGROUND

; Defaults
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  backgroundColor = 'white smoke' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  backgroundColor = 'white'
ENDELSE

; Setup
dataloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Data/Solar Temperature Density Beta/'
saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'

; Solar features
solarRadius = 696300. ; km
solarRadiusMm = 693.3 ; Mm
coreRange = [0, 25] ; %
radiativeRange = [25, 71] ; %
convectiveRange = [71, 100] ; %
photosphereRange = [100, 100.05] ; %
chromosphereRange = [100.05, 100.31] ; %
transitionRange = [100.31, 100.326] ; %
coronaRange = [100.326, 103] ; %

readcol, dataloc + 'Solar Atmosphere Temperature.txt', temperatureAtmosphereK, heightForTemperatureAtmosphereKm, /SILENT
readcol, dataloc + 'Solar Atmosphere Density.txt', densityAtmospheregcm3, heightForDensityAtmosphereKm, /SILENT
readcol, dataloc + 'Solar Interior Temperature.txt', heightForTemperatureInteriorPercent, temperatureInteriorK, /SILENT
readcol, dataloc + 'Solar Interior Density.txt', heightForDensityInteriorFraction, densityInteriorgcm3, /SILENT
readcol, dataloc + 'Solar Atmosphere Beta Lower Limit.txt', betaLowerLimit, heightForBetaLowerLimitMm, /SILENT
readcol, dataloc + 'Solar Atmosphere Beta Upper Limit.txt', betaUpperLimit, heightForBetaUpperLimitMm, /SILENT

; Data manipulation
heightForTemperatureAtmospherePercent = ((heightForTemperatureAtmosphereKm / solarRadius) + 1) * 100.
heightForDensityAtmospherePercent = ((heightForDensityAtmosphereKm / solarRadius) + 1) * 100.
heightForDensityInteriorPercent = heightForDensityInteriorFraction * 100.
heightForBetaLowerLimitPercent = ((heightForBetaLowerLimitMm / solarRadius / 1d3) + 1) * 100.
heightForBetaUpperLimitPercent = ((heightForBetaUpperLimitMm / solarRadius / 1d3) + 1) * 100.
heightFOrBetaOrderedForPolygon = [heightForBetaLowerLimitMm, 1e4, reverse(heightForBetaUpperLimitMm), 1e-2]
betaOrderedForPolygon = [betaLowerLimit, 1e2, reverse(betaUpperLimit), 1e2]
photosphereRangeMm = ([photosphereRange[0], photosphereRange[0], photosphereRange[1], photosphereRange[1]] - 100) * solarRadiusMm / 100. & photosphereRangeMm[0:1] = [0.01, 0.01]
chromosphereRangeMm = ([chromosphereRange[0], chromosphereRange[0], chromosphereRange[1], chromosphereRange[1]] - 100) * solarRadiusMm / 100.
transitionRangeMm = ([transitionRange[0], transitionRange[0], transitionRange[1], transitionRange[1]] - 100) * solarRadiusMm / 100.
coronaRangeMm = ([coronaRange[0], coronaRange[0], coronaRange[1], coronaRange[1]] - 100) * solarRadiusMm / 100. & coronaRangeMm[0:1] = [1500, 1500]

; Plot parameters
regionTransparency = 90

w = window(DIMENSIONS = [1600, 800], BACKGROUND_COLOR = backgroundColor)

; Interior plot
p1 = plot(heightForTemperatureInteriorPercent, temperatureInteriorK, 'r3', MARGIN = 0.15, AXIS_STYLE = 4, LAYOUT = [2, 1, 1], /CURRENT, FONT_SIZE = 20)
p2 = plot(heightForDensityInteriorPercent, densityInteriorgcm3, 'b3', MARGIN = 0.15, AXIS_STYLE = 4, LAYOUT = [2, 1, 1], /CURRENT)
a1 = axis('Y', LOCATION = 'left', TARGET = p1, TITLE = 'Temperature [K]', /LOG, COLOR = 'red', TICKFONT_SIZE = 18)
a2 = axis('Y', LOCATION = 'right', TARGET = p2, /LOG, COLOR = 'blue', TICKFONT_SIZE = 18)
a3 = axis('X', LOCATION = 'top', TARGET = p1, COLOR = foregroundBlackOrWhite, SHOWTEXT = 0, TICKFONT_SIZE = 18)
a4 = axis('X', LOCATION = 'bottom', TITLE = "Fraction of Sun's Radius [%]", TARGET = p1, COLOR = foregroundBlackOrWhite, TICKFONT_SIZE = 18)
p1.title = 'Solar Interior'
p1.font_size = 20

; Solar region labels for interior
polyCore = polygon([coreRange[0], coreRange[0], coreRange[1], coreRange[1]], [4e7, 1e8, 1e8, 4e7], /DATA, TARGET = p1, $
                   FILL_COLOR = 'yellow', /FILL_BACKGROUND)
polyCore2 = polygon([coreRange[0], coreRange[0], coreRange[1], coreRange[1]], [1e2, 4e7, 4e7, 1e2], /DATA, TARGET = p1, $
                    FILL_COLOR = 'yellow', /FILL_BACKGROUND, TRANSPARENCY = regionTransparency)
tCore = text(mean(coreRange), 5e7, 'Core', /DATA, ALIGNMENT = 0.5, COLOR = 'black', FONT_SIZE = 16)
polyRadiative = polygon([radiativeRange[0], radiativeRange[0], radiativeRange[1], radiativeRange[1]], [4e7, 1e8, 1e8, 4e7], /DATA, TARGET = p1, $
                        FILL_COLOR = 'gold', /FILL_BACKGROUND)
polyRadiative2 = polygon([radiativeRange[0], radiativeRange[0], radiativeRange[1], radiativeRange[1]], [1e2, 4e7, 4e7, 1e2], /DATA, TARGET = p1, $
                        FILL_COLOR = 'gold', /FILL_BACKGROUND, TRANSPARENCY = regionTransparency)
tRadiative = text(mean(radiativeRange), 5e7, 'Radiative Zone', /DATA, ALIGNMENT = 0.5, COLOR = 'black', FONT_SIZE = 16)
polyConvective = polygon([convectiveRange[0], convectiveRange[0], convectiveRange[1], convectiveRange[1]], [4e7, 1e8, 1e8, 4e7], /DATA, TARGET = p1, $
                         FILL_COLOR = 'orange', /FILL_BACKGROUND)
polyConvective2 = polygon([convectiveRange[0], convectiveRange[0], convectiveRange[1], convectiveRange[1]], [1e2, 4e7, 4e7, 1e2], /DATA, TARGET = p1, $
                         FILL_COLOR = 'orange', /FILL_BACKGROUND, TRANSPARENCY = regionTransparency)
tConvective = text(mean(convectiveRange), 5e7, 'Convection Zone', /DATA, ALIGNMENT = 0.5, COLOR = 'black', FONT_SIZE = 16)
p1.Order, /BRING_TO_FRONT
p2.Order, /BRING_TO_FRONT


; Atmosphere plot
p3 = plot(heightForTemperatureAtmospherePercent, temperatureAtmosphereK, 'r3', MARGIN = 0.15, AXIS_STYLE = 4, LAYOUT = [2, 1, 2], /CURRENT, $
          XRANGE = [100, 103])
p4 = plot(heightForDensityAtmospherePercent, densityAtmospheregcm3, 'b3', MARGIN = 0.15, AXIS_STYLE = 4, LAYOUT = [2, 1, 2], /CURRENT, $
          XRANGE = [100, 103])
a1 = axis('Y', LOCATION = 'left', TARGET = p3, /LOG, COLOR = 'red', TICKFONT_SIZE = 18)
a2 = axis('Y', LOCATION = 'right', TARGET = p4, TITLE = 'Density [$g cm^{-3}$]', /LOG, COLOR = 'blue', TICKFONT_SIZE = 18)
a3 = axis('X', LOCATION = 'top', TARGET = p3, COLOR = foregroundBlackOrWhite, SHOWTEXT = 0, TICKFONT_SIZE = 18)
a4 = axis('X', LOCATION = 'bottom', TITLE = "Fraction of Sun's Radius [%]", TARGET = p3, COLOR = foregroundBlackOrWhite, TICKFONT_SIZE = 18)
p3.title = 'Solar Atmosphere'
p3.font_size = 20

; Solar region labels for atmosphere
polyPhotosphere = polygon([photosphereRange[0], photosphereRange[0], photosphereRange[1], photosphereRange[1]], [5e6, 1e7, 1e7, 5e6], /DATA, TARGET = p3, $
                          FILL_COLOR = 'green', /FILL_BACKGROUND)
polyPhotosphere2 = polygon([photosphereRange[0], photosphereRange[0], photosphereRange[1], photosphereRange[1]], [1e3, 5e6, 5e6, 1e3], /DATA, TARGET = p3, $
                          FILL_COLOR = 'green', /FILL_BACKGROUND, TRANSPARENCY = regionTransparency)
tPhotosphere = text(0.575, 0.85, 'Photosphere', COLOR = 'green', TARGET = p3, FONT_SIZE = 16)
polyChromosphere = polygon([chromosphereRange[0], chromosphereRange[0], chromosphereRange[1], chromosphereRange[1]], [5e6, 1e7, 1e7, 5e6], /DATA, TARGET = p3, $
                           FILL_COLOR = 'dark turquoise', /FILL_BACKGROUND)
polyChromosphere2 = polygon([chromosphereRange[0], chromosphereRange[0], chromosphereRange[1], chromosphereRange[1]], [1e3, 5e6, 5e6, 1e3], /DATA, TARGET = p3, $
                            FILL_COLOR = 'dark turquoise', /FILL_BACKGROUND, TRANSPARENCY = regionTransparency)
tChromosphere = text(0.58, 0.777, 'Chromosphere', COLOR = 'dark turquoise', TARGET = p3, FONT_SIZE = 16)
polyTransition = polygon([transitionRange[0], transitionRange[0], transitionRange[1], transitionRange[1]], [5e6, 1e7, 1e7, 5e6], /DATA, TARGET = p3, $
                         FILL_COLOR = 'indigo', /FILL_BACKGROUND)
polyTransition2 = polygon([transitionRange[0], transitionRange[0], transitionRange[1], transitionRange[1]], [1e3, 5e6, 5e6, 1e3], /DATA, TARGET = p3, $
                         FILL_COLOR = 'indigo', /FILL_BACKGROUND, TRANSPARENCY = regionTransparency)
tTransition = text(0.65, 0.85, 'Transition Region', COLOR = 'indigo', TARGET = p3, FONT_SIZE = 16)
aTransition = arrow([0.69, 0.615], [0.85, 0.82], COLOR = 'indigo', THICK = 2)
polyCorona = polygon([coronaRange[0], coronaRange[0], coronaRange[1], coronaRange[1]], [5e6, 1e7, 1e7, 5e6], /DATA, TARGET = p3, $
                     FILL_COLOR = 'deep sky blue', /FILL_BACKGROUND)
polyCorona2 = polygon([coronaRange[0], coronaRange[0], coronaRange[1], coronaRange[1]], [1e3, 5e6, 5e6, 1e3], /DATA, TARGET = p3, $
                      FILL_COLOR = 'deep sky blue', /FILL_BACKGROUND, TRANSPARENCY = regionTransparency)
tCorona = text(mean(coronaRange), 6e6, 'Corona', ALIGNMENT = 0.5, /DATA, COLOR = 'black', TARGET = p3, FONT_SIZE = 16)
p3.Order, /BRING_TO_FRONT
p4.ORder, /BRING_TO_FRONT

; Beta plot
;p4 = plot(heightForBetaLowerLimitMm, betaLowerLimit, '2', $
;          XTITLE = "Fraction of Sun's Radius [%]", XRANGE = [1e-2, 1e4], /XLOG, XTICKNAME = ['0', '$1.4 \times 10^{-2}$', '$1.4 \times 10^{-1}$', '1.4', '14', '140', '$1.4 \times 10^3$'], $
;          YTITLE = 'Plasma $\beta$', /YLOG)
;p5 = plot(heightForBetaUpperLimitMm, betaUpperLimit, '2', /OVERPLOT)
;p6 = plot(p4.xrange, [1, 1], '--', /OVERPLOT)
;polyfill = polygon(heightFOrBetaOrderedForPolygon, betaOrderedForPolygon, /DATA, $
;                   FILL_COLOR = 'lime green', /FILL_BACKGROUND, FILL_TRANSPARENCY = 65)
;
;; Solar region labels for atmopshere on beta plot
;polyPhotosphere = polygon(photosphereRangeMm, [30, 100, 100, 30], /DATA, TARGET = p4, $
;                          FILL_COLOR = 'green', /FILL_BACKGROUND)
;tPhotosphere = text(0.01, 40, 'Photosphere', /DATA, COLOR = 'black', TARGET = p4, FONT_SIZE = 16)
;polyChromosphere = polygon(chromosphereRangeMm, [30, 100, 100, 30], /DATA, TARGET = p4, $
;                           FILL_COLOR = 'dark turquoise', /FILL_BACKGROUND)
;tChromosphere = text(0.34, 0.78, 'Chromosphere', COLOR = 'dark turquoise', TARGET = p4, FONT_SIZE = 16)
;polyTransition = polygon(transitionRangeMm, [30, 100, 100, 30], /DATA, TARGET = p4, $
;                         FILL_COLOR = 'indigo', /FILL_BACKGROUND)
;tTransition = text(0.44, 0.88, 'Transition Region', COLOR = 'indigo', TARGET = p4, FONT_SIZE = 16)
;aTransition = arrow([0.69, 0.615], [0.85, 0.82], COLOR = 'indigo', THICK = 2)
;polyCorona = polygon(coronaRangeMm, [30, 100, 100, 30], /DATA, TARGET = p4, $
;                     FILL_COLOR = 'deep sky blue', /FILL_BACKGROUND)
;tCorona = text(200, 40, 'Corona', ALIGNMENT = 0.5, /DATA, COLOR = 'black', TARGET = p4, FONT_SIZE = 16)

STOP
; Save plot and data
IF keyword_set(DARK_BACKGROUND) THEN saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/'
p1.save, saveloc + 'SolarTemperatureAndDensity.png', /TRANSPARENT
save, FILENAME = saveloc + 'IDLSavesets/SolarTemperatureAndDensity.sav'

END