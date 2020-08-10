;+
; NAME:
;   PlotDimmingPaper2Figure3UncertaintyComparison
;
; PURPOSE:
;   Create plot for a single event showing the relative uncertainties of the raw EVE data and the various fits
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
;   PNG and EPS versions of plot in 2 directories:
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/
;   2. Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/EPSs/ and PNGs/
;   Also produces a .sav of everything in 
;   Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/IDLSavesets/
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires JPMPrintNumber
;   Requires the Event15 171 Å Fit.sav file output from FitCoronalDimmingLightCurve.pro
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2015/10/05: James Paul Mason: Wrote script. 
;-
PRO PlotDimmingPaper2Figure3UncertaintyComparison, DARK_BACKGROUND = DARK_BACKGROUND

; Defaults
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  blueDarkOrLight = 'deep sky blue'
  backgroundColor = 'black' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  blueDarkOrLight = 'blue'
  backgroundColor = 'white'
ENDELSE

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

restore, saveloc1 + 'Event15 171 Å Fit.sav'

; Error plot
w = window(BACKGROUND_COLOR = backgroundColor)
p1 = errorplot(sod/3600., intensity, intensityError, '2', COLOR = foregroundBlackOrWhite, ERRORBAR_COLOR = foregroundBlackOrWhite, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
               TITLE = 'Event #15 - 2011/02/24 07:39 Dimming Fits', $
               XTITLE = 'UTC Time [Hour]', XCOLOR = foregroundBlackOrWhite, $
               YTITLE = 'Pre-flare Relative Intensity [%]', YCOLOR = foregroundBlackOrWhite, $
               NAME = 'EVE Fe IX 171 Å')
p2 = errorplot(sod/3600., parabolaCurve, parabolaError, 'r2', /OVERPLOT, ERRORBAR_COLOR = 'r', $
               YRANGE = p1.YRANGE, $
               NAME = 'Parabola')
p3 = errorplot(sod/3600., poly3Curve, poly3Error, 'g2', /OVERPLOT, ERRORBAR_COLOR = 'g', $
               YRANGE = p1.YRANGE, $
               NAME = '3rd Order Poly')
p4 = errorplot(sod/3600., poly4Curve, poly4Error, '2', COLOR = blueDarkOrLight, /OVERPLOT, ERRORBAR_COLOR = 'b', $
               YRANGE = p1.YRANGE, $
               NAME = '4th Order Poly')
p5 = errorplot(sod/3600., poly5Curve, poly5Error, COLOR = 'orange', '2', /OVERPLOT, ERRORBAR_COLOR = 'orange', $
               YRANGE = p1.YRANGE, $
               NAME = '5th Order Poly')
p6 = plot([7.583, 7.583], [-5, 1], '--', COLOR = foregroundBlackOrWhite, /OVERPLOT)
a = arrow([depthTimeSod / 3600, depthTimeSod / 3600], [0., depthIntensity], /DATA, COLOR = arrowColor, THICK = 2)
s = symbol(slopeTimesSod / 3600, [slopePointLeft, slopePointRight], /DATA, /SYM_FILLED, SYM_COLOR = circleColor, SYM_SIZE = 2, 'circle')
t1 = text(0.90, 0.82, 'SDO/EVE 2 min ave', ALIGNMENT = 1, FONT_COLOR = foregroundBlackOrWhite)
t2 = text(0.90, 0.78, 'Parabola $\chi^2$: '       + JPMPrintNumber(parabolaReducedChi), ALIGNMENT = 1, FONT_COLOR = 'red',    FONT_STYLE = bestChiBoolArray[0])
t3 = text(0.90, 0.74, '3rd Order Poly $\chi^2$: ' + JPMPrintNumber(poly3ReducedChi),    ALIGNMENT = 1, FONT_COLOR = 'green',  FONT_STYLE = bestChiBoolArray[1])
t4 = text(0.90, 0.70, '4th Order Poly $\chi^2$: ' + JPMPrintNumber(poly4ReducedChi),    ALIGNMENT = 1, FONT_COLOR = blueDarkOrLight, FONT_STYLE = bestChiBoolArray[2])
t5 = text(0.90, 0.66, '5th Order Poly $\chi^2$: ' + JPMPrintNumber(poly5ReducedChi),    ALIGNMENT = 1, FONT_COLOR = 'orange', FONT_STYLE = bestChiBoolArray[3])
t6 = text(0.15, 0.20, 'Dimming Depth = ' + JPMPrintNumber(depthIntensity) + ' ± ' + JPMPrintNumber(depthUncertainty) + '%', FONT_COLOR = arrowColor, FONT_STYLE = 1)
t7 = text(0.15, 0.16, 'Dimming Slope = ' + JPMPrintNumber(slope) + ' ± ' + JPMPrintNumber(slopeUncertainty) + '%/hour', FONT_COLOR = circleColor, FONT_STYLE = 1)
STOP
IF keyword_set(DARK_BACKGROUND) THEN p1.save, '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/DimmingFitsExample.png', /TRANSPARENT $
ELSE BEGIN
  p1.save, saveloc1 + 'Uncertainty Comparison.png'
  p1.save, saveloc2 + 'PNGs/UncertaintyComparison.png'
  p1.save, saveloc2 + 'EPSs/UncertaintyComparison.eps'
  save, FILENAME = saveloc2 + 'IDLSavesets/Figure3Saveset.sav', /COMPRESS
ENDELSE

END