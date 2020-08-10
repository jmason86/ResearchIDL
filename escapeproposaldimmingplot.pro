;+
; NAME:
;   EscapeProposalDimmingPlot
;
; PURPOSE:
;   Make a plot showing a good solar dimming for Kevin France's ESCAPE SMEX proposal
;
; INPUTS:
;   Savesets from my 2-2 week period study published in 2016
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   A plot
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires my preprocessed data from years ago
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2019-04-23: James Paul Mason: Wrote script.
;-
PRO EscapeProposalDimmingPlot

saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/2011216_04AUG_0357_M9.3/'
restore, saveloc + 'EVEScaledIrradiances.sav'
restore, saveloc + 'EVEScaledBrightCurves.sav'
restore, saveloc + 'LightCurveData.sav'

feix = dimmingCurves[*, 0]
fexvScaled = shift(scaledBrightCurves[*, 3], -9) * 0.74
aia = perdiff(total(cutouts171[*, 0:2], 2) + total(cutouts171[*, 4:*], 2), initial171Total, /RELATIVE)
aiaTimeJd = jd171

; Do subtraction for correction
corrected = feix - fexvScaled

hoursEve = (eveTimeJd - eveTimeJd[0]) * 24.
hoursAia = (aiaTimeJd - eveTimeJd[0]) * 24. 

p1 = plot(hoursEve, feix, color = 'dodger blue', thick = 3, font_size = 18, $ 
          XTITLE = 'hours', XRANGE = [3, 16], $ 
          YTITLE = 'intensity relative to baseline [%]', YRANGE = [-6, 6], $
          NAME = 'EVE 171 Å')

p2 = plot(hoursEve, fexvScaled, color = 'lime green', thick = 3, /OVERPLOT, $
          NAME = 'EVE 284 Å scaled & shifted')

p4 = plot(hoursEve, corrected, thick = 3, /OVERPLOT, $
          NAME = 'EVE Corrected (171 - 284)')
          
;p3 = plot(hoursAia, aia, 'tomato', thick = 3, /OVERPLOT, $
;          NAME = 'AIA without flare loops')

p0 = plot(p1.xrange, [0, 0], '--', color = 'grey', thick = 2, /OVERPLOT) 

l = legend(target = [p1, p2, p4], position = [0.922, 0.875], FONT_SIZE = 16)
STOP
p1.save, 'ESCAPE Proposal Solar Dimming Plot EVE.png', /TRANSPARENT

; Create arcsecond array
lowerBound = (-512 * 2.4) - 2.4/2.
upperBound = lowerBound + 1024. * 2.4
arcsecArray = range(lowerBound, upperBound, NPTS = 1024)


w = window(DIMENSIONS = [2000, 1000], /DEVICE)
i = image(referenceImage, arcsecArray, arcsecArray, AXIS_STYLE = 2, /CURRENT, POSITION = [0.05, 0.05, 0.45, 0.95], FONT_SIZE = 24, $
          TITLE = 'AIA 171 Dimming Image', $
          XTITLE = 'Arcsec', $
          YTITLE = 'Arcsec')
;c = contour(congrid(maskFlare, 1024, 1024), arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'tomato', /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])

i.save, 'ESCAPE Proposal Solar Dimming Plot AIA.png'

STOP

END