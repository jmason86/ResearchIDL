;+
; NAME:
;   PlotHeII304VsGoesLo
;
; PURPOSE:
;   Create a scatterplot of GOES lo channel (1-8 Å, which is used for flare classification) against
;   He II 304 Å peak. This relationship will then be used for other stars by Allison Youngblood. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   SAVE_PLOT: Set this to save the plot to disk in '/Users/jama6159/Dropbox/Research/Postdoc_LASP/Analysis/Stellar Dimming'
;
; OUTPUTS:
;   None
;
; OPTIONAL OUTPUTS:
;   Scatterplot will be saved to disk if SAVE_PLOT keyword set. Otherwise, it will just appear on screen
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2016-09-19: James Paul Mason: Wrote script.
;-
PRO PlotHeII304VsGoesLo, SAVE_PLOT = SAVE_PLOT

; Setup 
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Analysis/Stellar Dimming/'

; Restore the flare catalog
restore, '/Users/' + getenv('username') + '/Dropbox/Research/Data/Flare Catalog/merged_flare_catalog.sav'

; Pull out the He II 304 Å peaks 
heii304 = flare_catalog.evl.evl_lines[11].peak_irrad

; Pull out the pre-flare irradiance values for He II 304 Å
preflare = flare_catalog.evl.evl_lines[11].preflare_irrad

; Mask out bad data
goodIndices = where(heii304 NE -999. AND preflare NE -999.)
heii304 = heii304[goodIndices]
preflare = preflare[goodIndices]
goodFiniteIndices = where(finite(heii304) EQ 1 AND finite(preflare) EQ 1, numberGoodPoints)
heii304 = heii304[goodFiniteIndices]
preflare = preflare[goodFiniteIndices]

; Subtract pre-flare off of irradiance
relativeHeii304 = heii304 - preflare

; Grab GOES 1-8 Å peak flux
goesPeak = flare_catalog.goes.peak_flux
preflareGoes = flare_catalog.goes.bkgd_flux

; Mask out bad data
goesPeak = goesPeak[goodIndices]
preflareGoes = preflareGoes[goodIndices]
goesPeak = goesPeak[goodFiniteIndices]
preflareGoes = preflareGoes[goodFiniteIndices]

; Subtract pre-flare off from GOES
relativeGoes = goesPeak - preflareGoes

; Create scatterplot
p1 = scatterplot(relativeheii304, goesPeak, $ 
                 TITLE = 'Flare peak in SXR vs EUV; N = ' + JPMPrintNumber(numberGoodPoints, /NO_DECIMALS) + '; p = ' + JPMPrintNumber(correlate(relativeheii304, goesPeak)), $
                 XTITLE = 'Pre-flare Subtracted EVE He II 304 Å Flare Peak [W/m$^2$]', /XLOG, $
                 YTITLE = 'GOES 1-8 Å [W/m$^2$]', /YLOG)
IF keyword_set(SAVE_PLOT) THEN p1.save, saveloc + 'GOES Lo vs He II 304A.png'

save, heii304, preflare, relativeHeii304, goesPeak, FILENAME = saveloc + 'Goes Lo He II 304A Correlation.sav', /COMPRESS

END