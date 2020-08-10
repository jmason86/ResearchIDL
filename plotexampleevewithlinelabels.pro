;+
; NAME:
;   PlotExampleEVEWithLineLabels
;
; PURPOSE:
;   Create a plot of the EVE spectrum with spectral line labels and AIA bandpasses for reference. 
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
;   All variables are saved to disk and a plot is produced
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoftware
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2013/05/07: James Paul Mason: Wrote script.
;   2014/03/07: James Paul Mason: Got the AIA bandpasses working
;   2016/03/16: James Paul Mason: Updated to have saveloc and fixed a bunch of the stupid ssw dependences that used text() instead of text[] for an variable array
;-
PRO PlotExampleEVEWithLineLabels

; Setup
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/'

eve = eve_merge_evs(2010125, 2010125, meta = evemeta)
wave = evemeta.spectrummeta.wavelength
spec = eve.irradiance

w = window(DIMENSIONS = [1500, 700])
p = plot(wave, 1d6 * spec, font_size = 18, /CURRENT, $
         title = 'Example EVE Spectrum', $
         xtitle = 'Wavelength [nm]', XRANGE = [5, 105], $
         ytitle = 'Irradiance [µW/m!U2!N/nm]', /YLOG)
oplot_line_id, wave, spec * 1E6

; Use actual AIA bandpasses
aia = aia_get_response(/area, /dn)
FOR i = 0, 6 DO BEGIN 
  p = plot(aia.wave/10., aia.all[i,*]/total(aia.all[i,*]>0)*5e-2 > 10E-7, COLOR = JPMColors(i, /SIMPLE), THICK = 6, /OVERPLOT, FILL_COLOR = JPMColors(i, /SIMPLE), $ ;/FILL_BACKGROUND, $
           NAME = aia.channels[i])
  IF i EQ 0 THEN leg = legend(TARGET = p, POSITION = [0.9, 0.5]) ELSE leg.add, p
ENDFOR

save, filename = saveloc + 'AIA Bandpass and EVE Line Labels.sav', /COMPRESS

END