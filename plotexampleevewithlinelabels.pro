;+
; NAME:
;   PlotExampleEVEWithLineLabels
;
; PURPOSE:
;
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
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
;   Written by:
;     James Paul Mason
;     2013/5/7
;     2014/03/07 James Paul Mason: Got the AIA bandpasses working
;-
PRO PlotExampleEVEWithLineLabels

eve = eve_merge_evs(2010125, 2010125, meta = evemeta)
wave = evemeta.spectrummeta.wavelength
spec = eve.irradiance

w = window(DIMENSIONS = [1500, 700])
p = plot(wave, 1d6 * spec, font_size = 18, /CURRENT, $
         title = 'Example EVE Spectrum', $
         xtitle = 'Wavelength [nm]', XRANGE = [5, 105], $
         ytitle = 'Irradiance [µW/m!U2!N/nm]', /YLOG)
oplot_line_id, wave, spec*1E6
;oplot_aia_band, 0.1, 10 ; Tom's method, but assumes 2nm AIA bandpasses

; Use actual AIA bandpasses
aia = aia_get_response(/area, /dn)
FOR i = 0, 6 DO BEGIN 
  p = plot(aia.wave/10., aia.all[i,*]/total(aia.all[i,*]>0)*5e-2 > 10E-7, COLOR = JPMColors(i, /SIMPLE), THICK = 6, /OVERPLOT, FILL_COLOR = JPMColors(i, /SIMPLE), $ ;/FILL_BACKGROUND, $
           NAME = aia.channels[i])
  IF i EQ 0 THEN leg = legend(TARGET = p, POSITION = [0.9, 0.5]) ELSE leg.add, p
ENDFOR

save, p, filename = 'AIA Bandpass and EVE Line Labels.sav'

END