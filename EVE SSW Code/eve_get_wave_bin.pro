;docformat = 'rst'

;+
;Given a wavelength, find the index of the wave array which is closest
;to it.
; :Params:
;  wavelength: in, required
;    input wavelength in nanometers
;    
; :Keywords:
;  megsa: in, optional
;    if set, use MEGS A wavelength scale
;  megsb: in, optional
;    if set, use MEGS B wavelength scale
;  l2: in, optional
;    if set, use Level 2 wavelength scale
;  inverse: in, optional
;    if set, treat wavelength as an index instead of a wavelength and
;               find the wavelength that matches the index
; :Returns:
;   If /inverse is not set, index in the appropriate wave array matching this wavelength.
;   If /inverse is     set, wavelength matching this index.
;
;:Description:
;  You should set exactly one of /megsa, /megsb, and /l2. If none are set,
;  the function returns zero. If more than one is set, the first one
;  in the priority list /megsa, /megsb, /l2 is chosen
;-
function eve_get_wave_bin,wavelength,megsa=megsa,megsb=megsb,l2=l2,inverse=inverse
;  stop
  @megs_wave_defines.pro
  if keyword_set(inverse) then begin
    if keyword_set(megsa) then return,megsa_L1_wave[wavelength]
    if keyword_set(megsb) then return,megsb_L1_wave[wavelength]
    if keyword_set(l2)    then return,megs_l2_wave[wavelength]
  end
  if keyword_set(megsa) then return,max(where(wavelength ge megsa_L1_wave))
  if keyword_set(megsb) then return,max(where(wavelength ge megsb_L1_wave))
  if keyword_set(l2)    then return,max(where(wavelength ge megs_l2_wave ))
end

