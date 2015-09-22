;docformat = 'rst'
;+
;Given a spectrum and wavelength scale, calculate the "Stan bands"
;
;:Params:
;  irradiance_: in, required
;    Spectrum irradiance, array of irradiance values in w/m^2/nm. As a special case, if irradiance_ is not passed, the function
;    returns immediately, after setting low= and high=. This way, the function can be used to store and recall the band boundaries
;    easily.
;  wave: in, optional
;    wavelength bin centers, must be an array the same size as irradiance. If not passed, one of /megsa, /megsb, or /l2 must be passed
;    
;:Keywords:
;  photons: in, optional
;    If set, return value is in photons/cm^2/s, otherwise result is in W/m^2. Input is in W/m^2/nm regardless.
;  low: out, optional
;    lower bound of each Stan band in nm
;  high: out, optional
;    upper bound of each Stan band in nm
;  megsa: in, optional
;    Spectrum uses the native EVE Level 1 MEGS-A wavelength scale. If set, wave is ignored and doesn't need to be passed in
;  megsb: in, optional
;    Spectrum uses the native EVE Level 1 MEGS-B wavelength scale. If set, wave is ignored and doesn't need to be passed in
;  l2: in, optional
;    Spectrum uses the native EVE level 2 spectrum wavelength scale. If set, wave is ignored and doesn't need to be passed in
;  cal_data_path: in, optional
;    If set, use this path for the cross_sections.dat file. If not set, the program will use $EVE_CAL_DATA if set (in ssw) or
;    the path of the program eve_get_cross_sections.pro if $EVE_CAL_DATA is not set (not ssw).
;:Returns:
;  A 22 element array of irradiance, in each of the Stan bands, in W/m^2 or ph/cm^2/s depending on /photons flag. When there
;  are two overlapping bands, the first is medium cross section and the second is high. When there are three overlapping
;  bands, they are low, medium, and high. If for whatever reason the input spectrum does not support calculating a band,
;  the return value for that band is -1.
;  
;:Description:
; The "Stan bands" are a set of 22 bands covering the far and extreme ultraviolet spectrum from 0.05nm to 105nm. These bands
; are used as the input to various Earth atmosphere models. Since the bands are used for the interaction of the ultraviolet
; with Earth air, some properties of molecular nitrogen must be taken into account. Therefore, several of these bands appear
; to overlap (bands 11 and 12, 65.0-79.8nm, bands 13-15, 79.8-91.3nm, bands 16-18, 91.3-97.5nm). What is going on with each
; of these bands is that the band is subdivided into portions where molecular nitrogen absorption is low, medium, or high.
; Low is a cross section of less than 4 megabarns, medium is between 4 and 31 megabarns, and high is above 31 megabarns.
; Over these bands, the absorption cross section is very complicated and each band is subdivided into many microbands,
; each one covering a section of the band where the n2 cross section is all low, medium, or high. All of the microbands
; in the same cross section category are added together, to get the medium and high (65nm) or low, medium, and high (79.8nm, 91.3nm)
;
;Reference:
;  Solomon, S. C., and L. Qian (2005), 'Solar extreme-ultraviolet irradiance for general circulation models'
;     J. Geophys. Res., 110, A10306, doi:10.1029/2005JA011160
;-
function stan_bands,irradiance_,wave,low=stan_bands_low,high=stan_bands_high,photons=photons,megsa=megsa,megsb=megsb,l2=l2,cal_data_path=cal_data_path
  ;                    0    1    2    3    4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20     21
  stan_bands_low= [ 0.05, 0.4, 0.8, 1.8, 3.2,  7.0, 15.5, 22.4, 29.0, 32.0, 54.0, 65.0, 65.0, 79.8, 79.8, 79.8, 91.3, 91.3, 91.3, 97.5, 98.7, 102.7]
  stan_bands_high=[  0.4, 0.8, 1.8, 3.2, 7.0, 15.5, 22.4, 29.0, 32.0, 54.0, 65.0, 79.8, 79.8, 91.3, 91.3, 91.3, 97.5, 97.5, 97.5, 98.7,102.7, 105.0]
  cs_mid = 4 ;Megabarns
  cs_high=31
  
  ;special case to allow getting the stan bands boundaries quickly
  if n_elements(irradiance_) eq 0 then return,0
  
@megs_wave_defines.pro
  if keyword_set(megsa) then wave=megsa_l1_wave
  if keyword_set(megsb) then wave=megsb_l1_wave
  if keyword_set(l2)    then wave= megs_l2_wave

  irradiance=irradiance_
  if keyword_set(photons) then irradiance=eve_ph2watt(wave, irradiance, /inverse)

  w=where((irradiance le 0) or ~finite(irradiance),count)

  if(count gt 0) then irradiance[w]=-1

  common stan_bands_common,cs
  if n_elements(cs) eq 0 then begin
    cs=eve_get_cross_sections(cal_data_path=cal_data_path)
  end
  cs_wave=cs[0,*]
  cs_n2_megs=interpol(cs[3,*],cs_wave,wave)*1d18

  result=dblarr(n_elements(stan_bands_low))

  ;start at band 5 since MEGS doesn't measure below this
  for i=5,10 do result[i]=eve_integrate_line_wave(stan_bands_low[i],stan_bands_high[i],irradiance,wave)
  f_low =cs_n2_megs lt cs_mid
  f_mid =cs_n2_megs ge cs_mid and cs_n2_megs lt cs_high
  f_lm  =cs_n2_megs lt cs_high
  f_high=cs_n2_megs ge cs_high

  e=eve_integrate_line_wave(stan_bands_low[11],stan_bands_high[11],irradiance,wave)
  if e ge 0 then begin
    result[11]=eve_integrate_line_wave(/neg,stan_bands_low[11],stan_bands_high[11],irradiance*f_lm,wave)
    result[12]=eve_integrate_line_wave(/neg,stan_bands_low[12],stan_bands_high[12],irradiance*f_high,wave)
  end else begin
    result[11]=-1
    result[12]=-1
  end

  e=eve_integrate_line_wave(stan_bands_low[13],stan_bands_high[13],irradiance,wave)
  if e ge 0 then begin
    result[13]=eve_integrate_line_wave(/neg,stan_bands_low[13],stan_bands_high[13],irradiance*f_low,wave)
    result[14]=eve_integrate_line_wave(/neg,stan_bands_low[14],stan_bands_high[14],irradiance*f_mid,wave)
    result[15]=eve_integrate_line_wave(/neg,stan_bands_low[15],stan_bands_high[15],irradiance*f_high,wave)
  end else begin
    result[13]=-1
    result[14]=-1
    result[15]=-1
  end

  e=eve_integrate_line_wave(stan_bands_low[16],stan_bands_high[16],irradiance,wave)
  if e ge 0 then begin
    result[16]=eve_integrate_line_wave(/neg,stan_bands_low[16],stan_bands_high[16],irradiance*f_low,wave)
    result[17]=eve_integrate_line_wave(/neg,stan_bands_low[17],stan_bands_high[17],irradiance*f_mid,wave)
    result[18]=eve_integrate_line_wave(/neg,stan_bands_low[18],stan_bands_high[18],irradiance*f_high,wave)
  end else begin
    result[16]=-1
    result[17]=-1
    result[18]=-1
  end

  for i=19,21 do result[i]=eve_integrate_line_wave(stan_bands_low[i],stan_bands_high[i],irradiance,wave)
  w=where(result le 0,count)
  if count gt 0 then result[w]=-1
  return,result

end
