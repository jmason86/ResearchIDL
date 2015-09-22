;docformat = 'rst'

;+
;Integrate the the irradiance over a given bandpass. Originally created with AIA bandpasses in mind. 
; :Params: 
;  irradiance_: in, required
;   full array of irradiance from either MEGS A, MEGS B, or level 2 in (W/m^2)/nm
;  b_wave: in, required
;    wavelength scale of bandpass, in nm
;  bandpass_: in, required
;    bandpass at each wavelength given
; :Keywords:
;  megsa: in, optional
;    if set, input is on the MEGS A wavelength scale
;  megsb: in, optional
;    if set, input is on the MEGS B wavelength scale
;  l2: in, optional
;    if set, input is on the Level 2 wavelength scale
;  unc:in, optional
;    if set, do uncertainty propagaion instead of plain integration. Input irradiance is 1-sigma uncertainty
;    of irradiance and return is 1-sigma uncertainty in integration.
;  max_val: in, optional
;    Set this to a positive number to scale the bandpass such that the max is the given number.
;  norm_val: in, optional
;    Set this to a positive number to scale the bandpass such that the area under the bandpass
;    is the given number.
;  neg: in, optional
;    If set, treat fill values as zero and complete the integration regardless of the presence of fill data.
;:returns:
;  integrated weighted measurement or absolute integrated weighted uncertainty in measurement across the given
;  band. Since input is in (W/m^2)/nm, output is in W/m^2.
;
;  By default in the irradiance input, negative numbers are treated as fill values, and if any 
;  irradiances are negative where the bandpass is more tha 0.01, the data is presumed incomplete
;  and a fill value -1 is returned. If /neg is set, fill values are set to zero and integrated
;  normally, such that the absent values don't add to the total.
;   
;:Description:
;  You should set exactly one of /megsa, /megsb, and /l2. If none are set,
;  the function will break. If more than one is set, the first one
;  in the priority list /megsa, /megsb, /l2 is chosen.
;
;  This does an integration of the entire range of irradiance with instrument 
;  sensitivity weighting. This is a "band". If you need no instrument 
;  sensitivity and a limited range of wavelengths, use integrate_line().
;
;  This uses the Midpoint rule to integrate - Each bin is considered to be 
;  centered at its wavelength, and the measurement is considered to be the 
;  mean measurement over the bin.
;
;  For uncertainty propagation purposes, wavelength scale and instruemnt weighting
;  function are considered uncertainty-free.
;  
;-
function eve_integrate_band,irradiance_,b_wave,bandpass_,megsa=megsa,megsb=megsb,l2=l2,unc=unc,norm_val=norm_val,max_val=max_val,neg=neg
@megs2_format.common
  
  ;Get the correct wavelengths for the irradiance data. Don't try to do 
  ;anything with the data below the A1 cutoff
  b_min=eve_get_wave_bin(megsa=megsa,megsb=megsb,l2=l2,megsa_1_cutoff_freq)
  b_max=eve_get_wave_bin(megsa=megsa,megsb=megsb,l2=l2,megs_AtoB_cutoff_freq)-1
  if keyword_set(megsa) then i_wave=megsa_L1_wave[b_min:b_max]
  if keyword_set(megsb) then i_wave=megsb_L1_wave[b_min:b_max]
  if keyword_set(l2)    then i_wave=megs_L2_wave[b_min:b_max]
  irradiance=irradiance_[b_min:b_max]
  ;Measure the wave spacing
  delta_lambda=i_wave[1]-i_wave[0]

  ;resample the bandpass at the irradiance wavelengths
  bandpass=interpol(bandpass_,b_wave,i_wave)

  if n_elements(max_val) gt 0 then begin
    bandpass*=double(max_val)/max(bandpass)
  end else if n_elements(norm_val) gt 0 then begin
    area=total(bandpass)*delta_lambda
    bandpass*=double(norm_val)/area    
  end

  ;Deal with the case where the irradiance wavelength range is wider 
  ;than the bandpass wavelength range
  w=where(i_wave lt min(b_wave),nw)
  if nw gt 0 then bandpass[w]=0
  w=where(i_wave gt max(b_wave),nw)
  if nw gt 0 then bandpass[w]=0
  
  ;Decide if we are going to do it. If there is fill data in a high 
  ;part of the bandpass, skip it. Fill data will always be -1, so this
  ;finds spots where the irradiance is negative and the bandpass is
  ;more than the given threshold
  w=where(irradiance*bandpass lt -1e-2,count)
  if count gt 0 and ~keyword_set(neg) then begin
    ;message,/info,"Bad data in band "
    return,-1
  end

  ;We've decided to do it now, so fill in the -1 with 0
  w=where(irradiance lt 0,count)
  if count gt 0 then irradiance[w]=0

  ;Do the actual integration
  if keyword_set(unc) then begin
;Uncertainty propagation:: 
;  f=delta*total(weight*value)
;   =delta*h
;  h=total(weight*value)=weight_0*value_0+weight_1*value_1+...+weight_n*value_n
;  sig_h^2=weight_0^2*sig_0^2+weight_1^2*sig_1^2+...+weight_n^2*sig_n^2+[correlation term assumed 0]
;         =total(weight^2*sig^2)
;  sig_h=sqrt(total(weight^2*sig^2))
;  sig_f^2=delta^2*sig_h^2
;  sig_f=sqrt(delta^2*sig_h^2)
;       =delta*sig_h
;       =delta*sqrt(total(weight^2*sig^2))
;       
;In english, stdev of integration is grid spacing times RSS of product of the weighting and the stdev of measurements
    return,delta_lambda*sqrt(total(irradiance^2*bandpass^2))
  end else begin
    return,delta_lambda*total(irradiance*bandpass)
  end
end

