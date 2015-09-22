;docformat = 'rst'

;+
;Convenience function to call a user function on a bunch of spectra and gather its results. 
;:Params:
;  f: in, required
;      String name of function to call with each spectrum. This function must use the following prototype:
;         function your_function,irradiance,wave,any_other_parameters=any_other_parameters.
;           irradiance is a 1D array of spectral irradiance in W/m^2/nm
;           wave is a 1D array of wavelength scale in nm, will match size of irradiance
;           You may have any number of named parameters, and any named parameters not recognized by 
;           eve_process_spectrum() will be passed to your function by the _extra= convention. 
;  data_: in, required
;    Irradiance data. May be:
;     *L2 or L3 spectrum structure as read by eve_read_whole_fits() or read_latest_eve()
;     *Just the spectrum part of L2 or L3 (not including the metadata) such as returned by eve_merge_evs()
;     *A plain 1D or 2D array of spectra. If 2d, a single spectrum j must be represented by data[j,*]
;  wave_: in, optional
;    Wavelength scale if needed. If present, must be a 1D array and match size of spectrum. Only
;    needed if data is a plain array and none of the switches below are passed
;    
;:keywords:
;  megsa: in, optional
;  megsb: in, optional
;  l2: in, optional
;    set these switches to use the Level 1 MEGS-A, Level 1 MEGS-B, or Level 2 wavelength scales.
;    If you set one of these, you don't need to pass in wave.
;  _extra: in, optional
;    any other named parameters will be passed to function f()
;  
;:Returns:
;  An array of results. If your function returns a 1D numeric array result, the return value will be a 2D array.
;  If your function returns a structure, the result will be a 1D array of structures. If your function
;  returns a single number, the result will be a 1D array.
;
;Example - The following code loads 10 days worth of spectra, averages it to 1-minute, then calculates
;the stan bands for each 1-minute average spectrum::
; result=eve_merge_evs(2012001,2012010,n=6)
; stan_b=eve_process_spectrum('stan_bands',result,/photons)
; 
;:Categories:
;  user
;-
function eve_process_spectrum,f,data_,wave_,megsa=megsa,megsb=megsb,l2=l2,_extra=extra
  
  ;Set the default wavelength scale if needed
  @megs_wave_defines.pro
  if n_elements(wave_) eq 0 then begin
    if keyword_set(megsa) then begin
      wave_=megsa_L1_wave
    end else if keyword_set(megsb) then begin
      wave_=megsb_L1_wave
    end else if keyword_set(l2) then begin
      ;default is /l2
      wave_=megs_l2_wave
      l2=1 ;so that eve_integrate_line gets the default if needed
    end
  end
  case size(data_,/type) of
    8:begin
      ;It's a structure - try to find out what kind
      tn=tag_names(data_)
      w_spectrum=where(strupcase(tn) eq "SPECTRUM",spectrum_count)
      w_spectrummeta=where(strupcase(tn) eq "SPECTRUMMETA",spectrummeta_count)
      w_irradiance=where(strupcase(tn) eq "IRRADIANCE",irradiance_count)
      if spectrum_count gt 0 then begin
        ;It's an L2/L3 structure
        data=data_.spectrum.irradiance
        if spectrummeta_count gt 0 then begin
          wave=data_.spectrummeta.wavelength
        endif else begin
          if n_elements(wave_) gt 0 then begin
            wave=wave_
          endif else begin
            message,"L2/L3 spectrum structure found, but no wavelength scale found"
          endelse
        endelse
      endif else if irradiance_count gt 0 then begin
        ;It's the Spectrum part of an L2/L3 structure
        data=data_.irradiance
        if n_elements(wave_) gt 0 then begin
          wave=wave_
        endif else begin
          s=size(data,/dim)
          if s[0] eq 5200 then begin
            wave=dindgen(5200)*0.02+3.01
          end else begin
            message,"spectrum structure found, but no wavelength scale could be deduced"
          end
        endelse
      endif
      s=size(data,/dim)
      result={result:call_function(f,data[*,0],wave,_extra=extra)}
      result=make_array(s[1],value=result)
      for i=1,s[1]-1 do result[i]={result:call_function(f,data[*,i],wave,_extra=extra)}
      return,result.result
    end
    else:begin
      ;It's just an array of spectrum (spectra).
      n=size(data_,/n_dim)
      s=size(data,/dim)
      if n eq 1 then begin
        ;Just a 1D array - call the function
        return,call_function(f,data_,wave_,_extra=extra)  
      endif else begin
        ;See if one of the dimensions match the wavelength scale
        message,'Multiple spectra not yet supported'
      endelse
      
    end
  endcase
end