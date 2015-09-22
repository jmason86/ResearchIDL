;docformat = 'rst'
;+
;:returns:
; irradiance in watts/m^2/nm given photons/cm^2/sec/nm
;
;:Params:
;  wave: in, required
;   array of wavelengths in nm
;  irr_ph: in, required
;   array of irradiances in photons/cm^2/sec/nm
; 
;:Keywords:
;  inverse: in, optional
;    if set, input and output units are reversed: input is in W/m^2/nm and output is in ph/s/cm^2/nm
;
;:History:
;  02-17-03 DLW Original file creation.
;
;idver='$Id: ph2watt.pro,v 3.0 2011/03/22 15:24:53 dlwoodra Exp $'
;
;-
function eve_ph2watt, wave, irr_ph, inverse=inverse

if n_params() ne 2 then begin
  print,' Usage: irr_w = ph2watt( wave, irr_ph, [/inverse] )'
  print,'  where'
  print,'    wave is the wavelength in nm (vector or scalar)'
  print,'    irr_ph is irradiance in photons/cm^2/second/nm (same length)'
  print,'    irr_w is irradiance in watts/m^2/nm (same length as inputs)'
  print,'    /inverse performs inverse calculation, irr_ph is in W/m^2/nm, irr_w is in ph/cm^2/s/nm'
  message,"Incorrect number of parameters"
endif

if n_elements(wave) ne n_elements(irr_ph) then begin
  message,'Wave and irr_w have different number of elements'
endif

;energy conversion factor = Planck's constant * speed of light in vaccum
eve_physical_constant,planck_constant=h,speed_of_light=c

  conv_factor=1d5*wave/(h*c) ;1d5 factor rolls in m^2->cm^2 factor and nm->m factor for wavelength
                             ;Multiply a number expressed in joules by this factor to get a number in photons 
                             ;(or a number in watts to get photons/second)

  if ~keyword_set(inverse) then begin
    ;forward conversion ph->J
    return, irr_ph/conv_factor
  end else begin
    ;reverse conversion J->ph
    return, irr_ph*conv_factor
  end

end
