;docformat = 'rst'

;+
;Physical constants lookup function. All EVE functions which need physical 
;constants should call this function rather than have them hard-coded. 
;
;:Keywords:
;  planck_constant: out, optional
;    Planck's constant in SI units (J/Hz)
;  speed_of_light: out, optional
;    Vacuum speed of light in SI units (m/s)
;
;Authority for these constants is http://physics.nist.gov/cuu/Constants/index.html
;-
pro eve_physical_constant,planck_constant=planck_constant,speed_of_light=speed_of_light
  planck_constant=6.62606957d-34 ;J/Hz, 6.62606957(29) x10^-34 J s, from http://physics.nist.gov/cgi-bin/cuu/Value?h
  speed_of_light=299792458d ;m/s, exact by definition of meter, http://physics.nist.gov/cgi-bin/cuu/Value?c
end
  