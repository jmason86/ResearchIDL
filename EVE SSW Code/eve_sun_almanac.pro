;docformat = 'rst'
;+
;Matlab modulus function. Handles negative numbers the "proper" way, the same way that matlab does.
;For instance, if you have 8 mod 5, the answer is 3, and all modulus functions do it this way. On the
;other hand, implementations disagree on -3 mod 5. If the denominator is positive, the sensible thing
;is to have the result be between 0 and almost the denominator. However, most implementations including
;IDL mod will give a negative result. 
;
;:Params:
;  x: in, required
;    numerator
;  y: in, required
;    denominator
;  
;:Returns:
; Returns x mod y, and result shares sign of y. Referred to as "floored division" by Knuth. 
; 
;:Private:
;-
function eve_sun_almanac_mlmod,x,y
  return,x - floor(double(x)/double(y))*y
end

;+
;Low precision formulas for the Sun's coordinates and the equation of time
;From 2002 Astronomical Almanac, p. C24
;
;"The following formulas give the apparent coordinates of the Sun to a precision of 0.01deg and
;the equation of time to a precision of 0.1min between 1950 and 2050; on this page the time
;argument n is the number of days from J2000"
;
;:Params:
; JD: in, required
;   Julian Date, UTC time scale. May be scalar or 1D array
;:Returns:
;   a unit vector from Earth to Sun in GCI frame
;:Keywords:
; R: out, optional
;    distance from Earth to Sun in AU
; B0: out, optional
;    Heliocentric latitude of sub-Earth point in degrees. This is the Sun's "nodding angle" in SNR. Positive if the 
;    northern hemisphere is tilted towards the earth, negative if southern.
; ALPHA: out, optional
;    Right ascension of Sun in degrees
; DELTA: out, optional
;    Declination of Sun in degrees
; LAMBDA: out, optional
;   Ecliptic longitude of Sun in degrees (beta, ecliptic latitude, is zero in this low-precision formula)
; ALT_R: out, optional, private
;   Unit vector from Earth to Sun in GCI, calculated by an alternate method. You shouldn't use this, it's
;         for debugging this function only.
;-  
function eve_sun_almanac,JD,R=R,B0=B0,alpha=alpha,delta=delta,lambda=lambda,alt_r=r_alt
  ;Low precision formulas for the Sun's coordinates and the equation of time
  ;From 2002 Astronomical Almanac, p. C24
  ;
  ;The following formulas give the apparent coordinates of the Sun to a precision of 0.01deg and
  ;the equation of time to a precision of 0.1min between 1950 and 2050; on this page the time
  ;argument n is the number of days from J2000
  n=double(JD)-2451545d; =364.5+day of year (from B2-B3) + fraction of day from 0h UT
  L=eve_sun_almanac_mlmod(280.460d +0.9856474d*n,360d)                             ;degrees, Mean longitude of Sun, corrected for aberration;
  g=eve_sun_almanac_mlmod(357.528d +0.9856003d*n,360d)                             ;degrees, Mean Anomaly
                                                                   ;put L and g in the rage 0deg to 360deg by adding multiples of 360deg (eve_sun_almanac_mlmod does this)
  lambda=L+1.915d*sin(g*!dpi/180d)+0.020d*sin(2d*g*!dpi/180d)      ;Ecliptic longitude
  beta=0                                                           ;Eclipticl latitude
  epsilon=23.439d -0.0000004d*n                                    ;Obliquity of axis
  ;alpha=atan(cos(epsilon*!dtor)*tan(lambda*!dtor))                ;Right ascension (in same quadrant as lambda)
  ;Alternatively, alpha may be calculated directly from
  dradeg=180.d/!dpi
  ddtor=1.0d/dradeg
  f=180.d/!dpi
  t=tan(epsilon/2.d*ddtor)^2
  alpha=lambda-f*t*sin(2.d*lambda*ddtor)+(f/2.d)*t^2*sin(4.d*lambda*ddtor) 
  delta=asin(sin(epsilon*ddtor)*sin(lambda*ddtor))*dradeg            ;Declination
  R=1.00014d -0.01671d*cos(g*ddtor)-0.00014*cos(2*g*ddtor)         ;Distance from Sun to Earth, in AU
  ;Equatorial rectangular coordinates of the Sun, in au
  r_alt=r*[[cos(lambda*ddtor)],[cos(epsilon*ddtor)*sin(lambda*ddtor)],[sin(epsilon*ddtor)*sin(lambda*ddtor)]]
  ;Unit vector from Earth to Sun
  result=[[cos(alpha*ddtor)*cos(delta*ddtor)],[sin(alpha*ddtor)*cos(delta*ddtor)],[sin(delta*ddtor)]];
  ;B0 - heliocentric latitude of sub-earth point
  nG=[0.12235,-0.42307,0.89780] ;Solar north pole in GCI
  B0=dblarr(n_elements(JD))
  for i=0,n_elements(B0)-1 do begin
    B0[I]=-asin(total(nG*result[I,*]))*dradeg ;use dot product of earth-sun vector and sun north pole to get angle. Use asin to get 0deg when vectors are perpendicular
  end
  return,result
end
