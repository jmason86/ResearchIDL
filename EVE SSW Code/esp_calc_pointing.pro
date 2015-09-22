;docformat = 'rst'
;+
;Calculate ESP pointing in alpha/beta and heliocentric latitude and longitude. 
;This routine calculates the position of the ESP quadrant diode center
;of brightness in alpha and beta, explained below, and presuming that the spacecraft is 
;properly pointed at the center of the Sun, the corresponding heliocentric latitude and
;longitude of the center of brightness.
; :Params: 
;  diodes: in, required
;     calibrated/dark-corrected diode values. Can be quadrant fraction. These values are normalized before use.
;     Array of 4 values, index 0 to 3 match up with quadrant diode names Q0-Q3
;  jd: in, required
;    julian date of observation
;:Keywords:
;  alpha: out
;     Solar alpha angle (E/W) in arcminutes. Positive when center of brightness is east/left of ESP axis
;  beta: out
;     Solar beta angle (N/S) in arcminutes. Positive when center of brightness is north/above ESP axis
;  latitude: out
;     Heliocentric latitude of ESP center, in degrees, positive north, taking into account the B0 nodding
;  longitude: out
;     Heliocentric longitude of ESP center, in degrees, sub-earth longitude is zero.
;:Categories:
;  utility
;-
pro esp_calc_pointing,diodes,jd,alpha=alp,beta=bet,latitude=lat,longitude=lon
  esp_quad_to_ab,diodes,alp=alp,bet=bet  
  sam_ab_to_ll,jd,alp,bet,lat=lat,lon=lon
 end
