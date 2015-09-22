;+
; HISTORY:
;       PSH, 2009/01/13
;
;  PURPOSE:
;       Gives alpha (heliocentric) angle in radians
;
; INPUT:
;       xy: 2xN array of angles from sun center, in arcseconds. [0,*]: x;[1,*]: y
;       time: in any of the anytim formats
;
; EXAMPLE:
;       PRINT, psh_xy2alpha([0,0],'2002/02/26')*!RADEG
;
;-
FUNCTION xy2alpha, xy, time

       lonlat=xy2lonlat(xy,anytim(time))/!RADEG

       RETURN, acos(sin(!DPI/2.-lonlat[1,*])*cos(lonlat[0,*]))

END