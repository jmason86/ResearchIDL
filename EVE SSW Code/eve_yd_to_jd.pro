;docformat = 'rst'
;
;+
;:Categories:
;  time
;-
FUNCTION eve_yd_to_jd, yd
;
; $Id: yd_to_jd.pro,v 1.1.1.1 2009/05/28 21:58:01 evesdp Exp $
;
; Returns the double-precision Julian Day Number,
; given a Gregorian date of the form yyyyddd.ddd.

; B. G. Knapp, 87/02/04

  y = LONG(yd/1000)+9999L
  d = (ABS(yd) MOD 1000)-1931000.5D0
  RETURN,LONG(y*365.25D0)+y/400-y/100+d

END
