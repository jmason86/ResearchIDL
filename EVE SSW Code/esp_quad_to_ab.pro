;docformat = 'rst'
;+
;Calculate solar angles alpha and beta from EVE ESP quadrant data
; :Params: 
;  diodes: in, required
;     calibrated/dark-corrected diode values. Can be quadrant fraction. These values are normalized before use.
;     Array of 4 values, index 0 to 3 match up with quadrant diode names Q0-Q3
;     
; :Keywords:
;  alp: out
;     Solar alpha angle (E/W) in arcminutes. Positive when center of brightness is east/left of ESP axis
;  Bet: out
;     Solar beta angle (N/S) in arcminutes. Positive when center of brightness is north/above ESP axis
;-
pro esp_quad_to_ab,diodes,alp=alp,bet=bet
;ESP channels (From the right point of view it makes sense)
;                    +-----+-----+
;  +-----+-----+-----|5 Q0 |6 Q1 |-----+-----+                    ^^^^^^^^^
;  |1 366|2 257|3 Drk|-----+-----|8 171|9 304|   Solar North-->   East/Left
;  +-----+-----+-----|4 Q2 |7 Q3 |-----+-----+
;                    +-----+-----+
;channel name
;1       366
;2       257
;3       dark
;4       q2
;5       q0
;6       q1
;7       q3
;8       171
;9       304
;
;channel name
;5       q0
;6       q1
;4       q2
;7       q3
;8       171
;2       257
;9       304
;1       366
;3       dark
  ;Normalize diodes to sum to 1.0. This doesn't hurt if the diodes are already normalized.
  q=total(diodes[0:3])
  q0=diodes[0]/q
  q1=diodes[1]/q
  q2=diodes[2]/q
  q3=diodes[3]/q

  ;Quadrant diode coordinates in E/W (qy) and N/S (qx) directions
  qx=(q1+q3)-(q2+q0)
  qy=(q0+q1)-(q2+q3)

  ;Convert diode coordinates to arcminutes in E/W (alpha) and N/S directions
  alp_m=68.5752
  alp_b=1.45181
  bet_m=20.347
  bet_b=0.420920
  alp=alp_m*qy+alp_b
  bet=bet_m*qx+bet_b

end
