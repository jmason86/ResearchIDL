PRO ffd_equation_exploration

npts = 100

A = 1
alpha = jpmrange(0., 4., npts=npts)
replace = closest(2, alpha)
alpha[replace] = 2.0
W = jpmrange(1d0, 1d4, npts=npts)


;FOR i = 0, npts-1 DO BEGIN
;  yar = A * W^(-alpha[i])
;  IF i EQ 0 THEN BEGIN
;    p = plot(W, yar)
;  ENDIF ELSE BEGIN
;    p2 = plot(W, yar, /OVERPLOT, color=JPMColors(i))
;  ENDELSE
;  p.xlog = 1
;  p.ylog = 1
;ENDFOR

Wmin = 1d30
Wmax = 1d32
power = A / (-alpha + 2) * ( Wmax^(-alpha + 2) - Wmin^(-alpha + 2))

p = plot(alpha, power, $
         xtitle='alpha', $
         ytitle='power', /YLOG)


big = abs(A/(-alpha + 2) * (Wmax^(-alpha + 2)))
small = abs(A/(-alpha + 2) * (Wmin^(-alpha + 2)))

p = plot(alpha, big, $
         xtitle='alpha',$
         ytitle='W',/YLOG)
p2 = plot(alpha, small, /OVERPLOT, 'r')


STOP

END 