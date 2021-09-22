PRO ffd_equation_exploration

; Plot setup
fontsize = 16
fontcolor = 'white'
backgroundcolor = 'black'

npts = 100

A = 1d40
alpha = jpmrange(0., 4., npts=npts)
replace = closest(2, alpha)
alpha[replace] = 2.0
W = jpmrange(1d24, 1d36, npts=npts)

alf = [4.0, 2.0, 0.5]
power1 = A * W^(-alf[0])
power2 = A * W^(-alf[1])
power3 = A * W^(-alf[2])

; Renormalize for direct comparison of slopes (alphas)
power1 /= (power1[0] / power2[0])
power3 /= (power3[0] / power2[0]) 

win = window(BACKGROUND_COLOR = backgroundColor)
p1 = plot(W, power1, font_size=fontsize, font_color=fontcolor, background_color=backgroundcolor, 'dodger blue', thick=2, /CURRENT, $
          title='Understanding $\alpha$', $
          xtitle='Total Energy [ergs]', /XLOG, xcolor=fontcolor, $
          ytitle='Frequency (per year per erg)', /YLOG, ycolor=fontcolor, $
          name='$\alpha$=' + JPMPrintNumber(alf[0]))
p2 = plot(W, power2, thick=2, color=fontcolor, /OVERPLOT, $
          name='$\alpha$=' + JPMPrintNumber(alf[1]))
p3 = plot(W, power3, thick=2, 'tomato', /OVERPLOT, $
          name='$\alpha$=' + JPMPrintNumber(alf[2]))
t1 = text(0.5, 0.51, '$\alpha$=' + JPMPrintNumber(alf[0]), color=p1.color, font_size=fontsize)
t2 = text(0.5, 0.65, '$\alpha$=' + JPMPrintNumber(alf[1]), color=p2.color, font_size=fontsize)
t3 = text(0.5, 0.75, '$\alpha$=' + JPMPrintNumber(alf[2]), color=p3.color, font_size=fontsize)
;p1.save, 'energy_alpha.png', /TRANSPARENT
STOP

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

STOP
big = abs(A/(-alpha + 2) * (Wmax^(-alpha + 2)))
small = abs(A/(-alpha + 2) * (Wmin^(-alpha + 2)))

p = plot(alpha, big, $
         xtitle='alpha',$
         ytitle='W',/YLOG)
p2 = plot(alpha, small, /OVERPLOT, 'r')


STOP

END 