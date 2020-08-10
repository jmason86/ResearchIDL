pro colors8

;EXEMPLES :
; 
; XloadCT
; colors8
; XloadCT
; xx = vect(-2*!DPI,2*!DPI, 500, /n_el)
; window, 0, ret=2
; plot, xx, sin(xx), background = 0, color = 1
; oplot, xx, sin(xx-1), color = 2
; oplot, xx, sin(xx-2), color = 3
; oplot, xx, sin(xx-3), color = 4
; oplot, xx, sin(xx-4), color = 5
; oplot, xx, sin(xx-5), color = 6
; oplot, xx, sin(xx-6), color = 7
; loadCT,0
; XloadCT

; [0-black, 1-white, 2-red, 3-green, 4-blue, 5-yellow, 6-cyan, 7-magenta]

red =   [0, 1, 1, 0, 0, 1, 0, 1]
green = [0, 1, 0, 1, 0, 1, 1, 0]
blue =  [0, 1, 0, 0, 1, 0, 1, 1]
tvlct, 255*red, 255*green, 255*blue

return
end