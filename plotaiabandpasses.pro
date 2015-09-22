; Program to plot the AIA bandpasses as a function of wavelength. 
; 
; REQUIREMENTS: Solar software AIA package
;
; Extracted from Rachel Hock. 
; 
; James Paul Mason
; 2012/2/10

PRO PlotAIABandPasses

; Retrieve AIA bandpasses
aia = aia_get_response(/area)

; Plot
FOR i = 0, 6 DO p = plot(aia.wave/10., aia.all[i,*]/total(aia.all[i,*]>0)*5e-2 > 10E-10, COLOR = JPMColors(i, /SIMPLE), THICK = 6, $
                         /YLOG)
END