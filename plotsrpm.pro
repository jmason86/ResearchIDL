; Program to plot SRPM spectra only. Originally designed for viewing full-res spectra and identifiying precise line wavelengths. 
;
; James Paul Mason
; 2012/1/5

PRO PlotSRPM

; read in data
dataLoc = '/Users/jmason86/Dropbox/Research/Fontenla/Data/SRPM/Spectra/SRPM2/FullRes/'

readcol,dataloc+'EUV3Cont01.copt0.model1.tab',point,wave,intensity1,tbright,index1,index2,percent,SKIPLINE=5,/SILENT
readcol,dataloc+'EUV3Cont01.copt0.model2.tab',point,wave,intensity2,tbright,index1,index2,percent,SKIPLINE=5,/SILENT

; combine 0.8*model1 + 0.2*model2
intensity = 0.8*intensity1 + 0.2*intensity2

;produce plot
p1=plot(wave,intensity,'b2',/ylog,xrange=[200,210],$
        title='SRPM Spectrum',$
        xtitle='Wavelength (nm)',$
        ytitle='SSI (W m$^{-2}$ nm$^{-1}$)')



END
