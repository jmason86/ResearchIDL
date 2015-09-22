; Gamma determined by looking at median value per mu ring in 2007 and 2001 images,
; taking the log of the 2007 median intensity (because the 2001 images are already log),
; then finding a gamma that made them look the same: I2007 = I2001^gamma
; 
; James Paul Mason
; 4/22/11 

PRO findgamma

;read in histog tables -- median is the only required column
readcol, '/Users/jmason86/Dropbox/Research/Data/LevelsDetermination/MeudonK3histog20010616.tab',$
        SKIPLINE=3,index,mu,median2001,average,area,muhi,mulo, /SILENT
readcol, '/Users/jmason86/Dropbox/Research/Data/LevelsDetermination/MeudonK3histog20070510.tab',$
        SKIPLINE=3,index,mu,median2007,average,area,muhi,mulo, /SILENT

;;convert 2007 intensity to ln(intensity) to match 2001
;median2007 = alog(median2007)
;
;;free parameters
;gamma1 = [0.903,0.902,0.902,0.901,0.9,0.899,0.899,0.897,0.897,0.896]
;alpha = 0.43
;beta1 = alog(alpha)
;
;;alpha,gamma scaling equation
;median2001scaled = gamma1 * (median2007 - beta1)
;
;;plot comparison to get discreet gammas and alpha right.
;p1=plot(mu, median2007,xrange=[max(mu),min(mu)], 'r2',$
;       title='Scaling Meudon 2001 to Meudon 2007 - Discrete Gamma',$
;       xtitle='mu',$
;       ytitle='log Intensity',$
;       NAME='2007')
;p2=plot(mu, median2001scaled,xrange=[max(mu),min(mu)], 'g2',/OVERPLOT,$
;        NAME='2001 Scaled')
;leg=legend(target=[p1,p2],position=[0.65,0.85])
;
;;fit a function to the discreet gamma vector
;coeff = poly_fit(mu,gamma1,2)
;
;;use line fit to generate a new gamma and intensities
;gammafit = coeff(2)*mu^2 + coeff(1)*mu + coeff(0)
;median2001scaled2 = gammafit * (median2007 - beta1)
;
;
;;plot comparison of 2007/2001scaled with gammafit
;p1=plot(mu, median2007,xrange=[max(mu),min(mu)], 'r2',$
;       title='Scaling Meudon 2001 to Meudon 2007 - Gamma Fit',$
;       xtitle='mu',$
;       ytitle='log Intensity',$
;       NAME='2007')
;p2=plot(mu, median2001scaled2,xrange=[max(mu),min(mu)], 'g2',/OVERPLOT,$
;        NAME='2001 Scaled GammaFit')
;leg=legend(target=[p1,p2],position=[0.55,0.85])
;
;print, coeff

Inewlog = alog(median2007)
dens = median2001

gamma2 = deriv(median2007,median2001)
coeff = poly_fit(median2007(0:7),gamma2(0:7),2)
gamma3 = coeff(2)*median2007 + coeff(1)*median2007 + coeff(0)

;scale old density into intensity
alpha=0.7
Int2001scaled = alpha*exp(dens/(gamma3*median(dens))) * median(dens)

p1=plot(median2007,'r2')
p2=plot(Int2001scaled,'g2',/overplot)


p3=plot(Inewlog,median2001,'r2',$
        xtitle='Log Intensity 2007',$
        ytitle='Density 2001')
        




STOP

END