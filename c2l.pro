;First program to generate plots for AIA center to limb variation

PRO c2l

;==================================================================================================;
; Read input and arrange properly 
data_loc = '/Users/jmason86/Dropbox/Research/Data/CenterToLimb/'

readcol, data_loc+'AIA211ConvMerg.tab', a,b,c,d,e,f,g,h,i,j,k,$
         format='a,a,a,a,a,a,a,a,a,a,a',/silent
AIA1600 = readfits(data_loc+'compact_aia_test.lev1.2010-08-13T12-49-54.57Z.1600.image_lev1.fits', head)
AIA211 = readfits(data_loc+'compact_aia_test.lev1.2010-08-13T18-49-50.07Z.211.image_lev1.fits',head2)

mu = float([b(0),c(0),d(0),e(0),f(0),g(0),h(0),i(0),j(0),k(0)]) ; mu = 1/cosø
model1 = float([b(1),c(1),d(1),e(1),f(1),g(1),h(1),i(1),j(1),k(1)])
model2 = float([b(2),c(2),d(2),e(2),f(2),g(2),h(2),i(2),j(2),k(2)])
model3 = float([b(3),c(3),d(3),e(3),f(3),g(3),h(3),i(3),j(3),k(3)])
model4 = float([b(4),c(4),d(4),e(4),f(4),g(4),h(4),i(4),j(4),k(4)])
model5 = float([b(5),c(5),d(5),e(5),f(5),g(5),h(5),i(5),j(5),k(5)])
model6 = float([b(6),c(6),d(6),e(6),f(6),g(6),h(6),i(6),j(6),k(6)])
;--------------------------------------------------------------------------------------------------;

;==================================================================================================;
;Generate plots

x = range(0.,1.,npts=1024) ;vector spanning values of mu

p0 = plot(x, AIA211[1024,1024:2047], $
          xtitle='!9m', ytitle='Intensity',$
          title='Center to Limb Variation', $
          NAME='AIA')
p1 = plot(mu, model1*15, 'r2', /OVERPLOT, NAME = 'Model 1 *15')
p2 = plot(mu, model2*2, 'b2', /OVERPLOT, NAME = 'Model 2 *2')
p3 = plot(mu, model3, 'g2', /OVERPLOT, NAME = 'Model 3 *1')
p4 = plot(mu, model4*0.4, 'y2', /OVERPLOT, NAME = 'Model 4 *0.4')
p5 = plot(mu, model5*0.2, 'm2', /OVERPLOT, NAME = 'Model 5 *0.2')
p6 = plot(mu, model6*0.15, 'c2', /OVERPLOT, NAME = 'Model 6 *0.15')

leg = legend(TARGET=[p0,p1,p2,p3,p4,p5,p6], POSITION=[0.6, 800], /DATA)

;--------------------------------------------------------------------------------------------------;

STOP
END