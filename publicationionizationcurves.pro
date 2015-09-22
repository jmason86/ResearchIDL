; Program to create publication quality ionization curve plots. 
; Producing these plots for my paper with Juan at the end of 2011. 
;
; James Paul Mason
; 2011/11/15

PRO publicationIonizationCurves

; import data
dataloc = '/Users/jmason86/Dropbox/Research/Data/CHIANTI/SRPMIonizCurves_basedonCHIANTIversions/'
readcol,dataloc+'Ne_CH5.dat',ch5ne1,ch5ne2,ch5ne3,ch5ne4,ch5ne5,ch5ne6,ch5ne7,ch5ne8,ch5ne9,ch5ne10,format='d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'Ne_CH6.dat',ch6ne1,ch6ne2,ch6ne3,ch6ne4,ch6ne5,ch6ne6,ch6ne7,ch6ne8,ch6ne9,ch6ne10,format='d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'P_CH5.dat',ch5p1,ch5p2,ch5p3,ch5p4,ch5p5,ch5p6,ch5p7,ch5p8,ch5p9,ch5p10,ch5p11,ch5p12,ch5p13,ch5p14,ch5p15,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'P_CH6.dat',ch6p1,ch6p2,ch6p3,ch6p4,ch6p5,ch6p6,ch6p7,ch6p8,ch6p9,ch6p10,ch6p11,ch6p12,ch6p13,ch6p14,ch6p15,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'Ar_CH5.dat',ch5ar1,ch5ar2,ch5ar3,ch5ar4,ch5ar5,ch5ar6,ch5ar7,ch5ar8,ch5ar9,ch5ar10,ch5ar11,ch5ar12,ch5ar13,ch5ar14,ch5ar15,ch5ar16,ch5ar17,ch5ar18,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'Ar_CH6.dat',ch6ar1,ch6ar2,ch6ar3,ch6ar4,ch6ar5,ch6ar6,ch6ar7,ch6ar8,ch6ar9,ch6ar10,ch6ar11,ch6ar12,ch6ar13,ch6ar14,ch6ar15,ch6ar16,ch6ar17,ch6ar18,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'Fe_CH5.dat',ch5fe1,ch5fe2,ch5fe3,ch5fe4,ch5fe5,ch5fe6,ch5fe7,ch5fe8,ch5fe9,ch5fe10,ch5fe11,ch5fe12,ch5fe13,ch5fe14,ch5fe15,ch5fe16,ch5fe17,ch5fe18,ch5fe19,ch5fe20,ch5fe21,ch5fe22,ch5fe23,ch5fe24,ch5fe25,ch5fe26,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'Fe_CH6.dat',ch6fe1,ch6fe2,ch6fe3,ch6fe4,ch6fe5,ch6fe6,ch6fe7,ch6fe8,ch6fe9,ch6fe10,ch6fe11,ch6fe12,ch6fe13,ch6fe14,ch6fe15,ch6fe16,ch6fe17,ch6fe18,ch6fe19,ch6fe20,ch6fe21,ch6fe22,ch6fe23,ch6fe24,ch6fe25,ch6fe26,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'Ni_CH5.dat',ch5ni1,ch5ni2,ch5ni3,ch5ni4,ch5ni5,ch5ni6,ch5ni7,ch5ni8,ch5ni9,ch5ni10,ch5ni11,ch5ni12,ch5ni13,ch5ni14,ch5ni15,ch5ni16,ch5ni17,ch5ni18,ch5ni19,ch5ni20,ch5ni21,ch5ni22,ch5ni23,ch5ni24,ch5ni25,ch5ni26,ch5ni27,ch5ni28,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent
readcol,dataloc+'Ni_CH6.dat',ch6ni1,ch6ni2,ch6ni3,ch6ni4,ch6ni5,ch6ni6,ch6ni7,ch6ni8,ch6ni9,ch6ni10,ch6ni11,ch6ni12,ch6ni13,ch6ni14,ch6ni15,ch6ni16,ch6ni17,ch6ni18,ch6ni19,ch6ni20,ch6ni21,ch6ni22,ch6ni23,ch6ni24,ch6ni25,ch6ni26,ch6ni27,ch6ni28,format='d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d,d',/silent

; create temperature array -- taken from TestIonizationMcad.mcd Matchad spreadsheet
Temp = dblarr(201)
FOR i=0,200 DO Temp(i) = 4000 * exp(i*0.04)


; plot Fe data
p1 = plot(Temp,ch5fe2,'2', yrange=[0,1],xrange=[1E3,1E7],/xlog,$
          title='Comparison of Fe Ionization Curves based on CHIANTI 5,6',$
          xtitle='Temperature [K]',$
          ytitle='Ionization Fraction',$
          NAME='CHIANTI 5')
p2 = plot(Temp,ch6fe2, '2*-',/overplot,$
          NAME='CHIANTI 6')
p3 = plot(Temp,ch5fe4,'2',/overplot)
p4 = plot(Temp,ch6fe4,'2*-',/overplot)
p5 = plot(Temp,ch5fe9,'2',/overplot)
p6 = plot(Temp,ch6fe9,'2*-',/overplot)
p7 = plot(Temp,ch5fe15,'2',/overplot)
p8 = plot(Temp,ch6fe15,'2*-',/overplot)

text1 = text(0.29,0.83,'Fe II')
text2 = text(0.4,0.7,'Fe IV')
text3 = text(0.6,0.5,'Fe IX')
text4 = text(0.75,0.38,'Fe XV')

leg1 = legend(target=[p1,p2],position=[0.705,0.87])

p1.save, dataloc+'Fe.png'
p1.close

; plot Ni data
p99 = plot(Temp,ch5ni2,'2', yrange=[0,1],xrange=[1E3,1E7],/xlog,$
          title='Comparison of Ni Ionization Curves based on CHIANTI 5,6',$
          xtitle='Temperature [K]',$
          ytitle='Ionization Fraction',$
          NAME='CHIANTI 5')
p98 = plot(Temp,ch6ni2, '2*-',/overplot,$
          NAME='CHIANTI 6')
p97 = plot(Temp,ch5ni8,'2',/overplot)
p96 = plot(Temp,ch6ni8,'2*-',/overplot)
p95 = plot(Temp,ch5ni11,'2',/overplot)
p94 = plot(Temp,ch6ni11,'2*-',/overplot)
p93 = plot(Temp,ch5ni19,'2',/overplot)
p92 = plot(Temp,ch6ni19,'2*-',/overplot)

text99 = text(0.27,0.83,'Ni II')
text98 = text(0.6,0.78,'Ni VIII')
text97 = text(0.7,0.5,'Ni XI')
text98 = text(0.82,0.65,'Ni XIX')

leg99 = legend(target=[p99,p98],position=[0.705,0.87])

p99.save, dataloc+'Ni.png'
p99.close

; plot P data
pp1 = plot(Temp,ch5p2,'2', yrange=[0,1],xrange=[1E3,1E7],/xlog,$
          title='Comparison of P Ionization Curves based on CHIANTI 5,6',$
          xtitle='Temperature [K]',$
          ytitle='Ionization Fraction',$
          NAME='CHIANTI 5')
pp2 = plot(Temp,ch6p2, '2*-',/overplot,$
          NAME='CHIANTI 6')
pp3 = plot(Temp,ch5p4,'2',/overplot)
pp4 = plot(Temp,ch6p4,'2*-',/overplot)
pp5 = plot(Temp,ch5p7,'2',/overplot)
pp6 = plot(Temp,ch6p7,'2*-',/overplot)


textp1 = text(0.27,0.83,'P II')
textp2 = text(0.5,0.8,'P IV')
textp3 = text(0.63,0.64,'P XII')

legp1 = legend(target=[pp1,pp2],position=[0.705,0.87])

pp1.save, dataloc+'P.png'
pp1.close

; plot Ne data
pne1 = plot(Temp,ch5ne2,'2', yrange=[0,1],xrange=[1E3,1E7],/xlog,$
          title='Comparison of Ne Ionization Curves based on CHIANTI 5,6',$
          xtitle='Temperature [K]',$
          ytitle='Ionization Fraction',$
          NAME='CHIANTI 5')
pne2 = plot(Temp,ch6ne2, '2*-',/overplot,$
          NAME='CHIANTI 6')
pne5 = plot(Temp,ch5ne4,'2',/overplot)
pne6 = plot(Temp,ch6ne4,'2*-',/overplot)
pne7 = plot(Temp,ch5ne5,'2',/overplot)
pne8 = plot(Temp,ch6ne5,'2*-',/overplot)
pne9 = plot(Temp,ch5ne6,'2',/overplot)
pne10 = plot(Temp,ch6ne6,'2*-',/overplot)
pne13 = plot(Temp,ch5ne7,'2',/overplot)
pne14 = plot(Temp,ch6ne7,'2*-',/overplot)
pne15 = plot(Temp,ch5ne9,'2',/overplot)
pne16 = plot(Temp,ch6ne9,'2*-',/overplot)
pne17 = plot(Temp,ch5ne10,'2',/overplot)
pne18 = plot(Temp,ch6ne10,'2*-',/overplot)

textne1 = text(0.35,0.75,'Ne II')
textne2 = text(0.5,0.68,'Ne IV')
textne3 = text(0.6,0.62,'Ne V')
textne4 = text(0.63,0.57,'Ne VI')
textne5 = text(0.66,0.48,'Ne VII')
textne6 = text(0.66,0.85,'Ne IX')
textne7 = text(0.85,0.5,'Ne X')

legne1 = legend(target=[pne1,pne2],position=[0.15,0.87])

pne1.save, dataloc+'Ne.png'
pne1.close


; plot Ar data
par1 = plot(Temp,ch5ar4,'2', yrange=[0,1],xrange=[1E4,1E7],/xlog,$
          title='Comparison of Ar Ionization Curves based on CHIANTI 5,6',$
          xtitle='Temperature [K]',$
          ytitle='Ionization Fraction',$
          NAME='CHIANTI 5')
par2 = plot(Temp,ch6ar4, '2*-',/overplot,$
          NAME='CHIANTI 6')
par5 = plot(Temp,ch5ar5,'2',/overplot)
par6 = plot(Temp,ch6ar5,'2*-',/overplot)
par7 = plot(Temp,ch5ar6,'2',/overplot)
par8 = plot(Temp,ch6ar6,'2*-',/overplot)
par9 = plot(Temp,ch5ar16,'2',/overplot)
par10 = plot(Temp,ch6ar16,'2*-',/overplot)

textar1 = text(0.4,0.74,'Ar IV')
textar2 = text(0.47,0.67,'Ar V')
textar3 = text(0.52,0.48,'Ar VI')
textar4 = text(0.8,0.38, 'Ar XVI')

legp1 = legend(target=[par1,par2],position=[0.705,0.87])

par1.save, dataloc+'Ar.png'
par1.close
END

