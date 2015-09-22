; Program to produce comparison plots between:
;   SRPM spectrum with CHIANTI5, old ionization
;   SRPM spectrum with CHIANTI6, old ionization
;   SRPM spectrum with CHIANTI6, new ionization
;   EVE spectrum rocket flight (2008, quiet time)
;   EVE spectrum recent data (active time)
;
; CHIANTI6 has only been used to compute the new SRPM coronal spectrum. Do not expect any significant changes
;   to chromosphere, which would also require full non-LTE to run.
; EVE rocket data is in an IDL save file obtained from Rachel Hock. 
; EVE recent data is in specially formatted fits file, obtained from evesci2 machine.
; 
; James Paul Mason
; 8/5/11

PRO spectra_compare

;prep and read data
saveloc = '/Users/jmason86/Dropbox/Research/Data/Comparisons/SRPM_EVE/'
SRPMloc = '/Users/jmason86/Dropbox/Research/Data/SRPM/'
SRPMtmploc = '/Users/jmason86/Dropbox/Research/Data/SRPM/CHIANTIAnalysis/NewCHIANTI/'
EVEloc = '/Users/jmason86/Dropbox/Research/Data/SDO/EVE/'
EVERocketloc = '/Users/jmason86/Dropbox/Research/Data/SDO/EVE/RocketFlights/JF/'

;readcol, SRPMloc+'CHIANTI5_OldIonization_FEUV6Cont11.hopt0.conv1a.txt',point,wave1,intensity1,Tbright,index1,index2,percent, skipline=4
;readcol, SRPMloc+'CHIANTI6_OldIonization_FEUV6Cont11.hopt0.conv1a.txt',point,wave2,intensity2,Tbright,index1,index2,percent, skipline=4

readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI5_Oldionization_Spectrum_model1.tab',point,SRPMCH5Model1Wave,SRPMCH5Model1SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI5_Oldionization_Spectrum_model2.tab',point,SRPMCH5Model2Wave,SRPMCH5Model2SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI6_Oldionization_Spectrum_model1.tab',point,SRPMCH6Model1Wave,SRPMCH6Model1SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI6_Oldionization_Spectrum_model2.tab',point,SRPMCH6Model2Wave,SRPMCH6Model2SSI,Tbright,index1,index2,percent, skipline=4,/silent

readcol, EVERocketloc+'EveSpectrumRocket2008(4).tab',point,EVERocketwave,EVERocketintensity,Tbright,index1,index2,percent,skipline=4,/silent
readcol, EVERocketloc+'EveSpectrumRocket2010May.tab',point,EVERocketwave2,EVERocketintensity2,Tbright,index1,index2,percent,skipline=4,/silent


;Add 0.8Model1 and 0.2Model2 for CHIANTI 5 and 6
SRPMCH5SSI = 0.8*SRPMCH5Model1SSI + 0.2*SRPMCH5Model2SSI
SRPMCH6SSI = 0.8*SRPMCH6Model1SSI + 0.2*SRPMCH6Model2SSI
SRPMCH5wave = SRPMCH5Model1Wave
SRPMCH6wave = SRPMCH6Model1Wave

;produce plot 5-65
p1 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-2],aspect_ratio=3,dimensions=[14400,800],xrange=[5,65],$
         title='EUV Solar Spectrum: Comparison of Computation to Observation',$
         xtitle='Wavelength [nm]',$
         ytitle='SSI [W m$^{-2}$ nm$^{-1}$]',$
         NAME='CHIANTI 5') ;old ionization 
p2 = plot(SRPMCH6wave, SRPMCH6SSI, 'b2', /OVERPLOT,$
          NAME='CHIANTI 6') ;old ionization
;p3 = plot(wave3, intensity3, 'g2', /OVERPLOT,$
;          NAME='CHIANTI6 + New Ionization')
p4 = plot(EVERocketwave, EVERocketintensity, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2008') 
leg = legend(TARGET=[p1,p2,p4],position=[0.14,0.31])
p1.save,saveloc+'CHIANTI5_6_EVE_5-65nm.png',/transparent,resolution=400
p1.close

;produce plot 15-25
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-3],xrange=[15,25],$
         title='EUV Solar Spectrum: Comparison of Computation to Observation',$
         xtitle='Wavelength [nm]',$
         ytitle='SSI [W m$^{-2}$ nm$^{-1}$]',$
         NAME='CHIANTI 5') ;old ionization 
p6 = plot(SRPMCH6wave, SRPMCH6SSI, 'b2', /OVERPLOT,$
          NAME='CHIANTI 6') ;old ionization
;p7 = plot(wave3, intensity3, 'g2', /OVERPLOT,$
;          NAME='CHIANTI6 + New Ionization')
p8 = plot(EVERocketwave2, EVERocketintensity2, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2010') 
leg = legend(TARGET=[p1,p2,p4],position=[0.15,0.3])
p5.save,saveloc+'CHIANTI5_6_EVE_15-25nm.png',/transparent,resolution=400
p5.close

;produce plot 25-35
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-2],xrange=[25,35],$
         title='EUV Solar Spectrum: Comparison of Computation to Observation',$
         xtitle='Wavelength [nm]',$
         ytitle='SSI [W m$^{-2}$ nm$^{-1}$]',$
         NAME='CHIANTI 5') ;old ionization 
p6 = plot(SRPMCH6wave, SRPMCH6SSI, 'b2', /OVERPLOT,$
          NAME='CHIANTI 6') ;old ionization
;p7 = plot(wave3, intensity3, 'g2', /OVERPLOT,$
;          NAME='CHIANTI6 + New Ionization')
p8 = plot(EVERocketwave2, EVERocketintensity2, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2010') 
leg = legend(TARGET=[p1,p2,p4],position=[0.15,0.28])
p5.save,saveloc+'CHIANTI5_6_EVE_25-35nm.png',/transparent,resolution=400
p5.close

;produce plot 35-55
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-3],xrange=[35,55],$
         title='EUV Solar Spectrum: Comparison of Computation to Observation',$
         xtitle='Wavelength [nm]',$
         ytitle='SSI [W m$^{-2}$ nm$^{-1}$]',$
         NAME='CHIANTI 5') ;old ionization 
p6 = plot(SRPMCH6wave, SRPMCH6SSI, 'b2', /OVERPLOT,$
          NAME='CHIANTI 6') ;old ionization
;p7 = plot(wave3, intensity3, 'g2', /OVERPLOT,$
;          NAME='CHIANTI6 + New Ionization')
p8 = plot(EVERocketwave2, EVERocketintensity2, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2010') 
leg = legend(TARGET=[p1,p2,p4],position=[0.63,0.85])
p5.save,saveloc+'CHIANTI5_6_EVE_35-55nm.png',/transparent,resolution=400
p5.close

;produce plot 55-65
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-3],xrange=[55,65],$
         title='EUV Solar Spectrum: Comparison of Computation to Observation',$
         xtitle='Wavelength [nm]',$
         ytitle='SSI [W m$^{-2}$ nm$^{-1}$]',$
         NAME='CHIANTI 5') ;old ionization 
p6 = plot(SRPMCH6wave, SRPMCH6SSI, 'b2', /OVERPLOT,$
          NAME='CHIANTI 6') ;old ionization
;p7 = plot(wave3, intensity3, 'g2', /OVERPLOT,$
;          NAME='CHIANTI6 + New Ionization')
p8 = plot(EVERocketwave2, EVERocketintensity2, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2010')
leg = legend(TARGET=[p1,p2,p4],position=[0.45,0.87])
p5.save,saveloc+'CHIANTI5_6_EVE_55-65nm.png',/transparent,resolution=400
p5.close

END