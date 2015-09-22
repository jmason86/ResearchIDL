; Program (based on spectra_compare.pro) to produce publication quality comparison plots between:
;   SRPM spectrum with CHIANTI5, old ionization (SRPM1)
;   SRPM spectrum with CHIANTI6, old ionization (SRPM2)
;   SRPM spectrum with CHIANTI6, new ionization (SRPM3)
;   EVE spectrum rocket flight (2008, quiet time)
;   EVE spectrum recent data (active time)
;
; CHIANTI6 has only been used to compute the new SRPM coronal spectrum. Do not expect any significant changes
;   to chromosphere, which would also require full non-LTE to run.
; EVE rocket data is in an IDL save file obtained from Rachel Hock. 
; EVE recent data is in specially formatted fits file, obtained from evesci2 machine.
; 
; James Paul Mason
; 2011/12/29

PRO PublicationSpectraCompare

;prep and read data
saveloc = '/Users/jmason86/Dropbox/Research/Fontenla/Publish/Plots/SRPM_EVE/'
SRPMloc = '/Users/jmason86/Dropbox/Research/Fontenla/Data/SRPM/'
SRPMtmploc = '/Users/jmason86/Dropbox/Research/Fontenla/Data/SRPM/CHIANTIAnalysis/NewCHIANTI/'
EVEloc = '/Users/jmason86/Dropbox/Research/Fontenla/Data/SDO/EVE/'
EVERocketloc = '/Users/jmason86/Dropbox/Research/Fontenla/Data/SDO/EVE/RocketFlights/JF/'

;readcol, SRPMloc+'CHIANTI5_OldIonization_FEUV6Cont11.hopt0.conv1a.txt',point,wave1,intensity1,Tbright,index1,index2,percent, skipline=4
;readcol, SRPMloc+'CHIANTI6_OldIonization_FEUV6Cont11.hopt0.conv1a.txt',point,wave2,intensity2,Tbright,index1,index2,percent, skipline=4

readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI5_Oldionization_Spectrum_model1.tab',point,SRPMCH5Model1Wave,SRPMCH5Model1SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI5_Oldionization_Spectrum_model2.tab',point,SRPMCH5Model2Wave,SRPMCH5Model2SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI6_Oldionization_Spectrum_model1.tab',point,SRPMCH6OldModel1Wave,SRPMCH6OldModel1SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI6_Oldionization_Spectrum_model2.tab',point,SRPMCH6OldModel2Wave,SRPMCH6OldModel2SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI6_Newionization_Spectrum_model1.tab',point,SRPMCH6NewModel1Wave,SRPMCH6NewModel1SSI,Tbright,index1,index2,percent, skipline=4,/silent
readcol, SRPMtmploc+'Chromo1Corona0.5_CHIANTI6_Newionization_Spectrum_model2.tab',point,SRPMCH6NewModel2Wave,SRPMCH6NewModel2SSI,Tbright,index1,index2,percent, skipline=4,/silent

readcol, EVERocketloc+'EveSpectrumRocket2008(4).tab',point,EVERocketwave,EVERocketintensity,Tbright,index1,index2,percent,skipline=4,/silent
readcol, EVERocketloc+'EveSpectrumRocket2010May.tab',point,EVERocketwave2,EVERocketintensity2,Tbright,index1,index2,percent,skipline=4,/silent


;Add 0.8Model1 and 0.2Model2 for CHIANTI 5 and 6
SRPMCH5SSI = 0.8*SRPMCH5Model1SSI + 0.2*SRPMCH5Model2SSI
SRPMCH6OldSSI = 0.8*SRPMCH6OldModel1SSI + 0.2*SRPMCH6OldModel2SSI
SRPMCH6NewSSI = 0.8*SRPMCH6NewModel1SSI + 0.2*SRPMCH6NewModel2SSI
SRPMCH5wave = SRPMCH5Model1Wave
SRPMCH6Oldwave = SRPMCH6OldModel1Wave
SRPMCH6Newwave = SRPMCH6NewModel1Wave

;produce plot 5-65
;p1 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-2],aspect_ratio=3,dimensions=[14400,850],xrange=[5,65],$
;         ytitle='SSI (W m$^{-2}$ nm$^{-1}$)',ytickfont_size=17,$
;         xtitle='Wavelength (nm)',xtickfont_size=17,$
;         NAME='SRPM1') ;old ionization 
;p2 = plot(SRPMCH6Oldwave, SRPMCH6OldSSI, 'b2',/ylog,yrange=[1E-7,1E-2],aspect_ratio=3,dimensions=[14400,850],xrange=[5,65],/OVERPLOT,$
;          ;title='EUV Solar Spectrum: SRPM2',font_size=18,$
;          ;ytitle='SSI [W m$^{-2}$ nm$^{-1}$]',ytickfont_size=17,$
;          NAME='SRPM2') ;old ionization
;p3 = plot(SRPMCH6Newwave, SRPMCH6NewSSI, 'm2', /OVERPLOT,$
;          NAME='SRPM3')
;p4 = plot(EVERocketwave, EVERocketintensity, 'g2',/ylog,yrange=[1E-7,1E-2],aspect_ratio=3,dimensions=[14400,850],xrange=[5,65],/OVERPLOT,$
;          ;title='EUV Solar Spectrum: Rocket EVE (2008)',font_size=18,$
;          ;ytitle='SSI (W m$^{-2}$ nm$^{-1}$)',ytickfont_size=17,$
;          ;xtitle='Wavelength (nm)',xtickfont_size=17,$
;          NAME='Rocket EVE 2008') 
;leg = legend(TARGET=[p1,p2,p3,p4],position=[0.56,0.68])
;p1.save,saveloc+'SRPM1_2_EVE_5-65nm.png',/transparent,resolution=400
;p1.close

;produce plot 15-25
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-3],xrange=[15,25],dimensions=[1100,1100],LAYOUT=[2,2,1],$
         xtitle='Wavelength (nm)',xtickfont_size=17,$
         ytitle='SSI (W m$^{-2}$ nm$^{-1}$)',ytickfont_size=17,$
         NAME='SRPM1') ;old ionization 
p6 = plot(SRPMCH6Oldwave, SRPMCH6OldSSI, 'b2', /OVERPLOT,$
          NAME='SRPM2') ;old ionization
p7 = plot(SRPMCH6Newwave, SRPMCH6NewSSI, 'm2', /OVERPLOT,$
          NAME='SRPM3')
p8 = plot(EVERocketwave, EVERocketintensity, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2008') 
;leg = legend(TARGET=[p5,p6,p7,p8],position=[0.15,0.3])
;p5.save,saveloc+'SRPM1_2_EVE_15-25nm.png',/transparent,resolution=400
;p5.close

;produce plot 25-35
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-2],xrange=[25,35],dimensions=[800,800],LAYOUT=[2,2,2],/CURRENT,$
         xtitle='Wavelength (nm)',xtickfont_size=17,$
         ytitle='SSI (W m$^{-2}$ nm$^{-1}$)',ytickfont_size=17,$
         NAME='SRPM1') ;old ionization 
p6 = plot(SRPMCH6Oldwave, SRPMCH6OldSSI, 'b2', /OVERPLOT,$
          NAME='SRPM2') ;old ionization
p7 = plot(SRPMCH6Newwave, SRPMCH6NewSSI, 'm2', /OVERPLOT,$
          NAME='SRPM3')
p8 = plot(EVERocketwave, EVERocketintensity, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2008') 
;leg = legend(TARGET=[p1,p2,p4],position=[0.15,0.28])
;p5.save,saveloc+'SRPM1_2_EVE_25-35nm.png',/transparent,resolution=400
;p5.close

;produce plot 35-55
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-3],xrange=[35,55],dimensions=[800,800],LAYOUT=[2,2,3],/CURRENT,$
         xtitle='Wavelength (nm)',xtickfont_size=17,$
         ytitle='SSI (W m$^{-2}$ nm$^{-1}$)',ytickfont_size=17,$
         NAME='SRPM1') ;old ionization 
p6 = plot(SRPMCH6Oldwave, SRPMCH6OldSSI, 'b2', /OVERPLOT,$
          NAME='SRPM2') ;old ionization
p7 = plot(SRPMCH6Newwave, SRPMCH6NewSSI, 'm2', /OVERPLOT,$
          NAME='SRPM3')
p8 = plot(EVERocketwave, EVERocketintensity, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2008') 
;leg = legend(TARGET=[p1,p2,p4],position=[0.63,0.85])
;p5.save,saveloc+'SRPM1_2_EVE_35-55nm.png',/transparent,resolution=400
;p5.close

;produce plot 55-65
p5 = plot(SRPMCH5wave, SRPMCH5SSI, 'r2',/ylog,yrange=[1E-7,1E-3],xrange=[55,65],dimensions=[800,800],LAYOUT=[2,2,4],/CURRENT,$
         xtitle='Wavelength (nm)',xtickfont_size=17,$
         ytitle='SSI (W m$^{-2}$ nm$^{-1}$)',ytickfont_size=17,$
         NAME='SRPM1') ;old ionization 
p6 = plot(SRPMCH6Oldwave, SRPMCH6OldSSI, 'b2', /OVERPLOT,$
          NAME='SRPM2') ;old ionization
p7 = plot(SRPMCH6newwave, SRPMCH6NewSSI, 'm2', /OVERPLOT,$
          NAME='SRPM3')
p8 = plot(EVERocketwave, EVERocketintensity, 'g2', /OVERPLOT,$
          NAME='Rocket EVE 2008')
leg = legend(TARGET=[p5,p6,p7,p8],position=[0.42,0.54])
;p5.save,saveloc+'SRPM1_2_EVE_55-65nm.png',/transparent,resolution=400
p5.save,saveloc+'SRPM1_2_EVE2008_4panel.png',/transparent,resolution=400
p5.close

END