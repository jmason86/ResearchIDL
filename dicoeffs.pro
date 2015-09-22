; Program to generate Direct Ionization (DI), EA, RR, and DR coefficients from CHIANTI6 ioniz_rate and recomb_rate. 
; 
; The coefficient will be DIrate * exp(+X_0) / (alpha sqrt(kbT))
;   X_0 refers to the FIRST index in the array wihtin ioniz_rate.pro.
;     X_0 = ev1(0)/tev(i) where tev is the temperature converted to ev
; 
; Range of temperatures to input: [1/2 min of ionization curve, 2 * max of ionization curve]
; 20 points, linearly spaced.
;
; James Paul Mason
; 8/10/11

PRO DIcoeffs

;define constants
alpha = 1 ;alpha=5.287d+13
kb=1.380626d-16
K2eV = 8.617343d-5 ;conversion factor for Kelvin to eV
eV2K = double(1/K2eV) ;conversion factor for eV to Kelvin

;values from files
data_loc = '/Users/jmason86/Dropbox/Research/Fontenla/Data/CHIANTI/'
readcol,data_loc+'IonEquilibriaTemps.csv', ionSpecNote, atomNum, ionCharge, minLogT, maxLogT, format = 'a,a,a,f,f',/silent
readcol,data_loc+'ionizationEnergy.txt',ionSpecNote2,ev1,format='a,f',/silent
readcol,data_loc+'de.txt',ionSpecNote3,de,format='a,f',/silent

;open files
;spawn, 'rm '+data_loc+'ionizationEnergy.txt'
close,1 & spawn, 'rm '+data_loc+'Ionization_Recombination_Coeffs.txt'
close,2 & spawn, 'rm '+data_loc+'Ionization_Recombination_Rates.txt'
openw,1,data_loc+'Ionization_Recombination_Coeffs.txt',/append, width=200
openw,2,data_loc+'Ionization_Recombination_Rates.txt',/append, width=200
printf,1,'atomic#, ionCharge, type, Temperature[K], eKT, DIcoefficient, EAcoefficient, RRcoefficient, DRcoefficient, Source'
printf,2,'atomic#, ionCharge, type, Temperature[K], ekT, DIrate[cm^3s^-1], EArate[cm^3s^-1], RRrate[cm^3s^-1], DRrate[cm^3s^-1], Source'

;initialize arrays
DIcoeff = dblarr(n_elements(minLogT)) & EAcoeff = DIcoeff & DIarr = DIcoeff & EAarr = DIcoeff
recombcoeff = DIcoeff & drcoeff = DIcoeff & RRarr = DIcoeff & DRarr = DIcoeff

;loop through temperatures
FOR i=0,n_elements(minLogT)-1 DO BEGIN

  ;Temp = range( double(0.5*10^minLogT(i)) , double(2*10^maxLogT(i)), npts=10)
  Temp = range( double(10^maxLogT(i)) , double(10^minLogT(i)), npts=20, /reciprocal)

  FOR j=0,n_elements(Temp)-1 DO BEGIN
   
    ;Run CHIANTI codes and extract the output
    DI_EA = ioniz_rate(ionSpecNote(i),Temp(j))  ; MAKE SURE TO RETURN CHIANTI CODE TO NORMAL (SUMMED) RETURN
    IF i+1 LT n_elements(atomNum) THEN IF atomNum(i+1) EQ atomNum(i) THEN $ ; CHIANTI recombination rate values are for recombination out-of the ion to the next lower one. Juan's are for recombination into the ion from the next higher one, so must shift but be careful not to move across atoms. 
      RR_DR = recomb_rate(ionSpecNote(i+1),Temp(j)) ELSE BEGIN ; MAKE SURE TO RETURN CHIANTI CODE TO NORMAL (SUMMED) RETURN
        
        ;Special case: need to get the recombination rate from the fully ionized species which requires a bit of setup
        IF ionCharge(i)+1 LT 10 THEN $ ; deal with the fact that need to subtract off either 1,2 digits in strmid
        full_ion_name = string(strmid(ionSpecNote(i),0,strlen(ionSpecNote(i))-1) + strtrim(ionCharge(i)+2,1)) ELSE $
        full_ion_name = string(strmid(ionSpecNote(i),0,strlen(ionSpecNote(i))-2) + strtrim(ionCharge(i)+2,1))
        
        RR_DR = recomb_rate(full_ion_name,Temp(j))
    ENDELSE

    RRarr(i) = RR_DR(0) & IF n_elements(RR_DR) GT 1 THEN DRarr(i) = RR_DR(1) ELSE DRarr(i)=0d
    DIarr(i) = DI_EA(0) & EAarr(i) = DI_EA(1)
    
    ;Generate coefficients
    X_0 = double(ev1(i) / (K2eV * Temp(j)))
    xt_0 = double(de(i) / (K2eV * Temp(j)))
    DIcoeff(i) = DIarr(i) * exp(X_0) / sqrt(kb*Temp(j))
    IF finite(de(i)) THEN EAcoeff(i) = EAarr(i) * exp(xt_0) * sqrt(kb*Temp(j)) ELSE EAcoeff(i) = 0
    recombcoeff(i) = RRarr(i) * sqrt(kb*Temp(j))
    drcoeff(i) = DRarr(i) * sqrt(kb*Temp(j))

    ekt = ev1(i)*eV2K/Temp(j)
    
    ; define type: if DI or totR are 0, then set type =-1 so later codes will not use it
    IF DIarr(i) EQ 0 or RRarr(i) EQ 0 THEN type=-1 ELSE type=1

    ;output to file
    printf,1, atomNum(i),',', ionCharge(i),',', type,',', Temp(j),',', ekt,',', DIcoeff(i),',', EAcoeff(i),',', recombcoeff(i),',', drcoeff(i),',',15
    printf,2, atomNum(i),',', ionCharge(i),',', type,',', Temp(j),',', ekt,',', DIarr(i),',', EAarr(i),',', RRarr(i),',', DRarr(i),',', 15
    
  ENDFOR ;j loop
ENDFOR ;i loop

;generate plots
;p1 = plot(Temp,DIcoeff,'r2D-',$
;         title='Direct Ionization Coefficient vs Temp',$
;         xtitle='Temperature [K]',$
;         ytitle='DIrate * exp(X0) / (alpha * sqrt(kbT))',$
;         NAME='Carbon 4')
;p1.sym_color='blue'
;p1.sym_filled=1
;p1.sym_fill_color=0
;leg1 = legend(TARGET=[p1], position=[0.7,0.8])

;p2 = plot(Temp,DIarr,'b2',$
;         title='Direct Ionization Rate vs Temp',$
;         xtitle='Temperature [K]',$
;         ytitle='DIrate',$
;         NAME='Carbon 4')
;leg2 = legend(TARGET=[p2], position=[0.7,0.8])

close,1,2


END