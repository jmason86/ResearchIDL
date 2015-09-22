; Quick program to increase the range of logT a bit in the file IonEquilibriaTemps.
; The current temperatures have been hand selected to cover the ionization fraction down to about 0.1. 
; Want to expand the ionization fraction, but will now do it programmatically by adding 1 to the logT range. 
;   minLogT=minLogT-0.5 & maxLogT=maxLogT+0.5
;
; James Paul Mason
; 11/3/2011

PRO IncreaseTemperatureRange

;read in data
dataloc='/Users/jmason86/Dropbox/Research/Data/CHIANTI/'
readcol,dataloc+'IonEquilibriaTemps_original.csv',ionName,atomNum,ionCharge,minLogT,maxLogT,format='a,a,a,f,f',/silent

;open file for output
spawn,'rm '+dataloc+'IonEquilibriaTemps.csv'
close,1 & openw,1,dataloc+'IonEquilibriaTemps.csv',width=200,/append

;insert header row to file
printf,1,'Ion,Atom#,Ion Charge,min LogT, max LogT'

;symmetrically increase the temperature range for each ion and write 
FOR i=0,n_elements(minLogT)-1 DO BEGIN

  mod_minLogT=minLogT(i)-0.5
  mod_maxLogT=maxLogT(i)+0.5
  
  ;convert floats to strings for output
  mod_minLogT=strcompress(string(mod_minLogT),/remove_all)
  mod_maxLogT=strcompress(string(mod_maxLogT),/remove_all)
  
  ;output to file
  printf,1,ionName(i),',',atomNum(i),',',ionCharge(i),',',mod_minLogT,',',mod_maxLogT

ENDFOR ;i loop


close,1

END