; Program to create a batch list of all collision strength related information for use with ComputeColrstrength. 
;
; James Paul Mason
; 2011/12/28

PRO ComputeColrstrength_DBinsert

; read in the temperature ranges for each ion
dataloc = '/Users/jmason86/Dropbox/Research/Fontenla/Data/CHIANTI/'
readcol,dataloc+'IonEquilibriaTemps.csv',ionName,atomicNum,ionCharge,minLogT,maxLogT,format='a,a,a,f,f',/silent

; open file for output
close,1 & spawn, 'rm '+dataloc+'ComputeColrStrength_DBinsert.txt'
openw,1,dataloc+'ComputeColrStrength_DBinsert.txt',/append,width=200

; create shortnames for repeated bits
codeName='GFComputeColrstrength.exe '
midLine=' 0 seaton '
endOfLine=' 8 0.001'

; loop through every ion in the temperature list and create the abatched output
FOR i=0,n_elements(ionName)-1 DO BEGIN
  
  ; convert logT to standard and convert to string
  minT = 10.^minLogT(i) & maxT = 10.^maxLogT(i)
  minT=strcompress(string(minT),/remove_all) & maxT=strcompress(string(maxT),/remove_all)
  
  ; output
  printf,1,codeName,atomicNum(i),' ',ionCharge(i),midLine,ionName(i),'_Seaton.mdb ',minT,' ',maxT,endOfLine

ENDFOR



close,1
END