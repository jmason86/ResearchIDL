; Program to batch insert CHIANTI atomic data into SRPM's AtomicModels database
; 
; Call ChiantiFeedA.exe to do database insertion. 
; ChiantiFeedA call synopsis:
;     ChiantiFeedA.exe task string, atomic#, ion charge, max# levels, path to CHIANTI, min temp, max temp, number of Temp points, source key
; CHiantiFeedA call example: 
;     ChiantiFeedA.exe AtomicModel 2 1 25 F:\SRPM\AtomicData\CHIANTI6.0.1\ 10000 300000 6 15.2
;     
; James Paul Mason
; 
; 6/15/11

PRO CHIANTIbatch

ion_T_loc = '/Users/jmason86/Dropbox/Research/Data/CHIANTI/'
spawn, 'rm '+ion_T_loc+'CHIANTIFeedA_DBinsert.bat'
close,1 & openw,1, ion_T_loc+'CHIANTIFeedA_DBinsert.bat', /append, width=200
readcol,ion_T_loc+'IonEquilibriaTemps.csv', ion, minlogT, maxlogT, skipline=1,/silent


ctr=0 ;counter
FOR atom_dex=4,30 DO BEGIN ;Lithium to zinc
  FOR ion_dex=3,atom_dex-1 DO BEGIN ;maximum possible number of ion charges is always +1 atomic number
    
    atom_str = strcompress(string(atom_dex),/remove_all)
    ion_str = strcompress(string(ion_dex),/remove_all)
    printf,1,'ChiantiFeedA.exe AtomicModel '+atom_str+' '+ion_str+' 1000 F:\SRPM\AtomicData\CHIANTI6.0.1\ 10000 300000 6 15.0'
    
    print,ctr
    ctr=ctr+1
  ENDFOR ;ion_dex loop
ENDFOR ;atom_dex loop


close,1
END