; Program to create batch file for running entire spectrum using GFEffThinSpectraSrvr2 (Corona)
; 
; Coronal models are models 1010-1018
; Chromospheric models are 1000-1008
; 
; GFEffThinSpectraSrvr2 call synopsis:
;   runname atmosdb populadb elemdb modelid output indmodel indmu wavestart waveend configfilename
; GFEffThinSpectraSrvr2 call example:
;   F:\Spectra_Sandbox\GFEffThinSpectraSrvr2.exe spectra 1000 F:\Spectra_Sandbox\CHIANTI6EUV3Cont11.hopt0.bsf 0 -1 100  402 F:\Spectra_Sandbox\SpectConfigDataOpt0m11.txt
;
; James Paul Mason
; 7/24/11

PRO spectra_batch

;string vars to shorten lines later
prog = 'F:\Spectra_Sandbox\GFEffThinSpectraSrvr2.exe'
loc = 'F:\Spectra_Sandbox\CHIANTI6'
endname = 'Cont11.hopt0.bsf'
spec_name = ['FUV','EUV1','EUV2','EUV3','EUV4','EUV5','EUV6']
spec_rng_left = ['1000','700','400','100','40','10','1']
spec_rng_right = ['2020','1002','702','402','102','42','12']
config = 'F:\Spectra_Sandbox\SpectConfigDataOpt0m11.txt'

;prep
spawn, 'rm \\lasp-smb\jama6159\My Dropbox\Development\IDLWorkspace81\Research\RunAllSpectra.txt'
close, /all & openw,1,'\\lasp-smb\jama6159\My Dropbox\Development\IDLWorkspace81\Research\RunAllSpectra.txt',width=200,/append


FOR i=0,n_elements(spec_name)-1 DO BEGIN ;loop through sections of spectra
  FOR j=0,8 DO BEGIN ;loop through the 9 models
  
    model = '101'+strcompress(string(j),/remove_all) ;101 = coronal, 100 = chromospheric
    printf,1,prog+' spectra '+model+' '+loc+spec_name(i)+endname+' '+strcompress(string(j),/remove_all)+' -1 '+spec_rng_left(i)+' '+spec_rng_right(i)+' '+config

  ENDFOR ;j loop
ENDFOR ;i loop


close,1
END
