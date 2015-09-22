; Quick program to compute the ionization and recombination for an ion. 
; Uses Shull and Van Steenberg, ApJ, vol.48, p95, 1982
; Uses CHIANTI 6
; 
; James Paul Mason
; 2011/11/23

PRO ComputeIonizationRecombination

;select peak temperature of ionization curve 
;T = 2.24E6 ; Ni XIV
T = 2.05E6 ; Ni XIII
T = 2.6E6 ; Ni XV

;Shull

;DEFINE SHULL PARAMETERS

;Ni XIV parameters
;Acol = 3.8E-13
;ai = 0.1 ;approximate.. paper says 'typically ai is about 0.1'
;Tcol = 4.99E6
;Arad = 2.63E-10
;Xrad = 0.834
;Adi = 0.446
;Bdi = 0.332
;T0 = 5.97E5
;T1 = 8.84E5

;Ni XIII
;Acol = 6.35E-13
;ai = 0.1 ;approximate.. paper says 'typically ai is about 0.1'
;Tcol = 4.46E6
;Arad = 2.29E-10
;Xrad = 0.828
;Adi = 0.525
;Bdi = 0.192
;T0 = 6.65E5
;T1 = 1.89E6

;Ni XV
Acol = 2.17E-13
ai = 0.1 ;approximate.. paper says 'typically ai is about 0.1'
Tcol = 5.39E6
Arad = 3.16E-10
Xrad = 0.836
Adi = 0.363
Bdi = 0.337
T0 = 5.24E5
T1 = 1.28E6

;COMPUTE AND OUTPUT IONIZATION AND RECOMBINATION INTO/OUTOF ION OF INTEREST

;Shull Ionization/Recombination
ioniz_shull = Acol * sqrt(T) * (1 + ai * T/Tcol)^(-1) * exp(- Tcol/T)

recomb_shull = Arad * (T/1E4)^(-Xrad) + Adi*T^(-3/2) * exp(-T0/T) * (1 + Bdi * exp(-T1/T))

print, 'Shull ionization =',ioniz_shull,' cm^3/s'
print, 'Shull recombination =',recomb_shull,' cm^3/s' 

;CHIANTI6 Ionization/Recombination

print, 'CHIANTI ionization =', ioniz_rate('ni_15',T)
print, 'CHIANTI recombination =', recomb_rate('ni_16',T) ; ion+1 to get ionization INTO the ion of interest



END