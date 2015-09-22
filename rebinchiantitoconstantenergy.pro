;+
; NAME:
;   RebinCHIANTIToConstantEnergy
;
; PURPOSE:
;   Convert the CHIANTI spectrum that Tom Woods generated from Angstroms to constant energy bins
;
; INPUTS:
;   chianti_ref_sp.dat, hard coded reference
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Plot of spectrum
;   .txt file in similar format to original .dat
;   .txt file of only energies
;   .txt file of only irradiances
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   read_dat.pro from Tom Woods
;   Requires solarsoft
;
; EXAMPLE:
;   N/A, just run the code
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2012/12/8
;   2013/3/22, James Paul Mason: Added setup section
;-
PRO RebinCHIANTIToConstantEnergy

; Setup
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/CHIANTI/'

; Define constants
h = 6.626068d-34 ; [m^2 kg / s] Planck's constant
c = 299792458d   ; [m / s] speed of light
J2keV = 6.241506363d+15

; Load high resolution (0.02 Angstrom bins) CHIANTI reference spectrum
; Irradiance units = W/m^2/Å, wavelength units = Å
chiantiData = read_dat(saveloc + 'chianti_ref_sp.dat')

; Store irradiances from chiantiData
irradiancesAll = chiantiData[1:4,*]

; Do wavelength -> energy conversion
wavelengthsSI = reform(chiantiData[0, *] * 1d-10)
energiesIn_Joules = h * c / wavelengthsSI
energiesIn_keV = energiesIn_Joules * J2keV

; Get wavelength bins in 2xN array for rebinning later
wavelengthsDelta = mean(get_edges(wavelengthsSI, /WIDTH), /DOUBLE) ; get uniform spacing of wavelength bins
wavelengthsSI_2xN = get_edges([wavelengthsSI - wavelengthsDelta/2., wavelengthsSI[-1] + wavelengthsDelta/2.], /EDGES_2) ; translate by half bin width to get leading edges, add final trailing edge, convert N+1 to 2xN

; Get energy bins in 2xN array for rebinning later
energiesIn_keV_2xN = (h * c / wavelengthsSI_2xN) * J2keV

; Convert irradiance in J/s/m^2/Å to photons/s/m^2/Å
FOR i = 0, 3 DO irradiancesAll(i, *) = irradiancesAll(i, *) / energiesIn_Joules

; Convert irradiance in photons/s/m^2/Å to photons/s/m^2/keV and define photon flux in photons/s/m^2
wavelengthsWidths = get_edges(wavelengthsSI_2xN, /WIDTH) * 1E10 ; 2013/3/22: Converted to Å for multiplcation with irradiance below
energiesWidths = get_edges(energiesIn_keV_2xN, /WIDTH)
photonFluxAll = dblarr(4, n_elements(irradiancesAll(0, *)))
FOR i = 0, 3 DO BEGIN 
  irradiancesAll(i, *) = irradiancesAll(i, *) * wavelengthsWidths / energiesWidths
  photonFluxAll(i, *) = irradiancesAll(i, *) * energiesWidths
ENDFOR ; i loop

; Define X123 energy binning for use in converting CHIANTI bins to X123's
x123Energies_2xN = get_edges(dindgen(1025) * 0.0292d, /EDGES_2)  ; 1024 bins with 0.0292 keV/bin

; ssw_rebinner needs the two energy arrays to increase in the same direction
energiesIn_kev_2xN_reversed = get_edges(reverse(get_edges(energiesIn_keV_2xN, /EDGES_1)), /EDGES_2)

; Rebin CHIANTI bins to X123 bins and convert from photon flux in photons/s/m^2 to irradiance in photons/s/m^2/keV
ssw_rebinner, reverse(reform(photonFluxAll(0,*))), energiesIn_keV_2xN_reversed, photonFluxQS, X123Energies_2xN
ssw_rebinner, reverse(reform(photonFluxAll(1,*))), energiesIn_keV_2xN_reversed, photonFluxAR, X123Energies_2xN
ssw_rebinner, reverse(reform(photonFluxAll(2,*))), energiesIn_keV_2xN_reversed, photonFluxCH, X123Energies_2xN
ssw_rebinner, reverse(reform(photonFluxAll(3,*))), energiesIn_keV_2xN_reversed, photonFluxFlare, X123Energies_2xN
X123EnergyWidths = get_edges(X123Energies_2xN, /WIDTH) 
X123EnergyCenters = get_edges(findgen(1025) * 0.0292d, /MEAN)
irradianceQS = photonFluxQS / X123EnergyWidths 
irradianceAR = photonFluxAR / X123EnergyWidths 
irradianceCH = photonFluxCH / X123EnergyWidths 
irradianceFlare = photonFluxFlare / X123EnergyWidths 

; Create power spectrum
powerSpectrum = dblarr(1024)
energyAbove20keVIndices = where(X123EnergyCenters GE 20, COMPLEMENT = energyBelow20keVIndices)
powerSpectrum[energyAbove20keVIndices] = 100 * (X123EnergyCenters[energyAbove20keVIndices] / 20.)^(-4.5)
powerSpectrum[energyBelow20keVIndices] = 100 * (X123EnergyCenters[energyBelow20keVIndices] / 20.)^(-1.5)
powerSpectrum = powerSpectrum * 1E4 ; Converted from cm^-2 to m^-2
powerSpectrum[0] = 0.

; Apply power spectrum
irradianceFlare = irradianceFlare + powerSpectrum

; Plot the irradiance versus energy
p1 = plot(X123EnergyCenters, irradianceQS, /BUFFER , $
         title = 'Rebinned CHIANTI Spectrum to X123 Bins', $
         xtitle = 'Energy [keV]', /xlog, xrange = [0.1, 100], $
         ytitle = 'Irradiance [photons/s/m!U2!N/keV]', yrange = [0, 1E20], /ylog, $
         NAME = 'Quiet Sun')
p2 = plot(X123EnergyCenters, irradianceAR, /OVERPLOT, 'r', $
         NAME = 'Active Region')
p3 = plot(X123EnergyCenters, irradianceCH, /OVERPLOT, 'b', $
         NAME = 'Coronal Hole')
p4 = plot(X123EnergyCenters, irradianceFlare, /OVERPLOT, 'g', $
         NAME = 'Flare')
leg = legend(TARGET = [p4,p2,p1,p3], POSITION = [0.90,0.85])
p1.save, saveloc + 'rebinnedSpectrum.png'

; Write new file
spawn, 'rm "' + saveloc + 'chianti_ref_sp_energybinned.txt"'
openw, 1, saveloc + 'chianti_ref_sp_energybinned.txt', /append, width=200
printf,1, 'Channel(#) Energy(keV) QuietSunIrradiance(photons/s/m^2/keV) ActiveRegionIrradiance CoronalHoleIrradiance FlareIrradiance'
FOR i = 0, n_elements(X123EnergyCenters) - 1 DO printf, 1, strtrim(string(i),2), X123EnergyCenters(i), irradianceQS(i), irradianceAR(i), irradianceCH(i), irradianceFlare(i)
close,1

spawn, 'rm "' + saveloc + 'CHIANTIEnergies.txt"'
openw, 1, saveloc + 'CHIANTIEnergies.txt', /append
FOR i = 0, n_elements(X123EnergyCenters) - 1 DO printf, 1, X123EnergyCenters(i)
close,1

spawn, 'rm "' + saveloc + 'CHIANTIIrradiances.txt"'
openw, 1, saveloc + 'CHIANTIIrradiances.txt', /append
FOR i = 0, n_elements(X123EnergyCenters) - 1 DO printf, 1, irradianceQS(i), ',', irradianceAR(i), ',', irradianceCH(i), ',', irradianceFlare(i)
close,1

END