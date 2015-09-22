; RHESSI: 0.1 - 4 Å SDO/EVE: 60 - 1050 Å
; RHESSI: 124 - 3.1 keV SDO/EVE: 0.2 - 0.01 keV

@'range.pro'
PRO CreateCHIANTISpectrum

; Define constants
h = 6.626068d-34 ; [m^2 kg / s] Planck's constant
c = 299792458d   ; [m / s] speed of light
J2keV = 6.241506363d+15

; Make temperature array
temperatures = range(5E6, 25E6, INC = 1E6)

ch_synthetic, 0.1, 1200., output = chiantiSpectrum, density = 1E10, /PHOTONS, /NOPROT, $ 
              sngl_ion = ['he_2','o_1', 'o_2','o_3','o_4','o_5','o_6','o_7','o_8','si_2','si_3','si_4','si_5','si_6','si_7','si_8','si_9','si_10','si_11','si_12','si_13','si_14','s_2','s_3','s_4','s_5','s_6','s_7','s_8','s_9','s_10','s_11','s_12','s_13','s_14','s_15','s_16','fe_2','fe_4','fe_5','fe_6','fe_7','fe_8','fe_9','fe_10','fe_11','fe_12','fe_13','fe_14','fe_15','fe_16','fe_17','fe_18','fe_19','fe_20','fe_21','fe_22','fe_23','fe_24','fe_25','fe_26'], $
              ioneq_name = concat_dir(concat_dir(!xuvtop,'ioneq'),'chianti.ioneq'), $
              LOGT_ISOTHERMAL = alog10(temperatures)
              ;dem_name = concat_dir(concat_dir(!xuvtop,'dem'),'flare.dem')

inds = sort(chiantiSpectrum.lines.wvl)
wavelength = chiantiSpectrum.lines[inds].wvl
intensity = chiantiSpectrum.lines[inds].int

freefree, temperatures, wavelength, freeFreeContinuum, /PHOTONS
freebound, temperatures, wavelength, freeBoundContinuum, /PHOTONS
;two_photon,5.e+6,wavelength,twoPhotonContinuum, /PHOTONS

combinedIntensity = intensity*1E20 + freeFreeContinuum/1E20 + freeBoundContinuum/1E20

; Do wavelength -> energy conversion
wavelengthsSI = wavelength * 1d-10
energiesIn_Joules = h * c / wavelengthsSI
energiesIn_keV = energiesIn_Joules * J2keV

; Get wavelength bins in 2xN array
wavelengthsDelta = mean(get_edges(wavelengthsSI, /WIDTH), /DOUBLE) ; get uniform spacing of wavelength bins
wavelengthsSI_2xN = get_edges([wavelengthsSI - wavelengthsDelta/2., wavelengthsSI[-1] + wavelengthsDelta/2.], /EDGES_2) ; translate by half bin width to get leading edges, add final trailing edge, convert N+1 to 2xN

; Get energy bins in 2xN array
energiesIn_keV_2xN = (h * c / wavelengthsSI_2xN) * J2keV

; Convert irradiance in photons/s/m^2/Å to photons/s/m^2/keV 
wavelengthsWidths = get_edges(wavelengthsSI_2xN, /WIDTH)
energiesWidths = get_edges(energiesIn_keV_2xN, /WIDTH)
bla  = combinedIntensity * wavelengthsWidths / energiesWidths

; Create plot in energy
p1 = plot(energiesIn_keV, yar, /ylog, xrange = [0, 10], $
          title = 'CHIANTI Spectrum', $
          xtitle = 'Energy [keV]', $
          ytitle = 'Intensity [photons cm!U-2!N s!U-1!N sr!U-1!N]')
a1 = arrow([3.1, 10], [2, 2], /DATA, COLOR = 'blue', $
           ARROW_STYLE = 2, THICK = 2, $
           NAME = 'RHESSI')
a2 = arrow([0.01, 0.2], [2, 2], /DATA, COLOR = 'red', $
           THICK = 2, $
           NAME = 'EVE')
a3 = arrow([0.6, 5], [2, 2], /DATA, COLOR = 'green', $
           ARROW_STYLE = 3, THICK = 2, $
           NAME = 'MinXSS')           
t1 = text(6, 1E3, 'RHESSI', /DATA, COLOR = 'blue')
t2 = text(0, 1E3, 'EVE', /DATA, COLOR = 'red')
t3 = text(2, 1E3, 'MinXSS', /DATA, COLOR = 'green')

; Create plot in wavelength
p1 = plot(wavelength, combinedIntensity, /ylog, xrange = [0, 100], $
          title = 'CHIANTI Spectrum', $
          xtitle = 'Wavelength [Å]', $
          ytitle = 'Intensity [photons cm!U-2!N s!U-1!N sr!U-1!N]')
a1 = arrow([0, 4], [10, 10], /DATA, COLOR = 'blue', $
           THICK = 2, $
           NAME = 'RHESSI')
a2 = arrow([60, 1050], [10, 10], /DATA, COLOR = 'red', $
           ARROW_STYLE = 3, THICK = 2, $
           NAME = 'EVE')
a3 = arrow([2.5, 20], [10, 10], /DATA, COLOR = 'green', $
           ARROW_STYLE = 3, THICK = 2, $
           NAME = 'MinXSS')              
t1 = text(0, 1E11, 'RHESSI', /DATA, COLOR = 'blue')
t2 = text(80, 1E11, 'EVE', /DATA, COLOR = 'red')
t3 = text(10, 1E11, 'MinXSS', /DATA, COLOR = 'green')

STOP

p1.close

END