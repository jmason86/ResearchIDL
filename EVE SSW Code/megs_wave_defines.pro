;docformat = 'rst'
;
;
; Define wavelength scale minima for MEGS channels
;:private:
;
;min_megsb_L1_wave = 34.5d0  ;nm
;max_megsb_L1_wave = 105.5d0 ;nm
min_megsb_L1_wave = 33.0d0  ;nm
max_megsb_L1_wave = 107.0d0 ;nm

min_megsa1_L1_wave = 3.0d0 ;nm
max_megsa1_L1_wave = 39.0d0 ;nm (primary 5-19)

min_megsa2_L1_wave = 3.0d0 ;nm
max_megsa2_L1_wave = 39.0d0 ;nm (primary 16-38)

;megsa_1_cutoff_freq=6.4 ;nm ; version 1, 2
megsa_1_cutoff_freq=5.9 ;nm ; version 3
megsa_1to2_cutoff_freq = 17.24	; nm
megs_AtoB_cutoff_freq = 37.0	; nm

;
; Calculate number of bins for each channel
; for 0.01 nm sampling in L1 (or 100 bins/nm)
;
num_megsa1_L1_spectral_elements = $
  long((max_megsa1_L1_wave - min_megsa1_L1_wave + 1) * 100) ; 5 - 19 nm
num_megsa2_L1_spectral_elements = $
  long((max_megsa2_L1_wave - min_megsa2_L1_wave + 1) * 100) ;16 - 38 nm
; all megs_a
num_megsa_L1_spectral_elements = $
  long((max_megsa2_L1_wave - min_megsa1_L1_wave + 1) * 100) ;5 - 38 nm

; megs_b
; Calculate number of bins for each channel
; for 0.02 nm sampling in L1 (or 50 bins/nm)
num_megsb_L1_spectral_elements  = $
  long((max_megsb_L1_wave  - min_megsb_L1_wave) * 50)  ;33.0 - 107.0 nm

;
; Define L1 MEGS wavelength scales
;
;megsa1_L1_wave = min_megsa1_L1_wave + 0.01*dindgen(num_megsa1_L1_spectral_elements)
;megsa2_L1_wave = min_megsa2_L1_wave + 0.01*dindgen(num_megsa2_L1_spectral_elements)
megsa_L1_wave = eve_range(min_megsa1_L1_wave,max_megsa2_L1_wave,invdelta=100,num_megsa_L1_spectral_elements)
megsa1_l1_wave=megsa_l1_wave
megsa2_l1_wave=megsa_l1_wave

;megsb_L1_wave  = min_megsb_L1_wave  + 0.02*dindgen(num_megsb_L1_spectral_elements)
megsb_L1_wave  = eve_range(min_megsb_L1_wave,max_megsb_L1_wave,invdelta=50,num_megsb_L1_spectral_elements)
megs_L2_wave=    eve_range(min_megsa1_L1_wave,max_megsb_L1_wave,invdelta=50,num_megs_L2_spectral_elements)

megs_L2_atob_cutoff_bin=max(where(megs_l2_wave lt megs_atob_cutoff_freq)); Index of the last MEGS-A bin
