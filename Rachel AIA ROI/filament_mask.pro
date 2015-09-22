
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a304_derot_barbara.sav'
aia_lct, rr, gg, bb, /load, wave='304'
mask_filament = roimask( bytscl(min(img304, dim=3)>0<30) )
save, mask_filament, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a304_filament_mask.sav'

END

