;; Restore data
;restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a171_derot_james.sav'
;restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a193_derot_james.sav'
;restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/sector_maps.sav'
;restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a304_derot_barbara.sav'


;; Make reference and dimming images
!p.multi=[0,3,2,0,1]

img_ref_193 = total(img193[*,*,0:4], 3)/5.
img_dimming_193 = smooth(img_ref_193 - min(img193, dim=3), 2)
img_dimming_193 = 255-bytscl(alog10(img_dimming_193>0)>0<2.7)

aia_lct, rr, gg, bb, wave='193', /load
plot_image, bytscl(alog10(img_ref_193>0)>1.25<3)
loadct, 0
plot_image, img_dimming_193

img_ref_171 = total(img171[*,*,0:4], 3)/5.
img_dimming_171 = smooth(img_ref_171 - min(img171, dim=3), 2)
img_dimming_171 = 255-bytscl(alog10(img_dimming_171>0)>0<2.7)

aia_lct, rr, gg, bb, wave='171', /load
plot_image, bytscl(alog10(img_ref_171>0)>1.25<3)
loadct, 0
plot_image, img_dimming_171

img_ref_304 = total(img304[*,*,0:1], 3)/2.
img_dimming_304 = smooth(img_ref_304 - min(img304, dim=3), 2)
img_dimming_304 = 255-bytscl(alog10(img_dimming_304>0)>0<1.7)

aia_lct, rr, gg, bb, wave='304', /load
plot_image, bytscl(alog10(img_ref_304>0)>0.5<2.5)
loadct, 0
plot_image, img_dimming_304

!p.multi=0

;; Define polar CH region
print, 'Define polar CH region:'
mask_ch_int = roimask( img_dimming_193 )

;; Define polar AR region
print, 'Define AR regions:'
mask_ar_int = roimask( bytscl(alog10(img_ref_193>0)>1.25<3.5) )

;; Define core region
print, 'Define core region:'
mask_core_int = roimask( img_dimming_171 )

;; Combine and check masks
mask_disk = congrid(mask_limb, 1024, 1024)

mask_ch = mask_ch_int*mask_disk
mask_core = mask_core_int*mask_disk
mask_ar = mask_ar_int*mask_disk

;; Save masks
save, mask_ch, mask_core, mask_ar, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/cutout_masks_final.sav'



END
