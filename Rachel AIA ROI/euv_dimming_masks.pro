;; Restore sample saveset
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a171_derot_james.sav'

;; Make set of images to generate masks
min_img = min(img171, dim=3) - total(img171[*,*,0:4], 3)/3.
diff_img = total(img171[*,*,30:34], 3)/3. -total(img171[*,*,0:4], 3)/3.
img = total(img171[*,*,30:34], 3)/3.

;; Calculate AR mask
print, 'Define AR regions:'
mask_region = roimask( bytscl(alog10(img>0)>1.25<4) )

;; Calculate dimming mask
print, 'Define dimming regions:'
mask_dimming = roimask( bytscl(diff_img>(-100)<100) )

;; Calculate off-limb mask
print, 'Define off-limb regions:'
mask_offlimb = roimask( bytscl(diff_img>(-100)<100) )

;; Save masks
save, mask_region, mask_dimming, mask_offlimb, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/cutout_masks.sav'


END
