PRO aia_timing, jd, y, y_total

y_pre = mean(y[0:4])
y_total_pre = mean(y_total[0:4])

y_percent = (y-y_pre)/(y_total-y_total_pre)

jd_min = anytim2jd('2011-08-04T04:00')
jd_min = jd_min.int+jd_min.frac

igd = where(jd gt jd_min AND jd lt jd_min+5./24.d0 AND (y-y_pre) lt 0 AND (y_total-y_total_pre) lt 0)

tmp = min(y[igd], imin)
caldat, jd[igd[imin]], month, day, year, hour, minute, second
str = string(hour, format='(I02)')+':'+string(minute, format='(I02)')
str = str+', '+string((y[igd[imin]]-y_pre)*1e-6, format='(F9.2)')
str = str+', '+string(y_percent[igd[imin]]*100., format='(F6.1)')+'%'

tmp = max(y_percent[igd], imax)
caldat, jd[igd[imax]], month, day, year, hour, minute, second
str = str+', '+string(hour, format='(I02)')+':'+string(minute, format='(I02)')
str = str+', '+string((y[igd[imax]]-y_pre)*1e-6, format='(F9.2)')
str = str+', '+string(y_percent[igd[imax]]*100., format='(F6.1)')+'%'

y_range = minmax(y_percent[igd])
str = str+', '+string(y_range[0]*100., format='(F6.1)')+'-'+string(y_range[1]*100., format='(F6.1)')+'%'

print, str

END

;; Code that needs to be run once for this to work
;; Step 1: .r combine_aia_derot_img.pro
;; Step 2: .r sector_map.pro
;; Step 3: .r euv_dimming_masks.pro

;; ===============================
;; MAKE COMPOSITE IMAGE
;; ===============================

;;GOTO, LBL

GOTO, LBL3 ;;_img

;; Read in images
d1 = readfits('~/Desktop/EUV_dimming_workshop/IDL_savesets/25382093.fts.gz', h1)
d2 = readfits('~/Desktop/EUV_dimming_workshop/IDL_savesets/25382094.fts.gz', h2)
d3 = readfits('~/Desktop/EUV_dimming_workshop/IDL_savesets/25382095.fts.gz', h3)

;; Make LASCO image
ii = where(rr le 2000)
sol_mask = fltarr(1024, 1024)+1
sol_mask[ii] = 0
plot_image, sol_mask*(d3-d2)>(-4e-10)<(4e-10)

lasco_image = bytscl(sol_mask*(d3-d2)>(-3e-10)<(3e-10))
lasco_image[where(sol_mask eq 0)] = 0

;; Make AIA image
read_sdo, '~/Desktop/EUV_dimming_workshop/IDL_savesets/AIA20110804_0348_0131.fits', h, img, outdir='.'
aia_image = bytscl(alog10(img)>0.5<2.75)

;; Combine images
window, 30, xs=1024.*11.9/2.4, ys=1024.*11.9/2.4, /pixmap

lasco_image_resize = congrid(lasco_image, 1024.*11.9/2.4, 1024.*11.9/2.4)
tv, lasco_image_resize, 0, 0

aia_lct, rr, gg, bb, wave='131', /load 
tv, aia_image, 511.5*11.9/2.4-512., 507.5*11.9/2.4-512.
loadct, 0

;; Capture and save image
composite_img = tvrd(1500, 1000, 1024.*11.9/2.4-2000, 1024.*11.9/2.4-1600, /true)
write_png, 'lasco_aia_composite_image.png', composite_img

;; Delete all windows
wdelete, 30

LBL_img:

;; ===============================
;; Restore masks
;; ===============================

restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/sector_maps.sav'
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/cutout_masks.sav'
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a304_filament_mask.sav'
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/cutout_masks_final.sav'
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/2011216_04AUG_0357_M9.3_eve_l2_data.sav'

;; ===============================
;; Resize large masks
;; ===============================

mask_offlimb = mask_offlimb*congrid(mask_overlimb, 1024, 1024)
mask_dimming = mask_dimming*congrid(mask_limb, 1024, 1024)

sector_map_sm = congrid(sector_map, 1024, 1024)
limb_map_sm = congrid(limb_map, 1024, 1024)
mask_overlimb_sm = congrid(mask_overlimb, 1024, 1024)
mask_limb_sm = congrid(mask_limb, 1024, 1024)

limb_map_sm = congrid(limb_map, 1024, 1024)
nofflimb_sector = max(limb_map_sm)

sector_map_sm = congrid(sector_map, 1024, 1024)
nsector = max(sector_map_sm)

;; ===============================
;; Make feature masks
;; ===============================

radius = 4
kernel = shift( dist(2*radius+1), radius, radius) le radius
mask_filament = dilate(mask_filament, kernel)

radius = 16
kernel = shift( dist(2*radius+1), radius, radius) le radius
tmp = sector_map_sm*0+1
tmp[where(sector_map_sm eq 1)] = 0
tmp = dilate(tmp, kernel)
mask_flare = 1-tmp

mask_filament = tmp*mask_filament*congrid(mask_limb, 1024, 1024)

radius = 2
kernel = shift( dist(2*radius+1), radius, radius) le radius
mask_ar = dilate(mask_ar, kernel)
mask_ar = (mask_ar-mask_flare)>0

mask_core = (mask_core-mask_flare-mask_ar)>0
mask_filament_combo = (mask_filament-mask_ar-mask_flare-mask_ch-mask_core)>0
mask_qs = (congrid(mask_limb, 1024, 1024)-mask_ar-mask_core-mask_flare-mask_filament_combo-mask_ch)>0

;; ===============================
;; Run blob_analyze
;; ===============================

xx = ((findgen(1024)#replicate(1, 1024))-512)*2.4
yy = ((replicate(1, 1024)#findgen(1024))-512)*2.4

dimming_obj = obj_new('Blob_Analyzer', mask_dimming)
ndimming = dimming_obj -> numberofblobs()

offlimb_obj = obj_new('Blob_Analyzer', mask_offlimb)
nofflimb = offlimb_obj -> numberofblobs()

region_obj = obj_new('Blob_Analyzer', mask_region)
nregion = region_obj -> numberofblobs()

;; ===============================
;; Restore AIA 171, AIA 193, AIA 304 images
;; ===============================

;; AIA 171
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a171_derot_james.sav'
jd_171 = anytim2jd(hdr171.t_obs)
jd_171 = jd_171.int+jd_171.frac
n171 = n_elements(jd_171)

;; AIA 193
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a193_derot_james.sav'
jd_193 = anytim2jd(hdr193.t_obs)
jd_193 = jd_193.int+jd_193.frac
n193 = n_elements(jd_193)

;; AIA 304
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a304_derot_barbara.sav'
jd_304 = anytim2jd(hdr304.t_obs)
jd_304 = jd_304.int+jd_304.frac
n304 = n_elements(jd_304)

;; ===============================
;; Calculate cutout lightcurves
;; ===============================

;; AIA 171
aia_total_171 = total(total(img171, 1), 1)

aia_cutouts_171 = fltarr(n171,6)
aia_offlimb_171 = fltarr(n171)
aia_disk_171 = fltarr(n171)

aia_sectors_171 = fltarr(nsector, n171)
tmp = histogram(sector_map_sm, reverse_indices=indices1)

aia_offlimb_sectors_171 = fltarr(nofflimb_sector, n171)
tmp = histogram(limb_map_sm, reverse_indices=indices2)

for iimg=0, n171-1 do begin
   tmp_img = img171[*,*,iimg]

   aia_cutouts_171[iimg,3] = total(tmp_img*mask_ch)
   aia_cutouts_171[iimg,0] = total(tmp_img*mask_flare)
   aia_cutouts_171[iimg,1] = total(tmp_img*mask_core)
   aia_cutouts_171[iimg,4] = total(tmp_img*mask_ar)
   aia_cutouts_171[iimg,2] = total(tmp_img*mask_filament_combo)
   aia_cutouts_171[iimg,5] = total(tmp_img*mask_qs)

   aia_offlimb_171[iimg] = total(tmp_img*mask_overlimb_sm)
   aia_disk_171[iimg] = total(tmp_img*mask_limb_sm)

   for isector=0, nsector-1 do begin
      igd =  indices1[indices1[isector+1]:indices1[isector+2]-1]
      aia_sectors_171[isector, iimg] = total(tmp_img[igd])
   endfor

   for iofflimb_sector=0, nofflimb_sector-1 do begin
      igd =  indices2[indices2[iofflimb_sector+1]:indices2[iofflimb_sector+2]-1]
      aia_offlimb_sectors_171[iofflimb_sector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; AIA 193
aia_total_193 = total(total(img193, 1), 1)

aia_cutouts_193 = fltarr(n193,6)
aia_offlimb_193 = fltarr(n193)
aia_disk_193 = fltarr(n193)

aia_sectors_193 = fltarr(nsector, n193)
tmp = histogram(sector_map_sm, reverse_indices=indices1)

aia_offlimb_sectors_193 = fltarr(nofflimb_sector, n193)
tmp = histogram(limb_map_sm, reverse_indices=indices2)

for iimg=0, n193-1 do begin
   tmp_img = img193[*,*,iimg]

   aia_cutouts_193[iimg,3] = total(tmp_img*mask_ch)
   aia_cutouts_193[iimg,0] = total(tmp_img*mask_flare)
   aia_cutouts_193[iimg,1] = total(tmp_img*mask_core)
   aia_cutouts_193[iimg,4] = total(tmp_img*mask_ar)
   aia_cutouts_193[iimg,2] = total(tmp_img*mask_filament_combo)
   aia_cutouts_193[iimg,5] = total(tmp_img*mask_qs)

   aia_offlimb_193[iimg] = total(tmp_img*mask_overlimb_sm)
   aia_disk_193[iimg] = total(tmp_img*mask_limb_sm)

   for isector=0, nsector-1 do begin
      igd =  indices1[indices1[isector+1]:indices1[isector+2]-1]
      aia_sectors_193[isector, iimg] = total(tmp_img[igd])
   endfor

   for iofflimb_sector=0, nofflimb_sector-1 do begin
      igd =  indices2[indices2[iofflimb_sector+1]:indices2[iofflimb_sector+2]-1]
      aia_offlimb_sectors_193[iofflimb_sector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; AIA 304
aia_total_304 = total(total(img304, 1), 1)

aia_cutouts_304 = fltarr(n304,6)
aia_offlimb_304 = fltarr(n304)
aia_disk_304 = fltarr(n304)

aia_sectors_304 = fltarr(nsector, n304)
tmp = histogram(sector_map_sm, reverse_indices=indices1)

aia_offlimb_sectors_304 = fltarr(nofflimb_sector, n304)
tmp = histogram(limb_map_sm, reverse_indices=indices2)

for iimg=0, n304-1 do begin
   tmp_img = img304[*,*,iimg]

   aia_cutouts_304[iimg,3] = total(tmp_img*mask_ch)
   aia_cutouts_304[iimg,0] = total(tmp_img*mask_flare)
   aia_cutouts_304[iimg,1] = total(tmp_img*mask_core)
   aia_cutouts_304[iimg,4] = total(tmp_img*mask_ar)
   aia_cutouts_304[iimg,2] = total(tmp_img*mask_filament_combo)
   aia_cutouts_304[iimg,5] = total(tmp_img*mask_qs)

   aia_offlimb_304[iimg] = total(tmp_img*mask_overlimb_sm)
   aia_disk_304[iimg] = total(tmp_img*mask_limb_sm)

   for isector=0, nsector-1 do begin
      igd =  indices1[indices1[isector+1]:indices1[isector+2]-1]
      aia_sectors_304[isector, iimg] = total(tmp_img[igd])
   endfor

   for iofflimb_sector=0, nofflimb_sector-1 do begin
      igd =  indices2[indices2[iofflimb_sector+1]:indices2[iofflimb_sector+2]-1]
      aia_offlimb_sectors_304[iofflimb_sector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Filament
aia_filament_304 = fltarr(n304)
for iimg=0, n304-1 do begin
   tmp_img = img304[*,*,iimg]
   aia_filament_304[iimg] = total(tmp_img*mask_filament)
endfor

aia_filament_193 = fltarr(n193)
for iimg=0, n193-1 do begin
   tmp_img = img193[*,*,iimg]
   aia_filament_193[iimg] = total(tmp_img*mask_filament)
endfor

aia_filament_171 = fltarr(n171)
for iimg=0, n171-1 do begin
   tmp_img = img171[*,*,iimg]
   aia_filament_171[iimg] = total(tmp_img*mask_filament)
endfor

;; ===============================
;; Print entries for excel table
;; ===============================

print, 'AIA 193'
aia_timing, jd_193, total(aia_cutouts_193[*,1:*],2), total(aia_cutouts_193[*,1:*],2)
for i=1, 5 do aia_timing, jd_193, aia_cutouts_193[*,i], total(aia_cutouts_193[*,1:*],2)
print, ''
print, 'AIA 171'
aia_timing, jd_171, total(aia_cutouts_171[*,1:*],2), total(aia_cutouts_171[*,1:*],2)
for i=1, 5 do aia_timing, jd_171, aia_cutouts_171[*,i], total(aia_cutouts_171[*,1:*],2)
print, ''
print, 'AIA 304'
aia_timing, jd_304, total(aia_cutouts_304[*,1:*],2), total(aia_cutouts_304[*,1:*],2)
for i=1, 5 do aia_timing, jd_304, aia_cutouts_304[*,i], total(aia_cutouts_304[*,1:*],2)


;;print, ''
;;print, ''
;;aia_timing, jd_171, total(aia_sectors_171[1:*,*], 1)
;;aia_timing, jd_171, aia_filament_171
;;aia_timing, jd_193, total(aia_sectors_193[1:*,*], 1)
;;aia_timing, jd_193, aia_filament_193
;;aia_timing, jd_304, total(aia_sectors_304[1:*,*], 1)
;;aia_timing, jd_304, aia_filament_304

;; ===============================
;; FIGURE: OBSCURATION
;; ===============================

set_plot, 'ps'
device, /encap, xs=12, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/fig_filament.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
!p.charsize=0.9

;; Make filament reference image
!p.multi=[0,2,1]

aia_lct, rr, gg, bb, wave='304', /load
plot_image, bytscl(alog10(min(img304, dim=3)>0)>0<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 304: Filament Mask', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39
contour, mask_filament, xx, yy, /iso, levels = findgen(max(mask_filament)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255, thick=3
contour, mask_flare, xx, yy, /iso, levels = findgen(max(mask_flare)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255, thick=3, c_linestyle=1



;; Make lightcurves
!p.multi=[3,2,3,0,1]
!p.charsize=1.8

xr = minmax(jd_171)
tmp = label_date(date_format='%H:%I UT')

loadct, 39
tmp = total(aia_sectors_193[1:*,*], 1)
plot, jd_193, 1e-6*(tmp-mean(tmp[0:4])), line=1, yr=[-6, 4], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='AIA Counts (10!E6!N DN/s)', tit='AIA 193', xmargin=[4,6]
;oplot, jd_193, 1e-5*(aia_total_193-mean(aia_total_193[0:4]))
;oplot, jd_193, 1e-5*(aia_offlimb_193-mean(aia_offlimb_193[0:4]))
;oplot, jd_193, 1e-5*(aia_sectors_193[1,*]-mean(aia_sectors_193[1,0:4]))
oplot, jd_193, 1e-6*(aia_filament_193-mean(aia_filament_193[0:2])), color=250, thick=4
oplot, !x.crange, [0,0]

tmp = total(aia_sectors_171[1:*,*], 1)
plot, jd_171, 1e-6*(tmp-mean(tmp[0:4])), line=1, yr=[-6, 4], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='AIA Counts (10!E6!N DN/s)', tit='AIA 171', xmargin=[4,6]
oplot, jd_171, 1e-6*(aia_filament_171-mean(aia_filament_171[0:2])), color=250, thick=4
oplot, !x.crange, [0,0]

tmp = total(aia_sectors_304[1:*,*], 1)
plot, jd_304, 1e-6*(tmp-mean(tmp[0:2])), line=1, yr=[-.4, .2], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, xtit='4 August 2012', $
      ytit='AIA Counts (10!E5!N DN/s)', tit='AIA 304', xmargin=[4,6], /yst
oplot, jd_304, 1e-6*(aia_filament_304-mean(aia_filament_304[0:2])), color=250, thick=4
oplot, !x.crange, [0,0]


legend, ['Total disk dimming', 'Filament only'], line=[1,0], color=[0,250], thick=4, box=0, /right, /bottom, charsize=0.7, spacing=1, pspacing=1.5

device, /close
set_plot, 'x'
!p.charsize=1

;; ===============================
;; SECTOR IMAGES
;; ===============================
LBL3:

set_plot, 'ps'
device, /encap, xs=12, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/fig_sector.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
!p.charsize=1.0

;; Make sector reference image
!p.multi=[0,2,1]

aia_lct, rr, gg, bb, wave='171', /load
plot_image, bytscl(alog10(img171[*,*,32]>0)>1.25<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 171: Sector Definitions', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39

sector_map_limited = sector_map_sm
ibad = where(sector_angle ne 150, nbad)
for i=0, nbad-1 do begin
   ii = where(sector_map_sm eq ibad[i])
   sector_map_limited[ii] = 0
endfor
contour, sector_map_limited, xx, yy, /iso, levels = findgen(max(sector_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255, c_thick=2 ;;, c_linestyle=1

;; Make lightcurves
!p.multi=[2,2,2,0,1]
!p.charsize=1.0

a = uniq(sector_radius, sort(sector_radius))
r = sector_radius[a[1:*]]
nr = n_elements(r)

a = uniq(sector_angle, sort(sector_angle))
a = sector_angle[a[1:*]]
na = n_elements(a)

xr = [(anytim2jd('2011-08-04T03:00')).(0)+(anytim2jd('2011-08-04T03:00')).(1), (anytim2jd('2011-08-04T06:00')).(0)+(anytim2jd('2011-08-04T06:00')).(1)]
tmp = label_date(date_format='%H:%I UT')
loadct, 4

igd_jd = where(jd_193 le (anytim2jd('2011-08-04T6:30')).(0)+(anytim2jd('2011-08-04T6:30')).(1))
plot, [0], [0], line=1, yr=[-.2, .1], thick=4, /nodata, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=1, xminor=4, xr=xr, $
      ytit='AIA Counts (10!E6!N DN/s)', tit='AIA 193: EUV Wave', xmargin=[6,6]
oplot, !x.crange, [0,0]
colors = findgen(nr)/(nr-1)*225+25
for i=0, nr-1 do begin
   igd = max(where(sector_radius eq r[i] AND sector_angle eq 150, ngd))
   if ngd ge 1 then oplot, jd_193[igd_jd], (aia_sectors_193[igd,igd_jd]-mean(aia_sectors_193[igd,0:4]))*1e-6, color=colors[i], thick=4
endfor

igd_jd = where(jd_171 le (anytim2jd('2011-08-04T5:25')).(0)+(anytim2jd('2011-08-04T5:25')).(1))
plot, [0], [0], line=1, yr=[-.2, .1], thick=4, /nodata, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=1, xminor=4, xr=xr, $
      ytit='AIA Counts (10!E6!N DN/s)', tit='AIA 171: EUV Wave', xmargin=[6,6]
oplot, [!x.crange[0], (anytim2jd('2011-08-04T5:25')).(0)+(anytim2jd('2011-08-04T5:25')).(1)], [0,0]
colors = findgen(nr)/(nr-1)*225+25
for i=0, nr-1 do begin
   igd = max(where(sector_radius eq r[i] AND sector_angle eq 150, ngd))
   if ngd ge 1 then oplot, jd_171[igd_jd], (aia_sectors_171[igd,igd_jd]-mean(aia_sectors_171[igd,0:4]))*1e-6, color=colors[i], thick=4
endfor

legend, ['R='+string(r, format='(F3.1)')], color=colors, line=0, thick=4, box=0, /right, /bottom, charsize=0.7, spacing=1, pspacing=1.5

device, /close
set_plot, 'x'
!p.charsize=1

STOP

;; ===============================
;; SECTOR IMAGES (ALL)
;; ===============================

set_plot, 'ps'
device, /encap, xs=18, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/fig_sector_all.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
!p.charsize=1.8

;; Make sector reference image
!p.multi=[0,3,1]

aia_lct, rr, gg, bb, wave='171', /load
plot_image, bytscl(alog10(img171[*,*,32]>0)>1.25<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 171: Sector Definitions', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39
contour, sector_map_sm, xx, yy, /iso, levels = findgen(max(sector_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255, c_thick=2 ;;, c_linestyle=1
contour, limb_map_sm, xx, yy, /iso, levels = findgen(max(limb_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255, c_thick=2;;, c_linestyle=1

lon = replicate(36, 90-19)
lat = findgen(90-19)+19
xy = hel2arcmin(lat, lon, date=hdr171[0].t_obs)
loadct, 39
oplot, xy[0,*]*60., xy[1,*]*60., color=50, thick=8

pb0r = pb0r(hdr171[32].t_obs)
oplot, [0,0], [1.025*pb0r[2]*60., !y.crange[1]], color=250, thick=8

;; Make lightcurves
!p.multi=[4,3,2,0,1]
!p.charsize=1.8

a = uniq(sector_radius, sort(sector_radius))
r = sector_radius[a[1:*]]
nr = n_elements(r)

a = uniq(sector_angle, sort(sector_angle))
a = sector_angle[a[1:*]]
na = n_elements(a)

xr = minmax(jd_171)
tmp = label_date(date_format='%H:%I UT')
loadct, 4
igd_jd = where(jd_171 le (anytim2jd('2011-08-04T15:00')).(0)+(anytim2jd('2011-08-04T15:00')).(1))

tmp = total(aia_sectors_193[1:*,*], 1)
plot, jd_193, (tmp-mean(tmp[0:4]))*1e-6, line=1, yr=[-6, 4], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='10!E6!N DN/s', tit='AIA 193: On-disk Dimming', xmargin=[4,6]
oplot, !x.crange, [0,0]
colors = findgen(nr+1)/(nr)*225+25
oplot, jd_193, (aia_sectors_193[0,*]-mean(aia_sectors_193[0,0:4]))*1e-6, color=colors[0], thick=4
for i=0, nr-1 do begin
   igd = where(sector_radius eq r[i])
   tmp = total(aia_sectors_193[igd,*], 1)
   oplot, jd_193, (tmp-mean(tmp[0:4]))*1e-6, color=colors[i+1], thick=4
endfor

tmp = total(aia_sectors_171[1:*,*], 1)
plot, jd_171[igd_jd], (tmp[igd_jd]-mean(tmp[0:4]))*1e-6, line=1, yr=[-6, 4], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='10!E6!N DN/s', tit='AIA 171: On-disk Dimming', xmargin=[4,6]
oplot, !x.crange, [0,0]
colors = findgen(nr+1)/(nr)*225+25
oplot, jd_171[igd_jd], (aia_sectors_171[0,igd_jd]-mean(aia_sectors_171[0,0:4]))*1e-6, color=colors[0], thick=4
for i=0, nr-1 do begin
   igd = where(sector_radius eq r[i])
   tmp = total(aia_sectors_171[igd,*], 1)
   oplot, jd_171[igd_jd], (tmp[igd_jd]-mean(tmp[0:4]))*1e-6, color=colors[i+1], thick=4
endfor

legend, ['R=0.0', 'R='+string(r, format='(F3.1)'),'Total disk dimming'], color=[colors,0], line=[replicate(0,nr+1),1], thick=4, box=0, /right, /bottom, charsize=0.7, spacing=1, pspacing=1.5

plot, jd_193, (aia_offlimb_193-mean(aia_offlimb_193[0:4]))*1e-6, line=1, yr=[-2, 1], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='10!E6!N DN/s', tit='AIA 193: Off-limb Dimming', xmargin=[4,6]
oplot, !x.crange, [0,0]
colors = findgen(nofflimb_sector)/(nofflimb_sector-1)*225+25
for i=0, nofflimb_sector-1 do begin
   oplot, jd_193, (aia_offlimb_sectors_193[i,*]-mean(aia_offlimb_sectors_193[i,0:4]))*1e-6, color=colors[i], thick=4
endfor

plot, jd_171[igd_jd], (aia_offlimb_171[igd_jd]-mean(aia_offlimb_171[0:4]))*1e-6, line=1, yr=[-2, 1], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='10!E6!N DN/s', tit='AIA 171: Off-limb Dimming', xmargin=[4,6]
oplot, !x.crange, [0,0]
colors = findgen(nofflimb_sector)/(nofflimb_sector-1)*225+25
for i=0, nofflimb_sector-1 do begin
   oplot, jd_171[igd_jd], (aia_offlimb_sectors_171[i,igd_jd]-mean(aia_offlimb_sectors_171[i,0:4]))*1e-6, color=colors[i], thick=4
endfor

offlimb_angle = (findgen(nofflimb_sector)+0.5)/(nofflimb_sector)*360.
legend, [greek('theta')+'='+strtrim(string(offlimb_angle, format='(F5.1)'),2), 'Total off-limb dimming'], color=[colors,0], line=[replicate(0,nofflimb_sector),1], thick=4, box=0, /right, /bottom, charsize=0.7, spacing=1, pspacing=1.5


device, /close
set_plot, 'x'
!p.charsize=1

;; ===============================
;; FEATURE IMAGES
;; ===============================


colors = reverse(findgen(6)*50.)

set_plot, 'ps'
device, /encap, xs=12, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/fig_features.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
!p.charsize=0.9

img_ref_193 = total(img193[*,*,0:4], 3)/5.
img_dimming_193 = smooth(img_ref_193 - min(img193, dim=3), 2)
img_dimming_193 = 255-bytscl(alog10(img_dimming_193>0)>0<2.7)

img_ref_171 = total(img171[*,*,0:4], 3)/5.
img_dimming_171 = smooth(img_ref_171 - min(img171, dim=3), 2)
img_dimming_171 = 255-bytscl(alog10(img_dimming_171>0)>0<2.7)

img_ref_304 = total(img304[*,*,0:1], 3)/2.
img_dimming_304 = smooth(img_ref_304 - min(img304, dim=3), 2)
img_dimming_304 = 255-bytscl(alog10(img_dimming_304>0)>0<1.7)

!p.multi=[0,2,1]

loadct, 0
plot_image, img_dimming_171, xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 171: Dimming Image', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 4
contour, mask_ar, xx, yy, /iso, levels = [0,1], /over, /xst, /yst, xr=xr, yr=yr, c_color=colors[4], c_thick=6, c_linestyle=0
contour, mask_ch, xx, yy, /iso, levels = [0,1], /over, /xst, /yst, xr=xr, yr=yr, c_color=colors[3], c_thick=3, c_linestyle=0
contour, mask_filament_combo, xx, yy, /iso, levels = [0,1], /over, /xst, /yst, xr=xr, yr=yr, c_color=colors[2], c_thick=3, c_linestyle=0
contour, mask_core, xx, yy, /iso, levels = [0,1], /over, /xst, /yst, xr=xr, yr=yr, c_color=colors[1], c_thick=3, c_linestyle=0
contour, mask_flare, xx, yy, /iso, levels = [0,1], /over, /xst, /yst, xr=xr, yr=yr, c_color=colors[0], c_thick=3, c_linestyle=0

;; Make lightcurves
!p.multi=[3,2,3,0,1]
!p.charsize=1.8

xr = minmax(jd_171)
tmp = label_date(date_format='%H:%I UT')
loadct, 4
igd_jd = where(jd_171 le (anytim2jd('2011-08-04T15:00')).(0)+(anytim2jd('2011-08-04T15:00')).(1))

tmp = total(aia_cutouts_193[*,1:*], 2)
plot, jd_193, (tmp-mean(tmp[0:4]))*1e-6, line=1, yr=[-6, 4], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='AIA Counts (10!E6!N DN/s)', tit='AIA 193: Lightcurves', xmargin=[4,6]
oplot, !x.crange, [0,0]
for i=0, n_elements(aia_cutouts_193[0,*])-1 do begin
   oplot, jd_193, (aia_cutouts_193[*,i]-mean(aia_cutouts_193[0:4,i]))*1e-6, color=colors[i], thick=4
endfor

tmp = total(aia_cutouts_171[*,1:*], 2)
plot, jd_171, (tmp-mean(tmp[0:4]))*1e-6, line=1, yr=[-6, 4], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='AIA Counts (10!E6!N DN/s)', tit='AIA 171: Lightcurves', xmargin=[4,6]
oplot, !x.crange, [0,0]
for i=0, n_elements(aia_cutouts_171[0,*])-1 do begin
   oplot, jd_171, (aia_cutouts_171[*,i]-mean(aia_cutouts_171[0:4,i]))*1e-6, color=colors[i], thick=4
endfor

tmp = total(aia_cutouts_304[*,1:*], 2)
plot, jd_304, (tmp-mean(tmp[0:1]))*1e-6, line=1, yr=[-.6, .4], thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      ytit='AIA Counts (10!E6!N DN/s)', tit='AIA 304: Lightcurves', xmargin=[4,6],/yst
oplot, !x.crange, [0,0]
for i=0, n_elements(aia_cutouts_304[0,*])-1 do begin
   oplot, jd_304, (aia_cutouts_304[*,i]-mean(aia_cutouts_304[0:1,i]))*1e-6, color=colors[i], thick=4
endfor

legend, ['Flare', 'Core dimming', 'Filament', 'Polar CH', 'Other ARs', 'Quiet Sun', 'Total disk dimming'], color=[colors,0], line=[replicate(0,n_elements(aia_cutouts_193[0,*])),1], thick=4, box=0, /right, /bottom, charsize=0.7, spacing=1, pspacing=1.5



!p.multi=0
device, /close
set_plot, 'x'
!p.charsize=1


;; ===============================
;; IRRADIANCE IMAGES
;; ===============================

LBL:
set_plot, 'ps'
device, /encap, xs=18, ys=9, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/fig_irrad.eps', bits=8
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
!p.charsize=1.8


xr = minmax(jd_171)
tmp = label_date(date_format=' ')
loadct, 4

!p.multi=[0,2,1,0,0]
loadct, 0
tv, rearrange(composite_img, [2,3,1]), 2.25, 1.5, xs=3077./3477.*7.375, ys=7.375, /inch, true=3

!p.multi=[5,2,5,0,1]
loadct, 4

i_preflare = where(evl_time_jd ge (anytim2jd('2011-08-04T03:00')).(0)+(anytim2jd('2011-08-04T03:00')).(1) AND $
                   evl_time_jd lt (anytim2jd('2011-08-04T03:30')).(0)+(anytim2jd('2011-08-04T03:30')).(1))

plot, [0], [0], /nodata, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      yr=[-5, 30], /yst, xmargin=[8,8], ymargin=[0,1], $
      ytit='EVE Irradiance ('+greek('mu')+'W/m!E2!N)'
oplot, !x.crange, [0,0]
avg = mean(evl[i_preflare].line_irradiance[2])
oplot, evl_time_jd, (evl.line_irradiance[2]-avg)*1e6, nsum=6, thick=4, color=0
avg = mean(evl[i_preflare].line_irradiance[12])
oplot, evl_time_jd, (evl.line_irradiance[12]-avg)*1e6, nsum=6, thick=4, color=100
legend, ['EVE Fe XX 13.3 nm', 'EVE Fe XVI 33.5 nm'], line=0, color=[0,100], /top, /right, box=0, thick=4, charsize=0.7, spacing=1, pspacing=1.5
;xyouts, 0.02*(!x.crange[1]-!x.crange[0])+!x.crange[0], 0.9*(!y.crange[1]-!y.crange[0])+!y.crange[0], 'a)', charsize=1.2

avg = mean(evl[i_preflare].line_irradiance[6])
plot, evl_time_jd, (evl.line_irradiance[6]-avg)*1e6, nsum=16, thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      yr=[-2,6], yst=9, xmargin=[8,8], ymargin=[0,1], $
      ytit='EVE Irradiance ('+greek('mu')+'W/m!E2!N)';;, tit='Fe XII 19.5 nm'
oplot, !x.crange, [0,0]
axis, yaxis=1, yr=[-5, 15], ytit='AIA Counts (10!E6!N DN/s)', color=150, /yst, /save
oplot, jd_193, 1e-6*(aia_total_193-mean(aia_total_193[0:4])), color=150, thick=4
oplot, jd_193, 1e-6*(aia_disk_193-mean(aia_disk_193[0:4])), color=150, thick=4, line=2
oplot, jd_193, 1e-6*(aia_offlimb_193-mean(aia_offlimb_193[0:4])), color=150, thick=4, line=1
legend, ['EVE Fe XII 19.5 nm', 'AIA 195 '+['Total', 'On-disk', 'Off-Limb']], line=[0,0,2,1], color=[0,150,150,150], /top, /right, box=0, thick=4, charsize=0.7, spacing=1, pspacing=1.5
;xyouts, 0.02*(!x.crange[1]-!x.crange[0])+!x.crange[0], 0.9*(!y.crange[1]-!y.crange[0])+!y.crange[0], 'b)', charsize=1.2

avg = mean(evl[i_preflare].line_irradiance[3])
plot, evl_time_jd, (evl.line_irradiance[3]-avg)*1e6, nsum=16, thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      yr=[-4,4], yst=9, xmargin=[8,8], ymargin=[0,1], $
      ytit='EVE Irradiance ('+greek('mu')+'W/m!E2!N)';;, tit='Fe IX 17.1 nm'
oplot, !x.crange, [0,0]
axis, yaxis=1, yr=[-6, 6], ytit='AIA Counts (10!E6!N DN/s)', color=150, /yst, /save
oplot, jd_171, 1e-6*(aia_total_171-mean(aia_total_171[0:4])), color=150, thick=4
oplot, jd_171, 1e-6*(aia_disk_171-mean(aia_disk_171[0:4])), color=150, thick=4, line=2
oplot, jd_171, 1e-6*(aia_offlimb_171-mean(aia_offlimb_171[0:4])), color=150, thick=4, line=1
legend, ['Fe IX 17.1 nm', 'AIA 171 '+['Total', 'On-disk', 'Off-Limb']], line=[0,0,2,1], color=[0,150,150,150], /top, /right, box=0, thick=4, charsize=0.7, spacing=1, pspacing=1.5
;xyouts, 0.02*(!x.crange[1]-!x.crange[0])+!x.crange[0], 0.9*(!y.crange[1]-!y.crange[0])+!y.crange[0], 'c)', charsize=1.2

tmp = label_date(date_format='%H:%I UT')
avg = mean(evl[i_preflare].line_irradiance[11])
plot, evl_time_jd, (evl.line_irradiance[11]-avg)*1e6, nsum=16, thick=4, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xminor=6, xr=xr, $
      yr=[-10,80], yst=9, xmargin=[8,8], ymargin=[0,1], $
      ytit='EVE Irradiance ('+greek('mu')+'W/m!E2!N)', xtit='4 August 2011'
oplot, !x.crange, [0,0]
axis, yaxis=1, yr=[-.375, 3.0], ytit='AIA Counts (10!E6!N DN/s)', color=150, /yst, /save
oplot, jd_304, 1e-6*(aia_total_304-mean(aia_total_304[0:2])), color=150, thick=4
oplot, jd_304, 1e-6*(aia_disk_304-mean(aia_disk_304[0:4])), color=150, thick=4, line=2
oplot, jd_304, 1e-6*(aia_offlimb_304-mean(aia_offlimb_304[0:4])), color=150, thick=4, line=1
legend, ['He II 30.4 nm', 'AIA 304 '+['Total', 'On-disk', 'Off-Limb']], line=[0,0,2,1], color=[0,150,150,150], /top, /right, box=0, thick=4, charsize=0.7, spacing=1, pspacing=1.5
;xyouts, 0.02*(!x.crange[1]-!x.crange[0])+!x.crange[0], 0.9*(!y.crange[1]-!y.crange[0])+!y.crange[0], 'd)', charsize=1.2


!p.multi=0
device, /close
set_plot, 'x'
!p.charsize=1


END
