;; Code that needs to be run once for this to work
;; Step 1: .r combine_aia_derot_img.pro
;; Step 2: .r sector_map.pro
;; Step 3: .r euv_dimming_masks.pro

;; ===============================
;; GENERAL STUFF
;; ===============================

restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/sector_maps.sav'
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/cutout_masks.sav'

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

xx = ((findgen(1024)#replicate(1, 1024))-512)*2.4
yy = ((replicate(1, 1024)#findgen(1024))-512)*2.4

dimming_obj = obj_new('Blob_Analyzer', mask_dimming)
ndimming = dimming_obj -> numberofblobs()

offlimb_obj = obj_new('Blob_Analyzer', mask_offlimb)
nofflimb = offlimb_obj -> numberofblobs()

region_obj = obj_new('Blob_Analyzer', mask_region)
nregion = region_obj -> numberofblobs()

;; ===============================
;; AIA 304
;; ===============================

;; Restore AIA images
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a304_derot_barbara.sav'
nimg = n_elements(hdr304)

;; Define time variables
jd = anytim2jd(hdr304.t_obs)
jd = jd.int+jd.frac
tmp = label_date(date_format='%H:%I')

;; Make sector reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a304_sector_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img304[*,*,32]>0)>0<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 304: Sector Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
contour, sector_map_sm, xx, yy, /iso, levels = findgen(max(sector_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255
contour, limb_map_sm, xx, yy, /iso, levels = findgen(max(limb_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255

lon = replicate(36, 90-19)
lat = findgen(90-19)+19
xy = hel2arcmin(lat, lon, date=hdr304[0].t_obs)
loadct, 39
oplot, xy[0,*]*60., xy[1,*]*60., color=250, thick=8

device, /close
set_plot, 'x'

;; Make dimming reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a304_dimming_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img304[*,*,32]>0)>0<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 304: Cutout Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39
contour, mask_dimming, xx, yy, /iso, levels = findgen(max(mask_dimming)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=100, thick=3
contour, mask_offlimb, xx, yy, /iso, levels = findgen(max(mask_offlimb)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=150, thick=3

for idimming=0, ndimming-1 do begin
   ii = dimming_obj -> getindices(idimming)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(idimming+1, format='(I2)'),2), color=100, charsize=0.7
endfor

for iofflimb=0, nofflimb-1 do begin
   ii = offlimb_obj -> getindices(iofflimb)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(iofflimb+1, format='(I1)'),2), color=150, charsize=0.7
endfor

device, /close
set_plot, 'x'

;; Make output array: total
aia_total = total(total(img304, 1), 1)

;; Make output array: offlimb (all)
aia_offlimb_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img304[*,*,iimg]
   aia_offlimb_all[iimg] = total(tmp_img*mask_overlimb_sm)
endfor

;; Make output array: disk (all)
aia_disk_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img304[*,*,iimg]
   aia_disk_all[iimg] = total(tmp_img*mask_limb_sm)
endfor

;; Make output array: active regions
aia_regions = fltarr(nregion, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img304[*,*,iimg]
   for iregion=0, nregion-1 do begin
      ii = region_obj -> getindices(iregion)
      aia_regions[iregion, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: dimming regions
aia_dimmings = fltarr(ndimming, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img304[*,*,iimg]
   for idimming=0, ndimming-1 do begin
      ii = dimming_obj -> getindices(idimming)
      aia_dimmings[idimming, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: offlimb regions
aia_offlimbs = fltarr(nofflimb, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img304[*,*,iimg]
   for iofflimb=0, nofflimb-1 do begin
      ii = offlimb_obj -> getindices(iofflimb)
      aia_offlimbs[iofflimb, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: disk sectors
aia_sectors = fltarr(nsector, nimg)
tmp = histogram(sector_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img304[*,*,iimg]
   for isector=0, nsector-1 do begin
      igd =  indices[indices[isector+1]:indices[isector+2]-1]
      aia_sectors[isector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make output array: limb sectors
aia_offlimb_sectors = fltarr(nofflimb_sector, nimg)
tmp = histogram(limb_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img304[*,*,iimg]
   for iofflimb_sector=0, nofflimb_sector-1 do begin
      igd =  indices[indices[iofflimb_sector+1]:indices[iofflimb_sector+2]-1]
      aia_offlimb_sectors[iofflimb_sector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make plots
scale = 1.d0/abs(min(aia_disk_all-mean(aia_disk_all[0:4])))

a = uniq(sector_radius, sort(sector_radius))
r = sector_radius[a[1:*]]
nr = n_elements(r)

a = uniq(sector_angle, sort(sector_angle))
a = sector_angle[a[1:*]]
na = n_elements(a)

;; Make overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a304_lightcurves_overview.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

plot, jd, (aia_total-mean(aia_total[0:4]))*scale, yr=[-1.75, 1.5], /yst, /nodata, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 304', thick=4
oplot, !x.crange, [0,0]
oplot, jd, (aia_total-mean(aia_total[0:4]))*scale, color=50, thick=4
oplot, jd, (aia_disk_all-mean(aia_disk_all[0:4]))*scale, color=100, thick=4
oplot, jd, (aia_offlimb_all-mean(aia_offlimb_all[0:4]))*scale, color=170, thick=4
residual = aia_total-aia_disk_all-aia_offlimb_all
oplot, jd, (residual-mean(residual[0:4]))*scale, color=200, thick=4
legend, ['Total', 'Disk', 'Off-limb', 'Limb'], color=[50, 100, 170, 200], line=[0,0,0,0], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'


;; Make sector overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a304_lightcurves_sectors.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

tmp = total(aia_sectors[1:*,*], 1)
plot, jd, (tmp-mean(tmp[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 304', thick=4
oplot, !x.crange, [0,0]
colors = findgen(nr+1)/(nr)*225+25
oplot, jd, (aia_sectors[0,*]-mean(aia_sectors[0,0:4]))*scale, color=colors[0], thick=4
for i=0, nr-1 do begin
   igd = where(sector_radius eq r[i])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i+1], thick=4
endfor
legend, ['Total Dimming', 'R=0.00', 'R='+string(r, format='(F4.2)')], color=[0,colors], line=[1,replicate(0,nr+1)], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'

;; Make sector core plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a304_lightcurves_sector_cores.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

igd = where(sector_radius ge r[0] AND sector_radius le r[1])

tmp = total(aia_sectors[igd,*], 1)
plot, jd, (tmp-mean(tmp[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 304', thick=4
oplot, !x.crange, [0,0]

colors = findgen(na/2.)/(na/2.-1)*225+25
tmp_angle = -1
for i=0, na-1., 2. do begin
   igd = where(sector_radius ge r[0] AND sector_radius le r[1] and sector_angle ge a[i] AND sector_angle le a[i+1])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i/2.], thick=4
   tmp_angle = [tmp_angle, mean([a[i], a[i+1]])]
endfor
tmp_angle = tmp_angle[1:*]
legend, ['Sector Total', greek('theta')+'='+strtrim(string(tmp_angle, format='(I3)'),2)], color=[0,colors], $
        line=[1,replicate(0,n_elements(tmp_angle))], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'

STOP

;; ===============================
;; AIA 171
;; ===============================

;; Restore AIA images
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a171_derot_james.sav'
nimg = n_elements(hdr171)

;; Define time variables
jd = anytim2jd(hdr171.t_obs)
jd = jd.int+jd.frac
tmp = label_date(date_format='%H:%I')

;; Make sector reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a171_sector_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img171[*,*,32]>0)>1.25<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 171: Sector Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
contour, sector_map_sm, xx, yy, /iso, levels = findgen(max(sector_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255
contour, limb_map_sm, xx, yy, /iso, levels = findgen(max(limb_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255

lon = replicate(36, 90-19)
lat = findgen(90-19)+19
xy = hel2arcmin(lat, lon, date=hdr171[0].t_obs)
loadct, 39
oplot, xy[0,*]*60., xy[1,*]*60., color=250, thick=8

device, /close
set_plot, 'x'

;; Make dimming reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a171_dimming_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img171[*,*,32]>0)>1.25<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 171: Cutout Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39
contour, mask_dimming, xx, yy, /iso, levels = findgen(max(mask_dimming)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=100, thick=3
contour, mask_offlimb, xx, yy, /iso, levels = findgen(max(mask_offlimb)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=150, thick=3

for idimming=0, ndimming-1 do begin
   ii = dimming_obj -> getindices(idimming)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(idimming+1, format='(I2)'),2), color=100, charsize=0.7
endfor

for iofflimb=0, nofflimb-1 do begin
   ii = offlimb_obj -> getindices(iofflimb)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(iofflimb+1, format='(I1)'),2), color=150, charsize=0.7
endfor

device, /close
set_plot, 'x'

;; Make output array: total
aia_total = total(total(img171, 1), 1)

;; Make output array: offlimb (all)
aia_offlimb_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img171[*,*,iimg]
   aia_offlimb_all[iimg] = total(tmp_img*mask_overlimb_sm)
endfor

;; Make output array: disk (all)
aia_disk_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img171[*,*,iimg]
   aia_disk_all[iimg] = total(tmp_img*mask_limb_sm)
endfor

;; Make output array: active regions
aia_regions = fltarr(nregion, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img171[*,*,iimg]
   for iregion=0, nregion-1 do begin
      ii = region_obj -> getindices(iregion)
      aia_regions[iregion, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: dimming regions
aia_dimmings = fltarr(ndimming, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img171[*,*,iimg]
   for idimming=0, ndimming-1 do begin
      ii = dimming_obj -> getindices(idimming)
      aia_dimmings[idimming, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: offlimb regions
aia_offlimbs = fltarr(nofflimb, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img171[*,*,iimg]
   for iofflimb=0, nofflimb-1 do begin
      ii = offlimb_obj -> getindices(iofflimb)
      aia_offlimbs[iofflimb, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: disk sectors
aia_sectors = fltarr(nsector, nimg)
tmp = histogram(sector_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img171[*,*,iimg]
   for isector=0, nsector-1 do begin
      igd =  indices[indices[isector+1]:indices[isector+2]-1]
      aia_sectors[isector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make output array: limb sectors
aia_offlimb_sectors = fltarr(nofflimb_sector, nimg)
tmp = histogram(limb_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img171[*,*,iimg]
   for iofflimb_sector=0, nofflimb_sector-1 do begin
      igd =  indices[indices[iofflimb_sector+1]:indices[iofflimb_sector+2]-1]
      aia_offlimb_sectors[iofflimb_sector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make plots
scale = 1.d0/abs(min(aia_disk_all-mean(aia_disk_all[0:4])))

a = uniq(sector_radius, sort(sector_radius))
r = sector_radius[a[1:*]]
nr = n_elements(r)

a = uniq(sector_angle, sort(sector_angle))
a = sector_angle[a[1:*]]
na = n_elements(a)

;; Make overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a171_lightcurves_overview.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

plot, jd, (aia_total-mean(aia_total[0:4]))*scale, yr=[-1.75, 1.5], /yst, /nodata, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 171', thick=4
oplot, !x.crange, [0,0]
oplot, jd, (aia_total-mean(aia_total[0:4]))*scale, color=50, thick=4
oplot, jd, (aia_disk_all-mean(aia_disk_all[0:4]))*scale, color=100, thick=4
oplot, jd, (aia_offlimb_all-mean(aia_offlimb_all[0:4]))*scale, color=170, thick=4
residual = aia_total-aia_disk_all-aia_offlimb_all
oplot, jd, (residual-mean(residual[0:4]))*scale, color=200, thick=4
legend, ['Total', 'Disk', 'Off-limb', 'Limb'], color=[50, 100, 170, 200], line=[0,0,0,0], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'


;; Make sector overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a171_lightcurves_sectors.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

tmp = total(aia_sectors[1:*,*], 1)
plot, jd, (tmp-mean(tmp[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 171', thick=4
oplot, !x.crange, [0,0]
colors = findgen(nr+1)/(nr)*225+25
oplot, jd, (aia_sectors[0,*]-mean(aia_sectors[0,0:4]))*scale, color=colors[0], thick=4
for i=0, nr-1 do begin
   igd = where(sector_radius eq r[i])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i+1], thick=4
endfor
legend, ['Total Dimming', 'R=0.00', 'R='+string(r, format='(F4.2)')], color=[0,colors], line=[1,replicate(0,nr+1)], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'

;; Make sector core plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a171_lightcurves_sector_cores.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

igd = where(sector_radius ge r[0] AND sector_radius le r[1])

tmp = total(aia_sectors[igd,*], 1)
plot, jd, (tmp-mean(tmp[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 171', thick=4
oplot, !x.crange, [0,0]

colors = findgen(na/2.)/(na/2.-1)*225+25
tmp_angle = -1
for i=0, na-1., 2. do begin
   igd = where(sector_radius ge r[0] AND sector_radius le r[1] and sector_angle ge a[i] AND sector_angle le a[i+1])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i/2.], thick=4
   tmp_angle = [tmp_angle, mean([a[i], a[i+1]])]
endfor
tmp_angle = tmp_angle[1:*]
legend, ['Sector Total', greek('theta')+'='+strtrim(string(tmp_angle, format='(I3)'),2)], color=[0,colors], $
        line=[1,replicate(0,n_elements(tmp_angle))], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'



STOP

;; ===============================
;; AIA 193
;; ===============================

;; Restore AIA images
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a193_derot_james.sav'
nimg = n_elements(hdr193)

;; Define time variables
jd = anytim2jd(hdr193.t_obs)
jd = jd.int+jd.frac
tmp = label_date(date_format='%H:%I')

;; Make sector reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a193_sector_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img193[*,*,32]>0)>1.5<4), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 193: Sector Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
contour, sector_map_sm, xx, yy, /iso, levels = findgen(max(sector_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255
contour, limb_map_sm, xx, yy, /iso, levels = findgen(max(limb_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255

lon = replicate(36, 90-19)
lat = findgen(90-19)+19
xy = hel2arcmin(lat, lon, date=hdr193[0].t_obs)
loadct, 39
oplot, xy[0,*]*60., xy[1,*]*60., color=250, thick=8

device, /close
set_plot, 'x'

;; Make dimming reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a193_dimming_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img193[*,*,32]>0)>1.5<4), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 193: Cutout Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39
contour, mask_dimming, xx, yy, /iso, levels = findgen(max(mask_dimming)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=100, thick=3
contour, mask_offlimb, xx, yy, /iso, levels = findgen(max(mask_offlimb)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=150, thick=3

for idimming=0, ndimming-1 do begin
   ii = dimming_obj -> getindices(idimming)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(idimming+1, format='(I2)'),2), color=100, charsize=0.7
endfor

for iofflimb=0, nofflimb-1 do begin
   ii = offlimb_obj -> getindices(iofflimb)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(iofflimb+1, format='(I1)'),2), color=150, charsize=0.7
endfor

device, /close
set_plot, 'x'

;; Make output array: total
aia_total = total(total(img193, 1), 1)

;; Make output array: offlimb (all)
aia_offlimb_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img193[*,*,iimg]
   aia_offlimb_all[iimg] = total(tmp_img*mask_overlimb_sm)
endfor

;; Make output array: disk (all)
aia_disk_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img193[*,*,iimg]
   aia_disk_all[iimg] = total(tmp_img*mask_limb_sm)
endfor

;; Make output array: active regions
aia_regions = fltarr(nregion, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img193[*,*,iimg]
   for iregion=0, nregion-1 do begin
      ii = region_obj -> getindices(iregion)
      aia_regions[iregion, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: dimming regions
aia_dimmings = fltarr(ndimming, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img193[*,*,iimg]
   for idimming=0, ndimming-1 do begin
      ii = dimming_obj -> getindices(idimming)
      aia_dimmings[idimming, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: offlimb regions
aia_offlimbs = fltarr(nofflimb, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img193[*,*,iimg]
   for iofflimb=0, nofflimb-1 do begin
      ii = offlimb_obj -> getindices(iofflimb)
      aia_offlimbs[iofflimb, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: disk sectors
aia_sectors = fltarr(nsector, nimg)
tmp = histogram(sector_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img193[*,*,iimg]
   for isector=0, nsector-1 do begin
      igd =  indices[indices[isector+1]:indices[isector+2]-1]
      aia_sectors[isector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make output array: limb sectors
aia_offlimb_sectors = fltarr(nofflimb_sector, nimg)
tmp = histogram(limb_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img193[*,*,iimg]
   for iofflimb_sector=0, nofflimb_sector-1 do begin
      igd =  indices[indices[iofflimb_sector+1]:indices[iofflimb_sector+2]-1]
      aia_offlimb_sectors[iofflimb_sector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make plots
scale = 1.d0/abs(min(aia_disk_all-mean(aia_disk_all[0:4])))

a = uniq(sector_radius, sort(sector_radius))
r = sector_radius[a[1:*]]
nr = n_elements(r)

a = uniq(sector_angle, sort(sector_angle))
a = sector_angle[a[1:*]]
na = n_elements(a)

;; Make overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a193_lightcurves_overview.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

plot, jd, (aia_total-mean(aia_total[0:4]))*scale, yr=[-1.75, 1.5], /yst, /nodata, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 193', thick=4
oplot, !x.crange, [0,0]
oplot, jd, (aia_total-mean(aia_total[0:4]))*scale, color=50, thick=4
oplot, jd, (aia_disk_all-mean(aia_disk_all[0:4]))*scale, color=100, thick=4
oplot, jd, (aia_offlimb_all-mean(aia_offlimb_all[0:4]))*scale, color=170, thick=4
residual = aia_total-aia_disk_all-aia_offlimb_all
oplot, jd, (residual-mean(residual[0:4]))*scale, color=200, thick=4
legend, ['Total', 'Disk', 'Off-limb', 'Limb'], color=[50, 100, 170, 200], line=[0,0,0,0], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'


;; Make sector overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a193_lightcurves_sectors.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

plot, jd, (aia_disk_all-mean(aia_disk_all[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 193', thick=4
oplot, !x.crange, [0,0]
colors = findgen(nr+1)/(nr)*225+25
oplot, jd, (aia_sectors[0,*]-mean(aia_sectors[0,0:4]))*scale, color=colors[0], thick=4
for i=0, nr-1 do begin
   igd = where(sector_radius eq r[i])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i+1], thick=4
endfor
legend, ['Disk Total', 'R=0.00', 'R='+string(r, format='(F4.2)')], color=[0,colors], line=[1,replicate(0,nr+1)], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'

;; Make sector core plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a193_lightcurves_sector_cores.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

igd = where(sector_radius ge r[0] AND sector_radius le r[1])

tmp = total(aia_sectors[igd,*], 1)
plot, jd, (tmp-mean(tmp[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 193', thick=4
oplot, !x.crange, [0,0]

colors = findgen(na/2.)/(na/2.-1)*225+25
tmp_angle = -1
for i=0, na-1., 2. do begin
   igd = where(sector_radius ge r[0] AND sector_radius le r[1] and sector_angle ge a[i] AND sector_angle le a[i+1])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i/2.], thick=4
   tmp_angle = [tmp_angle, mean([a[i], a[i+1]])]
endfor
tmp_angle = tmp_angle[1:*]
legend, ['Sector Total', greek('theta')+'='+strtrim(string(tmp_angle, format='(I3)'),2)], color=[0,colors], $
        line=[1,replicate(0,n_elements(tmp_angle))], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'

;; ===============================
;; AIA 211
;; ===============================

;; Restore AIA images
restore, '~/Desktop/EUV_dimming_workshop/IDL_savesets/a211_derot_james.sav'
nimg = n_elements(hdr211)

;; Define time variables
jd = anytim2jd(hdr211.t_obs)
jd = jd.int+jd.frac
tmp = label_date(date_format='%H:%I')

;; Make sector reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a211_sector_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img211[*,*,32]>0)>0.5<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 211: Sector Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
contour, sector_map_sm, xx, yy, /iso, levels = findgen(max(sector_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255
contour, limb_map_sm, xx, yy, /iso, levels = findgen(max(limb_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255

lon = replicate(36, 90-19)
lat = findgen(90-19)+19
xy = hel2arcmin(lat, lon, date=hdr211[0].t_obs)
loadct, 39
oplot, xy[0,*]*60., xy[1,*]*60., color=250, thick=8

device, /close
set_plot, 'x'

;; Make dimming reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a211_dimming_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img211[*,*,32]>0)>0.55<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 211: Cutout Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39
contour, mask_dimming, xx, yy, /iso, levels = findgen(max(mask_dimming)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=100, thick=3
contour, mask_offlimb, xx, yy, /iso, levels = findgen(max(mask_offlimb)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=150, thick=3

for idimming=0, ndimming-1 do begin
   ii = dimming_obj -> getindices(idimming)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(idimming+1, format='(I2)'),2), color=100, charsize=0.7
endfor

for iofflimb=0, nofflimb-1 do begin
   ii = offlimb_obj -> getindices(iofflimb)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(iofflimb+1, format='(I1)'),2), color=150, charsize=0.7
endfor

device, /close
set_plot, 'x'

;; Make output array: total
aia_total = total(total(img211, 1), 1)

;; Make output array: offlimb (all)
aia_offlimb_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img211[*,*,iimg]
   aia_offlimb_all[iimg] = total(tmp_img*mask_overlimb_sm)
endfor

;; Make output array: disk (all)
aia_disk_all = fltarr(nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img211[*,*,iimg]
   aia_disk_all[iimg] = total(tmp_img*mask_limb_sm)
endfor

;; Make output array: active regions
aia_regions = fltarr(nregion, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img211[*,*,iimg]
   for iregion=0, nregion-1 do begin
      ii = region_obj -> getindices(iregion)
      aia_regions[iregion, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: dimming regions
aia_dimmings = fltarr(ndimming, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img211[*,*,iimg]
   for idimming=0, ndimming-1 do begin
      ii = dimming_obj -> getindices(idimming)
      aia_dimmings[idimming, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: offlimb regions
aia_offlimbs = fltarr(nofflimb, nimg)
for iimg=0, nimg-1 do begin
   tmp_img = img211[*,*,iimg]
   for iofflimb=0, nofflimb-1 do begin
      ii = offlimb_obj -> getindices(iofflimb)
      aia_offlimbs[iofflimb, iimg] = total(tmp_img[ii]) 
   endfor
endfor

;; Make output array: disk sectors
aia_sectors = fltarr(nsector, nimg)
tmp = histogram(sector_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img211[*,*,iimg]
   for isector=0, nsector-1 do begin
      igd =  indices[indices[isector+1]:indices[isector+2]-1]
      aia_sectors[isector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make output array: limb sectors
aia_offlimb_sectors = fltarr(nofflimb_sector, nimg)
tmp = histogram(limb_map_sm, reverse_indices=indices)
for iimg=0, nimg-1 do begin
   tmp_img = img211[*,*,iimg]
   for iofflimb_sector=0, nofflimb_sector-1 do begin
      igd =  indices[indices[iofflimb_sector+1]:indices[iofflimb_sector+2]-1]
      aia_offlimb_sectors[iofflimb_sector, iimg] = total(tmp_img[igd])
   endfor
endfor

;; Make plots
scale = 1.d0/abs(min(aia_disk_all-mean(aia_disk_all[0:4])))

a = uniq(sector_radius, sort(sector_radius))
r = sector_radius[a[1:*]]
nr = n_elements(r)

a = uniq(sector_angle, sort(sector_angle))
a = sector_angle[a[1:*]]
na = n_elements(a)

;; Make overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a211_lightcurves_overview.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

plot, jd, (aia_total-mean(aia_total[0:4]))*scale, yr=[-1.75, 1.5], /yst, /nodata, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 211', thick=4
oplot, !x.crange, [0,0]
oplot, jd, (aia_total-mean(aia_total[0:4]))*scale, color=50, thick=4
oplot, jd, (aia_disk_all-mean(aia_disk_all[0:4]))*scale, color=100, thick=4
oplot, jd, (aia_offlimb_all-mean(aia_offlimb_all[0:4]))*scale, color=170, thick=4
residual = aia_total-aia_disk_all-aia_offlimb_all
oplot, jd, (residual-mean(residual[0:4]))*scale, color=200, thick=4
legend, ['Total', 'Disk', 'Off-limb', 'Limb'], color=[50, 100, 170, 200], line=[0,0,0,0], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'


;; Make sector overview plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a211_lightcurves_sectors.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

plot, jd, (aia_disk_all-mean(aia_disk_all[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 211', thick=4
oplot, !x.crange, [0,0]
colors = findgen(nr+1)/(nr)*225+25
oplot, jd, (aia_sectors[0,*]-mean(aia_sectors[0,0:4]))*scale, color=colors[0], thick=4
for i=0, nr-1 do begin
   igd = where(sector_radius eq r[i])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i+1], thick=4
endfor
legend, ['Disk Total', 'R=0.00', 'R='+string(r, format='(F4.2)')], color=[0,colors], line=[1,replicate(0,nr+1)], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'

;; Make sector core plot
set_plot, 'ps'
device, /encap, xs=10, ys=6, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a211_lightcurves_sector_cores.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 4

igd = where(sector_radius ge r[0] AND sector_radius le r[1])

tmp = total(aia_sectors[igd,*], 1)
plot, jd, (tmp-mean(tmp[0:4]))*scale, /ynozero, yr=[-1.75, 1.5], /yst, line=1, $
      xtickformat='label_date', xtickunit='hour', xtickinterval=3, xtit='4 August 2011', $
      ytit='AIA Countrate (arbitrary units)', tit='AIA 211', thick=4
oplot, !x.crange, [0,0]

colors = findgen(na/2.)/(na/2.-1)*225+25
tmp_angle = -1
for i=0, na-1., 2. do begin
   igd = where(sector_radius ge r[0] AND sector_radius le r[1] and sector_angle ge a[i] AND sector_angle le a[i+1])
   tmp = total(aia_sectors[igd,*], 1)
   oplot, jd, (tmp-mean(tmp[0:4]))*scale, color=colors[i/2.], thick=4
   tmp_angle = [tmp_angle, mean([a[i], a[i+1]])]
endfor
tmp_angle = tmp_angle[1:*]
legend, ['Sector Total', greek('theta')+'='+strtrim(string(tmp_angle, format='(I3)'),2)], color=[0,colors], $
        line=[1,replicate(0,n_elements(tmp_angle))], thick=4, box=0, /right, /bottom

device, /close
set_plot, 'x'
;; ===============================
;; OTHER
;; ===============================

;; Make dimming reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a171_dimming_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img171[*,*,32]>0)>1.25<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 171: Cutout Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
loadct, 39
contour, mask_dimming, xx, yy, /iso, levels = findgen(max(mask_dimming)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=100, thick=3
contour, mask_offlimb, xx, yy, /iso, levels = findgen(max(mask_offlimb)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=150, thick=3
contour, mask_region, xx, yy, /iso, levels = findgen(max(mask_region)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=250, thick=3


for idimming=0, ndimming-1 do begin
   ii = dimming_obj -> getindices(idimming)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(idimming+1, format='(I2)'),2), color=100, charsize=0.7
endfor

for iofflimb=0, nofflimb-1 do begin
   ii = offlimb_obj -> getindices(iofflimb)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(iofflimb+1, format='(I1)'),2), color=150, charsize=0.7
endfor

for iregion=0, nregion-1 do begin
   ii = region_obj -> getindices(iregion)
   xyouts, mean(xx[ii]), mean(yy[ii]), '#'+strtrim(string(iregion+1, format='(I1)'),2), color=250, charsize=0.7
endfor

device, /close
set_plot, 'x'


;; Make sector reference image
set_plot, 'ps'
device, /encap, xs=8, ys=8, /inch, /color, file='~/Desktop/EUV_dimming_workshop/Plots/a171_sector_ref_img.eps'
!p.thick=2 & !x.thick=2 & !y.thick=2
!p.font=0
loadct, 0
plot_image, bytscl(alog10(img171[*,*,32]>0)>1.25<3), xst=1, yst=1, $
            xtit='Arcsec', ytit='Arcsec', $
            tit='AIA 171: Sector Map', $
            origin=[-512, -512]*2.4, scale=2.4
xr = !x.crange
yr = !y.crange
contour, sector_map_sm, xx, yy, /iso, levels = findgen(max(sector_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255
contour, limb_map_sm, xx, yy, /iso, levels = findgen(max(limb_map_sm)+1), /over, /xst, /yst, xr=xr, yr=yr, c_color=255

lon = replicate(36, 90-19)
lat = findgen(90-19)+19
xy = hel2arcmin(lat, lon, date=hdr171[0].t_obs)
loadct, 39
oplot, xy[0,*]*60., xy[1,*]*60., color=250, thick=8

device, /close
set_plot, 'x'
END
