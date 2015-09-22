;
;	plot X123 results versus SphinX 2009 spectrum
;
cd, '/Users/cepheid/Dropbox/bleh/jstone_files
!path = '/Users/cepheid/Dropbox/idl:'+!path
!path = '/Users/cepheid/Dropbox/bleh/idl:'+!path
restore, 'idl/x123_fluxcon_results.sav'   ;  energy, pspectrum, espectrum, lambda, spectrum, units

energy=reform(energy)
time_inds = where(pspectrum.time le 350 and pspectrum.time ge 189) ;set times +- from apogee
pflux = total(pspectrum[time_inds].data_pho, 2)/(n_elements(time_inds))

;
;	values from plot in Sylwester et al., Astrophy. J., May 2012
;
;	x1, x2, y1, y2 represent graphics pixel corners:  *p = pixel, *v = value
x1p = 62.
x1v = 1.0	; keV
x2p = 472.
x2v = 3.0	; keV
y1p = 430.
y1v = -4.0	; log(ph/sec/cm^2/keV)
y2p = 18.
y2v = 6.0	; log(ph/sec/cm^2/keV)

;  sdata is 2 x N array of SphinX 2009 spectrum
;		sdata[0,*] = X (energy pixels)
;		sdata[1,*] = Y (Flux pixels)
sdata = [ [108,89], [117,80], [130,79], [144,89], [155,125], [167,132], $
	[180,130], [192,137], [204,155], [215,163], [228,171], [241,153], $
	[252,190], [264,197], [276,207], [288,215], [300,210], [312,227], $
	[325,224], [337,241], [348,248], [360,253], [372,263], [385,271], $
	[397,281], [409,275], [422,285], [433,299], [446,290], [458,284], [470,313] ]

senergy = x1v + (reform(sdata[0,*])-x1p)*(x2v-x1v)/(x2p-x1p)
sfluxlog = y1v + (reform(sdata[1,*])-y1p)*(y2v-y1v)/(y2p-y1p)
sflux = 10.^sfluxlog

setplot, charsize=1.5
cc=rainbow(7)
!x.margin = [8,2]
!y.margin = [4,3]

plot, senergy, sflux, psym=10, xr=[0.535,5], xs=9,/xlog, yr=[1E-3,1E8], ys=1, /ylog, $
  xtitle='Energy [keV]', ytitle='Flux [photons s!U-1!N cm!U-2!N keV!U-1!N]', /nodata
axis, /xax, /xs, xr=(a2kev/10.)/(10^!x.crange), /xlog, xtitle='Wavelength [nm]',xticks=2,xtickv=[10,1],xminor=9
oplot, senergy, sflux, psym=10, color=cc[3]
oplot, energy, pflux, psym=10
;oplot, senergy, sflux*100., psym=10, color=cc[0]
;oplot, [3,6],[.033,.033], thick=4, color=cc[0] & arrow, 4.243, .033, 4.243, .005, /data, thick=4, hsize=-0.5, color=cc[0]
oplot, [3,6],[.033,.033], thick=4, color=cc[0] & arrow, 3.873, .033, 3.873, .005, /data, thick=4, hsize=-0.5, color=cc[0]

xyouts, /norm, .15, .45, 'X123 (23 June 2012)'
;xyouts, xx, yy/my, 'SphinX * 100', color=cc[0]
xyouts, /norm, .15, .40, 'SphinX (2009; Sylwester et al. 2012)', color=cc[3]
xyouts, /norm, .15, .35, 'RHESSI (2005-2009; Hannah et al. 2010)', color=cc[0]

write_jpeg_tv, 'x123_vs_sphinx.jpg'

; --- Begin Amir stuff ---
restore,'/Users/cepheid/Dropbox/bleh/jstone_files/idl/response_array.sav',/verb
restore,'sxr_3spectra_same-flux.sav',/verb
restore,'/Users/cepheid/Dropbox/bleh/jstone_files/see_hires_v10_2012175.sav',/verb
restore,'ch_ss_sp_dem_all.sav',/verb
i = where(xpswave le 7)
fluxnorm = total(xpsflux[i]*.1)
i = where(chwave le 70)
factors = fltarr(4)
for j=0,3 do factors[j] = fluxnorm / total(chsp[j,i])

; Get chianti bin energies from wavelengths
chen = a2kev / chwave
; Get 2xN Chianti wavelength array
chwave2xN = [chwave, chwave[-1]+mean(get_edges(chwave,/wid))] - mean(get_edges(chwave,/wid))/2.
chwave2xN = get_edges(chwave2xN,/edges_2)
; Get 2xN Chianti energy array
chen2xN = a2keV/chwave2xN
; Convert all Chianti spectra from photons/sec/sr/cm^2/Ang into photons/sec/cm^2/keV
for i=0,3 do chsp[i,*] *= 5.98e-5 * get_edges(chwave2xN,/wid) / get_edges(chen2xN,/wid)
; Get 2xN X123 energy array
energy2xN = [energy,energy[-1]+mean(get_edges(energy,/wid))]-mean(get_edges(energy,/wid))/2.
energy2xN = get_edges(energy2xN,/edges_2)

;ssw_rebinner,reform(chsp[0,*])*get_edges(chen2xN,/wid),chen2xN,qsrebin,energy2xN,/dbl & qsrebin /= get_edges(energy2xN,/wid)
qsrebin = dblarr(n_elements(energy)) & arrebin = qsrebin & chrebin = qsrebin & flrebin = qsrebin
; Rebin all Chianti spectra from original fine binning to X123 binning
; Could NOT use ssw_rebinner for some reason... it gave very weird results.  This manual rebinning works OK since original binning is very fine.
for i=0,255 do qsrebin[i] = total((reform(chsp[0,*])*get_edges(chen2xN,/wid))[where(chen ge energy2xN[0,i] and chen le energy2xN[1,i])])
for i=0,255 do arrebin[i] = total((reform(chsp[1,*])*get_edges(chen2xN,/wid))[where(chen ge energy2xN[0,i] and chen le energy2xN[1,i])])
for i=0,255 do chrebin[i] = total((reform(chsp[2,*])*get_edges(chen2xN,/wid))[where(chen ge energy2xN[0,i] and chen le energy2xN[1,i])])
for i=0,255 do flrebin[i] = total((reform(chsp[3,*])*get_edges(chen2xN,/wid))[where(chen ge energy2xN[0,i] and chen le energy2xN[1,i])])

; Get XPS bin energies from wavelengths
xpsen = (a2kev/10.)/xpswave
; Get 2xN XPS wavelength array
xpswave2xN = [xpswave, xpswave[-1]+mean(get_edges(xpswave,/wid))] - mean(get_edges(xpswave,/wid))/2.
xpswave2xN = get_edges(xpswave2xN,/edges_2)
; Get 2xN XPS energy array
xpsen2xN = (a2kev/10.)/xpswave2xN
; Convert XPS flux from W/m^2/nm to photons/sec/cm^2/keV
xpsflux_en = xpsflux * 0.1 * 1e-4  ; multiply by 0.1 nm bin widths, and convert m^-2 to cm^-2
xpsflux_en /= xpsen * 1.602d-16    ; convert W = J/s to photons/s
xpsflux_en /= get_edges(xpsen2xN,/wid)   ; divide out bin width in keV


plot, xpsen, xpsflux, psym=10, xr=[0.535, 5], xs=9, /xlog, yr=[1e2, 1e9], /ys, /ylog, xtitle='Energy [keV]', ytitle='Flux [photons s!U-1!N cm!U-2!N keV!U-1!N]', /nodata
axis, /xax, /xs, xr=(a2kev/10.)/(10^!x.crange), /xlog, xtitle='Wavelength [nm]',xticks=2,xtickv=[10,1],xminor=9
;oplot, energy, qsrebin*factors[0], psym=10, color=cc[0]
;oplot, energy, arrebin*factors[1], psym=10, color=cc[1]
;oplot, energy, chrebin*factors[2], psym=10, color=cc[4]
;oplot, energy, flrebin*factors[3], psym=10, color=cc[3]
oplot, energy, qsrebin*xps1.xps_qs, psym=10, color='009900'x
oplot, energy, arrebin*xps1.xps_ar, psym=10, color=cc[1]
oplot, energy, chrebin*xps1.xps_qs, psym=10, color=cc[4]
oplot, energy, flrebin*xps1.xps_flare/100, psym=10, color=cc[0]
oplot, xpsen, xpsflux_en, psym=10
xyouts, /norm, .5, .85, 'CHIANTI Quiet Sun', color='009900'x
xyouts, /norm, .5, .82, 'CHIANTI Coronal Hole', color=cc[4]
xyouts, /norm, .5, .79, 'CHIANTI Active Region', color=cc[1]
xyouts, /norm, .5, .76, 'CHIANTI Flare', color=cc[0]
xyouts, /norm, .5, .71, 'XPS L4 = QS + CH + (7.8e-3)AR + (3.0e-5)FL'

write_jpeg_tv, 'xpsL4_chianti.jpg'

plot, energy, pflux, psym=10, xr=[0.535,5], xs=9,/xlog, yr=[1E1,1E9], ys=1, /ylog, $
  xtitle='Energy [keV]', ytitle='Flux [photons s!U-1!N cm!U-2!N keV!U-1!N]', /nodata
axis, /xax, /xs, xr=(a2kev/10.)/(10^!x.crange), /xlog, xtitle='Wavelength [nm]',xticks=2,xtickv=[10,1],xminor=9
oplot, energy, qsrebin + arrebin*0.3, psym=10, color=cc[5],thick=1
oplot, xpsen, xpsflux_en, psym=10, color=cc[0]
oplot, energy, qsrebin + arrebin*0.03, psym=10, color=cc[3],thick=1
oplot, energy, pflux, psym=10

xyouts, /norm, .15, .40, 'X123 Observation'
;xyouts, xx, yy/my, 'SphinX * 100', color=cc[0]
xyouts, /norm, .15, .35, 'Model: XPS L4 = QS + CH + (7.8e-3)AR + (3.0e-7)FL', color=cc[0]
xyouts, /norm, .15, .20, 'X123 Eyeball fit -- QS + (0.03)AR', color=cc[3]
xyouts, /norm, .15, .25, 'XPS Eyeball fit -- QS + (0.3)AR', color=cc[5]

write_jpeg_tv, 'x123_vs_xps.jpg'


!x.margin = [8,2]
spec = mean(pspectrum[time_inds].spectrum,dim=2)/2.
err = (stddev(pspectrum[time_inds].spectrum,dim=2)/2.)/sqrt(n_elements(time_inds))
plot,spec,xr=[3,40],/xs, /xlog, psym=10, xtitle='Channel #', ytitle='Intensity [cts s!U-1!N]', title='Average count rate around apogee (+/- 90 sec)',/ylog,yr=[1e-2,400],/ys
errplot,spec+err,spec-err,width=1e-20,color=cc[0]
write_jpeg_tv, 'x123_counts.jpg'

!x.margin = [6,6]
plot,energy,psym=10,xr=[3,100],/xs,/xlog, xtitle='Channel #',ytitle='Energy [keV]',/ylog,ys=9,yr=[.1,10], title='Gain conversion'
axis, /yax, /ys, yr=(a2kev/10.)/(10^!y.crange), /ylog, ytitle='Wavelength [nm]',yticks=3,ytickv=[10,1,.1],yminor=9
xyouts, .15, .80, /norm, 'Gain: E (kev) = 0.116634 * Channel - 0.168317'
xyouts, .55, .50, /norm, 'Wavelength (nm) = 1.23984 / E(keV)'
write_jpeg_tv, 'x123_gain.jpg'

!x.margin = [8,2]
plot, response_array[0,*], response_array[1,*], psym=10, xr=[0.535, 30], xs=9, /xlog, yr=[1e-3, 1.1], /ys, /ylog, xtitle = 'Energy [keV]', ytitle = 'Response [cts/ph]'
axis, /xax, /xs, xr=(a2kev/10.)/(10^!x.crange), /xlog, xtitle='Wavelength [nm]',xticks=2,xtickv=[10,1],xminor=9
xyouts, 0.75, 0.75, /norm, 'X123-SDD!CPhotopeak only!C!CThicknesses:!CSi = 500 um!CBe = 8 um'
write_jpeg_tv, 'x123_response.jpg'


end
