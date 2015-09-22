a2keV = 12.39842  ; energy = a2keV / wavelength 

restore,'ch_ss_sp_dem_all.sav',/verb
chen = a2kev / chwave

chwave2xN = [chwave, chwave[-1]+mean(get_edges(chwave,/wid))] - mean(get_edges(chwave,/wid))/2.
chwave2xN = get_edges(chwave2xN,/edges_2)
chen2xN = a2keV/chwave2xN
for i=0,3 do chsp[i,*] *= 5.98e-5 * get_edges(chwave2xN,/wid) / get_edges(chen2xN,/wid)

plaw = dblarr(20000)
i = where(chen ge 20, comp = j)
plaw[i] = 100 * (chen[i]/20.)^(-4.)
plaw[j] = 100 * (chen[j]/20.)^(-1.5)

spec = chsp[3,*]/500. + plaw

setplot
;!p.charsize = 2 & !p.thick = 2
!x.thick = 1 & !y.thick = 1
cc = rainbow(7)
!x.margin = [6.5,1]
!y.margin = [3.5,2.5]

plot, chen, spec, xr=[0.05,400], /xlog, xs=9, yr=[1e-4,1e16], /ys, /ylog, xtitle='Energy [keV]', ytitle='Flux [photons s!U-1!N cm!U-2!N keV!U-1!N]', /nodata
axis, /xax, /xs, xr=(a2kev)/(10^!x.crange), /xlog, xtitle='Wavelength [Ang]',xticks=5,xtickv=[100,10,1,0.1,0.01],xminor=9
oplot, chen, spec
xyouts, 0.1, 1e2, /data, 'Example flare spectrum', charsize = 3
arrow, (a2kev/1050.) > 10^min(!x.crange), 2e14, (a2keV/50.) < 10^max(!x.crange), 2e14, /data, hsize=30, thick=5, hthick=5, /solid, color = cc[3]
xyouts, 0.08, 5e14, /data, 'EVE', color = cc[3], charsize = 3
arrow, 17000. < 10^max(!x.crange), 2e14, 3. > 10^min(!x.crange), 2e14, /data, hsize=30, thick=5, hthick=5, /solid, color = cc[0]
xyouts, 30., 5e14, /data, 'RHESSI', color = cc[0], charsize = 3
arrow, [1, 10], [2e13, 2e13], [30, 0.5], [2e13, 2e13], /data, hsize=30, thick=5, hthick=5, /solid, color = cc[4]
xyouts, 2.2, 2e12, 'MinXSS', color = cc[4], charsize = 3

; Convert from per-keV to per-A
spec2 = spec * get_edges(chen2xN,/wid) / get_edges(chwave2xN,/wid)

plot, chwave, spec2, xr=a2kev/reverse([0.05,400]), /xlog, xs=9, yr=[1e0,2e12], /ys, /ylog, xtitle='Wavelength [Ang]', ytitle='Flux [photons s!U-1!N cm!U-2!N Ang!U-1!N]', /nodata
axis, /xax, /xs, xr=(a2kev)/(10^!x.crange), /xlog, xtitle='Energy [keV]',xticks=5,xtickv=[100,10,1,0.1,0.01],xminor=9
oplot, chwave, spec2
xyouts, 10, 1e2, /data, 'Example flare spectrum', charsize = 3
arrow, 1050. < 10^max(!x.crange), 2e11, 50. > 10^min(!x.crange), 2e11, /data, hsize=30, thick=5, hthick=5, /solid, color = cc[3]
xyouts, 100., 3e11, /data, 'EVE', color = cc[3], charsize = 3
arrow, (a2keV/17000.) > 10^min(!x.crange), 2e11, (a2keV/3.) < 10^max(!x.crange), 2e11, /data, hsize=30, thick=5, hthick=5, /solid, color = cc[0]
xyouts, 0.2, 3e11, /data, 'RHESSI', color = cc[0], charsize = 3
arrow, a2kev/[1, 10], [5e10, 5e10], a2kev/[30, 0.5], [5e10, 5e10], /data, hsize=30, thick=5, hthick=5, /solid, color = cc[4]
xyouts, 2, 1.2e10, 'MinXSS', color = cc[4], charsize = 3
