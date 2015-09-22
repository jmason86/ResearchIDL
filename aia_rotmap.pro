function aia_rotmap, aiamap, rot_time, keeplimb=keeplimb, outhdr=outhdr, outname=outname
; outsave should be the original header if you want a fits file.  Need to set outname
; keep limb keeps the original off limb values.  Default is keep.
var=n_elements(keeplimb)
if (var eq 0) then keeplimb=1
var=n_elements(outhdr)
if (var eq 0) then saveit=0 else saveit=1
var=n_elements(outname)
if (var eq 0) then outname='dummy.fits'
aiamap2=aiamap
szz=size(aiamap)
for ii = 0, szz(3)-1 do begin
gmap=drot_map(aiamap(ii), time=rot_time, missing=-1)
  if (keeplimb) then begin
     dist_circle, circle, 4096
     limbarr=where(circle ge 1.01*(gmap.rsun/gmap.dx))
     gmap.data(limbarr)=aiamap(ii).data(limbarr)
  end 
  if (saveit) then begin
     hdr=replicate(create_struct(outhdr,'rottime','str'),n_elements(outhdr))
     hdr.rottime=rot_time
     posfits=strpos(outname, 'fits')
     writefits, strmid(outname, 0, posfits-1)+'_rot.fits', gmap.data, struct2fitshead(hdr)
  end
aiamap2(ii).data=gmap.data
end
return, aiamap2
end
