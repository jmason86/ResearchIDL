;+
; NAME: 
;   make_aia
;   
; PURPOSE:
;   Bring AIA data from Level 1 -> Level 1.5, and also correct for exposure time
;   
; INPUTS: 
;   infile: [string array?] list of one or more sdo FITS files, jsoc/rice compressed or not
;   
; OPTIONAL INPUTS:
;   None.
;
; KEYWORD PARAMETERS:
;   psfdo:  If = 1, perform point spread function deconvolution.  Note: requires all of the images in infile to be
;           the same wavelength, and process takes a long time so only use when necesary
;   savefits:  If = 1, writes a fits file of same name as infile with extension "proc" appended.
;   
; OUTPUTS:
;   ohdr: [?] New header file with Level 1.5 information updated
;   oimg: [?] Returned image file
;   
; OPTIONAL OUTPUTS: 
;   
;   
; RESTRICTIONS:
;   
;   
; EXAMPLE: 
;   
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     Barbara Thompson
;-

PRO make_aia, infile, ohdr, oimg, psfdo=psfdo, savefits=savefits
; Note! ohdr is original header if there is more than one image.  Need to fix with replicate.

IF NOT keyword_set(psfdo) THEN psfdo=0
IF NOT keyword_set(savefits) THEN savefits=0

;  Set up output array.  Doing read_sdo then aia_prep causes two arrays of size (4096,4096,n) to be created, which can burden your system memory
szz=n_elements(infile)
IF (szz eq 1) THEN oimg=dblarr(4096,4096) else oimg=dblarr(4096,4096,szz)

; if you don't want PSF deconvolution (which takes a loong time)
IF NOT psfdo THEN BEGIN
  IF (szz gt 1) THEN BEGIN
     aia_prep, infile(0), -1, hdr, img,/uncomp_delete
     ohdr=replicate(hdr, szz)
     for ii = 0, szz-1 do BEGIN 
       aia_prep, infile(ii), -1, hdr, img,/uncomp_delete
       ohdr(ii)=hdr
       img=double(img)/double(hdr.exptime) ; Correct for exposure time
       oimg(*,*,ii)=img
       oimg(0,0,*) = 0
       oimg(0,1,*) = 10000
       IF savefits THEN BEGIN
          posfits=strpos(infile(ii), 'fits')
          writefits, strmid(infile(ii), 0, posfits-1)+'_proc.fits', oimg, struct2fitshead(hdr)
       END ; savefits loop
     END ; for loop
  END ; szz loop

  IF (szz eq 1) THEN BEGIN
     aia_prep, infile, -1, ohdr, oimg,/uncomp_delete
     oimg=double(oimg)/double(ohdr.exptime)
     oimg(0,0) = 0
     oimg(0,1) = 10000
     IF savefits THEN BEGIN
        posfits=strpos(infile, 'fits')
        writefits, strmid(infile, 0, posfits-1)+'_proc.fits', oimg, struct2fitshead(hdr)
     END ; savefits loop
  END
END ; psfdo=0 loop

; if you do want PSF deconvolution - make sure all images are the same wavelength or modify code.
IF psfdo THEN BEGIN
  IF (szz gt 1) THEN BEGIN
     aia_prep, infile(0), -1, hdr, img,/uncomp_delete
     ohdr=replicate(hdr, szz)
     ;psf = aia_calc_psf(ohdr(0).wavelnth)
     okay=execute("restore, '~/Documents/data/AIA/PSF/aiapsf_'+strtrim(ohdr(0).wavelnth, 2)+'.save'")
     for ii = 0, szz-1 do BEGIN
       print, 'Processing image # '+strtrim(ii,2)
       read_sdo, infile(ii), hdr, img, /uncomp_delete 
       img1=aia_deconvolve_richardsonlucy(double(img), psf)
       aia_prep, hdr, img1, hdr2, img2, /uncomp_delete
       ohdr(ii)=hdr2
       img=double(img)/double(hdr.exptime)
       oimg(*,*,ii)=img
       oimg(0,0,*) = 0
       oimg(0,1,*) = 10000
       IF savefits THEN BEGIN
          posfits=strpos(infile(ii), 'fits')
          writefits, strmid(infile(ii), 0, posfits-1)+'_proc.fits', oimg, struct2fitshead(hdr2)
       END ; savefits loop
     END ; for loop
  END ; szz loop

  IF (szz eq 1) THEN BEGIN
     read_sdo, infile, hdr, img,/uncomp_delete
     ;psf = aia_calc_psf(hdr.wavelnth)
     okay=execute("restore, '~/Documents/data/AIA/PSF/aiapsf_'+strtrim(hdr.wavelnth, 2)+'.save'")
     img1=aia_deconvolve_richardsonlucy(double(img), psf)
     aia_prep, hdr, img1, ohdr, img2, /uncomp_delete
     oimg=double(oimg)/double(ohdr.exptime)
     oimg(0,0) = 0
     oimg(0,1) = 10000
     IF savefits THEN BEGIN
        posfits=strpos(infile, 'fits')
        writefits, strmid(infile, 0, posfits-1)+'_proc.fits', oimg, struct2fitshead(ohdr)
     END ; savefits loop
  END ; szz loop
END ; psfdo = 1 loop 
END 

