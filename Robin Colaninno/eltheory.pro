pro ELTHEORY,Rin,T,R,B,Bt,Br,Pol,LIMB=limb,CENTER=center
;+
; $Id: eltheory.pro,v 1.1 2008/02/07 22:22:57 nathan Exp $
; NAME:
;	ELTHEORY
;
; PURPOSE:
;	This procedure computes various properties of the Thomson scattered
;	light by an electron in the solar corona.
;
; CATEGORY:
;	CME
;
; CALLING SEQUENCE:
;	ELTHEORY, Rin, T, R, B, Bt, Br, Pol
;
; INPUTS:
;	Rin:	Impact Distance (in solar radii)
;	T:	Angle from plane of sky (in degrees)
;
; OUTPUTS:
;	R:	Distance of electron from sun center (in solar radii)
;	B:	Total Brightness for one electron at R,T
;	Bt:	Tangential Brightness for one electron at R,T
;	Br:	Radial Brightness for one electron at R,T
;	Pol:	Polarization for one electron at R,T
;
; KEYWORDS:
;	LIMB:	The limb darkening coefficient. If not set then use
;		0.63 (550 nm).
;	CENTER:	If set, then the output is returned as the central solar
;		disk brightness.  THe default is mean solar brightness.
;
; COMMON BLOCKS:
;	None
;
; SIDE EFFECTS:
;	None
;
; RESTRICTIONS:
;	None
;
; PROCEDURE:
;	This procedure uses the Thomson electron scattering equations 
;	to compute the total brightness, the radial and tangential 
;	components and the polarization associated with one electron.
;	Limb darkening of 0.63 is included.
;	The results are returned in units of mean solar brightness.
;	
;
; MODIFICATION HISTORY:
;	Created IDL version 16 May 1996, RA Howard, NRL
; $Log: eltheory.pro,v $
; Revision 1.1  2008/02/07 22:22:57  nathan
; moved from lasco/data_anal
;
;	RAH 11/25/98	Added common block for VDH coefficients
;	RAH 09/01/99	Added keyword for limb darkening
;	RAH 10/02/99	Changed output to be mean solar brightness
;	RAH 10/27/99	Added keyword for central/mean solar brightness
;
;	@(#)eltheory.pro	1.5 11/29/05 LASCO IDL LIBRARY
;-
;
COMMON COM_ELTHEORY,ael,bel,cel,del
;
;
;  const = sigma * pi / 2
;  where sigma is the scattering cross section (7.95e-26 per steradian)
;
const = 1.24878D-25
IF KEYWORD_SET(limb)  THEN u=limb ELSE u = 0.63		; Limb darkening
u = DOUBLE(u)
IF NOT KEYWORD_SET(center)  THEN const = const/(1-u/3)	; convert to mean solar brightness
Theta = double(t);<89.99d00	; ensure less than 90 degrees from POS
R = Rin/COS(Theta/!radeg)
sinchi2 = (double(Rin)/R)^2	; angle between sun center, electron, observer
s = (1.d00/R)<0.9999999d00	; sin(omega)
s2 = s*s
c2 = (1.-s2)>0		
c = SQRT (c2)			; cos(omega)
g = c2*(ALOG((1.+s)/c))/s
;
;  Compute Van de Hulst Coefficients
;  Expressions are given in Billings (1968) after Minnaert (1930)
;
ael = c*s2
cel = (4.-c*(3.+c2))/3.
bel = -(1.-3.*s2-G*(1.+3.*s2))/8.
del = (5.+s2-G*(5.-s2))/8.
;
;  Compute electron brightness
;  pB is polarized brightness (Bt-Br)
;
Bt = const*( cel + u*(del-cel) )
pB = const* sinchi2 *( (ael + u*(bel-ael) ) )
Br = Bt-pB
B = Bt+Br
Pol = pB/B
RETURN
END
