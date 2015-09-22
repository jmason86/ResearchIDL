; Temporary program to avoid running through GenerateCoronalDimmingParameters.pro user interaction for 245 events

PRO AddCMEVelocitiesToCoronalDimmingFlareArray

saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Computed On16-Oct-2012 17:45:48/'

restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Computed On16-Oct-2012 17:45:48/CoronalDimmingParametersComputedOn_16-Oct-2012 18:54:44.sav'
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/Coronal Dimming/merged_flare_catalog.sav'

origflareIDs = coronaldimmingflarearray.flareid
flareCatalogFlareIDs = Flare_Catalog.flare_id

allRelevantVelocities = fltarr(n_elements(origFlareIDs))
STOP
FOR i = 0, n_elements(origFlareIDs) - 1 DO BEGIN
  indexOfFlareID = where(flareCatalogFlareIDs EQ origflareids[i]) 
  velocity = flare_catalog[indexOfFlareID].CME.CME_LASCO_CDAW.VELOCITY
  IF velocity LE 0 THEN velocity = !VALUES.F_NAN
  allRelevantVelocities[i] = velocity
  
ENDFOR


struct_add_field, coronalDimmingFlareArray, 'CMEVelocity', allRelevantVelocities
STOP
save, coronalDimmingFlareArray, FILENAME = saveloc + 'CoronalDimmingParametersComputedOn_' + systim() + '.sav'




END