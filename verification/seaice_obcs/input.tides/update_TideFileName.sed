#- to update "data.obcs" content following code update from PR #752:
#  > mv data.obcs data.obcs.bak
#  > sed -f update_TideFileName.sed data.obcs.bak > data.obcs
#- Warning: This is case sensitive while data.obcs namelist are not !
s/\<tidalPeriod\> *=/OBCS_tidalPeriod=/g
s/OBNamFile/OBN_vTidAmFile/g
s/OBNphFile/OBN_vTidPhFile/g
s/OBSamFile/OBS_vTidAmFile/g
s/OBSphFile/OBS_vTidPhFile/g
s/OBEamFile/OBE_uTidAmFile/g
s/OBEphFile/OBE_uTidPhFile/g
s/OBWamFile/OBW_uTidAmFile/g
s/OBWphFile/OBW_uTidPhFile/g
