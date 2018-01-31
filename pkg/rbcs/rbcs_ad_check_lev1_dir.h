CADJ STORE rbct0 = comlev1, key = ikey_dynamics
CADJ STORE rbct1 = comlev1, key = ikey_dynamics
CADJ STORE rbcs0 = comlev1, key = ikey_dynamics
CADJ STORE rbcs1 = comlev1, key = ikey_dynamics
CADJ STORE rbcsldrec = comlev1, key = ikey_dynamics

#ifndef DISABLE_RBCS_MOM
CADJ STORE rbcu0 = comlev1, key = ikey_dynamics
CADJ STORE rbcu1 = comlev1, key = ikey_dynamics
CADJ STORE rbcv0 = comlev1, key = ikey_dynamics
CADJ STORE rbcv1 = comlev1, key = ikey_dynamics
#endif

#ifdef ALLOW_PTRACERS
CADJ STORE rbcptr0 = comlev1, key = ikey_dynamics
CADJ STORE rbcptr1 = comlev1, key = ikey_dynamics
#endif
