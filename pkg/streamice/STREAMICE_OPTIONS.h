C $Header: /u/gcmpack/MITgcm/pkg/streamice/STREAMICE_OPTIONS.h,v 1.1 2013/06/12 21:30:22 dgoldberg Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C CPP options file for MYPACKAGE
C
C Use this file for selecting options within package "streamice"

#ifndef STREAMICE_OPTIONS_H
#define STREAMICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_STREAMICE

#include "CPP_OPTIONS.h"

C Place CPP define/undef flag here

#define STREAMICE_CONSTRUCT_MATRIX
#undef STREAMICE_HYBRID_STRESS
#undef USE_ALT_RLOW
#undef STREAMICE_GEOM_FILE_SETUP
#undef STREAMICE_SMOOTH_FLOATATION

#endif /* ALLOW_MYPACKAGE */
#endif /* MYPACKAGE_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
