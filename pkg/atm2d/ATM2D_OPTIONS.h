C $Header: /u/gcmpack/MITgcm/pkg/atm2d/ATM2D_OPTIONS.h,v 1.2 2011/12/24 01:04:45 jmc Exp $
C $Name:  $

#ifndef ATM2D_OPTIONS_H
#define ATM2D_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_ATM2D
C     Package-specific Options & Macros go here

C- allow single grid-point debugging write to standard-output
#define ALLOW_DBUG_ATM2D

C turn on MPI or not
#undef ATM2D_MPI_ON

#define JBUGI 89
#define JBUGJ 43

#endif /* ALLOW_ATM2D */
#endif /* ATM2D_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
