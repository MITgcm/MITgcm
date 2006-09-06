C $Header: /u/gcmpack/MITgcm/pkg/atm2d/ATM2D_OPTIONS.h,v 1.1 2006/09/06 15:32:40 jscott Exp $
C $Name:  $

#ifndef ATM2D_OPTIONS_H
#define ATM2D_OPTIONS_H
#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_ATM2D

#include "CPP_OPTIONS.h"

C- allow single grid-point debugging write to standard-output
#define ALLOW_DBUG_ATM2D

C turn on MPI or not
#undef ATM2D_MPI_ON

C CPP Macros go here


#define JBUGI 89
#define JBUGJ 43


#endif /* ALLOW_ATM2D */
#endif /* ATM2D_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
