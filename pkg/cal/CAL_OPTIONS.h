C $Header: /u/gcmpack/MITgcm/pkg/cal/CAL_OPTIONS.h,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#ifndef CAL_OPTIONS_H
#define CAL_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_CAL

#include "CPP_OPTIONS.h"

C This package is currently controlled by the ECCO_CPPOPTIONS.h and
C to avoid compatibility issues you should *NOT* customize this file.
#define ALLOW_CALENDAR

C This CPP flag must be set for no apparent reason other than to stop
C cal_readparms() for causing to issue a fatal warning that it is
C undefined!!!
C TODO: delete this and related code!? AJA
#define ALLOW_CAL_NENDITER

#define ALLOW_ECCO
#include "ECCO_CPPOPTIONS.h"

#endif /* ALLOW_CAL */
#endif /* CAL_OPTIONS_H */
