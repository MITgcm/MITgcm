C $Header: /u/gcmpack/MITgcm/pkg/cal/CAL_OPTIONS.h,v 1.3 2003/11/25 16:07:03 heimbach Exp $
C $Name:  $

#ifndef CAL_OPTIONS_H
#define CAL_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_CAL

#include "CPP_OPTIONS.h"

CPH >>>>>> ALL CAL OPTIONS ARE CURRENTLY SET IN <<<<<<
CPH >>>>>> ECCO_CPPOPTIONS.h                    <<<<<<
CPH >>>>>> SHOULD REMAIN LIKE THIS UNTIL WE     <<<<<<
CPH >>>>>> DEFINE A CONSISTENT CHANGE OF POLICY <<<<<<

C This package is currently controlled by the ECCO_CPPOPTIONS.h and
C to avoid compatibility issues you should *NOT* customize this file.
cph#define ALLOW_CALENDAR

C This CPP flag must be set for no apparent reason other than to stop
C cal_readparms() for causing to issue a fatal warning that it is
C undefined!!!
C TODO: delete this and related code!? AJA
cph#define ALLOW_CAL_NENDITER

#endif /* ALLOW_CAL */
#endif /* CAL_OPTIONS_H */
