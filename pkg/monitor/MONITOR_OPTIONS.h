C $Header: /u/gcmpack/MITgcm/pkg/monitor/MONITOR_OPTIONS.h,v 1.2 2003/10/09 04:19:20 edhill Exp $
C $Name:  $

C CPP options file for monitor package
C
C Use this file for selecting options within the monitor package

#ifndef MONITOR_OPTIONS_H
#define MONITOR_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_MONITOR

#include "CPP_OPTIONS.h"

C Disable use of hFacZ
#undef MONITOR_TEST_HFACZ

#endif /* ALLOW_MONITOR */
#endif /* MONITOR_OPTIONS_H */
