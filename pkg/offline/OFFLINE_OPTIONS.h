C $Header: /u/gcmpack/MITgcm/pkg/offline/OFFLINE_OPTIONS.h,v 1.2 2007/10/09 00:13:15 jmc Exp $
C $Name:  $

#ifndef OFFLINE_OPTIONS_H
#define OFFLINE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_OFFLINE

#include "CPP_OPTIONS.h"

CBOP
C    !ROUTINE: OFFLINE_OPTIONS.h
C    !INTERFACE:

C    !DESCRIPTION:
c options for offline package
CEOP

#undef NOT_MODEL_FILES

#endif /* ALLOW_OFFLINE */
#endif /* OFFLINE_OPTIONS_H */
