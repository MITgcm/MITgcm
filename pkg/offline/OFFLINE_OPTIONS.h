C $Header: /u/gcmpack/MITgcm/pkg/offline/OFFLINE_OPTIONS.h,v 1.3 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

#ifndef OFFLINE_OPTIONS_H
#define OFFLINE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OFFLINE

CBOP
C    !ROUTINE: OFFLINE_OPTIONS.h
C    !INTERFACE:

C    !DESCRIPTION:
c options for offline package
CEOP

#undef NOT_MODEL_FILES

#endif /* ALLOW_OFFLINE */
#endif /* OFFLINE_OPTIONS_H */
