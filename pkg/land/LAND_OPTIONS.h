C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_OPTIONS.h,v 1.1 2003/06/12 17:54:22 jmc Exp $
C $Name:  $

C  CPP options file for Land package 
C
#ifndef _CPP_EEOPTIONS_H_
#include "CPP_OPTIONS.h"
#endif

#ifdef ALLOW_LAND

C  allow time average diagnostic:
#define ALLOW_LAND_TAVE

#endif /* ALLOW_LAND */
