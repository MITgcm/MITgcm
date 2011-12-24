C $Header: /u/gcmpack/MITgcm/pkg/mdsio/MDSIO_OPTIONS.h,v 1.10 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

#ifndef MDSIO_OPTIONS_H
#define MDSIO_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MDSIO
C     Package-specific Options & Macros go here

C Defining SAFE_IO stops the model from overwriting its own files
#undef  SAFE_IO

#ifdef SAFE_IO
#define _NEW_STATUS 'new'
#else
#define _NEW_STATUS 'unknown'
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#define ALLOW_BROKEN_MDSIO_GL
#define _OLD_STATUS 'unknown'
#else
#undef  ALLOW_BROKEN_MDSIO_GL
#define _OLD_STATUS 'old'
#endif

C I/O that includes tile halos in the files
#undef ALLOW_WHIO
#ifdef ALLOW_AUTODIFF_TAMC
# define ALLOW_WHIO
# define ALLOW_WHIO_3D
#endif

#endif /* ALLOW_MDSIO */
#endif /* MDSIO_OPTIONS_H */
