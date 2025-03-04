#ifndef MDSIO_OPTIONS_H
#define MDSIO_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MDSIO_OPTIONS.h
C !INTERFACE:
C #include "MDSIO_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "mdsio":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifdef ALLOW_MDSIO
C     Package-specific Options & Macros go here

C-  Defining SAFE_IO stops the model from overwriting its own files
#undef  SAFE_IO

#ifdef SAFE_IO
#define _NEW_STATUS 'new'
#else
#define _NEW_STATUS 'unknown'
#endif

#ifdef ALLOW_AUTODIFF
#define _OLD_STATUS 'unknown'
#else
#define _OLD_STATUS 'old'
#endif

C-  I/O that includes tile halos in the files
#ifdef ALLOW_AUTODIFF
C   Only used when pkg/autodiff is compiled:
# define ALLOW_WHIO
# define ALLOW_WHIO_3D
# undef EXCLUDE_WHIO_GLOBUFF_2D
# undef INCLUDE_WHIO_GLOBUFF_3D
#endif /* ALLOW_AUTODIFF */

#endif /* ALLOW_MDSIO */
#endif /* MDSIO_OPTIONS_H */
