C $Header: /u/gcmpack/MITgcm/pkg/mdsio/MDSIO_OPTIONS.h,v 1.2 2001/09/27 18:07:56 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
