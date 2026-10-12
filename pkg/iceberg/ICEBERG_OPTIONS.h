C     *==========================================================*
C     | ICEBERG_OPTIONS.h
C     | o CPP options file for ICEBERG package.
C     *==========================================================*
C     | Use this file for selecting options within the ICEBERG
C     | package.
C     *==========================================================*

#ifndef ICEBERG_OPTIONS_H
#define ICEBERG_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_ICEBERG
C     Package-specific Options & Macros go here

C     Allow per berg diagnostics via text files, increases runtime significantly
#undef ALLOW_PER_BERG_DIAG

#endif /* ALLOW_ICEBERG */
#endif /* ICEBERG_OPTIONS_H */
