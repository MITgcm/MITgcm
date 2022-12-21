CBOP
C !ROUTINE: MYPACKAGE_OPTIONS.h
C !INTERFACE:
C #include "MYPACKAGE_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for pkg "mypackage":
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef MYPACKAGE_OPTIONS_H
#define MYPACKAGE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MYPACKAGE
C Place CPP define/undef flag here

C to reduce memory storage, disable unused array with those CPP flags :
#define MYPACKAGE_3D_STATE
#define MYPACKAGE_2D_STATE
#define MYPACKAGE_TENDENCY

#undef MYPA_SPECIAL_COMPILE_OPTION1

#define MYPA_SPECIAL_COMPILE_OPTION2

#endif /* ALLOW_MYPACKAGE */
#endif /* MYPACKAGE_OPTIONS_H */
