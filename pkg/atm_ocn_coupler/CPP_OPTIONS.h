#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

CBOP
C !ROUTINE: CPP_OPTIONS.h
C !INTERFACE:
C #include "CPP_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | main CPP options file for the model:
C | Control which optional features to compile in model/src code.
C *==================================================================*
CEOP

C CPP flags controlling particular source code features

C-- Other option files:

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

C-  Place where multi-pkg header file ECCO_CPPOPTIONS.h used to be included

#endif /* CPP_OPTIONS_H */
