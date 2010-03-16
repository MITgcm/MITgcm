C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_OPTIONS.h,v 1.14 2010/03/16 00:14:47 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: GAD_OPTIONS.h

C !INTERFACE:
C #include "GAD_OPTIONS.h"

C !DESCRIPTION:
C Contains CPP macros/flags for controlling optional features of package.
CEOP

C CPP options file for GAD (Generic Advection Diffusion) package
C
C Use this file for selecting options within the GAD package

#ifndef GAD_OPTIONS_H
#define GAD_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_GENERIC_ADVDIFF

#include "CPP_OPTIONS.h"

C This flag selects the form of COSINE(lat) scaling of bi-harmonic term.
C *only for use on a lat-lon grid*
C Setting this flag here only affects the bi-harmonic tracer terms; to
C use COSINEMETH_III in the momentum equations set it CPP_OPTIONS.h
#define COSINEMETH_III

C This selects isotropic scaling of harmonic and bi-harmonic term when
C using the COSINE(lat) scaling.
C Setting this flag here only affects the tracer diffusion terms; to
C use ISOTROPIC_COS_SCALING of the horizontal viscosity terms in the
C momentum equations set it CPP_OPTIONS.h; the following line
C even overrides setting the flag in CPP_OPTIONS.h
#undef ISOTROPIC_COS_SCALING

C As of checkpoint41, the inclusion of multi-dimensional advection
C introduces excessive recomputation/storage for the adjoint.
C We can disable it here using CPP because run-time flags are insufficient.
#undef DISABLE_MULTIDIM_ADVECTION

C This enable the use of 2nd-Order Moment advection scheme (Prather, 1986) for
C Temperature and Salinity ; due to large memory space (10 times more / tracer)
C requirement, by default, this part of the code is not compiled.
#undef GAD_ALLOW_TS_SOM_ADV

C Hack to get rid of negatives caused by Redi.  Works by restricting the
C outgoing flux (only contributions computed in gad_calc_rhs) for each cell
C to be no more than the amount of tracer in the cell (see Smolarkiewicz
C MWR 1989 and Bott MWR 1989).
C The flux contributions computed in gad_calc_rhs which are affected by
C this hack are:
C - explicit diffusion, Redi and the non-local part of KPP
C - advection is affected only if multiDimAdvection=.FALSE.
C - vertical diffusion (including the diagonal contribution from GMRedi)
C   only if implicitDiffusion=.FALSE.
C - GM is affected only if GMREDI_AdvForm=.FALSE.
C
C The parameter SmolarkiewiczMaxFrac (defined in gad_init_fixed.F)
C specifies the maximal fraction of tracer that can leave a cell.
C By default it is 1.  This will prevent the tracer from going negative
C due to contributions from gad_calc_rhs alone.  In the presence of other
C contributions (or roundoff errors), it may be necessary to reduce this
C value to achieve strict positivity.
C
C This hack applies to all tracers except temperature and salinity!
C Do not use with Adams-Bashforth (for ptracers)!
C Do not use with OBCS!
#undef  GAD_SMOLARKIEWICZ_HACK

#else

C If GAD is disabled then so is multi-dimensional advection
#define DISABLE_MULTIDIM_ADVECTION

#endif /* ALLOW_GENERIC_ADVDIFF */
#endif /* GAD_OPTIONS_H */
