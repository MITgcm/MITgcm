/* 
 * $Header: /u/gcmpack/MITgcm/eesupp/src/gsl_ieee_env.c,v 1.2 2007/10/08 17:28:53 jmc Exp $
 * $Name:  $
 *
 * A cross-platform way of providing access to gsl_ieee_env_setup 
 * from Fortran on any system MITgcm runs on currently.
 */

#ifdef USE_GSL_IEEE
#include <gsl/gsl_math.h>
#include <gsl/gsl_ieee_utils.h>

void fgsl_ieee_env_setup ()
{
  gsl_ieee_env_setup ();
}

void fgsl_ieee_env_setup_ ()
{
  gsl_ieee_env_setup ();
}

void fgsl_ieee_env_setup__ ()
{
  gsl_ieee_env_setup ();
}

void FGSL_IEEE_ENV_SETUP ()
{
  gsl_ieee_env_setup ();
}
#endif
