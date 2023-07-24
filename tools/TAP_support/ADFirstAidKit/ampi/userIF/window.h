/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_WINDOW_H_
#define _AMPI_WINDOW_H_

/**
 * \file 
 * \ingroup UserInterfaceHeaders
 * The windows in the AMPI context need to be used to track extra information
 * simlilarly to the requests. Each window has a stack that is used to trace the
 * one-sided communication. We do not resort to the ampi tape since the
 * structures differ.
 */ 

#include <stddef.h>
#include "ampi/userIF/libConfig.h"
/**
 * \def Number of traced communications in a stack chunk
 */
#define AMPI_WINDOW_STACK_CHUNK_SIZE 1000
/*
 * @{
 * \name Structures and functions related to the tracing of one-sided
 * communication.
 */

typedef struct {
  void *origin_addr;
  int origin_count;
  MPI_Datatype origin_datatype;
  int target_rank;
  MPI_Aint target_disp;
  int target_count;
  MPI_Datatype target_datatype;
  void *adjointTempBuf;
  void *adjointBuf;
  int adjointCount; 
  void *idx;
} AMPI_WinRequest;

typedef struct {
    AMPI_WinRequest *v;
    int top;
    size_t size;
    MPI_Aint num_reqs;
} AMPI_Win_stack;


void AMPI_WIN_STACK_push(AMPI_Win_stack *s, AMPI_WinRequest req);
AMPI_WinRequest AMPI_WIN_STACK_pop(AMPI_Win_stack *s);
void AMPI_WIN_STACK_stack_init(AMPI_Win_stack *s);
void AMPI_WIN_STACK_destroy(AMPI_Win_stack *s);
int AMPI_WIN_STACK_full(AMPI_Win_stack *s);
void AMPI_WIN_STACK_expand(AMPI_Win_stack *s);
void AMPI_WIN_STACK_shrink(AMPI_Win_stack *s);
int AMPI_WIN_STACK_empty(AMPI_Win_stack *s);



/**
 * AMPI_Win augmented with extra information 
 */ 
typedef struct {
  void **map; /**< The mapped window for interleaved types */
  void *base; /**< The base of the original window */
  void *idx;  /**< Tape indices for the adjoint computation */
  MPI_Aint size; /**< Size of the window */
  AMPI_Win_stack *req_stack; /**< Bookkeeping of the executed one-sided communications */
  MPI_Win **plainWindow; /**< original MPI_Win */
  int num_reqs;
  MPI_Comm comm;
  MPI_Aint disp;
} AMPI_Win;


/**
 * Synchronzation of the window
 */
void AMPI_WIN_sync(AMPI_Win win);
/** @} */


#endif
