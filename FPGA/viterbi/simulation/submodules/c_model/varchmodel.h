/* ---------------------------------------------------------------------------------------
// FILE NAME        : varchmodel.h
// DESCRIPTION      : header of lib vblkmodel
// -------------------------------------------------------------------------------------*/
#ifndef _VARCHMODEL_H
#define _VARCHMODEL_H

int** vblock(int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs);
int** vcont (int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs);
int** vnone (int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs, int BSF);
int** vhyb  (int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs, int bm_init_state, int bm_init_value);

#endif /* end _VARCHMODEL_H */
