/* ---------------------------------------------------------------------------------------
// FILE NAME        : vcommonlib.h
// DESCRIPTION      : header of lib vcommonlib
// -------------------------------------------------------------------------------------*/
#ifndef _VCOMMONLIB_H
#define _VCOMMONLIB_H

#ifdef MEX_COMPILE
#include "mex.h"
#endif

void**   allocate_table (int nrows, int ncols, int size_of_element);
void     create_2D_table (void** table_ptr, int nrows, int ncols, int size_of_element);
void**   create_data_table (int nrows, int* N, STIMULY* stim, int size_of_element);
void**   create_poly_table (int nrows, int* N, int size_of_element);
void***  create_3D_table (int nrows1, int* nrows2, int ncols, int size_of_element);
void     initialize_2D_table (int** array, int nrows, int ncols, int initial_value);
void     print_array (int** array, int nrows, int ncols);
void     copy_file(char* source_file, char* destination_file);
int      hex2dec (char hex);
void     update_struct (STIMULY* stim , int stim_index, int fieldid, int val);
void     str2int (STIMULY* stim , int stim_size, int fieldid, char* strin);
int**    read_rcvsym_from_file(const char* symfile, int number_of_blocks, int* N, STIMULY* stim);
STIMULY* read_stim_from_file(const char* blk_file, int* nblocksOut);
#ifdef MEX_COMPILE
int**    read_rcvsym_from_cell(const mxArray* cell, int number_of_blocks, int* N, STIMULY* stim);
STIMULY* read_stim_from_matrix(const mxArray* matrix, int* nblocksOut);
#endif
void     free_2D_array (int** ptr, int nrows);
void     free_3D_array (int** * ptr, int nrows1, int* nrows2, int ncols);
void     print_3D_array (int** * ptr, int nrows1, int* nrows2, int ncols);
void     print_2D_array (int** ptr, int nrows, int* ncols);
void     write_dec_to_file(const char* dec_file, int** decoded_table, int nblocks, STIMULY* stim);
#ifdef MEX_COMPILE
void     write_dec_to_matrix(mxArray* plhs[], int index, int** decoded_table, int nblocks, STIMULY* stim);
#endif

#endif /* end _VCOMMONLIB_H */
