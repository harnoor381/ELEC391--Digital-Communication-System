// ---------------------------------------------------------------------------------------
// FILE NAME        : vcommonlib.h
// DESCRIPTION      : header of lib vcommonlib
// ---------------------------------------------------------------------------------------
#ifndef _VENGINELIB_H
#define _VENGINELIB_H

int**  create_next_state_table (int l_max, int k);
int    self_xor (int x);
int*** create_output_table (int* l, int k, int ncodes, int* n, int** polya);
int*   create_nstates(int* l, int ncodes);
int**  create_bm_table (int softbit);
int**  create_path_metric_table (int nstates, int time_length, int bm_init_state, int bm_init_value);
void   reset_path_metric_table(int** path_metric_table, int nstates, int time_length, int bm_init_state, int bm_init_value);
int**  create_state_history_table (int nstates, int time_length);
void** create_tb_table (int nrows, STIMULY* stim, int size_of_element);
void   acs (int* quantized_bits, int time_step, int sel_code, int l_max, int* l, int k, int* n, int softbit, int** next_state_table, int** * output_table, int** bm_table, int** path_metric_table, int** state_history_table);
int    get_max (int* lst, int lst_size);
int    get_size_max (STIMULY* stim, int nblocks);
int    get_bmg(int l_max, int n_max, int softbit);
int    best_state_finder(int** path_metric_table, int nstates, int last_step);
void   traceback(int** state_history_table, int blk, int tb_init_state, int** tb_table, int start, int end);
void   decode(int** tb_table, int blk, int l_max, int start, int end, int** decoded_table );

#endif // end _VENGINELIB_H
