#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vdef.h"
#include "vcommonlib.h"
#include "venginelib.h"

// ------------------------------------------------------------------------------------
// Function     : vblock
// Description  :
// input        : L - array of constrain length
//              : N - array of number of polynomials
//              : K - input bit with of the source information
//              : ncodes - number of code set.
//              : poly - array of polynomials (multiple code set)
//              : softbit - soft bits decoding
//              : V - maximum traceback
// output       : decoded bits table
// ------------------------------------------------------------------------------------
int** vblock(int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs)
{
    int i, j;
    // declare using working table
    int** next_state_table;
    int*** output_table;
    int** bm_table;
    int** path_metric_table;
    int** state_history_table;
    int** tb_table;
    int** decoded_table;
    
    int* nrows; //array of number of states.
    int two_power_k;

    int bmrows;
    int nstates;

    int time_length;

    int l_max;
    int n_max;
    int bmg;
    
    //
    int time_step;
    int blk;
    int* quantized_bits;
    
    // set time_length
    time_length = get_size_max(stim, nblocks) + 1;

    // calculate number of states for each code and put them to an array
    nrows = create_nstates(L, ncodes);

    two_power_k = (int)pow(2, K);

    // get bmg
    l_max = get_max(L, ncodes);
    n_max = get_max(N, ncodes);
    bmg = get_bmg(l_max, n_max, softbit);

    // generate the table
    next_state_table = create_next_state_table(l_max, K);
    output_table = create_output_table(L, K, ncodes, N, poly);
    bm_table = create_bm_table(softbit);

    // create path metric
    nstates = (int)pow(2, l_max - 1);
    path_metric_table = create_path_metric_table(nstates, time_length, 0, (int)pow(2, bmg - 2));

    // create survival table
    state_history_table = create_state_history_table(nstates, time_length);


    // print out table content for debugging

    printf ("--------Viterbi configuration table content---------\n\n");
    printf ("next state table \n");
    print_array(next_state_table, (int)pow(2, l_max - 1), two_power_k);

    printf ("\noutput table \n");
    print_3D_array(output_table, ncodes, nrows, two_power_k);

    printf ("\nbm table\n");
    bmrows = (int)pow(2, softbit - 1) * 2;
    print_array(bm_table, bmrows, 2);

    printf ("\n---Optimizing for bmg-----\n");
    printf (" ----- l_max = %d\n", l_max);
    printf (" ----- n_max = %d\n", n_max);
    printf (" ----- bmg   = %d\n", bmg);
    printf ("---------------------------\n");
    

    //setup traceback table
    tb_table = (int**)create_tb_table(nblocks, stim, sizeof(int));
    decoded_table = (int**)create_tb_table(nblocks, stim, sizeof(int));

    // print out stimulies for debugging
    printf("\n\n\n-------STIMULIES--------\n");
    printf("\nnumber of blocks = %d\n", nblocks);
    for (i = 0; i < nblocks; i++) {
        printf("\n-block %d----------------\n", i);
        printf(" sel_code      = %d\n", stim[i].sel_code);
        printf(" bm_init_state = %d\n", stim[i].bm_init_state);
        printf(" tb_length     = %d\n", stim[i].tb_length);
        printf(" tr_init_state = %d\n", stim[i].tr_init_state);
        printf(" tb_type       = %d\n", stim[i].tb_type);
        printf(" size_of_block = %d\n", stim[i].size_of_block);
        printf(" received symbols =\n");
        for (j = 0; j < N[stim[i].sel_code] * stim[i].size_of_block; j++) {
            printf (" %d", rcv_symbs[i][j]);
        }
        printf("\n-----------------------\n");
    } // end for i

    // Viterbi algorithm processing through all block of stimulies
    // first do ACS, than do traceback
    printf("\n\n\n ---------VITERBI ALGORITHM PROCESSING------------------\n\n");
    for (blk = 0; blk < nblocks; blk++) {
        printf("\n\n\n# processing data block %d\n", blk);
        // doing acs, read each symbol to quantized_bits
        printf ("constructing trellis ..........\n");

        for (time_step = 1; time_step < stim[blk].size_of_block + 1; time_step++) {
            // calculate quantized_bits
            quantized_bits = (int*)malloc(N[stim[blk].sel_code] * sizeof(int));
            for (i = 0; i < N[stim[blk].sel_code]; i++) {
                quantized_bits[i] = rcv_symbs[blk][(time_step - 1) * N[stim[blk].sel_code] + i];
            }// end loop i
            acs(quantized_bits, time_step, stim[blk].sel_code, l_max, L, K, N, softbit, next_state_table, output_table, bm_table, path_metric_table, state_history_table);
            free(quantized_bits);
        } // end acs loop


        //print path metric table & survival table for debugging
        printf ("\n\n path metric table\n");
        print_array(path_metric_table, nstates, time_length);

        printf ("\n\n state history table\n");
        print_array(state_history_table, nstates, time_length);


        // do trace back
        printf ("tracing back .....\n");
        traceback(state_history_table, blk, stim[blk].tr_init_state, tb_table, stim[blk].size_of_block + 1, 0);
        // output decoded bit
        decode(tb_table, blk, l_max, 0, stim[blk].size_of_block + 1, decoded_table);

        //reset path_metric_table
        reset_path_metric_table(path_metric_table, nstates, time_length, 0, (int)pow(2, bmg - 2));

    } // end blk loop

    printf("\n\n\n ---------DONE VITERBI ALGORITHM PROCESSING--------------\n\n");

    // print out content of path-metric table, survival table and traceback table for debuging
    printf("\n\n Content of table for debugging ----------------\n\n");

    //print trace_back table
    printf ("\n trace back path \n");
    printf ("---|---------------\n");
    for (i = 0; i < nblocks; i++) {
        printf(" %d |", i);
        for (j = 0; j < stim[i].size_of_block + 1; j ++) {
            printf (" %d", tb_table[i][j]);
        } //end for j
        printf("\n");
    } //end for i

    // cleaning up, freeing using memory
    //free(nrows);
    //free(path_metric_table);
    //free(state_history_table);
    //free_2D_array(tb_table, nblocks);
    //free(next_state_table);
    //free_3D_array(output_table, ncodes, nrows, two_power_k);


    // return decoded table
    return decoded_table;

} //end function vblock

// ------------------------------------------------------------------------------------
// Function     : vcont
// Description  :
// input        : L - array of constrain length
//              : N - array of number of polynomials
//              : K - input bit with of the source information
//              : ncodes - number of code set.
//              : poly - array of polynomials (multiple code set)
//              : softbit - soft bits decoding
//              : V - maximum traceback
// output       : decoded bits table
// ------------------------------------------------------------------------------------
int** vcont(int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs)
{
    int i, j;
    // declare using working table
    int** next_state_table;
    int** *output_table;
    int** bm_table;
    int** path_metric_table;
    int** state_history_table;
    int** tb_table;
    int** decoded_table;


    int* nrows; //array of number of states.
    int two_power_k;

    int bmrows;
    int nstates;


    int time_length;

    int l_max;
    int n_max;
    int bmg;
    
    int tr_init_step;

    //
    int time_step;
    int blk;
    int* quantized_bits;

    // set time_length
    time_length = V + 1;
    //time_length = get_size_max(stim, nblocks) + 1;

    // calculate number of states for each code and put them to an array
    nrows = create_nstates(L, ncodes);

    two_power_k = (int)pow(2, K);

    // get bmg
    l_max = get_max(L, ncodes);
    n_max = get_max(N, ncodes);
    bmg = get_bmg(l_max, n_max, softbit);

    // generate the table
    next_state_table = create_next_state_table(l_max, K);
    output_table = create_output_table(L, K, ncodes, N, poly);
    bm_table = create_bm_table(softbit);

    // create path metric
    nstates = (int)pow(2, l_max - 1);
    path_metric_table = create_path_metric_table(nstates, get_size_max(stim, nblocks) + 1, 0, (int)pow(2, bmg - 2));

    // create survival table
    state_history_table = create_state_history_table(nstates, get_size_max(stim, nblocks) + 1);


    // print out table content for debugging

    printf ("--------Viterbi configuration table content---------\n\n");
    printf ("next state table \n");
    print_array(next_state_table, (int)pow(2, l_max - 1), two_power_k);

    printf ("\noutput table \n");
    print_3D_array(output_table, ncodes, nrows, two_power_k);

    printf ("\nbm table\n");
    bmrows = (int)pow(2, softbit - 1) * 2;
    print_array(bm_table, bmrows, 2);

    printf ("\n---Optimizing for bmg-----\n");
    printf (" ----- l_max = %d\n", l_max);
    printf (" ----- n_max = %d\n", n_max);
    printf (" ----- bmg   = %d\n", bmg);
    printf ("---------------------------\n");



    //setup traceback table
    tb_table = (int**)create_tb_table(nblocks, stim, sizeof(int));
    decoded_table = (int**)create_tb_table(nblocks, stim, sizeof(int));

    // print out stimulies for debuging
    printf("\n\n\n-------STIMULIES--------\n");
    printf("\nnumber of blocks = %d\n", nblocks);
    for (i = 0; i < nblocks; i++) {
        printf("\n-block %d----------------\n", i);
        printf(" sel_code      = %d\n", stim[i].sel_code);
        printf(" bm_init_state = %d\n", stim[i].bm_init_state);
        printf(" tb_length     = %d\n", stim[i].tb_length);
        printf(" tr_init_state = %d\n", stim[i].tr_init_state);
        printf(" tb_type       = %d\n", stim[i].tb_type);
        printf(" size_of_block = %d\n", stim[i].size_of_block);
        printf(" received symbols =\n");
        for (j = 0; j < N[stim[i].sel_code] * stim[i].size_of_block; j++) {
            printf (" %d", rcv_symbs[i][j]);
        }
        printf("\n-----------------------\n");
    } // end for i

    // Viterbi algorithm processing through all block of stimulies
    // first do ACS, than do traceback
    printf("\n\n\n ---------VITERBI ALGORITHM PROCESSING------------------\n\n");
    for (blk = 0; blk < nblocks; blk++) {
        printf("\n\n\n# processing data block %d\n", blk);
        // doing acs, read each symbol to quantized_bits

        for (j = 0; j < ceil((double)stim[blk].size_of_block / V) ; j++) {
            printf ("constructing trellis ..........\n");
            for (time_step = j * V + 1; time_step < j * V + time_length; time_step++) {
                // caculate quantized_bits
                quantized_bits = (int*)malloc(N[stim[blk].sel_code] * sizeof(int));
                for (i = 0; i < N[stim[blk].sel_code]; i++) {
                    quantized_bits[i] = rcv_symbs[blk][(time_step - 1) * N[stim[blk].sel_code] + i];
                }// end loop i
                acs(quantized_bits, time_step, stim[blk].sel_code, l_max, L, K, N, softbit, next_state_table, output_table, bm_table, path_metric_table, state_history_table);
                free(quantized_bits);
                if (time_step == stim[blk].size_of_block) {
                    tr_init_step = time_step + 1;
                    break;
                } else {
                    tr_init_step = j * V + time_length;
                } //end if
            } // end acs loop

            if (j > 0) {
                // do trace back
                printf ("tracing back .....\n");
                traceback(state_history_table, blk, 0, tb_table, tr_init_step, (j - 1)*V );
                printf ("decoding .....\n");
                // output decoded bit
                decode(tb_table, blk, l_max, (j - 1)*V, j * V + 1, decoded_table);
            } //end if

        }// end for j



        //print path metric table & survival table for debugging
        //printf ("\n\n path metric table\n");
        //print_array(path_metric_table, nstates, stim[blk].size_of_block + 1);

        //printf ("\n\n state history table\n");
        //print_array(state_history_table, nstates, stim[blk].size_of_block + 1);

        //reset path_metric_table
        reset_path_metric_table(path_metric_table, nstates, stim[blk].size_of_block + 1, 0, (int)pow(2, bmg - 2));

    } // end blk loop

    printf("\n\n\n ---------DONE VITERBI ALGORITHM PROCESSING--------------\n\n");

    // print out content of path-metric table, survival table and traceback table for debugging
    printf("\n\n Content of table for debugging ----------------\n\n");

    //print trace_back table
    printf ("\n trace back path \n");
    printf ("---|---------------\n");
    for (i = 0; i < nblocks; i++) {
        printf(" %d |", i);
        for (j = 0; j < stim[i].size_of_block + 1; j ++) {
            printf (" %d", tb_table[i][j]);
        } //end for j
        printf("\n");
    } //end for i

    // cleaning up, freeing using memory
    //free(nrows);
    //free(path_metric_table);
    //free(state_history_table);
    //free_2D_array(tb_table, nblocks);
    //free(next_state_table);
    //free_3D_array(output_table, ncodes, nrows, two_power_k);


    // return decoded table
    return decoded_table;

} //end function vcont

// ------------------------------------------------------------------------------------
// Function     : vnone
// Description  :
// input        : L - array of constrain length
//              : N - array of number of polynomials
//              : K - input bit with of the source information
//              : ncodes - number of code set.
//              : poly - array of polynomials (multiple code set)
//              : softbit - soft bits decoding
//              : V - maximum tracback
// output       : decoded bits table
// ------------------------------------------------------------------------------------
int** vnone(int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs, int BSF)
{
    int i, j;
    // declare using working table
    int** next_state_table;
    int** *output_table;
    int** bm_table;
    int** path_metric_table;
    int** state_history_table;
    int** tb_table;
    int** decoded_table;


    int* nrows; //array of number of states.
    int two_power_k;

    int bmrows;
    int nstates;


    int time_length;

    int l_max;
    int n_max;
    int bmg;

    int tr_init_step;
    int tr_init_state;
    int dec_end;


    //
    int time_step;
    int blk;
    int* quantized_bits;
    
    // set time_length
    time_length = V + 1;
    //time_length = get_size_max(stim, nblocks) + 1;

    // calculate number of states for each code and put them to an array
    nrows = create_nstates(L, ncodes);

    two_power_k = (int)pow(2, K);

    // get bmg
    l_max = get_max(L, ncodes);
    n_max = get_max(N, ncodes);
    bmg = get_bmg(l_max, n_max, softbit);

    // generate the table
    next_state_table = create_next_state_table(l_max, K);
    output_table = create_output_table(L, K, ncodes, N, poly);
    bm_table = create_bm_table(softbit);

    // create path metric
    nstates = (int)pow(2, l_max - 1);
    path_metric_table = create_path_metric_table(nstates, get_size_max(stim, nblocks) + 1, 0, (int)pow(2, bmg - 2));

    // create survival table
    state_history_table = create_state_history_table(nstates, get_size_max(stim, nblocks) + 1);


    // print out table content for debugging

    printf ("--------Viterbi configuration table content---------\n\n");
    printf ("next state table \n");
    print_array(next_state_table, (int)pow(2, l_max - 1), two_power_k);

    printf ("\noutput table \n");
    print_3D_array(output_table, ncodes, nrows, two_power_k);

    printf ("\nbm table\n");
    bmrows = (int)pow(2, softbit - 1) * 2;
    print_array(bm_table, bmrows, 2);

    printf ("\n---Optimizing for bmg-----\n");
    printf (" ----- l_max = %d\n", l_max);
    printf (" ----- n_max = %d\n", n_max);
    printf (" ----- bmg   = %d\n", bmg);
    printf ("---------------------------\n");



    //setup traceback table
    tb_table = (int**)create_tb_table(nblocks, stim, sizeof(int));
    decoded_table = (int**)create_tb_table(nblocks, stim, sizeof(int));

    // print out stimulies for debugging
    printf("\n\n\n-------STIMULIES--------\n");
    printf("\nnumber of blocks = %d\n", nblocks);
    for (i = 0; i < nblocks; i++) {
        printf("\n-block %d----------------\n", i);
        printf(" sel_code      = %d\n", stim[i].sel_code);
        printf(" bm_init_state = %d\n", stim[i].bm_init_state);
        printf(" tb_length     = %d\n", stim[i].tb_length);
        printf(" tr_init_state = %d\n", stim[i].tr_init_state);
        printf(" tb_type       = %d\n", stim[i].tb_type);
        printf(" size_of_block = %d\n", stim[i].size_of_block);
        printf(" received symbols =\n");
        for (j = 0; j < N[stim[i].sel_code] * stim[i].size_of_block; j++) {
            printf (" %d", rcv_symbs[i][j]);
        }
        printf("\n-----------------------\n");
    } // end for i

    // Viterbi algorithm processing through all block of stimulies
    // first do ACS, than do traceback
    printf("\n\n\n ---------VITERBI ALGORITHM PROCESSING------------------\n\n");
    for (blk = 0; blk < nblocks; blk++) {

        printf("\n\n\n# processing data block %d\n", blk);
        // doing acs, read each symbol to quantized_bits


        for (j = 0; j < ceil((double)stim[blk].size_of_block / V); j++) {
            printf ("constructing trellis %d..........\n", j);
            for (time_step = j * V + 1; time_step < j * V + time_length; time_step++) {
                // calculate quantized_bits
                quantized_bits = (int*)malloc(N[stim[blk].sel_code] * sizeof(int));
                for (i = 0; i < N[stim[blk].sel_code]; i++) {
                    quantized_bits[i] = rcv_symbs[blk][(time_step - 1) * N[stim[blk].sel_code] + i];
                }// end loop i
                acs(quantized_bits, time_step, stim[blk].sel_code, l_max, L, K, N, softbit, next_state_table, output_table, bm_table, path_metric_table, state_history_table);
                free(quantized_bits);
                if (time_step == stim[blk].size_of_block) {
                    tr_init_step = time_step + 1;
                    if (BSF == 1 || (ncodes > 1 && L[stim[blk].sel_code] < l_max)) {
                        tr_init_state = best_state_finder(path_metric_table, nstates, time_step);
                    } else {
                        tr_init_state = stim[blk].tr_init_state;
                    } //end if
                    dec_end = tr_init_step;
                    printf ("\nbest state = %d\n\n", tr_init_state);
                    break;
                } else {
                    tr_init_step = j * V + time_length;
                    tr_init_state = 0;
                    dec_end = j * V + 1;
                } //end if
            } // end acs loop

            if (j > 0) {
                // do trace back
                printf ("tracing back .....\n");
                traceback(state_history_table, blk, tr_init_state, tb_table, tr_init_step, (j - 1)*V );
                printf ("decoding .....\n");
                // output decoded bit
                decode(tb_table, blk, l_max, (j - 1)*V, dec_end, decoded_table);
            } //end if


        }// end for j



        //print path metric table & survival table for debugging
        //printf ("\n\n path metric table\n");
        //print_array(path_metric_table, nstates, stim[blk].size_of_block + 1);

        //printf ("\n\n state history table\n");
        //print_array(state_history_table, nstates, stim[blk].size_of_block + 1);

        //reset path_metric_table
        reset_path_metric_table(path_metric_table, nstates, stim[blk].size_of_block + 1, 0, (int)pow(2, bmg - 2));

    } // end blk loop

    printf("\n\n\n ---------DONE VITERBI ALGORITHM PROCESSING--------------\n\n");

    // print out content of path-metric table, survival table and traceback table for debuging
    printf("\n\n Content of table for debugging ----------------\n\n");

    //print trace_back table
    printf ("\n trace back path \n");
    printf ("---|---------------\n");
    for (i = 0; i < nblocks; i++) {
        printf(" %d |", i);
        for (j = 0; j < stim[i].size_of_block + 1; j ++) {
            printf (" %d", tb_table[i][j]);
        } //end for j
        printf("\n");
    } //end for i

    // cleaning up, freeing using memory
    //free(nrows);
    //free(path_metric_table);
    //free(state_history_table);
    //free_2D_array(tb_table, nblocks);
    //free(next_state_table);
    //free_3D_array(output_table, ncodes, nrows, two_power_k);


    // return decoded table
    return decoded_table;

} //end function vnone

// ------------------------------------------------------------------------------------
// Function     : vhyb
// Description  :
// input        : L - array of constrain length
//              : N - array of number of polynomials
//              : K - input bit with of the source information
//              : ncodes - number of code set.
//              : poly - array of polynomials (multiple code set)
//              : softbit - soft bits decoding
//              : V - maximum tracback
// output       : decoded bits table
// ------------------------------------------------------------------------------------
int** vhyb(int nblocks, int* L, int* N, int K, int ncodes, int** poly, int softbit, int V, STIMULY* stim, int** rcv_symbs, int bm_init_state, int bm_init_value)
{
    int i, j;
    // declare using working table
    int** next_state_table;
    int** *output_table;
    int** bm_table;
    int** path_metric_table;
    int** state_history_table;
    int** tb_table;
    int** decoded_table;
    
    int* nrows; //array of number of states.
    int two_power_k;

    int bmrows;
    int nstates;

    int time_length;

    int l_max;
    int n_max;
    int bmg;
    
    int tr_init_step;
    int tr_init_state;
    int dec_end;
    
    //
    int time_step;
    int blk;
    int* quantized_bits;
    
    // set time_length
    time_length = V + 1;

    // calculate number of states for each code and put them to an array
    nrows = create_nstates(L, ncodes);

    two_power_k = (int)pow(2, K);

    // get bmg
    l_max = get_max(L, ncodes);
    n_max = get_max(N, ncodes);
    bmg = get_bmg(l_max, n_max, softbit);

    // generate the table
    next_state_table = create_next_state_table(l_max, K);
    output_table = create_output_table(L, K, ncodes, N, poly);
    bm_table = create_bm_table(softbit);

    // create path metric
    nstates = (int)pow(2, l_max - 1);
    //path_metric_table = create_path_metric_table(nstates, get_size_max(stim, nblocks) + 1, bm_init_state, bm_init_value);
    path_metric_table = create_path_metric_table(nstates, get_size_max(stim, nblocks) + 1, 0, (int)pow(2, bmg - 2));

    // create survival table
    state_history_table = create_state_history_table(nstates, get_size_max(stim, nblocks) + 1);
    
    // print out table content for debugging
    printf ("--------Viterbi configuration table content---------\n\n");
    printf ("next state table \n");
    print_array(next_state_table, (int)pow(2, l_max - 1), two_power_k);

    printf ("\noutput table \n");
    print_3D_array(output_table, ncodes, nrows, two_power_k);

    printf ("\nbm table\n");
    bmrows = (int)pow(2, softbit - 1) * 2;
    print_array(bm_table, bmrows, 2);

    printf ("\n---Optimizing for bmg-----\n");
    printf (" ----- l_max = %d\n", l_max);
    printf (" ----- n_max = %d\n", n_max);
    printf (" ----- bmg   = %d\n", bmg);
    printf ("---------------------------\n");
    
    //setup traceback table
    tb_table = (int**)create_tb_table(nblocks, stim, sizeof(int));
    decoded_table = (int**)create_tb_table(nblocks, stim, sizeof(int));

    // print out stimulies for debugging
    printf("\n\n\n-------STIMULIES--------\n");
    printf("\nnumber of blocks = %d\n", nblocks);
    for (i = 0; i < nblocks; i++) {
        printf("\n-block %d----------------\n", i);
        printf(" sel_code      = %d\n", stim[i].sel_code);
        printf(" bm_init_state = %d\n", stim[i].bm_init_state);
        printf(" tb_length     = %d\n", stim[i].tb_length);
        printf(" tr_init_state = %d\n", stim[i].tr_init_state);
        printf(" tb_type       = %d\n", stim[i].tb_type);
        printf(" size_of_block = %d\n", stim[i].size_of_block);
        printf(" received symbols =\n");
        for (j = 0; j < N[stim[i].sel_code] * stim[i].size_of_block; j++) {
            printf (" %d", rcv_symbs[i][j]);
        }
        printf("\n-----------------------\n");
    } // end for i

    // Viterbi algorithm processing through all block of stimulies
    // first do ACS, than do traceback
    printf("\n\n\n ---------VITERBI ALGORITHM PROCESSING------------------\n\n");
    for (blk = 0; blk < nblocks; blk++) {

        printf("\n\n\n# processing data block %d\n", blk);
        // doing acs, read each symbol to quantized_bits


        for (j = 0; j < ceil((double)stim[blk].size_of_block / V); j++) {
            printf ("constructing trellis ..........\n");
            for (time_step = j * V + 1; time_step < j * V + time_length; time_step++) {
                // calculate quantized_bits
                quantized_bits = (int*)malloc(N[stim[blk].sel_code] * sizeof(int));
                for (i = 0; i < N[stim[blk].sel_code]; i++) {
                    quantized_bits[i] = rcv_symbs[blk][(time_step - 1) * N[stim[blk].sel_code] + i];
                }// end loop i
                acs(quantized_bits, time_step, stim[blk].sel_code, l_max, L, K, N, softbit, next_state_table, output_table, bm_table, path_metric_table, state_history_table);
                free(quantized_bits);
                if (time_step == stim[blk].size_of_block) {
                    tr_init_step = time_step + 1;
                    if (ncodes > 1 && L[stim[blk].sel_code] < l_max) {
                        tr_init_state = best_state_finder(path_metric_table, nstates, time_step);
                    } else {
                        tr_init_state = stim[blk].tr_init_state;
                    } //end if
                    dec_end = tr_init_step;
                    printf ("\nbest state = %d\n\n", tr_init_state);
                    break;
                } else {
                    tr_init_step = j * V + time_length;
                    tr_init_state = 0;
                    dec_end = j * V + 1;
                } //end if
            } // end acs loop

            if (j > 0) {
                // do trace back
                printf ("tracing back .....\n");
                traceback(state_history_table, blk, tr_init_state, tb_table, tr_init_step, (j - 1)*V );
                printf ("decoding .....\n");
                // output decoded bit
                decode(tb_table, blk, l_max, (j - 1)*V, dec_end, decoded_table);
            } //end if


        }// end for j
                
        //print path metric table & survival table for debugging
        //printf ("\n\n path metric table\n");
        //print_array(path_metric_table, nstates, stim[blk].size_of_block + 1);

        //printf ("\n\n state history table\n");
        //print_array(state_history_table, nstates, stim[blk].size_of_block + 1);

        //reset path_metric_table
        //reset_path_metric_table(path_metric_table, nstates, time_length, bm_init_state, bm_init_value);
        reset_path_metric_table(path_metric_table, nstates, stim[blk].size_of_block + 1, 0, (int)pow(2, bmg - 2));

    } // end blk loop

    printf("\n\n\n ---------DONE VITERBI ALGORITHM PROCESSING--------------\n\n");

    // print out content of path-metric table, survival table and traceback table for debugging
    printf("\n\n Content of table for debugging ----------------\n\n");

    //print trace_back table
    printf ("\n trace back path \n");
    printf ("---|---------------\n");
    for (i = 0; i < nblocks; i++) {
        printf(" %d |", i);
        for (j = 0; j < stim[i].size_of_block + 1; j ++) {
            printf (" %d", tb_table[i][j]);
        } //end for j
        printf("\n");
    } //end for i

    // cleaning up, freeing using memory
    //free(nrows);
    //free(path_metric_table);
    //free(state_history_table);
    //free_2D_array(tb_table, nblocks);
    //free(next_state_table);
    //free_3D_array(output_table, ncodes, nrows, two_power_k);
    
    // return decoded table
    return decoded_table;

} //end function vhyb
