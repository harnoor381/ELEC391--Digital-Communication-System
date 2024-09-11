#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include "vdef.h"
#include "vcommonlib.h"
#include "venginelib.h"

// ------------------------------------------------------------------------------------
// Function     : create_next_state_table
// Description  : function to fill in the contents of next_state_table
//                the next state is identified by shifting right one bit
//                and shifting the input to the MSB of the states
// input        : the constrain length L and the information bit wide K
// output       : the next_state_table will be updated.
// ------------------------------------------------------------------------------------
int** create_next_state_table (int l_max, int k)
{

    int two_power_k;            // pow of bit width
    int current_state;          // current state
    int next_state;             // next state
    int inbit;                  // input bit to make the state change

    int nstates;
    int** next_state_table; // next state table which is a 3D array

    // calculate power of bit-width which will be the column in the next state tables
    two_power_k = (int)pow(2, k);
    nstates = (int)pow(2, l_max - 1);

    // create the table
    next_state_table = (int**)allocate_table(nstates, two_power_k, sizeof(int));
    create_2D_table((void**)next_state_table, nstates, two_power_k, sizeof(int));

    // loops through the possible states and finds the next state
    for (current_state = 0; current_state < nstates; current_state++) {
        next_state = current_state >> 1;                                        // shift right current state one bit to make next state
        for (inbit = 0; inbit < two_power_k; inbit++) {                         // repeat with each possible value of the input
            next_state +=  inbit << (l_max - 2);                                    // shift input bit to the MSB of state
            next_state_table[current_state][inbit] = next_state;    // assign to the table at position [state, input]
        } // end for
    } // end for

    return next_state_table;
} // end function create_next_state_table


// ------------------------------------------------------------------------------------
// Function     : self_xor
// Description  : this function will return XOR of 16 bits of input
// input        : an integer value
// output       : a XOR return of all 16 bits of the input
// ------------------------------------------------------------------------------------
int self_xor (int x)
{
    int y;
    /* take a snapshot of input */
    y = x;
    /* shift left and XOR together */
    y ^= y >> 1;
    y ^= y >> 2;
    y ^= y >> 4;
    y ^= y >> 8;
    y ^= y >> 16;
    /* we only need the LSB */
    y &= 1;
    return y;
} // end function self_xor

// ------------------------------------------------------------------------------------
// Function     : create_output_table
// Description  : function to fill in the contents of output_table
//            to calculate the out put value for output_table, the function follow 3 steps:
//                 1st  : combine the input with the current state, input bit is the MSB
//                 2nd : multiply (AND) the polynomial with combined bit sequence
//                 3rd : XOR all bits in the result
// input        : output_table, the constrain length L, the information bit wide K, number of polynomials and the polynomials.
// output       : the output_table will be updated.
// ------------------------------------------------------------------------------------
int*** create_output_table (int* l, int k, int ncodes, int* n, int** polya)
{
    int i;                      // use for loop
    int* nstates;               // number of states
    int sel_code;
    int two_power_k;            // pow of bit width
    int current_state;          // current state
    int combined_bits;          // combine between the input bit and the current state
    int inbit;                  // input bit to the encoder
    int poly;                   // the polynomials
    int one_encoded_bit;        // result of one encode bit (one polynomial)
    int encoded;                // the final encoded bits

    int** *output_table;

    // calculate number of states power of bit-width which will be the number of rows and column in output table
    two_power_k = (int)pow(2, k);
    nstates = create_nstates(l, ncodes);

    // create output_table
    output_table = (int***)create_3D_table(ncodes, nstates, two_power_k, sizeof(int));

    // loop through the table to fill in the content
    for (sel_code = 0; sel_code < ncodes; sel_code++) {
        for (current_state = 0; current_state < nstates[sel_code]; current_state++) {
            for (inbit = 0; inbit < two_power_k; inbit++) {                         // repeat with each possible value of the input
                combined_bits = current_state +  (inbit << (l[sel_code] - 1));      // combine the input with the current state, input bit is MSB
                encoded = 0;                                                        // initialize encoded
                for (i = 0; i < n[sel_code]; i++) {                                         // loop through all polynomials to calculate the output
                    poly = polya[sel_code][i];                                      // take the polynomial
                    one_encoded_bit = poly & combined_bits;                         // multiply (AND) the polynomial with combined bit sequence
                    one_encoded_bit = self_xor (one_encoded_bit);                   // XOR all bits
                    // now, we put it with the encoded by insert the new encoded bit to the left
                    encoded = (encoded << 1) + one_encoded_bit;
                } // end 4th for
                // write to out_put_table
                output_table[sel_code][current_state][inbit] = encoded;
            } // end 3rd for
        } // end 2sd for
    } // end 1st for

    free(nstates);

    return output_table;

} // end function create_output_table

// ------------------------------------------------------------------------------------
// Function     : create_nstates
// Description  :
//
// input        :
// output       :
// ------------------------------------------------------------------------------------
int* create_nstates(int* l, int ncodes)
{
    int* nstates;
    int i;

    nstates = (int*)malloc(ncodes * sizeof(int));
    // calculate number of states
    for (i = 0; i < ncodes; i++) {
        nstates[i] = (int)pow(2, l[i] - 1);
    } // end for

    return nstates;
} // end function create_nstates

// ------------------------------------------------------------------------------------
// Function     : create_bm_table
// Description  : function to generate Brand metric table
// input        : bm_table, softbit
// output       : bm_table will be updated
// ------------------------------------------------------------------------------------
int** create_bm_table (int softbit)
{
    int center;     // center of rows
    int nrows;      // number of rows
    int i;          // use in for loop statement
    int content;    // content of bmarray
    int** bm_table; // brand metric table

    /* calculate the center of rows (input) */
    center = (int)pow(2, softbit - 1);
    nrows = center * 2;

    // allocate and create the 2-D array
    bm_table = (int**)allocate_table(nrows, 2, sizeof(int));
    create_2D_table((void**)bm_table, nrows, 2, sizeof(int));

    if (softbit == 1) {
        bm_table[0][0] = 1;
        bm_table[0][1] = 0;
        bm_table[1][0] = 0;
        bm_table[1][1] = 1;
    } else {
        /* fill in the table */
        /* for hypothesis '0 */
        content = center - 1;       // first value
        for (i = 0; i < nrows; i++) {
            if ((i == center) || (i == center + 1)) {
                content = 1;
            } else {
                content++;
            }
            // write to array
            bm_table[i][0] = content;
        }

        /* for hypothesis '1 */
        content = center + 1;       // first value
        for (i = 0; i < nrows; i++) {
            if ((i == center) || (i == center + 1)) {
                content = nrows - 1;
            } else {
                content--;
            }
            /* write to array */
            bm_table[i][1] = content;
        }
    }

    return bm_table;
} // end function create_bm_table

// ------------------------------------------------------------------------------------
// Function     : create_path_metric_table
// Description  : function to generate Brand metric table
// input        :
// output       :
// ------------------------------------------------------------------------------------
int** create_path_metric_table (int nstates, int time_length, int bm_init_state, int bm_init_value)
{
    int** path_metric_table;
//  int i, j;

    // allocate for the 2-D table
    path_metric_table = (int**)allocate_table(nstates, time_length, sizeof(int));
    create_2D_table((void**)path_metric_table, nstates, time_length, sizeof(int));

    // initialize the table
    reset_path_metric_table(path_metric_table, nstates, time_length, bm_init_state, bm_init_value);

    return path_metric_table;

} // end function create_path_metric_table

// ------------------------------------------------------------------------------------
// Function     : reset_path_metric_table
// Description  : function to generate Brand metric table
// input        :
// output       :
// ------------------------------------------------------------------------------------
void reset_path_metric_table(int** path_metric_table, int nstates, int time_length, int bm_init_state, int bm_init_value)
{
    int i, j;

    // initialize the table
    for (i = 0; i < nstates; i++) {
        for (j = 0; j < time_length; j++) {
            path_metric_table[i][j] = 0;
        } // end for
    } // end for

    // setup trellis initial state and value
    path_metric_table[bm_init_state][0] = bm_init_value;
}// end function

// ------------------------------------------------------------------------------------
// Function     : create_state_history_table
// Description  : function to generate state_history_table
// input        :
// output       :
// ------------------------------------------------------------------------------------
int** create_state_history_table (int nstates, int time_length)
{
    int** state_history_table;
    int i, j;

    // allocate for the 2-D table
    state_history_table = (int**)allocate_table(nstates, time_length, sizeof(int));
    create_2D_table((void**)state_history_table, nstates, time_length, sizeof(int));

    // initialize the table
    for (i = 0; i < nstates; i++) {
        for (j = 0; j < time_length; j++) {
            state_history_table[i][j] = 0;
        } // end for
    } // end for


    return state_history_table;

} // end function create_state_history_table

// ------------------------------------------------------------------------------------
// Function     : create_tb_table
// Description  : function to generate state_tb_table
// input        :
// output       :
// ------------------------------------------------------------------------------------
void** create_tb_table (int nrows, STIMULY* stim, int size_of_element)
{
    int i;                          // use in for loop
    char** row_ptr;                 // temp pointer to store the begin of the table

    // allocate for row array
    row_ptr = malloc (nrows * sizeof(char*));
    // for each pointer of row, allocate an array for it
    for (i = 0; i < nrows; i++) {
        *(row_ptr + i) = malloc ((stim[i].size_of_block + 1) * size_of_element);
    } // end for
    return (void**)row_ptr;
} // end function create_tb_table

// ------------------------------------------------------------------------------------
// Function     : acs
// Description  : this function is used to do ACS
//            it loops through next_state_table and output_table to
//            calculate the brand metric base on bm_table
//            and add them with previous path metrics in path_metric_table.
//            This sum results will be compared with the last sums of these states to
//            make the decisions.
// input        : quantized_bits, time_step, next_state_table, output_table, bm_table,  path_metric_table, state_history_table,
//            the constrain length L, the information bit wide K, number of polynomials N, traceback length tb and softbit.
// output       : update path_metric_table and state_history
// ------------------------------------------------------------------------------------
void acs (int* quantized_bits, int time_step, int sel_code, int l_max, int* l, int k, int* n, int softbit, int** next_state_table, int** * output_table, int** bm_table, int** path_metric_table, int** state_history_table)
{
    int i, j;       // loop index

    int number_of_states;                       // number of states
    int two_power_k;                        // pow of bit width
    //int nrows;                                    // number of rows

    int current_state, next_state, represent_state;
    int brand_metric;
    int path_metric;
    int outbit;
    int rec_bit;                                    // one encoded of received bits

    // calculate number of states power of bit-width which will be the number of rows and columns in some table
    number_of_states = (int)pow(2, l_max - 1);
    two_power_k = (int)pow(2, k);

    // calculate number of rows (the number of value which the input can take)
    //nrows = (int)pow(2,softbit);

    // loop through the next_state_table and output_table to calculate brand metric
    for (current_state = 0; current_state < number_of_states; current_state++) {
        for (j = 0; j < two_power_k; j++) {
            // take the next state
            next_state = next_state_table[current_state][j];
            // define represent_state
            represent_state = current_state >> (l_max - l[sel_code]);
            // calculate branch metric for each state transition
            brand_metric = 0;
            // ---------- rec_bits = quantized_bits;
            for (i = 0; i < n[sel_code]; i++) {
                outbit = output_table[sel_code][represent_state][j] >> (i);
                outbit &= 1;
                rec_bit = quantized_bits[n[sel_code] - 1 - i];
                brand_metric += bm_table[rec_bit][outbit];
            } // end for
            // adding with path metric of current state to form path metric of next state
            path_metric = path_metric_table[current_state][time_step - 1] + brand_metric;
            // compare with path metric which is available, if the new one is greater than the old one, save the new one
            if (path_metric >= path_metric_table[next_state][time_step]) {
                // write the new one to path _metric_table
                path_metric_table[next_state][time_step] = path_metric;
                // record to state_history
                state_history_table[next_state][time_step] = current_state;
            } // end if
        } // end for
    } // end for

} // end function acs

// ------------------------------------------------------------------------------------
// Function     : get_max
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
int get_max (int* lst, int lst_size)
{
    int i;
    int tmp, max;

    max = 0;
    for (i = 0; i < lst_size; i++) {
        tmp = lst[i];
        if (tmp > max) {
            max = tmp;
        } // end if
    } // end for
    return max;
} // end function get_max

// ------------------------------------------------------------------------------------
// Function     : get_size_max
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
int get_size_max (STIMULY* stim, int nblocks)
{
    int i;
    int size_max;

    size_max = stim[0].size_of_block;
    for (i = 0; i < nblocks; i++) {
        if (stim[i].size_of_block > size_max ) {
            size_max = stim[i].size_of_block;
        }// end if
    }// end for

    return size_max;
}// end function get_size_max

// ------------------------------------------------------------------------------------
// Function     : get_bmg
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
int get_bmg(int l_max, int n_max, int softbit)
{
    int l_min;
    int threshold;
    int bmg_min = 9;
    int bmg;
    int bmg_opt;
    int i;

    l_min = (l_max > 3) ? l_max : 4;

    threshold = (int)(pow(2, softbit) - 2) * n_max * (l_min + 1);

    if (n_max < 4 ) {
        bmg_min = softbit + 5;
    } else {
        if (n_max < 6) {
            bmg_min = softbit + 6;
        } else {
            bmg_min = softbit + 7;
        } // end if
    } // end if

    bmg_opt = bmg_min;

    for (bmg = softbit; bmg < 24; bmg++) {
        i = (int)pow(2, bmg - 1) - 6;
        if ( i > threshold ) {
            bmg_opt = (bmg > bmg_min) ? bmg : bmg_min;
            return bmg_opt;
        } // end if
    } // end for

    return bmg_opt;
} // end function get_bmg

// ------------------------------------------------------------------------------------
// Function     : best_state_finder
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
int best_state_finder(int** path_metric_table, int nstates, int last_step)
{
    int i;
    int best_state, best_state_value;

    best_state = 0;
    best_state_value = 0;
    for (i = 0; i < nstates; i++) {
        if (best_state_value < path_metric_table[i][last_step]) {
            best_state_value = path_metric_table[i][last_step];
            best_state = i;
        } // end if
    } // end for i

    return best_state;

} //end function best_state_finder

// ------------------------------------------------------------------------------------
// Function     : traceback_blk
// Description  :
// input        : state_history_table, tb_index, tb_state
// output       : state
// ------------------------------------------------------------------------------------
void traceback(int** state_history_table, int blk, int tb_init_state, int** tb_table, int start, int end)
{
    int i;

    tb_table[blk][start - 1] = tb_init_state;
    //look at state_history_table, at tb_state and tb-index
    for (i = start - 2; i >= end; i--) {
        tb_table[blk][i] = state_history_table[tb_table[blk][i + 1]][i + 1];;
    } // end for i

} // end function traceback

// ------------------------------------------------------------------------------------
// Function     : decode
// Description  :
// input        : state_history_table, tb_index, tb_state
// output       : state
// ------------------------------------------------------------------------------------
void decode(int** tb_table, int blk, int l_max, int start, int end, int** decoded_table )
{
    int i;
    int decoded_bit;
    for (i = start; i <= end; i++) {
        // MSB of the survivor state is the decoded bit
        decoded_bit = tb_table[blk][i] >> (l_max - 2);
        decoded_table[blk][i] = decoded_bit;
    }// end for

}// end function decode
