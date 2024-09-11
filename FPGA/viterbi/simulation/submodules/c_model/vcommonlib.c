// ---------------------------------------------------------------------------------------
// FILE NAME        : vcommonlib.c
// DESCRIPTION      : library of common functions
// ---------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------
// include header
// ----------------------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include "vdef.h"
#include "vcommonlib.h"

// ------------------------------------------------------------------------------------
// Function     : allocate_table
// Description  : this function is used to memory to two dimension arrays
// input        : the pointer of the table, number of columns, number of rows, size of element
// output       : a reserved memory block for the table
// ------------------------------------------------------------------------------------
void** allocate_table (int nrows, int ncols, int size_of_element)
{
    void** table_ptr;
    table_ptr = malloc (nrows * sizeof(double*) + ncols * nrows * size_of_element);
    return table_ptr;
} // end function create_2D_table

// ------------------------------------------------------------------------------------
// Function     : create_2D_table
// Description  : this function is used to create a two dimension array
//                by formatting it as a 2D table
// input        : the pointer of the table, number of columns, number of rows
// output       : the 2D-formatted table
// ------------------------------------------------------------------------------------
void create_2D_table (void** table_ptr, int nrows, int ncols, int size_of_element)
{
    int i;                          // use in for loop
    char* data_ptr;                 // temp pointer to store the begin of the table

    // create the table format
    data_ptr = (char*) (table_ptr + nrows);
    for (i = 0; i < nrows; i++) {
        *(table_ptr + i) = data_ptr + i * ncols * size_of_element;
    } // end for

} // end function create_2D_table

// ------------------------------------------------------------------------------------
// Function     : create_data_table
// Description  : this function is used to create a two dimension array
//                which may have different size in each array. Those tables are used
//                to store received symbols and decoded bits
// input        : the pointer of the table, number of columns, number of rows
// output       : the 2D-formatted table
// ------------------------------------------------------------------------------------
void** create_data_table (int nrows, int* N, STIMULY* stim, int size_of_element)
{
    int i;                          // use in for loop
    char** row_ptr;                 // temp pointer to store the begin of the table

    // allocate for row array
    row_ptr = malloc (nrows * sizeof(char*));
    // for each pointer of row, allocate an array for it
    for (i = 0; i < nrows; i++) {
        *(row_ptr + i) = malloc (N[stim[i].sel_code] * stim[i].size_of_block * size_of_element);
    } // end for
    return (void**)row_ptr;
} // end function create_data_table

// ------------------------------------------------------------------------------------
// Function     : create_poly_table
// Description  : this function is used to create a two dimension array
//                which may have different size in each array. Those tables are used
//                to store received symbols and decoded bits
// input        : the pointer of the table, number of columns, number of rows
// output       : the 2D-formatted table
// ------------------------------------------------------------------------------------
void** create_poly_table (int nrows, int* N, int size_of_element)
{
    int i;                          // use in for loop
    char** row_ptr;                 // temp pointer to store the begin of the table

    // allocate for row array
    row_ptr = malloc (nrows * sizeof(char*));
    // for each pointer of row, allocate an array for it
    for (i = 0; i < nrows; i++) {
        *(row_ptr + i) = malloc (N[i] * size_of_element);
    } // end for
    return (void**)row_ptr;
} // end function create_poly_table

// ------------------------------------------------------------------------------------
// Function     : create_3D_table
// Description  : this function is used to create a three dimension array
//                which may have different size in each array. Those tables are used
//                to store received symbols and decoded bits
// input        : the pointer of the table, number of columns, number of rows
// output       : the 2D-formatted table
// ------------------------------------------------------------------------------------
void*** create_3D_table (int nrows1, int* nrows2, int ncols, int size_of_element)
{
    int i, j;                           // use in for loop
    char** *row_ptr;                    // temp pointer to store the begin of the table

    // allocate for row array
    row_ptr = malloc (nrows1 * sizeof(char**));
    // for each pointer of 1st row, allocate a 2D array for it
    for (i = 0; i < nrows1; i++) {
        *(row_ptr + i) = malloc (nrows2[i] * sizeof(char*));
        // for each pointer of 2st row, allocate an array for it
        for (j = 0; j < nrows2[i]; j++) {
            *(*(row_ptr + i) + j) = malloc (ncols * size_of_element);
        } // end for
    } // end for
    return (void***)row_ptr;
} // end function create_3D_table

// ------------------------------------------------------------------------------------
// Function     : initialize_2D_table
// Description  : this function is used to initialize the two dimension array
// input        : the array (table), number of rows, number of columns, initial value
// output       : no output, the array will be updated.
// ------------------------------------------------------------------------------------
void initialize_2D_table (int** array, int nrows, int ncols, int initial_value)
{
    int i, j;
    for (i = 0; i < nrows; i++) {
        for (j = 0; j < ncols; j++) {
            array[i][j] = initial_value;
        } // end for
    } // end for
} // end function initialize_2D_table

// ------------------------------------------------------------------------------------
// Function     : print_array
// Description  : this function will print array to screen
//                only for testing purpose
// input        : the array (table), number of rows, number of columns,
// output       : no output, the contents of the array will be printed to the screen.
// ------------------------------------------------------------------------------------
void print_array (int** array, int nrows, int ncols)
{
    int i, j;
    printf ("---|------------\n");
    for (i = 0; i < nrows; i++) {
        printf (" %d | ", i);
        for (j = 0; j < ncols; j++) {
            printf ("%d ", array[i][j]);
        } // end for
        printf ("\n");
    } // end for
} // end function  print_array

// ------------------------------------------------------------------------------------
// Function     : copy_file
// Description  : to copy one file to another
// input        : source file path, destination file path
// output       : the file
// ------------------------------------------------------------------------------------
void copy_file(char* source_file, char* destination_file)
{
    FILE* src, *des;            // file pointer for the source file and destination file
    char ch;                    // temp variable which use to store each line of the source file

    // open the source file for read
    if ((src = fopen(source_file, "r")) == NULL) {
        printf ("# ERROR: couldn't open %s\n", source_file);
        return;
    } // end if

    // open the destination file for write
    if ((des = fopen(destination_file, "w")) == NULL) {
        printf ("# ERROR: couldn't open %s\n", destination_file);
        return;
    } // end if

    // copy each symbol in source file to destination file
    printf ("copying from %s to %s \n", source_file, destination_file);
    while (!feof(src)) {
        // read a charactor from source file
        ch = getc(src);
        // write to destination file
        if (!feof(src)) {
            putc(ch, des);
        } // end if
    } // end of while

    //close files
    fclose(src);
    fclose(des);
} // end function copy_file

// ------------------------------------------------------------------------------------
// Function     : hex2dec
// Description  : this function will convert a hex number (type is char)
//                to dec number (type is int)
// input        : an string that contain hex number
// output       : a converted number (in type int)
// ------------------------------------------------------------------------------------
int hex2dec (char hex)
{

    int y;              // store the result
    char x;             // lower

    x = tolower(hex);

    if (x >= '0' && x <= '9') {
        y = x - '0';
    } else if (x >= 'a' && x <= 'f') {
        y = x - 'a' + 10;
    } else {
        y = 0;
    }   // end if

    return y;

} // end function hex2dec

// ------------------------------------------------------------------------------------
// Function     : update_struct
// Description  : this function will convert a hex number (type is char)
//                to dec number (type is int)
// input        : an string that contain hex number
// output       : a converted number (in type int)
// ------------------------------------------------------------------------------------
void update_struct (STIMULY* stim , int stim_index, int fieldid, int val)
{
    switch (fieldid) {
    case 0:
        stim[stim_index].sel_code = val;
        break;
    case 1:
        stim[stim_index].bm_init_state = val;
        break;
    case 2:
        stim[stim_index].tb_length = val;
        break;
    case 3:
        stim[stim_index].tr_init_state = val;
        break;
    case 4:
        stim[stim_index].tb_type = val;
        break;
    case 5:
        stim[stim_index].size_of_block = val;
        break;
    default :
        break;
    } // end switch

} // end function update_struct

// ------------------------------------------------------------------------------------
// Function     : str2int
// Description  : this function will convert a hex number (type is char)
//                to dec number (type is int)
// input        : an string that contain hex number
// output       : a converted number (in type int)
// ------------------------------------------------------------------------------------
void str2int (STIMULY* stim , int stim_size, int fieldid, char* strin)
{

    //char strin [128];                     //
    int y = 0;                              // store the result
    int i;                                  // use in for loop
    int stim_index = 0;                         // index of struct
    char write_2_array_ena = 'f';           //

    for (i = 0; i < 128; i++) {
        if ((strin[i] == ' ' || strin[i] == '\0') && write_2_array_ena == 't') {    //if we got the space character
            // write to struct
            update_struct(stim, stim_index, fieldid, y);
            y = 0;
            write_2_array_ena = 'f';
            stim_index++;
            if (stim_index > stim_size) {
                printf ("vcommomlib : ERROR : error in function str2int, number of block is incorrect\n");
            } // end if
        } else if (strin[i] >= '0' && strin[i] <= '9') {
            //
            y = y * 10 + (strin[i] - '0');  // get the integer value
            write_2_array_ena = 't';
        } // end if

        if (strin[i] == '\0') {
            break;
        }
    } // end for

} // end function hex2dec

// ------------------------------------------------------------------------------------
// Function     : read_rcvsym_from_file
// Description  : to read rceived symbols from file a_rcvsym.txt
// input        : file name
// output       : content of the file
// ------------------------------------------------------------------------------------
int** read_rcvsym_from_file (const char* symfile, int number_of_blocks, int* N, STIMULY* stim)
{
    FILE* file_ptr;                     // file pointer to open file for read
    symbol rcvsym;                      // to store each received symbol
    //symbol *rcvdata;
    int i, j, k;                        // index variable using in for loop
    int rcv_index = 0;                  // index of rcvsym_table for each row
    int** rcvsym_table;

    // allocate and create 3D table for rcvsym_table
    rcvsym_table = (int**)create_data_table(number_of_blocks, N, stim, sizeof(int));

    // open the file for read
    printf("symfile file %s\n", symfile);

    if ((file_ptr = fopen(symfile, "r")) == NULL) {
        printf ("# ERROR: couldn't open %s\n", symfile);
    } // end if

    // Read file. Each symbol is seperate with the others by space characters
    for (i = 0; i < number_of_blocks; i++) { // for each block
        rcv_index = 0;                      // reset for new block
        for (j = 0; j < stim[i].size_of_block; j++) {   // for each symbol
            fscanf(file_ptr, "%s", &rcvsym);
            //convert received symbols from hex to dec
            for (k = 0; k < 10; k++) {
                if (rcvsym[k] == '\0') { // stop when see NULL character
                    break;
                } else {
                    // if puncturing scheme is using, convert X to 0;
                    if (rcvsym[k] == 'x' || rcvsym[k] == 'X') {
                        rcvsym[k] = '0';
                    } //end if
                    // convert the symbol to integer, update it to rcvsym_table
                    rcvsym_table[i][rcv_index] = hex2dec(rcvsym[k]);
                    rcv_index++;
                } // end if
            } // end 3rd forloop
        } // end for 2nd for loop
    } // end 1st for loop

    // close file
    fclose(file_ptr);

    return rcvsym_table;
} // end function read_rcvsym_from_file

// ------------------------------------------------------------------------------------
// Function     : read_stim_from_file
// Description  : to read stimuli from file block_period_stim.txt
// input        : file name
// output       : number of block
// ------------------------------------------------------------------------------------
STIMULY* read_stim_from_file(const char* blk_file, int* nblocks)
{
    FILE* file_ptr;     // file pointer to open file for read

    STIMULY* stim;

    symbol nblocks_symb;

    char sel_code[128];
    char bm_init_state[128];
    char tb_length[128];
    char tr_init_state[128];
    char tb_type[128];
    char size_of_block[128];

    // open the file for read
    printf("block file %s\n", blk_file);
    if ((file_ptr = fopen(blk_file, "r")) == NULL) {
        printf ("# ERROR: couldn't open %s\n", blk_file);
        return 0;
    } // end if

    //read file from 1st line to 7th line

    // 1st line -> number of block
    fgets(nblocks_symb, sizeof(symbol), file_ptr);

    // 2nd line -> sel code
    fgets(sel_code, 128, file_ptr);

    // 3rd line -> bm_init_state
    fgets(bm_init_state, 128, file_ptr);

    // 4th line -> tb_length
    fgets(tb_length, 128, file_ptr);

    // 5th line -> tr_init_state
    fgets(tr_init_state, 128, file_ptr);

    // 6th line -> tb_type
    fgets(tb_type, 128, file_ptr);

    // 7th line -> size_of_block
    fgets(size_of_block, 128, file_ptr);


    // write stimulies to struct stim

    // converting number of block to integer from string
    *nblocks = atoi(nblocks_symb);

    // allocate for stim
    stim = (STIMULY*) malloc(*nblocks * sizeof(STIMULY));

    // then write sel_code to struct array stim
    str2int(stim, *nblocks, 0, sel_code);

    // write bm_init_state to struct array stim
    str2int(stim, *nblocks, 1, bm_init_state);

    // write tb_length to struct array stim
    str2int(stim, *nblocks, 2, tb_length);

    // write tr_init_state to struct array stim
    str2int(stim, *nblocks, 3, tr_init_state);

    // write tb_type to struct array stim
    str2int(stim, *nblocks, 4, tb_type);

    // write size_of_block to struct array stim
    str2int(stim, *nblocks, 5, size_of_block);

    // close file
    fclose(file_ptr);

    return stim;
}

// ------------------------------------------------------------------------------------
// Function     : free_2D_array
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
void free_2D_array (int** ptr, int nrows)
{
    int i;
    for (i = 0; i < nrows; i++) {
        free (ptr[i]);
    }
    free (ptr);
} // end function free_2D_array

// ------------------------------------------------------------------------------------
// Function     : free_3D_array
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
void free_3D_array (int** * ptr, int nrows1, int* nrows2, int ncols)
{
    int i, j;


    for (i = 0; i < nrows1; i++) {
        for (j = 0; j < nrows2[i]; j++) {
            //free (ptr[i][j]);
            //free(*(*(ptr + i)+j));
        }
        free (ptr[i]);
    }
    free (ptr);
}

// ------------------------------------------------------------------------------------
// Function     : print_3D_array
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
void print_3D_array (int** * ptr, int nrows1, int* nrows2, int ncols)
{
    int i, j, k;


    printf(" i | j |     k      \n");
    printf("---|---|------------\n");
    for (i = 0; i < nrows1; i++) {
        for (j = 0; j < nrows2[i]; j++) {
            printf(" %d | %d | ", i, j);
            for (k = 0; k < ncols; k++) {
                printf("%d ", ptr[i][j][k]);
            }
            printf("\n");
        }
        printf("---|---|------------\n");
    }
}

// ------------------------------------------------------------------------------------
// Function     : print_2D_array
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
void print_2D_array (int** ptr, int nrows, int* ncols)
{
    int i, j;

    printf(" i |    k      \n");
    printf("---|------------\n");
    for (i = 0; i < nrows; i++) {
        printf(" %d | ", i);
        for (j = 0; j < ncols[i]; j++) {
            printf("%d ", ptr[i][j]);
        } // end 2sd for
        printf("\n");
    } // end 1st for
} // end function print_2D_array

// ------------------------------------------------------------------------------------
// Function     : write_dec_to_file
// Description  :
// input        :
// output       :
// ------------------------------------------------------------------------------------
void write_dec_to_file(const char* dec_file, int** decoded_table, int nblocks, STIMULY* stim)
{
    FILE* file_ptr;
    int i, j;

    // open the file for write
    printf("dec_file file %s\n", dec_file);

    if ((file_ptr = fopen(dec_file, "w")) == NULL) {
        printf ("# ERROR: couldn't open %s\n", dec_file);
    } // end if

    for (i = 0; i < nblocks; i++) {
        for (j = 1; j <= stim[i].size_of_block; j++) {
            fprintf(file_ptr, "%d ", decoded_table[i][j]);
            if (j % 20 == 0 && j != stim[i].size_of_block) {
                fprintf(file_ptr, "\n");
            }// end if
        } //end for j
        fprintf(file_ptr, "\n");
    } //end for i

    fclose(file_ptr);
}

#ifdef MEX_COMPILE
void write_dec_to_matrix(mxArray* plhs[], int index, int** decoded_table, int nblocks, STIMULY* stim)
{
    int i, j;
    plhs[index] = mxCreateCellMatrix(1, nblocks);
    for (i = 0; i < nblocks; i++) {
        mxArray* arr = mxCreateNumericMatrix(1, stim[i].size_of_block, mxINT32_CLASS, mxREAL);
        int* data = (int*)mxGetData(arr);
        for (j = 1; j <= stim[i].size_of_block; j++) {
            *(data++) = decoded_table[i][j];
        }
        mxSetCell(plhs[index], i, arr);
    } 
}

int** read_rcvsym_from_cell(const mxArray* cell, int number_of_blocks, int* N, STIMULY* stim)
{
    int** rcvsym_table;

    int i, j;                        // index variable using in for loop
    int rcv_index = 0;               // index of rcvsym_table for each row

    // allocate and create 3D table for rcvsym_table
    rcvsym_table = (int**)create_data_table(number_of_blocks, N, stim, sizeof(int));
    
    // Read file. Each symbol is separate with the others by space characters
    for (i = 0; i < number_of_blocks; i++) { // for each block
        const mxArray* arr = mxGetCell(cell, i);
        int cols = (int)mxGetNumberOfElements(arr);
        if (cols != stim[i].size_of_block) {
            mexErrMsgIdAndTxt("Altera:Viterbi:Arguments", "Invalid rcvsym input, expected %i columns but had %i columns", i, stim[i].size_of_block);
        }
        
        rcv_index = 0;                      // reset for new block
        for (j = 0; j < stim[i].size_of_block; j++) {   // for each symbol
            const mxArray* strArr = mxGetCell(arr, j);
            int strLen = (int)mxGetN(strArr);
            char* str = (char*)mxCalloc(strLen + 1, sizeof(char));
            mxGetString(strArr, str, strLen + 1);

            int k;
            for (k = 0; k < strLen; k++) {
                if (str[k] == '\0') { // stop when see NULL character
                    break;
                }
                else {
                    // if puncturing scheme is using, convert X to 0;
                    if (str[k] == 'x' || str[k] == 'X') {
                        str[k] = '0';
                    } //end if
                    // convert the symbol to integer, update it to rcvsym_table
                    rcvsym_table[i][rcv_index] = hex2dec(str[k]);
                    rcv_index++;
                }
            } 
            
            mxFree(str);
        }
    }
    
    return rcvsym_table;
}

STIMULY* read_stim_from_matrix(const mxArray* matrix, int* nblocksOut)
{
    double* in_array = mxGetPr(matrix);
    size_t rows = mxGetM(matrix);
    *nblocksOut = (int)mxGetN(matrix);
    
    if (rows != 6) {
        mexErrMsgIdAndTxt("Altera:Viterbi:Arguments", "Invalid number of matrix rows in stim input (got %i expected %i)", rows, 6);
    }

    STIMULY* stim = (STIMULY*)malloc(*nblocksOut * sizeof(STIMULY));

    for (size_t j = 0; j < *nblocksOut; ++j) {
        STIMULY s;
        s.sel_code = (int)in_array[(j * 6) + 0];
        s.bm_init_state = (int)in_array[(j * 6) + 1];
        s.tb_length = (int)in_array[(j * 6) + 2];
        s.tr_init_state = (int)in_array[(j * 6) + 3];
        s.tb_type = (int)in_array[(j * 6) + 4];
        s.size_of_block = (int)in_array[(j * 6) + 5];
        stim[j] = s;
    }

    return stim;
}

#endif