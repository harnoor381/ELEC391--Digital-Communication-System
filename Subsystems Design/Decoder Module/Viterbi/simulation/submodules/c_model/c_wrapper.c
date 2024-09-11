#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vdef.h"
#include "vcommonlib.h"
#include "venginelib.h"
#include "varchmodel.h"

int main (int argc, char* argv)
{
    int* N;
    int* L;
    int** poly;
    int K = 1;
    int ncodes;
    int softbit;
    int V;
    int BSF;
    STIMULY* stim;
    int nblocks;
    int** rcv_symbs;
    int** decoded_table;

    printf("c_wrapper : starting c model \n ");
    ncodes = 5;

    L = (int*)malloc(ncodes * sizeof(int));
    L[0] = 5;
    L[1] = 3;
    L[2] = 4;
    L[3] = 3;
    L[4] = 3;

    N = (int*)malloc(ncodes * sizeof(int));
    N[0] = 4;
    N[1] = 2;
    N[2] = 3;
    N[3] = 2;
    N[4] = 2;

    poly = (int**)create_poly_table(ncodes, N, sizeof(int));
    poly[0][0] = 4;
    poly[0][1] = 23;
    poly[0][2] = 27;
    poly[0][3] = 31;

    poly[1][0] = 5;
    poly[1][1] = 7;

    poly[2][0] = 11;
    poly[2][1] = 13;
    poly[2][2] = 15;

    poly[3][0] = 5;
    poly[3][1] = 7;

    poly[4][0] = 5;
    poly[4][1] = 7;


    softbit = 3;
    V = 30;
    BSF = 0;
    
    char* blkstim_file = "block_period_stim.txt";
    char* rcvsym_file = "a_rcvsym.txt";
    char* dec_file = "simulation/modelsim/decoded_c.txt";
    
    stim = read_stim_from_file(blkstim_file, &nblocks);
    rcv_symbs = (int**)read_rcvsym_from_file(rcvsym_file, nblocks, N, stim);
    decoded_table = vnone(nblocks, L, N, K, ncodes, poly, softbit, V, stim, rcv_symbs, BSF);
    write_dec_to_file(dec_file, decoded_table, nblocks, stim);
    free(L);
    free(N);
    free_2D_array(poly, ncodes);
    free_2D_array(rcv_symbs, nblocks);
    free(decoded_table);
}
