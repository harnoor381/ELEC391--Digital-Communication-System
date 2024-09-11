#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "vdef.h"
#include "vcommonlib.h"
#include "venginelib.h"
#include "varchmodel.h"

void printhelp()
{
    printf("Commandline: ncodes L N code(0) .... code(ncodes) softbit V BSF architecture optimization RCVSYM_FILE BLKSTIM_FILE DEC_FILE\n");
    printf("\t notes: L,N,gx are underscore separated strings, so if there are 5 codes L could be 4_5_3_5_2 and code(0) could be 4_23_27_31 \n");
    printf("\t notes: V means traceback length\n");
    printf("\t notes: BSF - Best State Finder\n");
    printf("\t notes: RCVSYM_FILE - Received Symbol File\n");
    printf("\t notes: BLKSTIM_FILE - Block Stimulus File\n");
    printf("\t notes: DEC_FILE - Decoded File\n");
    printf("\t notes: architecture - hybrid or parallel\n");
    printf("\t notes: optimization - none or continuous\n");
}

int main (int argc, char* argv[])
{
    int K = 1;
    int ncodes;
    int softbit;
    int V;
    int BSF;
    STIMULY* stim;
    int nblocks;
    int** rcv_symbs;
    int** decoded_table;
    int** poly;

    printf("c_wrapper : starting c model \n ");

    if (argc < 15)  {
        printf("\n");
        printf("Incorrect arguments\n\n");
        printhelp();
        printf("\n");
        exit(1);
    }
    int v = 1;
    ncodes = atoi(argv[v++]);
    int* N = (int*)malloc(ncodes * sizeof(int));
    int* L = (int*)malloc(ncodes * sizeof(int));

    printf("Ncodes: %d\n", ncodes);
    char* L_string = (char*)malloc(sizeof(char) * (strlen(argv[v]) + 1));
    strcpy(L_string, argv[v++]);
    char* L_cur;
    int j = 0;
    L_cur = strtok(L_string, "_");
    while (L_cur != NULL) {
        L[j++] = atoi(L_cur);
        printf("L[%d]: %d\n", j - 1, L[j - 1]);
        L_cur = strtok(NULL, "_");
    }

    char* n_string = (char*)malloc(sizeof(char) * (strlen(argv[v]) + 1));
    strcpy(n_string, argv[v++]);
    char* n_cur;
    j = 0;
    n_cur = strtok(n_string, "_");
    while (n_cur != NULL) {
        N[j++] = atoi(n_cur);
        n_cur = strtok(NULL, "_");
        printf("N[%d]: %d\n", j - 1, N[j - 1]);

    }
    int i = 0;
    int maxN = 0;
    for (i = 0; i < ncodes; i++) {
        if (maxN < N[i]) {
            maxN = N[i];
        }
    }
    printf("maxN = %d\n", maxN);

    poly = (int**)create_poly_table(ncodes, N, sizeof(int));

    for (i = 0; i < ncodes; ++i) {
        char* poly_string = (char*)malloc(sizeof(char) * (strlen(argv[v]) + 1));
        strcpy(poly_string, argv[v++]);
        char* poly_cur;
        j = 0;
        poly_cur = strtok(poly_string, "_");
        while (poly_cur != NULL) {
            printf("poly: %d\n", atoi(poly_cur));
            poly[i][j++] = atoi(poly_cur);
            poly_cur = strtok(NULL, "_");
        }
        free(poly_string);
    }
    {
        int k = 0;
        int l = 0;
        for (k = 0; k < ncodes; k++) {
            printf("code %d = ", k);
            for (l = 0; l < N[k]; l++) {
                printf("%d, ", poly[k][l]);
            }
            printf("\n");
        }
    }
    printf("end\n");


    softbit = atoi(argv[v++]);
    printf("Softbit :%d\n", softbit);

    V = atoi(argv[v++]);
    printf("V :%d\n", V);

    BSF =  atoi(argv[v++]);
    printf("BSF :%d\n", BSF);

    char* arch =  argv[v++];
    printf("arch :%s\n", arch);
    char* opt =  argv[v++];
    printf("opt :%s\n", opt);

    char* rcvsym_file = argv[v++];
    printf("rcvsym_file :%s\n", rcvsym_file);

    char* blkstim_file = argv[v++];
    printf("blkstim_file :%s\n", blkstim_file);
    char* dec_file = argv[v++];
    printf("dec_file :%s\n", dec_file);
    // char * blkstim_file = "block_period_stim.txt";
    // char * rcvsym_file = "a_rcvsym.txt";
    // char * dec_file = "simulation/modelsim/decoded_c.txt";

    stim = read_stim_from_file(blkstim_file, &nblocks);
    rcv_symbs = (int**)read_rcvsym_from_file(rcvsym_file, nblocks, N, stim);
    if (strcmp(arch, "hybrid") == 0) {
        decoded_table = vhyb(nblocks, L, N, K, ncodes, poly, softbit, V, stim, rcv_symbs, BSF, 0);
    } else if (strcmp(opt, "none") == 0) {
        decoded_table = vnone(nblocks, L, N, K, ncodes, poly, softbit, V, stim, rcv_symbs, BSF);
    } else if (strcmp(opt, "continuous") == 0) {
        decoded_table = vcont(nblocks, L, N, K, ncodes, poly, softbit, V, stim, rcv_symbs);
    } else {
        decoded_table = vblock(nblocks, L, N, K, ncodes, poly, softbit, V, stim, rcv_symbs);
    }

    write_dec_to_file(dec_file, decoded_table, nblocks, stim);
    free_2D_array(poly, ncodes);
    free_2D_array(rcv_symbs, nblocks);
    free(decoded_table);
    free(N);
    free(L);
    free(n_string);
    return 0;
}
