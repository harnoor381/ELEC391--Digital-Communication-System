#include <string>
#include <vector>
#include <stdint.h>

extern "C" {
#include "vdef.h"
#include "vcommonlib.h"
#include "venginelib.h"
#include "varchmodel.h"
}

#ifdef MEX_COMPILE
#include "mex.h"
#endif

#ifdef MEX_COMPILE
inline int getInt(int index, const mxArray* prhs[])
{
    return (int)*mxGetPr(prhs[index]);
}

inline std::string getString(int index, const mxArray* prhs[])
{
    char* str = (char*)mxCalloc(mxGetN(prhs[index]) + 1, sizeof(char));
    mxGetString(prhs[index], str, mxGetN(prhs[index]) + 1);
    std::string result = std::string(str);
    mxFree(str);
    return result;
}

template<typename T>
inline std::vector<std::vector<T> > getMatrix(int index, const mxArray* prhs[])
{
    double* in_array = mxGetPr(prhs[index]);
    size_t m = mxGetM(prhs[index]);
    size_t n = mxGetN(prhs[index]);
    std::vector<std::vector<T> > matrix(m);
    for (size_t i = 0; i < m; ++i) {
        matrix[i] = std::vector<T>(n);
        for (size_t j = 0; j < n; ++j) {
            matrix[i][j] = (T)in_array[i * m + j];
        }
    }
    return matrix;
}

void printhelp()
{
    mexPrintf("vit_ii_mex(ncodes, L, N, [ code(0) , .... , code(ncodes) ], softbit, V, BSF, architecture, optimization, RCVSYM_FILE, BLKSTIM_FILE, DEC_FILE)\n");
    mexPrintf("\t notes: L,N,gx are underscore separated strings, so if there are 5 codes L could be 4_5_3_5_2 and code(0) could be 4_23_27_31 \n");
    mexPrintf("\t notes: V means traceback length\n");
    mexPrintf("\t notes: BSF - Best State Finder\n");
    mexPrintf("\t notes: RCVSYM_FILE - Received Symbol File\n");
    mexPrintf("\t notes: BLKSTIM_FILE - Block Stimulus File\n");
    mexPrintf("\t notes: DEC_FILE - Decoded File\n");
    mexPrintf("\t notes: architecture - hybrid or parallel\n");
    mexPrintf("\t notes: optimization - none or continuous\n");
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
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

    if (nrhs < 9 || nlhs != 1) {
        printhelp();
        mexErrMsgIdAndTxt("Altera:Viterbi:Arguments", "Arguments invalid.");
    }

    int v = 0;
    ncodes = getInt(v++, prhs);
    mexPrintf("Number of codes: %d\n", ncodes);
    std::vector<int> L = getMatrix<int>(v++, prhs)[0];
    std::vector<int> N = getMatrix<int>(v++, prhs)[0];

    int i = 0;
    int maxN = 0;
    for (i = 0; i < ncodes; i++) {
        if (maxN < N[i]) {
            maxN = N[i];
        }
    }
    mexPrintf("maxN = %d\n", maxN);

    poly = (int**)create_poly_table(ncodes, N.data(), sizeof(int));
    int numCodesIn = static_cast<int>(mxGetNumberOfElements(prhs[v]));
    if (numCodesIn != ncodes) {
        mexErrMsgIdAndTxt("Altera:Viterbi:Arguments", "Invalid number of polynomial codes (got %i expected %i)", numCodesIn, ncodes);
    }
    for (i = 0; i < ncodes; ++i) {
        mxArray* arr = mxGetCell(prhs[v], i);
        double* in_array = mxGetPr(arr);
        int size = static_cast<int>(mxGetN(arr));
        if (N[i] != size) {
            mexErrMsgIdAndTxt("Altera:Viterbi:Arguments", "Mismatch in N and provided polynomials for code %i (got %i expected %i)", i, size, N[i]);
        }
        for (int n = 0; n < N[i]; ++n) {
            poly[i][n] = (int)in_array[n];
        }
    }
    {
        int k = 0;
        int l = 0;
        for (k = 0; k < ncodes; k++) {
            mexPrintf("code %d = ", k);
            for (l = 0; l < N[k]; l++) {
                mexPrintf("%d, ", poly[k][l]);
            }
            mexPrintf("\n");
        }
    }
    mexPrintf("end\n");

    v++;
    softbit = getInt(v++, prhs);
    mexPrintf("Softbit :%d\n", softbit);

    V = getInt(v++, prhs);
    mexPrintf("V :%d\n", V);

    BSF = getInt(v++, prhs);
    mexPrintf("BSF :%d\n", BSF);

    std::string arch = getString(v++, prhs);
    mexPrintf("arch :%s\n", arch.c_str());
    std::string opt = getString(v++, prhs);
    mexPrintf("opt :%s\n", opt.c_str());
        
    stim = read_stim_from_matrix(prhs[v+1], &nblocks);
    rcv_symbs = (int**)read_rcvsym_from_cell(prhs[v], nblocks, N.data(), stim);
    if (arch == "hybrid") {
        decoded_table = vhyb(nblocks, L.data(), N.data(), K, ncodes, poly, softbit, V, stim, rcv_symbs, BSF, 0);
    } else if (opt == "none") {
        decoded_table = vnone(nblocks, L.data(), N.data(), K, ncodes, poly, softbit, V, stim, rcv_symbs, BSF);
    } else if (opt == "continuous") {
        decoded_table = vcont(nblocks, L.data(), N.data(), K, ncodes, poly, softbit, V, stim, rcv_symbs);
    } else {
        decoded_table = vblock(nblocks, L.data(), N.data(), K, ncodes, poly, softbit, V, stim, rcv_symbs);
    }

    write_dec_to_matrix(plhs, 0, decoded_table, nblocks, stim);
    free_2D_array(poly, ncodes);
    free_2D_array(rcv_symbs, nblocks);
    free(decoded_table);
}

#endif