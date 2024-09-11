// ---------------------------------------------------------------------------------------
// FILE NAME        : vdef.h
// DESCRIPTION      : header of lib vdef
// ---------------------------------------------------------------------------------------
#ifndef _VDEF_H
#define _VDEF_H

typedef char symbol[10];

typedef struct {
    int sel_code;
    int bm_init_state;
    int tb_length;
    int tr_init_state;
    int tb_type;
    int size_of_block;
} STIMULY;

#endif // end _VDEF_H

