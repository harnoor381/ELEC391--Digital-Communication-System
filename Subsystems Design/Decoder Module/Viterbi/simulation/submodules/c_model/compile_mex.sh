# (C) 2001-2018 Intel Corporation. All rights reserved.
# Your use of Intel Corporation's design tools, logic functions and other 
# software and tools, and its AMPP partner logic functions, and any output 
# files from any of the foregoing (including device programming or simulation 
# files), and any associated documentation or information are expressly subject 
# to the terms and conditions of the Intel Program License Subscription 
# Agreement, Intel FPGA IP License Agreement, or other applicable 
# license agreement, including, without limitation, that your use is for the 
# sole purpose of programming logic devices manufactured by Intel and sold by 
# Intel or its authorized distributors.  Please refer to the applicable 
# agreement for further details.


#!/bin/sh
mex -v CFLAGS="\$CFLAGS -std=c99" -DMEX_COMPILE mex_vit.cpp varchmodel.c vcommonlib.c venginelib.c -output vit_ii_mex
mex -v CFLAGS="\$CFLAGS -std=c99" -DMEX_COMPILE mex_vit_direct.cpp varchmodel.c vcommonlib.c venginelib.c -output vit_ii_mex_direct
mex -v CFLAGS="\$CFLAGS -std=c99" -DMEX_COMPILE mex_vit_direct_out.cpp varchmodel.c vcommonlib.c venginelib.c -output vit_ii_mex_direct_out