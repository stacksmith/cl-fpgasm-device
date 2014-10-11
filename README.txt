cl-fpgasm-device
================
WORK IN PROGRESS! frequent changes as I massage the data access patterns to match needs.

cl-fpgasm-device is a part of cl-fpgasm (a bare-metal FPGA assembler)

(load-device)   loads xc3s200

*dev* contains device database.  You can browse it (although it's really for fpgasm)... 

Symbols of tile names, primitive_sites, primitive_defs contain data!  You can evaluate things like SLICEM or R22C1 or CAPTURE to see what's inside.  

For instance, R22C1 will evaluate to 
#S(TILE
   :NAME R22C1
   :TYPE CENTER_SMALL
   :X 26
   :Y 2
   :PRIM-SITES #<HASH-TABLE :TEST EQL :COUNT 6 {1007F44743}>)

To see hashtables, use 

(keys (tile-prim-sites R22C1))

(RLL_X1Y3 VCC_X1Y3 SLICE_X0Y4 SLICE_X0Y5 SLICE_X1Y4 SLICE_X1Y5)

Now you can

(gethash 'RL_X1Y3 (tile-prim-sites R22C1))

Of course, you can just type RL_X1Y3 since primitive sites are values of their symbols...


