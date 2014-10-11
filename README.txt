cl-fpgasm-device
================
WORK IN PROGRESS! frequent changes as I massage the data access patterns to match needs.

cl-fpgasm-device is a part of cl-fpgasm (a bare-metal FPGA assembler)

(load-device)   loads xc3s200

*dev* contains device database.  You can browse it (although it's really for fpgasm)... 

Symbols of tile names, primitive_sites, primitive_defs contain data!  You can evaluate things like SLICEM or R22C1 or CAPTURE to see what's inside.  

For now, tiles are stored in a two-dimensional array.  Elements are parsed into structures, and data is maintained in lists.  As access patterns are determined, hashtables will be added to structures.
