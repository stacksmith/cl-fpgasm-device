# tweak an xdlrc file to allow Lisp reader to load it
#
s/# /; /g              #fix comments
s/#/_/g                #replace # prefix with _
s|\(_[A-Za-z]\+\):<eqn>|(\1\ eqn>)|g    #convert cfg assignments to pair lists