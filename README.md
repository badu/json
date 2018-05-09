# Json

Standard Go "encoding/json" package re-arranged to my needs.

# Modifications

* Most constants extracted from code
* some utility methods made receiver methods
* parts from strconv moved inside this package (mostly numeric, no numeric base specific)
* fast number scanner in strconv (numbers are already validated on decoding, so there is no need to validate)
* walker get / set for reflect (separations of concerns)
* marshal field struct different from unmarshal field struct
* encode state is the options carrier, instead of passing around options as parameter

# Removed features

* TextUnmarshaler and TextMarshaler 

# New features

* two sets of reflect field cache : one for encoding, other for decoding (W.I.P.)
* optional sort map keys on serializing
* encoding of Null typed structs (NullString, NullBool, e.t.c.)