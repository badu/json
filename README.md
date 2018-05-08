# Json

Standard Go "encoding/json" package re-arranged to my needs.

# Modifications

* Most constants extracted from code
* some utility functions made receiver functions
* TextUnmarshaler and TextMarshaler removed
* parts from strconv moved inside this package
* fast number scanner in strconv (numbers are already validated on deserializing, so there is no need to validate)
* walker get / set for reflect (separations of concerns)
* two sets of reflect field cache : one for serializing, other for deserializing
* marshal field struct different from unmarshal field struct
* encode state is the options carrier

# New feature
* optional sort map keys on serializing