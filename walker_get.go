/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"encoding/base64"
	"errors"
	"math"
	"sort"
	"strings"
	"time"
)

var (
	typeOfNo = TypeOf(Number(""))
)

func (v Value) reflectValue(walker *encodeState) {
	if !v.IsValid() {
		// TODO : maybe return error?
		// write "null"
		walker.Write(nullLiteral)
		return
	}
	v.walk(walker)
}

func (v Value) write(walker *encodeState) bool {
	switch v.Kind() {
	case Bool:

		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		if *(*bool)(v.Ptr) {
			walker.Write(trueLiteral)
		} else {
			walker.Write(falseLiteral)
		}
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}

	case Int:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendInt(walker.scratch[:0], int64(*(*int)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Int8:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendInt(walker.scratch[:0], int64(*(*int8)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Int16:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendInt(walker.scratch[:0], int64(*(*int16)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Int32:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendInt(walker.scratch[:0], int64(*(*int32)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Int64:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendInt(walker.scratch[:0], *(*int64)(v.Ptr)))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Uint:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendUint(walker.scratch[:0], uint64(*(*uint)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Uint8:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendUint(walker.scratch[:0], uint64(*(*uint8)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Uint16:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendUint(walker.scratch[:0], uint64(*(*uint16)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Uint32:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendUint(walker.scratch[:0], uint64(*(*uint32)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Uint64:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendUint(walker.scratch[:0], *(*uint64)(v.Ptr)))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case UintPtr:
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(AppendUint(walker.scratch[:0], uint64(*(*uintptr)(v.Ptr))))
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Float32:
		value := float64(*(*float32)(v.Ptr))
		if math.IsInf(value, 0) || math.IsNaN(value) {
			walker.error(&UnsupportedValueError{FormatFloat(value, 32)})
		}

		// Convert as if by ES6 number to string conversion.
		// This matches most other JSON generators.
		// See golang.org/issue/6384 and golang.org/issue/14135.
		// Like fmt %g, but the exponent cutoffs are different and exponents themselves are not padded to two digits.
		b := walker.scratch[:0]
		abs := math.Abs(value)
		fmt := fChr
		// Note: Must use float32 comparisons for underlying float32 value to get precise cutoffs right.
		if abs != 0 {
			if float32(abs) < 1e-6 || float32(abs) >= 1e21 {
				fmt = eChr
			}

		}
		b = AppendFloat(b, value, fmt, 32)
		if fmt == eChr {
			// clean up e-09 to e-9
			n := len(b)
			if n >= 4 && b[n-4] == eChr && b[n-3] == minus && b[n-2] == zero {
				b[n-2] = b[n-1]
				b = b[:n-1]
			}
		}

		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(b)
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case Float64:
		value := *(*float64)(v.Ptr)
		if math.IsInf(value, 0) || math.IsNaN(value) {
			walker.error(&UnsupportedValueError{FormatFloat(value, 64)})
		}

		// Convert as if by ES6 number to string conversion.
		// This matches most other JSON generators.
		// See golang.org/issue/6384 and golang.org/issue/14135.
		// Like fmt %g, but the exponent cutoffs are different and exponents themselves are not padded to two digits.
		b := walker.scratch[:0]
		abs := math.Abs(value)
		fmt := fChr
		if abs != 0 {
			if abs < 1e-6 || abs >= 1e21 {
				fmt = eChr
			}
		}
		b = AppendFloat(b, value, fmt, 64)
		if fmt == eChr {
			// clean up e-09 to e-9
			n := len(b)
			if n >= 4 && b[n-4] == eChr && b[n-3] == minus && b[n-2] == zero {
				b[n-2] = b[n-1]
				b = b[:n-1]
			}
		}

		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
		walker.Write(b)
		if walker.opts.quoted {
			walker.WriteByte(quote)
		}
	case String:
		theStr := *(*string)(v.Ptr)
		if v.Type == typeOfNo {
			// In Go1.5 the empty string encodes to "0", while this is not a valid number literal we keep compatibility so check validity after this.
			if len(theStr) == 0 {
				theStr = "0" // Number's zero-val
			}
			if !IsValidNumber(theStr) {
				walker.error(errors.New("json: invalid number literal `" + theStr + "`"))
			}
			walker.WriteString(theStr)
		} else {
			if walker.opts.quoted {
				sb, err := Marshal(theStr)
				if err != nil {
					walker.error(err)
				}
				walker.stringBytes(sb)
			} else {
				bStr := []byte(theStr)
				walker.stringBytes(bStr)
			}
		}

	default:
		return false
	}
	return true
}

func (v Value) walk(walker *encodeState) {
	// check Marshaler implementation
	if v.Type.Implements(marshalerType) {
		walker.marshalerEncoder(v)
		return
	}
	if v.Type.Kind() != Ptr {
		if v.Type.PtrTo().Implements(marshalerType) {
			if v.CanAddr() {
				walker.addrMarshalerEncoder(v)
				return
			}
		}
	}
	// no Marshaler
	switch v.Kind() {
	case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
		v.write(walker)
	case Interface:
		if v.IsNil() {
			// write "null"
			walker.Write(nullLiteral)
			return
		}
		v.Iface().reflectValue(walker)
	case Ptr:
		if v.IsNil() {
			// write "null"
			walker.Write(nullLiteral)
			return
		}
		v.Deref().walk(walker)
	case Map:
		v.mapEncoder(walker)
	case Array:
		v.arrayEncoder(walker)
	case Slice:
		v.sliceEncoder(walker)
	case Struct:
		v.structEncoder(walker)
	default:
		walker.error(&UnsupportedTypeError{v.Type})
	}
}

func (v Value) sliceEncoder(walker *encodeState) {
	// read the slice element
	deref := v.Type.ConvToSlice().ElemType
	// Byte slices get special treatment; arrays don't.
	if deref.Kind() == Uint8 {
		//println("Uint8 slice.")
		p := deref.PtrTo()
		// check if []int8 has it's own marshal implementation
		if !p.Implements(marshalerType) {
			if v.IsNil() {
				// write "null"
				walker.Write(nullLiteral)
				return
			}

			walker.WriteByte(quote)
			value := v.Bytes()
			if len(value) < 1024 {
				// for small buffers, using Encode directly is much faster.
				dst := make([]byte, base64.StdEncoding.EncodedLen(len(value)))
				base64.StdEncoding.Encode(dst, value)
				walker.Write(dst)
			} else {
				// for large buffers, avoid unnecessary extra temporary buffer space.
				enc := base64.NewEncoder(base64.StdEncoding, walker)
				enc.Write(value)
				enc.Close()
			}
			walker.WriteByte(quote)

			return
		}
	}

	if v.IsNil() {
		// write "null"
		walker.Write(nullLiteral)
		return
	}

	// process "array"
	v.arrayEncoder(walker)
}

func (v Value) arrayEncoder(walker *encodeState) {
	// Mark Array Start
	walker.WriteByte(squareOpen)
	for i := 0; i < v.Len(); i++ {
		if i > 0 {
			// Next Array Element
			walker.WriteByte(comma)
		}

		v.Index(i).walk(walker)
	}
	// Mark Array End
	walker.WriteByte(squareClose)
}

func (v Value) structEncoder(walker *encodeState) {

	cachedFields, _ := marshalerFieldCache.value.Load().(map[*RType]*marshalFields)
	fieldsInfo := cachedFields[v.Type]
	if fieldsInfo == nil {
		// Compute fields without lock.
		// Might duplicate effort but won't hold other computations back.
		fieldsInfo = v.Type.getMarshalFields()
		if fieldsInfo != nil {
			marshalerFieldCache.mu.Lock()
			cachedFields, _ = marshalerFieldCache.value.Load().(map[*RType]*marshalFields)

			newFieldsMap := make(map[*RType]*marshalFields, len(cachedFields)+1)

			for typeKey, fieldsValues := range cachedFields {
				newFieldsMap[typeKey] = fieldsValues
			}

			newFieldsMap[v.Type] = fieldsInfo
			marshalerFieldCache.value.Store(newFieldsMap)
			marshalerFieldCache.mu.Unlock()
		}
	}

	// Mark Struct Start
	walker.WriteByte(curlOpen)

	first := true
	for _, f := range *fieldsInfo {
		// restoring to original value type, since it gets altered below
		valueType := v.Type

		fieldValue := v

		for _, idx := range f.indexes {
			if valueType.Kind() == Ptr {
				valueType = valueType.Deref()
				if fieldValue.IsNil() {
					continue
				}
				fieldValue = fieldValue.Deref()
			}
			valueType = valueType.convToStruct().fields[idx].Type
			fieldValue = fieldValue.Field(idx)
		}

		// omitted invalid
		if !fieldValue.IsValid() {
			continue
		}

		if fieldValue.isEmptyValue() {
			if f.willOmit {
				continue
			}
		}

		// Serialize Nulls Condition : 1. is a struct which has a name that starts with "Null" and must have "omitempty"
		if strings.HasPrefix(f.Type.Name(), "Null") && fieldValue.Kind() == Struct && f.willOmit {

			cachedFields2, _ := marshalerFieldCache.value.Load().(map[*RType]*marshalFields)
			subFields := cachedFields2[fieldValue.Type]
			if subFields == nil {

				// Compute fields without lock.
				// Might duplicate effort but won't hold other computations back.
				subFields = fieldValue.Type.getMarshalFields()
				if subFields != nil {
					// store them
					marshalerFieldCache.mu.Lock()
					cachedFields2, _ = marshalerFieldCache.value.Load().(map[*RType]*marshalFields)

					newFieldsMap := make(map[*RType]*marshalFields, len(cachedFields2)+1)

					for typeKey, fieldsValues := range cachedFields2 {
						newFieldsMap[typeKey] = fieldsValues
					}

					newFieldsMap[fieldValue.Type] = subFields
					marshalerFieldCache.value.Store(newFieldsMap)
					marshalerFieldCache.mu.Unlock()
				}
			}

			// 2. has exactly two fields : one boolean (called Valid) and one of a basic type (called as the basic type - e.g. "String")
			if len(*subFields) == 2 {
				var validField, carrierField Value
				for _, sf := range *subFields {
					if bytes.Equal(sf.name, []byte("Valid")) {
						//foundValid = true
						subFieldType := fieldValue.Type
						validField = fieldValue
						for _, idx2 := range sf.indexes {
							if subFieldType.Kind() == Ptr {
								subFieldType = subFieldType.Deref()
								if validField.IsNil() {
									validField = Value{}
									break
								}
							}
							subFieldType = subFieldType.convToStruct().fields[idx2].Type
							validField = validField.Field(idx2)
						}
					} else {
						subFieldType := fieldValue.Type
						carrierField = fieldValue
						for _, idx2 := range sf.indexes {
							if subFieldType.Kind() == Ptr {
								subFieldType = subFieldType.Deref()
								if carrierField.IsNil() {
									carrierField = Value{}
									break
								}
							}
							subFieldType = subFieldType.convToStruct().fields[idx2].Type
							carrierField = carrierField.Field(idx2)
						}

					}
				}

				if validField.IsValid() && carrierField.IsValid() {
					isTrue := *(*bool)(validField.Ptr)
					switch carrierField.Kind() {
					case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
						if isTrue {
							if !first {
								walker.WriteByte(comma)
							}
							walker.stringBytes(f.name)
							walker.WriteByte(colon)
							walker.opts.quoted = f.isBasic
							carrierField.write(walker)
						}
						continue
					default:
						if !isTrue {
							// if it's time, we're avoiding writing "null"
							_, isTime := carrierField.Interface().(time.Time)
							if isTime {
								continue
							}
						}
						// even if it's time, we're allowing Marshaler implementations (just to be able to format it)
					}
				}
			}
		}

		if !first {
			walker.WriteByte(comma)
		}
		walker.stringBytes(f.name)
		walker.WriteByte(colon)
		walker.opts.quoted = f.isBasic
		fieldValue.walk(walker)
		if first {
			first = false
		}

	}

	//Mark Struct End
	walker.WriteByte(curlClose)
}

func (v Value) mapEncoder(walker *encodeState) {
	if v.IsNil() {
		// Write "null"
		walker.Write(nullLiteral)
		return
	}

	// new feature : optional sorting for map keys (default false)
	if walker.opts.willSortMapKeys {
		// Mark Map Start
		walker.WriteByte(curlOpen)

		result := make([]KeyValuePair, len(v.MapKeys()))

		for idx, key := range v.MapKeys() {
			result[idx].value = key
			if err := result[idx].resolve(); err != nil {
				//error(&MarshalerError{key.Type(), err})
				panic("Error : " + err.Error() + " on " + key.Type.Name())
			}
		}
		sort.Slice(result, func(i, j int) bool {
			//The result will be -1 if result[i].keyName < result[j].keyName, and +1 if result[i].keyName > result[j].keyName.
			if bytes.Compare(result[i].keyName, result[j].keyName) == -1 {
				return true
			}
			return false
		})

		for i, key := range result {
			if i > 0 {
				walker.WriteByte(comma)
			}
			walker.stringBytes(key.keyName)
			walker.WriteByte(colon)
			v.MapIndex(key.value).walk(walker)
		}

		// Mark Map End
		walker.WriteByte(curlClose)
		return
	}

	// Mark Map Start
	walker.WriteByte(curlOpen)
	// checking and setting keyKind
	keyKind := Invalid
	for _, key := range v.MapKeys() {
		keyKind = key.Kind()
		switch keyKind {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, String:
		default:
			panic("Bad key kind!")
		}
		break
	}
	// default, unsorted map keys
	for i, key := range v.MapKeys() {
		var keyName []byte

		switch keyKind {
		case Int:
			keyName = FormatInt(int64(*(*int)(key.Ptr)))
		case Int8:
			keyName = FormatInt(int64(*(*int8)(key.Ptr)))
		case Int16:
			keyName = FormatInt(int64(*(*int16)(key.Ptr)))
		case Int32:
			keyName = FormatInt(int64(*(*int32)(key.Ptr)))
		case Int64:
			keyName = FormatInt(*(*int64)(key.Ptr))
		case Uint:
			keyName = FormatUint(uint64(*(*uint)(key.Ptr)))
		case Uint8:
			keyName = FormatUint(uint64(*(*uint8)(key.Ptr)))
		case Uint16:
			keyName = FormatUint(uint64(*(*uint16)(key.Ptr)))
		case Uint32:
			keyName = FormatUint(uint64(*(*uint32)(key.Ptr)))
		case Uint64:
			keyName = FormatUint(*(*uint64)(key.Ptr))
		case UintPtr:
			keyName = FormatUint(uint64(*(*uintptr)(key.Ptr)))
		case String:
			keyName = []byte(*(*string)(key.Ptr))
		}
		if i > 0 {
			walker.WriteByte(comma)
		}
		walker.stringBytes(keyName)
		walker.WriteByte(colon)
		v.MapIndex(key).walk(walker)
	}

	// Mark Map End
	walker.WriteByte(curlClose)
}

// getMarshalFields returns a list of fields that JSON should recognize for the given type.
// The algorithm is breadth-first search over the set of structs to include - the top struct and then any reachable anonymous structs.
func (t *RType) getMarshalFields() *marshalFields {
	// Embedded fields to explore at the current level and the next.
	fields := marshalFields{}
	next := marshalFields{{Type: t}}

	// Count of queued names for current level and the next.
	count := map[*RType]int{}
	nextCount := map[*RType]int{}

	// Types already visited at an earlier level.
	visited := map[*RType]bool{}

	// Fields found.
	var result marshalFields

	//println("Start.")
	for len(next) > 0 {
		fields, next = next, fields[:0]
		count, nextCount = nextCount, map[*RType]int{}

		for _, curField := range fields {
			if visited[curField.Type] {
				continue
			}
			visited[curField.Type] = true

			// Scan curField.Type for fields to include.
			curField.Type.Fields(func(Type *RType, fName []byte, fTag []byte, pack []byte, embedded, exported bool, offset uintptr, index int) {
				//structField := curField.Type.Field(i)
				isNonExported := len(pack) > 0
				if embedded {
					typ := Type
					if typ.Kind() == Ptr {
						typ = typ.Deref()
					}
					if isNonExported && typ.Kind() != Struct {
						// Ignore embedded fields of unexported non-struct types.
						//continue
						return
					}
					// Do not ignore embedded fields of unexported struct types since they may have exported fields.
				} else if isNonExported {
					// Ignore unexported non-embedded fields.
					//continue
					return
				}

				// start processing tags
				tag := GetTagNamed(fTag, jsonTagName) //structField.Tag.Get(jsonTagName)

				if idx := bytes.IndexByte(tag, minus); idx != -1 {
					//if bytes.Equal(tag, ignoreOption) {
					//continue
					return
				}
				//println(string(fName) + " TAG : " + string(tag) + " out of " + string(fTag))
				jsonName, opts := parseTagNew(tag)
				if !isValidTagNew(jsonName) {
					//println(string(fName) + " Invalid tag : `" + string(tag) + "` `" + string(jsonName) + "`")
					// TODO : signal error as warning or something
					jsonName = []byte{} //""
				}

				indexes := make([]int, len(curField.indexes)+1)
				copy(indexes, curField.indexes)
				indexes[len(curField.indexes)] = index

				fieldType := Type

				if len(fieldType.Name()) == 0 && fieldType.Kind() == Ptr {
					// Follow pointer.
					fieldType = fieldType.Deref()
				}

				fieldKind := fieldType.Kind()

				// Record found field and index sequence.
				if len(jsonName) > 0 || !embedded || fieldKind != Struct {
					tagged := len(jsonName) > 0
					if len(jsonName) == 0 {
						jsonName = fName
					}
					// Only strings, floats, integers, and booleans implies isBasic.
					isBasic := false
					if opts.Contains(stringTagOption) {
						switch fieldKind {
						case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, UintPtr, Float32, Float64, String:
							isBasic = true
						}
					}
					f := MarshalField{
						name:      jsonName,
						tag:       tagged,
						indexes:   indexes,
						Type:      fieldType,
						willOmit:  opts.Contains(omitTagOption),
						equalFold: foldFunc(jsonName),
						isBasic:   isBasic,
					}

					result = append(result, f)

					if count[curField.Type] > 1 {
						// If there were multiple instances, add a second, so that the annihilation code will see a duplicate.
						// It only cares about the distinction between 1 or 2, so don't bother generating any more copies.
						result = append(result, result[len(result)-1])
					}
					//continue
					return
				}

				// Record new anonymous struct to explore in next round.
				nextCount[fieldType]++
				if nextCount[fieldType] == 1 {
					f := MarshalField{name: []byte(fieldType.Name()), indexes: indexes, Type: fieldType}
					next = append(next, f)
				}
			})

		}
	}

	sort.Slice(result, func(i, j int) bool {
		x := result
		// sort field by name, breaking ties with depth, then breaking ties with "name came from json tag", then breaking ties with index sequence.
		if !bytes.Equal(x[i].name, x[j].name) {
			//The result will be -1 if x[i].name < x[j].name, and +1 if x[i].name > x[j].name.
			if bytes.Compare(x[i].name, x[j].name) == -1 {
				return true
			}
			return false
		}
		if len(x[i].indexes) != len(x[j].indexes) {
			return len(x[i].indexes) < len(x[j].indexes)
		}
		if x[i].tag != x[j].tag {
			return x[i].tag
		}
		return marshalFields(x).Less(i, j)
	})

	// Delete all fields that are hidden by the Go rules for embedded fields, except that fields with JSON tags are promoted.

	// The fields are sorted in primary order of name, secondary order of field index length. Loop over names; for each name, delete hidden fields by choosing the one dominant field that survives.
	out := result[:0]
	for advance, i := 0, 0; i < len(result); i += advance {
		// One iteration per name. Find the sequence of fields with the name of this first field.
		fi := result[i]
		name := fi.name
		for advance = 1; i+advance < len(result); advance++ {
			fj := result[i+advance]
			if !bytes.Equal(fj.name, name) {
				break
			}
		}
		if advance == 1 { // Only one field with this name
			out = append(out, fi)
			continue
		}
		dominant, ok := result[i : i+advance].dominantMarshalField()
		if ok {
			out = append(out, dominant)
		}
	}

	result = out
	sort.Sort(marshalFields(result))

	return &result
}
