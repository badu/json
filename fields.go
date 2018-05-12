/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

func (m marshalFields) Len() int { return len(m) }

func (m marshalFields) Swap(i, j int) { m[i], m[j] = m[j], m[i] }

func (m marshalFields) Less(i, j int) bool {
	for k, xik := range m[i].indexes {
		if k >= len(m[j].indexes) {
			return false
		}
		if xik != m[j].indexes[k] {
			return xik < m[j].indexes[k]
		}
	}
	return len(m[i].indexes) < len(m[j].indexes)
}

func (m marshalFields) dominantMarshalField() (MarshalField, bool) {
	// The fields are sorted in increasing index-length order. The winner must therefore be one with the shortest index length. Drop all longer entries, which is easy: just truncate the slice.
	length := len(m[0].indexes)
	tagged := -1 // Index of first tagged field.
	for i, f := range m {
		if len(f.indexes) > length {
			m = m[:i]
			break
		}
		if f.tag {
			if tagged >= 0 {
				// Multiple tagged fields at the same level: conflict. Return no field.
				return MarshalField{}, false
			}
			tagged = i
		}
	}
	if tagged >= 0 {
		return m[tagged], true
	}
	// All remaining fields have the same length. If there's more than one, we have a conflict (two fields named "X" at the same level) and we return no field.
	if len(m) > 1 {
		return MarshalField{}, false
	}
	return m[0], true
}

// String returns the literal text of the number.
func (n Number) String() string { return string(n) }

// Float64 returns the number as a float64.
func (n Number) Float64() (float64, error) {
	return Atof64([]byte(n))
}

// Int64 returns the number as an int64.
func (n Number) Int64() (int64, error) {
	return IntParse([]byte(n))
}

func (w *KeyValuePair) resolve() error {
	switch w.value.Kind() {
	case String:
		// TODO : do this different
		w.keyName = []byte(*(*string)(w.value.Ptr))
	case Int:
		w.keyName = FormatInt(int64(*(*int)(w.value.Ptr)))
	case Int8:
		w.keyName = FormatInt(int64(*(*int8)(w.value.Ptr)))
	case Int16:
		w.keyName = FormatInt(int64(*(*int16)(w.value.Ptr)))
	case Int32:
		w.keyName = FormatInt(int64(*(*int32)(w.value.Ptr)))
	case Int64:
		w.keyName = FormatInt(*(*int64)(w.value.Ptr))
	case Uint:
		w.keyName = FormatUint(uint64(*(*uint)(w.value.Ptr)))
	case Uint8:
		w.keyName = FormatUint(uint64(*(*uint8)(w.value.Ptr)))
	case Uint16:
		w.keyName = FormatUint(uint64(*(*uint16)(w.value.Ptr)))
	case Uint32:
		w.keyName = FormatUint(uint64(*(*uint32)(w.value.Ptr)))
	case Uint64:
		w.keyName = FormatUint(*(*uint64)(w.value.Ptr))
	case UintPtr:
		w.keyName = FormatUint(uint64(*(*uintptr)(w.value.Ptr)))

	default:
		panic("unexpected map key type")
	}
	return nil
}
