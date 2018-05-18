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

func dominantMarshalField(m marshalFields) (MarshalField, bool) {
	// The fields are sorted in increasing index-length order. The winner must therefore be one with the shortest index length. Drop all longer entries, which is easy: just truncate the slice.
	length := len(m[0].indexes)
	tagged := -1 // Index of first tagged field.
	for i, f := range m {
		if len(f.indexes) > length {
			m = m[:i]
			break
		}
		if f.hasValidTag {
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
