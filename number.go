/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

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
