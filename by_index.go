/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

func (x byIndex) Len() int { return len(x) }

func (x byIndex) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (x byIndex) Less(i, j int) bool {
	for k, xik := range x[i].indexes {
		if k >= len(x[j].indexes) {
			return false
		}
		if xik != x[j].indexes[k] {
			return xik < x[j].indexes[k]
		}
	}
	return len(x[i].indexes) < len(x[j].indexes)
}
