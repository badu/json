package json

import (
	"unsafe"
)

//go:linkname IndexByte bytes.IndexByte
func IndexByte(s []byte, c byte) int

// resolveNameOff resolves a name offset from a base pointer.
//go:linkname resolveNameOff reflect.resolveNameOff
func resolveNameOff(ptrInModule unsafe.Pointer, off int32) unsafe.Pointer

// resolveTypeOff resolves an *Reflected offset from a base type.
//go:linkname resolveTypeOff reflect.resolveTypeOff
func resolveTypeOff(rtype unsafe.Pointer, off int32) unsafe.Pointer

// addReflectOff adds a pointer to the reflection lookup map in the runtime.
// It returns a new ID that can be used as a typeOff or textOffset, and will be resolved correctly. Implemented in the runtime package.
//go:linkname addReflectOff reflect.addReflectOff
func addReflectOff(ptr unsafe.Pointer) int32

// It returns a slice of the sections in each module,
// and a slice of *Reflected offsets in each module.
//
// The types in each module are sorted by string. That is, the first
// two linked types of the first module are:
//
//	d0 := sections[0]
//	t1 := (*rtype)(add(d0, offset[0][0]))
//	t2 := (*rtype)(add(d0, offset[0][1]))
//
// and
//
//	t1.String() < t2.String()
//
// Note that strings are not unique identifiers for types:
// there can be more than one with a given string.
// Only types we might want to look up are included:
// pointers, channels, maps, slices, and arrays.
//go:linkname typeLinks reflect.typelinks
func typeLinks() (sections []unsafe.Pointer, offset [][]int32)

//go:linkname unsafeNew reflect.unsafe_New
func unsafeNew(*RType) unsafe.Pointer

//go:linkname unsafeNewArray reflect.unsafe_NewArray
func unsafeNewArray(*RType, int) unsafe.Pointer

// typedmemmove copies a value of type t to dst from src.
//go:noescape
//go:linkname typedmemmove reflect.typedmemmove
func typedmemmove(t *RType, dst, src unsafe.Pointer)

// typedslicecopy copies a slice of elemType values from src to dst,
// returning the number of elements copied.
//go:noescape
//go:linkname typedslicecopy reflect.typedslicecopy
func typedslicecopy(elemType *RType, dst, src sliceHeader) int

//go:linkname ifaceE2I reflect.ifaceE2I
func ifaceE2I(t *RType, src interface{}, dst unsafe.Pointer)

/////////////////// Maps ////////////

//go:noescape
//go:linkname mapassign reflect.mapassign
func mapassign(t *RType, m unsafe.Pointer, key, val unsafe.Pointer)

// m escapes into the return value, but the caller of mapiterinit
// doesn't let the return value escape.
//go:noescape
//go:linkname mapiterinit reflect.mapiterinit
func mapiterinit(t *RType, m unsafe.Pointer) unsafe.Pointer

//go:noescape
//go:linkname mapaccess reflect.mapaccess
func mapaccess(t *RType, m unsafe.Pointer, key unsafe.Pointer) (val unsafe.Pointer)

//go:linkname makemap reflect.makemap
func makemap(t *RType, cap int) (m unsafe.Pointer)

//go:noescape
//go:linkname mapiterkey reflect.mapiterkey
func mapiterkey(it unsafe.Pointer) (key unsafe.Pointer)

//go:noescape
//go:linkname mapiternext reflect.mapiternext
func mapiternext(it unsafe.Pointer)

//go:noescape
//go:linkname maplen reflect.maplen
func maplen(m unsafe.Pointer) int
