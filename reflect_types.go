package json

import (
	"unsafe"
)

const (
	kindWidthFlag = 5 // there are 27 kinds

	// The lowest bits are flag bits:
	stickyROFlag    Flag = 1 << 5 // obtained via unexported not embedded field, so read-only
	embedROFlag     Flag = 1 << 6 // obtained via unexported embedded field, so read-only
	pointerFlag     Flag = 1 << 7 // value holds a pointer to the data
	addressableFlag Flag = 1 << 8 // v.CanAddr is true (implies pointerFlag)
	methodFlag      Flag = 1 << 9 // v is a method value
	// The next five bits give the Kind of the value.
	// This repeats Type.Kind() except for method values.
	kindMaskFlag Flag = 1<<kindWidthFlag - 1
	exportFlag   Flag = stickyROFlag | embedROFlag

	kindDirectIface = 1 << 5
	kindNoPointers  = 1 << 7
	kindMask        = (1 << 5) - 1
	// hasExtraInfoFlag means that there is a pointer, *info, just beyond the outer type structure.
	//
	// For example, if t.Kind() == Struct and t.extraTypeFlag&hasExtraInfoFlag != 0,
	// then t has info data and it can be accessed as:
	//
	// 	type tUncommon struct {
	// 		structType
	// 		u info
	// 	}
	// 	u := &(*tUncommon)(unsafe.Pointer(t)).u
	hasExtraInfoFlag uint8 = 1 << 0

	// hasExtraStarFlag means the name in the str field has an
	// extraneous '*' prefix. This is because for most types T in
	// a program, the type *T also exists and reusing the str data
	// saves binary size.
	hasExtraStarFlag uint8 = 1 << 1

	// hasNameFlag means the type has a name.
	hasNameFlag uint8 = 1 << 2

	PtrSize      = 4 << (^uintptr(0) >> 63) // unsafe.Sizeof(uintptr(0)) but an ideal const
	star    byte = '*'
)

const (
	Invalid       Kind = iota // 0
	Bool                      // 1
	Int                       // 2
	Int8                      // 3
	Int16                     // 4
	Int32                     // 5
	Int64                     // 6
	Uint                      // 7
	Uint8                     // 8
	Uint16                    // 9
	Uint32                    // 10
	Uint64                    // 11
	UintPtr                   // 12
	Float32                   // 13
	Float64                   // 14
	Complex64                 // 15
	Complex128                // 16
	Array                     // 17
	Chan                      // 18
	Func                      // 19
	Interface                 // 20
	Map                       // 21
	Ptr                       // 22
	Slice                     // 23
	String                    // 24
	Struct                    // 25
	UnsafePointer             // 26
)

var (
	kindNames = []string{
		Invalid:       "invalid",
		Bool:          "bool",
		Int:           "int",
		Int8:          "int8",
		Int16:         "int16",
		Int32:         "int32",
		Int64:         "int64",
		Uint:          "uint",
		Uint8:         "uint8",
		Uint16:        "uint16",
		Uint32:        "uint32",
		Uint64:        "uint64",
		UintPtr:       "uintptr",
		Float32:       "float32",
		Float64:       "float64",
		Complex64:     "complex64",
		Complex128:    "complex128",
		Array:         "array",
		Chan:          "chan",
		Func:          "func",
		Interface:     "interface",
		Map:           "map",
		Ptr:           "ptr",
		Slice:         "slice",
		String:        "string",
		Struct:        "struct",
		UnsafePointer: "unsafe.Pointer",
	}
)

type (
	// Aliases
	// -------
	ptr = unsafe.Pointer // alias, for readability

	Flag = uintptr
	// A Kind represents the specific kind of type that a Type represents. The zero Kind is not a valid kind.
	Kind = uint

	// Types
	// -----

	// Method on non-interface type
	// (COMPILER)
	method struct {
		nameOffset int32 // name of method
		typeOffset int32 // method type (without receiver)
		ifaceCall  int32 // fn used in interface call (one-word receiver)
		normCall   int32 // fn used for normal method call
	}
	// uncommonType is present only for types with names or methods
	// (if T is a named type, the uncommonTypes for T and *T have methods).
	// Using a pointer to this struct reduces the overall size required
	// to describe an unnamed type with no methods.
	// (COMPILER)
	uncommonType struct {
		pkgPath int32  // import path; empty for built-in types like int, string
		mCount  uint16 // number of methods
		_       uint16 // unused (future exported methods)
		mOffset uint32 // offset from this uncommontypeto [mCount]method
		_       uint32 // unused
	}
	// (COMPILER)
	uncommonStruct struct {
		structType
		u uncommonType
	}
	// (COMPILER)
	uncommonPtr struct {
		ptrType
		u uncommonType
	}
	// (COMPILER)
	uncommonSlice struct {
		sliceType
		u uncommonType
	}
	// (COMPILER)
	uncommonArray struct {
		arrayType
		u uncommonType
	}
	// (COMPILER)
	uncommonInterface struct {
		ifaceType
		u uncommonType
	}
	// (COMPILER)
	uncommonConcrete struct {
		RType
		u uncommonType
	}
	// arrayType represents a fixed array type.
	// (COMPILER)
	arrayType struct {
		RType     `reflect:"array"`
		ElemType  *RType // array element type
		SliceType *RType // slice type
		Len       uintptr
	}

	// ifaceMethod represents a method on an interface type
	// (COMPILER)
	ifaceMethod struct {
		nameOffset int32 // name of method
		typeOffset int32 // .(*MethodType) underneath
	}

	// ifaceType represents an interface type.
	// (COMPILER)
	ifaceType struct {
		RType   `reflect:"interface"`
		pkgPath name          // import path
		methods []ifaceMethod // sorted by hash
	}

	// mapType represents a map type.
	// (COMPILER)
	mapType struct {
		RType          `reflect:"map"`
		KeyType        *RType // map key type
		ElemType       *RType // map element (value) type
		bucket         *RType // x bucket structure
		header         *RType // x map header
		keySize        uint8  // size of key slot
		indirectKey    uint8  // store ptr to key instead of key itself
		valueSize      uint8  // size of value slot
		indirectValue  uint8  // store ptr to value instead of value itself
		bucketSize     uint16 // size of bucket
		reflexiveKey   bool   // true if k==k for all keys
		needsKeyUpdate bool   // true if we need to update key on an overwrite
	}

	// ptrType represents a pointer type.
	// (COMPILER)
	ptrType struct {
		RType `reflect:"ptr"`
		Type  *RType // pointer element (pointed at) type
	}

	// sliceType represents a slice type.
	// (COMPILER)
	sliceType struct {
		RType    `reflect:"slice"`
		ElemType *RType // slice element type
	}

	// Struct field (CORE)
	// (COMPILER)
	structField struct {
		name        name    // name is always non-empty
		Type        *RType  // type of field
		offsetEmbed uintptr // byte offset of field<<1 | isEmbed
	}

	// structType represents a struct type.
	// (COMPILER)
	structType struct {
		RType   `reflect:"struct"`
		pkgPath name
		fields  []structField // sorted by offset
	}

	// name is an encoded type name with optional extra data.
	//
	// The first byte is a bit field containing:
	//
	// 	1<<0 the name is exported
	// 	1<<1 tag data follows the name
	// 	1<<2 pkgPath nameOff follows the name and tag
	//
	// The next two bytes are the data length:
	//
	// 	 l := uint16(data[1])<<8 | uint16(data[2])
	//
	// Bytes [3:3+l] are the string data.
	//
	// If tag data follows then bytes 3+l and 3+l+1 are the tag length,
	// with the data following.
	//
	// If the import path follows, then 4 bytes at the end of
	// the data form a nameOff. The import path is only set for concrete
	// methods that are defined in a different package than their type.
	//
	// If a name starts with "*", then the exported bit represents
	// whether the pointed to type is exported.
	name struct {
		bytes *byte
	}

	// ifaceRtype is the header for an interface{} value.
	// (COMPILER)
	ifaceRtype struct {
		Type *RType
		word unsafe.Pointer
	}

	// stringHeader is a safe version of StringHeader used within this package.
	// (COMPILER)
	stringHeader struct {
		Data unsafe.Pointer
		Len  int
	}

	// sliceHeader is a safe version of SliceHeader used within this package.
	// (COMPILER)
	sliceHeader struct {
		Data unsafe.Pointer
		Len  int
		Cap  int
	}

	// Type is the representation of a Go type.
	//
	// Not all methods apply to all kinds of types. Restrictions,
	// if any, are noted in the documentation for each method.
	// Use the Kind method to find out the kind of type before
	// calling kind-specific methods. Calling a method
	// inappropriate to the kind of type causes a run-time panic.
	//
	// Type values are comparable, such as with the == operator,
	// so they can be used as map keys.
	// Two Type values are equal if they represent identical types.

	// Type is the common implementation of most values.
	// It is embedded in other, public struct types, but always with a unique tag like `reflect:"array"` or `reflect:"ptr"` so that code cannot convert from, say, *arrayType to *ptrType.
	//
	// Type must be kept in sync with ../runtime/type.go:/^type._type.
	// (COMPILER)
	RType struct {
		size          uintptr
		ptrData       uintptr // number of bytes in the type that can contain pointers : ignored
		hash          uint32  // hash of type; avoids computation in hash tables
		extraTypeFlag uint8   // extra type information flags
		align         uint8   // alignment of variable with this type
		_             uint8   // alignment of struct field with this type : ignored
		kind          uint8   // enumeration for C
		_             *struct {
			// algorithm table : ignored
			hash  func(unsafe.Pointer, uintptr) uintptr     // function for hashing objects of this type (ptr to object, seed) -> hash
			equal func(unsafe.Pointer, unsafe.Pointer) bool // function for comparing objects of this type (ptr to object A, ptr to object B) -> ==?
		}
		_         *byte // garbage collection data : ignored
		str       int32 // string form
		ptrToThis int32 // type for pointer to this type, may be zero
	}

	// Value is the reflection interface to a Go value.
	//
	// Not all methods apply to all kinds of values. Restrictions,
	// if any, are noted in the documentation for each method.
	// Use the Kind method to find out the kind of value before
	// calling kind-specific methods. Calling a method
	// inappropriate to the kind of type causes a run time panic.
	//
	// The zero Value represents no value.
	// Its IsValid method returns false, its Kind method returns Invalid,
	// its String method returns "<invalid Value>", and all other methods panic.
	// Most functions and methods never return an invalid value.
	// If one does, its documentation states the conditions explicitly.
	//
	// A Value can be used concurrently by multiple goroutines provided that
	// the underlying Go value can be used concurrently for the equivalent
	// direct operations.
	//
	// To compare two Values, compare the results of the Interface method.
	// Using == on two Values does not compare the underlying values
	// they represent.
	Value struct {
		Type *RType         // Type holds the type of the value represented by a Value.
		Ptr  unsafe.Pointer // Pointer-valued data or, if pointerFlag is set, pointer to data. Valid when either pointerFlag is set or Type.pointers() is true.
		Flag
		// A method value represents a curried method invocation
		// like r.Read for some receiver r. The Type+val+flag bits describe
		// the receiver r, but the flag's Kind bits say Func (methods are
		// functions), and the top bits of the flag give the method number
		// in r's type's method table.
	}
)
