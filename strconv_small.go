/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"errors"
	"math"
	"unicode/utf8"
)

const (
	fnParseFloat = "ParseFloat"
	fnParseUint  = "UintParse"
	fnParseInt   = "IntParse"

	maxUint64 = 1<<64 - 1

	// Maximum shift that we can do in one pass without overflow.
	// A uint has 32 or 64 bits, and we have to be able to accommodate 9<<k.
	uintSize = 32 << (^uint(0) >> 63)

	maxShift = uintSize - 4
	// Powers of ten taken from double-conversion library.
	// http://code.google.com/p/double-conversion/

	firstPowerOfTen = -348
	stepPowerOfTen  = 8

	nSmalls = 100

	smallsString = "00010203040506070809" +
		"10111213141516171819" +
		"20212223242526272829" +
		"30313233343536373839" +
		"40414243444546474849" +
		"50515253545556575859" +
		"60616263646566676869" +
		"70717273747576777879" +
		"80818283848586878889" +
		"90919293949596979899"

	host32bit = ^uint(0)>>32 == 0
)

type (
	// A NumError records a failed conversion.
	NumError struct {
		Func string // the failing function (ParseBool, ParseInt, ParseUint, ParseFloat)
		Num  string // the input
		Err  error  // the reason the conversion failed (e.g. ErrRange, ErrSyntax, etc.)
	}

	decimal struct {
		d     [800]byte // digits, big-endian representation
		nd    int       // number of digits used
		dp    int       // decimal point
		neg   bool      // negative flag
		trunc bool      // discarded nonzero digits beyond d[:nd]
	}

	leftCheat struct {
		delta  int    // number of new digits
		cutoff []byte // minus one digit if original < a.
	}

	// An extFloat represents an extended floating-point number, with more
	// precision than a float64. It does not try to save bits: the
	// number represented by the structure is mant*(2^exp), with a negative
	// sign if neg is true.
	extFloat struct {
		mant uint64
		exp  int
		neg  bool
	}

	// TODO: move elsewhere?
	floatInfo struct {
		mantbits uint
		expbits  uint
		bias     int
	}

	decimalSlice struct {
		d      []byte
		nd, dp int
		neg    bool
	}
)

var (
	NaN           = []byte("NaN")
	MinusInf      = []byte("-Inf")
	PlusInf       = []byte("+Inf")
	plusInf       = []byte("+inf")
	plusInfinity  = []byte("-infinity")
	minusInf      = []byte("-inf")
	minusInfinity = []byte("-infinity")
	inf           = []byte("inf")
	infinity      = []byte("infinity")
	// decimal power of ten to binary power of two.
	powTab = []int{1, 3, 6, 9, 13, 16, 19, 23, 26}

	// Exact powers of 10.
	float64pow10 = []float64{
		1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
		1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
		1e20, 1e21, 1e22,
	}

	float32pow10 = []float32{1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10}
	// ErrRange indicates that a value is out of range for the target type.
	ErrRange = errors.New("value out of range")

	// ErrSyntax indicates that a value does not have the right syntax for the target type.
	ErrSyntax = errors.New("invalid syntax")

	leftCheats = []leftCheat{
		// Leading digits of 1/2^i = 5^i.
		// 5^23 is not an exact 64-bit floating point number,
		// so have to use bc for the math.
		// Go up to 60 to be large enough for 32bit and 64bit platforms.
		/*
			seq 60 | sed 's/^/5^/' | bc |
			awk 'BEGIN{ print "\t{ 0, \"\" }," }
			{
				log2 = log(2)/log(10)
				printf("\t{ %d, \"%s\" },\t// * %d\n",
					int(log2*NR+1), $0, 2**NR)
			}'
		*/
		{0, []byte("")},
		{1, []byte("5")},                                           // * 2
		{1, []byte("25")},                                          // * 4
		{1, []byte("125")},                                         // * 8
		{2, []byte("625")},                                         // * 16
		{2, []byte("3125")},                                        // * 32
		{2, []byte("15625")},                                       // * 64
		{3, []byte("78125")},                                       // * 128
		{3, []byte("390625")},                                      // * 256
		{3, []byte("1953125")},                                     // * 512
		{4, []byte("9765625")},                                     // * 1024
		{4, []byte("48828125")},                                    // * 2048
		{4, []byte("244140625")},                                   // * 4096
		{4, []byte("1220703125")},                                  // * 8192
		{5, []byte("6103515625")},                                  // * 16384
		{5, []byte("30517578125")},                                 // * 32768
		{5, []byte("152587890625")},                                // * 65536
		{6, []byte("762939453125")},                                // * 131072
		{6, []byte("3814697265625")},                               // * 262144
		{6, []byte("19073486328125")},                              // * 524288
		{7, []byte("95367431640625")},                              // * 1048576
		{7, []byte("476837158203125")},                             // * 2097152
		{7, []byte("2384185791015625")},                            // * 4194304
		{7, []byte("11920928955078125")},                           // * 8388608
		{8, []byte("59604644775390625")},                           // * 16777216
		{8, []byte("298023223876953125")},                          // * 33554432
		{8, []byte("1490116119384765625")},                         // * 67108864
		{9, []byte("7450580596923828125")},                         // * 134217728
		{9, []byte("37252902984619140625")},                        // * 268435456
		{9, []byte("186264514923095703125")},                       // * 536870912
		{10, []byte("931322574615478515625")},                      // * 1073741824
		{10, []byte("4656612873077392578125")},                     // * 2147483648
		{10, []byte("23283064365386962890625")},                    // * 4294967296
		{10, []byte("116415321826934814453125")},                   // * 8589934592
		{11, []byte("582076609134674072265625")},                   // * 17179869184
		{11, []byte("2910383045673370361328125")},                  // * 34359738368
		{11, []byte("14551915228366851806640625")},                 // * 68719476736
		{12, []byte("72759576141834259033203125")},                 // * 137438953472
		{12, []byte("363797880709171295166015625")},                // * 274877906944
		{12, []byte("1818989403545856475830078125")},               // * 549755813888
		{13, []byte("9094947017729282379150390625")},               // * 1099511627776
		{13, []byte("45474735088646411895751953125")},              // * 2199023255552
		{13, []byte("227373675443232059478759765625")},             // * 4398046511104
		{13, []byte("1136868377216160297393798828125")},            // * 8796093022208
		{14, []byte("5684341886080801486968994140625")},            // * 17592186044416
		{14, []byte("28421709430404007434844970703125")},           // * 35184372088832
		{14, []byte("142108547152020037174224853515625")},          // * 70368744177664
		{15, []byte("710542735760100185871124267578125")},          // * 140737488355328
		{15, []byte("3552713678800500929355621337890625")},         // * 281474976710656
		{15, []byte("17763568394002504646778106689453125")},        // * 562949953421312
		{16, []byte("88817841970012523233890533447265625")},        // * 1125899906842624
		{16, []byte("444089209850062616169452667236328125")},       // * 2251799813685248
		{16, []byte("2220446049250313080847263336181640625")},      // * 4503599627370496
		{16, []byte("11102230246251565404236316680908203125")},     // * 9007199254740992
		{17, []byte("55511151231257827021181583404541015625")},     // * 18014398509481984
		{17, []byte("277555756156289135105907917022705078125")},    // * 36028797018963968
		{17, []byte("1387778780781445675529539585113525390625")},   // * 72057594037927936
		{18, []byte("6938893903907228377647697925567626953125")},   // * 144115188075855872
		{18, []byte("34694469519536141888238489627838134765625")},  // * 288230376151711744
		{18, []byte("173472347597680709441192448139190673828125")}, // * 576460752303423488
		{19, []byte("867361737988403547205962240695953369140625")}, // * 1152921504606846976
	}

	smallPowersOfTen = [...]extFloat{
		{1 << 63, -63, false},        // 1
		{0xa << 60, -60, false},      // 1e1
		{0x64 << 57, -57, false},     // 1e2
		{0x3e8 << 54, -54, false},    // 1e3
		{0x2710 << 50, -50, false},   // 1e4
		{0x186a0 << 47, -47, false},  // 1e5
		{0xf4240 << 44, -44, false},  // 1e6
		{0x989680 << 40, -40, false}, // 1e7
	}

	powersOfTen = [...]extFloat{
		{0xfa8fd5a0081c0288, -1220, false}, // 10^-348
		{0xbaaee17fa23ebf76, -1193, false}, // 10^-340
		{0x8b16fb203055ac76, -1166, false}, // 10^-332
		{0xcf42894a5dce35ea, -1140, false}, // 10^-324
		{0x9a6bb0aa55653b2d, -1113, false}, // 10^-316
		{0xe61acf033d1a45df, -1087, false}, // 10^-308
		{0xab70fe17c79ac6ca, -1060, false}, // 10^-300
		{0xff77b1fcbebcdc4f, -1034, false}, // 10^-292
		{0xbe5691ef416bd60c, -1007, false}, // 10^-284
		{0x8dd01fad907ffc3c, -980, false},  // 10^-276
		{0xd3515c2831559a83, -954, false},  // 10^-268
		{0x9d71ac8fada6c9b5, -927, false},  // 10^-260
		{0xea9c227723ee8bcb, -901, false},  // 10^-252
		{0xaecc49914078536d, -874, false},  // 10^-244
		{0x823c12795db6ce57, -847, false},  // 10^-236
		{0xc21094364dfb5637, -821, false},  // 10^-228
		{0x9096ea6f3848984f, -794, false},  // 10^-220
		{0xd77485cb25823ac7, -768, false},  // 10^-212
		{0xa086cfcd97bf97f4, -741, false},  // 10^-204
		{0xef340a98172aace5, -715, false},  // 10^-196
		{0xb23867fb2a35b28e, -688, false},  // 10^-188
		{0x84c8d4dfd2c63f3b, -661, false},  // 10^-180
		{0xc5dd44271ad3cdba, -635, false},  // 10^-172
		{0x936b9fcebb25c996, -608, false},  // 10^-164
		{0xdbac6c247d62a584, -582, false},  // 10^-156
		{0xa3ab66580d5fdaf6, -555, false},  // 10^-148
		{0xf3e2f893dec3f126, -529, false},  // 10^-140
		{0xb5b5ada8aaff80b8, -502, false},  // 10^-132
		{0x87625f056c7c4a8b, -475, false},  // 10^-124
		{0xc9bcff6034c13053, -449, false},  // 10^-116
		{0x964e858c91ba2655, -422, false},  // 10^-108
		{0xdff9772470297ebd, -396, false},  // 10^-100
		{0xa6dfbd9fb8e5b88f, -369, false},  // 10^-92
		{0xf8a95fcf88747d94, -343, false},  // 10^-84
		{0xb94470938fa89bcf, -316, false},  // 10^-76
		{0x8a08f0f8bf0f156b, -289, false},  // 10^-68
		{0xcdb02555653131b6, -263, false},  // 10^-60
		{0x993fe2c6d07b7fac, -236, false},  // 10^-52
		{0xe45c10c42a2b3b06, -210, false},  // 10^-44
		{0xaa242499697392d3, -183, false},  // 10^-36
		{0xfd87b5f28300ca0e, -157, false},  // 10^-28
		{0xbce5086492111aeb, -130, false},  // 10^-20
		{0x8cbccc096f5088cc, -103, false},  // 10^-12
		{0xd1b71758e219652c, -77, false},   // 10^-4
		{0x9c40000000000000, -50, false},   // 10^4
		{0xe8d4a51000000000, -24, false},   // 10^12
		{0xad78ebc5ac620000, 3, false},     // 10^20
		{0x813f3978f8940984, 30, false},    // 10^28
		{0xc097ce7bc90715b3, 56, false},    // 10^36
		{0x8f7e32ce7bea5c70, 83, false},    // 10^44
		{0xd5d238a4abe98068, 109, false},   // 10^52
		{0x9f4f2726179a2245, 136, false},   // 10^60
		{0xed63a231d4c4fb27, 162, false},   // 10^68
		{0xb0de65388cc8ada8, 189, false},   // 10^76
		{0x83c7088e1aab65db, 216, false},   // 10^84
		{0xc45d1df942711d9a, 242, false},   // 10^92
		{0x924d692ca61be758, 269, false},   // 10^100
		{0xda01ee641a708dea, 295, false},   // 10^108
		{0xa26da3999aef774a, 322, false},   // 10^116
		{0xf209787bb47d6b85, 348, false},   // 10^124
		{0xb454e4a179dd1877, 375, false},   // 10^132
		{0x865b86925b9bc5c2, 402, false},   // 10^140
		{0xc83553c5c8965d3d, 428, false},   // 10^148
		{0x952ab45cfa97a0b3, 455, false},   // 10^156
		{0xde469fbd99a05fe3, 481, false},   // 10^164
		{0xa59bc234db398c25, 508, false},   // 10^172
		{0xf6c69a72a3989f5c, 534, false},   // 10^180
		{0xb7dcbf5354e9bece, 561, false},   // 10^188
		{0x88fcf317f22241e2, 588, false},   // 10^196
		{0xcc20ce9bd35c78a5, 614, false},   // 10^204
		{0x98165af37b2153df, 641, false},   // 10^212
		{0xe2a0b5dc971f303a, 667, false},   // 10^220
		{0xa8d9d1535ce3b396, 694, false},   // 10^228
		{0xfb9b7cd9a4a7443c, 720, false},   // 10^236
		{0xbb764c4ca7a44410, 747, false},   // 10^244
		{0x8bab8eefb6409c1a, 774, false},   // 10^252
		{0xd01fef10a657842c, 800, false},   // 10^260
		{0x9b10a4e5e9913129, 827, false},   // 10^268
		{0xe7109bfba19c0c9d, 853, false},   // 10^276
		{0xac2820d9623bf429, 880, false},   // 10^284
		{0x80444b5e7aa7cf85, 907, false},   // 10^292
		{0xbf21e44003acdd2d, 933, false},   // 10^300
		{0x8e679c2f5e44ff8f, 960, false},   // 10^308
		{0xd433179d9c8cb841, 986, false},   // 10^316
		{0x9e19db92b4e31ba9, 1013, false},  // 10^324
		{0xeb96bf6ebadf77d9, 1039, false},  // 10^332
		{0xaf87023b9bf0ee6b, 1066, false},  // 10^340
	}

	uint64pow10 = [...]uint64{
		1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
		1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
	}
	float32info = floatInfo{23, 8, -127}

	float64info = floatInfo{52, 11, -1023}
)

// contains reports whether the string contains the byte c.
func contains(s []byte, c byte) bool {
	for i := 0; i < len(s); i++ {
		if s[i] == c {
			return true
		}
	}
	return false
}

func unhex(b byte) (v rune, ok bool) {
	c := rune(b)
	switch {
	case rune(zero) <= c && c <= rune(nine):
		return c - rune(zero), true
	case rune(aChr) <= c && c <= rune(fChr):
		return c - rune(aChr) + 10, true
	case rune(bigAChr) <= c && c <= rune(bigFChr):
		return c - rune(bigAChr) + 10, true
	}
	return
}

// UnquoteChar decodes the first character or byte in the escaped string
// or character literal represented by the string s.
// It returns four values:
//
//	1) value, the decoded Unicode code point or byte value;
//	2) multibyte, a boolean indicating whether the decoded character requires a multibyte UTF-8 representation;
//	3) tail, the remainder of the string after the character; and
//	4) an error that will be nil if the character is syntactically valid.
//
// The second argument, quote, specifies the type of literal being parsed
// and therefore which escaped quote character is permitted.
// If set to a single quote, it permits the sequence \' and disallows unescaped '.
// If set to a double quote, it permits \" and disallows unescaped ".
// If set to zero, it does not permit either escape and allows both quote characters to appear unescaped.
func UnquoteChar(s []byte, quoter byte) (value rune, multibyte bool, tail []byte, err error) {
	// easy cases
	switch c := s[0]; {
	case c == quoter && (quoter == sQuote || quoter == quote):
		err = ErrSyntax
		return
	case c >= utf8.RuneSelf:
		r, size := utf8.DecodeRune(s)
		return r, true, s[size:], nil
	case c != backSlash:
		return rune(s[0]), false, s[1:], nil
	}

	// hard case: c is backslash
	if len(s) <= 1 {
		err = ErrSyntax
		return
	}
	c := s[1]
	s = s[2:]

	switch c {
	case aChr:
		value = '\a'
	case 'b':
		value = '\b'
	case fChr:
		value = '\f'
	case nChr:
		value = rune(newLine)
	case 'r':
		value = rune(retChar)
	case 't':
		value = '\t'
	case 'v':
		value = '\v'
	case 'x', 'u', 'U':
		n := 0
		switch c {
		case 'x':
			n = 2
		case 'u':
			n = 4
		case 'U':
			n = 8
		}
		var v rune
		if len(s) < n {
			err = ErrSyntax
			return
		}
		for j := 0; j < n; j++ {
			x, ok := unhex(s[j])
			if !ok {
				err = ErrSyntax
				return
			}
			v = v<<4 | x
		}
		s = s[n:]
		if c == 'x' {
			// single-byte string, possibly not UTF-8
			value = v
			break
		}
		if v > utf8.MaxRune {
			err = ErrSyntax
			return
		}
		value = v
		multibyte = true
	case zero, one, two, three, four, five, six, seven:
		v := rune(c) - rune(zero)
		if len(s) < 2 {
			err = ErrSyntax
			return
		}
		for j := 0; j < 2; j++ { // one digit already; two more
			x := rune(s[j]) - rune(zero)
			if x < 0 || x > 7 {
				err = ErrSyntax
				return
			}
			v = (v << 3) | x
		}
		s = s[2:]
		if v > 255 {
			err = ErrSyntax
			return
		}
		value = v
	case backSlash:
		value = rune(backSlash)
	case sQuote, quote:
		if c != quoter {
			err = ErrSyntax
			return
		}
		value = rune(c)
	default:
		err = ErrSyntax
		return
	}
	tail = s
	return
}

//TODO : there is a version in util - named unquote
// Unquote interprets s as a single-quoted, double-quoted,
// or backquoted Go string literal, returning the string value
// that s quotes.  (If s is single-quoted, it would be a Go
// character literal; Unquote returns the corresponding
// one-character string.)
func Unquote(s []byte) ([]byte, error) {
	n := len(s)
	if n < 2 {
		return []byte{}, ErrSyntax
	}
	quoter := s[0]
	if quoter != s[n-1] {
		return []byte{}, ErrSyntax
	}
	s = s[1 : n-1]

	if quoter == '`' {
		if contains(s, '`') {
			return []byte{}, ErrSyntax
		}
		if contains(s, retChar) {
			// -1 because we know there is at least one \r to remove.
			buf := make([]byte, 0, len(s)-1)
			for i := 0; i < len(s); i++ {
				if s[i] != retChar {
					buf = append(buf, s[i])
				}
			}
			return buf, nil
		}
		return s, nil
	}
	if quoter != quote && quoter != sQuote {
		return []byte{}, ErrSyntax
	}
	if contains(s, newLine) {
		return []byte{}, ErrSyntax
	}

	// Is it trivial? Avoid allocation.
	if !contains(s, backSlash) && !contains(s, quoter) {
		switch quoter {
		case quote:
			return s, nil
		case sQuote:
			r, size := utf8.DecodeRune(s)
			if size == len(s) && (r != utf8.RuneError || size != 1) {
				return s, nil
			}
		}
	}

	var runeTmp [utf8.UTFMax]byte
	buf := make([]byte, 0, 3*len(s)/2) // Try to avoid more allocations.
	for len(s) > 0 {
		c, multibyte, ss, err := UnquoteChar(s, quoter)
		if err != nil {
			return []byte{}, err
		}
		s = ss
		if c < utf8.RuneSelf || !multibyte {
			buf = append(buf, byte(c))
		} else {
			n := utf8.EncodeRune(runeTmp[:], c)
			buf = append(buf, runeTmp[:n]...)
		}
		if quoter == sQuote && len(s) != 0 {
			// single-quoted must be single character
			return []byte{}, ErrSyntax
		}
	}
	return buf, nil
}

func Atof32(src []byte) (f float32, err error) {
	if val, ok := special(src); ok {
		return float32(val), nil
	}

	// Parse mantissa and exponent.
	mantissa, exp, neg, trunc, ok := readFloat(src)
	if ok {
		// Try pure floating-point arithmetic conversion.
		if !trunc {
			if f, ok := atof32exact(mantissa, exp, neg); ok {
				return f, nil
			}
		}
		// Try another fast path.
		ext := new(extFloat)
		if ok := ext.assignDecimal(mantissa, exp, neg, trunc, &float32info); ok {
			b, ovf := ext.floatBits(&float32info)
			f = math.Float32frombits(uint32(b))
			if ovf {
				err = rangeError(fnParseFloat, src)
			}
			return f, err
		}
	}

	var d decimal
	if !d.set(src) {
		return 0, syntaxError(fnParseFloat, src)
	}
	b, ovf := d.floatBits(&float32info)
	f = math.Float32frombits(uint32(b))
	if ovf {
		err = rangeError(fnParseFloat, src)
	}
	return f, err
}

func Atof64(src []byte) (f float64, err error) {
	if val, ok := special(src); ok {
		return val, nil
	}

	// Parse mantissa and exponent.
	mantissa, exp, neg, trunc, ok := readFloat(src)
	if ok {
		// Try pure floating-point arithmetic conversion.
		if !trunc {
			if f, ok := atof64exact(mantissa, exp, neg); ok {
				return f, nil
			}
		}
		// Try another fast path.
		ext := new(extFloat)
		if ok := ext.assignDecimal(mantissa, exp, neg, trunc, &float64info); ok {
			b, ovf := ext.floatBits(&float64info)
			f = math.Float64frombits(b)
			if ovf {
				err = rangeError(fnParseFloat, src)
			}
			return f, err
		}
	}

	var d decimal
	if !d.set(src) {
		return 0, syntaxError(fnParseFloat, src)
	}
	b, ovf := d.floatBits(&float64info)
	f = math.Float64frombits(b)
	if ovf {
		err = rangeError(fnParseFloat, src)
	}
	return f, err
}

func IntParse(src []byte) (i int64, err error) {
	// Empty string bad.
	if len(src) == 0 {
		return 0, syntaxError(fnParseInt, []byte(src))
	}

	// Pick off leading sign.
	s0 := src
	neg := false
	if src[0] == plus {
		src = src[1:]
	} else if src[0] == minus {
		neg = true
		src = src[1:]
	}

	// Convert unsigned and check range.
	var un uint64
	un, err = UintParse(src)
	if err != nil && err.(*NumError).Err != ErrRange {
		err.(*NumError).Func = fnParseInt
		err.(*NumError).Num = string(s0)
		return 0, err
	}

	cutoff := uint64(1 << uint(64-1))
	if !neg && un >= cutoff {
		return int64(cutoff - 1), rangeError(fnParseInt, []byte(s0))
	}
	if neg && un > cutoff {
		return -int64(cutoff), rangeError(fnParseInt, []byte(s0))
	}
	n := int64(un)
	if neg {
		n = -n
	}
	return n, nil
}

func UintParse(src []byte) (uint64, error) {
	if len(src) == 0 {
		return 0, syntaxError(fnParseUint, []byte(src))
	}

	// Cutoff is the smallest number such that cutoff*10> maxUint64.
	// Use compile-time constants for common cases.
	cutoff := uint64(maxUint64/10 + 1)

	var n uint64
	for _, c := range []byte(src) {
		var d byte
		switch {
		case zero <= c && c <= nine:
			d = c - zero
		case aChr <= c && c <= zChr:
			d = c - aChr + 10
		case bigAChr <= c && c <= bigZChr:
			d = c - bigAChr + 10
		default:
			return 0, syntaxError(fnParseUint, src)
		}

		if d >= byte(10) {
			return 0, syntaxError(fnParseUint, src)
		}

		if n >= cutoff {
			// n*base overflows
			return maxUint64, rangeError(fnParseUint, src)
		}
		n *= uint64(10)

		n1 := n + uint64(d)
		if n1 < n || n1 > maxUint64 {
			// n+v overflows
			return maxUint64, rangeError(fnParseUint, src)
		}
		n = n1
	}

	return n, nil
}

func FastNumber(item []byte) (uint64, bool) {
	result := uint64(0)
	hasMinus := false
	leni := len(item) - 1
	for idx := leni; idx >= 0; idx-- {
		switch item[idx] {
		case minus:
			hasMinus = true
		default:
			// substract rune '0'
			digit := item[idx] ^ 48
			// no need to validate digit, because we were pre-validated to get here
			result += uint64(digit) * uint64pow10[leni-idx]
		}
	}
	return result, hasMinus
}

// FormatFloat converts the floating-point number f to a string,
// according to the format fmt and precision prec. It rounds the
// result assuming that the original was obtained from a floating-point
// value of bitSize bits (32 for float32, 64 for float64).
//
// The format fmt is one of
// 'b' (-ddddp±ddd, a binary exponent),
// 'e' (-d.dddde±dd, a decimal exponent),
// 'E' (-d.ddddE±dd, a decimal exponent),
// 'f' (-ddd.dddd, no exponent),
// 'g' ('e' for large exponents, 'f' otherwise), or
// 'G' ('E' for large exponents, 'f' otherwise).
//
// The precision prec controls the number of digits
// (excluding the exponent) printed by the 'e', 'E', 'f', 'g', and 'G' formats.
// For 'e', 'E', and 'f' it is the number of digits after the decimal point.
// For 'g' and 'G' it is the total number of digits.
// The special precision -1 uses the smallest number of digits
// necessary such that ParseFloat will return f exactly.
func FormatFloat(src float64, bitSize int) string {
	return string(genericFtoa(src, gChr, bitSize))
}

// makeFloatBytes appends the string form of the floating-point number f,
// as generated by FormatFloat, to dst and returns the extended buffer.
func makeFloatBytes(src float64, fmt byte, bitSize int) []byte {
	return genericFtoa(src, fmt, bitSize)
}

// FormatUint returns the string representation of i in the given base,
// for 2 <= base <= 36. The result uses the lower-case letters 'a' to 'z'
// for digit values >= 10.
func FormatUint(src uint64) []byte {
	if src < nSmalls {
		return small(int(src))
	}
	return formatBits(src, false)
}

// FormatInt returns the string representation of i in the given base,
// for 2 <= base <= 36. The result uses the lower-case letters 'a' to 'z'
// for digit values >= 10.
func FormatInt(src int64) []byte {
	if 0 <= src && src < nSmalls {
		return small(int(src))
	}
	return formatBits(uint64(src), src < 0)
}

func equalIgnoreCase(s1, s2 []byte) bool {
	if len(s1) != len(s2) {
		return false
	}
	for i := 0; i < len(s1); i++ {
		c1 := s1[i]
		if bigAChr <= c1 && c1 <= bigZChr {
			c1 += aChr - bigAChr
		}
		c2 := s2[i]
		if bigAChr <= c2 && c2 <= bigZChr {
			c2 += aChr - bigAChr
		}
		if c1 != c2 {
			return false
		}
	}
	return true
}

func special(src []byte) (f float64, ok bool) {
	if len(src) == 0 {
		return
	}
	switch src[0] {
	default:
		return
	case plus:
		if equalIgnoreCase(src, plusInf) || equalIgnoreCase(src, plusInfinity) {
			return math.Inf(1), true
		}
	case minus:
		if equalIgnoreCase(src, minusInf) || equalIgnoreCase(src, minusInfinity) {
			return math.Inf(-1), true
		}
	case nChr, bigNChr:
		if equalIgnoreCase(src, NaN) {
			return math.NaN(), true
		}
	case iChr, bigIChr:
		if equalIgnoreCase(src, inf) || equalIgnoreCase(src, infinity) {
			return math.Inf(1), true
		}
	}
	return
}

// readFloat reads a decimal mantissa and exponent from a float string representation.
// It sets ok to false if the number could not fit return types or is invalid.
func readFloat(src []byte) (mantissa uint64, exp int, neg, trunc, ok bool) {
	const uint64digits = 19
	i := 0

	// optional sign
	if i >= len(src) {
		return
	}
	switch {
	case src[i] == plus:
		i++
	case src[i] == minus:
		neg = true
		i++
	}

	// digits
	sawdot := false
	sawdigits := false
	nd := 0
	ndMant := 0
	dp := 0
	for ; i < len(src); i++ {
		switch c := src[i]; true {
		case c == period:
			if sawdot {
				return
			}
			sawdot = true
			dp = nd
			continue

		case zero <= c && c <= nine:
			sawdigits = true
			if c == zero && nd == 0 { // ignore leading zeros
				dp--
				continue
			}
			nd++
			if ndMant < uint64digits {
				mantissa *= 10
				mantissa += uint64(c - zero)
				ndMant++
			} else if src[i] != zero {
				trunc = true
			}
			continue
		}
		break
	}
	if !sawdigits {
		return
	}
	if !sawdot {
		dp = nd
	}

	// optional exponent moves decimal point.
	// if we read a very large, very long number,
	// just be sure to move the decimal point by
	// a lot (say, 100000).  it doesn't matter if it's
	// not the exact number.
	if i < len(src) && (src[i] == eChr || src[i] == bigEChr) {
		i++
		if i >= len(src) {
			return
		}
		esign := 1
		if src[i] == plus {
			i++
		} else if src[i] == minus {
			i++
			esign = -1
		}
		if i >= len(src) || src[i] < zero || src[i] > nine {
			return
		}
		e := 0
		for ; i < len(src) && zero <= src[i] && src[i] <= nine; i++ {
			if e < 10000 {
				e = e*10 + int(src[i]) - int(zero)
			}
		}
		dp += e * esign
	}

	if i != len(src) {
		return
	}

	if mantissa != 0 {
		exp = dp - ndMant
	}
	ok = true
	return

}

// If possible to convert decimal representation to 64-bit float f exactly,
// entirely in floating-point math, do so, avoiding the expense of decimalToFloatBits.
// Three common cases:
//	value is exact integer
//	value is exact integer * exact power of ten
//	value is exact integer / exact power of ten
// These all produce potentially inexact but correctly rounded answers.
func atof64exact(mantissa uint64, exp int, neg bool) (f float64, ok bool) {
	if mantissa>>float64info.mantbits != 0 {
		return
	}
	f = float64(mantissa)
	if neg {
		f = -f
	}
	switch {
	case exp == 0:
		// an integer.
		return f, true
		// Exact integers are <= 10^15.
		// Exact powers of ten are <= 10^22.
	case exp > 0 && exp <= 15+22: // int * 10^k
		// If exponent is big but number of digits is not,
		// can move a few zeros into the integer part.
		if exp > 22 {
			f *= float64pow10[exp-22]
			exp = 22
		}
		if f > 1e15 || f < -1e15 {
			// the exponent was really too large.
			return
		}
		return f * float64pow10[exp], true
	case exp < 0 && exp >= -22: // int / 10^k
		return f / float64pow10[-exp], true
	}
	return
}

// If possible to compute mantissa*10^exp to 32-bit float f exactly,
// entirely in floating-point math, do so, avoiding the machinery above.
func atof32exact(mantissa uint64, exp int, neg bool) (f float32, ok bool) {
	if mantissa>>float32info.mantbits != 0 {
		return
	}
	f = float32(mantissa)
	if neg {
		f = -f
	}
	switch {
	case exp == 0:
		return f, true
		// Exact integers are <= 10^7.
		// Exact powers of ten are <= 10^10.
	case exp > 0 && exp <= 7+10: // int * 10^k
		// If exponent is big but number of digits is not,
		// can move a few zeros into the integer part.
		if exp > 10 {
			f *= float32pow10[exp-10]
			exp = 10
		}
		if f > 1e7 || f < -1e7 {
			// the exponent was really too large.
			return
		}
		return f * float32pow10[exp], true
	case exp < 0 && exp >= -10: // int / 10^k
		return f / float32pow10[-exp], true
	}
	return
}

func syntaxError(fn string, str []byte) *NumError {
	return &NumError{fn, string(str), ErrSyntax}
}

func rangeError(fn string, str []byte) *NumError {
	return &NumError{fn, string(str), ErrRange}
}

// Is the leading prefix of b lexicographically less than s?
func prefixIsLessThan(b []byte, s []byte) bool {
	for i := 0; i < len(s); i++ {
		if i >= len(b) {
			return true
		}
		if b[i] != s[i] {
			return b[i] < s[i]
		}
	}
	return false
}

// If we chop a at nd digits, should we round up?
func shouldRoundUp(a *decimal, nd int) bool {
	if nd < 0 || nd >= a.nd {
		return false
	}
	if a.d[nd] == five && nd+1 == a.nd { // exactly halfway - round to even
		// if we truncated, a little higher than what's recorded - always round up
		if a.trunc {
			return true
		}
		return nd > 0 && (a.d[nd-1]-zero)%2 != 0
	}
	// not halfway - digit tells all
	return a.d[nd] >= five
}

// frexp10Many applies a common shift by a power of ten to a, b, c.
func frexp10Many(a, b, c *extFloat) (exp10 int) {
	exp10, i := c.frexp10()
	a.multiply(powersOfTen[i])
	b.multiply(powersOfTen[i])
	return
}

func genericFtoa(val float64, fmt byte, bitSize int) []byte {
	var bits uint64
	var fltInf *floatInfo
	switch bitSize {
	case 32:
		bits = uint64(math.Float32bits(float32(val)))
		fltInf = &float32info
	case 64:
		bits = math.Float64bits(val)
		fltInf = &float64info
	default:
		panic("strconv: illegal makeFloatBytes/FormatFloat bitSize")
	}

	neg := bits>>(fltInf.expbits+fltInf.mantbits) != 0
	exp := int(bits>>fltInf.mantbits) & (1<<fltInf.expbits - 1)
	mant := bits & (uint64(1)<<fltInf.mantbits - 1)

	switch exp {
	case 1<<fltInf.expbits - 1:
		// Inf, NaN
		var s []byte
		switch {
		case mant != 0:
			s = NaN
		case neg:
			s = MinusInf
		default:
			s = PlusInf
		}
		return s

	case 0:
		// denormalized
		exp++

	default:
		// add implicit top bit
		mant |= uint64(1) << fltInf.mantbits
	}
	exp += fltInf.bias

	var digs decimalSlice
	ok := false
	// Negative precision means "only as much as needed to be exact."

	// Try Grisu3 algorithm.
	f := new(extFloat)
	lower, upper := f.assignComputeBounds(mant, exp, neg, fltInf)
	var buf [32]byte
	digs.d = buf[:]
	ok = f.shortestDecimal(&digs, &lower, &upper)
	if !ok {
		return bigFtoa(-1, fmt, neg, mant, exp, fltInf)
	}

	prec := -1
	// Precision for shortest representation mode.
	switch fmt {
	case eChr:
		prec = max(digs.nd-1, 0)
	case fChr:
		prec = max(digs.nd-digs.dp, 0)
	case gChr:
		prec = digs.nd
	}

	if !ok {
		return bigFtoa(prec, fmt, neg, mant, exp, fltInf)
	}
	return formatDigits(neg, digs, prec, fmt)
}

// bigFtoa uses multiprecision computations to format a float.
func bigFtoa(prec int, fmt byte, neg bool, mant uint64, exp int, flt *floatInfo) []byte {
	d := new(decimal)
	d.assign(mant)
	d.shift(exp - int(flt.mantbits))
	var digs decimalSlice

	d.roundShortest(mant, exp, flt)
	digs = decimalSlice{d: d.d[:], nd: d.nd, dp: d.dp}
	// Precision for shortest representation mode.
	switch fmt {
	case eChr:
		prec = digs.nd - 1
	case fChr:
		prec = max(digs.nd-digs.dp, 0)
	case gChr:
		prec = digs.nd
	}

	return formatDigits(neg, digs, prec, fmt)
}

func formatDigits(neg bool, digs decimalSlice, prec int, fmt byte) []byte {
	switch fmt {
	case eChr:
		return fmtE(neg, digs, prec, fmt)
	case fChr:
		return fmtF(neg, digs, prec)
	case gChr:
		// trailing fractional zeros in 'e' form will be trimmed.
		eprec := prec
		if eprec > digs.nd && digs.nd >= digs.dp {
			eprec = digs.nd
		}
		// %e is used if the exponent from the conversion
		// is less than -4 or greater than or equal to the precision.
		// if precision was the shortest possible, use precision 6 for this decision.
		eprec = 6

		exp := digs.dp - 1
		if exp < -4 || exp >= eprec {
			if prec > digs.nd {
				prec = digs.nd
			}
			return fmtE(neg, digs, prec-1, fmt+eChr-gChr)
		}
		if prec > digs.dp {
			prec = digs.nd
		}
		return fmtF(neg, digs, max(prec-digs.dp, 0))
	}
	var dst []byte
	// unknown format
	return append(dst, '%', fmt)
}

// %e: -d.ddddde±dd
func fmtE(neg bool, d decimalSlice, prec int, fmt byte) []byte {
	dst := make([]byte, 0, 24)
	// sign
	if neg {
		dst = append(dst, minus)
	}

	// first digit
	ch := byte(zero)
	if d.nd != 0 {
		ch = d.d[0]
	}
	dst = append(dst, ch)

	// .moredigits
	if prec > 0 {
		dst = append(dst, period)
		i := 1
		m := min(d.nd, prec+1)
		if i < m {
			dst = append(dst, d.d[i:m]...)
			i = m
		}
		for ; i <= prec; i++ {
			dst = append(dst, zero)
		}
	}

	// e±
	dst = append(dst, fmt)
	exp := d.dp - 1
	if d.nd == 0 { // special case: 0 has exponent 0
		exp = 0
	}
	if exp < 0 {
		ch = minus
		exp = -exp
	} else {
		ch = plus
	}
	dst = append(dst, ch)

	// dd or ddd
	switch {
	case exp < 10:
		dst = append(dst, zero, byte(exp)+zero)
	case exp < 100:
		dst = append(dst, byte(exp/10)+zero, byte(exp%10)+zero)
	default:
		dst = append(dst, byte(exp/100)+zero, byte(exp/10)%10+zero, byte(exp%10)+zero)
	}

	return dst
}

// %f: -ddddddd.ddddd
func fmtF(neg bool, d decimalSlice, prec int) []byte {
	dst := make([]byte, 0, 24)
	// sign
	if neg {
		dst = append(dst, minus)
	}

	// integer, padded with zeros as needed.
	if d.dp > 0 {
		m := min(d.nd, d.dp)
		dst = append(dst, d.d[:m]...)
		for ; m < d.dp; m++ {
			dst = append(dst, zero)
		}
	} else {
		dst = append(dst, zero)
	}

	// fraction
	if prec > 0 {
		dst = append(dst, period)
		for i := 0; i < prec; i++ {
			ch := byte(zero)
			if j := d.dp + i; 0 <= j && j < d.nd {
				ch = d.d[j]
			}
			dst = append(dst, ch)
		}
	}

	return dst
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// small returns the string for an i with 0 <= i < nSmalls.
func small(i int) []byte {
	off := 0
	if i < 10 {
		off = 1
	}
	return []byte(smallsString[i*2+off : i*2+2])
}

// formatBits computes the string representation of u in base 10.
// If neg is set, u is treated as negative int64 value. If append_ is
// set, the string is appended to dst and the resulting byte slice is
// returned as the first result value; otherwise the string is returned
// as the second result value.
//
func formatBits(u uint64, neg bool) []byte {
	var a [64 + 1]byte // +1 for sign of 64bit value in base 2
	i := len(a)

	if neg {
		u = -u
	}

	// convert bits
	// We use uint values where we can because those will
	// fit into a single register even on a 32bit machine.

	// common case: use constants for / because
	// the compiler can optimize it into a multiply+shift

	if host32bit {
		// convert the lower digits using 32bit operations
		for u >= 1e9 {
			// Avoid using r = a%b in addition to q = a/b
			// since 64bit division and modulo operations
			// are calculated by runtime functions on 32bit machines.
			q := u / 1e9
			us := uint(u - q*1e9) // u % 1e9 fits into a uint
			for j := 4; j > 0; j-- {
				is := us % 100 * 2
				us /= 100
				i -= 2
				a[i+1] = smallsString[is+1]
				a[i+0] = smallsString[is+0]
			}

			// us < 10, since it contains the last digit
			// from the initial 9-digit us.
			i--
			a[i] = smallsString[us*2+1]

			u = q
		}
		// u < 1e9
	}

	// u guaranteed to fit into a uint
	us := uint(u)
	for us >= 100 {
		is := us % 100 * 2
		us /= 100
		i -= 2
		a[i+1] = smallsString[is+1]
		a[i+0] = smallsString[is+0]
	}

	// us < 100
	is := us * 2
	i--
	a[i] = smallsString[is+1]
	if us >= 10 {
		i--
		a[i] = smallsString[is]
	}

	// add sign, if any
	if neg {
		i--
		a[i] = minus
	}

	return a[i:]
}

// trim trailing zeros from number.
// (They are meaningless; the decimal point is tracked
// independent of the number of digits.)
func (d *decimal) trim() {
	for d.nd > 0 && d.d[d.nd-1] == zero {
		d.nd--
	}
	if d.nd == 0 {
		d.dp = 0
	}
}

// Binary shift right (/ 2) by k bits.  k <= maxShift to avoid overflow.
func (d *decimal) rightShift(k uint) {
	r := 0 // read pointer
	w := 0 // write pointer

	// Pick up enough leading digits to cover first shift.
	var n uint
	for ; n>>k == 0; r++ {
		if r >= d.nd {
			if n == 0 {
				// d == 0; shouldn't get here, but handle anyway.
				d.nd = 0
				return
			}
			for n>>k == 0 {
				n = n * 10
				r++
			}
			break
		}
		c := uint(d.d[r])
		n = n*10 + c - uint(zero)
	}
	d.dp -= r - 1

	var mask uint = (1 << k) - 1

	// Pick up d digit, put down d digit.
	for ; r < d.nd; r++ {
		c := uint(d.d[r])
		dig := n >> k
		n &= mask
		d.d[w] = byte(dig + uint(zero))
		w++
		n = n*10 + c - uint(zero)
	}

	// Put down extra digits.
	for n > 0 {
		dig := n >> k
		n &= mask
		if w < len(d.d) {
			d.d[w] = byte(dig + uint(zero))
			w++
		} else if dig > 0 {
			d.trunc = true
		}
		n = n * 10
	}

	d.nd = w
	d.trim()
}

// Binary shift left (* 2) by k bits.  k <= maxShift to avoid overflow.
func (d *decimal) leftShift(k uint) {
	delta := leftCheats[k].delta
	if prefixIsLessThan(d.d[0:d.nd], leftCheats[k].cutoff) {
		delta--
	}

	r := d.nd         // read index
	w := d.nd + delta // write index

	// Pick up d digit, put down d digit.
	var n uint
	for r--; r >= 0; r-- {
		n += (uint(d.d[r]) - uint(zero)) << k
		quo := n / 10
		rem := n - 10*quo
		w--
		if w < len(d.d) {
			d.d[w] = byte(rem + uint(zero))
		} else if rem != 0 {
			d.trunc = true
		}
		n = quo
	}

	// Put down extra digits.
	for n > 0 {
		quo := n / 10
		rem := n - 10*quo
		w--
		if w < len(d.d) {
			d.d[w] = byte(rem + uint(zero))
		} else if rem != 0 {
			d.trunc = true
		}
		n = quo
	}

	d.nd += delta
	if d.nd >= len(d.d) {
		d.nd = len(d.d)
	}
	d.dp += delta
	d.trim()
}

func (d *decimal) set(s []byte) (ok bool) {
	i := 0
	d.neg = false
	d.trunc = false

	// optional sign
	if i >= len(s) {
		return
	}
	switch {
	case s[i] == plus:
		i++
	case s[i] == minus:
		d.neg = true
		i++
	}

	// digits
	sawdot := false
	sawdigits := false
	for ; i < len(s); i++ {
		switch {
		case s[i] == period:
			if sawdot {
				return
			}
			sawdot = true
			d.dp = d.nd
			continue

		case zero <= s[i] && s[i] <= nine:
			sawdigits = true
			if s[i] == zero && d.nd == 0 { // ignore leading zeros
				d.dp--
				continue
			}
			if d.nd < len(d.d) {
				d.d[d.nd] = s[i]
				d.nd++
			} else if s[i] != zero {
				d.trunc = true
			}
			continue
		}
		break
	}
	if !sawdigits {
		return
	}
	if !sawdot {
		d.dp = d.nd
	}

	// optional exponent moves decimal point.
	// if we read a very large, very long number,
	// just be sure to move the decimal point by
	// a lot (say, 100000).  it doesn't matter if it's
	// not the exact number.
	if i < len(s) && (s[i] == eChr || s[i] == bigEChr) {
		i++
		if i >= len(s) {
			return
		}
		esign := 1
		if s[i] == plus {
			i++
		} else if s[i] == minus {
			i++
			esign = -1
		}
		if i >= len(s) || s[i] < zero || s[i] > nine {
			return
		}
		e := 0
		for ; i < len(s) && zero <= s[i] && s[i] <= nine; i++ {
			if e < 10000 {
				e = e*10 + int(s[i]) - int(zero)
			}
		}
		d.dp += e * esign
	}

	if i != len(s) {
		return
	}

	ok = true
	return
}

func (d *decimal) floatBits(flt *floatInfo) (b uint64, overflow bool) {
	var exp int
	var mant uint64

	// Zero is always a special case.
	if d.nd == 0 {
		mant = 0
		exp = flt.bias
		goto out
	}

	// Obvious overflow/underflow.
	// These bounds are for 64-bit floats.
	// Will have to change if we want to support 80-bit floats in the future.
	if d.dp > 310 {
		goto overflow
	}
	if d.dp < -330 {
		// zero
		mant = 0
		exp = flt.bias
		goto out
	}

	// Scale by powers of two until in range [0.5, 1.0)
	exp = 0
	for d.dp > 0 {
		var n int
		if d.dp >= len(powTab) {
			n = 27
		} else {
			n = powTab[d.dp]
		}
		d.shift(-n)
		exp += n
	}
	for d.dp < 0 || d.dp == 0 && d.d[0] < five {
		var n int
		if -d.dp >= len(powTab) {
			n = 27
		} else {
			n = powTab[-d.dp]
		}
		d.shift(n)
		exp -= n
	}

	// Our range is [0.5,1) but floating point range is [1,2).
	exp--

	// Minimum representable exponent is flt.bias+1.
	// If the exponent is smaller, move it up and
	// adjust d accordingly.
	if exp < flt.bias+1 {
		n := flt.bias + 1 - exp
		d.shift(-n)
		exp += n
	}

	if exp-flt.bias >= 1<<flt.expbits-1 {
		goto overflow
	}

	// Extract 1+flt.mantbits bits.
	d.shift(int(1 + flt.mantbits))
	mant = d.roundedInteger()

	// Rounding might have added a bit; shift down.
	if mant == 2<<flt.mantbits {
		mant >>= 1
		exp++
		if exp-flt.bias >= 1<<flt.expbits-1 {
			goto overflow
		}
	}

	// Denormalized?
	if mant&(1<<flt.mantbits) == 0 {
		exp = flt.bias
	}
	goto out

overflow:
	// ±Inf
	mant = 0
	exp = 1<<flt.expbits - 1 + flt.bias
	overflow = true

out:
	// Assemble bits.
	bits := mant & (uint64(1)<<flt.mantbits - 1)
	bits |= uint64((exp-flt.bias)&(1<<flt.expbits-1)) << flt.mantbits
	if d.neg {
		bits |= 1 << flt.mantbits << flt.expbits
	}
	return bits, overflow
}

// Assign v to a.
func (d *decimal) assign(v uint64) {
	var buf [24]byte

	// Write reversed decimal in buf.
	n := 0
	for v > 0 {
		v1 := v / 10
		v -= 10 * v1
		buf[n] = byte(v + uint64(zero))
		n++
		v = v1
	}

	// Reverse again to produce forward decimal in d.d.
	d.nd = 0
	for n--; n >= 0; n-- {
		d.d[d.nd] = buf[n]
		d.nd++
	}
	d.dp = d.nd
	d.trim()
}

// Binary shift left (k > 0) or right (k < 0).
func (d *decimal) shift(k int) {
	switch {
	case d.nd == 0:
		// nothing to do: d == 0
	case k > 0:
		for k > maxShift {
			d.leftShift(maxShift)
			k -= maxShift
		}
		d.leftShift(uint(k))
	case k < 0:
		for k < -maxShift {
			d.rightShift(maxShift)
			k += maxShift
		}
		d.rightShift(uint(-k))
	}
}

// Round a to nd digits (or fewer).
// If nd is zero, it means we're rounding
// just to the left of the digits, as in
// 0.09 -> 0.1.
func (d *decimal) round(nd int) {
	if nd < 0 || nd >= d.nd {
		return
	}
	if shouldRoundUp(d, nd) {
		d.roundUp(nd)
	} else {
		d.roundDown(nd)
	}
}

// Round a down to nd digits (or fewer).
func (d *decimal) roundDown(nd int) {
	if nd < 0 || nd >= d.nd {
		return
	}
	d.nd = nd
	d.trim()
}

// Round a up to nd digits (or fewer).
func (d *decimal) roundUp(nd int) {
	if nd < 0 || nd >= d.nd {
		return
	}

	// round up
	for i := nd - 1; i >= 0; i-- {
		c := d.d[i]
		if c < nine { // can stop after this digit
			d.d[i]++
			d.nd = i + 1
			return
		}
	}

	// Number is all 9s.
	// Change to single 1 with adjusted decimal point.
	d.d[0] = one
	d.nd = 1
	d.dp++
}

// Extract integer part, rounded appropriately.
// No guarantees about overflow.
func (d *decimal) roundedInteger() uint64 {
	if d.dp > 20 {
		return 0xFFFFFFFFFFFFFFFF
	}
	var i int
	n := uint64(0)
	for i = 0; i < d.dp && i < d.nd; i++ {
		n = n*10 + uint64(d.d[i]-zero)
	}
	for ; i < d.dp; i++ {
		n *= 10
	}
	if shouldRoundUp(d, d.dp) {
		n++
	}
	return n
}

// roundShortest rounds d (= mant * 2^exp) to the shortest number of digits
// that will let the original floating point value be precisely reconstructed.
func (d *decimal) roundShortest(mant uint64, exp int, flt *floatInfo) {
	// If mantissa is zero, the number is zero; stop now.
	if mant == 0 {
		d.nd = 0
		return
	}

	// Compute upper and lower such that any decimal number
	// between upper and lower (possibly inclusive)
	// will round to the original floating point number.

	// We may see at once that the number is already shortest.
	//
	// Suppose d is not denormal, so that 2^exp <= d < 10^dp.
	// The closest shorter number is at least 10^(dp-nd) away.
	// The lower/upper bounds computed below are at distance
	// at most 2^(exp-mantbits).
	//
	// So the number is already shortest if 10^(dp-nd) > 2^(exp-mantbits),
	// or equivalently log2(10)*(dp-nd) > exp-mantbits.
	// It is true if 332/100*(dp-nd) >= exp-mantbits (log2(10) > 3.32).
	minexp := flt.bias + 1 // minimum possible exponent
	if exp > minexp && 332*(d.dp-d.nd) >= 100*(exp-int(flt.mantbits)) {
		// The number is already shortest.
		return
	}

	// d = mant << (exp - mantbits)
	// Next highest floating point number is mant+1 << exp-mantbits.
	// Our upper bound is halfway between, mant*2+1 << exp-mantbits-1.
	upper := new(decimal)
	upper.assign(mant*2 + 1)
	upper.shift(exp - int(flt.mantbits) - 1)

	// d = mant << (exp - mantbits)
	// Next lowest floating point number is mant-1 << exp-mantbits,
	// unless mant-1 drops the significant bit and exp is not the minimum exp,
	// in which case the next lowest is mant*2-1 << exp-mantbits-1.
	// Either way, call it mantlo << explo-mantbits.
	// Our lower bound is halfway between, mantlo*2+1 << explo-mantbits-1.
	var mantlo uint64
	var explo int
	if mant > 1<<flt.mantbits || exp == minexp {
		mantlo = mant - 1
		explo = exp
	} else {
		mantlo = mant*2 - 1
		explo = exp - 1
	}
	lower := new(decimal)
	lower.assign(mantlo*2 + 1)
	lower.shift(explo - int(flt.mantbits) - 1)

	// The upper and lower bounds are possible outputs only if
	// the original mantissa is even, so that IEEE round-to-even
	// would round to the original mantissa and not the neighbors.
	inclusive := mant%2 == 0

	// Now we can figure out the minimum number of digits required.
	// Walk along until d has distinguished itself from upper and lower.
	for i := 0; i < d.nd; i++ {
		l := byte(zero) // lower digit
		if i < lower.nd {
			l = lower.d[i]
		}
		m := d.d[i]     // middle digit
		u := byte(zero) // upper digit
		if i < upper.nd {
			u = upper.d[i]
		}

		// Okay to round down (truncate) if lower has a different digit
		// or if lower is inclusive and is exactly the result of rounding
		// down (i.e., and we have reached the final digit of lower).
		okdown := l != m || inclusive && i+1 == lower.nd

		// Okay to round up if upper has a different digit and either upper
		// is inclusive or upper is bigger than the result of rounding up.
		okup := m != u && (inclusive || m+1 < u || i+1 < upper.nd)

		// If it's okay to do either, then round to the nearest one.
		// If it's okay to do only one, do it.
		switch {
		case okdown && okup:
			d.round(i + 1)
			return
		case okdown:
			d.roundDown(i + 1)
			return
		case okup:
			d.roundUp(i + 1)
			return
		}
	}
}

// adjustLastDigitFixed assumes d contains the representation of the integral part
// of some number, whose fractional part is num / (den << shift). The numerator
// num is only known up to an uncertainty of size ε, assumed to be less than
// (den << shift)/2.
//
// It will increase the last digit by one to account for correct rounding, typically
// when the fractional part is greater than 1/2, and will return false if ε is such
// that no correct answer can be given.
func (d *decimalSlice) adjustLastDigitFixed(num, den uint64, shift uint, ε uint64) bool {
	if num > den<<shift {
		panic("strconv: num > den<<shift in adjustLastDigitFixed")
	}
	if 2*ε > den<<shift {
		panic("strconv: ε > (den<<shift)/2")
	}
	if 2*(num+ε) < den<<shift {
		return true
	}
	if 2*(num-ε) > den<<shift {
		// increment d by 1.
		i := d.nd - 1
		for ; i >= 0; i-- {
			if d.d[i] == nine {
				d.nd--
			} else {
				break
			}
		}
		if i < 0 {
			d.d[0] = one
			d.nd = 1
			d.dp++
		} else {
			d.d[i]++
		}
		return true
	}
	return false
}

// adjustLastDigit modifies d = x-currentDiff*ε, to get closest to
// d = x-targetDiff*ε, without becoming smaller than x-maxDiff*ε.
// It assumes that a decimal digit is worth ulpDecimal*ε, and that
// all data is known with an error estimate of ulpBinary*ε.
func (d *decimalSlice) adjustLastDigit(currentDiff, targetDiff, maxDiff, ulpDecimal, ulpBinary uint64) bool {
	if ulpDecimal < 2*ulpBinary {
		// Approximation is too wide.
		return false
	}
	for currentDiff+ulpDecimal/2+ulpBinary < targetDiff {
		d.d[d.nd-1]--
		currentDiff += ulpDecimal
	}
	if currentDiff+ulpDecimal <= targetDiff+ulpDecimal/2+ulpBinary {
		// we have two choices, and don't know what to do.
		return false
	}
	if currentDiff < ulpBinary || currentDiff > maxDiff-ulpBinary {
		// we went too far
		return false
	}
	if d.nd == 1 && d.d[0] == zero {
		// the number has actually reached zero.
		d.nd = 0
		d.dp = 0
	}
	return true
}

func (e *NumError) Error() string {
	return e.Func + ": " + "parsing `" + e.Num + "`: " + e.Err.Error()
}

// floatBits returns the bits of the float64 that best approximates
// the extFloat passed as receiver. Overflow is set to true if
// the resulting float64 is ±Inf.
func (f *extFloat) floatBits(flt *floatInfo) (bits uint64, overflow bool) {
	f.normalize()

	exp := f.exp + 63

	// Exponent too small.
	if exp < flt.bias+1 {
		n := flt.bias + 1 - exp
		f.mant >>= uint(n)
		exp += n
	}

	// Extract 1+flt.mantbits bits from the 64-bit mantissa.
	mant := f.mant >> (63 - flt.mantbits)
	if f.mant&(1<<(62-flt.mantbits)) != 0 {
		// Round up.
		mant += 1
	}

	// Rounding might have added a bit; shift down.
	if mant == 2<<flt.mantbits {
		mant >>= 1
		exp++
	}

	// Infinities.
	if exp-flt.bias >= 1<<flt.expbits-1 {
		// ±Inf
		mant = 0
		exp = 1<<flt.expbits - 1 + flt.bias
		overflow = true
	} else if mant&(1<<flt.mantbits) == 0 {
		// Denormalized?
		exp = flt.bias
	}
	// Assemble bits.
	bits = mant & (uint64(1)<<flt.mantbits - 1)
	bits |= uint64((exp-flt.bias)&(1<<flt.expbits-1)) << flt.mantbits
	if f.neg {
		bits |= 1 << (flt.mantbits + flt.expbits)
	}
	return
}

// AssignComputeBounds sets f to the floating point value
// defined by mant, exp and precision given by flt. It returns
// lower, upper such that any number in the closed interval
// [lower, upper] is converted back to the same floating point number.
func (f *extFloat) assignComputeBounds(mant uint64, exp int, neg bool, flt *floatInfo) (lower, upper extFloat) {
	f.mant = mant
	f.exp = exp - int(flt.mantbits)
	f.neg = neg
	if f.exp <= 0 && mant == (mant>>uint(-f.exp))<<uint(-f.exp) {
		// An exact integer
		f.mant >>= uint(-f.exp)
		f.exp = 0
		return *f, *f
	}
	expBiased := exp - flt.bias

	upper = extFloat{mant: 2*f.mant + 1, exp: f.exp - 1, neg: f.neg}
	if mant != 1<<flt.mantbits || expBiased == 1 {
		lower = extFloat{mant: 2*f.mant - 1, exp: f.exp - 1, neg: f.neg}
	} else {
		lower = extFloat{mant: 4*f.mant - 1, exp: f.exp - 2, neg: f.neg}
	}
	return
}

// Normalize normalizes f so that the highest bit of the mantissa is
// set, and returns the number by which the mantissa was left-shifted.
func (f *extFloat) normalize() (shift uint) {
	mant, exp := f.mant, f.exp
	if mant == 0 {
		return 0
	}
	if mant>>(64-32) == 0 {
		mant <<= 32
		exp -= 32
	}
	if mant>>(64-16) == 0 {
		mant <<= 16
		exp -= 16
	}
	if mant>>(64-8) == 0 {
		mant <<= 8
		exp -= 8
	}
	if mant>>(64-4) == 0 {
		mant <<= 4
		exp -= 4
	}
	if mant>>(64-2) == 0 {
		mant <<= 2
		exp -= 2
	}
	if mant>>(64-1) == 0 {
		mant <<= 1
		exp -= 1
	}
	shift = uint(f.exp - exp)
	f.mant, f.exp = mant, exp
	return
}

// Multiply sets f to the product f*g: the result is correctly rounded,
// but not normalized.
func (f *extFloat) multiply(g extFloat) {
	fhi, flo := f.mant>>32, uint64(uint32(f.mant))
	ghi, glo := g.mant>>32, uint64(uint32(g.mant))

	// Cross products.
	cross1 := fhi * glo
	cross2 := flo * ghi

	// f.mant*g.mant is fhi*ghi << 64 + (cross1+cross2) << 32 + flo*glo
	f.mant = fhi*ghi + (cross1 >> 32) + (cross2 >> 32)
	rem := uint64(uint32(cross1)) + uint64(uint32(cross2)) + ((flo * glo) >> 32)
	// Round up.
	rem += 1 << 31

	f.mant += rem >> 32
	f.exp = f.exp + g.exp + 64
}

// AssignDecimal sets f to an approximate value mantissa*10^exp. It
// reports whether the value represented by f is guaranteed to be the
// best approximation of d after being rounded to a float64 or
// float32 depending on flt.
func (f *extFloat) assignDecimal(mantissa uint64, exp10 int, neg bool, trunc bool, flt *floatInfo) (ok bool) {
	const uint64digits = 19
	const errorscale = 8
	errors := 0 // An upper bound for error, computed in errorscale*ulp.
	if trunc {
		// the decimal number was truncated.
		errors += errorscale / 2
	}

	f.mant = mantissa
	f.exp = 0
	f.neg = neg

	// Multiply by powers of ten.
	i := (exp10 - firstPowerOfTen) / stepPowerOfTen
	if exp10 < firstPowerOfTen || i >= len(powersOfTen) {
		return false
	}
	adjExp := (exp10 - firstPowerOfTen) % stepPowerOfTen

	// We multiply by exp%step
	if adjExp < uint64digits && mantissa < uint64pow10[uint64digits-adjExp] {
		// We can multiply the mantissa exactly.
		f.mant *= uint64pow10[adjExp]
		f.normalize()
	} else {
		f.normalize()
		f.multiply(smallPowersOfTen[adjExp])
		errors += errorscale / 2
	}

	// We multiply by 10 to the exp - exp%step.
	f.multiply(powersOfTen[i])
	if errors > 0 {
		errors += 1
	}
	errors += errorscale / 2

	// Normalize
	shift := f.normalize()
	errors <<= shift

	// Now f is a good approximation of the decimal.
	// Check whether the error is too large: that is, if the mantissa
	// is perturbated by the error, the resulting float64 will change.
	// The 64 bits mantissa is 1 + 52 bits for float64 + 11 extra bits.
	//
	// In many cases the approximation will be good enough.
	denormalExp := flt.bias - 63
	var extrabits uint
	if f.exp <= denormalExp {
		// f.mant * 2^f.exp is smaller than 2^(flt.bias+1).
		extrabits = 63 - flt.mantbits + 1 + uint(denormalExp-f.exp)
	} else {
		extrabits = 63 - flt.mantbits
	}

	halfway := uint64(1) << (extrabits - 1)
	mantExtra := f.mant & (1<<extrabits - 1)

	// Do a signed comparison here! If the error estimate could make
	// the mantissa round differently for the conversion to double,
	// then we can't give a definite answer.
	if int64(halfway)-int64(errors) < int64(mantExtra) &&
		int64(mantExtra) < int64(halfway)+int64(errors) {
		return false
	}
	return true
}

// Frexp10 is an analogue of math.Frexp for decimal powers. It scales
// f by an approximate power of ten 10^-exp, and returns exp10, so
// that f*10^exp10 has the same value as the old f, up to an ulp,
// as well as the index of 10^-exp in the powersOfTen table.
func (f *extFloat) frexp10() (exp10, index int) {
	// The constants expMin and expMax constrain the final value of the
	// binary exponent of f. We want a small integral part in the result
	// because finding digits of an integer requires divisions, whereas
	// digits of the fractional part can be found by repeatedly multiplying
	// by 10.
	const expMin = -60
	const expMax = -32
	// Find power of ten such that x * 10^n has a binary exponent
	// between expMin and expMax.
	approxExp10 := ((expMin+expMax)/2 - f.exp) * 28 / 93 // log(10)/log(2) is close to 93/28.
	i := (approxExp10 - firstPowerOfTen) / stepPowerOfTen
Loop:
	for {
		exp := f.exp + powersOfTen[i].exp + 64
		switch {
		case exp < expMin:
			i++
		case exp > expMax:
			i--
		default:
			break Loop
		}
	}
	// Apply the desired decimal shift on f. It will have exponent
	// in the desired range. This is multiplication by 10^-exp10.
	f.multiply(powersOfTen[i])

	return -(firstPowerOfTen + i*stepPowerOfTen), i
}

// ShortestDecimal stores in d the shortest decimal representation of f
// which belongs to the open interval (lower, upper), where f is supposed
// to lie. It returns false whenever the result is unsure. The implementation
// uses the Grisu3 algorithm.
func (f *extFloat) shortestDecimal(d *decimalSlice, lower, upper *extFloat) bool {
	if f.mant == 0 {
		d.nd = 0
		d.dp = 0
		d.neg = f.neg
		return true
	}
	if f.exp == 0 && *lower == *f && *lower == *upper {
		// an exact integer.
		var buf [24]byte
		n := len(buf) - 1
		for v := f.mant; v > 0; {
			v1 := v / 10
			v -= 10 * v1
			buf[n] = byte(v + uint64(zero))
			n--
			v = v1
		}
		nd := len(buf) - n - 1
		for i := 0; i < nd; i++ {
			d.d[i] = buf[n+1+i]
		}
		d.nd, d.dp = nd, nd
		for d.nd > 0 && d.d[d.nd-1] == zero {
			d.nd--
		}
		if d.nd == 0 {
			d.dp = 0
		}
		d.neg = f.neg
		return true
	}
	upper.normalize()
	// Uniformize exponents.
	if f.exp > upper.exp {
		f.mant <<= uint(f.exp - upper.exp)
		f.exp = upper.exp
	}
	if lower.exp > upper.exp {
		lower.mant <<= uint(lower.exp - upper.exp)
		lower.exp = upper.exp
	}

	exp10 := frexp10Many(lower, f, upper)
	// Take a safety margin due to rounding in frexp10Many, but we lose precision.
	upper.mant++
	lower.mant--

	// The shortest representation of f is either rounded up or down, but
	// in any case, it is a truncation of upper.
	shift := uint(-upper.exp)
	integer := uint32(upper.mant >> shift)
	fraction := upper.mant - (uint64(integer) << shift)

	// How far we can go down from upper until the result is wrong.
	allowance := upper.mant - lower.mant
	// How far we should go to get a very precise result.
	targetDiff := upper.mant - f.mant

	// Count integral digits: there are at most 10.
	var integerDigits int
	for i, pow := 0, uint64(1); i < 20; i++ {
		if pow > uint64(integer) {
			integerDigits = i
			break
		}
		pow *= 10
	}
	for i := 0; i < integerDigits; i++ {
		pow := uint64pow10[integerDigits-i-1]
		digit := integer / uint32(pow)
		d.d[i] = byte(digit + uint32(zero))
		integer -= digit * uint32(pow)
		// evaluate whether we should stop.
		if currentDiff := uint64(integer)<<shift + fraction; currentDiff < allowance {
			d.nd = i + 1
			d.dp = integerDigits + exp10
			d.neg = f.neg
			// Sometimes allowance is so large the last digit might need to be
			// decremented to get closer to f.
			return d.adjustLastDigit(currentDiff, targetDiff, allowance, pow<<shift, 2)
		}
	}
	d.nd = integerDigits
	d.dp = d.nd + exp10
	d.neg = f.neg

	// Compute digits of the fractional part. At each step fraction does not
	// overflow. The choice of minExp implies that fraction is less than 2^60.
	var digit int
	multiplier := uint64(1)
	for {
		fraction *= 10
		multiplier *= 10
		digit = int(fraction >> shift)
		d.d[d.nd] = byte(digit + int(zero))
		d.nd++
		fraction -= uint64(digit) << shift
		if fraction < allowance*multiplier {
			// We are in the admissible range. Note that if allowance is about to
			// overflow, that is, allowance > 2^64/10, the condition is automatically
			// true due to the limited range of fraction.
			return d.adjustLastDigit(
				fraction, targetDiff*multiplier, allowance*multiplier,
				1<<shift, multiplier*2)
		}
	}
}
