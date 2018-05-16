/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"io"
	"math/rand"
	"testing"
)

const N = 10000       // make this bigger for a larger (and slower) test
var testString string // test data for write tests
var testBytes []byte  // test data; same as testString but as a slice.

type negativeReader struct{}

func (r *negativeReader) Read([]byte) (int, error) { return -1, nil }

func init() {
	testBytes = make([]byte, N)
	for i := 0; i < N; i++ {
		testBytes[i] = 'a' + byte(i%26)
	}
	testString = string(testBytes)
}

// Verify that contents of buf match the string s.
func check(t *testing.T, testname string, buf *Buffer, s string) {
	bytes := buf.Bytes()
	str := buf.String()
	if buf.Len() != len(bytes) {
		t.Errorf("%s: buf.Len() == %d, len(buf.Bytes()) == %d", testname, buf.Len(), len(bytes))
	}

	if buf.Len() != len(str) {
		t.Errorf("%s: buf.Len() == %d, len(buf.String()) == %d", testname, buf.Len(), len(str))
	}

	if buf.Len() != len(s) {
		t.Errorf("%s: buf.Len() == %d, len(s) == %d", testname, buf.Len(), len(s))
	}

	if string(bytes) != s {
		t.Errorf("%s: string(buf.Bytes()) == %q, s == %q", testname, string(bytes), s)
	}
}

// Fill buf through n writes of string fus.
// The initial contents of buf corresponds to the string s;
// the result is the final contents of buf returned as a string.
func fillString(t *testing.T, testname string, buf *Buffer, s string, n int, fus string) string {
	check(t, testname+" (fill 1)", buf, s)
	for ; n > 0; n-- {
		m, err := buf.WriteString(fus)
		if m != len(fus) {
			t.Errorf(testname+" (fill 2): m == %d, expected %d", m, len(fus))
		}
		if err != nil {
			t.Errorf(testname+" (fill 3): err should always be nil, found err == %s", err)
		}
		s += fus
		check(t, testname+" (fill 4)", buf, s)
	}
	return s
}

// Fill buf through n writes of byte slice fub.
// The initial contents of buf corresponds to the string s;
// the result is the final contents of buf returned as a string.
func fillBytes(t *testing.T, testname string, buf *Buffer, s string, n int, fub []byte) string {
	check(t, testname+" (fill 1)", buf, s)
	for ; n > 0; n-- {
		m, err := buf.Write(fub)
		if m != len(fub) {
			t.Errorf(testname+" (fill 2): m == %d, expected %d", m, len(fub))
		}
		if err != nil {
			t.Errorf(testname+" (fill 3): err should always be nil, found err == %s", err)
		}
		s += string(fub)
		check(t, testname+" (fill 4)", buf, s)
	}
	return s
}

func TestNewBuffer(t *testing.T) {
	buf := NewBuffer(testBytes)
	check(t, "NewBuffer", buf, testString)
}

// Empty buf through repeated reads into fub.
// The initial contents of buf corresponds to the string s.
func empty(t *testing.T, testname string, buf *Buffer, s string, fub []byte) {
	check(t, testname+" (empty 1)", buf, s)

	for {
		n, err := buf.Read(fub)
		if n == 0 {
			break
		}
		if err != nil {
			t.Errorf(testname+" (empty 2): err should always be nil, found err == %s", err)
		}
		s = s[n:]
		check(t, testname+" (empty 3)", buf, s)
	}

	check(t, testname+" (empty 4)", buf, "")
}

func TestBasicOperations(t *testing.T) {
	var buf Buffer

	for i := 0; i < 5; i++ {
		check(t, "TestBasicOperations (1)", &buf, "")

		buf.Reset()
		check(t, "TestBasicOperations (2)", &buf, "")

		buf.Truncate(0)
		check(t, "TestBasicOperations (3)", &buf, "")

		n, err := buf.Write(testBytes[0:1])
		if n != 1 {
			t.Errorf("wrote 1 byte, but n == %d", n)
		}
		if err != nil {
			t.Errorf("err should always be nil, but err == %s", err)
		}
		check(t, "TestBasicOperations (4)", &buf, "a")

		buf.WriteByte(testString[1])
		check(t, "TestBasicOperations (5)", &buf, "ab")

		n, err = buf.Write(testBytes[2:26])
		if n != 24 {
			t.Errorf("wrote 24 bytes, but n == %d", n)
		}
		check(t, "TestBasicOperations (6)", &buf, testString[0:26])

		buf.Truncate(26)
		check(t, "TestBasicOperations (7)", &buf, testString[0:26])

		buf.Truncate(20)
		check(t, "TestBasicOperations (8)", &buf, testString[0:20])

		empty(t, "TestBasicOperations (9)", &buf, testString[0:20], make([]byte, 5))
		empty(t, "TestBasicOperations (10)", &buf, "", make([]byte, 100))

		buf.WriteByte(testString[1])
		buf.Bytes()
		c, err := buf.ReadByte()
		if err != nil {
			t.Error("ReadByte unexpected eof")
		}
		if c != testString[1] {
			t.Errorf("ReadByte wrong value c=%v", c)
		}
		c, err = buf.ReadByte()
		if err == nil {
			t.Error("ReadByte unexpected not eof")
		}
	}
}

func TestLargeStringWrites(t *testing.T) {
	var buf Buffer
	limit := 30
	if testing.Short() {
		limit = 9
	}
	for i := 3; i < limit; i += 3 {
		s := fillString(t, "TestLargeWrites (1)", &buf, "", 5, testString)
		empty(t, "TestLargeStringWrites (2)", &buf, s, make([]byte, len(testString)/i))
	}
	check(t, "TestLargeStringWrites (3)", &buf, "")
}

func TestLargeByteWrites(t *testing.T) {
	var buf Buffer
	limit := 30
	if testing.Short() {
		limit = 9
	}
	for i := 3; i < limit; i += 3 {
		s := fillBytes(t, "TestLargeWrites (1)", &buf, "", 5, testBytes)
		empty(t, "TestLargeByteWrites (2)", &buf, s, make([]byte, len(testString)/i))
	}
	check(t, "TestLargeByteWrites (3)", &buf, "")
}

func TestLargeStringReads(t *testing.T) {
	var buf Buffer
	for i := 3; i < 30; i += 3 {
		s := fillString(t, "TestLargeReads (1)", &buf, "", 5, testString[0:len(testString)/i])
		empty(t, "TestLargeReads (2)", &buf, s, make([]byte, len(testString)))
	}
	check(t, "TestLargeStringReads (3)", &buf, "")
}

func TestLargeByteReads(t *testing.T) {
	var buf Buffer
	for i := 3; i < 30; i += 3 {
		s := fillBytes(t, "TestLargeReads (1)", &buf, "", 5, testBytes[0:len(testBytes)/i])
		empty(t, "TestLargeReads (2)", &buf, s, make([]byte, len(testString)))
	}
	check(t, "TestLargeByteReads (3)", &buf, "")
}

func TestMixedReadsAndWrites(t *testing.T) {
	var buf Buffer
	s := ""
	for i := 0; i < 50; i++ {
		wlen := rand.Intn(len(testString))
		if i%2 == 0 {
			s = fillString(t, "TestMixedReadsAndWrites (1)", &buf, s, 1, testString[0:wlen])
		} else {
			s = fillBytes(t, "TestMixedReadsAndWrites (1)", &buf, s, 1, testBytes[0:wlen])
		}

		rlen := rand.Intn(len(testString))
		fub := make([]byte, rlen)
		n, _ := buf.Read(fub)
		s = s[n:]
	}
	empty(t, "TestMixedReadsAndWrites (2)", &buf, s, make([]byte, buf.Len()))
}

func TestCapWithPreallocatedSlice(t *testing.T) {
	buf := NewBuffer(make([]byte, 10))
	n := buf.Cap()
	if n != 10 {
		t.Errorf("expected 10, got %d", n)
	}
}

func TestCapWithSliceAndWrittenData(t *testing.T) {
	buf := NewBuffer(make([]byte, 0, 10))
	buf.Write([]byte("test"))
	n := buf.Cap()
	if n != 10 {
		t.Errorf("expected 10, got %d", n)
	}
}

func TestNil(t *testing.T) {
	var b *Buffer
	if b.String() != "<nil>" {
		t.Errorf("expected <nil>; got %q", b.String())
	}
}

func TestReadFrom(t *testing.T) {
	var buf Buffer
	for i := 3; i < 30; i += 3 {
		s := fillBytes(t, "TestReadFrom (1)", &buf, "", 5, testBytes[0:len(testBytes)/i])
		var b Buffer
		b.ReadFrom(&buf)
		empty(t, "TestReadFrom (2)", &b, s, make([]byte, len(testString)))
	}
}

func TestReadFromNegativeReader(t *testing.T) {
	var b Buffer
	defer func() {
		switch err := recover().(type) {
		case nil:
			t.Fatal("bytes.Buffer.ReadFrom didn't panic")
		case error:
			// this is the error string of errNegativeRead
			wantError := "bytes.Buffer: reader returned negative count from Read"
			if err.Error() != wantError {
				t.Fatalf("recovered panic: got %v, want %v", err.Error(), wantError)
			}
		default:
			t.Fatalf("unexpected panic value: %#v", err)
		}
	}()

	b.ReadFrom(new(negativeReader))
}

func TestWriteTo(t *testing.T) {
	var buf Buffer
	for i := 3; i < 30; i += 3 {
		s := fillBytes(t, "TestWriteTo (1)", &buf, "", 5, testBytes[0:len(testBytes)/i])
		var b Buffer
		buf.WriteTo(&b)
		empty(t, "TestWriteTo (2)", &b, s, make([]byte, len(testString)))
	}
}

func TestNext(t *testing.T) {
	b := []byte{0, 1, 2, 3, 4}
	tmp := make([]byte, 5)
	for i := 0; i <= 5; i++ {
		for j := i; j <= 5; j++ {
			for k := 0; k <= 6; k++ {
				// 0 <= i <= j <= 5; 0 <= k <= 6
				// Check that if we start with a buffer
				// of length j at offset i and ask for
				// Next(k), we get the right bytes.
				buf := NewBuffer(b[0:j])
				n, _ := buf.Read(tmp[0:i])
				if n != i {
					t.Fatalf("Read %d returned %d", i, n)
				}
				bb := buf.Next(k)
				want := k
				if want > j-i {
					want = j - i
				}
				if len(bb) != want {
					t.Fatalf("in %d,%d: len(Next(%d)) == %d", i, j, k, len(bb))
				}
				for l, v := range bb {
					if v != byte(l+i) {
						t.Fatalf("in %d,%d: Next(%d)[%d] = %d, want %d", i, j, k, l, v, l+i)
					}
				}
			}
		}
	}
}

var readBytesTests = []struct {
	buffer   string
	delim    byte
	expected []string
	err      error
}{
	{"", 0, []string{""}, io.EOF},
	{"a\x00", 0, []string{"a\x00"}, nil},
	{"abbbaaaba", 'b', []string{"ab", "b", "b", "aaab"}, nil},
	{"hello\x01world", 1, []string{"hello\x01"}, nil},
	{"foo\nbar", 0, []string{"foo\nbar"}, io.EOF},
	{"alpha\nbeta\ngamma\n", '\n', []string{"alpha\n", "beta\n", "gamma\n"}, nil},
	{"alpha\nbeta\ngamma", '\n', []string{"alpha\n", "beta\n", "gamma"}, io.EOF},
}

func BenchmarkReadString(b *testing.B) {
	const n = 32 << 10

	data := make([]byte, n)
	data[n-1] = 'x'
	b.SetBytes(int64(n))
	for i := 0; i < b.N; i++ {
		buf := NewBuffer(data)
		_, err := buf.ReadString('x')
		if err != nil {
			b.Fatal(err)
		}
	}
}

func TestGrowOverflow(t *testing.T) {
	defer func() {
		if err := recover(); err != ErrTooLarge {
			t.Errorf("after too-large Grow, recover() = %v; want %v", err, ErrTooLarge)
		}
	}()

	buf := NewBuffer(make([]byte, 1))
	const maxInt = int(^uint(0) >> 1)
	buf.Grow(maxInt)
}

// Was a bug: used to give EOF reading empty slice at EOF.
func TestReadEmptyAtEOF(t *testing.T) {
	b := new(Buffer)
	slice := make([]byte, 0)
	n, err := b.Read(slice)
	if err != nil {
		t.Errorf("read error: %v", err)
	}
	if n != 0 {
		t.Errorf("wrong count; got %d want 0", n)
	}
}

func TestUnreadByte(t *testing.T) {
	b := new(Buffer)

	// check at EOF
	if err := b.UnreadByte(); err == nil {
		t.Fatal("UnreadByte at EOF: got no error")
	}
	if _, err := b.ReadByte(); err == nil {
		t.Fatal("ReadByte at EOF: got no error")
	}
	if err := b.UnreadByte(); err == nil {
		t.Fatal("UnreadByte after ReadByte at EOF: got no error")
	}

	// check not at EOF
	b.WriteString("abcdefghijklmnopqrstuvwxyz")

	// after unsuccessful read
	if n, err := b.Read(nil); n != 0 || err != nil {
		t.Fatalf("Read(nil) = %d,%v; want 0,nil", n, err)
	}
	if err := b.UnreadByte(); err == nil {
		t.Fatal("UnreadByte after Read(nil): got no error")
	}

	// after successful read
	if _, err := b.ReadBytes('m'); err != nil {
		t.Fatalf("ReadBytes: %v", err)
	}
	if err := b.UnreadByte(); err != nil {
		t.Fatalf("UnreadByte: %v", err)
	}
	c, err := b.ReadByte()
	if err != nil {
		t.Fatalf("ReadByte: %v", err)
	}
	if c != 'm' {
		t.Errorf("ReadByte = %q; want %q", c, 'm')
	}
}

// Tests that we occasionally compact. Issue 5154.
func TestBufferGrowth(t *testing.T) {
	var b Buffer
	buf := make([]byte, 1024)
	b.Write(buf[0:1])
	var cap0 int
	for i := 0; i < 5<<10; i++ {
		b.Write(buf)
		b.Read(buf)
		if i == 0 {
			cap0 = b.Cap()
		}
	}
	cap1 := b.Cap()
	// (*Buffer).grow allows for 2x capacity slop before sliding,
	// so set our error threshold at 3x.
	if cap1 > cap0*3 {
		t.Errorf("buffer cap = %d; too big (grew from %d)", cap1, cap0)
	}
}

func BenchmarkWriteByte(b *testing.B) {
	const n = 4 << 10
	b.SetBytes(n)
	buf := NewBuffer(make([]byte, n))
	for i := 0; i < b.N; i++ {
		buf.Reset()
		for i := 0; i < n; i++ {
			buf.WriteByte('x')
		}
	}
}

// From Issue 5154.
func BenchmarkBufferNotEmptyWriteRead(b *testing.B) {
	buf := make([]byte, 1024)
	for i := 0; i < b.N; i++ {
		var b Buffer
		b.Write(buf[0:1])
		for i := 0; i < 5<<10; i++ {
			b.Write(buf)
			b.Read(buf)
		}
	}
}

// Check that we don't compact too often. From Issue 5154.
func BenchmarkBufferFullSmallReads(b *testing.B) {
	buf := make([]byte, 1024)
	for i := 0; i < b.N; i++ {
		var b Buffer
		b.Write(buf)
		for b.Len()+20 < b.Cap() {
			b.Write(buf[:10])
		}
		for i := 0; i < 5<<10; i++ {
			b.Read(buf[:1])
			b.Write(buf[:1])
		}
	}
}

func TestWriteBytes(t *testing.T) {
	var a Buffer
	a.WriteQuoted([]byte(`Nulla`))
	if !bytes.Equal(a.Bytes(), []byte(`"Nulla"`)) {
		t.Fatal("Not equal")
	}

	a.Reset()
	a.WriteByte(':')
	a.WriteQuoted([]byte(`Nulla`))
	if !bytes.Equal(a.Bytes(), []byte(`:"Nulla"`)) {
		t.Fatal("Not equal")
	}

	a.Reset()
	a.WriteQuoted([]byte(`Nulla!`))
	a.WriteQuoted([]byte(`42`))
	if !bytes.Equal(a.Bytes(), []byte(`"Nulla!""42"`)) {
		t.Fatal("Not equal")
	}

	var b Buffer
	b.Write([]byte(`Nulla`))
	b.WriteBytes(backSlash, 'u', '0', '0')
	if !bytes.Equal(b.Bytes(), []byte(`Nulla\u00`)) {
		t.Fatal("Not equal")
	}

	var c Buffer
	c.WriteByte('a')
	c.WriteByte('b')
	if !bytes.Equal(c.Bytes(), []byte(`ab`)) {
		t.Fatal("Not equal")
	}

	var d Buffer
	d.WriteByte(':')
	d.WriteString("Bogdan")
	if !bytes.Equal(d.Bytes(), []byte(`:Bogdan`)) {
		t.Fatal("Not equal")
	}

	var e Buffer
	e.WriteByte(':')
	e.WriteBytes(backSlash, 'u', '0', '0')
	if !bytes.Equal(e.Bytes(), []byte(`:\u00`)) {
		t.Fatal("Not equal")
	}
}
