/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"bytes"
	"compress/gzip"
	"encoding/json"
	"io/ioutil"
	"os"
	"strings"
	"testing"
)

type (
	codeResponse struct {
		Tree     *codeNode `json:"tree"`
		Username string    `json:"username"`
	}

	codeNode struct {
		Name     string      `json:"name"`
		Kids     []*codeNode `json:"kids"`
		CLWeight float64     `json:"cl_weight"`
		Touches  int         `json:"touches"`
		MinT     int64       `json:"min_t"`
		MaxT     int64       `json:"max_t"`
		MeanT    int64       `json:"mean_t"`
	}
)

var (
	codeStruct codeResponse
)

func codeInit() []byte {
	f, err := os.Open("testdata/code.json.gz")
	if err != nil {
		panic(err)
	}
	defer f.Close()
	gz, err := gzip.NewReader(f)
	if err != nil {
		panic(err)
	}
	data, err := ioutil.ReadAll(gz)
	if err != nil {
		panic(err)
	}

	codeJSON := data

	if err := Unmarshal(codeJSON, &codeStruct); err != nil {
		panic("unmarshal code.json: " + err.Error())
	}

	if data, err = Marshal(&codeStruct); err != nil {
		panic("marshal code.json: " + err.Error())
	}

	if !bytes.Equal(data, codeJSON) {
		println("different lengths", len(data), len(codeJSON))
		for i := 0; i < len(data) && i < len(codeJSON); i++ {
			if data[i] != codeJSON[i] {
				println("re-marshal: changed at byte", i)
				println("orig: ", string(codeJSON[i-10:i+10]))
				println("new: ", string(data[i-10:i+10]))
				break
			}
		}
		panic("re-marshal code.json: different result")
	}

	return codeJSON
}

func oldCodeInit() []byte {
	f, err := os.Open("testdata/code.json.gz")
	if err != nil {
		panic(err)
	}
	defer f.Close()
	gz, err := gzip.NewReader(f)
	if err != nil {
		panic(err)
	}
	data, err := ioutil.ReadAll(gz)
	if err != nil {
		panic(err)
	}

	codeJSON := data

	if err := json.Unmarshal(codeJSON, &codeStruct); err != nil {
		panic("unmarshal code.json: " + err.Error())
	}

	if data, err = json.Marshal(&codeStruct); err != nil {
		panic("marshal code.json: " + err.Error())
	}

	if !bytes.Equal(data, codeJSON) {
		println("different lengths", len(data), len(codeJSON))
		for i := 0; i < len(data) && i < len(codeJSON); i++ {
			if data[i] != codeJSON[i] {
				println("re-marshal: changed at byte", i)
				println("orig: ", string(codeJSON[i-10:i+10]))
				println("new: ", string(data[i-10:i+10]))
				break
			}
		}
		panic("re-marshal code.json: different result")
	}
	return codeJSON
}

func BenchmarkCodeEncoder(b *testing.B) {

	b.StopTimer()
	codeJSON := codeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		enc := NewEncoder(ioutil.Discard)
		for pb.Next() {
			if err := enc.Encode(&codeStruct); err != nil {
				b.Fatal("Encode:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkOldCodeEncoder(b *testing.B) {

	b.StopTimer()
	codeJSON := oldCodeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		enc := json.NewEncoder(ioutil.Discard)
		for pb.Next() {
			if err := enc.Encode(&codeStruct); err != nil {
				b.Fatal("Encode:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkCodeMarshal(b *testing.B) {

	b.StopTimer()
	codeJSON := codeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			if _, err := Marshal(&codeStruct); err != nil {
				b.Fatal("Marshal:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkOldCodeMarshal(b *testing.B) {

	b.StopTimer()
	codeJSON := oldCodeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			if _, err := json.Marshal(&codeStruct); err != nil {
				b.Fatal("Marshal:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkCodeDecoder(b *testing.B) {

	b.StopTimer()
	codeJSON := codeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		var buf Buffer
		dec := NewDecoder(&buf)
		var r codeResponse
		for pb.Next() {
			buf.Write(codeJSON)
			// hide EOF
			buf.WriteByte('\n')
			buf.WriteByte('\n')
			buf.WriteByte('\n')
			if err := dec.Decode(&r); err != nil {
				b.Fatal("Decode:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkOldCodeDecoder(b *testing.B) {

	b.StopTimer()
	codeJSON := oldCodeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		var buf Buffer
		dec := json.NewDecoder(&buf)
		var r codeResponse
		for pb.Next() {
			buf.Write(codeJSON)
			// hide EOF
			buf.WriteByte('\n')
			buf.WriteByte('\n')
			buf.WriteByte('\n')
			if err := dec.Decode(&r); err != nil {
				b.Fatal("Decode:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkUnicodeDecoder(b *testing.B) {
	j := []byte(`"\uD83D\uDE01"`)
	b.SetBytes(int64(len(j)))
	r := bytes.NewReader(j)
	dec := NewDecoder(r)
	var out string
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if err := dec.Decode(&out); err != nil {
			b.Fatal("Decode:", err)
		}
		r.Seek(0, 0)
	}
}

func BenchmarkOldUnicodeDecoder(b *testing.B) {
	j := []byte(`"\uD83D\uDE01"`)
	b.SetBytes(int64(len(j)))
	r := bytes.NewReader(j)
	dec := json.NewDecoder(r)
	var out string
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if err := dec.Decode(&out); err != nil {
			b.Fatal("Decode:", err)
		}
		r.Seek(0, 0)
	}
}

func BenchmarkDecoderStream(b *testing.B) {
	b.StopTimer()
	var buf Buffer
	dec := NewDecoder(&buf)
	buf.WriteString(`"` + strings.Repeat("x", 1000000) + `"` + "\n\n\n")
	var x interface{}
	if err := dec.Decode(&x); err != nil {
		b.Fatal("Decode:", err)
	}
	ones := strings.Repeat(" 1\n", 300000) + "\n\n\n"
	b.StartTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if i%300000 == 0 {
			buf.WriteString(ones)
		}
		x = nil
		if err := dec.Decode(&x); err != nil || x != 1.0 {
			b.Fatalf("Decode: %v after %d", err, i)
		}
	}
}

func BenchmarkOldDecoderStream(b *testing.B) {
	b.StopTimer()
	var buf Buffer
	dec := json.NewDecoder(&buf)
	buf.WriteString(`"` + strings.Repeat("x", 1000000) + `"` + "\n\n\n")
	var x interface{}
	if err := dec.Decode(&x); err != nil {
		b.Fatal("Decode:", err)
	}
	ones := strings.Repeat(" 1\n", 300000) + "\n\n\n"
	b.StartTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		if i%300000 == 0 {
			buf.WriteString(ones)
		}
		x = nil
		if err := dec.Decode(&x); err != nil || x != 1.0 {
			b.Fatalf("Decode: %v after %d", err, i)
		}
	}
}

func BenchmarkCodeUnmarshal(b *testing.B) {

	b.StopTimer()
	codeJSON := codeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			var r codeResponse
			if err := Unmarshal(codeJSON, &r); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkOldCodeUnmarshal(b *testing.B) {

	b.StopTimer()
	codeJSON := oldCodeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			var r codeResponse
			if err := json.Unmarshal(codeJSON, &r); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
	b.SetBytes(int64(len(codeJSON)))
}

func BenchmarkCodeUnmarshalReuse(b *testing.B) {

	b.StopTimer()
	codeJSON := codeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		var r codeResponse
		for pb.Next() {
			if err := Unmarshal(codeJSON, &r); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkOldCodeUnmarshalReuse(b *testing.B) {

	b.StopTimer()
	codeJSON := oldCodeInit()
	b.StartTimer()

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		var r codeResponse
		for pb.Next() {
			if err := json.Unmarshal(codeJSON, &r); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkUnmarshalString(b *testing.B) {
	b.ReportAllocs()
	data := []byte(`"hello, world"`)
	b.RunParallel(func(pb *testing.PB) {
		var s string
		for pb.Next() {
			if err := Unmarshal(data, &s); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkOldUnmarshalString(b *testing.B) {
	b.ReportAllocs()
	data := []byte(`"hello, world"`)
	b.RunParallel(func(pb *testing.PB) {
		var s string
		for pb.Next() {
			if err := json.Unmarshal(data, &s); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkUnmarshalFloat64(b *testing.B) {
	b.ReportAllocs()
	data := []byte(`3.14`)
	b.RunParallel(func(pb *testing.PB) {
		var f float64
		for pb.Next() {
			if err := Unmarshal(data, &f); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkOldUnmarshalFloat64(b *testing.B) {
	b.ReportAllocs()
	data := []byte(`3.14`)
	b.RunParallel(func(pb *testing.PB) {
		var f float64
		for pb.Next() {
			if err := json.Unmarshal(data, &f); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkUnmarshalInt64(b *testing.B) {
	b.ReportAllocs()
	data := []byte(`3`)
	b.RunParallel(func(pb *testing.PB) {
		var x int64
		for pb.Next() {
			if err := Unmarshal(data, &x); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkOldUnmarshalInt64(b *testing.B) {
	b.ReportAllocs()
	data := []byte(`3`)
	b.RunParallel(func(pb *testing.PB) {
		var x int64
		for pb.Next() {
			if err := json.Unmarshal(data, &x); err != nil {
				b.Fatal("Unmarshal:", err)
			}
		}
	})
}

func BenchmarkIssue10335(b *testing.B) {
	b.ReportAllocs()
	j := []byte(`{"a":{ }}`)
	b.RunParallel(func(pb *testing.PB) {
		var s struct{}
		for pb.Next() {
			if err := Unmarshal(j, &s); err != nil {
				b.Fatal(err)
			}
		}
	})
}
func BenchmarkOldIssue10335(b *testing.B) {
	b.ReportAllocs()
	j := []byte(`{"a":{ }}`)
	b.RunParallel(func(pb *testing.PB) {
		var s struct{}
		for pb.Next() {
			if err := json.Unmarshal(j, &s); err != nil {
				b.Fatal(err)
			}
		}
	})
}

func BenchmarkUnmapped(b *testing.B) {
	b.ReportAllocs()
	j := []byte(`{"s": "hello", "y": 2, "o": {"x": 0}, "a": [1, 99, {"x": 1}]}`)
	b.RunParallel(func(pb *testing.PB) {
		var s struct{}
		for pb.Next() {
			if err := Unmarshal(j, &s); err != nil {
				b.Fatal(err)
			}
		}
	})
}

func BenchmarkOldUnmapped(b *testing.B) {
	b.ReportAllocs()
	j := []byte(`{"s": "hello", "y": 2, "o": {"x": 0}, "a": [1, 99, {"x": 1}]}`)
	b.RunParallel(func(pb *testing.PB) {
		var s struct{}
		for pb.Next() {
			if err := json.Unmarshal(j, &s); err != nil {
				b.Fatal(err)
			}
		}
	})
}

func BenchmarkEncoderEncode(b *testing.B) {
	b.ReportAllocs()
	type T struct {
		X, Y string
	}
	v := &T{"foo", "bar"}
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			if err := NewEncoder(ioutil.Discard).Encode(v); err != nil {
				b.Fatal(err)
			}
		}
	})
}
func BenchmarkOldEncoderEncode(b *testing.B) {
	b.ReportAllocs()
	type T struct {
		X, Y string
	}
	v := &T{"foo", "bar"}
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			if err := json.NewEncoder(ioutil.Discard).Encode(v); err != nil {
				b.Fatal(err)
			}
		}
	})
}

func BenchmarkSkipValue(b *testing.B) {
	initBig(false)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		nextValue(&benchScan, jsonBig)
	}
	b.SetBytes(int64(len(jsonBig)))
}
