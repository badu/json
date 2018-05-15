/*
 * Copyright 2009-2018 The Go Authors. All rights reserved.
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file.
 */

package json

import (
	"fmt"
	"io"
	"log"
	"os"
	"strings"
)

func ExampleMarshal() {
	type ColorGroup struct {
		ID     int
		Name   string
		Colors []string
	}
	group := ColorGroup{
		ID:     1,
		Name:   "Reds",
		Colors: []string{"Crimson", "Red", "Ruby", "Maroon"},
	}
	b, err := Marshal(group)
	if err != nil {
		fmt.Println("error:", err)
	}
	os.Stdout.Write(b)
	// Output:
	// {"ID":1,"Name":"Reds","Colors":["Crimson","Red","Ruby","Maroon"]}
}

func ExampleUnmarshal() {
	var jsonBlob = []byte(`[
	{"Name": "Platypus", "Order": "Monotremata"},
	{"Name": "Quoll",    "Order": "Dasyuromorphia"}
]`)
	type Animal struct {
		Name  string
		Order string
	}
	var animals []Animal
	err := Unmarshal(jsonBlob, &animals)
	if err != nil {
		fmt.Println("error:", err)
	}
	fmt.Printf("%+v", animals)
	// Output:
	// [{Name:Platypus Order:Monotremata} {Name:Quoll Order:Dasyuromorphia}]
}

// This example uses a Decoder to decode a stream of distinct JSON values.
func ExampleDecoder() {
	const jsonStream = `
	{"Name": "Ed", "Text": "Knock knock."}
	{"Name": "Sam", "Text": "Who's there?"}
	{"Name": "Ed", "Text": "Go fmt."}
	{"Name": "Sam", "Text": "Go fmt who?"}
	{"Name": "Ed", "Text": "Go fmt yourself!"}
`
	type Message struct {
		Name, Text string
	}
	dec := NewDecoder(strings.NewReader(jsonStream))
	for {
		var m Message
		if err := dec.Decode(&m); err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("%s: %s\n", m.Name, m.Text)
	}
	// Output:
	// Ed: Knock knock.
	// Sam: Who's there?
	// Ed: Go fmt.
	// Sam: Go fmt who?
	// Ed: Go fmt yourself!
}

// This example uses a Decoder to decode a stream of distinct JSON values.
func ExampleDecoder_Token() {
	const jsonStream = `
	{"Message": "Hello", "Array": [1, 2, 3], "Null": null, "Number": 1.234}
`
	dec := NewDecoder(strings.NewReader(jsonStream))
	for {
		t, err := dec.Token()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatal(err)
		}
		switch t {
		case curlOpen:
			fmt.Print("{")
		case curlClose:
			fmt.Print("}")
		case squareOpen:
			fmt.Print("[")
		case squareClose:
			fmt.Print("]")
		default:
			fmt.Printf("%T: %v", t, t)
		}
		if dec.More() {
			fmt.Printf(" (more)")
		}
		fmt.Printf("\n")
	}
	// Output:
	// { (more)
	// string: Message (more)
	// string: Hello (more)
	// string: Array (more)
	// [ (more)
	// float64: 1 (more)
	// float64: 2 (more)
	// float64: 3
	// ] (more)
	// string: Null (more)
	// <nil>: <nil> (more)
	// string: Number (more)
	// float64: 1.234
	// }
}

// This example uses a Decoder to decode a streaming array of JSON objects.
func ExampleDecoder_Decode_stream() {
	const jsonStream = `
	[
		{"Name": "Ed", "Text": "Knock knock."},
		{"Name": "Sam", "Text": "Who's there?"},
		{"Name": "Ed", "Text": "Go fmt."},
		{"Name": "Sam", "Text": "Go fmt who?"},
		{"Name": "Ed", "Text": "Go fmt yourself!"}
	]
`
	type Message struct {
		Name, Text string
	}
	dec := NewDecoder(strings.NewReader(jsonStream))

	// read open bracket
	t, err := dec.Token()
	if err != nil {
		log.Fatal(err)
	}
	switch t {
	case squareOpen:
		fmt.Print("[\n")
	default:
		panic("Expecting square bracket open.")
	}

	// while the array contains values
	for dec.More() {
		var m Message
		// decode an array value (Message)
		err := dec.Decode(&m)
		if err != nil {
			log.Fatal(err)
		}

		fmt.Printf("%v: %v\n", m.Name, m.Text)
	}

	// read closing bracket
	t, err = dec.Token()
	if err != nil {
		log.Fatal(err)
	}
	switch t {
	case squareClose:
		fmt.Print("]\n")
	default:
		panic("Expecting square bracket close.")
	}
	// Output:
	// [
	// Ed: Knock knock.
	// Sam: Who's there?
	// Ed: Go fmt.
	// Sam: Go fmt who?
	// Ed: Go fmt yourself!
	// ]

}

// This example uses RawMessage to delay parsing part of a JSON message.
func ExampleRawMessage_unmarshal() {
	type Color struct {
		Space string
		Point RawMessage // delay parsing until we know the color space
	}
	type RGB struct {
		R uint8
		G uint8
		B uint8
	}
	type YCbCr struct {
		Y  uint8
		Cb int8
		Cr int8
	}

	var j = []byte(`[
	{"Space": "YCbCr", "Point": {"Y": 255, "Cb": 0, "Cr": -10}},
	{"Space": "RGB",   "Point": {"R": 98, "G": 218, "B": 255}}
]`)
	var colors []Color
	err := Unmarshal(j, &colors)
	if err != nil {
		log.Fatalln("error:", err)
	}

	for _, c := range colors {
		var dst interface{}
		switch c.Space {
		case "RGB":
			dst = new(RGB)
		case "YCbCr":
			dst = new(YCbCr)
		}
		err := Unmarshal(c.Point, dst)
		if err != nil {
			log.Fatalln("error:", err)
		}
		fmt.Println(c.Space, dst)
	}
	// Output:
	// YCbCr &{255 0 -10}
	// RGB &{98 218 255}
}

// This example uses RawMessage to use a precomputed JSON during marshal.
func ExampleRawMessage_marshal() {
	h := RawMessage(`{"precomputed": true}`)

	c := struct {
		Header *RawMessage `json:"header"`
		Body   string      `json:"body"`
	}{Header: &h, Body: "Hello Gophers!"}

	b, err := MarshalIndent(&c, "", "\t")
	if err != nil {
		fmt.Println("error:", err)
	}
	os.Stdout.Write(b)

	// Output:
	// {
	// 	"header": {
	// 		"precomputed": true
	// 	},
	// 	"body": "Hello Gophers!"
	// }
}

func ExampleIndent() {
	type Road struct {
		Name   string
		Number int
	}
	roads := []Road{
		{"Diamond Fork", 29},
		{"Sheep Creek", 51},
	}

	b, err := Marshal(roads)
	if err != nil {
		log.Fatal(err)
	}

	var out Buffer
	Indent(&out, b, "=", "\t")
	out.Bytes() // force write peding byte
	out.WriteTo(os.Stdout)
	// Output:
	// [
	// =	{
	// =		"Name": "Diamond Fork",
	// =		"Number": 29
	// =	},
	// =	{
	// =		"Name": "Sheep Creek",
	// =		"Number": 51
	// =	}
	// =]
}

func ExampleMarshalIndent() {
	data := map[string]int{
		"a": 1,
		"b": 2,
	}

	json, err := MarshalIndent(data, "<prefix>", "<indent>")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(string(json))
	// Output:
	// {
	// <prefix><indent>"a": 1,
	// <prefix><indent>"b": 2
	// <prefix>}
}

type Animal int

const (
	Unknown Animal = iota
	Gopher
	Zebra
)

func (a *Animal) UnmarshalJSON(b []byte) error {
	var s string
	if err := Unmarshal(b, &s); err != nil {
		return err
	}
	switch strings.ToLower(s) {
	default:
		*a = Unknown
	case "gopher":
		*a = Gopher
	case "zebra":
		*a = Zebra
	}

	return nil
}

func (a Animal) MarshalJSON() ([]byte, error) {
	var s string
	switch a {
	default:
		s = "unknown"
	case Gopher:
		s = "gopher"
	case Zebra:
		s = "zebra"
	}

	return Marshal(s)
}

func Example_customMarshalJSON() {
	blob := `["gopher","armadillo","zebra","unknown","gopher","bee","gopher","zebra"]`
	var zoo []Animal
	if err := Unmarshal([]byte(blob), &zoo); err != nil {
		log.Fatal(err)
	}

	census := make(map[Animal]int)
	for _, animal := range zoo {
		census[animal] += 1
	}

	fmt.Printf("Zoo Census:\n* Gophers: %d\n* Zebras:  %d\n* Unknown: %d\n",
		census[Gopher], census[Zebra], census[Unknown])

	// Output:
	// Zoo Census:
	// * Gophers: 3
	// * Zebras:  2
	// * Unknown: 3
}
