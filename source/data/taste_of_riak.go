package main

import (
	"fmt"
	"github.com/mrb/riakpbc"
	"log"
	"strconv"
)

// A struct for the demonstrating how to work with complex objects in Riak.
type ExampleData struct {
	Three int `json:"three"`
}

func main() {
	// Create a new client instance to connect to each of the nodes in a locally
	// configured Riak cluster. If you happen to have the cluster behind a load
	// balancer, simply provide the IP and port your load balancer is listening
	// on.
	client := riakpbc.NewClient([]string{
		"127.0.0.1:10017",
		"127.0.0.1:10027",
		"127.0.0.1:10037",
	})

	// Actually connect to each of the nodes in the cluster (or Dial them).
	if err := client.Dial(); err != nil {
		log.Fatalf("Dialing failed: %v", err)
	}

	// Set a client ID for your client.
	if _, err := client.SetClientId("taste-of-riak"); err != nil {
		log.Fatalf("Setting client ID failed: %v", err)
	}

	fmt.Println("Creating objects...")

	// Store the integer 1 with a key of "one".
	if _, err := client.StoreObject("test", "one", 1); err != nil {
		log.Print(err.Error())
	}

	// Store the string "two" with the key of "two".
	if _, err := client.StoreObject("test", "two", "two"); err != nil {
		log.Print(err.Error())
	}

	fmt.Println("Reading objects...")

	// Fetch the key "one" and print its value.
	one, err := client.FetchObject("test", "one")
	if err != nil {
		log.Print(err.Error())
	}
	fmt.Println(string(one.GetContent()[0].GetValue()))

	// Parse the value for "one" into an integer and then compare it to 1.
	one_value, err := strconv.ParseInt(string(one.GetContent()[0].GetValue()), 10, 64)
	if err != nil {
		log.Print(err.Error())
	}
	fmt.Println(one_value == 1)

	// Compare the value for "two" and compare it to "two".
	two, err := client.FetchObject("test", "two")
	if err != nil {
		log.Print(err.Error())
	}
	fmt.Println(string(two.GetContent()[0].GetValue()))
	fmt.Println(string(two.GetContent()[0].GetValue()) == "two")

	fmt.Println("Deleting objects...")

	// Delete the key and value associated with "one" from Riak.
	if _, err := client.DeleteObject("test", "one"); err != nil {
		log.Print(err.Error())
	}

	// Delete the key and value associated with "two" from Riak.
	if _, err := client.DeleteObject("test", "two"); err != nil {
		log.Print(err.Error())
	}

	// Close the client connections.
	client.Close()

	// Create a coder that marshalls/unmarshalls JSON and associate it with a
	// new client instance.
	coder := riakpbc.NewCoder("json", riakpbc.JsonMarshaller, riakpbc.JsonUnmarshaller)
	coder_client := riakpbc.NewClientWithCoder([]string{
		"127.0.0.1:10017",
		"127.0.0.1:10027",
		"127.0.0.1:10037",
	}, coder)

	// Actually connect to each of the nodes in the cluster (or Dial them).
	if err := coder_client.Dial(); err != nil {
		log.Fatalf("Dialing failed: %v", err)
	}

	// Set a client ID for your client.
	if _, err := coder_client.SetClientId("taste-of-riak-coder"); err != nil {
		log.Fatalf("Setting client ID failed: %v", err)
	}

	fmt.Println("Creating, reading, and deleting complex objects...")

	// Create an ExampleData struct.
	data := ExampleData{
		Three: 3,
	}

	// Store the struct in Riak with the key "three".
	if _, err := coder_client.StoreStruct("test", "three", &data); err != nil {
		log.Print(err.Error())
	}

	// Read the key "three" from Riak and access the Three property from the
	// struct.
	out := &ExampleData{}
	if _, err := coder_client.FetchStruct("test", "three", out); err != nil {
		log.Print(err.Error())
	}
	fmt.Println(out.Three)
	fmt.Println(out.Three == 3)

	// Delete the key and value associated with "three" from Riak.
	if _, err := client.DeleteObject("test", "three"); err != nil {
		log.Print(err.Error())
	}

	// Close the client connections.
	coder_client.Close()
}
