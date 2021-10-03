---
title: Richer Error Handling Over gRPC in Go and Rust
date: 2021-10-03
---

Lately, I've been working a fair amount with gRPC as a communication protocol
between services written in Rust and Go. More specifically, as part of a major
rewrite of a couple of our backend services, I've been working on porting several
plain HTTP APIs to work over gRPC (for example, to take advantage of [gRPC streaming RPCs](https://grpc.io/docs/what-is-grpc/core-concepts/#server-streaming-rpc)).

A major part of any RPC service is being able to specify how errors are propagated
from a server back to the client. Out of the box, gRPC provides a fairly
[minimal error model](https://www.grpc.io/docs/guides/error/#standard-error-model),
built around sending back [status codes](https://grpc.github.io/grpc/core/md_doc_statuscodes.html)
(e.g. `OK`, `NOT_FOUND`, `INTERNAL`, etc.), along with human-readable error messages,
as part of a `Status` [protobuf message](https://github.com/grpc/grpc/blob/master/src/proto/grpc/status/status.proto):

```protobuf
package google.rpc;

// The `Status` type defines a logical error model that is
// suitable for different programming environments,
// including REST APIs and RPC APIs.
message Status {
  // A simple error code that can be easily handled by the
  // client. The actual error code is defined by
  // `google.rpc.Code`.
  int32 code = 1;

  // A developer-facing human-readable error message in
  // English. It should both explain the error and offer an
  // actionable resolution to it.
  string message = 2;

  // Additional error information that the client code can
  // use to handle the error, such as retry info or a help
  // link.
  repeated google.protobuf.Any details = 3;
}
```

Typically, gRPC client and server libraries implement wrapper interfaces for `Status`
that are native to the language they're designed for. In my case, I need to encode
information about errors in a [`Status` returned by a Go gRPC server](https://pkg.go.dev/google.golang.org/grpc@v1.41.0/internal/status),
and decode the corresponding [`Status` struct](https://docs.rs/tonic/0.5.2/tonic/struct.Status.html)
from a Rust client that's powered by the [Tonic gRPC library](https://github.com/hyperium/tonic) for Rust
(using [Prost](https://github.com/tokio-rs/prost) under the hood).

In many situations, it'd be nice to send more information about an error, beyond
a simple status code and human-readable message, that can be interpreted programmatically.
That's where the `details` field in the definition of `Status` above comes in. This
field is used in gRPC's [richer error model](https://www.grpc.io/docs/guides/error/#richer-error-model)
to send back arbitrary protobuf messages along with an error `Status`. Internally,
this uses the protobuf [`Any` type](https://github.com/protocolbuffers/protobuf/blob/master/src/google/protobuf/any.proto),
which essentially allows packing and unpacking an arbitrary (typed) message as a
raw blob of bytes.

The Go gRPC server library makes it really easy to attach details to any error
returned by an RPC. However, I spent an embarrassingly long time figuring out how
to decode error details in Tonic. The goal of this blog post is to document how
to do this, so that it might be discoverable by anyone trying to accomplish the
same thing.

### Encoding error details on a Go server

To attach an arbitrary protobuf message to a `Status` in Go, we can simply use
the [`Status.WithDetails`](https://pkg.go.dev/google.golang.org/grpc/status#Status.WithDetails)
method. I originally found this from [Johan Brandhorst's blog](https://jbrandhorst.com/post/grpc-errors/).

```go
st := status.New(codes.Unavailable, "Try again later")
retryInfo := &errdetails.RetryInfo{}
st, _ = st.WithDetails(retryInfo)
```

Most programs will define custom error types to pass around information about errors
internally. Fortunately, the gRPC library makes it easy to convert an internal error
type into a `Status` at the boundaries of your application, by implementing a
`GRPCStatus()` method on your type, which returns a `*Status`. Then, if an RPC
handler returns one of your internal errors, the server library will ensure that it
gets converted to a `Status` in the way that you've specified.

### Decoding error details in Rust (with Tonic)

Now that we've encoded error details on the Go server, the next thing we need to
do is decode them from a Rust client.

The first thing to look at is the [`Status` type](https://github.com/hyperium/tonic/blob/5c1bb90ce82ecf90843a7c959edd7ef8fc280f62/tonic/src/status.rs#L445)
in Tonic, which is returned when an RPC encounters an error.

```rust
impl Status {
    /// Get the opaque error details of this `Status`.
    pub fn details(&self) -> &[u8] {
        &self.details
    }
}
```

There's a method called `details()`, which seems promising, but it returns a `&[u8]`,
that is, a raw array of bytes. We need some way to decode this raw byte array into
some structure, from which we can extract the messages we encoded on the server.

With some [digging](https://github.com/hyperium/tonic/blob/5c1bb90ce82ecf90843a7c959edd7ef8fc280f62/tonic/src/status.rs#L398),
I found out that the `details` field gets populated with raw bytes decoded from base64
obtained from the `grpc-status-details-bin` (trailing) header from the underlying
HTTP/2 response.

This is the part that I initially found confusing, because it's not immediately clear
what the details header actually contains. It turns out that it contains an encoding
of the `Status` protobuf message above. That is, a `Status` has a field called `details`,
which contains a `Status` containing a `details` fields that contains the error details.

To decode the outer `Status` protobuf, you can import the `tonic-types` crate into
your project. Then, you can decode the internal `Any` fields into the actual type that
you want. For example (note the difference between `tonic::Status` and `tonic_types::Status`):

```rust
fn extract_retry_info(
    status: tonic_types::Status
) -> Option<RetryInfo> {
    status
        .details
        // We only consider the first error detail here, but
        // a Status can contain multiple detail fields.
        .first()
        // Any has a type_url field that you can use to
        // dynamically choose which type to decode the value
        // into.
        .and_then(|any| RetryInfo::decode(&*any.value).ok())
}

fn process_grpc_status(status: tonic::Status) {
    let retry_info: Option<RetryInfo> =
        tonic_types::Status::decode(status.details())
            .ok()
            .and_then(extract_retry_info);

    if let Some(retry_info) = retry_info {
        // Do something with retry_info...
    }
}
```

There you go! By replacing `RetryInfo` with any protobuf message we want, we can
extend gRPC's error handling model to pass back arbitrary information to help
clients respond to errors.[^1]

[^1]: There are some limitations, namely on the size of headers, noted on Google's
[guidelines for handling errors over gRPC](https://cloud.google.com/apis/design/errors#error_model).

