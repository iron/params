# params

A plugin for the [Iron](https://github.com/iron/iron) web framework that parses parameters from incoming requests from the following sources:

* JSON data (`Content-Type: application/json`)
* URL-encoded GET parameters
* URL-encoded `Content-Type: application/x-www-form-urlencoded` parameters
* Multipart form data (`Content-Type: multipart/form-data`)

See `examples/params.rs` for details.
