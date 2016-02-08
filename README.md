# params

A plugin for the [Iron](https://github.com/iron/iron) web framework that parses parameters from incoming requests from the following sources:

* JSON data (`Content-Type: application/json`)
* URL-encoded GET parameters
* URL-encoded `Content-Type: application/x-www-form-urlencoded` parameters
* Multipart form data (`Content-Type: multipart/form-data`)

This plugin combines all request parameters from these sources into a single `params::Map` accessible through any Iron request using `req.get_ref::<params::Params>()`. Example:

```rust
fn handler(req: &mut Request) -> IronResult<Response> {
    use params::{Map, Params};

    let map: Map = try!(req.get_ref::<Params>());

    match map.find(&["user", "name"]) {
        // Assume, for example, the request URL here is:
        // http://localhost:3000/handler?user[name]=Marie
        Some(&Value::String(ref name)) => assert_eq!(name, "Marie"),
        _ => panic!("Unexpected parameter type!"),
    }
}
```

You can perform custom request parameter deserialization by implementing `params::FromValue` for any `Sized` type. See `src/conversion.rs` for inspiration!

This crate is fully documented, and we're [working on](https://github.com/iron/params/issues/10) hosting it at [ironframework.io](http://ironframework.io).

See `examples/params.rs` for an interactive example.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.
