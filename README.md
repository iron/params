# params [![Build Status](https://secure.travis-ci.org/iron/params.png?branch=master)](https://travis-ci.org/iron/params)

A plugin for the [Iron](https://github.com/iron/iron) web framework that parses parameters from incoming requests from the following sources:

* JSON data (`Content-Type: application/json`)
* URL-encoded GET parameters
* URL-encoded `Content-Type: application/x-www-form-urlencoded` parameters
* Multipart form data (`Content-Type: multipart/form-data`)

This plugin combines all request parameters from these sources into a single `params::Map` accessible through any Iron request using `req.get_ref::<params::Params>()`. Working example:

```rust
// Visit `http://127.0.0.1:3000/?user[name]=Marie` to be greeted with a welcome message. Any other
// request will return a 404 error.

extern crate iron;
extern crate params;

use iron::prelude::*;

fn handle_user(req: &mut Request) -> IronResult<Response> {
    use params::{Params, Value};

    let map = req.get_ref::<Params>().unwrap();

    match map.find(&["user", "name"]) {
        Some(&Value::String(ref name)) if name == "Marie" => {
            Ok(Response::with((iron::status::Ok, "Welcome back, Marie!")))
        },
        _ => Ok(Response::with(iron::status::NotFound)),
    }
}

fn main() {
    Iron::new(handle_user).http("127.0.0.1:3000").unwrap();
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
