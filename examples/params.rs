extern crate iron;
extern crate params;

use iron::prelude::*;
use iron::status;
use params::Params;

fn handle(req: &mut Request) -> IronResult<Response> {
    println!("{:?}", req.get_ref::<Params>());
    Ok(Response::with(status::Ok))
}

// Execute the following cURL requests and watch your terminal for the parsed parameters.
//
// `curl -i "localhost:3000" -H "Content-Type: application/json" -d '{"name":"jason","age":2}'`
// => Ok({"age": U64(2), "name": String("jason")})
//
// `curl -i -X POST "http://localhost:3000/" --data "fruit=apple&name=iron&fruit=pear"`
// => Ok({"fruit": String("pear"), "name": String("iron")})
//
// `curl -i "http://localhost:3000/?x\[\]=1&x\[\]=2" -F "images[]=@/path/to/file.jpg"`
// => Ok({
//        "images": Array([File(File {
//            path: "/tmp/path/to/file.jpg",
//            filename: Some("file.jpg"),
//            content_type: Mime(Image, Jpeg, []),
//            size: 1234
//        })]),
//        "x": Array([String("1"), String("2")])
//    })
//
// `curl -i -X POST "http://localhost:3000/" --data "x[][]=2"`
// => Err(InvalidPath)
fn main() {
    Iron::new(Chain::new(handle)).http("localhost:3000").unwrap();
}
