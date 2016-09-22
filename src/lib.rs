//! Request parameters parser plugin for Iron
//!
//! This plugin is a multi-source request parameters parser.

extern crate bodyparser;
extern crate iron;
extern crate multipart;
extern crate num;
extern crate plugin;
#[cfg(feature = "serde")]
extern crate serde as serde_crate;
extern crate serde_json;
extern crate urlencoded;
extern crate tempdir;

#[cfg(test)]
#[macro_use]
extern crate maplit;

mod conversion;
#[cfg(feature = "serde")]
pub mod serde;

use multipart::server::{Multipart, MultipartData, SaveDir};
use iron::{headers, mime, Request};
use iron::mime::Mime;
use iron::request::Body;
use iron::typemap::Key;
use plugin::{Extensible, Pluggable, Plugin};
use serde_json::value::Value as Json;
use std::collections::BTreeMap;
use std::error::Error as StdError;
use std::{fmt, fs};
use std::io;
use std::ops::{Deref, DerefMut};
use std::path::Path;
use tempdir::TempDir;
pub use conversion::FromValue;

/// A representation of all possible types of request parameters.
#[derive(Clone, PartialEq)]
pub enum Value {
    /// A `null` from the request's JSON body.
    Null,
    /// A `true` or `false` from the request's JSON body.
    Boolean(bool),
    /// A signed integer from the request's JSON body.
    I64(i64),
    /// An unsigned integer from the request's JSON body.
    U64(u64),
    /// A floating point number from the request's JSON body.
    F64(f64),
    /// Either a string from the request's JSON body or a plain text value from the request's URL
    /// (GET) parameters, `application/x-www-form-urlencoded` parameters, or `multipart/form-data`
    /// parameters.
    String(String),
    /// A temporary file processed from a `multipart/form-data` request.
    File(File),
    /// Either an array of JSON values or an amalgamation of URL (GET) parameters,
    /// `application/x-www-form-urlencoded` parameters, or `multipart/form-data` parameters. Such
    /// parameters should be in the form `my_list[]=1&my_list[]=2` to be processed into this
    /// variant.
    Array(Vec<Value>),
    /// Either an object of JSON keys and values or an amalgamation of URL (GET) parameters,
    /// `application/x-www-form-urlencoded` parameters, or `multipart/form-data` parameters. Such
    /// parameters should be in the form `my_map[x]=1&my_map[y]=2` to be processed into this
    /// variant.
    Map(Map),
}

impl Value {
    fn assign(&mut self, path: &str, value: Value) -> Result<(), ParamsError> {
        assert!(!path.is_empty());

        let (key, remainder) = try!(eat_index(path));

        if key.is_empty() {
            match *self {
                Value::Array(ref mut array) => {
                    if remainder.is_empty() {
                        array.push(value);
                        return Ok(());
                    }

                    let (next_key, _) = try!(eat_index(remainder));
                    if next_key.is_empty() {
                        // Two array indices in a row ("[][]") is illegal.
                        return Err(InvalidPath);
                    }

                    if let Some(map) = array.last_mut() {
                        if !try!(map.contains_key(next_key)) {
                            return map.assign(remainder, value);
                        }
                    }

                    let mut map = Value::Map(Map::new());
                    try!(map.assign(remainder, value));
                    array.push(map);
                    Ok(())
                },
                _ => Err(CannotAppend),
            }
        } else {
            match *self {
                Value::Map(ref mut map) => {
                    if remainder.is_empty() {
                        map.0.insert(String::from(key), value);
                        return Ok(());
                    }

                    let (next_key, _) = try!(eat_index(remainder));
                    let collection = map.0.entry(String::from(key)).or_insert_with(|| {
                        if next_key.is_empty() {
                            Value::Array(vec![])
                        } else {
                            Value::Map(Map::new())
                        }
                    });

                    collection.assign(remainder, value)
                },
                _ => Err(CannotInsert),
            }
        }
    }

    fn contains_key(&mut self, key: &str) -> Result<bool, ParamsError> {
        match *self {
            Value::Map(ref map) => Ok(map.contains_key(key)),
            _ => Err(CannotInsert),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Null => f.write_str("null"),
            Value::Boolean(value) => value.fmt(f),
            Value::I64(value) => value.fmt(f),
            Value::U64(value) => value.fmt(f),
            Value::F64(value) => value.fmt(f),
            Value::String(ref value) => value.fmt(f),
            Value::File(ref value) => value.fmt(f),
            Value::Array(ref value) => value.fmt(f),
            Value::Map(ref value) => value.fmt(f),
        }
    }
}

/// An uploaded file that was received as part of `multipart/form-data` parsing.
///
/// Files are streamed to disk because they may not fit in memory.
pub struct File(multipart::server::SavedFile);

impl File {
    /// Attempts to open the file in read-only mode.
    pub fn open(&self) -> io::Result<fs::File> {
        fs::File::open(&self.0.path)
    }

    /// The path to the temporary file where the data was saved.
    pub fn path(&self) -> &Path {
        self.0.path.as_path()
    }

    /// The filename that was specified in the request, unfiltered.
    ///
    /// ## Warning
    ///
    /// This may or may not be legal on the local filesystem, and so you should *not* blindly
    /// append it to a `Path`, as such behavior could easily be exploited by a malicious client.
    pub fn filename(&self) -> Option<&str> {
        self.0.filename.as_ref().map(|f| &**f)
    }

    /// The size of the file, in bytes.
    pub fn size(&self) -> u64 {
        self.0.size
    }
}

/// This implementation does not copy or modify the underlying file.
impl Clone for File {
    fn clone(&self) -> File {
        File(multipart::server::SavedFile {
            path: self.0.path.clone(),
            filename: self.0.filename.clone(),
            size: self.0.size,
        })
    }
}

impl fmt::Debug for File {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("File")
            .field("path", &self.0.path)
            .field("filename", &self.0.filename)
            .field("size", &self.0.size)
            .finish()
    }
}

/// Checks only for equality of the file's path.
impl PartialEq for File {
    fn eq(&self, other: &File) -> bool {
        self.0.path == other.0.path
    }
}

impl From<multipart::server::SavedFile> for File {
    fn from(file: multipart::server::SavedFile) -> File {
        File(file)
    }
}

/// A type that maps keys to request parameter values.
#[derive(Clone, PartialEq)]
pub struct Map(pub BTreeMap<String, Value>);

impl Map {
    /// Creates an empty map.
    pub fn new() -> Map {
        Map(BTreeMap::new())
    }

    /// Inserts a parameter value to the specified key path.
    ///
    /// Key paths are a series of values starting with a plain name and followed by zero or more
    /// array-like indices. `name` is "the value called `name`", `names[]` is "the array called
    /// `names`", `pts[][x]` and `pts[][y]` are the `x` and `y` values of a map at the end of
    /// an array.
    ///
    /// This method is used during the internal parsing processes and is only made public in the
    /// name of hypothetical extensibility.
    ///
    /// # Examples
    ///
    /// ```
    /// # use params::{Map, Value};
    /// let mut map = Map::new();
    /// map.assign("name", Value::String("Callie".into())).unwrap();
    /// assert_eq!(format!("{:?}", map), r#"{"name": "Callie"}"#);
    /// ```
    ///
    /// ```
    /// # use params::{Map, Value};
    /// let mut map = Map::new();
    /// map.assign("names[]", Value::String("Anne".into())).unwrap();
    /// map.assign("names[]", Value::String("Bob".into())).unwrap();
    /// assert_eq!(format!("{:?}", map), r#"{"names": ["Anne", "Bob"]}"#);
    /// ```
    ///
    /// ```
    /// # use params::{Map, Value};
    /// let mut map = Map::new();
    /// map.assign("pts[][x]", Value::I64(3)).unwrap();
    /// map.assign("pts[][y]", Value::I64(9)).unwrap();
    /// assert_eq!(format!("{:?}", map), r#"{"pts": [{"x": 3, "y": 9}]}"#);
    /// ```
    pub fn assign(&mut self, path: &str, value: Value) -> Result<(), ParamsError> {
        let (base, remainder) = try!(eat_base(path));
        if remainder.is_empty() {
            self.0.insert(String::from(base), value);
            return Ok(());
        }

        let (key, _) = try!(eat_index(remainder));
        let collection = self.0.entry(String::from(base)).or_insert_with(|| {
            if key.is_empty() {
                Value::Array(vec![])
            } else {
                Value::Map(Map::new())
            }
        });

        try!(collection.assign(remainder, value));

        Ok(())
    }

    /// Traverses nested `Map`s to find the specified value by key.
    ///
    /// # Examples
    ///
    /// ```
    /// # use params::{Map, Value};
    /// let mut map = Map::new();
    /// map.assign("user[name]", Value::String("Marie".into())).unwrap();
    ///
    /// match map.find(&["user", "name"]) {
    ///     Some(&Value::String(ref name)) => assert_eq!(name, "Marie"),
    ///     _ => panic!("Unexpected parameter type!"),
    /// }
    ///
    /// assert!(map.find(&["user", "age"]).is_none());
    /// ```
    pub fn find(&self, keys: &[&str]) -> Option<&Value> {
        if keys.is_empty() {
            return None;
        }

        let mut value = self.0.get(keys[0]);

        for key in &keys[1..] {
            value = match value {
                Some(&Value::Map(ref map)) => map.0.get(*key),
                _ => return None,
            }
        }

        value
    }

    /// Converts this map to a raw `BTreeMap` with values of all the same type.
    ///
    /// ```
    /// # use params::{Map, Value};
    /// # use std::collections::BTreeMap;
    /// let mut map = Map::new();
    /// map.assign("x", Value::String("10.5".into())).unwrap();
    /// map.assign("y", Value::I64(15)).unwrap();
    ///
    /// let mut expected = BTreeMap::new();
    /// expected.insert(String::from("x"), 10.5f32);
    /// expected.insert(String::from("y"), 15.0f32);
    ///
    /// assert_eq!(map.to_strict_map::<f32>(), Some(expected));
    /// ```
    pub fn to_strict_map<T: FromValue>(&self) -> Option<BTreeMap<String, T>> {
        let mut map = BTreeMap::new();

        for (key, value) in &self.0 {
            if let Some(converted_value) = T::from_value(value) {
                map.insert(key.clone(), converted_value);
            } else {
                return None;
            }
        }

        Some(map)
    }
}

impl Deref for Map {
    type Target = BTreeMap<String, Value>;

    fn deref(&self) -> &BTreeMap<String, Value> {
        &self.0
    }
}

impl DerefMut for Map {
    fn deref_mut(&mut self) -> &mut BTreeMap<String, Value> {
        &mut self.0
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

fn eat_base<'a>(path: &'a str) -> Result<(&'a str, &'a str), ParamsError> {
    let open = path.char_indices().find(|&(_, c)| c == '[').map(|(i, _)| i).unwrap_or(path.len());
    let base = &path[..open];
    let remainder = &path[open..];
    match base.is_empty() {
        false => Ok((base, remainder)),
        true => Err(InvalidPath),
    }
}

#[test]
fn test_eat_base() {
    assert_eq!(eat_base("before[after]").unwrap(), ("before", "[after]"));
    assert!(eat_base("[][something]").is_err());
}

fn eat_index<'a>(path: &'a str) -> Result<(&'a str, &'a str), ParamsError> {
    if path.chars().next() != Some('[') {
        return Err(InvalidPath);
    }

    let index = path.char_indices().skip(1).find(|&(_, c)| c == ']');
    let close = try!(index.ok_or(InvalidPath)).0;
    let key = &path[1..close];
    let remainder = &path[1 + close..];

    Ok((key, remainder))
}

#[test]
fn test_eat_index() {
    assert_eq!(eat_index("[something][fishy]").unwrap(), ("something", "[fishy]"));
    assert_eq!(eat_index("[][something]").unwrap(), ("", "[something]"));
    assert!(eat_index("invalid[]").is_err());
}

/// An error representing any of the possible errors that can occur during parameter processing.
#[derive(Debug)]
pub enum ParamsError {
    /// An error from parsing the request body.
    BodyError(bodyparser::BodyError),
    /// An error from parsing URL encoded data.
    UrlDecodingError(urlencoded::UrlDecodingError),
    /// An I/O error from reading a `multipart/form-data` request body to temporary files.
    IoError(io::Error),
    /// Invalid parameter path.
    InvalidPath,
    /// Tried to append to a non-array value.
    CannotAppend,
    /// Tried to insert into a non-map value.
    CannotInsert,
    /// Tried to make a `Map` from a non-object root JSON value.
    NotJsonObject,
}

use ParamsError::*;

impl fmt::Display for ParamsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.description().fmt(f)
    }
}

impl StdError for ParamsError {
    fn description(&self) -> &str {
        match *self {
            BodyError(ref err) => err.description(),
            UrlDecodingError(ref err) => err.description(),
            IoError(ref err) => err.description(),
            InvalidPath => "Invalid parameter path.",
            CannotAppend => "Cannot append to a non-array value.",
            CannotInsert => "Cannot insert into a non-map value.",
            NotJsonObject => "Tried to make a `Map` from a non-object root JSON value.",
        }
    }

    fn cause(&self) -> Option<&StdError> {
        match *self {
            BodyError(ref err) => Some(err),
            UrlDecodingError(ref err) => Some(err),
            IoError(ref err) => Some(err),
            _ => None,
        }
    }
}

impl From<bodyparser::BodyError> for ParamsError {
    fn from(err: bodyparser::BodyError) -> ParamsError {
        BodyError(err)
    }
}

impl From<io::Error> for ParamsError {
    fn from(err: io::Error) -> ParamsError {
        IoError(err)
    }
}

/// Plugin for `iron::Request` that processes and combines request parameters from the various
/// request sources.
///
/// The following sources are merged into a `Map`:
///
/// * JSON data (`Content-Type: application/json`)
/// * URL-encoded GET parameters
/// * URL-encoded `Content-Type: application/x-www-form-urlencoded` parameters
/// * Multipart form data (`Content-Type: multipart/form-data`)
///
/// Use `req.get_ref::<Params>()` where `req` is an `iron::Request` to get a `Map` of the request's
/// parameters.
pub struct Params;

impl Key for Params {
    type Value = Map;
}

impl<'a, 'b> Plugin<Request<'a, 'b>> for Params {
    type Error = ParamsError;

    fn eval(req: &mut Request) -> Result<Map, ParamsError> {
        let mut map = try!(try_parse_json_into_map(req));
        let has_json_body = !map.is_empty();
        if let Some(dir) = try!(try_parse_multipart(req, &mut map)) {
            append_multipart_save_dir(req, dir);
        }
        try!(try_parse_url_encoded::<urlencoded::UrlEncodedQuery>(req, &mut map));
        if !has_json_body {
            try!(try_parse_url_encoded::<urlencoded::UrlEncodedBody>(req, &mut map));
        }

        Ok(map)
    }
}

fn try_parse_json_into_map(req: &mut Request) -> Result<Map, ParamsError> {
    let need_parse = req.headers.get::<headers::ContentType>().map(|header| {
        match **header {
            Mime(mime::TopLevel::Application, mime::SubLevel::Json, _) => true,
            _ => false
        }
    }).unwrap_or(false);

    if !need_parse {
        return Ok(Map::new());
    }

    match *try!(req.get_ref::<bodyparser::Json>()) {
        Some(ref json) => json.to_map(),
        None => Ok(Map::new()),
    }
}

trait ToParams {
    fn to_map(&self) -> Result<Map, ParamsError>;
    fn to_value(&self) -> Result<Value, ParamsError>;
}

impl ToParams for Json {
    fn to_map(&self) -> Result<Map, ParamsError> {
        match try!(self.to_value()) {
            Value::Map(map) => Ok(map),
            _ => Err(NotJsonObject),
        }
    }

    fn to_value(&self) -> Result<Value, ParamsError> {
        match *self {
            Json::I64(value) => Ok(Value::I64(value)),
            Json::U64(value) => Ok(Value::U64(value)),
            Json::F64(value) => Ok(Value::F64(value)),
            Json::String(ref value) => Ok(Value::String(value.clone())),
            Json::Bool(value) => Ok(Value::Boolean(value)),
            Json::Null => Ok(Value::Null),
            Json::Array(ref value) => {
                let result = value.iter().map(|v| v.to_value()).collect();
                Ok(Value::Array(try!(result)))
            },
            Json::Object(ref value) => {
                let mut result = Map::new();
                for (key, json) in value {
                    result.insert(key.clone(), try!(json.to_value()));
                }

                Ok(Value::Map(result))
            },
        }
    }
}

fn try_parse_multipart(req: &mut Request, map: &mut Map)
    -> Result<Option<multipart::server::SaveDir>, ParamsError>
{
    // This is a wrapper that allows an Iron request to be processed by the `multipart` crate. Its
    // implementation of `multipart::server::HttpRequest` is taken from `multipart` itself, which
    // requires the `iron` crate feature on compilation. To minimize a messy dependency graph that
    // is historically proven to cause versioning nightmares, `params` does not make use of the
    // feature.
    struct MultipartIronRequest<'r, 'a: 'r, 'b: 'a>(&'r mut Request<'a, 'b>);

    impl<'r, 'a, 'b> multipart::server::HttpRequest for MultipartIronRequest<'r, 'a, 'b> {
        type Body = &'r mut Body<'a, 'b>;

        fn multipart_boundary(&self) -> Option<&str> {
            let content_type = match self.0.headers.get::<headers::ContentType>() {
                Some(content_type) => content_type,
                None => return None,
            };

            if let Mime(mime::TopLevel::Multipart, mime::SubLevel::FormData, _) = **content_type {
                content_type.get_param("boundary").map(|b| b.as_str())
            } else {
                None
            }
        }

        fn body(self) -> &'r mut Body<'a, 'b> {
            &mut self.0.body
        }
    }

    let mut multipart = match Multipart::from_request(MultipartIronRequest(req)) {
        Ok(multipart) => multipart,
        Err(_) => return Ok(None),
    };

    let mut temp_dir = None;

    while let Some(field) = try!(multipart.read_entry()) {
        match field.data {
            MultipartData::Text(text) => {
                try!(map.assign(&field.name, Value::String(String::from(text))));
            },
            MultipartData::File(mut file) => {
                if temp_dir.is_none() {
                    temp_dir = Some(try!(TempDir::new("multipart")));
                }
                let saved_file = try!(file.save_in(temp_dir.as_ref().unwrap().path()));
                try!(map.assign(&field.name, Value::File(saved_file.into())));
            },
        }
    }

    Ok(temp_dir.map(|dir| SaveDir::Temp(dir)))
}

fn append_multipart_save_dir(req: &mut Request, dir: multipart::server::SaveDir) {
    let mut extensions = req.extensions_mut();

    if !extensions.contains::<SaveDirExt>() {
        extensions.insert::<SaveDirExt>(vec![]);
    }

    extensions.get_mut::<SaveDirExt>().unwrap().push(dir);

    struct SaveDirExt;

    impl Key for SaveDirExt {
        type Value = Vec<multipart::server::SaveDir>;
    }
}

fn try_parse_url_encoded<'a, 'b, P>(req: &mut Request<'a, 'b>, map: &mut Map)
    -> Result<(), ParamsError>
    where P: Plugin<Request<'a, 'b>, Error=urlencoded::UrlDecodingError>,
          P: Key<Value=urlencoded::QueryMap>
{
    let hash_map = match req.get::<P>() {
        Ok(hash_map) => hash_map,
        Err(urlencoded::UrlDecodingError::EmptyQuery) => return Ok(()),
        Err(err) => return Err(UrlDecodingError(err)),
    };

    for (path, vec) in hash_map {
        for value in vec {
            try!(map.assign(&path, Value::String(value)));
        }
    }

    Ok(())
}
