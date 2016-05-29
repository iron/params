use serde_crate::de;
use serde_crate::de::value::Error;
use serde_crate::de::value::ValueDeserializer as SerdeValueDeserializer;
use std::collections::btree_map::Iter as BTreeMapIter;
use std::iter::Peekable;
use super::Value;

struct ValueDeserializer<'a>(&'a Value);

type DeResult<T> = Result<T, de::value::Error>;

macro_rules! impl_primitive {
    ($ty:ty, $impl_method:ident, $visitor_method:ident, $req_type:ident) => {
        #[inline]
        fn $impl_method<V: ::serde_crate::de::Visitor>(&mut self, mut visitor: V)
            -> Result<V::Value, ::serde_crate::de::value::Error>
        {
            match <$ty as ::FromValue>::from_value(&self.0) {
                Some(v) => visitor.$visitor_method(v),
                None => Err(Error::InvalidType(de::Type::$req_type)),
            }
        }
    }
}

impl<'a> de::Deserializer for ValueDeserializer<'a> {
    type Error = Error;

    #[inline]
    fn deserialize<V: de::Visitor>(&mut self, _v: V) -> Result<V::Value, Error> {
        let message = "Deserializer::deserialize is not supported";
        Err(Error::Custom(message.into()))
    }

    impl_primitive!(bool, deserialize_bool, visit_bool, Bool);
    impl_primitive!(u8, deserialize_u8, visit_u8, U8);
    impl_primitive!(u16, deserialize_u16, visit_u16, U16);
    impl_primitive!(u32, deserialize_u32, visit_u32, U32);
    impl_primitive!(u64, deserialize_u64, visit_u64, U64);
    impl_primitive!(usize, deserialize_usize, visit_usize, Usize);
    impl_primitive!(i8, deserialize_i8, visit_i8, I8);
    impl_primitive!(i16, deserialize_i16, visit_i16, I16);
    impl_primitive!(i32, deserialize_i32, visit_i32, I32);
    impl_primitive!(i64, deserialize_i64, visit_i64, I64);
    impl_primitive!(isize, deserialize_isize, visit_isize, Isize);
    impl_primitive!(f32, deserialize_f32, visit_f32, F32);
    impl_primitive!(f64, deserialize_f64, visit_f64, F64);
    impl_primitive!(String, deserialize_string, visit_string, String);

    fn deserialize_option<V: de::Visitor>(&mut self, mut visitor: V) -> Result<V::Value, Error> {
        match *self.0 {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_seq<V: de::Visitor>(&mut self, mut visitor: V) -> Result<V::Value, Error> {
        struct SeqVisitor<'a> {
            vec: &'a Vec<Value>,
            index: usize,
        }

        impl<'a> de::SeqVisitor for SeqVisitor<'a> {
            type Error = Error;

            fn visit<T: de::Deserialize>(&mut self) -> Result<Option<T>, Self::Error> {
                if self.index < self.vec.len() {
                    let mut deserializer = ValueDeserializer(&self.vec[self.index]);
                    self.index += 1;
                    Ok(Some(try!(de::Deserialize::deserialize(&mut deserializer))))
                } else {
                    Ok(None)
                }
            }

            fn end(&mut self) -> Result<(), Self::Error> {
                if self.index == self.vec.len() {
                    Ok(())
                } else {
                    Err(Error::InvalidLength(self.vec.len()))
                }
            }
        }

        match *self.0 {
            Value::Array(ref v) => visitor.visit_seq(SeqVisitor { vec: v, index: 0 }),
            _ => Err(Error::InvalidType(de::Type::Seq)),
        }
    }

    fn deserialize_map<V: de::Visitor>(&mut self, mut visitor: V) -> Result<V::Value, Error> {
        struct MapVisitor<'a> {
            iter: Peekable<BTreeMapIter<'a, String, Value>>,
        }

        impl<'a> de::MapVisitor for MapVisitor<'a> {
            type Error = Error;

            fn visit_key<K: de::Deserialize>(&mut self) -> Result<Option<K>, Self::Error> {
                if let Some(v) = self.iter.peek() {
                    let mut str_deserializer = v.0.as_str().into_deserializer();
                    Ok(Some(try!(de::Deserialize::deserialize(&mut str_deserializer))))
                } else {
                    Ok(None)
                }
            }

            fn visit_value<V: de::Deserialize>(&mut self) -> Result<V, Self::Error> {
                if let Some(v) = self.iter.next() {
                    let mut deserializer = ValueDeserializer(v.1);
                    Ok(try!(de::Deserialize::deserialize(&mut deserializer)))
                } else {
                    // This is unreachable; how could a `visit_key` return `Ok(Some(...))` if the
                    // iterator has reached its end?
                    //
                    // Even though panicking would make sense here, Serde's end-of-stream error is
                    // too perfect to pass up!
                    Err(Error::EndOfStream)
                }
            }

            fn end(&mut self) -> Result<(), Self::Error> {
                if let None = self.iter.peek() {
                    Ok(())
                } else {
                    Err(Error::InvalidLength(self.iter.len()))
                }
            }
        }

        match *self.0 {
            Value::Map(ref m) => visitor.visit_map(MapVisitor { iter: m.iter().peekable() }),
            _ => Err(Error::InvalidType(de::Type::Map)),
        }
    }
}

/// Deserialize the `Value` into a type backed by Serde.
///
/// This interface is particularly useful for unloading parameters into `struct`s without having to
/// manually implement `FromValue` on the `struct`.
///
/// This function utilizes `FromValue` where appropriate, so e.g. `String`s can be converted to
/// numeric types if necessary and possible.
pub fn deserialize<T: de::Deserialize>(value: &Value) -> Result<T, Error> {
    let mut deserializer = ValueDeserializer(value);
    de::Deserialize::deserialize(&mut deserializer)
}

#[cfg(test)]
mod tests {
    use serde_crate::de;
    use std::collections::BTreeMap;
    use super::*;
    use ::{Map, Value};

    #[test]
    fn primitives() {
        assert_eq!(deserialize::<bool>(&Value::Boolean(true)), Ok(true));
        assert_eq!(deserialize::<u8>(&Value::U64(100)), Ok(100));
        assert_eq!(deserialize::<u16>(&Value::U64(10000)), Ok(10000));
        assert_eq!(deserialize::<u32>(&Value::U64(10000000)), Ok(10000000));
        assert_eq!(deserialize::<u64>(&Value::U64(100000000000)), Ok(100000000000));
        assert_eq!(deserialize::<usize>(&Value::U64(10000000)), Ok(10000000));
        assert_eq!(deserialize::<i8>(&Value::I64(-100)), Ok(-100));
        assert_eq!(deserialize::<i16>(&Value::I64(-10000)), Ok(-10000));
        assert_eq!(deserialize::<i32>(&Value::I64(-10000000)), Ok(-10000000));
        assert_eq!(deserialize::<i64>(&Value::I64(-100000000000)), Ok(-100000000000));
        assert_eq!(deserialize::<isize>(&Value::I64(-10000000)), Ok(-10000000));
        assert_eq!(deserialize::<f32>(&Value::F64(10000.0)), Ok(10000.0));
        assert_eq!(deserialize::<f64>(&Value::F64(10000.0)), Ok(10000.0));
        assert_eq!(deserialize::<String>(&Value::String("hi!".to_owned())), Ok("hi!".to_owned()));
    }

    #[test]
    fn out_of_bounds() {
        assert!(deserialize::<u8>(&Value::U64(1000)).is_err());
        assert!(deserialize::<u8>(&Value::String("1000".to_owned())).is_err());
        assert!(deserialize::<i8>(&Value::I64(-1000)).is_err());
        assert!(deserialize::<i8>(&Value::String("-1000".to_owned())).is_err());
    }

    #[test]
    fn option() {
        assert_eq!(deserialize::<Option<bool>>(&Value::Boolean(true)), Ok(Some(true)));
        assert_eq!(deserialize::<Option<bool>>(&Value::Null), Ok(None));
    }

    #[test]
    fn sequence() {
        let actual = Value::Array(vec![Value::U64(1), Value::U64(2), Value::U64(3)]);
        let expected = vec![1, 2, 3];
        assert_eq!(deserialize::<Vec<u64>>(&actual), Ok(expected));

        let actual = Value::Array(vec![Value::Boolean(true), Value::Null, Value::Boolean(false)]);
        let expected = vec![Some(true), None, Some(false)];
        assert_eq!(deserialize::<Vec<Option<bool>>>(&actual), Ok(expected));

        let actual = Value::Array(vec![Value::U64(1), Value::Null]);
        assert!(deserialize::<Vec<u64>>(&actual).is_err());
    }

    #[test]
    fn map() {
        let actual = Value::Map(Map(btreemap! {
            "a".to_owned() => Value::I64(1),
            "b".to_owned() => Value::F64(2.5),
            "c".to_owned() => Value::U64(3),
        }));
        let expected = btreemap! {
            "a".to_owned() => 1i64,
            "b".to_owned() => 2,
            "c".to_owned() => 3,
        };
        assert_eq!(deserialize::<BTreeMap<String, i64>>(&actual), Ok(expected));

        let actual = Value::Map(Map(btreemap! {
            "a".to_owned() => Value::I64(1),
            "b".to_owned() => Value::Null,
        }));
        assert!(deserialize::<BTreeMap<String, Option<i64>>>(&actual).is_ok());
        assert!(deserialize::<BTreeMap<String, i64>>(&actual).is_err());
    }

    #[test]
    fn map_struct() {
        let actual = Value::Map(Map(btreemap! {
            "x".to_owned() => Value::I64(100),
            "y".to_owned() => Value::F64(200.5),
        }));
        let expected = Point { x: 100, y: 200 };
        assert_eq!(deserialize::<Point>(&actual), Ok(expected));

        let actual = Value::Map(Map(btreemap! {
            "x".to_owned() => Value::I64(100),
            "y".to_owned() => Value::Null,
        }));
        assert!(deserialize::<Point>(&actual).is_err());

        // Trying to avoid relying on codegen or macros for this crate. Sorry about the following
        // manual deserialization implementation!

        #[derive(Debug, PartialEq)]
        struct Point {
            x: i32,
            y: i32,
        }

        enum PointField {
            X,
            Y,
        }

        impl de::Deserialize for PointField {
            fn deserialize<D: de::Deserializer>(deserializer: &mut D)
                -> Result<PointField, D::Error>
            {
                struct PointFieldVisitor;

                impl de::Visitor for PointFieldVisitor {
                    type Value = PointField;

                    fn visit_str<E: de::Error>(&mut self, value: &str) -> Result<PointField, E> {
                        match value {
                            "x" => Ok(PointField::X),
                            "y" => Ok(PointField::Y),
                            _ => Err(de::Error::custom("expected x or y")),
                        }
                    }
                }

                deserializer.deserialize(PointFieldVisitor)
            }
        }

        impl de::Deserialize for Point {
            fn deserialize<D: de::Deserializer>(deserializer: &mut D) -> Result<Point, D::Error> {
                static FIELDS: &'static [&'static str] = &["x", "y"];
                deserializer.deserialize_struct("Point", FIELDS, PointVisitor)
            }
        }

        struct PointVisitor;

        impl de::Visitor for PointVisitor {
            type Value = Point;

            fn visit_map<V: de::MapVisitor>(&mut self, mut visitor: V) -> Result<Point, V::Error> {
                let mut x = None;
                let mut y = None;

                loop {
                    match try!(visitor.visit_key()) {
                        Some(PointField::X) => x = Some(try!(visitor.visit_value())),
                        Some(PointField::Y) => y = Some(try!(visitor.visit_value())),
                        None => break,
                    }
                }

                let x = match x {
                    Some(x) => x,
                    None => try!(visitor.missing_field("x")),
                };

                let y = match y {
                    Some(y) => y,
                    None => try!(visitor.missing_field("y")),
                };

                try!(visitor.end());

                Ok(Point { x: x, y: y })
            }
        }
    }
}
