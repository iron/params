use std::collections::BTreeMap;
use std::str::FromStr;

use num::NumCast;

use super::{File, Value};

/// An interface for converting from `Value` variants.
pub trait FromValue: Sized {
    /// Returns `Some` if the conversion was successful, `None` otherwise.
    fn from_value(value: &Value) -> Option<Self>;
}

impl FromValue for Value {
    fn from_value(value: &Value) -> Option<Self> {
        Some(value.clone())
    }
}

macro_rules! num_from_value {
    ($ty:ty) => {
        /// Casts from any of the numeric variants (`I64`, `U64`, `F64`) and converts from the
        /// `String` variant using `std::str::FromStr`.
        impl FromValue for $ty {
            fn from_value(value: &Value) -> Option<$ty> {
                match *value {
                    Value::I64(value) => NumCast::from(value),
                    Value::U64(value) => NumCast::from(value),
                    Value::F64(value) => NumCast::from(value),
                    Value::String(ref value) => FromStr::from_str(value).ok(),
                    _ => None,
                }
            }
        }
    }
}

num_from_value!(u8);
num_from_value!(u16);
num_from_value!(u32);
num_from_value!(u64);
num_from_value!(usize);
num_from_value!(i8);
num_from_value!(i16);
num_from_value!(i32);
num_from_value!(i64);
num_from_value!(isize);
num_from_value!(f32);
num_from_value!(f64);

/// Converts from any of the flat variants (`Null`, `Boolean`, `I64`, `U64`, `F64`, `String`).
///
/// `Null` is converted to an empty string. All other variants make use of `std::string::ToString`,
/// so the respective `Display` formatted results are used. For example, `Boolean` values are
/// converted into either `"false"` or `"true"`.
impl FromValue for String {
    fn from_value(value: &Value) -> Option<String> {
        match *value {
            Value::Null => Some(String::new()),
            Value::Boolean(value) => Some(value.to_string()),
            Value::I64(value) => Some(value.to_string()),
            Value::U64(value) => Some(value.to_string()),
            Value::F64(value) => Some(value.to_string()),
            Value::String(ref value) => Some(value.clone()),
            _ => None,
        }
    }
}

static FALSE_STRINGS: &'static [&'static str] = &["0", "f", "F", "false", "FALSE", "off", "OFF"];
static TRUE_STRINGS: &'static [&'static str] = &["1", "t", "T", "true", "TRUE", "on", "ON"];

/// Converts from common representations of `false` and `true`.
///
/// * `Boolean` values are mapped directly.
/// * `I64` and `U64` map `0` and `1` to `false` and `true` respectively.
/// * `String` maps `"0"`, `"f"`, `"F"`, `"false"`, `"FALSE"`, `"off"`, and `"OFF"` to `false`.
/// * `String` maps `"1"`, `"t"`, `"T"`, `"true"`, `"TRUE"`, `"on"`, and `"ON"` to `true`.
/// * All other variants return `None`.
impl FromValue for bool {
    fn from_value(value: &Value) -> Option<bool> {
        match *value {
            Value::Boolean(value) => Some(value),
            Value::I64(value) if value == 0 => Some(false),
            Value::I64(value) if value == 1 => Some(true),
            Value::U64(value) if value == 0 => Some(false),
            Value::U64(value) if value == 1 => Some(true),
            Value::String(ref value) if FALSE_STRINGS.contains(&&value[..]) => Some(false),
            Value::String(ref value) if TRUE_STRINGS.contains(&&value[..]) => Some(true),
            _ => None,
        }
    }
}

/// Extracts from `File` variant.
impl FromValue for File {
    fn from_value(value: &Value) -> Option<File> {
        match *value {
            Value::File(ref file) => Some(file.clone()),
            _ => None,
        }
    }
}

/// Converts the `Null` variant to `None` and delegates any other variants to `T::from_value`.
impl<T: FromValue> FromValue for Option<T> {
    fn from_value(value: &Value) -> Option<Option<T>> {
        match *value {
            Value::Null => Some(None),
            _ => T::from_value(value).map(Some),
        }
    }
}

/// Converts from `Array` variant.
impl<T: FromValue> FromValue for Vec<T> {
    fn from_value(value: &Value) -> Option<Vec<T>> {
        match *value {
            Value::Array(ref array) => {
                let mut vec = Vec::with_capacity(array.len());
                for value in array {
                    match T::from_value(value) {
                        Some(value) => vec.push(value),
                        None => return None,
                    }
                }
                Some(vec)
            },
            _ => None,
        }
    }
}

/// Converts using `Map::to_strict_map` if the variant is `Map`, returns `None` otherwise.
impl<T: FromValue> FromValue for BTreeMap<String, T> {
    fn from_value(value: &Value) -> Option<BTreeMap<String, T>> {
        match *value {
            Value::Map(ref map) => map.to_strict_map(),
            _ => None,
        }
    }
}

#[test]
fn cast_within_bounds() {
    assert_eq!(<u8 as FromValue>::from_value(&Value::U64(100)), Some(100));
    assert_eq!(<u8 as FromValue>::from_value(&Value::String("100".to_owned())), Some(100));
    assert_eq!(<i8 as FromValue>::from_value(&Value::I64(-100)), Some(-100));
    assert_eq!(<i8 as FromValue>::from_value(&Value::String("-100".to_owned())), Some(-100));
}

#[test]
fn cast_out_of_bounds() {
    assert!(<u8 as FromValue>::from_value(&Value::U64(1000)).is_none());
    assert!(<u8 as FromValue>::from_value(&Value::String("1000".to_owned())).is_none());
    assert!(<i8 as FromValue>::from_value(&Value::I64(-1000)).is_none());
    assert!(<i8 as FromValue>::from_value(&Value::String("-1000".to_owned())).is_none());
}
