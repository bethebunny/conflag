use std::collections::HashMap;
use std::rc::Rc;

use serde::de::{self, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, VariantAccess, Visitor};
use serde::Deserialize;

use crate::scope::ScopePtr;
use crate::thunk::Thunk;
use crate::{Error, Result, Value};

pub struct Deserializer {
    // This string starts with the input data and characters are truncated off
    // the beginning as data is parsed.
    value: Result<Rc<Value>>,
}

impl Deserializer {
    // By convention, `Deserializer` constructors are named like `from_xyz`.
    // That way basic use cases are satisfied by something like
    // `serde_json::from_str(...)` while advanced use cases that require a
    // deserializer can make one with `serde_json::Deserializer::from_str(...)`.
    pub fn from_str(input: &str) -> Self {
        Deserializer {
            value: crate::parse(input),
        }
    }
}

// By convention, the public API of a Serde deserializer is one or more
// `from_xyz` methods such as `from_str`, `from_bytes`, or `from_reader`
// depending on what Rust types the deserializer is able to consume as input.
//
// This basic deserializer supports only `from_str`.
pub fn from_str<'a, T>(s: &'a str) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut deserializer = Deserializer::from_str(s);
    T::deserialize(&mut deserializer)
}

fn as_int(f: f64) -> Result<i128> {
    if f.fract() != 0. {
        Err(Error::Custom(format!(
            "Can't convert non-int to integer type: {f}"
        )))
    } else if f > u64::MAX as f64 || f < i64::MIN as f64 {
        Err(Error::Custom(format!(
            "Can't convert large float to integer type: {f}"
        )))
    } else {
        Ok(f.trunc() as i128)
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match &**self.value.as_ref()? {
            Value::Object(_) => self.deserialize_map(visitor),
            Value::Array(_) => self.deserialize_seq(visitor),
            Value::Number(_) => self.deserialize_f64(visitor),
            Value::String(_) => self.deserialize_string(visitor),
            Value::Boolean(_) => self.deserialize_bool(visitor),
            Value::Null => self.deserialize_option(visitor),
            v => Err(Error::Custom(format!(
                "Value {v} doesn't correspond to native Rust type"
            ))),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(self.value.as_ref()?.bool()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor
            .visit_i8(i8::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> i8"))))?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor.visit_i16(
            i16::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> i16"))))?,
        )
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor.visit_i32(
            i32::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> i32"))))?,
        )
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor.visit_i64(
            i64::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> i64"))))?,
        )
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor
            .visit_u8(u8::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> u8"))))?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor.visit_u16(
            u16::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> u16"))))?,
        )
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor.visit_u32(
            u32::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> u32"))))?,
        )
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = as_int(self.value.as_ref()?.number()?)?;
        visitor.visit_u64(
            u64::try_from(n).or_else(|e| Err(Error::Custom(format!("{e}: {n} -> u64"))))?,
        )
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let n = self.value.as_ref()?.number()?;
        visitor.visit_f32(n as f32)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f64(self.value.as_ref()?.number()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // TODO: allow deserializing a u8 as a char, or a single character string
        self.deserialize_u8(visitor).map(|v| v.into())
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_str(self.value.as_ref()?.str()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    // TODO: this doesn't work, something something borrows/references/owned values
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_byte_buf(visitor)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match &**self.value.as_ref()? {
            Value::Array(values) => {
                let values: Result<Vec<u8>> = values
                    .iter()
                    .map(|v| {
                        let v = v.evaluate()?;
                        let n = as_int(v.number()?)?;
                        u8::try_from(n).map_err(|e| Error::Custom(format!("{e}: {v} -> u8")))
                    })
                    .collect();
                visitor.visit_byte_buf(values?)
            }
            v => Err(Error::Custom(format!("Can't convert from '{v}' to bytes"))),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match &**self.value.as_ref()? {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    // From Serde docs: https://serde.rs/impl-deserializer.html
    // In Serde, unit means an anonymous value containing no data.
    // Personally I think this is a bit restrictive; since unit isn't really a meaningful
    // concept in JSON, I think this should just accept any value. I might change this to just deserialize_any().
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match &**self.value.as_ref()? {
            Value::Null => visitor.visit_unit(),
            v => Err(Error::Custom(format!(
                "Unit type received non-null value: {v}"
            ))),
        }
    }

    // From Serde docs: https://serde.rs/impl-deserializer.html
    // Unit struct means a named value containing no data.
    // Personally I think this is a bit restrictive; since unit isn't really a meaningful
    // concept in JSON, I think this should just accept any value. I might change this to just deserialize_any().
    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    // From Serde docs: https://serde.rs/impl-deserializer.html
    // serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain. That means not
    // parsing anything other than the contained value.
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match &**self.value.as_ref()? {
            Value::Array(values) => visitor.visit_seq(ArrayIterator::from(values)),
            v => Err(Error::Custom(format!("Invalid sequence type: {v}"))),
        }
    }

    // There's no tuple type in conflag, so just ignore sequence length and parse an array.
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // There's no tuple type in conflag, so just ignore sequence length and parse an array.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if let Value::Object(scope) = &**self.value.as_ref()? {
            visitor.visit_map(ScopeIterator::from(scope))
        } else {
            Err(Error::BadFunctionCall)
        }
    }

    // There's no special struct types, just deserialize like a map.
    // TODO: wait no we only need to evaluate things that are in the struct!!
    // which is (potentially) a lot more efficient.
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if let Value::Object(scope) = &**self.value.as_ref()? {
            visitor.visit_map(StructIterator::new(scope, _fields.iter()))
        } else {
            Err(Error::Custom(format!(
                "Can't deserialize non-object to struct: {}",
                self.value.as_ref()?
            )))
        }
    }

    // Following the same enum conventions as Serde's JSON implementation
    // https://serde.rs/json.html
    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match &**self.value.as_ref()? {
            Value::Object(_) | Value::String(_) => visitor.visit_enum(Enum(self)),
            v => Err(Error::Custom(format!(
                "Invalid enum variant value type: {v}"
            ))),
        }
    }

    // From Serde docs: https://serde.rs/impl-deserializer.html
    // An identifier in Serde is the type that identifies a field of a struct or
    // the variant of an enum. In conflag struct fields and enum variants are
    // represented as strings.
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match &**self.value.as_ref()? {
            Value::Object(scope) => {
                let values = scope.values();
                if values.len() != 1 {
                    Err(Error::Custom(format!(
                        "Couldn't choose unique identifier from object"
                    )))
                } else {
                    visitor.visit_string(values.keys().next().unwrap().clone())
                }
            }
            _ => self.deserialize_str(visitor),
        }
    }

    // From Serde docs: https://serde.rs/impl-deserializer.html
    // Like `deserialize_any` but indicates to the `Deserializer` that it makes
    // no difference which `Visitor` method is called because the data is
    // ignored.
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }
}

struct ArrayIterator<'a> {
    iter: core::slice::Iter<'a, Thunk>,
}

impl<'a, 'de> SeqAccess<'de> for ArrayIterator<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(thunk) = self.iter.next() {
            seed.deserialize(&mut Deserializer {
                value: thunk.evaluate(),
            })
            .map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<'de> From<&'de Vec<Thunk>> for ArrayIterator<'de> {
    fn from(value: &'de Vec<Thunk>) -> Self {
        ArrayIterator { iter: value.iter() }
    }
}

struct ScopeIterator<'a> {
    iter: std::collections::hash_map::Iter<'a, String, Thunk>,
    next_value: Option<Thunk>,
}

impl<'a, 'de: 'a> MapAccess<'de> for ScopeIterator<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        if let Some((k, v)) = self.iter.next() {
            self.next_value = Some(v.clone());
            seed.deserialize(&mut Deserializer {
                value: Ok(Rc::new(Value::String(k.clone()))),
            })
            .map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        let value = self.next_value.take().unwrap().evaluate();
        seed.deserialize(&mut Deserializer { value })
    }
}

impl<'de> From<&'de ScopePtr> for ScopeIterator<'de> {
    fn from(value: &'de ScopePtr) -> Self {
        ScopeIterator {
            iter: value.values().iter(),
            next_value: None,
        }
    }
}

struct StructIterator<'a> {
    iter_keys: core::slice::Iter<'a, &'a str>,
    scope: &'a HashMap<String, Thunk>,
    next_value: Option<&'a Thunk>,
}

impl<'a, 'de: 'a> MapAccess<'de> for StructIterator<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        if let Some(key) = self.iter_keys.next() {
            let key = String::from(*key);
            self.next_value = Some(&self.scope[&key]);
            // This is super awkward and inefficient, there has to be a better way here :P
            seed.deserialize(&mut Deserializer {
                value: Ok(Rc::new(Value::String(key))),
            })
            .map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        let value = self.next_value.take().unwrap().evaluate();
        seed.deserialize(&mut Deserializer { value })
    }
}

impl<'a> StructIterator<'a> {
    fn new(scope: &'a ScopePtr, keys: core::slice::Iter<'a, &str>) -> Self {
        StructIterator {
            iter_keys: keys,
            scope: scope.values(),
            next_value: None,
        }
    }
}

struct Enum<'a>(&'a mut Deserializer);

impl<'a, 'de> EnumAccess<'de> for Enum<'a> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> std::result::Result<(V::Value, Self::Variant), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.0).map(|v| (v, self))
    }
}

impl<'a> Enum<'a> {
    fn unpack_single_scope_value(&self) -> Result<Rc<Value>> {
        match &**self.0.value.as_ref()? {
            Value::Object(scope) => {
                let values = scope.values();
                if values.len() != 1 {
                    Err(Error::Custom(format!(
                        "Couldn't choose unique identifier from object"
                    )))
                } else {
                    values.values().next().unwrap().evaluate()
                }
            }
            _ => Err(Error::Custom(format!(
                "Invalid enum variant object {}",
                self.0.value.as_ref()?
            ))),
        }
    }
}

impl<'a, 'de> VariantAccess<'de> for Enum<'a> {
    type Error = Error;

    fn unit_variant(self) -> std::result::Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> std::result::Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        let mut deserializer = Deserializer {
            value: self.unpack_single_scope_value(),
        };
        seed.deserialize(&mut deserializer)
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> std::result::Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let mut deserializer = Deserializer {
            value: self.unpack_single_scope_value(),
        };
        serde::Deserializer::deserialize_tuple(&mut deserializer, len, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> std::result::Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let mut deserializer = Deserializer {
            value: self.unpack_single_scope_value(),
        };
        // To have the real name we'd have to put it into the class data, which
        // we currently don't really have, and we don't use the name for anything,
        // so just pass a mock static name.
        serde::Deserializer::deserialize_struct(&mut deserializer, "_name", fields, visitor)
    }
}

#[cfg(test)]
mod test {
    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    fn test_bool() {
        let s: bool = from_str("true").unwrap();
        assert_eq!(true, s);
    }

    #[test]
    fn test_i8_invalid() {
        let v: Result<i8> = from_str("true");
        assert_matches!(v, Err(Error::Custom(_)));
    }

    #[test]
    fn test_i8_non_int() {
        let v: Result<i8> = from_str("4.1");
        assert_matches!(v, Err(Error::Custom(_)));
    }

    #[test]
    fn test_i8_too_big() {
        let v: Result<i8> = from_str("200");
        assert_matches!(v, Err(Error::Custom(_)));
    }

    #[test]
    fn test_i8_too_small() {
        let v: Result<i8> = from_str("-200");
        assert_matches!(v, Err(Error::Custom(_)));
    }

    #[test]
    fn test_i8() {
        let v: i8 = from_str("100").unwrap();
        assert_eq!(100, v);

        let v: i8 = from_str("-100").unwrap();
        assert_eq!(-100, v);
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct TestStruct {
        //<'a> {
        b: bool,
        s: String,
        // sr: &'a str,
        // o: Option<&'a [u8]>,
        o: Option<String>,
    }

    #[test]
    fn test_struct() {
        let v: TestStruct = from_str(
            r#"{
            b: true,
            s: "okay",
            // sr: "this is a ref",
            // o: [1, 2, 3, 4, 5],
            o: "yup it's a string",
        }"#,
        )
        .unwrap();
        assert_eq!(v.b, true);
        assert_eq!(v.s, "okay");
        // assert_eq!(v.sr, "this is a ref");
        // assert_eq!(v.o.unwrap(), [1, 2, 3, 4, 5]);
        assert_eq!(v.o.unwrap(), "yup it's a string");
    }

    #[test]
    fn test_struct_deserializes_only_necessary_values() {
        #[derive(Deserialize, Debug)]
        struct TestStruct {
            a: bool,
            b: String,
        }

        let v: TestStruct = from_str(
            r#"{
            a: true,
            b: "okay",
            c: "is a thing",
            error: f(x) + 1,
            error2: 1 + "lol",
        }"#,
        )
        .unwrap();

        assert_eq!(v.a, true);
        assert_eq!(v.b, "okay");
    }

    #[test]
    fn test_string_array() {
        let v: Vec<String> = from_str(
            r#"[
                "cheese",
                "crackers",
                "ugh",
                ""
        ]"#,
        )
        .unwrap();
        assert_eq!(4, v.len());
        assert_eq!("cheese", v[0]);
        assert_eq!("crackers", v[1]);
        assert_eq!("ugh", v[2]);
        assert_eq!("", v[3]);
    }

    #[test]
    fn test_tuple_type() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct TestTupleType(u32, String, Vec<bool>);

        let v: TestTupleType = from_str(
            r#"[
                12,
                "test",
                [true, false, true, true]
            ]"#,
        )
        .unwrap();
        assert_eq!(12, v.0);
        assert_eq!("test", v.1);
        assert_eq!([true, false, true, true], &v.2[..]);
    }

    #[test]
    fn test_enum_externally_tagged() {
        // Taken from https://serde.rs/json.html
        #[derive(Deserialize, Debug)]
        enum E {
            W { a: i32, b: i32 },
            X(i32, i32),
            Y(i32),
            Z,
        }
        let w: E = from_str(r#"{W:{a:0,b:0}}"#).unwrap();
        let x: E = from_str(r#"{X:[0,0]}"#).unwrap();
        let y: E = from_str(r#"{Y:0}"#).unwrap();
        let z: E = from_str(r#""Z""#).unwrap();

        assert_matches!(w, E::W { a: 0, b: 0 });
        assert_matches!(x, E::X(0, 0));
        assert_matches!(y, E::Y(0));
        assert_matches!(z, E::Z);
    }

    #[test]
    fn test_enum_untagged() {
        #[allow(unused)]
        #[derive(Deserialize, Debug)]
        #[serde(untagged)]
        enum Message {
            Request {
                id: String,
                method: String,
                params: String,
            },
            Response {
                id: String,
                result: String,
            },
        }

        let m1: Message = from_str(r#"{id: "...", method: "...", params: "..."}"#).unwrap();
        let m2: Message = from_str(r#"{id: "...", result: "..."}"#).unwrap();

        assert_matches!(m1, Message::Request { .. });
        assert_matches!(m2, Message::Response { .. });
    }

    #[test]
    fn test_enum_adjacently_tagged() {
        #[allow(unused)]
        #[derive(Deserialize, Debug)]
        #[serde(tag = "t", content = "c")]
        enum Block {
            Para(Vec<u8>),
            Str(String),
        }

        let b1: Block = from_str(r#"{t: "Para", c: [1, 2, 3]}"#).unwrap();
        let b2: Block = from_str(r#"{t: "Str", c: "the string"}"#).unwrap();

        assert_matches!(b1, Block::Para { .. });
        assert_matches!(b2, Block::Str { .. });
    }

    #[test]
    fn test_enum_adjacently_tagged_dict_order_bug() {
        // Okay, this is a bit hard to explain (I don't understand it amazingly well yet)
        // basically Serde will use different code paths to deserialize the Vec<u8> here
        // depending on whether "t" or "c" comes first in the HashMap ordering.

        // This test attempts to force a failure for this bug by instantiating many of these dicts,
        // triggering the bug with high likelihood.
        // test_adjacently_tagged only triggers the bug ~50% of the time, so 20 loops should
        // give a false-positive pass rate <1e-6.

        // I don't exactly understand what determines the dict order; it's non-deterministic,
        // and appears to change per dict instance. This test correctly fails with high probability,
        // and not always on the first iteration. It also still fails when test_adjacently_tagged
        // passes.

        // Hypothesis on why this bug happens:
        // - If "t" is parsed first, then we know the variant type, so Serde
        //   parses "c" according to its expected type;
        // - If "c" is parsed first, we are still going to deserialize it,
        //   but we do so via `deserialize_any()`, which will parse [1, 2, 3] as
        //   Vec<float>.
        // There's a few things in the solution space here
        // 1. We already want to be parsing structs specially given only their
        //    expected fields, so as to not do unecessary computation for fields
        //    that aren't part of the struct. As long as Serde provides "t" before "c"
        //    this should fix this particular bug.
        // 2. Unordered maps are awkward anyway. Right now pretty printing sorts keys,
        //    but I don't really like how that prints them in a different order than they appear
        //    in the source. We should use an ordered map type for scopes, which would also solve this bug.

        // Implementing 1. was the easiest, and fixed the bug!
        // We wanted to do so anyway, but will look at 2. another time
        //   (requires depending on say, indexmap crate)
        #[allow(unused)]
        #[derive(Deserialize, Debug)]
        #[serde(tag = "t", content = "c")]
        enum Block {
            Para(Vec<u8>),
            Str(String),
        }

        for _ in 0..20 {
            let b1: Block = from_str(r#"{t: "Para", c: [1, 2, 3]}"#).unwrap();
            assert_matches!(b1, Block::Para { .. });
        }
    }

    #[test]
    fn test_enum_internally_tagged() {
        #[allow(unused)]
        #[derive(Deserialize, Debug)]
        #[serde(tag = "type")]
        enum Message {
            Request {
                id: String,
                method: String,
                params: String,
            },
            Response {
                id: String,
                result: String,
            },
        }

        let m1: Message =
            from_str(r#"{type: "Request", id: "...", method: "...", params: "..."}"#).unwrap();
        let m2: Message = from_str(r#"{type: "Response", id: "...", result: "..."}"#).unwrap();

        assert_matches!(m1, Message::Request { .. });
        assert_matches!(m2, Message::Response { .. });
    }
}
