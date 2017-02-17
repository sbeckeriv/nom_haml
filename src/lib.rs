#[macro_use]
extern crate nom;
use nom::{IResult,digit, space, alpha, alphanumeric};
use std::str::{self,FromStr};
use std::collections::HashMap;
#[derive(Debug)]
struct HAMLParser{
    haml: String,
}

type AttrMap = HashMap<String, String>;

#[derive(Debug, PartialEq)]
struct Node{
    children: Vec<Node>,
    tag: String,
    attributes: Option<AttrMap>,
    id: Option<String>,
}


named!(single_quote_string<&str>,
  delimited!(
    tag!("'"),
    map_res!(escaped!(call!(alphanumeric), '\\', is_a!("\'n\\")), std::str::from_utf8),
    tag!("'")
  )
);

named!(double_quote_string<&str>,
  delimited!(
    tag!("\""),
    map_res!(escaped!(call!(alphanumeric), '\\', is_a!("\"n\\")), std::str::from_utf8),
    tag!("\"")
  )
);

named!(tag_id<&str>,
  chain!(
    tag!("#") ~
    id: map_res!( alphanumeric, std::str::from_utf8),
    || id
  )
);

named!(tag_named<&str>,
  chain!(
    tag!("%") ~
    tag: map_res!( alphanumeric, std::str::from_utf8),
    || tag
  )
);

named!(attribute_hash_key_value<(&str, &str)>,
       chain!(
           tag!(":") ~
           space? ~
           key: map_res!( alphanumeric, std::str::from_utf8) ~
           space? ~
           tag!("=>") ~
           space? ~
           value: alt!(single_quote_string | double_quote_string | map_res!( alphanumeric, std::str::from_utf8)) ,
           || (key,value)
        )
      );

named!(attributes_list<AttrMap>,
       dbg_dmp!(
            map!(
           ws!(
                delimited!(
                    alt!(tag!("{") | tag!("(")),
                    separated_list!(
                        tag!(","),
                        attribute_hash_key_value
                    ),
                    alt!(tag!("}") | tag!(")"))
            )
                ),
                |tuple_vec|{
                    println!("{:?}",tuple_vec);
                     let mut h= AttrMap::new();
                        for (k, v) in tuple_vec {
                          h.insert(String::from(k), String::from(v));
                        }
                    h
                }
        )
    )
);

named!(html_tag<(Node)>,
       do_parse!(
               tag: opt!(complete!(tag_named)) >>
               id: opt!(complete!(tag_id)) >>
               attributes_list: opt!(complete!(attributes_list)) >>
               (Node{children: vec![], tag: String::from(tag.unwrap_or("div")), id: id.map(|text| String::from(text)), attributes: attributes_list }))
       );

#[cfg(test)]
mod tests {
    use nom::{IResult};
    use super::*;

    #[test]
    fn it_parses_tag_id() {
        let empty = &b""[..];
        //should not allow this kind of set.
        assert_eq!(tag_id("#id".as_bytes()), IResult::Done(empty, ("id")));
    }

    #[test]
    fn it_parses_tag_nammed() {
        let empty = &b""[..];
        //should not allow this kind of set.
        assert_eq!(tag_named("%p".as_bytes()), IResult::Done(empty, ("p")));
    }

    #[test]
    fn it_parses_html_tag() {
        let empty = &b""[..];
        //should not allow this kind of set.
        let mut attrs= AttrMap::new();
        let node =  Node{children: vec![], tag: "p".to_string(), id: None, attributes: None};
        assert_eq!(html_tag("%p".as_bytes()), IResult::Done(empty, (node)));
        let node =  Node{children: vec![], tag: "p".to_string(), id: Some("banana".to_string()), attributes: None};
        assert_eq!(html_tag("%p#banana".as_bytes()), IResult::Done(empty, (node)));
        attrs.insert("d".to_string(),"3".to_string());
        let node =  Node{children: vec![], tag: "p".to_string(), id: Some("banana".to_string()), attributes: Some(attrs.clone())};
        assert_eq!(html_tag("%p#banana(:d=>3)".as_bytes()), IResult::Done(empty, (node)));
        let node =  Node{children: vec![], tag: "p".to_string(), id: None, attributes: Some(attrs.clone())};
        assert_eq!(html_tag("%p(:d=>3)".as_bytes()), IResult::Done(empty, (node)));
    }

    #[test]
    fn it_parses_attributes_not_allowed() {
        let empty = &b""[..];
        let mut attrs= AttrMap::new();
        attrs.insert("a".to_string(),"3".to_string());
        //should not allow this kind of set.
        assert_eq!(attributes_list("(:a => 3}".as_bytes()), IResult::Done(empty, attrs));
    }

    #[test]
    fn it_parses_attributes() {
        let empty = &b""[..];
        let mut attrs=AttrMap::new();
        assert_eq!(attributes_list("{}".as_bytes()), IResult::Done(empty,attrs.clone()));
        attrs.insert("a".to_string(),"3".to_string());
        assert_eq!(attributes_list("(:a=>3)".as_bytes()), IResult::Done(empty, attrs.clone()));
        assert_eq!(attributes_list("{:a=>\"3\"}".as_bytes()), IResult::Done(empty, attrs.clone()));
        assert_eq!(attributes_list("{:a=> '3'}".as_bytes()), IResult::Done(empty, attrs.clone()));
        attrs.insert("b".to_string(),"4".to_string());
        assert_eq!(attributes_list("{:a=>3, :b=>'4'}".as_bytes()), IResult::Done(empty, attrs));
    }
}
