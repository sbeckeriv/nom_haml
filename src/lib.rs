#![recursion_limit = "300"]
#[macro_use] extern crate pest;
#[macro_use] extern crate error_chain;
#[macro_use] extern crate nom;
use nom::{anychar,  space, alphanumeric, };
use std::str::{self};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::sync::Arc;

struct HAMLParser {
    haml: String,
    context: HashMap<String,Arc<fmt::Display>>,
}

type AttrMap = HashMap<String, String>;

type ContextCode = String;
type Text = String;

#[derive(Debug, Clone, PartialEq)]
struct HamlNode {
    children: Vec<HamlNode>,
    tag: String,
    attributes: Option<AttrMap>,
    id: Option<String>,
    context_lookup: Option<ContextCode>,
    contents: String,
    class: Vec<String>,
}

named!(context_lookup<ContextCode>,
  chain!(
    tag!("= ") ~
    data: map_res!( alphanumeric, std::str::from_utf8),
    || ContextCode::from(data)
  )
);

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

named!(doctype<&str>,
  chain!(
    tag!("!!!") ~
    data: map_res!( alphanumeric, std::str::from_utf8),
    || data
  )
);

named!(tag_class<&str>,
  chain!(
    tag!(".") ~
    class: map_res!( alphanumeric, std::str::from_utf8),
    || class
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
                     let mut h= AttrMap::new();
                        for (k, v) in tuple_vec {
                          h.insert(String::from(k), String::from(v));
                        }
                    h
                }
        )
);

named!(html_tag<(HamlNode)>,
       do_parse!(
               tag: opt!(complete!(tag_named)) >>
               id: opt!(complete!(tag_id)) >>
               class: many0!(tag_class) >>
               attributes_list: opt!(complete!(attributes_list)) >>
               context_lookup: opt!(complete!(context_lookup)) >>
               contents: many0!(anychar) >>
               (HamlNode{children: vec![], tag: String::from(tag.unwrap_or("div")),
                   id: id.map(|text| String::from(text)),
                   contents: contents.into_iter().collect::<String>(),
                   attributes: attributes_list,
                   context_lookup: context_lookup,
                   class: class.into_iter().map(|text| String::from(text)).collect(),
               })
               )
       );

named!(html_line<(Vec<&str>, HamlNode)>,
       do_parse!(
               whitespace: many0!(map_res!(alt!(tag!(" ")|tag!("\t")), str::from_utf8)) >>
               node: html_tag >>
               (whitespace, node)
               )
       );

named!(the_rest<(Text)>,
       do_parse!(
               contents: many0!(anychar)>>
               (contents.into_iter().collect::<Text>())
                )
       );

#[derive(Debug, Clone, PartialEq)]
enum HamlCode{
    HamlNodeBlock(HamlNode),
    CodeBlock(ContextCode),
    TextBlock(Text),
}

named!(haml_line<(Vec<&str>, HamlCode)>,
       do_parse!(
               whitespace: many0!(map_res!(alt!(tag!(" ")|tag!("\t")), str::from_utf8)) >>
               line: alt!(
                    // context before tag. tag can include context
                    context_lookup => { |h| HamlCode::CodeBlock(h)      }   |
                    html_tag => { |h|       HamlCode::HamlNodeBlock(h)  }   |
                    the_rest => { |h|       HamlCode::TextBlock(h)      }
                )>> 
               (whitespace, line)
               )
       );

#[cfg(test)]
mod tests {
    //#![feature(trace_macros)]
    use nom::IResult;
    use super::*;

    #[test]
    fn it_parses_haml_line_text_only() {
        let empty = &b""[..];
        let parsed_node = haml_line("prints".as_bytes());
        match parsed_node {
            IResult::Done(x, tup)=>{
                assert_eq!(tup.1,  HamlCode::TextBlock("prints".to_string()));
            }
            _ => { assert_eq!(false, true); }
        }
    }


    #[test]
    fn it_parses_haml_line_context_block() {
        let empty = &b""[..];
        let parsed_node = haml_line("= prints".as_bytes());
        match parsed_node {
            IResult::Done(x, tup)=>{
                assert_eq!(tup.1,  HamlCode::CodeBlock("prints".to_string()));
            }
            _ => { assert_eq!(false, true); }
        }
    }


    #[test]
    fn it_parses_haml_line_haml_tag() {
        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            id: None,
            context_lookup: None,
            contents: "".to_string(),
            attributes: None,
        };

        let empty = &b""[..];
        let parsed_node = haml_line("%p".as_bytes());
        match parsed_node {
            IResult::Done(x, tup)=>{
                assert_eq!(tup.1,  HamlCode::HamlNodeBlock(node.clone()));
            }
            _ => { assert_eq!(false, true); }
        }
    }

    #[test]
    fn it_parses_tag_id() {
        let empty = &b""[..];
        assert_eq!(tag_id("#id".as_bytes()), IResult::Done(empty, ("id")));
    }

    #[test]
    fn it_parses_tag_nammed() {
        let empty = &b""[..];
        assert_eq!(tag_named("%p".as_bytes()), IResult::Done(empty, ("p")));
    }

    #[test]
    fn it_parses_html_line() {
        let empty = &b""[..];
        let mut attrs = AttrMap::new();

        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            id: None,
            context_lookup: None,
            contents: "".to_string(),
            attributes: None,
        };
        assert_eq!(html_line("%p".as_bytes()), IResult::Done(empty, (vec![], node.clone())));
        assert_eq!(html_line("\t\t%p".as_bytes()), IResult::Done(empty, (vec!["\t","\t"], node.clone())));
        assert_eq!(html_line("  %p".as_bytes()), IResult::Done(empty, (vec![" "," "], node)));
    }

    #[test]
    fn it_parses_html_tag() {
        let empty = &b""[..];
        let mut attrs = AttrMap::new();

        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            id: None,
            context_lookup: None,
            contents: "".to_string(),
            attributes: None,
        };
        assert_eq!(html_tag("%p".as_bytes()), IResult::Done(empty, (node)));

        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            id: None,
            context_lookup: None,
            contents: " Yes sir".to_string(),
            attributes: None,
        };
        assert_eq!(html_tag("%p Yes sir".as_bytes()), IResult::Done(empty, (node)));

        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            contents: "".to_string(),
            context_lookup: None,
            id: Some("banana".to_string()),
            attributes: None,
        };
        assert_eq!(html_tag("%p#banana".as_bytes()), IResult::Done(empty, (node)));

        let node = HamlNode {
            class: vec!["pan".to_string(),"cakes".to_string()],
            children: vec![],
            tag: "p".to_string(),
            contents: "".to_string(),
            context_lookup: None,
            id: Some("banana".to_string()),
            attributes: None,
        };
        assert_eq!(html_tag("%p#banana.pan.cakes".as_bytes()), IResult::Done(empty, (node)));

        attrs.insert("d".to_string(), "3".to_string());
        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            contents: "".to_string(),
            context_lookup: None,
            id: Some("banana".to_string()),
            attributes: Some(attrs.clone()),
        };
        assert_eq!(html_tag("%p#banana(:d=>3)".as_bytes()), IResult::Done(empty, (node)));

        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            contents: "".to_string(),
            context_lookup: None,
            id: None,
            attributes: Some(attrs.clone()),
        };
        assert_eq!(html_tag("%p(:d=>3)".as_bytes()), IResult::Done(empty, (node)));

        let node = HamlNode {
            class: vec![],
            children: vec![],
            tag: "p".to_string(),
            contents: "".to_string(),
            context_lookup: Some("banana".to_string()),
            id: None,
            attributes: Some(attrs.clone()),
        };
        assert_eq!(html_tag("%p(:d=>3)= banana".as_bytes()), IResult::Done(empty, (node)));
    }

    #[test]
    fn it_parses_attributes_not_allowed() {
        let empty = &b""[..];
        let mut attrs = AttrMap::new();
        attrs.insert("a".to_string(), "3".to_string());
        // should not allow this kind of set.
        assert_eq!(attributes_list("(:a => 3}".as_bytes()), IResult::Done(empty, attrs));
    }

    #[test]
    fn it_parses_attributes() {
        let empty = &b""[..];
        let mut attrs = AttrMap::new();
        assert_eq!(attributes_list("{}".as_bytes()), IResult::Done(empty,attrs.clone()));

        attrs.insert("a".to_string(), "3".to_string());
        assert_eq!(attributes_list("(:a=>3)".as_bytes()), IResult::Done(empty, attrs.clone()));

        assert_eq!(attributes_list("{:a=>\"3\"}".as_bytes()), IResult::Done(empty, attrs.clone()));

        assert_eq!(attributes_list("{:a=> '3'}".as_bytes()), IResult::Done(empty, attrs.clone()));

        attrs.insert("b".to_string(), "4".to_string());
        assert_eq!(attributes_list("{:a=>3, :b=>'4'}".as_bytes()), IResult::Done(empty, attrs));
    }
}
