#![recursion_limit = "300"]
#[macro_use]
extern crate pest;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate nom;
extern crate id_tree;
use id_tree::InsertBehavior::*;
use id_tree::*;
use nom::{anychar, space, alphanumeric};
use nom::IResult;
use std::str;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::sync::Arc;

type AttrMap = HashMap<String, String>;

type ContextCode = String;
type Text = String;

#[derive(Debug, Clone, PartialEq)]
struct HamlNode {
    pub children: Vec<HamlCode>,
    pub tag: String,
    pub attributes: Option<AttrMap>,
    pub id: Option<String>,
    pub context_lookup: Option<ContextCode>,
    pub contents: String,
    pub class: Vec<String>,
}


#[derive(Debug, Clone, PartialEq)]
enum HamlCode {
    HamlNodeBlock(HamlNode),
    CodeBlock(ContextCode),
    TextBlock(Text),
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

struct HAMLParser {
    haml: String,
    nodes: Option<Tree<HamlCode>>,
}

impl HAMLParser {
    fn display(self, context: HashMap<String, Arc<fmt::Display>>) -> String {
        "".to_string()
    }
    fn parse(&mut self) -> Result<(), ()> {
        let mut tree: Tree<HamlCode> = Tree::new();
        let mut previous_depth = 0;
        let mut current_node: Option<NodeId> = None;
        let mut whitespacer: Option<String> = None;
        for line in self.haml.lines() {
            let tag = haml_line(line.as_bytes());
            match tag {
                IResult::Done(_, (whitespace, haml_code)) => {
                    let current_depth = if whitespacer.is_some() {
                        
                        // TODO: make sure they are the same type of spacing
                        // TODO: make sure its a valid count not 3 spaces
                        whitespace.len() / whitespacer.as_ref().unwrap().len() 
                    } else if whitespace.len() != 0 && whitespacer.is_none() {
                        whitespacer = Some(whitespace.into_iter().collect::<String>());
                        1
                    } else {
                        0
                    };

                    // depth is greater change the current stack to the last child of the
                    // previous_depth
                    // if depth is less pop depth times
                    // if its the same its another child?
                    println!("{} {}", previous_depth, current_depth);
                    if current_node.is_some() {
                        if current_depth == previous_depth {
                            let child_node = Node::new(haml_code);
                            tree.insert(child_node, UnderNode(current_node.as_ref().unwrap()))
                                .unwrap();
                        } else if current_depth == previous_depth + 1 {
                            // get last child. this is now the current node
                            let child_node = Node::new(haml_code);
                            tree.insert(child_node, UnderNode(current_node.as_ref().unwrap()))
                                .unwrap();

                            let last_child = {
                                let node = tree.get(current_node.as_ref().unwrap()).unwrap();
                                // TODO remove clone?
                                node.children().last().unwrap().clone()
                            };
                            current_node = Some(last_child);
                        } else if current_depth > previous_depth {
                            panic!("Jumped depth to far from {} to {}", previous_depth, current_depth);
                            // current_node.parent n times is now current node
                        } else {
                            //TODO remove clones
                            let mut parent = current_node.as_ref().unwrap().clone();
                            for _ in 0..(previous_depth - current_depth)+1 {
                               match tree.get(&parent).unwrap().parent(){
                                   Some(parent_id) => { parent = parent_id.clone();},
                                   None => panic!("unwinding to far current depth {} new {}", previous_depth, current_depth)
                               }
                            }
                            current_node = Some(parent);

                            let child_node = Node::new(haml_code);
                            current_node = Some(tree.insert(child_node, UnderNode(current_node.as_ref().unwrap()))
                                .unwrap());
                        }
                    } else {
                        match haml_code {
                            HamlCode::HamlNodeBlock(_) => {
                                current_node = Some(tree.insert(Node::new(haml_code), AsRoot)
                                    .unwrap());
                            }
                            _ => panic!("No base node in stack found {:?}", haml_code),
                        }
                    }
                    previous_depth = current_depth;
                }
                _ => {}
            }

        }
        self.nodes = Some(tree);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    //#![feature(trace_macros)]
    use nom::IResult;
    use super::*;
    fn print_pre_order(buffer: String, tree: &Tree<HamlCode>, node_id: &NodeId) {
        let node_ref = tree.get(node_id).unwrap();

        println!("{}{:?}", buffer, node_ref.data());

        for child_id in node_ref.children() {
            let new_buff = format!("{}  ",buffer);
            print_pre_order(new_buff, tree, &child_id);
        }
    }
    #[test]
    fn it_parses_simple_string() {
        let haml = r#"%p 
  .give
    .up
      now
    .or
      later
"#;
        let mut parser = HAMLParser {
            nodes: None,
            haml: haml.to_string(),
        };
        parser.parse();
        let root = parser.nodes.as_ref().unwrap().root_node_id().unwrap().clone();
        print_pre_order("".to_string(), &parser.nodes.unwrap(), &root);

    }

    #[test]
    #[ignore]
    fn it_parses_haml_line_text_only() {
        let empty = &b""[..];
        let parsed_node = haml_line("prints".as_bytes());
        match parsed_node {
            IResult::Done(x, tup) => {
                assert_eq!(tup.1,  HamlCode::TextBlock("prints".to_string()));
            }
            _ => {
                assert_eq!(false, true);
            }
        }
    }


    #[test]
    fn it_parses_haml_line_context_block() {
        let empty = &b""[..];
        let parsed_node = haml_line("= prints".as_bytes());
        match parsed_node {
            IResult::Done(x, tup) => {
                assert_eq!(tup.1,  HamlCode::CodeBlock("prints".to_string()));
            }
            _ => {
                assert_eq!(false, true);
            }
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
            IResult::Done(x, tup) => {
                assert_eq!(tup.1,  HamlCode::HamlNodeBlock(node.clone()));
            }
            _ => {
                assert_eq!(false, true);
            }
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
    #[ignore]
    fn it_parses_html_tag_requires_tag() {
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
        // should be incomplete
        assert_eq!(html_tag("just the text".as_bytes()), IResult::Done(empty, (node)));
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
