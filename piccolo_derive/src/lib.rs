#![feature(get_type_id)]
#![recursion_limit="128"]
extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::{Body, VariantData, Variant};

#[proc_macro_derive(Foreign)]
pub fn foreign(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
//    match &ast.body {
//        &Body::Struct(ref v) => {
//            match v {
//                &VariantData::Struct(ref fields) => {
//                    for item in fields {
//                        println!("{:?}", item);
//                    }
//                },
//                _ => {}
//            }
//        },
//        _ => {}
//    }
    let gen = impl_foreign(&ast);
    gen.parse().unwrap()
}

fn impl_foreign(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = &ast.ident;
    let names = match &ast.body {
        &Body::Struct(ref v) => {
            match v {
                &VariantData::Struct(ref fields) => {
                    fields.iter().cloned().map(|n| n.ident.unwrap()).collect()
                },
                _ => { vec![] }
            }
        },
        _ => { vec![] }
    };
    let values = names.clone();
    let pls = names.clone();
    let halp = names.clone();

    quote! {
        impl Foreign for #name {
            fn get_name(&self) -> &'static str {
                stringify!(#name)
            }

            fn box_clone(&self) -> Box<Foreign> {
                Box::new(#name {
                    #(#names: self.#values.clone()),*
                })
            }

            fn compare(&self, rhs: &Foreign) -> Option<::std::cmp::Ordering> {
                //println!("{}", rhs.is::<#name>());
                //println!("{:?}, {:?}", ::std::any::TypeId::of::<#name>(), rhs.get_type_id());
                //panic!("wat");
                if rhs.is::<#name>() {
                    let rhs = rhs.downcast_ref::<#name>().unwrap();
                    if #(self.#pls == rhs.#halp)&&* {
                        return Some(::std::cmp::Ordering::Equal)
                    }
                }
                //if rhs.is::<#name>() {
                    //rhs.downcast_ref::<#name>().unwrap();
                //}
                //Any::downcast_ref::<#name>(&rhs).unwrap();
                None
                //match rhs.downcast_ref::<Value>() {
                //    _ => panic!("o hec")
                //}
                //match *rhs {
                //    Value::Foreign(ref f) => {
                //        //println!("{:?}", f.num);
                //        Any::downcast_ref::<#name>(f).unwrap();
                //        None
                //    },
                //    _ => None,
                //}
                //match rhs  {
                //    let eq = #(self.#pls == rhs.#halp)&&*;
                //    if eq {
                //        Some(::std::cmp::Ordering::Equal)
                //    } else {
                //        None
                //    }
                //}
                //None
            }
        }
    }
}

