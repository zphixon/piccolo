#![recursion_limit="128"]
extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::{Body, VariantData};

#[proc_macro_derive(Foreign)]
pub fn foreign(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
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
    let names2 = names.clone();
    let names3 = names.clone();
    let names4 = names.clone();
    let names5 = names.clone();
    let names6 = names.clone();
    let names7 = names.clone();
    let names8 = names.clone();
    let names9 = names.clone();

    quote! {
        impl Foreign for #name {
            fn get_name(&self) -> &'static str {
                stringify!(#name)
            }

            //fn box_clone(&self) -> Box<Foreign> {
            //    Box::new(#name {
            //        #(#names: self.#names2.clone()),*
            //    })
            //}

            fn get(&self, name: &str) -> Option<::value::Value> {
                #(if name == stringify!(#names5) {
                    return Some(self.#names6.clone().into())
                })*
                None
            }

            fn set(&mut self, name: &str, value: ::value::Value) -> Result<(), ()> {
                //print!("set {} to {:?}", name, value);
                #(if name == stringify!(#names7) {
                    //println!(" from {:?}", self.#names9);
                    //self.#names8.replace(value.into());
                    use ::value::TryInto;
                    self.#names8 = value.try_into().map_err(|_| ())?;
                    return Ok(())
                })*
                Err(())
            }

            fn compare(&self, rhs: &ForeignOuter) -> Option<::std::cmp::Ordering> {
                let rhs = rhs.inner.borrow();
                if rhs.is::<#name>() {
                    let rhs = rhs.downcast_ref::<#name>().unwrap();
                    if #(self.#names3 == rhs.#names4)&&* {
                        return Some(::std::cmp::Ordering::Equal)
                    }
                }
                None
            }
        }

        impl Into<::value::Value> for #name {
            fn into(self) -> ::value::Value {
                ::value::Value::Foreign(::foreign::ForeignOuter::new(self))
            }
        }
    }
}

