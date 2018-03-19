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

    quote! {
        impl Foreign for #name {
            fn get_name(&self) -> &'static str {
                stringify!(#name)
            }

            fn compare(&self, rhs: &Foreign) -> Option<::std::cmp::Ordering> {
                if rhs.is::<#name>() {
                    let rhs = rhs.downcast_ref::<#name>().unwrap();
                    if #(self.#names == rhs.#names2)&&* {
                        return Some(::std::cmp::Ordering::Equal)
                    }
                }
                None
            }
        }
    }
}

