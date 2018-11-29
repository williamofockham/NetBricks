#![feature(proc_macro_diagnostic)]
#![recursion_limit = "128"]
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate static_assertions;
#[macro_use]
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as P2TS;
use quote::{quote, ToTokens};
use std::fmt;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::Bracket;
use syn::{bracketed, parenthesized, AttributeArgs, Ident, ItemFn, LitInt, Token};

#[derive(Debug)]
struct HdrType {
    hdr: Ident,
    prev_hdr: Option<Ident>,
}

#[derive(Debug)]
enum ValType {
    Int(LitInt),
    Val(Ident),
}

impl ValType {
    fn to_assert(&self, op: &Ident, is_ident: bool, is_fn: bool) -> P2TS {
        match (op.to_string().as_str(), is_ident, is_fn) {
            ("neq", _, true) => quote! {not(eq(r.#self()))},
            ("neq", true, false) => quote! {not(eq(r.#self))},
            ("neq", false, false) => quote! {not(eq(#self))},
            ("nleq", _, true) => quote! {not(leq(r.#self()))},
            ("nleq", true, false) => quote! {not(leq(r.#self))},
            ("nleq", false, false) => quote! {not(leq(#self))},
            ("ngeq", _, true) => quote! {not(geq(r.#self()))},
            ("ngeq", true, false) => quote! {not(geq(r.#self))},
            ("ngeq", false, false) => quote! {not(geq(#self))},
            ("ngt", _, true) => quote! {not(gt(r.#self()))},
            ("ngt", true, false) => quote! {not(gt(r.#self))},
            ("ngt", false, false) => quote! {not(gt(#self))},
            ("nlt", _, true) => quote! {not(lt(r.#self()))},
            ("nlt", true, false) => quote! {not(lt(r.#self))},
            ("nlt", false, false) => quote! {not(lt(#self))},
            (_, _, true) => quote! {#op(r.#self())},
            (_, true, false) => quote! {#op(r.#self)},
            (_, _, _) => quote! {#op(#self)},
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ValType::Int(ref t) => write!(f, "{}", t.value().to_string()),
            ValType::Val(ref t) => write!(f, "{}", t.to_string()),
        }
    }
}

impl ToTokens for ValType {
    fn to_tokens(&self, tokens: &mut P2TS) {
        match *self {
            ValType::Int(ref t) => {
                t.to_tokens(tokens);
            }
            ValType::Val(ref t) => {
                t.to_tokens(tokens);
            }
        }
    }
}

#[derive(Debug)]
struct Check {
    is_fn_lhs: bool,
    is_fn_rhs: bool,
    call: Ident,
    op: Ident,
    val: ValType,
    loc_lhs: Option<Ident>,
    loc_rhs: Option<HdrType>,
    cast: Option<Ident>,
}

#[derive(Debug)]
struct Checker {
    order: Vec<HdrType>,
    checks: Vec<Check>,
}

impl Parse for HdrType {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Ident) {
            let hdr = input.parse::<Ident>()?;
            if input.peek(Token![<]) && input.peek3(Token![>]) {
                let _ = input.parse::<Token![<]>();
                let inner = input.parse::<Ident>()?;
                let _ = input.parse::<Token![>]>();
                Ok(HdrType {
                    hdr: hdr,
                    prev_hdr: Some(inner),
                })
            } else {
                Ok(HdrType {
                    hdr: hdr,
                    prev_hdr: None,
                })
            }
        } else {
            unimplemented!()
        }
    }
}

impl Parse for Check {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut is_fun_lhs = false;
        let mut is_fun_rhs = false;
        let mut cast = None;

        let contents;
        parenthesized!(contents in input);

        if contents.peek(Ident) {
            is_fun_lhs = true;
        } else if contents.peek(Token![.]) && contents.peek2(Ident) {
            contents.parse::<Token![.]>()?;
        } else {
            unimplemented!()
        }

        let call = contents.parse::<Ident>()?;

        let loc_lhs = if contents.peek(Bracket) {
            let inner;
            bracketed!(inner in contents);
            Some(inner.parse::<Ident>()?)
        } else {
            None
        };

        if contents.peek(Token![as]) {
            contents.parse::<Token![as]>()?;
            cast = Some(contents.parse::<Ident>()?);
        }

        contents.parse::<Token![,]>()?;

        let op = contents.parse::<Ident>()?;

        contents.parse::<Token![,]>()?;

        let val: ValType = if contents.peek(Ident) {
            is_fun_rhs = true;
            ValType::Val(contents.parse::<Ident>()?)
        } else if contents.peek(Token![.]) && contents.peek2(Ident) {
            contents.parse::<Token![.]>()?;
            ValType::Val(contents.parse::<Ident>()?)
        } else if contents.peek(LitInt) {
            ValType::Int(contents.parse::<LitInt>()?)
        } else {
            panic!("")
        };

        let loc_rhs = if contents.peek(Bracket) {
            let inner_rhs;
            bracketed!(inner_rhs in contents);
            let hdr = inner_rhs.parse::<Ident>()?;

            if inner_rhs.peek(Token![<]) && inner_rhs.peek3(Token![>]) {
                let _ = inner_rhs.parse::<Token![<]>();
                let inner_angle = inner_rhs.parse::<Ident>()?;
                let _ = inner_rhs.parse::<Token![>]>();
                Some(HdrType {
                    hdr: hdr,
                    prev_hdr: Some(inner_angle),
                })
            } else {
                Some(HdrType {
                    hdr: hdr,
                    prev_hdr: None,
                })
            }
        } else {
            is_fun_rhs = false;
            None
        };

        Ok(Check {
            is_fn_lhs: is_fun_lhs,
            is_fn_rhs: is_fun_rhs,
            call: call,
            loc_lhs: loc_lhs,
            loc_rhs: loc_rhs,
            op: op,
            val: val,
            cast: cast,
        })
    }
}

impl Parse for Checker {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut hdr_order: Vec<HdrType> = vec![];
        let mut checks: Vec<Check> = vec![];

        loop {
            if input.peek(Ident) && input.peek2(Token![:]) {
                let ident = input.parse::<Ident>()?;
                match ident.to_string().as_str() {
                    "order" => {
                        input.parse::<Token![:]>()?;
                        if input.peek(Bracket) {
                            let contents;
                            bracketed!(contents in input);
                            hdr_order =
                                Punctuated::<HdrType, Token![=>]>::parse_terminated(&contents)?
                                    .into_iter()
                                    .collect()
                        }
                    }
                    "checks" => {
                        input.parse::<Token![:]>()?;
                        if input.peek(Bracket) {
                            let contents;
                            bracketed!(contents in input);
                            checks = Punctuated::<Check, Token![,]>::parse_terminated(&contents)?
                                .into_iter()
                                .collect()
                        }
                    }
                    _ => break,
                }
            } else {
                break;
            }

            if input.is_empty() {
                break;
            } else {
                input.parse::<Token![,]>()?;
            }
        }

        //println!("Fuck: {:#?}, {:#?}", hdr_order, checks);

        Ok(Checker {
            order: hdr_order,
            checks: checks,
        })
    }
}

#[proc_macro]
pub fn ingress_check(input: TokenStream) -> TokenStream {
    let Checker { order, checks } = parse_macro_input!(input as Checker);

    // Error Checks
    // TODO: Include spans
    if order.is_empty() {
        // throw error
    }

    let mut curr_type: &Ident = &order.first().unwrap().hdr;
    let init_var = Ident::new("_pkt", Span::call_site());

    let runner = order
        .iter()
        .enumerate()
        .map(|(i, HdrType { hdr, prev_hdr })| {
            let var = Ident::new(&format!("_pkt{}", hdr), Span::call_site());
            let obj = Ident::new(&format!("_pkt{}", curr_type), Span::call_site());
            let hget = Ident::new(&format!("{}_hdr", var), Span::call_site());
            let hget_payload = Ident::new(&format!("{}_payload", hget), Span::call_site());
            let hdr_type = if let Some(prev) = prev_hdr {
                quote!{#hdr<#prev>}
            } else {
                quote!{#hdr}
            };

            let save = quote! {
                let #hget_payload = unsafe {
                    std::slice::from_raw_parts((#hget as *const #hdr_type) as *const u8, std::mem::size_of::<T>())
                };
                _CHECK.lock().unwrap().insert(stringify!(#hdr), #hget_payload.to_vec());
            };

            let checks = checks.iter().map(
                |Check {
                     is_fn_lhs,
                     call,
                     op,
                     val,
                     loc_lhs,
                     cast,
                     ..
                 }| {
                    if let Some(loc) = loc_lhs {
                        if loc == hdr {
                            let rhs = val.to_assert(op, false, false);

                            if let Some(c) = cast {
                                if *is_fn_lhs {
                                    quote! {
                                        assert_that!(&(#hget.#call() as #c), #rhs);
                                    }
                                } else {
                                    quote! {
                                        assert_that!(&(#hget.#call as #c), #rhs);
                                    }
                                }
                            } else {
                                if *is_fn_lhs {
                                    quote! {
                                        assert_that!(&#hget.#call(), #rhs);
                                    }
                                } else {
                                    quote! {
                                        assert_that!(&#hget.#call, #rhs);
                                    }
                                }
                            }
                        } else {
                            quote! {}
                        }
                    } else {
                        quote! {}
                    }
                },
            );

            let get_header = quote! {
                let #hget = #var.get_header();
            };

            let stmt = if i > 0 {
                if let Some(h) = prev_hdr {
                    quote! {
                        let #var = #obj.parse_header::<#hdr<#h>>();
                        {
                            #get_header
                            {
                                #save
                            }
                            #({#checks})*
                        }
                    }
                } else {
                    quote! {
                        let #var = #obj.parse_header::<#hdr>();
                        {
                            #get_header
                            {
                                #save
                            }
                            #({#checks})*
                        }
                    }
                }
            } else {
                quote! {
                    let #var = #init_var.parse_header::<#hdr>();
                    {
                        #get_header
                        {
                            #save
                        }

                        #({#checks})*
                    }
                }
            };

            curr_type = hdr;
            stmt
        });

    let expanded = if cfg!(debug_assertions) {
        quote! {
        let #init_var = pkt.clone().reset();
        #(#runner)*
        ()
        }
    } else {
        quote! {}
    };

    TokenStream::from(expanded)
}

#[proc_macro]
pub fn egress_check(input: TokenStream) -> TokenStream {
    let Checker { order, checks } = parse_macro_input!(input as Checker);

    let mut curr_type: &Ident = &order.first().unwrap().hdr;
    let init_var = Ident::new("_pkt", Span::call_site());

    let runner = order
        .iter()
        .enumerate()
        .map(|(i, HdrType { hdr, prev_hdr })| {
            let var = Ident::new(&format!("_pkt{}", hdr), Span::call_site());
            let obj = Ident::new(&format!("_pkt{}", curr_type), Span::call_site());
            let hget = Ident::new(&format!("{}_hdr", var), Span::call_site());

            let checks = checks.iter().map(
                |Check {
                     is_fn_lhs,
                     is_fn_rhs,
                     call,
                     op,
                     val,
                     loc_lhs,
                     loc_rhs,
                     cast,
                 }| {
                    if let Some(loc) = loc_lhs {
                        if loc == hdr {
                            let q = if let Some(HdrType {hdr: loc_rhs_hdr, prev_hdr: loc_rhs_prev}) = loc_rhs {
                                let rhs = Ident::new(
                                    &format!("{}", loc_rhs_hdr).to_lowercase(),
                                    Span::call_site(),
                                );

                                let rhs_type = if let Some(rhs_prev_type) = loc_rhs_prev {
                                    quote! {
                                        #loc_rhs_hdr<#rhs_prev_type>
                                    }
                                } else {
                                    quote! {
                                        #loc_rhs_hdr
                                    }
                                };
                                (
                                    Some(rhs.clone()),
                                    quote! {
                                        let m = _CHECK.lock().unwrap();
                                        let #rhs: Option<#rhs_type> = match m.get(stringify!(#loc_rhs_hdr)) {
                                            Some(v) => {
                                                let _h: #rhs_type = unsafe { std::ptr::read(v.as_ptr() as *const _) };
                                                Some(_h)
                                            },
                                            _ => None
                                        };
                                    },
                                )
                            } else {
                                (None, quote! {})
                            };

                            let qc = if let Some(c) = cast {
                                (quote! {#hget.#call as #c}, quote! {#hget.#call() as #c})
                            } else {
                                (quote! {#hget.#call}, quote! {#hget.#call()})
                            };

                            if *is_fn_lhs && *is_fn_rhs {
                                let lhs = qc.1;
                                let _rhs = q.0.unwrap();
                                let m = q.1;
                                let rhs = val.to_assert(op, *is_fn_rhs, *is_fn_rhs);
                                quote! {
                                    #m
                                    if let Some(r) = #_rhs {
                                        assert_that!(&(#lhs), #rhs);
                                    }
                                }
                            } else if *is_fn_lhs && !*is_fn_rhs {
                                let lhs = qc.1;
                                let is_field = q.0.is_some();
                                let rhs = val.to_assert(op, is_field, *is_fn_rhs);
                                if is_field {
                                    let _rhs = q.0.unwrap();
                                    let m = q.1;
                                    quote! {
                                        #m
                                        if let Some(r) = #_rhs {
                                            assert_that!(&(#lhs), #rhs);
                                        }

                                    }
                                } else {
                                    quote! {
                                        assert_that!(&(#lhs), #rhs);
                                    }
                                }
                            } else if !*is_fn_lhs && *is_fn_rhs {
                                let lhs = qc.0;
                                let _rhs = q.0.unwrap();
                                let m = q.1;
                                let rhs = val.to_assert(op, *is_fn_rhs, *is_fn_rhs);
                                quote! {
                                    #m
                                    if let Some(r) = #_rhs {
                                        assert_that!(&(#lhs), #rhs);
                                    }
                                }
                            } else {
                                let lhs = qc.0;
                                let is_field = q.0.is_some();
                                let rhs = val.to_assert(op, is_field, *is_fn_rhs);
                                if is_field {
                                    let _rhs = q.0.unwrap();
                                    let m = q.1;
                                    quote! {
                                        #m
                                        if let Some(r) = #_rhs {
                                            assert_that!(&(#lhs), #rhs);
                                        }
                                    }
                                } else {
                                    quote! {
                                        assert_that!(&(#lhs), #rhs);
                                    }
                                }
                            }
                        } else {
                            quote! {}
                        }
                    } else {
                        quote! {}
                    }
                },
            );

            let get_header = quote! {
                let #hget = #var.get_header();
            };

            let stmt = if i > 0 {
                if let Some(h) = prev_hdr {
                    quote! {
                        let #var = #obj.parse_header::<#hdr<#h>>();
                        {
                            #get_header
                            #({#checks})*
                        }
                    }
                } else {
                    quote! {
                        let #var = #obj.parse_header::<#hdr>();
                        {
                            #get_header
                            #({#checks})*
                        }
                    }
                }
            } else {
                quote! {
                    let #var = #init_var.parse_header::<#hdr>();
                    {
                        #get_header
                        #({#checks})*
                     }
                }
            };

            curr_type = hdr;
            stmt
        });

    let expanded = if cfg!(debug_assertions) {
        quote! {
            let #init_var = pkt.clone().reset();
            #(#runner)*
            ()
        }
    } else {
        quote! {}
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _a = parse_macro_input!(args as AttributeArgs);
    let _input = parse_macro_input!(input as ItemFn);

    let chk_map = quote! {
        use std::{collections::HashMap, sync::Mutex};
        use once_cell::sync::{Lazy, OnceCell};
        use once_cell::sync_lazy;
        use galvanic_assert::assert_that;
        use galvanic_assert::matchers::*;

        static _CHECK: Lazy<Mutex<HashMap<&'static str, Vec<u8>>>> = sync_lazy! {
            let mut m = HashMap::new();
            Mutex::new(m)
        };
    };

    let expanded = if cfg!(debug_assertions) {
        quote! {
            #chk_map

            #_input

        }
    } else {
        quote! {
            #_input
        }
    };

    TokenStream::from(expanded)
}
