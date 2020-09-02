extern crate proc_macro;

use proc_macro2::{Ident, Literal, TokenStream};
use proc_macro_error::{abort, proc_macro_error};
use quote::quote;
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Type};

// TODO: use appropriate span in quotes

#[proc_macro_derive(Instruction, attributes(instruction, labelable))]
#[proc_macro_error]
pub fn derive_instruction(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the string representation
    let input: DeriveInput = syn::parse(input).unwrap();

    // Build the impl
    let gen = impl_instruction(&input).unwrap();

    // Return the generated impl
    gen.into()
}

#[derive(Debug)]
struct Instruction {
    ident: Ident,
    opcode: Literal,
    args: Vec<Type>,
    labelable: Option<usize>,
}

fn impl_instruction(ast: &DeriveInput) -> syn::parse::Result<TokenStream> {
    let t = ast.ident.clone();
    let mut instructions = Vec::new();

    if let Data::Enum(ref e) = ast.data {
        for variant in e.variants.iter() {
            // Let's parse the opcode
            let mut opcode = None;
            for attr in variant.attrs.iter() {
                if let Some(ident) = attr.path.get_ident() {
                    if ident.to_string() == "instruction" {
                        let args: Literal = attr.parse_args()?;
                        opcode = Some(args);
                    }
                }
            }

            if opcode.is_none() {
                abort!(variant.span(), "No opcode for this instruction");
            }
            let opcode = opcode.unwrap();

            // Let's parse the fields
            let (args, labelable) = match variant.fields {
                syn::Fields::Named(_) => {
                    abort!(variant.fields.span(), "Named fields are not supported");
                }
                syn::Fields::Unnamed(ref fields) => {
                    let args = fields.unnamed.iter().map(|f| f.ty.clone()).collect();
                    // TODO: warn if multiple labelable are set
                    let labelable = fields.unnamed.iter().position(|f| {
                        f.attrs.iter().any(|attr| {
                            attr.path
                                .get_ident()
                                .filter(|ident| ident.to_string() == "labelable")
                                .is_some()
                        })
                    });
                    (args, labelable)
                }
                syn::Fields::Unit => (Vec::new(), None),
            };

            instructions.push(Instruction {
                ident: variant.ident.clone(),
                opcode,
                args,
                labelable,
            });
        }
    } else {
        abort!(ast.span(), "should be only called on enums")
    }

    let match_encode = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let opcode = instruction.opcode.clone();
        let arm = if instruction.args.is_empty() {
            quote! {
                Self::#t => { vec![#opcode] }
            }
        } else {
            let (pat, body) = instruction.args.iter().enumerate().fold(
                (quote! {}, quote! {}),
                |(pat, body), (i, arg)| {
                    let id = Ident::new(format!("arg{}", i).as_str(), arg.span());
                    let pat = quote! { #pat #id, };
                    let body = quote! {
                        #body
                        v.extend(#id.encode());
                    };
                    (pat, body)
                },
            );

            quote! {
                Self::#t(#pat) => {
                    let mut v = vec![#opcode];
                    #body
                    v
                }
            }
        };

        quote! {
            #arm,
            #acc
        }
    });

    let match_decode = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let opcode = instruction.opcode.clone();
        let arm = if instruction.args.is_empty() {
            quote! {
                #opcode => { Self::#t }
            }
        } else {
            let (pat, body) = instruction.args.iter().enumerate().fold(
                (quote! {}, quote! {}),
                |(pat, body), (i, arg)| {
                    let id = Ident::new(format!("arg{}", i).as_str(), arg.span());
                    let pat = quote! { #pat #id, };
                    let body = quote! {
                        #body
                        let #id = self::#arg::decode(reader)?;
                    };
                    (pat, body)
                },
            );

            quote! {
                #opcode => {
                    #body
                    Self::#t(#pat)
                }
            }
        };

        quote! {
            #arm,
            #acc
        }
    });

    let match_label = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let arm = if instruction.args.is_empty() {
            quote! {
                Self::#t => None
            }
        } else if let Some(labelable) = instruction.labelable {
            let pat = instruction
                .args
                .iter()
                .enumerate()
                .fold(quote! {}, |pat, (i, arg)| {
                    let id = Ident::new(format!("arg{}", i).as_str(), arg.span());
                    quote! { #pat #id, }
                });

            let labelable = Ident::new(format!("arg{}", labelable).as_str(), t.span());

            quote! {
                Self::#t(#pat) => {
                    let #labelable = #labelable.resolve_label(address)?;
                    Some(Self::#t(#pat))
                }
            }
        } else {
            quote! {
                Self::#t(..) => None
            }
        };

        quote! {
            #arm,
            #acc
        }
    });

    let im = quote! {
        impl Encodable for #t {
            fn encode(self) -> Vec<u8> {
                match self {
                    #match_encode
                }
            }
        }

        impl Decodable for #t {
            fn decode<T: ::std::io::Read>(reader: &mut T) -> Option<Self> {
                let mut buf = [0];
                reader.read_exact(&mut buf).ok()?;
                let inst = buf[0];
                let inst = match inst {
                    #match_decode
                    _ => return None,
                };
                Some(inst)
            }
        }

        impl Labelable for #t {
            fn resolve_label(self, address: u16) -> Option<Self> {
                match self {
                    #match_label
                }
            }
        }
    };

    Ok(im.into())
}
