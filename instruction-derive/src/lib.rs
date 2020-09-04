extern crate proc_macro;

use proc_macro2::{Ident, Literal, Span, TokenStream};
use proc_macro_error::{abort, proc_macro_error};
use quote::{quote, quote_spanned};
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
            quote_spanned! { Span::call_site() =>
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

            quote_spanned! { Span::call_site() =>
                Self::#t(#pat) => {
                    let mut v = vec![#opcode];
                    #body
                    v
                }
            }
        };

        quote_spanned! { Span::call_site() =>
            #arm,
            #acc
        }
    });

    let match_decode = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let opcode = instruction.opcode.clone();
        let arm = if instruction.args.is_empty() {
            quote_spanned! { Span::call_site() =>
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

            quote_spanned! { Span::call_site() =>
                #opcode => {
                    #body
                    Self::#t(#pat)
                }
            }
        };

        quote_spanned! { Span::call_site() =>
            #arm,
            #acc
        }
    });

    let match_label = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let arm = if instruction.args.is_empty() {
            quote_spanned! { Span::call_site() =>
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

            quote_spanned! { Span::call_site() =>
                Self::#t(#pat) => {
                    let #labelable = #labelable.resolve_label(address)?;
                    Some(Self::#t(#pat))
                }
            }
        } else {
            quote_spanned! { Span::call_site() =>
                Self::#t(..) => None
            }
        };

        quote_spanned! { Span::call_site() =>
            #arm,
            #acc
        }
    });

    let match_display = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let inst = t.to_string().to_lowercase();
        let arm = if instruction.args.is_empty() {
            let inst = Literal::string(&inst);
            quote_spanned! { Span::call_site() =>
                Self::#t => write!(f, #inst)
            }
        } else {
            let pat = instruction
                .args
                .iter()
                .enumerate()
                .fold(quote! {}, |pat, (i, arg)| {
                    let id = Ident::new(format!("arg{}", i).as_str(), arg.span());
                    quote! { #pat #id, }
                });

            let parts: Vec<_> = std::iter::repeat("{}")
                .take(instruction.args.len())
                .collect();
            let literal = format!("{} {}", inst, parts.join(", "));
            let literal = Literal::string(&literal);

            quote_spanned! { Span::call_site() =>
                Self::#t(#pat) => write!(f, #literal, #pat)
            }
        };

        quote_spanned! { Span::call_site() =>
            #arm,
            #acc
        }
    });

    let (parser_alt, parser_funcs) = instructions.iter().enumerate().fold((quote!(), quote!()), |(alt, funcs), (i, instruction)| {
        let t = instruction.ident.clone();
        let inst = t.to_string().to_lowercase();
        let func_name = Ident::new(format!("parse_instruction_{}", inst).as_str(), t.span());
        let inst = Literal::string(&inst);

        let parser = if instruction.args.is_empty() {
            quote_spanned! { Span::call_site() =>
                fn #func_name(input: &str) -> ::nom::IResult<&str, (Option<&str>, Self)> {
                    let (input, _) = ::nom::bytes::complete::tag_no_case(#inst)(input)?;
                    Ok((input, (None, Self::#t)))
                }
            }
        } else {
            let (body, pat) = instruction.args.iter().enumerate().fold(
                (quote!(), quote!()),
                |(body, pat), (i, arg)| {
                    let id = Ident::new(format!("arg{}", i).as_str(), arg.span());

                    let body = if i > 0 {
                        quote_spanned! { Span::call_site() =>
                            #body
                            let (input, _) = ::nom::character::complete::char(',')(input)?;
                            let (input, _) = ::nom::character::complete::space0(input)?;
                        }
                    } else {
                        body
                    };

                    let body = if Some(i) == instruction.labelable {
                        quote_spanned! { Span::call_site() =>
                            #body
                            let (input, (label, #id)) = crate::processor::Parsable::parse_labelable(input)?;
                            let (input, _) = ::nom::character::complete::space0(input)?;
                        }
                    } else {
                        quote_spanned! { Span::call_site() =>
                            #body
                            let (input, #id) = crate::processor::Parsable::parse(input)?;
                            let (input, _) = ::nom::character::complete::space0(input)?;
                        }
                    };
                    let pat = quote! { #pat #id, };
                    (body, pat)
                },
            );

            quote_spanned! { Span::call_site() =>
                fn #func_name(input: &str) -> ::nom::IResult<&str, (Option<&str>, Self)> {
                    let (input, _) = ::nom::bytes::complete::tag_no_case(#inst)(input)?;
                    let (input, _) = ::nom::character::complete::space1(input)?;
                    let label: Option<&str> = None;
                    #body
                    Ok((input, (label, Self::#t(#pat))))
                }
            }
        };

        let alt = if i == 0 {
            quote_spanned! { Span::call_site() =>
                Self::#func_name(input)
            }
        } else {
            quote_spanned! { Span::call_site() =>
                #alt.or_else(|_| Self::#func_name(input))
            }
        };

        let funcs = quote! {
            #parser
            #funcs
        };

        (alt, funcs)
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

            fn label() -> Self {
                unimplemented!()
            }
        }

        impl ::std::fmt::Display for #t {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #match_display
                }
            }
        }

        impl #t {
            pub fn parse(input: &str) -> ::nom::IResult<&str, (Option<&str>, Self)> {
                #parser_alt
            }

            #parser_funcs
        }
    };

    Ok(im.into())
}
