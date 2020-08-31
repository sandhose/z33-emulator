extern crate proc_macro;

use proc_macro2::{Ident, Literal, TokenStream};
use proc_macro_error::{abort, proc_macro_error};
use quote::quote;
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Type};

#[proc_macro_derive(Instruction, attributes(instruction))]
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
    arg1: Option<Type>,
    arg2: Option<Type>,
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
            let (arg1, arg2) = match variant.fields {
                syn::Fields::Named(_) => {
                    abort!(variant.fields.span(), "Named fields are not supported");
                }
                syn::Fields::Unnamed(ref fields) => match &fields.unnamed.len() {
                    1 => {
                        let mut it = fields.unnamed.iter();
                        let a = it.next().unwrap();
                        (Some(a.ty.clone()), None)
                    }
                    2 => {
                        let mut it = fields.unnamed.iter();
                        let a = it.next().unwrap();
                        let b = it.next().unwrap();
                        (Some(a.ty.clone()), Some(b.ty.clone()))
                    }
                    _ => {
                        abort!(fields.span(), "Unsupported number of fields");
                    }
                },
                syn::Fields::Unit => (None, None),
            };

            instructions.push(Instruction {
                ident: variant.ident.clone(),
                opcode,
                arg1,
                arg2,
            });
        }
    } else {
        abort!(ast.span(), "should be only called on enums")
    }

    let match_encode = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let opcode = instruction.opcode.clone();
        let arm = match (instruction.arg1.clone(), instruction.arg2.clone()) {
            (None, None) => quote! {
                Self::#t => { (#opcode, 0x00, 0x00) }
            },
            (Some(_), None) => quote! {
                Self::#t (arg1) => { (#opcode, arg1.encode(), 0x00) }
            },
            (Some(_), Some(_)) => quote! {
                Self::#t (arg1, arg2) => { (#opcode, arg1.encode(), arg2.encode()) }
            },
            _ => abort!(ast.span(), "error in macro"),
        };

        quote! {
            #arm,
            #acc
        }
    });

    let match_decode = instructions.iter().fold(quote!(), |acc, instruction| {
        let t = instruction.ident.clone();
        let opcode = instruction.opcode.clone();
        let arm = match (instruction.arg1.clone(), instruction.arg2.clone()) {
            (None, None) => quote! {
                #opcode => Self::#t
            },
            (Some(arg1), None) => quote! {
                #opcode => { Self::#t(self::#arg1::decode(arg1)?) }
            },
            (Some(arg1), Some(arg2)) => quote! {
                #opcode => { Self::#t(self::#arg1::decode(arg1)?, self::#arg2::decode(arg2)?) }
            },
            _ => abort!(ast.span(), "error in macro"),
        };
        quote! {
            #arm,
            #acc
        }
    });

    let im = quote! {
        impl Mmaped for #t {
            fn encode(self) -> u32 {
                let (p1, p2, p3) = match self {
                    #match_encode
                };

                assert!(p1 & 0x1F == p1);
                assert!(p2 & 0x3FFFFF == p2);
                assert!(p3 & 0x1F == p3);

                (p1 << 27) + (p2 << 5) + (p3)
            }

            fn decode(val: u32) -> Option<Self> {
                let inst = (val >> 27) & 0x1F; // 5 bits
                let arg1 = (val >> 5) & 0x3FFFFF; // 22 bits
                let arg2 = (val & 0x1F); // 5 bits
                let inst = match inst {
                    #match_decode
                    _ => return None,
                };
                Some(inst)
            }

            fn bitsize() -> u8 {
                32
            }
        }
    };

    Ok(im.into())
}
