use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, Parser},
    punctuated::Punctuated,
    *,
};

#[derive(Clone)]
struct Options {
    name: Ident,
    entries: Vec<Entry>,
}

impl Parse for Options {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let content;
        braced!(content in input);
        let entries = content
            .parse_terminated(Entry::parse, Token![,])?
            .into_iter()
            .collect();
        Ok(Self { name, entries })
    }
}

#[derive(Clone)]
struct Entry {
    name: Ident,
    kind: Ident,
    required: bool,
}

impl Parse for Entry {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let kind: Ident = input.parse()?;
        let required = input.lookahead1().peek(Token![?]);
        if required {
            input.parse::<Token![?]>()?;
        }
        Ok(Self {
            name,
            kind,
            required,
        })
    }
}

#[proc_macro]
pub fn options(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Options { name, entries } = parse_macro_input!(input as Options);
    let members = build_members(entries.clone());
    let args = build_args(entries.clone());
    quote! {
        struct #name {
            #members
        }

        impl #name {
            fn as_options() -> Vec<serenity::builder::CreateApplicationCommandOption> {
                vec![#args]
            }
        }
    }
    .into()
}

fn build_members(entries: Vec<Entry>) -> TokenStream {
    let mut members = Punctuated::<Field, Token![,]>::new();
    for Entry {
        name,
        kind,
        required,
    } in entries
    {
        let member = if required {
            quote! { pub #name: #kind }
        } else {
            quote! { pub #name: Option<#kind> }
        };
        members.push(Field::parse_named.parse2(member).unwrap());
    }
    quote!(#members)
}

fn build_args(entries: Vec<Entry>) -> TokenStream {
    let mut args = Punctuated::<Expr, Token![,]>::new();
    for Entry {
        name,
        kind,
        required,
    } in entries
    {
        let name = format!("\"{}\"", name);
        let kind = resolve_kind(kind);
        let arg = quote! {
            serenity::builder::CreateApplicationCommandOption::default()
                .required(#required)
                .name(#name)
                .description("Raises help")
                .kind(#kind)
                .to_owned()
        };
        args.push(Expr::parse.parse2(arg).unwrap());
    }
    quote!(#args)
}

fn resolve_kind(kind: Ident) -> Type {
    Type::parse
        .parse_str(match kind.to_string().as_str() {
            "String" => "serenity::model::prelude::command::CommandOptionType::String",
            "bool" => "serenity::model::prelude::command::CommandOptionType::Boolean",
            _ => panic!("Unknown kind {kind}"),
        })
        .unwrap()
}
