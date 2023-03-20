use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
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
        Ok(Self {
            name,
            entries,
        })
    }
}

#[derive(Clone)]
struct Entry {
    desc: LitStr,
    name: Ident,
    kind: Ident,
    required: bool,
}

impl Parse for Entry {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let desc: LitStr = input.parse()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let kind: Ident = input.parse()?;
        let required = !input.parse::<Token![?]>().is_ok();
        Ok(Self {
            desc,
            name,
            kind,
            required,
        })
    }
}

#[proc_macro]
pub fn options(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let options = parse_macro_input!(input as Options);
    let members = build_members(options.clone());
    let args = build_args(options.clone());
    let try_from = build_try_from(options.clone());
    quote! {
        #members

        #args

        #try_from
    }
    .into()
}

fn build_members(options: Options) -> TokenStream {
    let Options { name, entries, .. } = options;
    let mut members = Punctuated::<Field, Token![,]>::new();
    for Entry {
        name,
        kind,
        required,
        ..
    } in entries
    {
        let member = if required {
            quote! { pub #name: #kind }
        } else {
            quote! { pub #name: Option<#kind> }
        };
        members.push(Field::parse_named.parse2(member).unwrap());
    }
    quote! {
        pub struct #name {
            #members
        }
    }
}

fn build_args(options: Options) -> TokenStream {
    let Options {
        name,
        entries,
    } = options;
    let mut args = Punctuated::<Expr, Token![,]>::new();
    for Entry {
        desc,
        name,
        kind,
        required,
    } in entries
    {
        let name = name.to_string();
        let kind = resolve_kind(kind);
        let arg = quote! {
            serenity::builder::CreateApplicationCommandOption::default()
                .required(#required)
                .name(#name)
                .description(#desc)
                .kind(#kind)
                .to_owned()
        };
        args.push(Expr::parse.parse2(arg).unwrap());
    }
    quote! {
        impl #name {
            pub fn as_options() -> Vec<serenity::builder::CreateApplicationCommandOption> {
                vec![#args]
            }

            pub fn apply(command: &mut serenity::builder::CreateApplicationCommand) -> &mut serenity::builder::CreateApplicationCommand {
                #name::as_options()
                    .into_iter()
                    .fold(command, |cmd, opt| cmd.add_option(opt))
            }
        }
    }
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

fn build_try_from(options: Options) -> TokenStream {
    let Options { name, entries } = options;
    let mut defs = Punctuated::<TokenStream, Token![;]>::new();
    let mut enters = Punctuated::<Ident, Token![,]>::new();
    for Entry {
        name,
        kind,
        required,
        ..
    } in entries
    {
        let name_s = name.to_string();
        let kind_s = kind.to_string();
        let stmt = match kind_s.as_str() {
            "String" => {
                quote! { let #name = inject(value, #name_s).and_then(|x| x.as_str()).map(|x| x.to_string()) }
            }
            "bool" => quote! { let #name = inject(value, #name_s).and_then(|x| x.as_bool()) },
            _ => unreachable!(),
        };
        defs.push(if required {
            quote! { #stmt.unwrap() }
        } else {
            stmt
        });
        enters.push(Ident::parse.parse2(name.to_token_stream()).unwrap())
    }
    defs.push_punct(parse_quote! { ; });
    quote! {
        impl From<&serenity::model::prelude::interaction::application_command::ApplicationCommandInteraction> for #name {
            fn from(value: &serenity::model::prelude::interaction::application_command::ApplicationCommandInteraction) -> Self {
                fn inject<'a>(
                    value: &'a serenity::model::prelude::interaction::application_command::ApplicationCommandInteraction,
                    name: &'static str,
                ) -> Option<&'a serenity::json::Value> {
                    value.data.options.iter().find_map(|x| {
                        if x.name == name {
                            x.value.as_ref()
                        } else {
                            None
                        }
                    })
                }

                #defs

                Self { #enters }
            }
        }
    }
}
