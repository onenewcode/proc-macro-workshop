use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Fields, MetaNameValue};
#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    
    // Check if this is a struct and extract its fields
    let fields = match &ast.data {
        syn::Data::Struct(data_struct) => &data_struct.fields,
        _ => panic!("CustomDebug can only be derived for structs"),
    };
    
    // Generate code for each field directly in the match arms
    let expanded = match fields {
        Fields::Named(named_fields) => {
            let field_calls = named_fields.named.iter().map(|field| {
                let field_name = &field.ident;
                
                // Check for debug attribute
                for attr in &field.attrs {
                    if attr.path().is_ident("debug") {
                        // In syn 2.0, we use the meta field to access attribute metadata
                        if let syn::Meta::NameValue(MetaNameValue { value, .. }) = &attr.meta {
                            // Check if the value is a string literal
                            if let syn::Expr::Lit(syn::ExprLit { lit, .. }) = value {
                                if let syn::Lit::Str(str_lit) = lit {
                                    // Extract the string value
                                    let format_str = str_lit.value();
                                    // Create a closure that captures the field value
                                    return quote! {
                                        .field(stringify!(#field_name), &{
                                            // Create a wrapper type that holds the field value
                                            struct DebugWrapper<T>(T);
                                            impl<T> std::fmt::Debug for DebugWrapper<T>
                                            where
                                                T: std::fmt::Binary,
                                            {
                                                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                                                    write!(f, #format_str, self.0)
                                                }
                                            }
                                            DebugWrapper(self.#field_name)
                                        })
                                    };
                                }
                            }
                        }
                        break;
                    }
                }
                
                // Default debug format
                quote! { .field(stringify!(#field_name), &self.#field_name) }
            });
            
            quote! {
                impl std::fmt::Debug for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                        f.debug_struct(stringify!(#name))
                            #(#field_calls)*
                            .finish()
                    }
                }
            }
        },
        Fields::Unnamed(unnamed_fields) => {
            let field_calls = unnamed_fields.unnamed.iter().enumerate().map(|(i, _)| {
                let index = syn::Index::from(i);
                quote! { .field(&self.#index) }
            });
            
            quote! {
                impl std::fmt::Debug for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                        f.debug_tuple(stringify!(#name))
                            #(#field_calls)*
                            .finish()
                    }
                }
            }
        },
        Fields::Unit => {
            quote! {
                impl std::fmt::Debug for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                        f.debug_struct(stringify!(#name))
                            .finish()
                    }
                }
            }
        },
    };

    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}
