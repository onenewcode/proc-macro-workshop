use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Fields, Ident, Type, TypePath, GenericArgument, PathArguments};
use quote::quote;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    let struct_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), struct_name.span());
    
    let mut fields = Vec::new();
    let mut field_inits = Vec::new();
    let mut setter_methods = Vec::new();
    let mut build_fields = Vec::new();
    
    if let Data::Struct(data_struct) = &input.data {
        if let Fields::Named(named_fields) = &data_struct.fields {
            for field in &named_fields.named {
                let field_name = field.ident.as_ref().unwrap();
                let field_type = &field.ty;
                let field_span = field_name.span();
                
                // Check for builder attributes
                let mut is_repeated = false;
                let mut each_attr = None;
                
                for attr in &field.attrs {
                    if attr.path().is_ident("builder") {
                        // Use parse_nested_meta to parse the attribute
                        let result = attr.parse_nested_meta(|meta| {
                            if meta.path.is_ident("each") {
                                let value = meta.value()?;
                                let lit_str = value.parse::<syn::LitStr>()?;
                                each_attr = Some(lit_str.value());
                                is_repeated = true;
                                Ok(())
                            } else {
                                Err(meta.error("expected `each = \"...\"`"))
                            }
                        });
                        
                        if result.is_err() {
                            // Unrecognized attribute
                            return TokenStream::from(quote! {
                                compile_error!("expected `builder(each = \"...\")`");
                            });
                        }
                    }
                }
                
                if is_repeated {
                    // For repeated fields (Vec<T> with #[builder(each = "...")])
                    let each_name = Ident::new(&each_attr.unwrap(), field_span);
                    
                    // Store as the same Vec<T> type, not Vec<Vec<T>>
                    fields.push(quote! {
                        #field_name: #field_type
                    });
                    field_inits.push(quote! {
                        #field_name: Default::default()
                    });
                    
                    // Create a setter that pushes to the vector
                    setter_methods.push(quote! {
                        pub fn #each_name(&mut self, value: <#field_type as IntoIterator>::Item) -> &mut Self {
                            self.#field_name.push(value);
                            self
                        }
                    });
                    
                    build_fields.push(quote! {
                        #field_name: self.#field_name.clone()
                    });
                } else if let Some(inner_type) = get_option_inner_type(field_type) {
                    // For Option<T> fields
                    fields.push(quote! {
                        #field_name: #field_type
                    });
                    field_inits.push(quote! {
                        #field_name: Default::default()
                    });
                    
                    setter_methods.push(quote! {
                        pub fn #field_name(&mut self, #field_name: #inner_type) -> &mut Self {
                            self.#field_name = Some(#field_name);
                            self
                        }
                    });
                    
                    build_fields.push(quote! {
                        #field_name: self.#field_name.take()
                    });
                } else {
                    // For regular fields
                    fields.push(quote! {
                        #field_name: core::option::Option<#field_type>
                    });
                    field_inits.push(quote! {
                        #field_name: core::option::Option::None
                    });
                    
                    setter_methods.push(quote! {
                        pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                            self.#field_name = core::option::Option::Some(#field_name);
                            self
                        }
                    });
                    
                    build_fields.push(quote! {
                        #field_name: self.#field_name.take().ok_or("field not set")?
                    });
                }
            }
        }
    }
    
    let output = quote! {
        pub struct #builder_name {
            #(#fields,)*
        }
        
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_inits,)*
                }
            }
        }
        
        impl #builder_name {
            #(#setter_methods)*
            
            pub fn build(&mut self) -> core::result::Result<#struct_name, &'static str> {
                core::result::Result::Ok(#struct_name {
                    #(#build_fields,)*
                })
            }
        }
    };
    
    TokenStream::from(output)
}

fn get_option_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            if segment.ident == "Option" {
                if let PathArguments::AngleBracketed(angle_bracketed) = &segment.arguments {
                    if let Some(GenericArgument::Type(inner_type)) = angle_bracketed.args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}
