use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use std::collections::HashSet;
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::{
    Attribute, Data, DataStruct, DeriveInput, Expr, ExprLit, Field, Fields, GenericArgument,
    GenericParam, Generics, Ident, Index, Lit, LitStr, Meta, MetaNameValue, PathArguments, Token,
    Type, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait, TypeParamBound, TypeParen, TypePath,
    TypePtr, TypeReference, TypeSlice, TypeTraitObject, TypeTuple, WherePredicate,
    parse_macro_input, parse_quote,
};

/// 生成遵循 #[debug] 特定调整的自定义 Debug 实现
///
/// 这是 CustomDebug 派生宏的入口点，它接收 Rust 源代码的标记流，
/// 解析为 DeriveInput 结构，然后调用 expand_custom_debug 函数生成
/// 自定义的 Debug trait 实现。
///
/// # 参数
/// - `input`: Rust 源代码的标记流，包含带有 #[derive(CustomDebug)] 注解的结构体定义
///
/// # 返回值
/// 生成的 Debug trait 实现的标记流
#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand_custom_debug(input) {
        Ok(stream) => stream.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// 展开 CustomDebug 派生宏，生成 Debug trait 实现
///
/// 该函数是 CustomDebug 派生宏的核心，负责解析输入的结构体定义，
/// 处理泛型边界，并生成自定义的 Debug trait 实现代码。
///
/// # 参数
/// - `input`: 解析后的 DeriveInput 结构，包含结构体的完整信息
///
/// # 返回值
/// 生成的 Debug trait 实现的 TokenStream2，或解析/生成过程中的错误
fn expand_custom_debug(input: DeriveInput) -> syn::Result<TokenStream2> {
    let DeriveInput {
        ident: name,
        generics,
        attrs,
        data,
        ..
    } = input;
    let mut generics = generics;

    let data = match data {
        Data::Struct(data) => data,
        _ => {
            return Err(syn::Error::new_spanned(
                &name,
                "CustomDebug can only be derived for structs",
            ));
        }
    };

    let predicates = match parse_bound_attribute(&attrs)? {
        Some(overridden) => overridden,
        None => infer_bounds(&data, &generics)?,
    };
    if !predicates.is_empty() {
        generics
            .make_where_clause()
            .predicates
            .extend(predicates.into_iter());
    }

    let body = build_debug_body(&name, &data)?;

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                #body
            }
        }
    })
}

/// 解析 #[debug(bound = "...")] 属性，允许调用者完全覆盖推断的边界
///
/// 该函数从结构体的属性中提取自定义的 where 子句边界，
/// 如果存在的话，这些边界将完全替换自动推断的边界。
///
/// # 参数
/// - `attrs`: 结构体的属性列表
///
/// # 返回值
/// 如果存在有效的 #[debug(bound = "...")] 属性，则返回解析后的 where 子句谓词列表；
/// 否则返回 None。如果属性格式错误，返回 syn::Error。
fn parse_bound_attribute(attrs: &[Attribute]) -> syn::Result<Option<Vec<WherePredicate>>> {
    let mut predicates = Vec::new();
    let mut saw_bound = false;

    for attr in attrs {
        if attr.path().is_ident("debug") && matches!(&attr.meta, Meta::List(_)) {
            saw_bound = true;
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("bound") {
                    let lit: LitStr = meta.value()?.parse()?;
                    predicates.extend(parse_where_predicates(&lit)?);
                    Ok(())
                } else {
                    Err(meta.error("unsupported debug attribute"))
                }
            })?;
        }
    }

    Ok(saw_bound.then_some(predicates))
}

/// 解析字符串字面量中的 where 子句谓词列表
///
/// 该函数将用户在 `#[debug(bound = "...")]` 属性中指定的字符串解析为 Rust 的 where 子句谓词
/// 例如，将 `"T: Debug, U: Display"` 解析为包含两个 WherePredicate 的向量
///
/// # 参数
/// - `lit`: 包含 where 子句谓词的字符串字面量
///
/// # 返回值
/// 解析成功时返回包含 WherePredicate 的向量，失败时返回 syn::Error
fn parse_where_predicates(lit: &LitStr) -> syn::Result<Vec<WherePredicate>> {
    lit.parse_with(|input: ParseStream| {
        let predicates =
            Punctuated::<WherePredicate, Token![,]>::parse_terminated(input)?.into_iter();
        Ok(predicates.collect())
    })
}

/// 为结构体的泛型参数自动推断 Debug 边界
///
/// 该函数分析结构体的字段类型，自动为实际使用的泛型参数和关联类型生成 `std::fmt::Debug` 边界
/// 除非泛型参数仅出现在 `PhantomData` 中（此时不需要 Debug 边界）
///
/// # 参数
/// - `data`: 结构体的数据结构信息
/// - `generics`: 结构体的泛型参数信息
///
/// # 返回值
/// 解析成功时返回包含 where 子句谓词的向量，失败时返回 syn::Error
fn infer_bounds(data: &DataStruct, generics: &Generics) -> syn::Result<Vec<WherePredicate>> {
    // 从泛型参数中提取所有类型参数（忽略生命周期和常量参数）
    let type_params = generics
        .params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(ty) => Some(ty.ident.clone()),
            _ => None,
        })
        .collect::<Vec<Ident>>();

    if type_params.is_empty() {
        return Ok(Vec::new());
    }

    // - type_param_set: 存储所有类型参数的集合，用于快速查找
    // - used_params: 存储实际被使用的类型参数
    // - associated: 存储被使用的关联类型
    // - assoc_seen: 确保关联类型只被添加一次（去重）
    let type_param_set: HashSet<Ident> = type_params.iter().cloned().collect();
    let mut used_params = HashSet::new();
    let mut associated = Vec::new();
    let mut assoc_seen = HashSet::new();

    // 遍历结构体的所有字段
    for field in &data.fields {
        // 如果字段类型是 PhantomData 且只包含类型参数，则跳过
        if is_phantom_data(&field.ty, &type_param_set) {
            continue;
        }
        // 收集字段类型中使用的泛型参数和关联类型
        collect_usage(
            &field.ty,
            &type_param_set,
            &mut used_params,
            &mut associated,
            &mut assoc_seen,
        );
    }

    // 生成 where 子句谓词
    let mut predicates = Vec::new();

    // 为所有被使用的类型参数添加 Debug 边界
    for ident in type_params {
        if used_params.contains(&ident) {
            predicates.push(parse_quote!(#ident: std::fmt::Debug));
        }
    }

    // 为所有被使用的关联类型添加 Debug 边界
    for assoc in associated {
        predicates.push(parse_quote!(#assoc: std::fmt::Debug));
    }

    Ok(predicates)
}

/// 递归收集类型中使用的泛型参数和关联类型
///
/// 该函数遍历类型结构，收集其中实际使用的泛型参数和关联类型
/// 用于确定哪些泛型参数需要添加 Debug 边界
///
/// # 参数
/// - `ty`: 要分析的类型
/// - `params`: 所有泛型参数的集合
/// - `used_params`: 输出参数，存储实际被使用的泛型参数
/// - `associated`: 输出参数，存储被使用的关联类型
/// - `assoc_seen`: 输出参数，确保关联类型只被添加一次（去重）
fn collect_usage(
    ty: &Type,
    params: &HashSet<Ident>,
    used_params: &mut HashSet<Ident>,
    associated: &mut Vec<TypePath>,
    assoc_seen: &mut HashSet<String>,
) {
    match ty {
        // 处理复合类型：数组、组、括号、引用、切片、指针
        // 递归处理内部元素类型
        Type::Array(TypeArray { elem, .. })
        | Type::Group(TypeGroup { elem, .. })
        | Type::Paren(TypeParen { elem, .. })
        | Type::Reference(TypeReference { elem, .. })
        | Type::Slice(TypeSlice { elem, .. })
        | Type::Ptr(TypePtr { elem, .. }) => {
            collect_usage(elem, params, used_params, associated, assoc_seen);
        }
        // 处理元组类型：递归处理每个元素类型
        Type::Tuple(TypeTuple { elems, .. }) => {
            for ty in elems {
                collect_usage(ty, params, used_params, associated, assoc_seen);
            }
        }
        // 处理函数类型：递归处理输入和输出类型
        Type::BareFn(TypeBareFn { inputs, output, .. }) => {
            for input in inputs {
                collect_usage(&input.ty, params, used_params, associated, assoc_seen);
            }
            if let syn::ReturnType::Type(_, ty) = output {
                collect_usage(ty, params, used_params, associated, assoc_seen);
            }
        }
        // 处理 impl trait 和 trait object 类型：收集边界中的类型信息
        Type::ImplTrait(TypeImplTrait { bounds, .. })
        | Type::TraitObject(TypeTraitObject { bounds, .. }) => {
            for bound in bounds {
                collect_from_bound(bound, params, used_params, associated, assoc_seen);
            }
        }
        // 处理路径类型：收集类型参数和关联类型
        Type::Path(type_path) => {
            collect_type_path(type_path, params, used_params, associated, assoc_seen)
        }
        // 忽略其他类型（如 never 类型、动态类型等）
        _ => {}
    }
}

/// 从类型路径中收集泛型参数和关联类型
///
/// 该函数分析类型路径，识别其中使用的泛型参数和关联类型
/// - 对于 `Self::AssocType` 形式的关联类型，收集完整路径
/// - 对于直接使用的泛型参数（如 `T`），添加到已使用参数集合
/// - 递归处理路径中的类型参数
///
/// # 参数
/// - `type_path`: 要分析的类型路径
/// - `params`: 所有泛型参数的集合
/// - `used_params`: 输出参数，存储实际被使用的泛型参数
/// - `associated`: 输出参数，存储被使用的关联类型
/// - `assoc_seen`: 输出参数，确保关联类型只被添加一次（去重）
fn collect_type_path(
    type_path: &TypePath,
    params: &HashSet<Ident>,
    used_params: &mut HashSet<Ident>,
    associated: &mut Vec<TypePath>,
    assoc_seen: &mut HashSet<String>,
) {
    // 处理具有 qself（如 <T as Trait>::AssocType）的类型路径
    if let Some(qself) = &type_path.qself {
        // 收集 qself 类型中的参数使用情况
        collect_usage(&qself.ty, params, used_params, associated, assoc_seen);
        // 如果 qself 类型是单个泛型参数，将整个路径作为关联类型收集
        if let Some(base) = single_ident(&qself.ty) {
            if params.contains(&base) {
                push_assoc_type(associated, assoc_seen, type_path);
            }
        }
    }
    // 处理普通类型路径
    else if let Some(first) = type_path.path.segments.first() {
        // 如果路径的第一个段是泛型参数
        if params.contains(&first.ident) {
            // 如果只有一个段（如 `T`），标记为已使用
            if type_path.path.segments.len() == 1 {
                used_params.insert(first.ident.clone());
            }
            // 如果有多个段（如 `T::AssocType`），作为关联类型收集
            else {
                push_assoc_type(associated, assoc_seen, type_path);
            }
        }
    }

    // 递归处理路径中的所有类型参数
    for segment in &type_path.path.segments {
        match &segment.arguments {
            // 处理角括号参数（如 `Vec<T>`）
            PathArguments::AngleBracketed(args) => {
                for arg in &args.args {
                    collect_from_generic_argument(arg, params, used_params, associated, assoc_seen);
                }
            }
            // 处理圆括号参数（如函数指针类型）
            PathArguments::Parenthesized(args) => {
                for input in &args.inputs {
                    collect_usage(input, params, used_params, associated, assoc_seen);
                }
                if let syn::ReturnType::Type(_, ty) = &args.output {
                    collect_usage(ty, params, used_params, associated, assoc_seen);
                }
            }
            // 没有参数，直接跳过
            PathArguments::None => {}
        }
    }
}

/// 从泛型参数中收集类型信息
///
/// 该函数处理各种类型的泛型参数，递归收集其中的类型使用情况
///
/// # 参数
/// - `arg`: 要分析的泛型参数
/// - `params`: 所有泛型参数的集合
/// - `used_params`: 输出参数，存储实际被使用的泛型参数
/// - `associated`: 输出参数，存储被使用的关联类型
/// - `assoc_seen`: 输出参数，确保关联类型只被添加一次（去重）
fn collect_from_generic_argument(
    arg: &GenericArgument,
    params: &HashSet<Ident>,
    used_params: &mut HashSet<Ident>,
    associated: &mut Vec<TypePath>,
    assoc_seen: &mut HashSet<String>,
) {
    match arg {
        // 处理类型参数（如 `T`）
        GenericArgument::Type(ty) => collect_usage(ty, params, used_params, associated, assoc_seen),
        // 处理关联类型参数（如 `Item = T`）
        GenericArgument::AssocType(assoc_type) => {
            // 收集关联类型自身的泛型参数
            if let Some(generics) = &assoc_type.generics {
                for nested in &generics.args {
                    collect_from_generic_argument(
                        nested,
                        params,
                        used_params,
                        associated,
                        assoc_seen,
                    );
                }
            }
            // 收集关联类型定义中的类型使用情况
            collect_usage(&assoc_type.ty, params, used_params, associated, assoc_seen);
        }
        // 处理关联常量参数（如 `N = 5`）
        GenericArgument::AssocConst(assoc_const) => {
            // 收集关联常量自身的泛型参数
            if let Some(generics) = &assoc_const.generics {
                for nested in &generics.args {
                    collect_from_generic_argument(
                        nested,
                        params,
                        used_params,
                        associated,
                        assoc_seen,
                    );
                }
            }
        }
        // 处理约束参数（如 `where T: Trait`）
        GenericArgument::Constraint(constraint) => {
            // 收集约束自身的泛型参数
            if let Some(generics) = &constraint.generics {
                for nested in &generics.args {
                    collect_from_generic_argument(
                        nested,
                        params,
                        used_params,
                        associated,
                        assoc_seen,
                    );
                }
            }
            // 收集约束边界中的类型使用情况
            for bound in &constraint.bounds {
                collect_from_bound(bound, params, used_params, associated, assoc_seen);
            }
        }
        // 忽略生命周期参数和常量参数
        GenericArgument::Lifetime(_) | GenericArgument::Const(_) => {}
        // 忽略其他类型的泛型参数
        _ => {}
    }
}

/// 从类型参数边界中收集类型信息
///
/// 该函数处理类型参数的边界条件（如 `T: Debug + Clone`），提取其中的类型路径
///
/// # 参数
/// - `bound`: 要分析的类型参数边界
/// - `params`: 所有泛型参数的集合
/// - `used_params`: 输出参数，存储实际被使用的泛型参数
/// - `associated`: 输出参数，存储被使用的关联类型
/// - `assoc_seen`: 输出参数，确保关联类型只被添加一次（去重）
fn collect_from_bound(
    bound: &TypeParamBound,
    params: &HashSet<Ident>,
    used_params: &mut HashSet<Ident>,
    associated: &mut Vec<TypePath>,
    assoc_seen: &mut HashSet<String>,
) {
    // 只处理 trait 边界（忽略生命周期边界）
    if let TypeParamBound::Trait(trait_bound) = bound {
        // 将 trait 边界转换为类型路径进行处理
        let type_path = TypePath {
            qself: None,
            path: trait_bound.path.clone(),
        };
        collect_type_path(&type_path, params, used_params, associated, assoc_seen);
    }
}

/// 将关联类型添加到列表中，并确保不重复
///
/// 该函数使用类型路径的字符串表示作为键，避免添加重复的关联类型
///
/// # 参数
/// - `associated`: 存储关联类型的列表
/// - `seen`: 已添加的关联类型的集合（用于去重）
/// - `ty`: 要添加的关联类型
fn push_assoc_type(associated: &mut Vec<TypePath>, seen: &mut HashSet<String>, ty: &TypePath) {
    // 将类型路径转换为字符串作为去重键
    let key = ty.to_token_stream().to_string();
    // 如果是新的关联类型，则添加到列表中
    if seen.insert(key) {
        associated.push(ty.clone());
    }
}

/// 检查类型是否为单个标识符（如 `T`）
///
/// 该函数用于识别直接使用的泛型参数，而不是复杂类型
///
/// # 参数
/// - `ty`: 要检查的类型
///
/// # 返回值
/// 如果类型是单个标识符，则返回该标识符；否则返回 None
fn single_ident(ty: &Type) -> Option<Ident> {
    // 检查是否为路径类型且没有 qself
    if let Type::Path(type_path) = ty {
        // 检查路径是否只有一个段
        if type_path.qself.is_none() && type_path.path.segments.len() == 1 {
            return Some(type_path.path.segments[0].ident.clone());
        }
    }
    // 不是单个标识符
    None
}

/// 检查类型是否为只包含泛型参数的 PhantomData
///
/// 该函数用于识别 `PhantomData<T>` 形式的类型，其中 T 是泛型参数
/// 这样的类型不需要为 T 添加 Debug 边界
///
/// # 参数
/// - `ty`: 要检查的类型
/// - `params`: 所有泛型参数的集合
///
/// # 返回值
/// 如果类型是只包含泛型参数的 PhantomData，则返回 true；否则返回 false
fn is_phantom_data(ty: &Type, params: &HashSet<Ident>) -> bool {
    // 检查类型是否为路径类型且没有 qself
    let type_path = match ty {
        Type::Path(type_path) if type_path.qself.is_none() => type_path,
        _ => return false,
    };

    // 获取路径的最后一个段
    let last = match type_path.path.segments.last() {
        Some(segment) => segment,
        None => return false,
    };

    if last.ident != "PhantomData" {
        return false;
    }

    // 检查 PhantomData 的类型参数是否为单个泛型参数
    if let PathArguments::AngleBracketed(args) = &last.arguments {
        if args.args.len() == 1 {
            if let GenericArgument::Type(Type::Path(inner)) = &args.args[0] {
                if inner.qself.is_none()
                    && inner.path.segments.len() == 1
                    && params.contains(&inner.path.segments[0].ident)
                {
                    return true;
                }
            }
        }
    }

    false
}

/// 构建 Debug trait 实现的主体部分
///
/// 根据结构体的字段类型（命名、未命名、单元结构体）生成不同的 Debug 实现代码。
/// 使用 std::fmt 的辅助函数（如 debug_struct、debug_tuple）来保持生成的代码简洁。
///
/// # 参数
/// - `name`: 结构体的标识符
/// - `data`: 结构体的数据结构信息
///
/// # 返回值
/// 生成的 Debug::fmt 方法体的 TokenStream2，或生成过程中的错误
fn build_debug_body(name: &Ident, data: &DataStruct) -> syn::Result<TokenStream2> {
    // 根据结构体的字段类型（命名、未命名、单元结构体）生成不同的Debug实现
    match &data.fields {
        Fields::Named(named) => {
            let mut fields_tokens = Vec::new();

            for field in &named.named {
                let ident = field.ident.as_ref().unwrap();
                // 生成字段访问表达式（如self.a, self.b）
                let accessor = quote! { self.#ident };
                let value = build_field_value(field, accessor)?;
                fields_tokens.push(quote! {
                    debug.field(stringify!(#ident), #value);
                });
            }

            // 生成完整的Debug实现代码：创建struct调试器，添加所有字段，然后完成
            Ok(quote! {
                let mut debug = f.debug_struct(stringify!(#name));
                #(#fields_tokens)*
                debug.finish()
            })
        }

        // 处理未命名字段结构体（如 struct S(i32, String)）
        Fields::Unnamed(unnamed) => {
            let mut fields_tokens = Vec::new();

            // 遍历所有未命名字段，为每个字段生成Debug字段输出代码
            for (index, field) in unnamed.unnamed.iter().enumerate() {
                // 将索引转换为syn::Index类型（用于生成self.0, self.1等）
                let index = Index::from(index);
                // 生成字段访问表达式（如self.0, self.1）
                let accessor = quote! { self.#index };
                // 生成字段值的格式化表达式
                let value = build_field_value(field, accessor)?;

                fields_tokens.push(quote! {
                    debug.field(#value);
                });
            }
            Ok(quote! {
                let mut debug = f.debug_tuple(stringify!(#name));
                #(#fields_tokens)*
                debug.finish()
            })
        }

        // 处理单元结构体（如 struct S;）
        Fields::Unit => Ok(quote! {
            // 单元结构体只需要输出结构体名称
            f.debug_struct(stringify!(#name)).finish()
        }),
    }
}

/// 为结构体字段生成格式化的值表达式
///
/// 如果字段带有#[debug = "..."]属性，则使用自定义的格式化字符串
/// 否则，使用默认的Debug格式化
///
/// # 参数
/// * `field` - 结构体字段的元数据
/// * `accessor` - 字段的访问表达式（如self.field或self.0）
///
/// # 返回值
/// 生成的格式化值表达式的TokenStream
fn build_field_value(field: &Field, accessor: TokenStream2) -> syn::Result<TokenStream2> {
    // 尝试从字段属性中查找自定义格式化字符串
    if let Some(format_str) = find_format(&field.attrs)? {
        // 如果找到自定义格式化字符串，使用format_args!生成格式化表达式
        Ok(quote! {
            &format_args!(#format_str, &(#accessor))
        })
    } else {
        // 否则，使用默认的Debug格式化
        Ok(quote! {
            &(#accessor)
        })
    }
}

/// 从字段的属性中查找自定义格式化字符串
///
/// 查找形式为#[debug = "..."]的属性，并提取其中的字符串字面量
///
/// # 参数
/// * `attrs` - 字段的属性列表
///
/// # 返回值
/// 如果找到有效的格式化字符串，返回Some(LitStr)；否则返回None
/// 如果存在格式错误或重复的debug属性，返回syn::Error
fn find_format(attrs: &[Attribute]) -> syn::Result<Option<LitStr>> {
    let mut format = None;

    // 遍历所有属性
    for attr in attrs {
        // 只处理路径为"debug"的属性
        if !attr.path().is_ident("debug") {
            continue;
        }

        // 字段级别的debug属性语法仅限于#[debug = "..."]的形式
        if let Meta::NameValue(MetaNameValue { value, .. }) = &attr.meta {
            // 检查是否已经找到过debug属性（避免重复）
            if format.is_some() {
                return Err(syn::Error::new_spanned(attr, "duplicate debug attribute"));
            }

            // 确保属性值是字符串字面量
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(lit), ..
            }) = value
            {
                // 存储找到的格式化字符串
                format = Some(lit.clone());
            } else {
                // 如果不是字符串字面量，返回错误
                return Err(syn::Error::new_spanned(value, "expected string literal"));
            }
        }
    }

    // 返回找到的格式化字符串（如果有）
    Ok(format)
}
