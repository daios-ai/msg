# ggml.ms — MindScript wrapper for libggml.so (ELF + SysV ABI)
# Policy: Aggregates cross the FFI boundary as handles only.
# Opaque C pointers are exposed as tagged VTHandle values.
#
# Coverage:
# - Context / memory / enumeration
# - Tensor meta & introspection
# - Naming / flags
# - Core math (binary/unary), broadcast, rows/indexing, reductions
# - Matmul (incl. precision), out-prods, softmax (fused), RoPE (+multi, +back)
# - Flash attention (ext + back)
# - Convolutions / pooling / resize / pad / roll / windows / rel-pos
# - Graph build/dup/size/traversal/compute/status
# - Custom ops (map*, custom_4d/inplace)
# - Loss/optimizers
# - Quantization (incl. row helpers) + type traits
# - fp16/bf16 scalar + row conversions
# - Logging / abort callbacks
# - Threadpool params helpers
# - Misc: GUID, UTF-8 fopen, unravel_index
#
# NOTE:
# - Enums are bound as 32-bit signed ints.
# - In-place variants return a view handle, matching C API docs.
# - When C returns structs (e.g., ggml_bf16_t), ffiOpen boxes the return into heap storage and returns a handle.

let _C = ffiOpen({
  "version": "1",
  "lib": "./libggml.so",

  "types": {
    # -------- scalars --------
    "bool":   { "kind":"int", "bits":8,  "signed":false },
    "i16":    { "kind":"int", "bits":16, "signed":true },
    "u16":    { "kind":"int", "bits":16, "signed":false },
    "i32":    { "kind":"int", "bits":32, "signed":true },
    "u32":    { "kind":"int", "bits":32, "signed":false },
    "i64":    { "kind":"int", "bits":64, "signed":true },
    "size_t": { "kind":"int", "bits":64, "signed":false },

    "f32": { "kind":"float", "bits":32 },
    "f64": { "kind":"float", "bits":64 },

    # -------- pointers / handles --------
    "charp":  { "kind":"pointer", "to": { "kind":"int", "bits":8, "signed":true } },
    "voidp":  { "kind":"pointer", "to": { "kind":"void" }, "tag":"c.void*" },
    "f32ptr": { "kind":"pointer", "to": { "kind":"float","bits":32 }, "tag":"c.float*" },

    "ggml_context": { "kind":"pointer", "to": { "kind":"void" }, "tag":"ggml.context*" },
    "ggml_tensor":  { "kind":"pointer", "to": { "kind":"void" }, "tag":"ggml.tensor*"  },
    "ggml_cgraph":  { "kind":"pointer", "to": { "kind":"void" }, "tag":"ggml.cgraph*"  },

    # pointer-to-pointer (for ggml_build_backward_expand grad_accs**)
    "ggml_tensor_ptr": { "kind":"pointer", "to":"ggml_tensor", "tag":"ggml.tensor**" },

    # -------- enums as ints --------
    "ggml_type":     { "kind":"int", "bits":32, "signed":true },
    "ggml_op":       { "kind":"int", "bits":32, "signed":true },
    "ggml_prec":     { "kind":"int", "bits":32, "signed":true },
    "ggml_status":   { "kind":"int", "bits":32, "signed":true },
    "ggml_log_level":{ "kind":"int", "bits":32, "signed":true },
    "ggml_op_pool":  { "kind":"int", "bits":32, "signed":true },
    "ggml_sort_order":{ "kind":"int","bits":32, "signed":true },

    # -------- small structs --------
    # struct ggml_init_params { size_t mem_size; void * mem_buffer; bool no_alloc; }
    "ggml_init_params": {
      "kind":"struct",
      "fields":[
        { "name":"mem_size",   "type":"size_t" },
        { "name":"mem_buffer", "type":"voidp"  },
        { "name":"no_alloc",   "type":"bool"   }
      ]
    },

    # struct ggml_bf16_t { uint16_t bits; }
    "ggml_bf16_t": {
      "kind":"struct",
      "fields":[ { "name":"bits", "type":"u16" } ]
    },

    # -------- function pointers --------
    # void (*ggml_abort_callback_t)(const char * msg)
    "ggml_abort_cb": {
      "kind":"funcptr",
      "ret": { "kind":"void" },
      "params": [ { "kind":"pointer", "to": { "kind":"int","bits":8,"signed":true } } ],
      "variadic": false
    },
    # void ggml_log_callback(enum ggml_log_level level, const char * text, void * user_data)
    "ggml_log_cb": {
      "kind":"funcptr",
      "ret": { "kind":"void" },
      "params": [ "ggml_log_level", "charp", "voidp" ],
      "variadic": false
    }
  },

  "functions": [
    # ======= Version / timing =======
    { "name":"ggml_version",   "ret":"charp", "params":[], "ret_as_str":true },
    { "name":"ggml_commit",    "ret":"charp", "params":[], "ret_as_str":true },
    { "name":"ggml_time_init", "ret":{ "kind":"void" }, "params":[] },
    { "name":"ggml_time_ms",   "ret":"i64", "params":[] },
    { "name":"ggml_time_us",   "ret":"i64", "params":[] },
    { "name":"ggml_cycles",         "ret":"i64", "params":[] },
    { "name":"ggml_cycles_per_ms",  "ret":"i64", "params":[] },

    # ======= Context / memory =======
    { "name":"ggml_init",  "ret":"ggml_context", "params":[ "ggml_init_params" ] },
    { "name":"ggml_free",  "ret":{ "kind":"void" }, "params":[ "ggml_context" ] },
    { "name":"ggml_reset", "ret":{ "kind":"void" }, "params":[ "ggml_context" ] },
    { "name":"ggml_used_mem", "ret":"size_t", "params":[ "ggml_context" ] },
    { "name":"ggml_get_no_alloc", "ret":"bool", "params":[ "ggml_context" ] },
    { "name":"ggml_set_no_alloc", "ret":{ "kind":"void" }, "params":[ "ggml_context","bool" ] },
    { "name":"ggml_get_mem_buffer", "ret":"voidp", "params":[ "ggml_context" ] },
    { "name":"ggml_get_mem_size", "ret":"size_t", "params":[ "ggml_context" ] },
    { "name":"ggml_get_max_tensor_size", "ret":"size_t","params":[ "ggml_context" ] },
    { "name":"ggml_new_buffer", "ret":"voidp", "params":[ "ggml_context","size_t" ] },

    # Context enumeration / lookup
    { "name":"ggml_get_first_tensor", "ret":"ggml_tensor", "params":[ "ggml_context" ] },
    { "name":"ggml_get_next_tensor",  "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_get_tensor",       "ret":"ggml_tensor", "params":[ "ggml_context","charp" ] },

    # ======= Tensor meta & introspection =======
    { "name":"ggml_nelements",   "ret":"i64",   "params":[ "ggml_tensor" ] },
    { "name":"ggml_nrows",       "ret":"i64",   "params":[ "ggml_tensor" ] },
    { "name":"ggml_nbytes",      "ret":"size_t","params":[ "ggml_tensor" ] },
    { "name":"ggml_nbytes_pad",  "ret":"size_t","params":[ "ggml_tensor" ] },
    { "name":"ggml_element_size","ret":"size_t","params":[ "ggml_tensor" ] },
    { "name":"ggml_blck_size",   "ret":"i64",   "params":[ "ggml_type" ] },
    { "name":"ggml_row_size",    "ret":"size_t","params":[ "ggml_type","i64" ] },

    { "name":"ggml_type_name",   "ret":"charp", "params":[ "ggml_type" ], "ret_as_str":true },
    { "name":"ggml_op_name",     "ret":"charp", "params":[ "ggml_op" ],   "ret_as_str":true },
    { "name":"ggml_op_symbol",   "ret":"charp", "params":[ "ggml_op" ],   "ret_as_str":true },

    { "name":"ggml_is_transposed", "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_permuted",   "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_empty",      "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_scalar",     "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_vector",     "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_matrix",     "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_3d",         "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_n_dims",        "ret":"i32",  "params":[ "ggml_tensor" ] },

    { "name":"ggml_is_contiguous",   "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_contiguous_0", "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_contiguous_1", "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_contiguous_2", "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_contiguously_allocated","ret":"bool","params":[ "ggml_tensor" ] },
    { "name":"ggml_is_contiguous_channels", "ret":"bool", "params":[ "ggml_tensor" ] },
    { "name":"ggml_is_contiguous_rows",     "ret":"bool", "params":[ "ggml_tensor" ] },

    { "name":"ggml_can_repeat",    "ret":"bool", "params":[ "ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_tensor_overhead", "ret":"size_t", "params":[] },
    { "name":"ggml_validate_row_data","ret":"bool","params":[ "ggml_type","voidp","size_t" ] },

    { "name":"ggml_unravel_index", "ret":{ "kind":"void" },
      "params":[ "ggml_tensor","i64",{ "kind":"pointer","to":"i64" },{ "kind":"pointer","to":"i64" },{ "kind":"pointer","to":"i64" },{ "kind":"pointer","to":"i64" } ] },

    { "name":"ggml_get_unary_op","ret":"i32", "params":[ "ggml_tensor" ] },
    { "name":"ggml_get_glu_op",  "ret":"i32", "params":[ "ggml_tensor" ] },
    { "name":"ggml_op_desc",     "ret":"charp", "params":[ "ggml_tensor" ], "ret_as_str":true },

    # ======= Data access =======
    { "name":"ggml_get_data",     "ret":"voidp",  "params":[ "ggml_tensor" ] },
    { "name":"ggml_get_data_f32", "ret":"f32ptr", "params":[ "ggml_tensor" ] },
    { "name":"ggml_set_zero",     "ret":"ggml_tensor", "params":[ "ggml_tensor" ] },

    # ======= Tensor creation / views =======
    { "name":"ggml_new_tensor",    "ret":"ggml_tensor", "params":[ "ggml_context","ggml_type","i32",{ "kind":"pointer","to":"i64" } ] },
    { "name":"ggml_new_tensor_1d", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_type","i64" ] },
    { "name":"ggml_new_tensor_2d", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_type","i64","i64" ] },
    { "name":"ggml_new_tensor_3d", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_type","i64","i64","i64" ] },
    { "name":"ggml_new_tensor_4d", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_type","i64","i64","i64","i64" ] },
    { "name":"ggml_dup_tensor",    "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_view_tensor",   "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor" ] },

    # ======= Names & flags =======
    { "name":"ggml_get_name",   "ret":"charp", "params":[ "ggml_tensor" ], "ret_as_str":true },
    { "name":"ggml_set_name",   "ret":"ggml_tensor", "params":[ "ggml_tensor","charp" ] },
    { "name":"ggml_format_name","ret":"ggml_tensor", "params":[ "ggml_tensor","charp" ] },
    { "name":"ggml_set_input",  "ret":{ "kind":"void" }, "params":[ "ggml_tensor" ] },
    { "name":"ggml_set_output", "ret":{ "kind":"void" }, "params":[ "ggml_tensor" ] },
    { "name":"ggml_set_param",  "ret":{ "kind":"void" }, "params":[ "ggml_tensor" ] },
    { "name":"ggml_set_loss",   "ret":{ "kind":"void" }, "params":[ "ggml_tensor" ] },

    # ======= Core binary ops =======
    { "name":"ggml_add", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_add_inplace", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_sub", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_sub_inplace", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_mul", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_mul_inplace", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_div", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_div_inplace", "ret":"ggml_tensor", "params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },

    # ======= Unary & activation =======
    { "name":"ggml_sqr",  "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sqr_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sqrt", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sqrt_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_log",  "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_log_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sin",  "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sin_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_cos",  "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_cos_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_exp",  "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_exp_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },

    { "name":"ggml_abs", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_abs_inplace", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sgn", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sgn_inplace", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_neg", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_neg_inplace", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_step", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_step_inplace", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },

    { "name":"ggml_tanh", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_tanh_inplace", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_elu", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_elu_inplace", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_relu", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_relu_inplace", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_leaky_relu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32","bool" ] },
    { "name":"ggml_sigmoid","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sigmoid_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_gelu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_gelu_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_gelu_erf","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_gelu_erf_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_gelu_quick","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_gelu_quick_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_silu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_silu_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_silu_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_hardswish","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_hardsigmoid","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_xielu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32","f32","f32","f32" ] },

    # ======= Reductions / rows / broadcast =======
    { "name":"ggml_sum", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_sum_rows","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_mean","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_argmax","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_count_equal","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },

    { "name":"ggml_repeat","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_repeat_4d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64","i64" ] },
    { "name":"ggml_repeat_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_concat","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32" ] },

    { "name":"ggml_add1","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_add1_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_add_id","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_acc","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t","size_t","size_t","size_t" ] },
    { "name":"ggml_acc_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t","size_t","size_t","size_t" ] },

    # ======= Rows / indexing =======
    { "name":"ggml_get_rows", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_get_rows_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_set_rows","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },

    { "name":"ggml_diag", "ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_diag_mask_inf","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },
    { "name":"ggml_diag_mask_inf_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },
    { "name":"ggml_diag_mask_zero","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },
    { "name":"ggml_diag_mask_zero_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },

    # ======= Softmax & fused softmax =======
    { "name":"ggml_soft_max","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_soft_max_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },

    { "name":"ggml_soft_max_ext","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","f32","f32" ] },
    { "name":"ggml_soft_max_ext_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","f32","f32" ] },
    { "name":"ggml_soft_max_add_sinks","ret":{ "kind":"void" },"params":[ "ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_soft_max_ext_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","f32","f32" ] },
    { "name":"ggml_soft_max_ext_back_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","f32","f32" ] },

    # ======= Norms =======
    { "name":"ggml_norm","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_norm_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_rms_norm","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_rms_norm_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_group_norm","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","f32" ] },
    { "name":"ggml_group_norm_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","f32" ] },
    { "name":"ggml_l2_norm","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_l2_norm_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_rms_norm_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","f32" ] },

    # ======= Matmul / products =======
    { "name":"ggml_mul_mat","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_mul_mat_set_prec","ret":{ "kind":"void" },"params":[ "ggml_tensor","ggml_prec" ] },
    { "name":"ggml_mul_mat_id","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_out_prod","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },

    # ======= Movement / reshape / cast / set =======
    { "name":"ggml_scale","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_scale_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32" ] },
    { "name":"ggml_scale_bias","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32","f32" ] },
    { "name":"ggml_scale_bias_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32","f32" ] },

    { "name":"ggml_set","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t","size_t","size_t","size_t" ] },
    { "name":"ggml_set_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t","size_t","size_t","size_t" ] },
    { "name":"ggml_set_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t" ] },
    { "name":"ggml_set_1d_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t" ] },
    { "name":"ggml_set_2d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t","size_t" ] },
    { "name":"ggml_set_2d_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","size_t","size_t" ] },

    { "name":"ggml_cpy","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_cast","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_type" ] },
    { "name":"ggml_add_cast","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_type" ] },

    { "name":"ggml_cont","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_cont_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64" ] },
    { "name":"ggml_cont_2d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64" ] },
    { "name":"ggml_cont_3d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64" ] },
    { "name":"ggml_cont_4d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64","i64" ] },

    { "name":"ggml_reshape","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_reshape_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64" ] },
    { "name":"ggml_reshape_2d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64" ] },
    { "name":"ggml_reshape_3d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64" ] },
    { "name":"ggml_reshape_4d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64","i64" ] },

    { "name":"ggml_view_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","size_t" ] },
    { "name":"ggml_view_2d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","size_t","size_t" ] },
    { "name":"ggml_view_3d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64","size_t","size_t","size_t" ] },
    { "name":"ggml_view_4d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64","i64","size_t","size_t","size_t","size_t" ] },

    { "name":"ggml_permute","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32","i32","i32" ] },
    { "name":"ggml_transpose","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },

    # ======= Clamp / arange / argsort / topk =======
    { "name":"ggml_clamp","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","f32","f32" ] },
    { "name":"ggml_arange","ret":"ggml_tensor","params":[ "ggml_context","f32","f32","f32" ] },
    { "name":"ggml_argsort","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_sort_order" ] },
    { "name":"ggml_top_k","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },

    # ======= Trunc / floor / ceil / round =======
    { "name":"ggml_trunc","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_trunc_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_floor","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_floor_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_ceil","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_ceil_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_round","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_round_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },

    # ======= GLU family =======
    { "name":"ggml_glu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","bool" ] },
    { "name":"ggml_reglu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_reglu_swapped","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_geglu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_geglu_swapped","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_swiglu","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_swiglu_swapped","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_geglu_erf","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_geglu_erf_swapped","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_geglu_quick","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_geglu_quick_swapped","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor" ] },
    { "name":"ggml_glu_split","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32" ] },
    { "name":"ggml_reglu_split","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_geglu_split","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_swiglu_split","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_geglu_erf_split","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_geglu_quick_split","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_swiglu_oai","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","f32","f32" ] },

    # ======= RoPE =======
    { "name":"ggml_rope","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32" ] },
    { "name":"ggml_rope_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32" ] },
    { "name":"ggml_rope_ext","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","i32","i32","i32","f32","f32","f32","f32","f32","f32" ] },
    { "name":"ggml_rope_multi","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","i32",{ "kind":"pointer","to":"i32" },"i32","i32","f32","f32","f32","f32","f32","f32" ] },
    { "name":"ggml_rope_ext_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","i32","i32","i32","f32","f32","f32","f32","f32","f32" ] },
    { "name":"ggml_rope_multi_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","i32",{ "kind":"pointer","to":"i32" },"i32","i32","f32","f32","f32","f32","f32","f32" ] },
    { "name":"ggml_rope_yarn_corr_dims","ret":{ "kind":"void" }, "params":[ "i32","i32","f32","f32","f32",{ "kind":"pointer","to":"f32" } ] },
    { "name":"ggml_rope_ext_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","i32","i32","i32","f32","f32","f32","f32","f32","f32" ] },
    { "name":"ggml_rope_multi_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","i32",{ "kind":"pointer","to":"i32" },"i32","i32","f32","f32","f32","f32","f32","f32" ] },

    # ======= Flash attention =======
    { "name":"ggml_flash_attn_ext","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","f32","f32","f32" ] },
    { "name":"ggml_flash_attn_ext_set_prec","ret":{ "kind":"void" }, "params":[ "ggml_tensor","ggml_prec" ] },
    { "name":"ggml_flash_attn_ext_get_prec","ret":"ggml_prec", "params":[ "ggml_tensor" ] },
    { "name":"ggml_flash_attn_ext_add_sinks","ret":{ "kind":"void" }, "params":[ "ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_flash_attn_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","bool" ] },

    # ======= SSM / RWKV / Gated linear attn =======
    { "name":"ggml_ssm_conv","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_ssm_scan","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_rwkv_wkv6","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_gated_linear_attn","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","f32" ] },
    { "name":"ggml_rwkv_wkv7","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor" ] },

    # ======= Convolution / pooling / resize / pad / roll =======
    { "name":"ggml_im2col","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32","i32","i32","i32","bool","ggml_type" ] },
    { "name":"ggml_im2col_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor",{ "kind":"pointer","to":"i64" },"i32","i32","i32","i32","i32","i32","bool" ] },

    { "name":"ggml_conv_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32" ] },
    { "name":"ggml_conv_1d_ph","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32" ] },
    { "name":"ggml_conv_1d_dw","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32" ] },
    { "name":"ggml_conv_1d_dw_ph","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32" ] },
    { "name":"ggml_conv_transpose_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32" ] },

    { "name":"ggml_conv_2d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32","i32","i32","i32" ] },
    { "name":"ggml_conv_2d_dw","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32","i32","i32","i32" ] },
    { "name":"ggml_conv_2d_dw_direct","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32","i32","i32","i32" ] },
    { "name":"ggml_conv_2d_direct","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32","i32","i32","i32" ] },
    { "name":"ggml_conv_transpose_2d_p0","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32" ] },
    { "name":"ggml_conv_2d_sk_p0","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_conv_2d_s1_ph","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },

    { "name":"ggml_im2col_3d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i64","i32","i32","i32","i32","i32","i32","i32","i32","ggml_type" ] },
    { "name":"ggml_conv_3d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i64","i32","i32","i32","i32","i32","i32","i32","i32" ] },
    { "name":"ggml_conv_3d_direct","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32","i32","i32","i32","i32","i32","i32","i32","i32" ] },

    { "name":"ggml_pool_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32","i32","i32" ] }, # op as i32 (enum)
    { "name":"ggml_pool_2d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32","i32","i32","i32","f32","f32" ] },
    { "name":"ggml_pool_2d_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","i32","i32","i32","i32","i32","f32","f32" ] },

    { "name":"ggml_upscale","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32" ] }, # mode as i32
    { "name":"ggml_interpolate","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i64","i64","i64","i64","u32" ] },

    { "name":"ggml_pad","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32","i32","i32" ] },
    { "name":"ggml_pad_ext","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32","i32","i32","i32","i32","i32","i32" ] },
    { "name":"ggml_pad_reflect_1d","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32" ] },

    { "name":"ggml_roll","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32","i32","i32" ] },

    { "name":"ggml_timestep_embedding","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32" ] },

    # Windows / relative position
    { "name":"ggml_win_part","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },
    { "name":"ggml_win_unpart","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32","i32" ] },
    { "name":"ggml_get_rel_pos","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32","i32" ] },
    { "name":"ggml_add_rel_pos","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_add_rel_pos_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },

    # ======= Generic unary dispatcher =======
    { "name":"ggml_unary","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },
    { "name":"ggml_unary_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","i32" ] },

    # ======= Graph API =======
    { "name":"ggml_new_graph",            "ret":"ggml_cgraph", "params":[ "ggml_context" ] },
    { "name":"ggml_new_graph_custom",     "ret":"ggml_cgraph", "params":[ "ggml_context","size_t","bool" ] },
    { "name":"ggml_build_forward_expand", "ret":{ "kind":"void" }, "params":[ "ggml_cgraph","ggml_tensor" ] },
    { "name":"ggml_build_backward_expand","ret":{ "kind":"void" }, "params":[ "ggml_context","ggml_cgraph","ggml_tensor_ptr" ] },

    { "name":"ggml_graph_dup",            "ret":"ggml_cgraph", "params":[ "ggml_context","ggml_cgraph","bool" ] },
    { "name":"ggml_graph_cpy",            "ret":{ "kind":"void" }, "params":[ "ggml_cgraph","ggml_cgraph" ] },
    { "name":"ggml_graph_reset",          "ret":{ "kind":"void" }, "params":[ "ggml_cgraph" ] },
    { "name":"ggml_graph_clear",          "ret":{ "kind":"void" }, "params":[ "ggml_cgraph" ] },
    { "name":"ggml_graph_size",           "ret":"i32", "params":[ "ggml_cgraph" ] },
    { "name":"ggml_graph_node",           "ret":"ggml_tensor", "params":[ "ggml_cgraph","i32" ] },
    { "name":"ggml_graph_nodes",          "ret":{ "kind":"pointer","to":"ggml_tensor" }, "params":[ "ggml_cgraph" ] },
    { "name":"ggml_graph_add_node",       "ret":{ "kind":"void" }, "params":[ "ggml_cgraph","ggml_tensor" ] },
    { "name":"ggml_graph_overhead",       "ret":"size_t", "params":[] },
    { "name":"ggml_graph_overhead_custom","ret":"size_t", "params":[ "size_t","bool" ] },

    { "name":"ggml_graph_get_tensor",     "ret":"ggml_tensor", "params":[ { "kind":"pointer","to":"ggml_cgraph" },"charp" ] },
    { "name":"ggml_graph_get_grad",       "ret":"ggml_tensor", "params":[ { "kind":"pointer","to":"ggml_cgraph" },"ggml_tensor" ] },
    { "name":"ggml_graph_get_grad_acc",   "ret":"ggml_tensor", "params":[ { "kind":"pointer","to":"ggml_cgraph" },"ggml_tensor" ] },

    { "name":"ggml_graph_print",          "ret":{ "kind":"void" }, "params":[ "ggml_cgraph" ] },
    { "name":"ggml_graph_dump_dot",       "ret":{ "kind":"void" }, "params":[ "ggml_cgraph","ggml_cgraph","charp" ] },

    # Compute (returns ggml_status)
    { "name":"ggml_graph_compute_with_ctx","ret":"ggml_status", "params":[ "ggml_context","ggml_cgraph","i32" ] },
    { "name":"ggml_status_to_string",     "ret":"charp", "params":[ "ggml_status" ], "ret_as_str":true },

    # Element get/set (f32)
    { "name":"ggml_set_f32_1d", "ret":{ "kind":"void" }, "params":[ "ggml_tensor","i32","f32" ] },
    { "name":"ggml_get_f32_1d", "ret":"f32", "params":[ "ggml_tensor","i32" ] },

    # ======= Custom ops =======
    { "name":"ggml_map_custom1","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },
    { "name":"ggml_map_custom1_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },
    { "name":"ggml_map_custom2","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },
    { "name":"ggml_map_custom2_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },
    { "name":"ggml_map_custom3","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },
    { "name":"ggml_map_custom3_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },

    { "name":"ggml_custom_4d","ret":"ggml_tensor","params":[ "ggml_context","ggml_type","i64","i64","i64","i64",{ "kind":"pointer","to":"ggml_tensor" },"i32",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },
    { "name":"ggml_custom_inplace","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor",{ "kind":"pointer","to":"ggml_tensor" },"i32",{ "kind":"pointer","to":{ "kind":"void" } },"i32","voidp" ] },

    # ======= Loss / Optimizers =======
    { "name":"ggml_cross_entropy_loss","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_cross_entropy_loss_back","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_opt_step_adamw","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor","ggml_tensor" ] },
    { "name":"ggml_opt_step_sgd","ret":"ggml_tensor","params":[ "ggml_context","ggml_tensor","ggml_tensor","ggml_tensor" ] },

    # ======= Quantization =======
    { "name":"ggml_ftype_to_ggml_type",        "ret":"ggml_type", "params":[ "i32" ] },
    { "name":"ggml_quantize_init",              "ret":{ "kind":"void" }, "params":[ "ggml_type" ] },
    { "name":"ggml_quantize_free",              "ret":{ "kind":"void" }, "params":[] },
    { "name":"ggml_quantize_requires_imatrix",  "ret":"bool", "params":[ "ggml_type" ] },
    { "name":"ggml_quantize_chunk", "ret":"size_t",
      "params":[
        "ggml_type",
        { "kind":"pointer","to":"f32" },
        "voidp",
        "i64","i64","i64",
        { "kind":"pointer","to":"f32" }
      ]
    },

    # ======= fp16 / bf16 conversions (scalar + row) =======
    { "name":"ggml_fp16_to_fp32", "ret":"f32", "params":[ "u16" ] },
    { "name":"ggml_fp32_to_fp16", "ret":"u16", "params":[ "f32" ] },
    { "name":"ggml_fp16_to_fp32_row", "ret":{ "kind":"void" }, "params":[ { "kind":"pointer","to":"u16" }, { "kind":"pointer","to":"f32" }, "i64" ] },
    { "name":"ggml_fp32_to_fp16_row", "ret":{ "kind":"void" }, "params":[ { "kind":"pointer","to":"f32" }, { "kind":"pointer","to":"u16" }, "i64" ] },

    { "name":"ggml_bf16_to_fp32","ret":"f32","params":[ "ggml_bf16_t" ] },
    { "name":"ggml_fp32_to_bf16","ret":"ggml_bf16_t","params":[ "f32" ] },
    { "name":"ggml_bf16_to_fp32_row","ret":{ "kind":"void" }, "params":[ { "kind":"pointer","to":"ggml_bf16_t" }, { "kind":"pointer","to":"f32" }, "i64" ] },
    { "name":"ggml_fp32_to_bf16_row_ref","ret":{ "kind":"void" }, "params":[ { "kind":"pointer","to":"f32" }, { "kind":"pointer","to":"ggml_bf16_t" }, "i64" ] },
    { "name":"ggml_fp32_to_bf16_row","ret":{ "kind":"void" }, "params":[ { "kind":"pointer","to":"f32" }, { "kind":"pointer","to":"ggml_bf16_t" }, "i64" ] },

    # ======= Type traits =======
    { "name":"ggml_get_type_traits","ret":{ "kind":"pointer","to":{ "kind":"void" } }, "params":[ "ggml_type" ] },

    # ======= Logging / abort =======
    { "name":"ggml_log_set",           "ret":{ "kind":"void" }, "params":[ "ggml_log_cb", "voidp" ] },
    { "name":"ggml_set_abort_callback","ret":"ggml_abort_cb", "params":[ "ggml_abort_cb" ] },

    # ======= Threadpool params =======
    { "name":"ggml_threadpool_params_default","ret":{ "kind":"struct","fields":[
        { "name":"cpumask0","type":"bool" },  # Placeholder: we only expose helper entrypoints (opaque use)
      ] }, "params":[ "i32" ] }, # We don't use the returned struct directly; see note below.
    { "name":"ggml_threadpool_params_init","ret":{ "kind":"void" }, "params":[ { "kind":"pointer","to":{ "kind":"void" } },"i32" ] },
    { "name":"ggml_threadpool_params_match","ret":"bool", "params":[ { "kind":"pointer","to":{ "kind":"void" } }, { "kind":"pointer","to":{ "kind":"void" } } ] },

    # ======= Misc =======
    { "name":"ggml_guid_matches","ret":"bool","params":[ { "kind":"pointer","to":{ "kind":"int","bits":8,"signed":false } }, { "kind":"pointer","to":{ "kind":"int","bits":8,"signed":false } } ] },
    { "name":"ggml_fopen","ret":{ "kind":"pointer","to":{ "kind":"void" }, "tag":"c.FILE*" }, "params":[ "charp","charp" ] }
  ]
})

# -------------------------
# Public metadata & enums
# -------------------------
let version = "ggml@wrap-1.0.0"
let close = _C.close

# ggml_type (subset)
let TYPE_F32 = 0
let TYPE_F16 = 1
let TYPE_Q4_0 = 2
let TYPE_Q4_1 = 3
let TYPE_Q5_0 = 6
let TYPE_Q5_1 = 7
let TYPE_Q8_0 = 8
let TYPE_Q8_1 = 9
let TYPE_I8  = 24
let TYPE_I16 = 25
let TYPE_I32 = 26
let TYPE_I64 = 27
let TYPE_F64 = 28
let TYPE_BF16 = 30
let TYPE_TQ1_0 = 34
let TYPE_TQ2_0 = 35
let TYPE_MXFP4 = 39

# ggml_prec
let PREC_DEFAULT = 0
let PREC_F32     = 10

# ggml_status
let STATUS_ALLOC_FAILED = -2
let STATUS_FAILED       = -1
let STATUS_SUCCESS      = 0
let STATUS_ABORTED      = 1

# ggml_sort_order
let SORT_ASC  = 0
let SORT_DESC = 1

# ggml_op_pool
let POOL_MAX = 0
let POOL_AVG = 1

# -------------------------
# Raw aliases (few highlights; most are kept under _C.*)
# -------------------------
let versionStr = _C.ggml_version
let commitStr  = _C.ggml_commit

let timeInit   = _C.ggml_time_init
let timeMs     = _C.ggml_time_ms
let timeUs     = _C.ggml_time_us
let cycles     = _C.ggml_cycles
let cyclesPerMs= _C.ggml_cycles_per_ms

let statusStr  = _C.ggml_status_to_string

# Core ops (examples)
let add = _C.ggml_add
let mul = _C.ggml_mul
let div = _C.ggml_div
let softMax = _C.ggml_soft_max
let mulMat = _C.ggml_mul_mat
let setMatmulPrec = _C.ggml_mul_mat_set_prec

# Graph
let newGraph       = _C.ggml_new_graph
let newGraphCustom = _C.ggml_new_graph_custom
let buildForward   = _C.ggml_build_forward_expand
let buildBackward  = _C.ggml_build_backward_expand
let graphPrint     = _C.ggml_graph_print
let graphDumpDot   = _C.ggml_graph_dump_dot
let computeRaw     = _C.ggml_graph_compute_with_ctx

# Data access
let getData     = _C.ggml_get_data
let getDataF32  = _C.ggml_get_data_f32
let setZero     = _C.ggml_set_zero
let setF32_1d   = _C.ggml_set_f32_1d
let getF32_1d   = _C.ggml_get_f32_1d

# Quantization
let qInit         = _C.ggml_quantize_init
let qFree         = _C.ggml_quantize_free
let qNeedsImatrix = _C.ggml_quantize_requires_imatrix
let qChunk        = _C.ggml_quantize_chunk

# fp16/bf16
let fp16ToF32 = _C.ggml_fp16_to_fp32
let f32ToFp16 = _C.ggml_fp32_to_fp16
let fp16RowToF32 = _C.ggml_fp16_to_fp32_row
let f32RowToFp16 = _C.ggml_fp32_to_fp16_row

let bf16ToF32 = _C.ggml_bf16_to_fp32
let f32ToBf16 = _C.ggml_fp32_to_bf16
let bf16RowToF32 = _C.ggml_bf16_to_fp32_row
let f32RowToBf16Ref = _C.ggml_fp32_to_bf16_row_ref
let f32RowToBf16    = _C.ggml_fp32_to_bf16_row

# Logging / abort
let logSet       = _C.ggml_log_set
let setAbortCB   = _C.ggml_set_abort_callback

# -------------------------
# Helpers (handles only; no defaults)
# -------------------------

# Create a ggml context and auto-free it with ggml_free on GC.
let createContext = fun(memBytes: Int, noAlloc: Bool, memBuffer: Any?) -> Any? do
  let params = _C.__mem.box("ggml_init_params", {
    mem_size: memBytes,
    mem_buffer: memBuffer,   # null or a valid void* handle you manage
    no_alloc: noAlloc
  })
  let ctx = _C.ggml_init(params)
  if ctx == null then return null end
  _C.__mem.gc(ctx, { sym: "ggml_free" })
  ctx
end

# Convenience F32 tensor creators (no implicit init)
let f32_1d = fun(ctx: Any, ne0: Int) -> Any? do _C.ggml_new_tensor_1d(ctx, TYPE_F32, ne0) end
let f32_2d = fun(ctx: Any, ne0: Int, ne1: Int) -> Any? do _C.ggml_new_tensor_2d(ctx, TYPE_F32, ne0, ne1) end
let f32_3d = fun(ctx: Any, ne0: Int, ne1: Int, ne2: Int) -> Any? do _C.ggml_new_tensor_3d(ctx, TYPE_F32, ne0, ne1, ne2) end
let f32_4d = fun(ctx: Any, ne0: Int, ne1: Int, ne2: Int, ne3: Int) -> Any? do _C.ggml_new_tensor_4d(ctx, TYPE_F32, ne0, ne1, ne2, ne3) end

# Set a tensor's name (returns the same tensor handle)
let nameTensor = fun(t: Any, s: Str) -> Any do _C.ggml_set_name(t, s) end

# Build a forward graph from one or more roots.
let buildForwardGraph = fun(ctx: Any, roots: [Any]) -> Any? do
  let g = _C.ggml_new_graph(ctx)
  if g == null then return null end
  for t in roots do _C.ggml_build_forward_expand(g, t) end
  g
end

# Dump a graph to a DOT file (filename: Str).
# C signature is (gb, gf, filename). Pass null for gb if unused.
let dumpGraphDotFile = fun(gf: Any, gb: Any?, filename: Str) -> Void do
  _C.ggml_graph_dump_dot(gb, gf, filename)
end

# Compute wrapper: returns struct-like record with status info.
let compute = fun(ctx: Any, g: Any, nThreads: Int) -> { status: Int, ok: Bool, text: Str } do
  let st = _C.ggml_graph_compute_with_ctx(ctx, g, nThreads)
  { status: st, ok: (st == STATUS_SUCCESS), text: _C.ggml_status_to_string(st) }
end

# Matmul precision helper (PREC_F32 is safer for some models)
let setMatmulPrecision = fun(node: Any, prec: Int) -> Void do
  _C.ggml_mul_mat_set_prec(node, prec)
end

# BF16 bits accessor from boxed struct
let bf16Bits = fun(bf16Box: Any) -> Int do
  _C.__mem.getf("ggml_bf16_t", bf16Box, "bits")
end

# 1D f32 element IO (native)
let writeF32_1d = fun(t: Any, i: Int, value: Num) -> Void do _C.ggml_set_f32_1d(t, i, value) end
let readF32_1d  = fun(t: Any, i: Int) -> Num do _C.ggml_get_f32_1d(t, i) end
