# ggml.ms — public wrapper module for GGML (https://github.com/ggml-org/ggml)
#
# Wraps a useful, curated subset of the GGML C API using MindScript `ffiOpen`.
# Policy: **aggregates = handles only**. No inline struct/array literals cross FFI.
#
# --- 1) Load raw FFI privately -------------------------------------------------

let _C = ffiOpen({
  version: "1",
  lib: "libggml.so",
  types: {
    # scalars
    i32:   { kind:"int",   bits:32, signed:true  },
    u32:   { kind:"int",   bits:32, signed:false },
    i64:   { kind:"int",   bits:64, signed:true  },
    u64:   { kind:"int",   bits:64, signed:false },
    size_t:{ kind:"int",   bits:64, signed:false },  # SysV/ELF (LP64)
    bool:  { kind:"int",   bits:8,  signed:false },   # _Bool
    f32:   { kind:"float", bits:32 },
    f64:   { kind:"float", bits:64 },

    # common pointers / handles
    voidp: { kind:"pointer", to:{kind:"void"}, tag:"ggml.ptr.void" },
    charp: { kind:"pointer", to:{kind:"int", bits:8, signed:true}, tag:"c.str*" },

    Ctx:   { kind:"pointer", to:{kind:"void"}, tag:"ggml.context*" },
    Tensor:{ kind:"pointer", to:{kind:"void"}, tag:"ggml.tensor*" },
    CGraph:{ kind:"pointer", to:{kind:"void"}, tag:"ggml.cgraph*" },
    FILEp: { kind:"pointer", to:{kind:"void"}, tag:"FILE*" },

    # enums (as i32)
    ggml_type: { kind:"int", bits:32, signed:true },
    ggml_log_level: { kind:"int", bits:32, signed:true },

    # struct ggml_init_params (by-value aggregate param to ggml_init)
    ggml_init_params: { kind:"struct", fields:[
      { name:"mem_size",   type:"size_t" },
      { name:"mem_buffer", type:"voidp"   },
      { name:"no_alloc",   type:"bool"    }
    ]},

    # helpers for arrays
    i64p: { kind:"pointer", to:"i64", tag:"i64*" },
    Tensorp: { kind:"pointer", to:"Tensor", tag:"ggml.tensor**" }
  },
  functions: [
    # misc / version
    { name:"ggml_version", ret:"charp", params:[], ret_as_str:true },
    { name:"ggml_commit",  ret:"charp", params:[], ret_as_str:true },

    # time utils
    { name:"ggml_time_init",    ret:{kind:"void"}, params:[] },
    { name:"ggml_time_ms",      ret:"i64", params:[] },
    { name:"ggml_time_us",      ret:"i64", params:[] },
    { name:"ggml_cycles",       ret:"i64", params:[] },
    { name:"ggml_cycles_per_ms",ret:"i64", params:[] },

    # file helper
    { name:"ggml_fopen", ret:"FILEp", params:["charp","charp"] },

    # type / size helpers
    { name:"ggml_type_size", ret:"size_t", params:["ggml_type"] },
    { name:"ggml_row_size",  ret:"size_t", params:["ggml_type","i64"] },
    { name:"ggml_type_name",  ret:"charp", params:["ggml_type"], ret_as_str:true },

    # context lifecycle
    { name:"ggml_init",  ret:"Ctx", params:[ "ggml_init_params" ] },   # NOTE: by-value struct → pass handle
    { name:"ggml_reset", ret:{kind:"void"}, params:["Ctx"] },
    { name:"ggml_free",  ret:{kind:"void"}, params:["Ctx"] },

    { name:"ggml_used_mem",        ret:"size_t", params:["Ctx"] },
    { name:"ggml_get_no_alloc",    ret:"bool", params:["Ctx"] },
    { name:"ggml_set_no_alloc",    ret:{kind:"void"}, params:["Ctx","bool"] },
    { name:"ggml_get_mem_buffer",  ret:"voidp", params:["Ctx"] },
    { name:"ggml_get_mem_size",    ret:"size_t", params:["Ctx"] },
    { name:"ggml_get_max_tensor_size", ret:"size_t", params:["Ctx"] },

    # tensor creation
    { name:"ggml_new_tensor",   ret:"Tensor", params:["Ctx","ggml_type","i32","i64p"] },
    { name:"ggml_new_tensor_1d",ret:"Tensor", params:["Ctx","ggml_type","i64"] },
    { name:"ggml_new_tensor_2d",ret:"Tensor", params:["Ctx","ggml_type","i64","i64"] },
    { name:"ggml_new_tensor_3d",ret:"Tensor", params:["Ctx","ggml_type","i64","i64","i64"] },
    { name:"ggml_new_tensor_4d",ret:"Tensor", params:["Ctx","ggml_type","i64","i64","i64","i64"] },

    { name:"ggml_dup_tensor",   ret:"Tensor", params:["Ctx","Tensor"] },
    { name:"ggml_view_tensor",  ret:"Tensor", params:["Ctx","Tensor"] },

    # tensor queries
    { name:"ggml_nelements", ret:"i64", params:["Tensor"] },
    { name:"ggml_nrows",     ret:"i64", params:["Tensor"] },
    { name:"ggml_nbytes",    ret:"size_t", params:["Tensor"] },
    { name:"ggml_nbytes_pad",ret:"size_t", params:["Tensor"] },
    { name:"ggml_element_size", ret:"size_t", params:["Tensor"] },

    { name:"ggml_get_data",      ret:"voidp", params:["Tensor"] },
    { name:"ggml_get_data_f32",  ret:{ kind:"pointer", to:"f32", tag:"float*" }, params:["Tensor"] },
    { name:"ggml_get_name",      ret:"charp", params:["Tensor"], ret_as_str:true },
    { name:"ggml_set_name",      ret:"Tensor", params:["Tensor","charp"] },

    # tensor flags
    { name:"ggml_set_input",  ret:{kind:"void"}, params:["Tensor"] },
    { name:"ggml_set_output", ret:{kind:"void"}, params:["Tensor"] },
    { name:"ggml_set_param",  ret:{kind:"void"}, params:["Tensor"] },
    { name:"ggml_set_loss",   ret:{kind:"void"}, params:["Tensor"] },

    # basic operations (representative set)
    { name:"ggml_add",   ret:"Tensor", params:["Ctx","Tensor","Tensor"] },
    { name:"ggml_mul",   ret:"Tensor", params:["Ctx","Tensor","Tensor"] },
    { name:"ggml_div",   ret:"Tensor", params:["Ctx","Tensor","Tensor"] },
    { name:"ggml_sum",   ret:"Tensor", params:["Ctx","Tensor"] },
    { name:"ggml_mul_mat", ret:"Tensor", params:["Ctx","Tensor","Tensor"] },
    { name:"ggml_relu",  ret:"Tensor", params:["Ctx","Tensor"] },
    { name:"ggml_gelu",  ret:"Tensor", params:["Ctx","Tensor"] },
    { name:"ggml_soft_max", ret:"Tensor", params:["Ctx","Tensor"] },
    { name:"ggml_norm",  ret:"Tensor", params:["Ctx","Tensor","f32"] },

    # view/shape utilities
    { name:"ggml_transpose", ret:"Tensor", params:["Ctx","Tensor"] },
    { name:"ggml_cont",      ret:"Tensor", params:["Ctx","Tensor"] },
    { name:"ggml_reshape_2d",ret:"Tensor", params:["Ctx","Tensor","i64","i64"] },

    # data movement / cast
    { name:"ggml_cpy",   ret:"Tensor", params:["Ctx","Tensor","Tensor"] },
    { name:"ggml_cast",  ret:"Tensor", params:["Ctx","Tensor","ggml_type"] },

    # graph
    { name:"ggml_new_graph", ret:"CGraph", params:["Ctx"] },
    { name:"ggml_new_graph_custom", ret:"CGraph", params:["Ctx","size_t","bool"] },
    { name:"ggml_build_forward_expand", ret:{kind:"void"}, params:["CGraph","Tensor"] },
    { name:"ggml_graph_size",  ret:"i32", params:["CGraph"] },
    { name:"ggml_graph_node",  ret:"Tensor", params:["CGraph","i32"] },
    { name:"ggml_graph_print", ret:{kind:"void"}, params:["CGraph"] }
  ]
})

# --- 2) Public surface ---------------------------------------------------------

let version = "ggml@wrap-0.1.0"
let close   = _C.close

# GGML types (enum ggml_type)
let TYPE_F32      = 0
let TYPE_F16      = 1
let TYPE_Q4_0     = 2
let TYPE_Q4_1     = 3
let TYPE_Q5_0     = 6
let TYPE_Q5_1     = 7
let TYPE_Q8_0     = 8
let TYPE_Q8_1     = 9
let TYPE_Q2_K     = 10
let TYPE_Q3_K     = 11
let TYPE_Q4_K     = 12
let TYPE_Q5_K     = 13
let TYPE_Q6_K     = 14
let TYPE_Q8_K     = 15
let TYPE_IQ2_XXS  = 16
let TYPE_IQ2_XS   = 17
let TYPE_IQ3_XXS  = 18
let TYPE_IQ1_S    = 19
let TYPE_IQ4_NL   = 20
let TYPE_IQ3_S    = 21
let TYPE_IQ2_S    = 22
let TYPE_IQ4_XS   = 23
let TYPE_I8       = 24
let TYPE_I16      = 25
let TYPE_I32      = 26
let TYPE_I64      = 27
let TYPE_F64      = 28
let TYPE_IQ1_M    = 29
let TYPE_BF16     = 30
let TYPE_TQ1_0    = 34
let TYPE_TQ2_0    = 35
let TYPE_MXFP4    = 39

# Logging levels (subset)
let LOG_NONE  = 0
let LOG_DEBUG = 1
let LOG_INFO  = 2
let LOG_WARN  = 3
let LOG_ERROR = 4

# Convenience: strings
let versionString = _C.ggml_version
let commitString  = _C.ggml_commit

# --- Helpers following the wrapper pattern ------------------------------------

# Make ggml_init_params as a handle (required: aggregates are handles only)
let makeInitParams = fun(memSizeBytes, memBuffer, noAlloc) {
  # memBuffer: either Null or a handle to previously-allocated memory
  _C.__mem.box("ggml_init_params", { mem_size: memSizeBytes, mem_buffer: memBuffer, no_alloc: noAlloc })
}

# Create / free context
let createContext = fun(memSizeBytes, noAlloc=false) {
  let params = makeInitParams(memSizeBytes, null, noAlloc)
  let ctx = _C.ggml_init(params)
  if ctx == null then fail("createContext: ggml_init returned null") end
  # Attach destructor so users don't leak if they drop the handle
  _C.__mem.gc(ctx, { sym:"ggml_free" })
  ctx
}

let resetContext   = _C.ggml_reset
let freeContext    = _C.ggml_free         # explicit free if you want manual control
let usedMem        = _C.ggml_used_mem
let setNoAlloc     = _C.ggml_set_no_alloc
let getNoAlloc     = _C.ggml_get_no_alloc
let getMemSize     = _C.ggml_get_mem_size
let getMaxTensorSz = _C.ggml_get_max_tensor_size

# Tensor builders (thin)
let tensor1d = fun(ctx, type, ne0) { _C.ggml_new_tensor_1d(ctx, type, ne0) }
let tensor2d = fun(ctx, type, ne0, ne1) { _C.ggml_new_tensor_2d(ctx, type, ne0, ne1) }
let tensor3d = fun(ctx, type, ne0, ne1, ne2) { _C.ggml_new_tensor_3d(ctx, type, ne0, ne1, ne2) }
let tensor4d = fun(ctx, type, ne0, ne1, ne2, ne3) { _C.ggml_new_tensor_4d(ctx, type, ne0, ne1, ne2, ne3) }

# Names & roles
let getName   = _C.ggml_get_name
let setName   = _C.ggml_set_name
let setInput  = _C.ggml_set_input
let setOutput = _C.ggml_set_output
let setParam  = _C.ggml_set_param
let setLoss   = _C.ggml_set_loss

# Raw data access (unsafe if you write inconsistent types; keep as handles)
let dataPtr     = _C.ggml_get_data            # -> void*
let dataPtrF32  = _C.ggml_get_data_f32        # -> float*
let nelements   = _C.ggml_nelements
let nrows       = _C.ggml_nrows
let nbytes      = _C.ggml_nbytes
let nbytesPadded= _C.ggml_nbytes_pad
let elementSize = _C.ggml_element_size

# Arithmetic / ops (representative selection)
let add      = _C.ggml_add
let mul      = _C.ggml_mul
let div      = _C.ggml_div
let sum      = _C.ggml_sum
let mulMat   = _C.ggml_mul_mat
let relu     = _C.ggml_relu
let gelu     = _C.ggml_gelu
let softMax  = _C.ggml_soft_max
let norm     = _C.ggml_norm

# Shape / layout
let transpose = _C.ggml_transpose
let cont      = _C.ggml_cont
let reshape2d = _C.ggml_reshape_2d

# Data movement / cast
let copy = _C.ggml_cpy
let cast = _C.ggml_cast

# Graph helpers (forward-only build; execute via your chosen backend)
let newGraph        = _C.ggml_new_graph
let newGraphCustom  = _C.ggml_new_graph_custom
let buildForward    = _C.ggml_build_forward_expand
let graphSize       = _C.ggml_graph_size
let graphNode       = _C.ggml_graph_node
let graphPrint      = _C.ggml_graph_print

# --- Higher-level ergonomic helpers -------------------------------------------

# Convenience: create a 1D F32 tensor and fill from a JS-style array of numbers
# NOTE: copies via __mem; you still pass/receive handles.
let tensor1dF32 = fun(ctx, len, valuesArrOrNull) {
  let t = tensor1d(ctx, TYPE_F32, len)
  if t == null then fail("tensor1dF32: allocation failed") end
  if valuesArrOrNull != null then
    let p = dataPtrF32(t)
    _C.__mem.copy(p, valuesArrOrNull, len * 4)
  end
  t
}

# Read back a tensor of F32 into a MindScript array (when contiguous)
let readF32 = fun(t) {
  # Best-effort: require contiguous layout
  # (You can explicitly call cont(ctx, t) yourself.)
  let bytes = nbytes(t)
  let p = dataPtrF32(t)
  _C.__mem.string(p, bytes)  # raw bytes; consumer can reinterpret if needed
}

# Build a tiny demo graph: y = a*x^2 + b
let demoQuad = fun(ctx) {
  let x  = tensor1d(ctx, TYPE_F32, 1)
  setParam(x)
  setName(x, "x")
  let a  = tensor1d(ctx, TYPE_F32, 1)
  let b  = tensor1d(ctx, TYPE_F32, 1)
  setName(a, "a")
  setName(b, "b")
  let x2 = mul(ctx, x, x)
  let ax2= mul(ctx, a, x2)
  let y  = add(ctx, ax2, b)
  let gf = newGraph(ctx)
  buildForward(gf, y)
  { x:x, a:a, b:b, y:y, g:gf }
}

# --- Notes --------------------------------------------------------------------
# * Many GGML APIs return pointers to internal arena memory. We do not auto-free
#   tensors; the context owns them. The context handle has a destructor attached
#   by default (ggml_free).
# * Functions taking struct-by-value (e.g., ggml_init) require a handle created
#   via _C.__mem.box("ggml_init_params", { ... }).
# * If you need more ops, expose them 1:1 by adding to `_C.functions` above, then
#   re-export an alias here. Keep to the "handles only" rule for aggregates.
