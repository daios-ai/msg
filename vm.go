// vm.go
package mindscript

// A minimal, internal bytecode VM.
// - Goroutine-safe (no package-level mutability).
// - Preserves the public, stable engine surface (Interpreter methods).
// - CALL delegates to Interpreter.Apply to keep currying & native dispatch.

import (
	"fmt"
	"math"
)

// -----------------------------
// Instruction encoding
// -----------------------------

type opcode uint8

const (
	opNop opcode = iota

	// constants & globals
	opConst      // push consts[k]
	opLoadGlobal // push Env[name];  imm = const index (name as VTStr)

	// stack/values
	opMakeArr // pop N → array; imm = N

	// property & index
	opGetProp // obj.get(name);   imm = const index (name as VTStr)
	opGetIdx  // pop idx,obj → push obj[idx]

	// arithmetic / compare / unary
	opSub
	opMul
	opDiv
	opMod
	opEq
	opNe
	opLt
	opLe
	opGt
	opGe
	opNeg
	opNot

	// control flow
	opJump        // ip = imm
	opJumpIfFalse // pop cond; if false => ip = imm
	opReturn      // pop v; signal Return with v
	opBreak       // signal Break with top (or null if empty)
	opContinue    // signal Continue with top (or null if empty)

	// calls/closures
	opCall // argc = imm; pops argc args then callee; pushes result
)

// pack/unpack helpers
func pack(op opcode, imm uint32) uint32 { return uint32(op)<<24 | (imm & 0xFFFFFF) }
func uop(i uint32) opcode               { return opcode(i >> 24) }
func uimm(i uint32) uint32              { return i & 0xFFFFFF }

// unwrap a VTMap to *MapObject (nil if not a map)
func asMap(v Value) *MapObject {
	if v.Tag != VTMap {
		return nil
	}
	return v.Data.(*MapObject)
}

// -----------------------------
// Bytecode container (internal)
// -----------------------------

type Chunk struct {
	Code   []uint32
	Consts []Value
}

// -----------------------------
// VM status/result
// -----------------------------

type vmStatus int

const (
	vmOK vmStatus = iota
	vmReturn
	vmBreak
	vmContinue
	vmRuntimeError
)

type vmResult struct {
	status vmStatus
	value  Value
}

// -----------------------------
// VM machine
// -----------------------------

type vm struct {
	ip    *Interpreter
	chunk *Chunk
	env   *Env
	stack []Value
	sp    int
	iptr  int
}

func (m *vm) push(v Value) {
	if m.sp >= len(m.stack) {
		newCap := len(m.stack) * 2
		if newCap == 0 {
			newCap = 16
		}
		ns := make([]Value, newCap)
		copy(ns, m.stack)
		m.stack = ns
	}
	m.stack[m.sp] = v
	m.sp++
}

func (m *vm) pop() Value {
	if m.sp == 0 {
		return errNull("stack underflow")
	}
	m.sp--
	return m.stack[m.sp]
}

func (m *vm) top() Value {
	if m.sp == 0 {
		return Null
	}
	return m.stack[m.sp-1]
}

func (m *vm) fail(msg string) vmResult {
	return vmResult{status: vmRuntimeError, value: errNull(msg)}
}

// -----------------------------
// Numeric helpers (mirror interpreter semantics)
// -----------------------------

// Numeric helpers (mirror interpreter semantics)
func (m *vm) binNum(op opcode, a, b Value) (Value, *vmResult) {
	// numbers
	if isNumber(a) && isNumber(b) {
		lf, rf := toFloat(a), toFloat(b)
		bothInt := a.Tag == VTInt && b.Tag == VTInt
		switch op {
		case opSub:
			if bothInt {
				return Int(a.Data.(int64) - b.Data.(int64)), nil
			}
			return Num(lf - rf), nil
		case opMul:
			if bothInt {
				return Int(a.Data.(int64) * b.Data.(int64)), nil
			}
			return Num(lf * rf), nil
		case opDiv:
			if (b.Tag == VTInt && b.Data.(int64) == 0) || (b.Tag == VTNum && b.Data.(float64) == 0.0) {
				res := m.fail("division by zero")
				return Null, &res
			}
			if bothInt {
				return Int(a.Data.(int64) / b.Data.(int64)), nil
			}
			return Num(lf / rf), nil
		case opMod:
			// guard zero (match division error text)
			if (b.Tag == VTInt && b.Data.(int64) == 0) || (b.Tag == VTNum && b.Data.(float64) == 0.0) {
				res := m.fail("division by zero")
				return Null, &res
			}
			if bothInt {
				return Int(a.Data.(int64) % b.Data.(int64)), nil
			}
			return Num(math.Mod(lf, rf)), nil
		case opLt:
			if bothInt {
				return Bool(a.Data.(int64) < b.Data.(int64)), nil
			}
			return Bool(lf < rf), nil
		case opLe:
			if bothInt {
				return Bool(a.Data.(int64) <= b.Data.(int64)), nil
			}
			return Bool(lf <= rf), nil
		case opGt:
			if bothInt {
				return Bool(a.Data.(int64) > b.Data.(int64)), nil
			}
			return Bool(lf > rf), nil
		case opGe:
			if bothInt {
				return Bool(a.Data.(int64) >= b.Data.(int64)), nil
			}
			return Bool(lf >= rf), nil
		}
	}

	// string comparisons
	if a.Tag == VTStr && b.Tag == VTStr {
		as, bs := a.Data.(string), b.Data.(string)
		switch op {
		case opLt:
			return Bool(as < bs), nil
		case opLe:
			return Bool(as <= bs), nil
		case opGt:
			return Bool(as > bs), nil
		case opGe:
			return Bool(as >= bs), nil
		}
	}
	return Value{}, &vmResult{status: vmRuntimeError, value: errNull("bad numeric operator")}
}

// -----------------------------
// VM runner
// -----------------------------

func (ip *Interpreter) runChunk(chunk *Chunk, env *Env, initStackCap int) vmResult {
	m := &vm{
		ip:    ip,
		chunk: chunk,
		// evaluate in the provided environment (global-chain root for this run)
		env:   env,
		stack: make([]Value, 0, initStackCap),
	}
	code := chunk.Code
	consts := chunk.Consts

	for m.iptr < len(code) {
		raw := code[m.iptr]
		m.iptr++
		opc := uop(raw)
		imm := uimm(raw)

		switch opc {

		case opNop:
			// no-op

		// ---- constants & globals ----
		case opConst:
			if int(imm) >= len(consts) {
				return m.fail("const index out of range")
			}
			m.push(consts[imm])

		case opLoadGlobal:
			if int(imm) >= len(consts) {
				return m.fail("name index out of range")
			}
			k := consts[imm]
			if k.Tag != VTStr {
				return m.fail("global name must be string const")
			}
			v, err := m.env.Get(k.Data.(string))
			if err != nil {
				return m.fail(err.Error())
			}
			m.push(v)

		// ---- arrays ----
		case opMakeArr:
			n := int(imm)
			if n < 0 || n > m.sp {
				return m.fail("bad array size")
			}
			start := m.sp - n
			elems := make([]Value, n)
			copy(elems, m.stack[start:m.sp])
			m.sp = start
			m.push(Arr(elems))

		// ---- properties / indices ----
		case opGetProp:
			if int(imm) >= len(consts) {
				return m.fail("name index out of range")
			}
			k := consts[imm]
			if k.Tag != VTStr {
				return m.fail("property name must be string const")
			}
			obj := m.pop()
			key := k.Data.(string)

			// Map lookup: use MapObject entries (annotation on keys is meta)
			if obj.Tag == VTMap {
				mo := asMap(obj)
				if mo == nil {
					return m.fail("property get requires map or module with string key")
				}
				if v, ok := mo.Entries[key]; ok {
					m.push(v)
					break
				}
				return m.fail(fmt.Sprintf("unknown property %q", key))
			}

			// Module export lookup stays the same
			if obj.Tag == VTModule {
				mod := obj.Data.(*Module)
				if v, ok := mod.get(key); ok {
					m.push(v)
					break
				}
				return m.fail(fmt.Sprintf("unknown property %q on module", key))
			}

			return m.fail("property get requires map or module with string key")

		case opGetIdx:
			idx := m.pop()
			obj := m.pop()

			// array[int]
			if obj.Tag == VTArray && idx.Tag == VTInt {
				xs := obj.Data.([]Value)
				if len(xs) == 0 {
					return m.fail("index on empty array")
				}
				i := int(idx.Data.(int64))
				if i < 0 {
					i = (i%len(xs) + len(xs)) % len(xs)
				}
				if i < 0 || i >= len(xs) {
					return m.fail("array index out of range")
				}
				m.push(xs[i])
				break
			}

			// map[string]
			if obj.Tag == VTMap && idx.Tag == VTStr {
				mo := asMap(obj)
				if mo == nil {
					return m.fail("index requires array[int] or map[string]")
				}
				k := idx.Data.(string)
				if v, ok := mo.Entries[k]; ok {
					m.push(v)
					break
				}
				return m.fail(fmt.Sprintf("unknown key %q", k))
			}

			return m.fail("index requires array[int] or map[string]")

		// ---- arithmetic / compare / unary ----

		case opSub, opMul, opDiv, opMod, opLt, opLe, opGt, opGe:
			b := m.pop()
			a := m.pop()
			if out, failRes := m.binNum(opc, a, b); failRes != nil {
				return *failRes
			} else {
				m.push(out)
			}

		case opEq, opNe:
			b := m.pop()
			a := m.pop()
			eq := m.ip.deepEqual(a, b)
			if opc == opEq {
				m.push(Bool(eq))
			} else {
				m.push(Bool(!eq))
			}

		case opNeg:
			x := m.pop()
			switch x.Tag {
			case VTInt:
				m.push(Int(-x.Data.(int64)))
			case VTNum:
				m.push(Num(-x.Data.(float64)))
			default:
				return m.fail("unary - expects number")
			}

		case opNot:
			x := m.pop()
			if x.Tag != VTBool {
				return m.fail("not expects boolean")
			}
			m.push(Bool(!x.Data.(bool)))

		// ---- control flow ----
		case opJump:
			m.iptr = int(imm)

		case opJumpIfFalse:
			cond := m.pop()
			if cond.Tag != VTBool {
				return m.fail("condition must be boolean")
			}
			if !cond.Data.(bool) {
				m.iptr = int(imm)
			}

		case opReturn:
			var v Value = Null
			if m.sp > 0 {
				v = m.pop()
			}
			return vmResult{status: vmReturn, value: v}

		case opBreak:
			v := m.top()
			if m.sp == 0 {
				v = Null
			}
			return vmResult{status: vmBreak, value: v}

		case opContinue:
			v := m.top()
			if m.sp == 0 {
				v = Null
			}
			return vmResult{status: vmContinue, value: v}

		// ---- calls ----
		case opCall:
			nargs := int(imm)

			// Stack layout (top on the right):
			// [..., callee, arg1, ..., argN]
			calleeIdx := m.sp - nargs - 1
			if calleeIdx < 0 {
				return m.fail("stack underflow in call")
			}
			callee := m.stack[calleeIdx]

			// Collect args in order
			args := make([]Value, nargs)
			copy(args, m.stack[calleeIdx+1:m.sp])

			// Pop callee + args
			m.sp = calleeIdx
			m.stack = m.stack[:m.sp]

			// Use current frame env as the call-site (for natives & closures)
			res := m.ip.applyArgsScoped(callee, args, m.env)

			// Push result
			m.push(res)

		default:
			return m.fail("unknown opcode")
		}
	}

	if m.sp == 0 {
		return vmResult{status: vmOK, value: Null}
	}
	return vmResult{status: vmOK, value: m.top()}
}
