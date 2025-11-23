#include "asm.hh"
#include "compiler.hh"
#include "debug.hh"
#include "file.hh"
#include "frontend.hh"
#include "ir.hh"
#include "parser.hh"
#include "verify.hh"

#include <utility>

std::string write_buffer;
bool write_stdout = false;

void emit_impl(std::string_view fmt, auto &&...args)
{
    auto str = std::vformat(fmt, std::make_format_args(std::forward<decltype((args))>(args)...));
#ifndef _DEBUG
    if (!opts.testing && write_stdout) {
        std::print("{}", str);
    }
#else
    if (!opts.testing) {
        std::print("{}", str);
    }
#endif
    write_buffer += str;
}

#define emit(_s_, ...) emit_impl("    " _s_ "\n" __VA_OPT__(, __VA_ARGS__))

void emit_prologue(int stack_size)
{
    emit("push rbp\n"
         "    mov rbp, rsp");
    if (stack_size > 0) {
        emit("sub rsp, {}", align_up(stack_size, 16));
    }
}

void emit_epilogue()
{
    emit("leave");
}

uint64_t get_key(IRArg src)
{
    if (src.arg_type == IRArgType::Vreg) {
        return src.u.vreg;
    }
    if (src.arg_type == IRArgType::Variable) {
        return reinterpret_cast<uint64_t>(src.u.variable);
    }
    if (src.arg_type == IRArgType::Parameter) {
        return reinterpret_cast<uint64_t>(src.u.variable);
    }
    TODO();
}

void debug_stack_location(int location, IRArg src)
{
    if (src.arg_type == IRArgType::Vreg) {
        emit(";; [rbp{:+}]: v{}", location, src.u.vreg);
    } else {
        emit(";; [rbp{:+}]: {}", location, src.u.variable->name);
    }
}

void debug_stack_location(int location, ssize_t target)
{
    emit(";; [rbp{:+}]: v{}", location, target);
}

void extend_stack(int &offset, IRFunction &ir_fn, const IRArg &src)
{
    if (ir_fn.stack_offsets.contains(get_key(src))) {
        return;
    }

    if (src.arg_type == IRArgType::Variable) {
        offset += align_up(get_unaliased_type(src.u.variable->type)->byte_size(), 8);
    } else {
        offset += 8;
    }

    debug_stack_location(-offset, src);
    ir_fn.stack_offsets[get_key(src)] = offset;
}

bool is_on_stack(IRArgType src_type)
{
    return src_type == IRArgType::Variable || src_type == IRArgType::Parameter
        || src_type == IRArgType::Vreg;
}

int allocate_stack(IRFunction &ir_fn)
{
    auto &stack_offsets = ir_fn.stack_offsets;
    int stack_size = 0;
    for (auto *arg : ir_fn.ast->params) {
        auto ir_arg = IRArg::make_parameter(&arg->var);
        extend_stack(stack_size, ir_fn, ir_arg);
    }
    for (auto *bb : ir_fn.basic_blocks) {
        if (bb->code.empty()) {
            continue;
        }
        for (auto *ir : bb->code) {
            if (is_on_stack(ir->left.arg_type)) {
                extend_stack(stack_size, ir_fn, ir->left);
            }
            if (ir->type == AstType::Binary && is_on_stack(ir->right.arg_type)) {
                extend_stack(stack_size, ir_fn, ir->right);
            }
            if (ir->has_vreg_target()) {
                stack_size += 8;
                debug_stack_location(-stack_size, ir->target);
                stack_offsets[ir->target] = stack_size;
            }
        }
    }
    return stack_size;
}

std::string stack_addr(const IRFunction &ir_fn, uint64_t lookup)
{
    std::string ret("[rbp-");
    ret += std::to_string(ir_fn.stack_offsets.at(lookup));
    ret += "]";
    return ret;
}

constexpr std::string param_regs[] = { "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

std::string stack_addr_or_const(const IRFunction &ir_fn, const IRArg &src)
{
    if (src.arg_type == IRArgType::Constant) {
        return extract_integer_constant(src.u.constant);
    }
    return stack_addr(ir_fn, get_key(src));
}

std::string extract_ir_arg(const IRFunction &ir_fn, IRArg arg)
{
    if (arg.arg_type == IRArgType::Parameter) {
        // NOTE: Parameters in registers are also pushed to the stack
        return stack_addr(ir_fn, get_key(arg));
    }
    if (arg.arg_type == IRArgType::String) {
        return std::format("str_{}", arg.string_index - 1);
    }
    return stack_addr_or_const(ir_fn, arg);
}

void emit_asm_stmt(Compiler &, const IRFunction &ir_fn, IR *ir)
{
    if (ir->operation == Operation::Return) {
        if (ir->left.arg_type != IRArgType::Empty) {
            if (ir->left.arg_type != IRArgType::Vreg || ir->left.u.vreg != -1) {
                emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            }
        }
        emit_epilogue();
        emit("ret");
        return;
    }
    TODO();
}

void emit_jump(IR *ir, BasicBlock *target)
{
    if (ir->basic_block_index + 1 != target->index) {
        emit("jmp {}", target->label_name);
    } else {
        emit("; fallthrough");
    }
}

std::string invert_jcc(const std::string &jcc)
{
    static const std::unordered_map<std::string, std::string> opposites = {
        { "je", "jne" },
        { "jne", "je" },
        { "jg", "jle" },
        { "jle", "jg" },
        { "jge", "jl" },
        { "jl", "jge" },
        { "ja", "jbe" },
        { "jbe", "ja" },
        { "jae", "jb" },
        { "jb", "jae" },
    };
    if (auto it = opposites.find(jcc); it != opposites.end()) {
        return it->second;
    }
    TODO();
    return {};
}

void emit_cond_jump(IR *ir, const std::string &jcc, BasicBlock *false_block, BasicBlock *true_block)
{
    if (ir->basic_block_index + 1 == true_block->index) {
        emit("; true block fallthrough");
        emit("{} {}", jcc, false_block->label_name);
    } else if (ir->basic_block_index + 1 == false_block->index) {
        emit("; false block fallthrough");
        emit("{} {}", invert_jcc(jcc), true_block->label_name);
    } else {
        emit("{} {}", jcc, false_block->label_name);
        emit("jmp {}", true_block->label_name);
    }
}

void emit_asm_cast(const IRFunction &ir_fn, IR *ir)
{
    assert(ir->right.arg_type == IRArgType::Type);
    assert(dynamic_cast<IRCast *>(ir) != nullptr);

    // Adapted from chibicc
    // https://github.com/rui314/chibicc/blob/main/codegen.c#L373
    constexpr const char *i32i8 = "movsx eax, al";
    constexpr const char *i32u8 = "movzx eax, al";
    constexpr const char *i32i16 = "movsx eax, ax";
    constexpr const char *i32u16 = "movzx eax, ax";
    constexpr const char *i32i64 = "movsxd rax, eax";
    constexpr const char *u32i64 = "mov eax, eax";

    // clang-format off
    static constexpr const char *cast_table[8][8]{
        // i8      i16      i32      i64      u8       u16      u32      u64
        { nullptr, nullptr, nullptr, i32i64,  i32u8,   i32u16,  nullptr, i32i64 },  // i8
        { i32i8,   nullptr, nullptr, i32i64,  i32u8,   i32u16,  nullptr, i32i64 },  // i16
        { i32i8,   i32i16,  nullptr, i32i64,  i32u8,   i32u16,  nullptr, i32i64 },  // i32
        { i32i8,   i32i16,  nullptr, nullptr, i32u8,   i32u16,  nullptr, nullptr }, // i64
        { i32i8,   nullptr, nullptr, i32i64,  nullptr, nullptr, nullptr, i32i64 },  // u8
        { i32i8,   i32i16,  nullptr, i32i64,  i32u8,   nullptr, nullptr, i32i64 },  // u16
        { i32i8,   i32i16,  nullptr, u32i64,  i32u8,   i32u16,  nullptr, u32i64 },  // u32
        { i32i8,   i32i16,  nullptr, nullptr, i32u8,   i32u16,  nullptr, nullptr }, // u64
    };
    // clang-format on

    auto *cast = dynamic_cast<IRCast *>(ir);
    // TODO: de-alias all types during IR gen
    auto *to_type = get_unaliased_type(cast->cast_type);
    auto *from_type = get_unaliased_type(ir->right.u.type);

    emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));

    enum {
        I8,
        I16,
        I32,
        I64,
        U8,
        U16,
        U32,
        U64
    };

    static auto get_id = [](Type *type) {
        switch (type->size) {
            case 64:
                return type->is_unsigned() ? U64 : I64;
            case 32:
                return type->is_unsigned() ? U32 : I32;
            case 16:
                return type->is_unsigned() ? U16 : I16;
            case 8:
                return type->is_unsigned() ? U8 : I8;
        }
    };

    if (auto const *res = cast_table[get_id(from_type)][get_id(to_type)]) {
        emit("{}", res);
    }
    emit("mov {}, rax", stack_addr(ir_fn, cast->target));
}

void emit_asm_unary(Compiler &, const IRFunction &ir_fn, IR *ir)
{
    static int stack_balance = 0;
    switch (ir->operation) {
        case Operation::Negate:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("neg rax");
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Not:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("not rax");
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::AddressOf:
            // HACK: when taking the addr of a string, it's already a pointer
            // so just a mov is needed. otherwise we will take the address of the pointer
            if (ir->left.u.variable->type->is_string()) {
                emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            } else {
                emit("lea rax, {}", extract_ir_arg(ir_fn, ir->left));
            }
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Load:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Dereference:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("mov rax, [rax]");
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::PushArg:
            if (ir->target < ssize(param_regs)) {
                emit("mov {}, {}", param_regs[ir->target], extract_ir_arg(ir_fn, ir->left));
            } else {
                if (ir->left.arg_type == IRArgType::Constant) {
                    emit("push {}", extract_integer_constant(ir->left.u.constant));
                } else {
                    emit("push qword {}", stack_addr(ir_fn, get_key(ir->left)));
                }
                stack_balance += 8;
            }
            break;
        case Operation::Call:
            if (has_flag(ir->left.u.function->attributes, FunctionAttributes::BUILTIN_PRINT)) {
                emit("mov al, 0");
                emit("call printf wrt ..plt");
            } else {
                emit("call {}", ir->left.u.function->name);
            }
            if (stack_balance > 0) {
                emit("add rsp, {}", stack_balance);
                stack_balance = 0;
            }
            if (ir->target != -1) {
                emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            }
            break;
        case Operation::Cast:
            emit_asm_cast(ir_fn, ir);
            break;
        case Operation::Branch:
            emit_jump(ir, ir->left.u.basic_block);
            break;
        default:
            TODO();
    }
}

void emit_compare(const IRFunction &ir_fn, const IRArg &arg)
{
    if (arg.arg_type == IRArgType::Constant
        && get_int_literal(arg.u.constant) > max_for_type(s32_type())) {
        emit("mov rcx, {}", extract_ir_arg(ir_fn, arg));
        emit("cmp rax, rcx");
    } else {
        emit("cmp rax, {}", extract_ir_arg(ir_fn, arg));
    }
}

void emit_asm_comparison(const IRFunction &ir_fn, IR *ir)
{
    const char *op = [ir, func = __func__]() {
        auto *type = ir->ast->expr_type;
        assert(type);
        bool is_unsigned = type->is_unsigned();
        switch (ir->operation) {
            case Operation::Equals:
                return "sete";
            case Operation::NotEquals:
                return "setne";
            case Operation::Greater:
                return is_unsigned ? "seta" : "setg";
            case Operation::GreaterEquals:
                return is_unsigned ? "setae" : "setge";
            case Operation::Less:
                return is_unsigned ? "setb" : "setl";
            case Operation::LessEquals:
                return is_unsigned ? "setbe" : "setle";
            default:
                todo(func, __FILE__, __LINE__);
        }
    }();
    emit("mov qword {}, 0", stack_addr(ir_fn, ir->target));
    emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
    emit_compare(ir_fn, ir->right);
    emit("{} {}", op, stack_addr(ir_fn, ir->target));
}

void emit_asm_binary(const IRFunction &ir_fn, IR *ir)
{
    switch (ir->operation) {
        case Operation::Assign:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", extract_ir_arg(ir_fn, ir->left));
            break;
        case Operation::Store:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("mov qword [rax], {}", extract_ir_arg(ir_fn, ir->right));
            break;
        case Operation::Add:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("add rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Subtract:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("sub rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Multiply:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("imul rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Divide:
            [[fallthrough]];
        case Operation::Modulo: {
            emit("push rdx");
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("mov r10, {}", extract_ir_arg(ir_fn, ir->right));
            auto *type = ir->ast->expr_type;
            assert(type);
            if (type->is_unsigned()) {
                emit("xor edx, edx");
                emit("div r10");
            } else {
                emit("cqo"); // TODO: cdq when using different reg sizes
                emit("idiv r10");
            }
            if (ir->operation == Operation::Modulo) {
                emit("mov {}, rdx", stack_addr(ir_fn, ir->target));
            } else {
                emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            }
            emit("pop rdx");
            break;
        }
        case Operation::And:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("and rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Or:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("or rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Xor:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("xor rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::LeftShift:
        case Operation::RightShift:
        case Operation::LeftRotate:
            [[fallthrough]];
        case Operation::RightRotate: {
            const char *op = [&, func = __func__]() {
                switch (ir->operation) {
                    case Operation::RightShift: {
                        auto *type = ir->ast->expr_type;
                        assert(type);
                        return type->is_unsigned() ? "shr" : "sar";
                    }
                    case Operation::LeftShift:
                        return "shl";
                    case Operation::LeftRotate:
                        return "rol";
                    case Operation::RightRotate:
                        return "ror";
                    default:
                        todo(func, __FILE__, __LINE__);
                }
            }();
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            if (is_on_stack(ir->right.arg_type)) {
                emit("mov cl, {}", extract_ir_arg(ir_fn, ir->right));
                emit("{} rax, cl", op); // nasm doesn't like cl not being named explicitly
            } else {
                emit("{} rax, {}", op, extract_ir_arg(ir_fn, ir->right));
            }
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        }
        case Operation::Equals:
        case Operation::NotEquals:
        case Operation::Greater:
        case Operation::GreaterEquals:
        case Operation::Less:
            [[fallthrough]];
        case Operation::LessEquals:
            emit_asm_comparison(ir_fn, ir);
            break;
        case Operation::BranchEq:
        case Operation::BranchNe:
        case Operation::BranchUGt:
        case Operation::BranchSGt:
        case Operation::BranchUGe:
        case Operation::BranchSGe:
        case Operation::BranchULt:
        case Operation::BranchSLt:
        case Operation::BranchULe:
            [[fallthrough]];
        case Operation::BranchSLe: {
            auto pick_jcc = [ir] {
                switch (ir->operation) {
                    case Operation::BranchEq:
                        return "je";
                    case Operation::BranchNe:
                        return "jne";
                    case Operation::BranchUGt:
                        return "ja";
                    case Operation::BranchSGt:
                        return "jg";
                    case Operation::BranchUGe:
                        return "jae";
                    case Operation::BranchSGe:
                        return "jge";
                    case Operation::BranchULt:
                        return "jb";
                    case Operation::BranchSLt:
                        return "jl";
                    case Operation::BranchULe:
                        return "jbe";
                    case Operation::BranchSLe:
                        return "jle";
                    default:
                        std::unreachable();
                }
            };
            auto *br = dynamic_cast<IRCondBranch *>(ir);
            const char *jcc = pick_jcc();
            emit("mov rax, {}", extract_ir_arg(ir_fn, br->left));
            emit_compare(ir_fn, br->right);
            emit_cond_jump(ir, jcc, br->true_block, br->false_block);
            break;
        }
        default:
            TODO();
    }
}

void emit_asm(Compiler &cc, const IRFunction &ir_fn, IR *ir)
{
    switch (ir->type) {
        case AstType::Binary:
            emit_asm_binary(ir_fn, ir);
            break;
        case AstType::Unary:
            emit_asm_unary(cc, ir_fn, ir);
            break;
        case AstType::Statement:
            emit_asm_stmt(cc, ir_fn, ir);
            break;
        default:
            dbgln("got unknown IR:");
            print_ir(stdout_file(), ir);
            TODO();
    }
}

std::string param_offset(size_t index)
{
    return std::format("[rbp+{}]", (index - ssize(param_regs) + 2) * 8);
}

void emit_asm_function(Compiler &cc, IRFunction &ir_fn)
{
    if (has_flag(ir_fn.ast->attributes, FunctionAttributes::DUMP_ASM)) {
        write_stdout = true;
        if (!opts.testing) {
            std::println("{}============= {}ASM for `{}`{} ============={}", colors::Cyan,
                colors::DefaultBold, demangled_name(ir_fn.ast->name), colors::Cyan,
                colors::Default);
        }
    }
    emit_impl("global {0}\n"
              "{0}:\n",
        ir_fn.ast->name);
    int stack_size = allocate_stack(ir_fn);
    emit_prologue(stack_size);
    for (size_t i = 0; i < ir_fn.ast->params.size(); ++i) {
        auto *arg = ir_fn.ast->params[i];
        if (i < size(param_regs)) {
            emit("mov {}, {}", stack_addr(ir_fn, reinterpret_cast<uint64_t>(&arg->var)),
                param_regs[i]);
        } else {
            emit("mov rax, {}", param_offset(i));
            emit("mov {}, rax", stack_addr(ir_fn, reinterpret_cast<uint64_t>(&arg->var)));
        }
    }
    for (auto *bb : ir_fn.basic_blocks) {
        if (!bb->reachable) {
            continue;
        }
        emit_impl("{}:\n", bb->label_name);
        for (auto *ir : bb->code) {
            if (bb->code.empty()) {
                continue;
            }
            emit_asm(cc, ir_fn, ir);
        }
    }
    emit_impl("\n");
    if (write_stdout) {
        write_stdout = false;
    }
}

std::string string_to_nasm(const std::string &s)
{
    std::string ret;
    bool did_escape = false;
    bool in_string = false;
    for (char i : s) {
        if (is_control_char(i)) {
            if (in_string) {
                ret += "\", ";
                in_string = false;
            } else if (did_escape) {
                ret += ", ";
            }
            ret += std::format("{}", static_cast<int>(i));
            did_escape = true;
        } else {
            if (did_escape) {
                ret += ", \"";
                did_escape = false;
            } else if (!in_string) {
                ret += "\"";
            }
            ret += i;
            in_string = true;
        }
    }
    if (!did_escape) {
        ret += "\"";
    }
    return ret;
}

void emit_asm(Compiler &cc)
{
    std::string output = opts.output_name;

    File output_file;
    output_file.buffered = false;
    if (!output_file.open(output, OpenFlags::OpenOrCreate | OpenFlags::TRUNCATE)) {
        die("{}: unable to open or create output file '{}'", cc.lexer.input.filename, output);
    }

    emit_impl("section .text\n"
              "extern printf\n");
    for (auto *ir_fn : cc.ir_builder.functions) {
        emit_asm_function(cc, *ir_fn);
    }

    emit_impl("\nsection .data\n");
    for (size_t i = 0; i < string_map.size(); ++i) {
        emit_impl("str_{}: db {}, 0\n", i, string_to_nasm(string_map[i]));
    }

    if (cc.test_mode.compare_type == CompareType::Asm) {
        cc.test_mode.compare_file = write_comparison_file(
            ".asm", nullptr, [](File &f, void *) { f.write(write_buffer); });
    }

    if (!output_file.write(write_buffer)) {
        die("{}: unable to write to output file '{}'", cc.lexer.input.filename, output);
    }
    write_buffer.clear();
}

void compile_to_exe(const std::string &asm_file, const std::string &output_name)
{
    if (spawn_blocking_process(
            "/usr/bin/nasm", { "-f elf64", "-o", output_name + ".o", asm_file })) {
        die("nasm failure", colors::Red, colors::Default);
    } else if (spawn_blocking_process(
                   "/usr/bin/gcc", { "-no-pie", output_name + ".o", "-o", output_name })) {
        die("gcc failure", colors::Red, colors::Default);
    }
}
