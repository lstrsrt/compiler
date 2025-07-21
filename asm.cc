#include "compiler.hh"

#include <fcntl.h>
#include <unistd.h>

// #define ONLY_PRINT_ASM

#ifdef ONLY_PRINT_ASM
struct OutputFile {
    void create(std::string_view) { }

    void write(std::string_view) { }

    void close() { }
};
#else
struct OutputFile {
    void create(std::string_view _name)
    {
        name = _name;
        file_handle = creat(name.data(), 0644);
        if (file_handle == -1) {
            perror("creat");
            exit(1);
        }
    }

    void write(std::string_view str)
    {
        write_buffer += str;
    }

    void close()
    {
        if (file_handle >= 0) {
            ::write(file_handle, write_buffer.c_str(), write_buffer.length());
            // fsync(fd); // TODO - is this necessary?
            ::close(file_handle);
        }
        name = {};
        file_handle = -1;
        write_buffer = {};
    }

private:
    std::string name;
    int file_handle = -1;
    std::string write_buffer;
};
#endif

OutputFile output_file;

void __emit(std::string_view fmt, auto &&...args)
{
    auto str = std::vformat(fmt, std::make_format_args(args...));
    if (!opts.testing) {
        dbg("{}", str);
    }
    output_file.write(str);
}

#define emit(_s_, ...) __emit("    " _s_ "\n" __VA_OPT__(, __VA_ARGS__))

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
    emit("mov rsp, rbp\n"
         "    pop rbp");
}

uint64_t get_key(IRArg src)
{
    if (src.arg_type == IRArgType::Vreg) {
        return src.u.vreg;
    }
    if (src.arg_type == IRArgType::Variable) {
        return reinterpret_cast<uint64_t>(src.u.variable);
    }
    assert(!"get_key unhandled source type");
}

void debug_stack_location(int location, IRArg src)
{
    if (src.arg_type == IRArgType::Variable) {
        emit(";; [rbp{:+}]: {}", location, src.u.variable->name);
    } else {
        emit(";; [rbp{:+}]: v{}", location, src.u.vreg);
    }
}

void debug_stack_location(int location, ssize_t target)
{
    emit(";; [rbp{:+}]: v{}", location, target);
}

void extend_stack(int &offset, IRFunction &ir_fn, const IRArg &src)
{
    if (ir_fn.stack_offsets.find(get_key(src)) != ir_fn.stack_offsets.end()) {
        return;
    }

    if (src.arg_type == IRArgType::Variable) {
        offset += align_up(get_unaliased_type(src.u.variable->type)->size, 8);
    } else if (src.arg_type == IRArgType::Vreg) {
        offset += 8;
    }

    debug_stack_location(-offset, src);
    ir_fn.stack_offsets[get_key(src)] = offset;
}

bool is_on_stack(IRArgType src_type)
{
    return src_type == IRArgType::Variable || src_type == IRArgType::Vreg;
}

int allocate_stack(IRFunction &ir_fn)
{
    auto &stack_offsets = ir_fn.stack_offsets;
    int stack_size = 0;
    for (auto *bb : ir_fn.basic_blocks) {
        for (auto *ir : bb->code) {
            if (ir->operation == Operation::None) {
                continue;
            }
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
        auto index = arg.u.variable->param_index;
        if (index < ssize(param_regs)) {
            return param_regs[index];
        }
        return std::format("[rbp+{}]", (index - ssize(param_regs) + 2) * 8);
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
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
        }
        emit_epilogue();
        emit("ret");
    }
}

int stack_balance = 0;
int reg_restores = 0;

void emit_asm_unary(Compiler &, const IRFunction &ir_fn, IR *ir)
{
    switch (ir->operation) {
        case Operation::Negate:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("neg rax");
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::PushArg:
            if (ir->target < ssize(param_regs)) {
                emit("push {}", param_regs[ir->target]);
                emit("mov {}, {}", param_regs[ir->target], extract_ir_arg(ir_fn, ir->left));
                ++reg_restores;
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
            emit("xor eax, eax");
            emit("call {}", ir->left.u.function->name);
            if (stack_balance > 0) {
                emit("add rsp, {}", stack_balance);
                stack_balance = 0;
            }
            if (ir->target != -1) {
                emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            }
            while (reg_restores) {
                emit("pop {}", param_regs[reg_restores - 1]);
                --reg_restores;
            }
            assert(reg_restores == 0);
            break;
        case Operation::Cast:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            break;
        case Operation::Branch:
            if (ir->basic_block_index + 1 != ir->left.u.basic_block->index) {
                emit("jmp {}", ir->left.u.basic_block->label_name);
            }
            break;
        case Operation::Fallthrough:
            break;
        default:
            TODO();
    }
}

void emit_asm_comparison(const IRFunction &ir_fn, IR *ir)
{
    constexpr std::array ops{ "sete", "setne", "setg", "setge", "setl", "setle" };
    auto op_index
        = static_cast<size_t>(to_underlying(ir->operation) - to_underlying(Operation::Equals));
    assert(op_index < ops.size());
    emit("mov qword {}, 0", stack_addr(ir_fn, ir->target));
    emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
    emit("cmp rax, {}", extract_ir_arg(ir_fn, ir->right));
    emit("{} {}", ops[op_index], stack_addr(ir_fn, ir->target));
}

void emit_asm_binary(const IRFunction &ir_fn, IR *ir)
{
    switch (ir->operation) {
        case Operation::Assign:
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->right));
            emit("mov {}, rax", extract_ir_arg(ir_fn, ir->left));
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
        case Operation::Modulo:
            emit("push rdx");
            emit("mov rax, {}", extract_ir_arg(ir_fn, ir->left));
            emit("cqo");
            emit("mov r10, {}", extract_ir_arg(ir_fn, ir->right));
            emit("idiv r10");
            if (ir->operation == Operation::Modulo) {
                emit("mov {}, rdx", stack_addr(ir_fn, ir->target));
            } else {
                emit("mov {}, rax", stack_addr(ir_fn, ir->target));
            }
            emit("pop rdx");
            break;
        case Operation::Equals:
        case Operation::NotEquals:
        case Operation::Greater:
        case Operation::GreaterEquals:
        case Operation::Less:
            [[fallthrough]];
        case Operation::LessEquals:
            emit_asm_comparison(ir_fn, ir);
            break;
        case Operation::CondBranch: {
            auto cond = static_cast<IRBranch *>(ir)->cond;
            if (cond.arg_type == IRArgType::Constant) {
                if (cond.u.constant->u.u64) {
                    if (ir->basic_block_index + 1 != ir->left.u.basic_block->index) {
                        emit("jmp {}", ir->left.u.basic_block->label_name);
                    }
                } else {
                    if (ir->basic_block_index + 1 != ir->right.u.basic_block->index) {
                        emit("jmp {}", ir->right.u.basic_block->label_name);
                    }
                }
            } else {
                emit("cmp qword {}, 0", extract_ir_arg(ir_fn, cond));
                // False block
                if (ir->basic_block_index + 1 != ir->right.u.basic_block->index) {
                    emit("je {}", ir->right.u.basic_block->label_name);
                    // Fall through if we dominate the true block
                    if (ir->basic_block_index + 1 != ir->left.u.basic_block->index) {
                        emit("jmp {}", ir->left.u.basic_block->label_name);
                    }
                } else {
                    emit("jne {}", ir->left.u.basic_block->label_name);
                    // Fall through if we dominate the false block
                }
            }
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
            TODO();
    }
}

void emit_asm_function(Compiler &cc, IRFunction &ir_fn)
{
    __emit("\nglobal {0}\n"
           "{0}:\n",
        ir_fn.ast->name);
    int stack_size = allocate_stack(ir_fn);
    emit_prologue(stack_size);
    for (size_t i = 0; i < ir_fn.basic_blocks.size(); ++i) {
        auto *bb = ir_fn.basic_blocks[i];
        __emit("{}:\n", bb->label_name);
        for (auto *ir : bb->code) {
            emit_asm(cc, ir_fn, ir);
        }
    }
}

std::string escape_string(const std::string &s)
{
    std::string ret;
    bool did_escape = false;
    bool in_string = false;
    for (size_t i = 0; i < s.size(); ++i) {
        if (is_control_char(s[i])) {
            if (in_string) {
                ret += "\", ";
                in_string = false;
            } else if (did_escape) {
                ret += ", ";
            }
            ret += std::format("{}", static_cast<int>(s[i]));
            did_escape = true;
        } else {
            if (did_escape) {
                ret += ", \"";
                did_escape = false;
            } else if (!in_string) {
                ret += "\"";
            }
            ret += s[i];
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
    output_file.create("output.asm");
    __emit("section .text\n");
    for (auto *ir_fn : cc.ir_builder.functions) {
        emit_asm_function(cc, *ir_fn);
    }
    __emit("\nsection .data\n");
    for (size_t i = 0; i < string_map.size(); ++i) {
        __emit("str_{}: db {}, 0\n", i, escape_string(string_map[i]));
    }
    output_file.close();
}
