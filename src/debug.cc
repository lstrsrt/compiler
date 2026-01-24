#include "debug.hh"
#include "new-ir.hh"
#include "parser.hh"
#include "utils.hh"

std::string to_string(AstType type)
{
    using enum AstType;
    switch (type) {
#define __ENUMERATE_AST_TYPE(type) \
    case type:                     \
        return #type;
        ENUMERATE_AST_TYPES()
#undef __ENUMERATE_AST_TYPE
    }
    return "Unknown type";
}

std::string to_string(Operation operation)
{
    using enum Operation;
    switch (operation) {
#define __ENUMERATE_OPERATION(operation) \
    case operation:                      \
        return #operation;
        ENUMERATE_OPERATIONS()
#undef __ENUMERATE_OPERATION
    }
    return "Unknown operation";
}

std::string type_kind_to_string(TypeFlags flags)
{
    switch (flags & TypeFlags::kind_mask) {
        case TypeFlags::Unknown:
            return "<unknown>";
        case TypeFlags::Void:
            return "Void";
        case TypeFlags::Integer:
            return "Integer";
        case TypeFlags::Boolean:
            return "Boolean";
        case TypeFlags::String:
            return "String";
        default:
            TODO();
    }
    return "";
}

std::string type_flags_to_string(TypeFlags flags)
{
    // NOTE: builtin is skipped
    std::string ret{};
    if (has_flag(flags, TypeFlags::UNRESOLVED)) {
        ret += "unresolved";
        return ret;
    }
    if (has_flag(flags, TypeFlags::ALIAS)) {
        ret += "alias ";
    }
    if (has_flag(flags, TypeFlags::ENUM)) {
        ret += "enum ";
    }
    if (has_flag(flags, TypeFlags::UNSIGNED)) {
        ret += "unsigned ";
    }
    if (!has_flag(flags, TypeFlags::ALIAS | TypeFlags::UNRESOLVED)) {
        ret += type_kind_to_string(flags) + " ";
    }
    return ret;
}

void print_type(Type *type)
{
    std::println("{} {}", type->get_name(), type_flags_to_string(type->flags));
    if (type->real) {
        auto *real = type->real;
        std::string indent = "    ";
        while (real) {
            std::println("{}{} {}", indent, real->name, type_flags_to_string(real->flags));
            real = real->real;
            indent += "    ";
        }
    }
}

void print_types(Scope *scope)
{
    for (const auto &[name, type] : scope->types) {
        print_type(type);
    }
}

std::string escape_string(const std::string &s)
{
    std::string ret;
    ret.reserve(s.size());

    for (char c : s) {
        if (std::isprint(static_cast<unsigned char>(c))) {
            ret += c;
            continue;
        }
        switch (c) {
            case '\0':
                // TODO other control chars
                ret += "\\0";
                break;
            case '\a':
                ret += "\\a";
                break;
            case '\b':
                ret += "\\b";
                break;
            case '\e':
                ret += "\\e";
                break;
            case '\f':
                ret += "\\f";
                break;
            case '\n':
                ret += "\\n";
                break;
            case '\r':
                ret += "\\r";
                break;
            case '\t':
                ret += "\\t";
                break;
            case '\v':
                ret += "\\v";
                break;
            case '\\':
                ret += "\\";
                break;
            case '\"':
                ret += "\"";
                break;
            default:
                return ret += std::format("\\x{:X}", static_cast<uint32_t>(c));
        }
    }

    return ret;
}

static std::string var_type_name(Variable *var)
{
    return var->type->name.empty() ? "<auto>" : var->type->get_name();
}

static void print_var_decl(File &file, AstVariableDecl *var_decl)
{
    auto *type = var_decl->var.type;
    if (type->name.empty()) {
        file.fwrite("{}: <auto>", var_decl->var.name);
    } else {
        // TODO: different behavior for pointers
        const auto flags_str = type_flags_to_string(type->flags);
        file.fwrite("{}: {} {}", var_decl->var.name, type->get_name(), flags_str);
        if (type->has_flag(TypeFlags::ALIAS)) {
            file.fwrite("-> {}", get_unaliased_type(type)->name);
        } else if (!type->has_flag(TypeFlags::UNRESOLVED)) {
            file.fwrite("size {}", type->byte_size());
        }
    }
    file.write("]\n");
    file.commit();
}

static void print_enum_decl(File &file, AstEnumDecl *decl)
{
    file.fwriteln("{}]", decl->enum_type->get_name());
    file.commit();
}

static void print_node(File &file, Ast *ast, std::string_view indent)
{
    if (ast->operation == Operation::EnumMember) {
        if (!static_cast<AstEnumMember *>(ast)->expr) {
            file.fwrite("{}<unevaluated>\n", indent);
        } else {
            print_node(file, static_cast<AstEnumMember *>(ast)->expr, indent);
        }
        return;
    }

    if (ast->operation == Operation::None) {
        file.fwrite("{}", indent);
        if (ast->type == AstType::Integer || ast->type == AstType::Boolean) {
            auto *literal = static_cast<AstLiteral *>(ast);
            file.fwrite(
                "{} ({})", extract_integer_constant(literal), literal->expr_type->get_name());
        } else if (ast->type == AstType::String) {
            auto *string = static_cast<AstString *>(ast);
            file.fwrite("\"{}\" (string)", escape_string(string->string));
        } else if (ast->type == AstType::Identifier) {
            auto *ident = static_cast<AstIdentifier *>(ast);
            file.fwrite("{} ({})", ident->string, var_type_name(ident->var));
        } else {
            file.fwrite("[{}]", to_string(ast->type));
        }
        file.write("\n");
        return;
    }

    file.fwrite("{}[{}", indent, to_string(ast->operation));
    if (ast->operation == Operation::Call) {
        auto *call = static_cast<AstCall *>(ast);
        file.fwriteln(" {} ({} args)]", call->name, call->args.size());
    } else if (ast->operation == Operation::FunctionDecl) {
        auto *function = static_cast<AstFunction *>(ast);
        file.fwriteln(" {} -> {}]", function->name, function->return_type->get_name());
        for (auto *p : function->params) {
            file.fwrite("{}[Param: ", indent);
            print_var_decl(file, p);
        }
    } else if (ast->operation == Operation::VariableDecl) {
        file.write(" ");
        print_var_decl(file, static_cast<AstVariableDecl *>(ast));
    } else if (ast->operation == Operation::EnumDecl) {
        file.write(" ");
        print_enum_decl(file, static_cast<AstEnumDecl *>(ast));
    } else if (ast->operation == Operation::Cast) {
        file.fwriteln(" {}]", static_cast<AstCast *>(ast)->cast_type->get_name());
    } else {
        file.write("]\n");
    }
    file.commit();
}

void print_ast(File &file, const StmtVec &ast_vec, const std::string &indent)
{
    for (auto *ast : ast_vec) {
        print_ast(file, ast, indent);
    }
    file.commit();
}

void print_ast(File &file, Ast *ast, std::string indent)
{
    if (!ast) {
        return;
    }

    auto prev_indent = indent;
    print_node(file, ast, indent);
    if (ast->operation != Operation::None) {
        indent += "    ";
    }

    switch (ast->type) {
        case AstType::Unary: {
            if (ast->operation == Operation::Call) {
                auto *call = static_cast<AstCall *>(ast);
                for (Ast *arg : call->args) {
                    print_ast(file, arg, indent);
                }
            } else if (ast->operation == Operation::Negate || ast->operation == Operation::Not
                || ast->operation == Operation::AddressOf
                || ast->operation == Operation::Dereference
                || ast->operation == Operation::LogicalNot || ast->operation == Operation::Cast
                || ast->operation == Operation::Load || ast->operation == Operation::Store) {
                print_ast(file, static_cast<AstUnary *>(ast)->operand, indent);
            } else {
                TODO();
            }
            break;
        }
        case AstType::Binary: {
            auto *binop = static_cast<AstBinary *>(ast);
            print_ast(file, binop->left, indent);
            print_ast(file, binop->right, indent);
            break;
        }
        case AstType::Statement: {
            if (ast->operation == Operation::Return) {
                print_ast(file, static_cast<AstReturn *>(ast)->expr, indent);
            } else if (ast->operation == Operation::FunctionDecl) {
                print_ast(file, static_cast<AstFunction *>(ast)->body, indent);
            } else if (ast->operation == Operation::VariableDecl) {
                print_ast(file, static_cast<AstVariableDecl *>(ast)->init_expr, indent);
            } else if (ast->operation == Operation::If) {
                auto *if_stmt = static_cast<AstIf *>(ast);
                print_ast(file, if_stmt->expr, indent);
                print_ast(file, if_stmt->body, indent);
                if (if_stmt->else_body) {
                    // HACK:
                    file.fwriteln("{}[Else]", prev_indent);
                    print_ast(file, if_stmt->else_body, indent);
                }
            } else if (ast->operation == Operation::While) {
                print_ast(file, static_cast<AstWhile *>(ast)->expr, indent);
                print_ast(file, static_cast<AstWhile *>(ast)->body, indent);
            } else if (ast->operation == Operation::For) {
                print_ast(file, static_cast<AstFor *>(ast)->var_decl, indent);
                print_ast(file, static_cast<AstFor *>(ast)->change, indent);
                print_ast(file, static_cast<AstFor *>(ast)->cmp, indent);
                print_ast(file, static_cast<AstFor *>(ast)->body, indent);
            } else if (ast->operation == Operation::Break
                || ast->operation == Operation::Continue) {
                break;
            } else if (ast->operation == Operation::EnumDecl) {
                auto *decl = static_cast<AstEnumDecl *>(ast);
                for (auto *member : decl->members->vector) {
                    file.fwrite("{}{} = ", indent, static_cast<AstEnumMember *>(member)->name);
                    print_node(file, member, "");
                }
            } else {
                TODO();
            }
            break;
        }
        case AstType::Block: {
            indent += "    ";
            print_ast(file, static_cast<AstBlock *>(ast)->stmts, indent);
            file.write("\n");
            break;
        }
        // Leaf nodes are fully handled in print_node
        case AstType::Integer:
        case AstType::Boolean:
        case AstType::String:
        case AstType::Enum:
            [[fallthrough]];
        case AstType::Identifier:
            break;
        default:
            TODO();
    }
    file.commit();
}

std::string to_string(IRArgType type)
{
    using enum IRArgType;
    switch (type) {
#define __ENUMERATE_IR_ARG_TYPE(type) \
    case type:                        \
        return #type;
        ENUMERATE_IR_ARG_TYPES()
#undef __ENUMERATE_IR_ARG_TYPE
    }
    return "Unknown source type";
}

std::string get_ir_arg_value(const IRArg &src)
{
    switch (src.arg_type) {
        case IRArgType::Empty:
            return "";
        case IRArgType::Undef:
            return "undef";
        case IRArgType::SSA:
            return '%' + src.u.ssa->source->name + '_' + std::to_string(src.u.ssa->index);
        case IRArgType::Constant:
            return extract_integer_constant(src.u.constant);
        case IRArgType::String:
            return "\"" + escape_string(src.u.string->string) + "\"";
        case IRArgType::Vreg:
            return "v" + std::to_string(src.u.vreg);
        case IRArgType::Parameter:
            [[fallthrough]];
        case IRArgType::Variable:
            return src.u.variable->name;
        case IRArgType::Function:
            return src.u.function->name;
        case IRArgType::BasicBlock:
            return std::to_string(src.u.basic_block->index);
        case IRArgType::Type:
            return src.u.type->get_name();
        default:
            TODO();
    }
    assert(!"get_ir_arg_value unhandled source type");
}

void print_ir(File &file, BasicBlock *bb)
{
    for (auto *code : bb->code) {
        print_ir(file, code);
    }
    file.commit();
}

void print_ir(File &file, IR *ir)
{
    std::string target = ir->has_vreg_target() ? std::format("v{} = ", ir->target) : "";
    file.fwrite("    {}{} ", target, to_string(ir->operation));
    if (auto *phi = dynamic_cast<IRPhi *>(ir)) {
        for (size_t i = 0; i < size(phi->phi_operands); ++i) {
            const auto &op = phi->phi_operands[i];
            file.fwrite("[ {} <{}>, {} ]", get_ir_arg_value(op.value), to_string(op.value.arg_type),
                op.block->index);
            if (i < size(phi->phi_operands) - 1) {
                file.write(", ");
            }
        }
        file.write("\n");
        file.commit();
        return;
    }
    if (auto *br = dynamic_cast<IRCondBranch *>(ir)) {
        file.fwrite("{} <BasicBlock>, ", std::to_string(br->true_block->index));
        if (br->false_block) {
            file.fwrite("{} <BasicBlock>, ", std::to_string(br->false_block->index));
        }
        file.fwrite(
            "{} <{}>", get_ir_arg_value(ir->get_left()), to_string(ir->get_left().arg_type));
        file.fwriteln(
            ", {} <{}>", get_ir_arg_value(ir->get_right()), to_string(ir->get_right().arg_type));
        file.commit();
        return;
    }
    file.fwrite("{} <{}>", get_ir_arg_value(ir->get_left()), to_string(ir->get_left().arg_type));
    if (ir->type == AstType::Binary) {
        file.fwrite(
            ", {} <{}>", get_ir_arg_value(ir->get_right()), to_string(ir->get_right().arg_type));
    } else if (ir->operation == Operation::Cast) {
        auto *cast = dynamic_cast<IRCast *>(ir);
        file.fwrite(", {} <Type>", cast->cast_type->get_name());
    }
    file.write("\n");
    file.commit();
}

void print_ir(File &file, const IRFunction &ir_fn)
{
    file.fwriteln("{}:", ir_fn.ast->name);
    for (size_t i = 0; i < ir_fn.basic_blocks.size(); ++i) {
        auto *bb = ir_fn.basic_blocks[i];
        const char *s = bb->reachable ? "live" : "dead";
        const char *s2 = bb->terminal ? "terminal" : "non-terminal";
        file.fwrite("{} ({}, {}){}", i, s, s2, size(bb->predecessors) ? " preds: " : "");
        if (size(bb->predecessors)) {
            for (size_t j = 0; j < size(bb->predecessors); ++j) {
                file.fwrite("{}", bb->predecessors[j]->index);
                if (j < size(bb->predecessors) - 1) {
                    file.write(", ");
                }
            }
        }
        file.write("\n");
        print_ir(file, bb);
        file.write("\n");
    }
    file.write("\n");
    file.commit();
}

namespace new_ir {

std::string to_string(InstType type)
{
    using enum InstType;

#define __ENUMERATE_INST_TYPE(type) \
    case type:                      \
        return #type;

    switch (type) {
        ENUMERATE_INST_TYPES()
    }

#undef __ENUMERATE_INST_TYPE
}

std::string to_string(const Integer &i, InstType type)
{
    switch (type) {
        case InstType::Ptr:
            return std::format("{:x}", i.value);
        case InstType::S1:
            return std::format("{}", i.value != 0);
        case InstType::S8:
            return std::format("{}", static_cast<int8_t>(i.value));
        case InstType::S16:
            return std::format("{}", static_cast<int16_t>(i.value));
        case InstType::S32:
            return std::format("{}", static_cast<int32_t>(i.value));
        case InstType::S64:
            return std::format("{}", static_cast<int64_t>(i.value));
        case InstType::U8:
            return std::format("{}", static_cast<uint8_t>(i.value));
        case InstType::U16:
            return std::format("{}", static_cast<uint16_t>(i.value));
        case InstType::U32:
            return std::format("{}", static_cast<uint32_t>(i.value));
        case InstType::U64:
            return std::format("{}", i.value);
        default:
            internal_error("{} is not an integer type", to_string(type));
    }
}

void print(File &file, BasicBlock *bb, SkipUnreachable skip_unreachable)
{
    if (!bb->reachable && skip_unreachable == SkipUnreachable::Yes) {
        return;
    }

    file.fwriteln("  bb{} {} (terminal={}, reachable={}):", bb->index_in_fn, colored(bb->name),
        bb->terminal, bb->reachable);
    if (!bb->predecessors.empty()) {
        file.write("  # preds:");
        for (size_t i = 0; i < size(bb->predecessors); ++i) {
            file.fwrite(
                " bb{} {}", bb->predecessors[i]->index_in_fn, colored(bb->predecessors[i]->name));
            if (i < size(bb->predecessors) - 1) {
                file.write(",");
            }
        }
        file.write("\n");
    }
    for (auto *inst : bb->code) {
        file.fwrite("    [{}] {} ", inst->index_in_bb, to_string(inst->type));
        switch (inst->kind) {
            case InstKind::Nop:
                file.fwrite("Nop {}", inst->name);
                break;
            case InstKind::Identity:
                file.fwrite("{} =", inst->name);
                break;
            case InstKind::Const:
                file.fwrite("{} = {}", inst->name,
                    to_string(static_cast<ConstInst *>(inst)->constant, inst->type));
                break;
            case InstKind::Arg:
                file.fwrite("{} = Arg {}", inst->name, static_cast<ArgInst *>(inst)->index);
                break;
            case InstKind::String:
                file.fwrite("{} = \"{}\"", inst->name, *static_cast<StringInst *>(inst)->string);
                break;
            case InstKind::Undef:
                file.fwrite("{} = undef", inst->name);
                break;
            case InstKind::Var: {
                auto *var = static_cast<VarInst *>(inst);
                file.fwrite("{} = {}", inst->name, var->variable->name);
            } break;
            case InstKind::SSA: {
                file.fwrite("{}", inst->name);
            } break;
            case InstKind::Unary:
                [[fallthrough]];
            case InstKind::Binary: {
                file.fwrite("{} = {}", inst->name, to_string(inst->operation));
            } break;
            case InstKind::Cast:
                file.fwrite("{} =", inst->name);
                break;
            case InstKind::Call:
                file.fwrite("{} = {} {}", inst->name, to_string(inst->operation),
                    static_cast<CallInst *>(inst)->function->name);
                break;
            case InstKind::Phi: {
                auto *phi = static_cast<PhiInst *>(inst);
                file.fwrite("{} =", inst->name);
                if (!phi->incoming.empty()) {
                    file.write(" [");
                    for (size_t i = 0; i < size(phi->incoming); ++i) {
                        file.fwrite(" {} {}", colored(phi->incoming[i].block->name),
                            phi->incoming[i].value->name);
                        if (i < size(phi->incoming) - 1) {
                            file.write(",");
                        }
                    }
                    file.write(" ]");
                }
            } break;
            default:
                file.fwriteln("skipping...");
        }
        if (inst->operation == Operation::Alloca) {
            file.fwrite(" {}", to_string(static_cast<AllocaInst *>(inst)->inst_type));
        }
        for (size_t i = 0; i < size(inst->args); ++i) {
            file.fwrite(" {}", inst->args[i]->name);
            if (i < size(inst->args) - 1) {
                file.write(",");
            }
        }
        if (inst->kind == InstKind::Cast) {
            file.fwrite(" to {}", to_string(static_cast<CastInst *>(inst)->cast));
        }
        file.write("\n");
    }
    if (!bb->successors.empty()) {
        file.write("  # succs:");
        for (size_t i = 0; i < size(bb->successors); ++i) {
            file.fwrite(
                " bb{} {}", bb->successors[i]->index_in_fn, colored(bb->successors[i]->name));
            if (i < size(bb->successors) - 1) {
                file.write(",");
            }
        }
        file.write("\n");
    }
    file.write("\n");
    file.commit();
}

void print(File &file, Function *fn, SkipUnreachable skip_unreachable)
{
    file.fwriteln("fn {}:", fn->name);
    for (auto *bb : fn->blocks) {
        print(file, bb, skip_unreachable);
    }
    file.write("\n");
    file.commit();
}

void print(File &file, IRBuilder &irb, SkipUnreachable skip_unreachable)
{
    for (auto *fn : irb.fns) {
        print(file, fn, skip_unreachable);
    }
    file.commit();
}

} // namespace new_ir
