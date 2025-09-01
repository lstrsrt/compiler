#include "debug.hh"

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
        case TypeFlags::Integer:
            return "Integer";
        case TypeFlags::Boolean:
            return "Boolean";
        case TypeFlags::Void:
            return "Void";
        case TypeFlags::String:
            return "String";
        default:
            TODO();
    }
    return "";
}

std::string type_flags_to_string(TypeFlags flags)
{
    std::string ret{};
    if (has_flag(flags, TypeFlags::UNRESOLVED)) {
        ret += "unresolved";
        return ret;
    }
    if (has_flag(flags, TypeFlags::ALIAS)) {
        ret += "alias ";
    }
    if (has_flag(flags, TypeFlags::UNSIGNED)) {
        ret += "unsigned ";
    }
    if (!has_flag(flags, TypeFlags::ALIAS | TypeFlags::UNRESOLVED)) {
        ret += type_kind_to_string(flags) + " ";
    }
    return ret;
}

void print_types(Scope *scope)
{
    for (const auto &[name, type] : scope->types) {
        std::println("{} {}", name, type_flags_to_string(type->flags));
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
}

static std::string var_type_name(Variable *var)
{
    return var->type->name.empty() ? "<auto>" : var->type->name;
}

static void print_var_decl(AstVariableDecl *var_decl)
{
    auto *type = var_decl->var.type;
    if (type->name.empty()) {
        std::print("{}: <auto>", var_decl->var.name);
    } else {
        const auto flags_str = type_flags_to_string(type->flags);
        std::print("{}: {} {}", var_decl->var.name, type->name, flags_str);
        if (type->has_flag(TypeFlags::ALIAS)) {
            std::print("-> {}", get_unaliased_type(type)->name);
        } else if (!type->has_flag(TypeFlags::UNRESOLVED)) {
            std::print("size {}", type->size);
        }
    }
    std::print("]\n");
}

static void print_node(Ast *ast, std::string_view indent)
{
    if (ast->operation == Operation::None) {
        std::print("{}", indent);
        if (ast->type == AstType::Integer || ast->type == AstType::Boolean) {
            auto *literal = static_cast<AstLiteral *>(ast);
            std::print("{} ({})", extract_integer_constant(literal), literal->literal_type->name);
        } else if (ast->type == AstType::String) {
            auto *string = static_cast<AstString *>(ast);
            // TODO - print escaped?
            std::print("{} (string)", string->string);
        } else if (ast->type == AstType::Identifier) {
            auto *ident = static_cast<AstIdentifier *>(ast);
            std::print("{} ({})", ident->string, var_type_name(ident->var));
        } else {
            std::print("[{}]", to_string(ast->type));
        }
        std::print("\n");
        return;
    }

    std::print("{}[{}", indent, to_string(ast->operation));
    if (ast->operation == Operation::Call) {
        auto call = static_cast<AstCall *>(ast);
        std::println(" {} ({} args)]", call->name, call->args.size());
    } else if (ast->operation == Operation::FunctionDecl) {
        auto function = static_cast<AstFunction *>(ast);
        std::println(" {} -> {}]", function->name, function->return_type->name);
        for (size_t i = 0; i < function->params.size(); i++) {
            std::print("{}[Param: ", indent);
            auto *p = function->params[i];
            print_var_decl(p);
        }
    } else if (ast->operation == Operation::VariableDecl) {
        std::print(" ");
        print_var_decl(static_cast<AstVariableDecl *>(ast));
    } else if (ast->operation == Operation::Cast) {
        std::println(" {}]", static_cast<AstCast *>(ast)->cast_type->name);
    } else {
        std::println("]");
    }
}

void print_ast(const std::vector<Ast *> &ast_vec, std::string indent)
{
    for (auto *ast : ast_vec) {
        print_ast(ast, indent);
    }
}

void print_ast(Ast *ast, std::string indent)
{
    if (!ast) {
        return;
    }

    auto prev_indent = indent;
    print_node(ast, indent);
    if (ast->operation != Operation::None) {
        indent += "    ";
    }

    switch (ast->type) {
        case AstType::Unary: {
            if (ast->operation == Operation::Call) {
                auto call = static_cast<AstCall *>(ast);
                for (Ast *arg : call->args) {
                    print_ast(arg, indent);
                }
            } else if (ast->operation == Operation::Negate) {
                print_ast(static_cast<AstNegate *>(ast)->operand, indent);
            } else if (ast->operation == Operation::Cast) {
                print_ast(static_cast<AstCast *>(ast)->expr, indent);
            }
            break;
        }
        case AstType::Binary: {
            auto binop = static_cast<AstBinary *>(ast);
            print_ast(binop->left, indent);
            print_ast(binop->right, indent);
            break;
        }
        case AstType::Statement: {
            if (ast->operation == Operation::Return) {
                print_ast(static_cast<AstReturn *>(ast)->expr, indent);
            } else if (ast->operation == Operation::FunctionDecl) {
                print_ast(static_cast<AstFunction *>(ast)->body, indent);
            } else if (ast->operation == Operation::VariableDecl) {
                print_ast(static_cast<AstVariableDecl *>(ast)->init_expr, indent);
            } else if (ast->operation == Operation::If) {
                auto *if_stmt = static_cast<AstIf *>(ast);
                print_ast(if_stmt->expr, indent);
                print_ast(if_stmt->body, indent);
                if (if_stmt->else_body) {
                    // HACK:
                    std::println("{}[Else]", prev_indent);
                    print_ast(if_stmt->else_body, indent);
                }
            } else if (ast->operation == Operation::While) {
                print_ast(static_cast<AstWhile *>(ast)->expr, indent);
                print_ast(static_cast<AstWhile *>(ast)->body, indent);
            }
            break;
        }
        case AstType::Block: {
            indent += "    ";
            print_ast(static_cast<AstBlock *>(ast)->stmts, indent);
            break;
        }
        // Leaf nodes are fully handled in print_node
        case AstType::Integer:
        case AstType::Boolean:
        case AstType::String:
        case AstType::Identifier:
            break;
        default:
            TODO();
    }
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
        case IRArgType::Constant:
            return extract_integer_constant(src.u.constant);
        case IRArgType::String:
            return src.u.string->string;
        case IRArgType::Vreg:
            return std::to_string(src.u.vreg);
        case IRArgType::Parameter:
            [[fallthrough]];
        case IRArgType::Variable:
            return src.u.variable->name;
        case IRArgType::Function:
            return src.u.function->name;
        case IRArgType::BasicBlock:
            return std::to_string(src.u.basic_block->index);
        case IRArgType::Type:
            return src.u.type->name;
        default:
            TODO();
    }
    assert(!"get_ir_arg_value unhandled source type");
}

void print_ir(BasicBlock *bb)
{
    for (auto *code : bb->code) {
        print_ir(code);
    }
    /*if (!bb->successors.empty()) {
        std::println("successors:");
        for (auto *s : bb->successors) {
            std::println("    {}", s->index);
        }
    }*/
}

void print_ir(IR *ir)
{
    std::string target = ir->has_vreg_target() ? std::format("v{} = ", ir->target) : "";
    std::print("    {}{} ", target, to_string(ir->operation));
    if (ir->operation == Operation::CondBranch) {
        auto cond = static_cast<IRBranch *>(ir)->cond;
        std::print("{} <{}>, ", get_ir_arg_value(cond), to_string(cond.arg_type));
    }
    std::print("{} <{}>", get_ir_arg_value(ir->left), to_string(ir->left.arg_type));
    if (ir->type == AstType::Binary || ir->operation == Operation::Cast) {
        std::print(", {} <{}>", get_ir_arg_value(ir->right), to_string(ir->right.arg_type));
    }
    std::print("\n");
}

void print_ir(const IRFunction &ir_fn)
{
    for (size_t i = 0; i < ir_fn.basic_blocks.size(); ++i) {
        auto *bb = ir_fn.basic_blocks[i];
        const char *s = bb->reachable ? "live" : "dead";
        const char *s2 = bb->terminal ? "terminal" : "non-terminal";
        std::println("{} ({}, {}):", i, s, s2);
        print_ir(bb);
    }
    std::print("\n");
}
