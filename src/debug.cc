#include "debug.hh"
#include "parser.hh"

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
    // NOTE: builtin is skipped
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

static std::string var_type_name(Variable *var)
{
    return var->type->name.empty() ? "<auto>" : var->type->name;
}

static void print_var_decl(File &file, AstVariableDecl *var_decl)
{
    auto *type = var_decl->var.type;
    if (type->name.empty()) {
        file.fwrite("{}: <auto>", var_decl->var.name);
    } else {
        const auto flags_str = type_flags_to_string(type->flags);
        file.fwrite("{}: {} {}", var_decl->var.name, type->name, flags_str);
        if (type->has_flag(TypeFlags::ALIAS)) {
            file.fwrite("-> {}", get_unaliased_type(type)->name);
        } else if (!type->has_flag(TypeFlags::UNRESOLVED)) {
            file.fwrite("size {}", type->size);
        }
    }
    file.fwrite("]\n");
    file.commit();
}

static void print_node(File &file, Ast *ast, std::string_view indent)
{
    if (ast->operation == Operation::None) {
        file.fwrite("{}", indent);
        if (ast->type == AstType::Integer || ast->type == AstType::Boolean) {
            auto *literal = static_cast<AstLiteral *>(ast);
            file.fwrite("{} ({})", extract_integer_constant(literal), literal->expr_type->name);
        } else if (ast->type == AstType::String) {
            auto *string = static_cast<AstString *>(ast);
            // TODO - print escaped?
            file.fwrite("{} (string)", string->string);
        } else if (ast->type == AstType::Identifier) {
            auto *ident = static_cast<AstIdentifier *>(ast);
            file.fwrite("{} ({})", ident->string, var_type_name(ident->var));
        } else {
            file.fwrite("[{}]", to_string(ast->type));
        }
        file.fwrite("\n");
        return;
    }

    file.fwrite("{}[{}", indent, to_string(ast->operation));
    if (ast->operation == Operation::Call) {
        auto *call = static_cast<AstCall *>(ast);
        file.fwriteln(" {} ({} args)]", call->name, call->args.size());
    } else if (ast->operation == Operation::FunctionDecl) {
        auto *function = static_cast<AstFunction *>(ast);
        file.fwriteln(" {} -> {}]", function->name, function->return_type->name);
        for (size_t i = 0; i < function->params.size(); i++) {
            file.fwrite("{}[Param: ", indent);
            auto *p = function->params[i];
            print_var_decl(file, p);
        }
    } else if (ast->operation == Operation::VariableDecl) {
        file.fwrite(" ");
        print_var_decl(file, static_cast<AstVariableDecl *>(ast));
    } else if (ast->operation == Operation::Cast) {
        file.fwriteln(" {}]", static_cast<AstCast *>(ast)->cast_type->name);
    } else {
        file.fwriteln("]");
    }
    file.commit();
}

void print_ast(File &file, const std::vector<Ast *> &ast_vec, std::string indent)
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
            } else if (ast->operation == Operation::Negate || ast->operation == Operation::AddressOf
                || ast->operation == Operation::Dereference
                || ast->operation == Operation::LogicalNot || ast->operation == Operation::Cast) {
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
            } else {
                TODO();
            }
            break;
        }
        case AstType::Block: {
            indent += "    ";
            print_ast(file, static_cast<AstBlock *>(ast)->stmts, indent);
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
        case IRArgType::Constant:
            return extract_integer_constant(src.u.constant);
        case IRArgType::String:
            return src.u.string->string;
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
            return src.u.type->name;
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
    if (auto *br = dynamic_cast<IRCondBranch *>(ir)) {
        file.fwrite("{} <BasicBlock>, ", std::to_string(br->true_block->index));
        if (br->false_block) {
            file.fwrite("{} <BasicBlock>, ", std::to_string(br->false_block->index));
        }
        file.fwrite("{} <{}>", get_ir_arg_value(ir->left), to_string(ir->left.arg_type));
        file.fwriteln(", {} <{}>", get_ir_arg_value(ir->right), to_string(ir->right.arg_type));
        return;
    }
    file.fwrite("{} <{}>", get_ir_arg_value(ir->left), to_string(ir->left.arg_type));
    if (ir->type == AstType::Binary || ir->operation == Operation::Cast) {
        file.fwrite(", {} <{}>", get_ir_arg_value(ir->right), to_string(ir->right.arg_type));
    }
    file.fwrite("\n");
    file.commit();
}

void print_ir(File &file, const IRFunction &ir_fn)
{
    file.fwriteln("{}:", ir_fn.ast->name);
    for (size_t i = 0; i < ir_fn.basic_blocks.size(); ++i) {
        auto *bb = ir_fn.basic_blocks[i];
        const char *s = bb->reachable ? "live" : "dead";
        const char *s2 = bb->terminal ? "terminal" : "non-terminal";
        file.fwriteln("{} ({}, {}):", i, s, s2);
        print_ir(file, bb);
    }
    file.fwrite("\n");
    file.commit();
}
