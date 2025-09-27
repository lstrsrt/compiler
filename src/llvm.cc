#include "llvm.hh"
#include "compiler.hh"
#include "diagnose.hh"
#include "parser.hh"
#include "verify.hh"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>

#include <map>
#include <memory>

namespace llvm_gen {

namespace g {
    inline std::unique_ptr<llvm::LLVMContext> context;
    inline std::unique_ptr<llvm::IRBuilder<>> ir_builder;
    inline std::unique_ptr<llvm::Module> module;
    inline std::map<std::string, llvm::AllocaInst *> named_values;
} // namespace g

llvm::LLVMContext &context()
{
    return *g::context;
}

auto *ir_builder()
{
    return g::ir_builder.get();
}

auto *module()
{
    return g::module.get();
}

auto *current_function()
{
    return ir_builder()->GetInsertBlock()->getParent();
}

llvm::Type *primitive_type(Type *t)
{
    if (t->get_kind() == TypeFlags::Void) {
        return ir_builder()->getVoidTy();
    }

    switch (t->size) {
        case 8:
            return ir_builder()->getInt64Ty();
        case 4:
            return ir_builder()->getInt32Ty();
        case 1:
            return ir_builder()->getInt1Ty();
    }
    TODO();
}

llvm::AllocaInst *create_entry_block_alloca(llvm::Function *function, Variable *var)
{
    llvm::IRBuilder<> tmp(&function->getEntryBlock(), function->getEntryBlock().begin());
    return tmp.CreateAlloca(primitive_type(var->unaliased_type()), nullptr, var->name);
}

llvm::AllocaInst *create_entry_block_alloca(
    llvm::Function *function, llvm::Type *type, llvm::StringRef name)
{
    llvm::IRBuilder<> tmp(&function->getEntryBlock(), function->getEntryBlock().begin());
    return tmp.CreateAlloca(type, nullptr, name);
}

llvm::Value *generate(Compiler &, Ast *, bool alloca = false);

void ensure_returns(Compiler &cc, AstFunction *ast, llvm::Function &function)
{
    bool is_main = function.getName() == "main";
    bool is_void = function.getReturnType()->isVoidTy();
    for (auto &bb : function) {
        llvm::Instruction *last = bb.empty() ? nullptr : &bb.back();
        if (last && last->isTerminator()) {
            continue;
        }
        llvm::IRBuilder<> builder(&bb);
        if (is_void) {
            builder.CreateRetVoid();
        } else if (is_main) {
            auto *zero = llvm::Constant::getNullValue(ir_builder()->getInt32Ty());
            builder.CreateRet(zero);
        } else {
            diag_error_at(cc, ast->location, ErrorType::Verification,
                "non-void function does not return a value on all paths");
        }
    }
}

llvm::Function *generate_function(Compiler &cc, AstFunction *f)
{
    auto *function = module()->getFunction(f->name);
    if (!function) {
        // generate proto
        std::vector<llvm::Type *> types(f->params.size(), nullptr);
        for (size_t i = 0; i < types.size(); ++i) {
            types[i] = primitive_type(f->params[i]->var.unaliased_type());
        }
        auto *proto_ft = llvm::FunctionType::get(
            primitive_type(get_unaliased_type(f->return_type)), types, /*isVarArg=*/false);
        function
            = llvm::Function::Create(proto_ft, llvm::Function::ExternalLinkage, f->name, module());
        for (unsigned i = 0; auto &arg : function->args()) {
            arg.setName(f->params[i++]->var.name);
        }
    }

    if (!function->empty()) {
        dbgln("function cannot be redefined");
        return nullptr;
    }

    auto *bb = llvm::BasicBlock::Create(context(), "entry", function);
    auto *last = ir_builder()->GetInsertBlock();
    ir_builder()->SetInsertPoint(bb);

    g::named_values.clear();
    for (auto &arg : function->args()) {
        auto *alloca = create_entry_block_alloca(function, arg.getType(), arg.getName());
        ir_builder()->CreateStore(&arg, alloca);
        g::named_values[std::string(arg.getName())] = alloca;
    }

    generate(cc, f->body);
    ir_builder()->SetInsertPoint(last);

    ensure_returns(cc, f, *function);
    if (llvm::verifyFunction(*function, &llvm::errs())) {
        die("llvm verification error");
    }
    return function;
}

llvm::Value *generate_stmt(Compiler &cc, Ast *ast)
{
    switch (ast->operation) {
        case Operation::Return: {
            auto *ret = static_cast<AstReturn *>(ast);
            if (ret->expr) {
                return ir_builder()->CreateRet(generate(cc, ret->expr));
            }
            return ir_builder()->CreateRetVoid();
        }
        case Operation::VariableDecl: {
            auto *var_decl = static_cast<AstVariableDecl *>(ast);
            auto *alloca = create_entry_block_alloca(current_function(), &var_decl->var);
            g::named_values[var_decl->var.name] = alloca;
            if (var_decl->init_expr) {
                ir_builder()->CreateStore(generate(cc, var_decl->init_expr), alloca);
            } else {
                ir_builder()->CreateStore(
                    llvm::ConstantInt::getNullValue(primitive_type(var_decl->var.unaliased_type())),
                    alloca);
            }
            break;
        }
        case Operation::FunctionDecl: {
            generate_function(cc, static_cast<AstFunction *>(ast));
            break;
        }
        case Operation::If: {
            auto *if_stmt = static_cast<AstIf *>(ast);
            auto *function = current_function();
            auto *builder = ir_builder();

            auto *then_bb = llvm::BasicBlock::Create(context(), "then", function);

            bool has_else = if_stmt->else_body;
            auto *else_bb = llvm::BasicBlock::Create(context(), "else");

            auto *merge_bb = llvm::BasicBlock::Create(context(), "ifcont");

            auto *cond = generate(cc, if_stmt->expr);
            builder->CreateCondBr(cond, then_bb, else_bb);
            builder->SetInsertPoint(then_bb);

            auto *then = generate(cc, if_stmt->body);
            if (!then) {
                return nullptr;
            }
            builder->CreateBr(merge_bb);

            then_bb = builder->GetInsertBlock();
            function->insert(function->end(), else_bb);
            builder->SetInsertPoint(else_bb);

            llvm::Value *else_value = nullptr;
            if (has_else) {
                else_value = generate(cc, if_stmt->else_body);
                if (!else_value) {
                    return nullptr;
                }
            }

            builder->CreateBr(merge_bb);
            else_bb = builder->GetInsertBlock();

            function->insert(function->end(), merge_bb);
            builder->SetInsertPoint(merge_bb);
            return nullptr;
        }
        case Operation::While: {
            auto *while_stmt = static_cast<AstWhile *>(ast);
            auto *function = current_function();
            auto *builder = ir_builder();

            auto *cond_bb = llvm::BasicBlock::Create(context(), "while", function);
            auto *then_bb = llvm::BasicBlock::Create(context(), "then");
            auto *merge_bb = llvm::BasicBlock::Create(context(), "whilecont");

            builder->CreateBr(cond_bb);
            builder->SetInsertPoint(cond_bb);

            auto *cond = generate(cc, while_stmt->expr);
            builder->CreateCondBr(cond, then_bb, merge_bb);
            builder->SetInsertPoint(then_bb);

            auto *then = generate(cc, while_stmt->body);
            if (!then) {
                return nullptr;
            }
            builder->CreateBr(cond_bb);
            then_bb = builder->GetInsertBlock();
            function->insert(function->end(), then_bb);

            function->insert(function->end(), merge_bb);
            builder->SetInsertPoint(merge_bb);
            return nullptr;
        }
        default:
            TODO();
    }
    return nullptr;
}

llvm::Value *generate_unary(Compiler &cc, Ast *ast)
{
    switch (ast->operation) {
        case Operation::Negate: {
            auto *neg = static_cast<AstNegate *>(ast);
            return ir_builder()->CreateNeg(generate(cc, neg->operand), "neg");
        }
        case Operation::Call: {
            auto *call = static_cast<AstCall *>(ast);
            auto *callee = module()->getFunction(call->name);
            if (!callee) {
                // FIXME: this happens when a function is called before we defined it
                dbgln("unknown function");
                return nullptr;
            }
            std::vector<llvm::Value *> args;
            for (auto &arg : call->args) {
                args.push_back(generate(cc, arg));
                if (!args.back()) {
                    dbgln("call arg gen failure");
                    return nullptr;
                }
            }
            return ir_builder()->CreateCall(callee, args); 
        }
        case Operation::Cast: {
            auto *cast = static_cast<AstCast *>(ast);
            ExprConstness constness;
            // TODO: is_signed from cast_type or cast->expr type?
            auto is_signed = !get_expression_type(cc, cast->expr, &constness, TypeOverridable::No)
                                  ->has_flag(TypeFlags::UNSIGNED);
            return ir_builder()->CreateIntCast(
                generate(cc, cast->expr), primitive_type(cast->cast_type), is_signed, "cast");
        }
        default:
            TODO();
    }
}

llvm::Value *generate_binary(Compiler &cc, Ast *ast)
{
    auto *binary = static_cast<AstBinary *>(ast);
    bool alloca = ast->operation == Operation::Assign;
    auto *lhs = generate(cc, binary->left, alloca);
    auto *rhs = generate(cc, binary->right);
    ExprConstness constness;
    bool is_signed = !get_expression_type(cc, binary, &constness, TypeOverridable::No)
                          ->has_flag(TypeFlags::UNSIGNED);
    if (!lhs || !rhs) {
        return nullptr;
    }
    switch (ast->operation) {
        case Operation::Subtract:
            return ir_builder()->CreateSub(lhs, rhs, "sub");
        case Operation::Add:
            return ir_builder()->CreateAdd(lhs, rhs, "add");
        case Operation::Multiply:
            return ir_builder()->CreateMul(lhs, rhs, "mul");
        case Operation::Divide:
            if (is_signed) {
                return ir_builder()->CreateSDiv(lhs, rhs, "div");
            }
            return ir_builder()->CreateUDiv(lhs, rhs, "div");
        case Operation::Modulo: {
            auto op = is_signed
                ? llvm::Instruction::BinaryOps::SRem
                : llvm::Instruction::BinaryOps::URem;
            return ir_builder()->CreateBinOp(op, lhs, rhs, "mod");
        }
        case Operation::Assign: {
            return ir_builder()->CreateStore(rhs, lhs);
        }
        case Operation::NotEquals:
            return ir_builder()->CreateICmpNE(lhs, rhs, "cmp");
        case Operation::Equals:
            return ir_builder()->CreateICmpEQ(lhs, rhs, "cmp");
        case Operation::Less:
            if (is_signed) {
                return ir_builder()->CreateICmpSLT(lhs, rhs, "cmp");
            }
            return ir_builder()->CreateICmpULT(lhs, rhs, "cmp");
        case Operation::LessEquals:
            if (is_signed) {
                return ir_builder()->CreateICmpSLE(lhs, rhs, "cmp");
            }
            return ir_builder()->CreateICmpULE(lhs, rhs, "cmp");
        case Operation::Greater:
            if (is_signed) {
                return ir_builder()->CreateICmpSGT(lhs, rhs, "cmp");
            }
            return ir_builder()->CreateICmpUGT(lhs, rhs, "cmp");
        case Operation::GreaterEquals:
            if (is_signed) {
                return ir_builder()->CreateICmpSGE(lhs, rhs, "cmp");
            }
            return ir_builder()->CreateICmpUGE(lhs, rhs, "cmp");
        default:
            TODO();
    }
}

llvm::Value *generate(Compiler &cc, Ast *ast, bool alloca)
{
    switch (ast->type) {
        case AstType::Boolean: {
            auto boolean = static_cast<AstLiteral *>(ast)->u.boolean;
            return llvm::ConstantInt::getBool(ir_builder()->getInt1Ty(), boolean);
        }
        case AstType::Integer: {
            auto *literal = static_cast<AstLiteral *>(ast);
            auto integer = literal->u.u64;
            auto is_signed = !literal->expr_type->has_flag(TypeFlags::UNSIGNED);
            return llvm::ConstantInt::get(
                primitive_type(get_unaliased_type(literal->expr_type)), integer, is_signed);
        }
        case AstType::Identifier: {
            if (alloca) {
                return g::named_values[static_cast<AstIdentifier *>(ast)->var->name];
            }
            auto *ident = static_cast<AstIdentifier *>(ast);
            auto name = ident->var->name;
            return ir_builder()->CreateLoad(
                primitive_type(ident->var->unaliased_type()), g::named_values[name], name);
        }
        case AstType::Block:
            for (auto *stmt : static_cast<AstBlock *>(ast)->stmts) {
                generate(cc, stmt);
            }
            return llvm::ConstantInt::get(ir_builder()->getInt32Ty(), 0, /*IsSigned=*/true);
        case AstType::Unary:
            return generate_unary(cc, ast);
        case AstType::Binary:
            return generate_binary(cc, ast);
        case AstType::Statement:
            return generate_stmt(cc, ast);
        case AstType::String:
            TODO();
    }
    return nullptr;
}

void run(Compiler &cc, Ast *ast)
{
    g::context = std::make_unique<llvm::LLVMContext>();
    g::module = std::make_unique<llvm::Module>("test", context());
    g::ir_builder = std::make_unique<llvm::IRBuilder<>>(context());
    generate(cc, ast);
}

void dump()
{
#ifdef _DEBUG
    module()->print(llvm::errs(), nullptr);
#endif
    if (auto err = llvm::writeToOutput("llvm-output.ll", [](llvm::raw_ostream &os) -> llvm::Error {
            module()->print(os, nullptr);
            return llvm::Error::success();
        })) {
        die("llvm write error");
    }
}

} // namespace llvm_gen
