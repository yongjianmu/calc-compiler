#include "llvm/ADT/APInt.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <iostream>
#include <unordered_map>
#include <string>

#include "typedef.h"

/********************************************************************
 * !! REFERENCE CODE: http://llvm.org/docs/tutorial/LangImpl03.html *
 ********************************************************************/

using namespace llvm;
using namespace std;

static LLVMContext C;
static IRBuilder<NoFolder> Builder(C);
static std::unique_ptr<Module> M = llvm::make_unique<Module>("calc", C);
static std::map<string, Value*> NamedValues;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

static std::string IdentifierStr; // Filled in if tok_identifier
static long long NumVal;             // Filled in if tok_number
static bool BoolVal;              // Filled in if tok_bool
static int Opera;
unordered_map<string, int> ump_op_type;

static int get_bin_op(string op)
{
    if(ump_op_type.find(op) == ump_op_type.end())
    {
        return op_none;
    }

    return ump_op_type[op];
}

static int LastChar = ' ';

static int gettok() {
    while (isspace(LastChar))
      LastChar = getchar();

    if ((LastChar == '(') || (LastChar == ')')){
        IdentifierStr.clear();
        IdentifierStr.push_back(LastChar);
        LastChar = getchar();
        return IdentifierStr.back() == '(' ? TOK_LBRA : TOK_RBRA;
    } 

    if (isalpha(LastChar)) {
        IdentifierStr.clear();
        IdentifierStr.push_back(LastChar);
        while (isalnum(LastChar = getchar()))
          IdentifierStr.push_back(LastChar);

        if (IdentifierStr.compare("if") == 0) {
            return TOK_BR;
        }

        if ((IdentifierStr.compare("true") == 0) || (IdentifierStr.compare("false") == 0)) {
            BoolVal = IdentifierStr == "true" ? true : false;
            return TOK_BOOL;
        }

        return TOK_VAR;
    }

    if (isdigit(LastChar)) {
        IdentifierStr.clear();
        do {
            IdentifierStr.push_back(LastChar);
            LastChar = getchar();
        } while (isdigit(LastChar));

        return TOK_NUMBER;
    }

    // Comment until end of line.
    if (LastChar == '#' || LastChar == '$' || LastChar == '^' || LastChar == '.') {
        do LastChar = getchar();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if (LastChar != EOF)
          return gettok();
    }

    if (LastChar == '-' || LastChar == '+' || LastChar == '*' || LastChar == '/' \
                || LastChar == '%' || LastChar == '>' || LastChar == '<' || LastChar == '=' \
                || LastChar == '!'\
       ) 
    {
        IdentifierStr = LastChar;
        LastChar = getchar();
        if (isdigit(LastChar)) {
            do {
                IdentifierStr += LastChar;
                LastChar = getchar();
            } while (isdigit(LastChar));
            NumVal = stoi(IdentifierStr);
            return TOK_NUMBER;
        }

        while(LastChar == '-' || LastChar == '+' || LastChar == '*' || LastChar == '/' \
                    || LastChar == '%' || LastChar == '>' || LastChar == '<' || LastChar == '=' \
                    || LastChar == '!'\
             )
        {
            IdentifierStr.push_back(LastChar);
            LastChar = getchar();
        }
        Opera = get_bin_op(IdentifierStr);
        if (Opera != op_none) {
            return TOK_OP;
        }
        return TOK_UNDEFINED;
    }

    if (LastChar == EOF)
    {
        return TOK_EOF;
    }

    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}


//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

/// ExprAST - Base class for all expression nodes.
class ExprAST {
    public:
        virtual ~ExprAST() {}
        virtual Value *codegen() = 0;
};

/// VariableExprAST - Expression class for referencing a variable.
// a0 ~ a5
class VariableExprAST : public ExprAST {
    string str;

    public:
    VariableExprAST(string STR) : str(STR) {}
    Value *codegen()
    {
        return NamedValues[str];
    }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
    int Op;
    std::unique_ptr<ExprAST> SLHS, SRHS;
    public:
    BinaryExprAST(int op, std::unique_ptr<ExprAST> SLHS, std::unique_ptr<ExprAST> SRHS) 
        : Op(op), SLHS(std::move(SLHS)), SRHS(std::move(SRHS)) {}
    Value* codegen()
    {
        Value* V = nullptr, * LHS = SLHS->codegen(), * RHS = SRHS->codegen();

        if(LHS && RHS)
        {
            switch(Op)
            {
                case op_add:
                    V = Builder.CreateAdd(LHS, RHS, "ADDTMP");
                    break;
                case op_sub:
                    V = Builder.CreateSub(LHS, RHS, "SUBTMP");
                    break;
                case op_mul:
                    V = Builder.CreateMul(LHS, RHS, "MULTMP");
                    break;
                case op_div:
                    V = Builder.CreateSDiv(LHS, RHS, "DIVTMP");
                    break;
                case op_mod:
                    V = Builder.CreateSRem(LHS, RHS, "MODTMP");
                    break;
                case op_gt:
                    V = Builder.CreateICmpSGT(LHS, RHS, "GTTMP");
                    break;
                case op_ge:
                    V = Builder.CreateICmpSGE(LHS, RHS, "GETMP");
                    break;
                case op_lt:
                    V = Builder.CreateICmpSLT(LHS, RHS, "LTTMP");
                    break;
                case op_le:
                    V = Builder.CreateICmpSLE(LHS, RHS, "LETMP");
                    break;
                case op_eq:
                    V = Builder.CreateICmpEQ(LHS, RHS, "EQTMP");
                    break;
                case op_ne:
                    V = Builder.CreateICmpNE(LHS, RHS, "NETMP");
                    break;
                default:
                    break;
            }
        }
        return V;
    }
};

/// NumberExprAST - Expression class for number.
class NumberExprAST : public ExprAST {
    long long Val;

    public:
    NumberExprAST(long long Val) : Val(Val) {}
    Value *codegen() 
    {
        return ConstantInt::get(C, APInt(64, Val, true));
    }
};

/// BoolExprAST - Expression class for bool.
class BoolExprAST : public ExprAST {
    bool Bool;

    public:
    BoolExprAST(bool Val) : Bool(Val) {}
    Value *codegen()
    {
        return Bool ? ConstantInt::get(C, APInt(64, 1)) : ConstantInt::get(C, APInt(64, 0));
    }
};

/// BRExprAST - Expression class for if statement
class BRExprAST : public ExprAST {
    std::unique_ptr<ExprAST> St, Br1, Br2;

    public:
    BRExprAST(std::unique_ptr<ExprAST> St, std::unique_ptr<ExprAST> Br1, std::unique_ptr<ExprAST> Br2)
        : St(std::move(St)), Br1(std::move(Br1)), Br2(std::move(Br2)) {}
    Value* codegen()
    {
        // TODO: codegen for Branch expresstion
        return nullptr;
    }
};

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;

static int getNextToken() 
{ 
    return CurTok = gettok(); 
}

static std::unique_ptr<ExprAST> MainLoop();

static std::unique_ptr<ExprAST> ParseBRExpr()
{
    std::unique_ptr<ExprAST> ret = nullptr;
    getNextToken(); 
    // TODO: Branch expression
    return ret;
}

static std::unique_ptr<ExprAST> ParseOPExpr()
{
    std::unique_ptr<ExprAST> ret = nullptr;
    int Cur_OP = Opera;
    getNextToken();
    // TODO: OP expression
    return ret;
}

static std::unique_ptr<ExprAST> MainLoop() {
    static int left_bracket = 0;
    static std::unique_ptr<ExprAST> DFS = nullptr, result = nullptr;
    while (CurTok != TOK_EOF) 
    {
        switch (CurTok) 
        {
            case TOK_BOOL:
                result = std::move(make_unique<BoolExprAST>(BoolVal));
                getNextToken();
                break;
            case TOK_NUMBER:
                result = std::move(make_unique<NumberExprAST>(NumVal));
                getNextToken();
                break;
            case TOK_VAR:
                assert(IdentifierStr == "a0" || IdentifierStr == "a1" || IdentifierStr == "a2" || IdentifierStr == "a3" || IdentifierStr == "a4" || IdentifierStr == "a5");

                result = make_unique<VariableExprAST>(IdentifierStr);
                getNextToken();
                break;
            case TOK_LBRA:
                ++left_bracket;
                getNextToken();
                if(CurTok == TOK_BR)
                {
                    DFS = std::move(ParseBRExpr());
                }
                else if(CurTok == TOK_OP)
                {
                    DFS = std::move(ParseOPExpr());
                }
                else{/*This Branch should not be triggled*/}
                break;
            case TOK_RBRA:
                if (left_bracket > 0) {
                    --left_bracket;
                    getNextToken();
                    result = std::move(DFS);
                }
            default:
                break;
        }

        if(result)
        {
            Value* V = result->codegen();
            Builder.CreateRet(V);
        }
    }


    return nullptr;
}

/////////////////////////////////////////////////////////////
/// Compile

static void initHashTable()
{
    ump_op_type[str_add] = op_add;
    ump_op_type[str_sub] = op_sub;
    ump_op_type[str_mul] = op_mul;
    ump_op_type[str_div] = op_div;
    ump_op_type[str_mod] = op_mod;
    ump_op_type[str_gt] = op_gt;
    ump_op_type[str_ge] = op_ge;
    ump_op_type[str_lt] = op_lt;
    ump_op_type[str_le] = op_le;
    ump_op_type[str_eq] = op_eq;
    ump_op_type[str_neq] = op_ne;
}

static int compile() {
    M->setTargetTriple(llvm::sys::getProcessTriple());
    std::vector<Type *> SixInts(6, Type::getInt64Ty(C));
    FunctionType *FT = FunctionType::get(Type::getInt64Ty(C), SixInts, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, "f", &*M);
    BasicBlock *BB = BasicBlock::Create(C, "entry", F);
    Builder.SetInsertPoint(BB);


    // parse the source program
    // generate correct LLVM instead of just an empty function
    for (auto &Arg : F->args()) {
        NamedValues[Arg.getName()] = &Arg;
    }

    initHashTable();

    // Start Parse
    getNextToken();
    MainLoop();
    M->dump();
    return 0;

    //Value *RetVal = ConstantInt::get(C, APInt(64, 0));
    //Builder.CreateRet(RetVal);
    //assert(!verifyModule(*M, &outs()));
    //return 0;
}

int main(void) { return compile(); }
