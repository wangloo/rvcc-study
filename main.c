#include "rvcc.h"

// 局部和全局变量的域
typedef struct VarScope VarScope;
struct VarScope {
  VarScope *next; // 下一个变量域
  char *name;     // 变量域名称
  Obj *var;       // 对应的变量
};

typedef struct TagScope TagScope;
struct TagScope {
  TagScope *next; // 下一标签域
  char *name;     // 域名城
  Type *ty;       // 域类型
};

// 表示一个块域
typedef struct Scope Scope;
struct Scope {
  Scope *next;    // 指向上一级的域

  // C有两个域：变量域，结构体标签域
  TagScope *tags; // 指向当前域内的结构体标签
  VarScope *vars; // 指向当前域内的变量
};
// 在解析时，全部的变量实例都被累加到这个列表里。
Obj *Locals;  // 局部变量
Obj *Globals; // 全局变量

// 所有域的链表
static Scope *Scp = &(Scope){};

static Obj *findvar(Token *tok)
{
  // 此处越先匹配的域，越深层
  for (Scope *s = Scp; s; s = s->next)
    // 遍历域内的所有变量
    for (VarScope *s2 = s->vars; s2; s2 = s2->next)
      if (equal(tok, s2->name))
        return s2->var;
  return NULL;
}

// 进入域
static void enter_scope(void) {
  Scope *s = calloc(1, sizeof(Scope));
  // 后来的在链表头部
  s->next = Scp;
  Scp = s;
}
// 结束当前域
static void leave_scope(void) {
  Scp = Scp->next;
  // 没释放资源吗?
}


// 将变量存入当前的域中
static VarScope *push_scope(char *name, Obj *var) {
  VarScope *s = calloc(1, sizeof(VarScope));
  s->name = name;
  s->var = var;
  // 后来的在链表头部；
  s->next = Scp->vars;
  Scp->vars = s;
  return s;
}


// 在链表里新增一个局部变量
static Obj *new_local(char *name, Type *ty)
{
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->next = Locals;
  var->ty = ty;
  var->is_local = true;
  Locals = var;
  push_scope(name, var);
  return var;
}

// 在链表里新增一个全局变量
static Obj *new_global(char *name, Type *ty)
{
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->next = Globals;
  var->ty = ty;
  var->is_local = false;
  Globals = var;
  push_scope(name, var);
  return var;
}

// 获取标识符
static char *get_ident(Token *tok)
{
  if (tok->kind != TK_IDENT)
    errorTok(tok, "expected an identifier");
  return strndup(tok->loc, tok->len);
}

// 新建一个二叉树节点
// 某些类型的Node是需要left和right的，比如说+-
static Node *newbinary(NodeKind kind, Node *left, Node *right, Token *tok)
{
  Node *nd = calloc(1, sizeof(Node));
  nd->kind = kind;
  nd->left = left;
  nd->right = right;
  nd->tok = tok;
  return nd;
}

// 新建一个节点，不需要孩子
// 可能通过Node其他成员来维护/访问
// 比如说 compound_stmt ==> "{"
static Node *newnode(NodeKind kind, Token *tok)
{
  return newbinary(kind, NULL, NULL, tok);
}


static Node *newnum(int64_t val, Token *tok)
{
  Node *nd = newnode(ND_NUM, tok);
  nd->val = val;
  return nd;
}

static Node *newvar(Obj *var, Token *tok)
{
  Node *nd = newbinary(ND_VAR, NULL, NULL, tok);
  nd->var = var;
  return nd;
}

static Node *newadd(Node *left, Node *right, Token *tok)
{
  // 为左右部添加类型
  // 在stmt之后确实会对整个语法树 add_type
  // 但是在stmt解析的过程中，就会调用new_sub/new_add，
  // 这里面就会需要这个加减法子树的type树，所以在这里添加
  add_type(left);
  add_type(right);

  // num + num
  if (is_integer(left->ty) && is_integer(right->ty))
    return newbinary(ND_ADD, left, right, tok);

  // 不能解析 ptr + ptr
  if (left->ty->base && right->ty->base)
    errorTok(tok, "invalid operands");

  // 将 num + ptr 转换为 ptr + num
  if (is_integer(left->ty) && right->ty->base) {
    Node *tmp = left;
    left = right;
    right = tmp;
  }

  // ptr + num
  // 指针加法， ptr+1，不是1个字节，而是一个元素的空间，所以需要 *size 操作
  right = newbinary(ND_MUL, right, newnum(left->ty->base->size, tok), tok);
  return newbinary(ND_ADD, left, right, tok);
}

static Node *newsub(Node *left, Node *right, Token *tok)
{
  // 为左右部添加类型
  // 在stmt之后确实会对整个语法树 add_type
  // 但是在stmt解析的过程中，就会调用new_sub/new_add，
  // 这里面就会需要这个加减法子树的type树，所以在这里添加
  add_type(left);
  add_type(right);

  // num - num
  if (is_integer(left->ty) && is_integer(right->ty))
    return newbinary(ND_SUB, left, right, tok);

  // ptr - num
  if (left->ty->kind == TY_PTR && is_integer(right->ty)) {
    right = newbinary(ND_MUL, right, newnum(left->ty->base->size, tok), tok);
    add_type(right);
    Node *nd = newbinary(ND_SUB, left, right, tok);
    // 节点类型为指针
    nd->ty = left->ty;
    return nd;
  }

  // ptr - ptr，返回两指针之间有多少元素
  if (left->ty->kind == TY_PTR
      && right->ty->kind == TY_PTR) {
    Node *nd = newbinary(ND_SUB, left, right, tok);
    nd->ty = TyInt;
    return newbinary(ND_DIV, nd, newnum(left->ty->base->size, tok), tok);
  }
  errorTok(tok, "invalid operands");
  return NULL;
}


static bool is_typename(Token *tok) {
  return equal(tok, "int") || equal(tok, "char") || equal(tok, "short") ||
         equal(tok, "long") || equal(tok, "void") || equal(tok, "struct") ||
         equal(tok, "union");
}

// 新增唯一名称
static char *new_unique_name(void)
{
  static int id = 0;
  return format(".L..%d", id++);
}

// 新增匿名全局变量
static Obj *new_anon_global(Type *ty)
{
  return new_global(new_unique_name(), ty);
}

// 新增字符串字面量
static Obj *new_string_literal(char *str, Type *ty)
{
  Obj *var = new_anon_global(ty);
  var->initdata = str;
  return var;
}

static Type *findtag(Token *tok) {
  for (Scope *s = Scp; s; s = s->next)
    for (TagScope *s2 = s->tags; s2; s2 = s2->next)
      if (equal(tok, s2->name))
        return s2->ty;
  return NULL;
}

static void push_tagscope(Token *tok, Type *ty) {
  TagScope *s = calloc(1, sizeof(TagScope));
  s->name = strndup(tok->loc, tok->len);
  s->ty = ty;
  s->next = Scp->tags;
  Scp->tags = s;
}



static Node *struct_ref(Node *left, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);

// program = (functionDefinition | globalVariable)*
// functionDefinition = declspec declarator (";" | "{" compoundStmt)
// globalVariable = declspec declarator
// compoundStmt = (declaration | stmt*) "}"
// declaration =
//        declspec (declarator ("=" assign)? ("," declarator ("=" assign)?)*)? ";"
// declspec = ("void" | "int" | "long" | "short" | "char" | structDecl | unionDecl)+
// structDecl = structUnionDecl
// unionDecl = structUnionDecl
// structUnionDecl = ident? ("{" struct Members)?
// declarator = "*"* ("(" declarator ")" | ident) typeSuffix
// typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
// stmt = ("return") expr ";"
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | expr? ";"
//        | "{" compoundStmt
// exprStmt = expt? ";"
// expr = assign ("," expr)?
// assign = equality ("=" assign)?
// equality = add ("<" add | ">" add | "<=" add | ">=" add | "!=" add | "==" add)
// add = mul ("+" mul | "-" mul)
// mul = unary ("*" unary | "/" unary)
// unary = ("+" | "-" | "&" | "*") unary | postfix
// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
// primary = "(" "{" stmt+ "}" ")"
//          | "(" expr ")"
//          | ident func-args?
//          | num
//          | str
//          | "sizeof" unary
// funcall = ident "(" (assign ("," assign)*)? ")"
static Token *function(Token *tok, Type *base);
static Token *global_variable(Token *tok, Type *base);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *declaration(Token **rest, Token *tok);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// declspec = ("void" | "int" | "long" | "short" | "char" | structDecl | unionDecl)+
// declarator specifier
static Type *declspec(Token **rest, Token *tok)
{
  // 类型的组合，被表示为例如：LONG+LONG=1<<9
  // 可知 long int 和 int long 是等价的
  enum {
    VOID = 1 << 0,
    CHAR = 1 << 2,
    SHORT = 1 << 4,
    INT = 1 << 6,
    LONG = 1 << 8,
    OTHER = 1 << 10,
  };
  Type *ty = TyInt;
  int counter = 0; // 记录类型相加的值

  // 遍历所有类型的 tok
  while (is_typename(tok)) {
    if (equal(tok, "struct") || equal(tok, "union")) {
      // structDecl
      if (equal(tok, "struct"))
        ty = struct_decl(&tok, tok->next);
      // unionDecl
      else
        ty = union_decl(&tok, tok->next);
      counter += OTHER;
      continue;
    }

    // 对于出现的类型名加入 counter
    // 每一步的counter都需要有合法值
    // "void"
    if (equal(tok, "void"))
      counter += VOID;
    // "char"
    else if (equal(tok, "char"))
      counter += CHAR;
    // "short"
    else if (equal(tok, "short"))
      counter += SHORT;
    // "int"
    else if (equal(tok, "int"))
      counter += INT;
    // "long"
    else if (equal(tok, "long"))
      counter += LONG;
    else
      unreachable();

    // 根据 counter 值映射到对应的 Type
    switch (counter) {
    case VOID:
      ty = TyVoid;
      break;
    case CHAR:
      ty = TyChar;
      break;
    case SHORT:
    case SHORT + INT:
      ty = TyShort;
      break;
    case INT:
      ty = TyInt;
      break;
    case LONG:
    case LONG + INT:
      ty = TyLong;
      break;
    default:
      errorTok(tok, "invalid type");

    }
    tok = tok->next;
  }
  *rest = tok;
  return ty;
}

// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty)
{
  // 存储形参的链表
  Type head = {};
  Type *cur = &head;

  while (!equal(tok, ")")) {
    // param ("," param)*
    if (cur != &head)
      tok = skip(tok, ",");
    Type *basety = declspec(&tok, tok);
    Type *declarty = declarator(&tok, tok, basety);
    cur->next = copytype(declarty);
    cur = cur->next;
  }
  // 封装一个函数节点
  ty = functype(ty);
  // 传递形参
  ty->params = head.next;
  *rest = tok->next;
  return ty;
}

static bool is_function(Token *tok)
{
  if (equal(tok, ";"))
    return false;

  // 虚设变量，用于调用declarator
  Type dummy = {};
  Type *ty = declarator(&tok, tok, &dummy);
  return ty->kind == TY_FUNC;
}

// typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty)
{
  // "(" funcParams
  if (equal(tok, "(")) {
    return func_params(rest, tok->next, ty);
  }
  // "[" num "]"
  if (equal(tok, "[")) {
    tok = tok->next;
    if (tok->kind != TK_NUM)
      errorTok(tok, "expected a number");
    int sz = tok->val;
    tok = skip(tok->next, "]");
    ty = type_suffix(rest, tok, ty);
    return arrayof(ty, sz);
  }
  // ε
  *rest = tok;
  return ty;
}

// declarator = "*"* ("(" declarator ")" | ident) typeSuffix
static Type *declarator(Token **rest, Token *tok, Type *ty)
{
  // "*"*
  // 构建所有的（多重）指针
  while (consume(&tok, tok, "*"))
    ty = pointerto(ty);

  // "(" declarator ")"
  if (equal(tok, "(")) {
    // 记录 "(" 的位置
    Token *start = tok;
    Type dummy = {};
    // 使tok 前进到")"之后的位置
    declarator(&tok, start->next, &dummy);
    tok = skip(tok, ")");
    // 获取到")"后面的类型后缀，ty为解析完的类型，rest指向分号
    ty = type_suffix(rest, tok, ty);
    // 解析ty整体作为base去构造，返回Type类型
    return declarator(&tok, start->next, ty);
  }

  if (tok->kind != TK_IDENT)
    errorTok(tok, "expected a variable name");

  // typeSuffix
  ty = type_suffix(rest, tok->next, ty);

  // ident
  // 变量名 or 函数名
  ty->name = tok;
  return ty;
}

// ident "(" (assign ("," assign)*)? ")"
static Node *funcall(Token **rest, Token *tok)
{
  Node head = {};
  Node *cur = &head;
  Token *start = tok;

  tok = tok->next->next;
  while (!equal(tok, ")")) {
    if (cur != &head)
      tok = skip(tok, ",");
    cur->next = assign(&tok, tok);
    cur = cur->next;
  }

  Node *nd = newnode(ND_FUNCALL, tok);
  nd->func_name = strndup(start->loc, start->len);
  nd->args = head.next;
  *rest = skip(tok, ")");
  return nd;
}


// 语法分析入口函数
// program = (functionDefinition | globalVariable)*
Obj *parse(Token **rest, Token *tok)
{
  Globals = NULL;

  while (tok->kind != TK_EOF) {
    Type *basety = declspec(&tok, tok);

    // 函数
    if (is_function(tok)) {
      tok = function(tok, basety);
      continue;
    }

    // 全局变量
    tok = global_variable(tok, basety);
  }
  *rest = tok;
  return Globals;
}

static void create_param_lvars(Type *param)
{
  if (param) {
    // 递归到形参最底部
    // 现将最底部的加入Locals中，之后的都逐个加入到顶部，保持顺序不变
    create_param_lvars(param->next);
    // 添加到Locals中
    new_local(get_ident(param->name), param);
  }
}

// functionDefinition = declspec declarator (";" | "{" compoundStmt)
static Token *function(Token *tok, Type *base)
{
  Type *ty = declarator(&tok, tok, base);

  Obj *fn = new_global(get_ident(ty->name), ty);
  fn->is_function = true;
  fn->is_definition = !consume(&tok, tok, ";");

  // 判断是否没有函数定义
  if (!fn->is_definition)
    return tok;

  // 清空全局变量Locals
  Locals = NULL;
  // 进入新的域
  enter_scope();


  // 函数参数
  create_param_lvars(ty->params);
  fn->params = Locals;

  // "{"
  tok = skip(tok, "{");

  // 函数题存储语句的AST，locals存储变量
  fn->body = compound_stmt(&tok, tok);
  fn->locals = Locals;
  // 结束当前域
  leave_scope();
  return tok;
}

// globalVariable = declspec declarator
static Token *global_variable(Token *tok, Type *base)
{
  bool first = true;

  while (!consume(&tok, tok, ";")) {
    if (!first)
      tok = skip(tok, ",");
    first = false;
    Type *ty = declarator(&tok, tok, base);
    new_global(get_ident(ty->name), ty);
  }
  return tok;
}


// declaration =
//        declspec (declarator ("=" assign)? ("," declarator ("=" assign)?)*)? ";"
static Node *declaration(Token **rest, Token *tok)
{
  // declspec
  // 声明的 基础类型
  Type *basety = declspec(&tok, tok);

  Node head = {};
  Node *cur = &head;
  // 对变量声明次数的计数
  int i = 0;

  // (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
  while (!equal(tok, ";")) {
    // 第1个变量不必匹配 ","
    if (i++ > 0)
      tok = skip(tok, ",");

    // declarator
    Type *ty = declarator(&tok, tok, basety);
    if (ty->kind == TY_VOID)
      errorTok(tok, "variable declared void");
    Obj *var = new_local(get_ident(ty->name), ty);

    // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
    if (!equal(tok, "="))
      continue;

    // 解析"="后面的token
    Node *left = newvar(var, ty->name);
    // 解析递归赋值语句
    // tok->next 跳过 "="
    Node *right = assign(&tok, tok->next);
    Node *node = newbinary(ND_ASSIGN, left, right, tok);
    // 存放在表达式语句中
    cur->next = newbinary(ND_EXPR_STMT, NULL, node, tok);
    cur = cur->next;
  }

  // 将所有表达式语句，存放在代码块中
  Node *nd = newnode(ND_BLOCK, tok);
  nd->body = head.next;
  *rest = tok->next;
  return nd;
}

// compoundStmt = (declaration | stmt*) "}"
static Node *compound_stmt(Token **rest, Token *tok)
{
  Node head = {};
  Node *cur = &head;

  // 进入新的域
  enter_scope();

  // stmt*
  while (!equal(tok, "}")) {
    // declaration
    if (is_typename(tok))
      cur->next = declaration(&tok, tok);
    // stmt
    else
      cur->next = stmt(&tok, tok);
    cur = cur->next;
    // 构造完AST后，为节点添加类型信息
    add_type(cur);
  }

  // 结束当前的域
  leave_scope();

  Node *nd = newnode(ND_BLOCK, tok);
  nd->body = head.next;
  *rest = tok->next;
  return nd;
}

// 解析表达式语句
// stmt = ("return") expr ";"
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | expr? ";"
//        | "{" compoundStmt
static Node *stmt(Token **rest, Token *tok)
{
  // "while" "(" expr ")" stmt
  if (equal(tok, "while")) {
    Node *nd = newnode(ND_FOR, tok);
    tok = skip(tok->next, "(");
    // cond
    nd->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    nd->then = stmt(&tok, tok);
    *rest = tok;
    return nd;
  }
  // "for" "(" exprStmt expr? ";" expr? ")" stmt
  if (equal(tok, "for")) {
    Node *nd = newnode(ND_FOR, tok);
    tok = skip(tok->next, "(");
    // init
    // init的处理比较特殊，for 循环的init是一条statement，
    // 后两个仅仅是语句。不用判空，因为在内部会判断
    nd->init = expr_stmt(&tok, tok);
    // cond
    if (!equal(tok, ";")) {
      nd->cond = expr(&tok, tok);
    }
    tok = skip(tok, ";");
    // inc
    if (!equal(tok, ")")) {
      nd->inc = expr(&tok, tok);
    }

    tok = skip(tok, ")");
    nd->then = stmt(&tok, tok);
    *rest = tok;
    return nd;
  }
  // "if" "(" expr ")" stmt ("else" stmt)?
  if (equal(tok, "if")) {
    Node *nd = newnode(ND_IF, tok);
    tok = skip(tok->next, "(");
    nd->cond = expr(&tok, tok);
    tok = skip(tok, ")");
    nd->then = stmt(&tok, tok);
    // "else" stmt
    if (equal(tok, "else")) {
      nd->els = stmt(&tok, tok->next);
    }
    *rest = tok;
    return nd;
  }

  // "{" compoundStmt
  if (equal(tok, "{")) {
    Node *nd = newnode(ND_BLOCK, tok);
    nd->body = compound_stmt(rest, tok->next);
    return nd;
  }
  // "return" expr ";"
  if (equal(tok, "return")) {
    Node *nd = newbinary(ND_RETURN, NULL, expr(&tok, tok->next), tok);
    *rest = skip(tok, ";");
    return nd;
  }
  // 空语句判断
  if (equal(tok, ";")) {
    *rest = tok->next;
    // 这里用一个body成员为空的ND_BLOCK来表示空语句，
    // 因为在处理ND_BLOCK时会遍历body，空的话则不会产生影响
    return newnode(ND_BLOCK, tok);
  }
  Node *nd = newbinary(ND_EXPR_STMT, NULL, expr(&tok, tok), tok);
  *rest = skip(tok, ";");
  return nd;
}

// 解析表达式语句
// exprStmt = expr? ";"
static Node *expr_stmt(Token **rest, Token *tok)
{
  // ";"
  if (equal(tok, ";")) {
    *rest = tok->next;
    return newnode(ND_BLOCK, tok);
  }

  // expr ";"
  Node *nd = newbinary(ND_EXPR_STMT, NULL, expr(&tok, tok), tok);
  *rest = skip(tok, ";");
  return nd;
}

// expr = assign ("," expr)?
static Node *expr(Token **rest, Token *tok) {
  Node *nd = assign(&tok, tok);

  // ("," expr)?
  if (equal(tok, ","))
    return newbinary(ND_COMMA, nd, expr(rest, tok->next), tok);

  *rest = tok;
  return nd;
}

// 解析赋值
// assign = equality ("=" assign)?
static Node *assign(Token **rest, Token *tok)
{
  Node *nd = equality(&tok, tok);

  // 可能存在递归赋值，如a=b=1
  // ("=" assign)
  if (equal(tok, "=")) {
    nd = newbinary(ND_ASSIGN, nd, assign(&tok, tok->next), tok);
  }

  *rest = tok;
  return nd;
}

// 解析条件运算符
// equality = add ("<" add | ">" add | "<=" add | ">=" add | "!=" add | "==" add)
static Node *equality(Token **rest, Token *tok)
{
  // add
  Node *nd = add(&tok, tok);

  // ("<" add | ">" add | "<=" add | ">=" add | "!=" add | "==" add)
  while (1) {
    // "<" add
    if (equal(tok, "<")) {
      nd = newbinary(ND_LT, nd, add(&tok, tok->next), tok);
      continue;
    }
    // ">" add ==> 改变孩子的左右顺序转换成 "<" 的情况
    if (equal(tok, ">")) {
      nd = newbinary(ND_LT, add(&tok, tok->next), nd, tok);
      continue;
    }
    // "<=" add
    if (equal(tok, "<=")) {
      nd = newbinary(ND_LE, nd, add(&tok, tok->next), tok);
      continue;
    }
    // ">=" add ==> 改变孩子的左右顺序转换成 "<=" 的情况
    if (equal(tok, ">=")) {
      nd = newbinary(ND_LE, add(&tok, tok->next), nd, tok);
      continue;
    }
    // "!=" add
    if (equal(tok, "!=")) {
      nd = newbinary(ND_NE, nd, add(&tok, tok->next), tok);
      continue;
    }
    // "==" add
    if (equal(tok, "==")) {
      nd = newbinary(ND_EQ, nd, add(&tok, tok->next), tok);
      continue;
    }

    *rest = tok;
    return nd;
  }

}

// 解析加减
// add = mul ("+" mul | "-" mul)
static Node *add(Token **rest, Token *tok)
{
  // mul
  Node *nd = mul(&tok, tok);

  // ("+" mul | "-" mul)
  while (1) {
    // "+" mul
    if (equal(tok, "+")) {
      nd = newadd(nd, mul(&tok, tok->next), tok);
      continue;
    }
    // "-" mul
    if (equal(tok, "-")) {
      nd = newsub(nd, mul(&tok, tok->next), tok);
      continue;
    }

    *rest = tok;
    return nd;
  }
}
// 解析乘除
// mul = primary( "*" primary | "/" primary)
static Node *mul(Token **rest, Token *tok)
{
  Node *nd = unary(&tok, tok);

  // ("*" primary | "/" primary)
  while (1) {
    // "*" primiary
    if (equal(tok, "*")) {
      nd = newbinary(ND_MUL, nd, unary(&tok, tok->next), tok);
      continue;
    }

    // "/" primary
    if (equal(tok, "/")) {
      nd = newbinary(ND_DIV, nd, unary(&tok, tok->next), tok);
      continue;
    }

    *rest = tok;
    return nd;
  }
}

// unary = ("+" | "-" | "&" | "*") unary | primary
static Node *unary(Token **rest, Token *tok)
{
  Node *nd = NULL;

  // "+" unary
  if (equal(tok, "+")) {
    return unary(rest, tok->next);
  }
  // "-" unary
  if (equal(tok, "-")) {
    nd = newbinary(ND_NEG, NULL, unary(rest, tok->next), tok);
    return nd;
  }
  // "&" unary
  if (equal(tok, "&")) {
    nd = newbinary(ND_ADDR, NULL, unary(rest, tok->next), tok);
    return nd;
  }
  // "*" unary
  if (equal(tok, "*")) {
    nd = newbinary(ND_DEREF, NULL, unary(rest, tok->next), tok);
    return nd;
  }

  return postfix(rest, tok);
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
static Node *postfix(Token **rest, Token *tok)
{
  // primary
  Node *nd = primary(&tok, tok);

  while (true) {
    // ("[" expr "]")*
    // x[y] 等价于 *(x+y)
    // x[y][z] ==> *(*(x+y)+z)
    if (equal(tok, "[")) {
      Node *idx = expr(&tok, tok->next);
      tok = skip(tok, "]");
      nd = newbinary(ND_DEREF, NULL, newadd(nd, idx, tok), tok);
      continue;
    }

    // "." indent
    if (equal(tok, ".")) {
      nd = struct_ref(nd, tok->next);
      tok = tok->next->next;
      continue;
    }
    // "->" ident
    if (equal(tok, "->")) {
      // x->y 等价于 (*x).y
      nd = newbinary(ND_DEREF, NULL, nd, tok);
      nd = struct_ref(nd, tok->next);
      tok = tok->next->next;
      continue;
    }
    *rest = tok;
    return nd;
  }

}


// 解析括号、数字、变量
// primary = "(" "{" stmt+ "}" ")"
//          | "(" expr ")"
//          | ident func-args?
//          | num
//          | str
//          | "sizeof" unary
static Node *primary(Token **rest, Token *tok)
{
  // "(" "{" stmt+ "}" ")"
  if (equal(tok, "(") && equal(tok->next, "{")) {
    // This is a GNU statement expression.
    Node *nd = newnode(ND_STMT_EXPR, tok);
    nd->body = compound_stmt(&tok, tok->next->next)->body;
    *rest = skip(tok, ")");
    return nd;
  }

  // "(" expr ")"
  if (equal(tok, "(")) {
    Node *nd = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return nd;
  }
  // ident args?
  if (tok->kind == TK_IDENT) {
    // 函数调用
    // args = "(" ")"
    if (equal(tok->next, "(")) {
      return funcall(rest, tok);
    } else {
      Obj *var = findvar(tok);
      if (!var) {
        // 未声明就使用变量，报错
        errorTok(tok, "undefined variable");
      }
      *rest = tok->next;
      return newvar(var, tok);
    }
  }
  // num
  if (tok->kind == TK_NUM) {
    Node *nd = newnum(tok->val, tok);
    *rest = tok->next;
    return nd;
  }
  if (tok->kind == TK_STR) {
    Obj *var = new_string_literal(tok->str, tok->ty);
    *rest = tok->next;
    return newvar(var, tok);
  }

  // "sizeof" unary
  if (equal(tok, "sizeof")) {
    Node *nd = unary(&tok, tok->next);
    add_type(nd);
    *rest = tok;
    return newnum(nd->ty->size, tok);
  }

  error("unexpected char '%c'\n", tok->val);
  return NULL;
}


// structMembers = (declspec declarator ("," declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head = {};
  Member *cur = &head;

  while (!equal(tok, "}")) {
    // declspec
    Type *basety = declspec(&tok, tok);
    int first = true;

    while (!consume(&tok, tok, ";")) {
      if (!first)
        tok = skip(tok, ",");
      first = false;

      Member *mem = calloc(1, sizeof(Member));
      // declarator
      mem->ty = declarator(&tok, tok, basety);
      mem->name = mem->ty->name;
      cur = cur->next = mem;
    }
  }

  *rest = tok->next;
  ty->mems = head.next;
}

// structUnionDecl = ident? ("{" struct Members)?
static Type *struct_union_decl(Token **rest, Token *tok) {
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    Type *ty = findtag(tag);
    if (!ty)
      errorTok(tok, "unknown struct type");
    *rest = tok;
    return ty;
  }

  // 构造一个结构体
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_STRUCT;
  struct_members(rest, tok->next, ty);
  ty->align = 1;

  // 如果有名就注册结构体类型
  if (tag)
    push_tagscope(tag, ty);
  return ty;
}

// structDecl = structUnionDecl
static Type *struct_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TY_STRUCT;

  // 结构体内成员的偏移量
  int offset = 0;
  for (Member *mem = ty->mems; mem; mem = mem->next) {
    mem->offset = offset;
    offset += mem->ty->size;

    if (ty->align < mem->ty->align)
      ty->align = mem->ty->align;
  };
  ty->size = align_to(offset, ty->align);
  return ty;
}

// unionDecl = structUnionDecl
static Type *union_decl(Token **rest, Token *tok) {
  Type *ty = struct_union_decl(rest, tok);
  ty->kind = TY_UNION;

  // 联合体需要设置为最大的对其量与大小，变量偏移量都默认为0
  for (Member *mem = ty->mems; mem; mem = mem->next) {
    mem->offset = 0;
    if (ty->align < mem->ty->align)
      ty->align = mem->ty->align;
    if (ty->size < mem->ty->size)
      ty->size = mem->ty->size;
  }
  // 将大小对齐
  ty->size = align_to(ty->size, ty->align);
  return ty;
}

// 获取结构体成员
static Member *get_struct_member(Type *ty, Token *tok) {
  for (Member *mem = ty->mems; mem; mem = mem->next) {
    if (mem->name->len == tok->len &&
        !strncmp(mem->name->loc, tok->loc, tok->len))
      return mem;
  }
  errorTok(tok, "no such member");
  return NULL;
}

// 构建结构体成员的节点
static Node *struct_ref(Node *left, Token *tok) {
  add_type(left);
  if (left->ty->kind != TY_STRUCT && left->ty->kind != TY_UNION)
    errorTok(left->tok, "not a struct or union");

  Node *nd = newbinary(ND_MEMBER, NULL, left, tok);
  nd->mem = get_struct_member(left->ty, tok);
  return nd;
}


// 目标文件的路径
static char *OptO;
// 输入文件的路径
static char *InputPath;

// 输出程序的使用说明
static void usage(int status)
{
  fprintf(stderr, "rvcc [-o <path> ] <file>\n");
  exit(status);
}

// 解析传入程序的参数
static void parse_args(int argc, char **argv) {
  // 遍历所有传入程序的参数
  for (int i = 1; i < argc; i++) {
    // 如果存在help，则直接显示用法说明
    if (!strcmp(argv[i], "--help"))
      usage(0);

    // 解析-o XXX的参数
    if (!strcmp(argv[i], "-o")) {
      // 不存在目标文件则报错
      if (!argv[++i])
        usage(1);
      // 目标文件的路径
      OptO = argv[i];
      continue;
    }

    // 解析-oXXX的参数
    if (!strncmp(argv[i], "-o", 2)) {
      // 目标文件的路径
      OptO = argv[i] + 2;
      continue;
    }

    // 解析-的参数
    if (argv[i][0] == '-' && argv[i][1] != '\0')
      error("unknow argument: %s", argv[i]);

    // 其他情况则匹配为输入文件
    InputPath = argv[i];
  }
  // 未解析道输入文件时报错
  if (!InputPath)
    error("no input files");
}

static FILE *openfile(char *path)
{
  if (!path || !strcmp(path, "-"))
    return stdout;

  // 以写入模式打开文件
  FILE *out = fopen(path, "w");
  if (!out)
    error("cannot open output file: %s: %s", path,
      strerror(errno));
  return out;
}

int main(int Argc, char **Argv) {
  // 解析传入程序的参数
  parse_args(Argc, Argv);

  // 词法分析
  Token *tok = tokenize_file(InputPath);

  // 语法分析
  Obj *prog = parse(&tok, tok);
  if (tok->kind != TK_EOF)
    error("extra token, kind: %d\n", tok->kind);

  // 生成代码
  FILE *out = openfile(OptO);
  // .fiule 文件编号 文件名
  fprintf(out, ".file 1 \"%s\"\n", InputPath);
  codegen(prog, out);
  return 0;
}

