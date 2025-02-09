#include "rvcc.h"

// 局部和全局变量 或是typedef, enum常量的域
typedef struct VarScope VarScope;
struct VarScope {
  VarScope *next; // 下一个变量域
  char *name;     // 变量域名称
  Obj *var;       // 对应的变量
  Type *typede;   // 别名
  Type *enumty;   // 枚举的类型
  int enumval;    // 枚举的值
};

// 结构体标签，联合体标签，枚举标签的域
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

  // C有两个域：变量（或类型别名）域，结构体（或联合体、枚举）标签域
  TagScope *tags; // 指向当前域内的结构体标签
  VarScope *vars; // 指向当前域内的变量
};

// 变量属性
typedef struct {
  bool is_typedef; // 是否为类型别名
  bool is_static;  // 是否为文件域内
} VarAttr;

// 在解析时，全部的变量实例都被累加到这个列表里。
Obj *Locals;  // 局部变量
Obj *Globals; // 全局变量

// 当前函数内的goto和标签列表
static Node *Gotos;
static Node *Labels;

static char *BrkLabel;

// 所有域的链表
static Scope *Scp = &(Scope){};
// 指向当前正在解析的函数
static Obj *CurrentFn;

// 通过名称查找变量
static VarScope *findvar(Token *tok) {
  // 此处越先匹配的域，越深层
  for (Scope *s = Scp; s; s = s->next)
    // 遍历域内的所有变量
    for (VarScope *s2 = s->vars; s2; s2 = s2->next)
      if (equal(tok, s2->name))
        return s2;
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
static VarScope *push_scope(char *name) {
  VarScope *s = calloc(1, sizeof(VarScope));
  s->name = name;
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
  push_scope(name)->var = var;
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
  push_scope(name)->var = var;
  return var;
}

// 获取标识符
static char *get_ident(Token *tok)
{
  if (tok->kind != TK_IDENT)
    errorTok(tok, "expected an identifier");
  return strndup(tok->loc, tok->len);
}

// 查找类型别名
static Type *find_typdef(Token *tok) {
  // 类型别名是个标识符
  if (tok->kind == TK_IDENT) {
    // 查找是否存在与变量域内
    VarScope *s = findvar(tok);
    if (s)
      return s->typede;
  }
  return NULL;
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

// 新建一个单叉树
static Node *newunary(NodeKind kind, Node *expr, Token *tok) {
  Node *nd = newnode(kind, tok);
  nd->right = expr;
  return nd;
}

static Node *newnum(int64_t val, Token *tok)
{
  Node *nd = newnode(ND_NUM, tok);
  nd->val = val;
  return nd;
}

static Node *newlong(int64_t val, Token *tok) {
  Node *nd = newnode(ND_NUM, tok);
  nd->val = val;
  nd->ty = TyLong;
  return nd;
}

// 新建一个变量节点
static Node *new_varnode(Obj *var, Token *tok)
{
  Node *nd = newbinary(ND_VAR, NULL, NULL, tok);
  nd->var = var;
  return nd;
}

// 新转换
Node *newcast(Node *expr, Type *ty) {
  add_type(expr);

  Node *nd = calloc(1, sizeof(Node));
  nd->kind = ND_CAST;
  nd->tok = expr->tok;
  nd->left = expr; // left表示之前的类型
  nd->ty = copytype(ty);
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
  right = newbinary(ND_MUL, right, newlong(left->ty->base->size, tok), tok);
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
    right = newbinary(ND_MUL, right, newlong(left->ty->base->size, tok), tok);
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
         equal(tok, "long") || equal(tok, "void") || equal(tok, "_Bool") ||
         equal(tok, "struct") || equal(tok, "union") || equal(tok, "typedef") ||
         equal(tok, "enum") || equal(tok, "static") || find_typdef(tok);
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

// program = ( typedef | functionDefinition | globalVariable)*
// functionDefinition = declspec declarator (";" | "{" compoundStmt)
// globalVariable = declspec declarator
// compoundStmt = (declaration | stmt*) "}"
// declaration =
//        declspec (declarator ("=" assign)? ("," declarator ("=" assign)?)*)? ";"
// declspec = ("void" | "_Bool" | "int" | "long" | "short" | "char"
//             | "typedef" | "static"
//             | structDecl | unionDecl | typedefName)+
//             | enumSpecifier)+
// structDecl = structUnionDecl
// unionDecl = structUnionDecl
// structUnionDecl = ident? ("{" struct Members)?
// declarator = "*"* ("(" declarator ")" | ident) typeSuffix
// typeSuffix = "(" funcParams | "[" arrayDimensions | ε
// arrayDimensions = num? "]" typesuffix
// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
// stmt = ("return") expr ";"
//        | "for" "(" (exprStmt | declspec declarator)  expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "goto" ident ";"
//        | "break" ";"
//        | "ident ":" stmt
//        | expr? ";"
//        | "{" compoundStmt
// exprStmt = expt? ";"
// expr = assign ("," expr)?
// assign = logOr (assignOp assign)?
// logOr = logAnd ("||" logAnd)*
// logAnd = bitOr ("&&" bitOr)*
// bitOr = BitXor ("|" bitXor)*
// bitXor = BitAnd ("^" bitAnd)*
// bitAnd = equality ("&" equality)*
// assignOp = "=" | "+" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
// equality = add ("<" add | ">" add | "<=" add | ">=" add | "!=" add | "==" add)
// add = mul ("+" mul | "-" mul)
// mul = cast ("*" cast | "/" cast | "%" cast)
// cast = "(" typeName ") cast | unary
// unary = ("+" | "-" | "&" | "*" | "!" | "~") cast | ("++" | "--") unary | postfix
// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
// primary = "(" "{" stmt+ "}" ")"
//          | "(" expr ")"
//          | ident func-args?
//          | num
//          | str
//          | "sizeof" unary
//          | "sizeof" "(" typeName ")"
// typeName = declspec abstractDeclarator
// abstractDeclarator = "*"* ("(" abstractDeclarator ")")? typeSuffix
// funcall = ident "(" (assign ("," assign)*)? ")"
static Token *function(Token *tok, Type *base, VarAttr *attr);
static Token *global_variable(Token *tok, Type *base);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *declaration(Token **rest, Token *tok, Type *basety);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Type *enum_specifier(Token **rest, Token *tok);
static Type *type_suffix(Token **rest, Token *tok, Type *ty);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *log_or(Token **rest, Token *tok);
static Node *log_and(Token **rest, Token *tok);
static Node *bit_or(Token **rest, Token *tok);
static Node *bit_xor(Token **rest, Token *tok);
static Node *bit_and(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *cast(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Type *typename(Token  **rest, Token *tok);

// declspec = ("void" | "_Bool" | "int" | "long" | "short" | "char"
//             | "typedef" | "static"
//             | structDecl | unionDecl | typedefName
//             | enumSpecifier)+
// declarator specifier
static Type *declspec(Token **rest, Token *tok, VarAttr *attr)
{
  // 类型的组合，被表示为例如：LONG+LONG=1<<11
  // 可知 long int 和 int long 是等价的
  enum {
    VOID = 1 << 0,
    BOOL = 1 << 2,
    CHAR = 1 << 4,
    SHORT = 1 << 6,
    INT = 1 << 8,
    LONG = 1 << 10,
    OTHER = 1 << 12,
  };
  Type *ty = TyInt;
  int counter = 0; // 记录类型相加的值

  // 遍历所有类型的 tok
  while (is_typename(tok)) {
    // 处理typedef 或 static 关键字
    if (equal(tok, "typedef") || equal(tok, "static")) {
      if (!attr)
        errorTok(tok, "storage class specifier is not allowed in this context");
      if (equal(tok, "typedef"))
        attr->is_typedef = true;
      else
        attr->is_static = true;

      // typedef 和 static 不应一起使用
      if (attr->is_static && attr->is_typedef)
        errorTok(tok, "typedef and static may not be used together");
      tok = tok->next;
      continue;
    }

    // 处理用户定义的关键字
    Type *ty2 = find_typdef(tok);
    if (equal(tok, "struct") || equal(tok, "union") || equal(tok, "enum") ||
        ty2) {
      if (counter)
        break;

      if (equal(tok, "struct")) {
        // structDecl
        ty = struct_decl(&tok, tok->next);
      } else if (equal(tok, "union")) {
        // unionDecl
        ty = union_decl(&tok, tok->next);
      } else if (equal(tok, "enum")) {
        ty = enum_specifier(&tok, tok->next);
      } else {
        // 将类型设为类型别名指向的类型
        ty = ty2;
        tok = tok->next;
      }

      counter += OTHER;
      continue;
    }

    // 对于出现的类型名加入 counter
    // 每一步的counter都需要有合法值
    // "void"
    if (equal(tok, "void"))
      counter += VOID;
    // "_Bool"
    else if (equal(tok, "_Bool"))
      counter += BOOL;
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
    case BOOL:
      ty = TyBool;
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
    case LONG + LONG:
    case LONG + LONG + INT:
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
    Type *ty2 = declspec(&tok, tok, NULL);
    ty2  = declarator(&tok, tok, ty2);

    // T类型的数组被转换为 T*
    if (ty2->kind == TY_ARRAY) {
      Token *name = ty2->name;
      ty2 = pointerto(ty2->base);
      ty2->name = name;
    }
    // 将类型复制到行参链表一份
    cur->next = copytype(ty2);
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


// arrayDimensions = num? "]" typeSuffix
static Type *array_dimensions(Token **rest, Token *tok, Type *ty) {
  // "]" 无数组维度的 "[]"
  if (equal(tok, "]")) {
    ty = type_suffix(rest, tok->next, ty);
    return arrayof(ty, -1);
  }

  // 有数组维度的情况
  if (tok->kind != TK_NUM)
    errorTok(tok, "expected a numer");
  int sz = tok->val;
  tok = skip(tok->next, "]");
  ty = type_suffix(rest, tok, ty);
  return arrayof(ty, sz);
}

// typeSuffix = "(" funcParams | "[" arrayDimensions | ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  // "(" funcParams
  if (equal(tok, "(")) {
    return func_params(rest, tok->next, ty);
  }
  // "[" arrayDimensions
  if (equal(tok, "["))
    return array_dimensions(rest, tok->next, ty);
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
  Type *ty, *param_ty;

  VarScope *s = findvar(start);
  if (!s)
    errorTok(start, "implicit declaration of a function");
  if (!s->var || s->var->ty->kind != TY_FUNC)
    errorTok(start, "not a function");

  ty = s->var->ty;
  param_ty = ty->params;
  tok = tok->next->next;
  while (!equal(tok, ")")) {
    if (cur != &head)
      tok = skip(tok, ",");
    // assign
    Node *arg = assign(&tok, tok);
    add_type(arg);

    if (param_ty) {
      if (param_ty->kind == TY_STRUCT || param_ty->kind == TY_UNION)
        errorTok(arg->tok, "passing struct or union is not support yet");
      // 将参数节点的类型进行转换
      arg = newcast(arg, param_ty);
      // 前进到下一个形参类型
      param_ty = param_ty->next;
    }
    cur->next = arg;
    cur = cur->next;
    add_type(cur);
  }

  Node *nd = newnode(ND_FUNCALL, tok);
  nd->func_name = strndup(start->loc, start->len);
  nd->args = head.next;
  // 函数类型
  nd->func_type = ty;
  // 读取的返回类型
  nd->ty = ty->returnty;
  *rest = skip(tok, ")");
  return nd;
}

// 解析类型别名
static Token *parse_typedef(Token *tok, Type *basety) {
  bool first = true;

  while (!consume(&tok, tok, ";")) {
    if (!first)
      tok = skip(tok, ",");
    first = false;

    Type *ty = declarator(&tok, tok, basety);
    // 类型别名的变量名存入变量域中，并设置别名
    push_scope(get_ident(ty->name))->typede = ty;
  }
  return tok;
}


// 语法分析入口函数
// program = (typedef | functionDefinition | globalVariable)*
Obj *parse(Token **rest, Token *tok)
{
  Globals = NULL;

  while (tok->kind != TK_EOF) {
    VarAttr attr = {};
    Type *basety = declspec(&tok, tok, &attr);

    // typedef
    if (attr.is_typedef) {
      tok = parse_typedef(tok, basety);
      continue;
    }

    // 函数
    if (is_function(tok)) {
      tok = function(tok, basety, &attr);
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

// 匹配goto和标签
// 因为标签可能会出现在goto后面，所以要在解析完函数后再进行goto和标签的解析
static void resolve_gotolabels(void) {
  // 遍历使goto对应上label
  for (Node *x = Gotos; x; x = x->goto_next) {
    for (Node *y = Labels; y; y = y->goto_next) {
      if (!strcmp(x->label, y->label)) {
        x->unique_label = y->unique_label;
        break;
      }
    }

    if (x->unique_label == NULL)
      errorTok(x->tok->next, "use of undeclared label");
  }

  Gotos = NULL;
  Labels = NULL;
}

// functionDefinition = declspec declarator (";" | "{" compoundStmt)
static Token *function(Token *tok, Type *base, VarAttr *attr)
{
  Type *ty = declarator(&tok, tok, base);

  Obj *fn = new_global(get_ident(ty->name), ty);
  fn->is_function = true;
  fn->is_definition = !consume(&tok, tok, ";");
  fn->is_static = attr->is_static;

  // 判断是否没有函数定义
  if (!fn->is_definition)
    return tok;

  CurrentFn = fn;
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

  // 处理goto和标签
  resolve_gotolabels();
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
static Node *declaration(Token **rest, Token *tok, Type *basety)
{
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
    if (ty->size < 0)
      errorTok(tok, "variable has incomplete type");
    if (ty->kind == TY_VOID)
      errorTok(tok, "variable declared void");
    Obj *var = new_local(get_ident(ty->name), ty);

    // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
    if (!equal(tok, "="))
      continue;

    // 解析"="后面的token
    Node *left = new_varnode(var, ty->name);
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
    if (is_typename(tok) && !equal(tok->next, ":")) {
      VarAttr attr = {};
      Type *basety = declspec(&tok, tok, &attr);

      // 解析typedef的语句
      if (attr.is_typedef) {
        tok = parse_typedef(tok, basety);
        continue;
      }
      // 解析变量声明的语句
      cur->next = declaration(&tok, tok, basety);
    }
    // stmt
    else {
      cur->next = stmt(&tok, tok);
    }
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
//        | "for" "(" (exprStmt | declspec declarator)  expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "goto" ident ";"
//        | "ident ":" stmt
//        | "break" ";"
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

    // 存储此前break标签的名称
    char *brk = BrkLabel;
    // 设备break标签的名称
    BrkLabel = nd->brk_label = new_unique_name();
    // stmt
    nd->then = stmt(&tok, tok);
    // 恢复此前的break标签
    BrkLabel = brk;
    *rest = tok;
    return nd;
  }
  // "for" "(" (exprStmt | declspec declarator)  expr? ";" expr? ")" stmt
  if (equal(tok, "for")) {
    Node *nd = newnode(ND_FOR, tok);
    tok = skip(tok->next, "(");

    // 进入for循环域
    enter_scope();

    // 存储此前break标签的名称
    char *brk = BrkLabel;
    // 设备break标签的名称
    BrkLabel = nd->brk_label = new_unique_name();

    // init
    // init的处理比较特殊，for 循环的init是一条statement，
    // 后两个仅仅是语句。不用判空，因为在内部会判断
    if (is_typename(tok)) {
      // 初始化循环变量
      Type *basety = declspec(&tok, tok, NULL);
      nd->init = declaration(&tok, tok, basety);
    } else {
      nd->init = expr_stmt(&tok, tok);
    }
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

    // 退出for循环域
    leave_scope();
    // 恢复此前的break标签
    BrkLabel = brk;
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

  // "goto" ident ";"
  if (equal(tok, "goto")) {
    Node *nd = newnode(ND_GOTO, tok);
    nd->label = get_ident(tok->next);
    // 将nd同时存入Gotos，最后用于解析uniqueue_label
    nd->goto_next = Gotos;
    Gotos = nd;
    *rest = skip(tok->next->next, ";");
    return nd;
  }

  // ident ":" stmt
  if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
    Node *nd = newnode(ND_LABEL, tok);
    nd->label = strndup(tok->loc, tok->len);
    nd->unique_label = new_unique_name();
    nd->right = stmt(rest, tok->next->next);
    nd->goto_next = Labels;
    Labels = nd;
    return nd;
  }

  if (equal(tok, "break")) {
    if (!BrkLabel)
      errorTok(tok, "stray break");
    // 跳转到break标签的位置
    Node *nd = newnode(ND_GOTO, tok);
    nd->unique_label = BrkLabel;
    *rest = skip(tok->next, ";");
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
    Node *nd = newnode(ND_RETURN, tok);
    Node *exp = expr(&tok, tok->next);

    add_type(exp);
    // 对返回值的类型进行转换
    nd->right = newcast(exp, CurrentFn->ty->returnty);

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

// 转换 A op= B 为 TMP = &A, *TMP = *TMP OP B
static Node *to_assign(Node *binary) {
  // A
  add_type(binary->left);
  // B
  add_type(binary->right);
  Token *tok = binary->tok;

  // TMP
  Obj *var = new_local("", pointerto(binary->left->ty));

  // TMP = &A
  Node *expr1 = newbinary(ND_ASSIGN, new_varnode(var, tok),
                          newunary(ND_ADDR, binary->left, tok), tok);

  // *TMP = *TMP op B
  Node *expr2 = newbinary(
      ND_ASSIGN, newunary(ND_DEREF, new_varnode(var, tok), tok),
      newbinary(binary->kind, newunary(ND_DEREF, new_varnode(var, tok), tok),
                binary->right, tok),
      tok);
  // TMP = &A, *TMPO = &TMP op B
  return newbinary(ND_COMMA, expr1, expr2, tok);
}


// 解析赋值
// assign = logOr (assignOp assign)?
// assignOp = "=" | "+" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
static Node *assign(Token **rest, Token *tok)
{
  Node *nd = log_or(&tok, tok);

  // 可能存在递归赋值，如a=b=1
  // ("=" assign)
  if (equal(tok, "=")) {
    nd = newbinary(ND_ASSIGN, nd, assign(rest, tok->next), tok);
    return nd;
  }

  // ("+=" assign)
  if (equal(tok, "+="))
    return to_assign(newadd(nd, assign(rest, tok->next), tok));

  // ("-=" assign)
  if (equal(tok, "-="))
    return to_assign(newsub(nd, assign(rest, tok->next), tok));

  // ("*=" assign)
  if (equal(tok, "*="))
    return to_assign(newbinary(ND_MUL, nd, assign(rest, tok->next), tok));

  // ("/=" assign)
  if (equal(tok, "/="))
    return to_assign(newbinary(ND_DIV, nd, assign(rest, tok->next), tok));

  // ("%=" assign)
  if (equal(tok, "%="))
    return to_assign(newbinary(ND_MOD, nd, assign(rest, tok->next), tok));

  // ("&=" assign)
  if (equal(tok, "&="))
    return to_assign(newbinary(ND_BITAND, nd, assign(rest, tok->next), tok));

  // ("|=" assign)
  if (equal(tok, "|="))
    return to_assign(newbinary(ND_BITOR, nd, assign(rest, tok->next), tok));

  // ("^=" assign)
  if (equal(tok, "^="))
    return to_assign(newbinary(ND_BITXOR, nd, assign(rest, tok->next), tok));

  *rest = tok;
  return nd;
}
// 逻辑或
// logOr = logAnd ("||" logAnd)*
static Node *log_or(Token **rest, Token *tok) {
  Node *nd = log_and(&tok, tok);
  while (equal(tok, "||")) {
    Token *start = tok;
    nd = newbinary(ND_LOGOR, nd, log_and(&tok, tok->next), start);
  }
  *rest = tok;
  return nd;
}

// 逻辑与
// logAnd = bitOr ("&&" bitOr)*
static Node *log_and(Token **rest, Token *tok) {
  Node *nd = bit_or(&tok, tok);
  while (equal(tok, "&&")) {
    Token *start = tok;
    nd = newbinary(ND_LOGAND, nd, bit_or(&tok, tok->next), start);
  }
  *rest = tok;
  return nd;
}


// 按位或
// bitOr = BitXor ("|" bitXor)*
static Node *bit_or(Token **rest, Token *tok) {
  Node *nd = bit_xor(&tok, tok);
  while (equal(tok, "|")) {
    Token *start = tok;
    nd = newbinary(ND_BITOR, nd, bit_xor(&tok, tok->next), start);
  }
  *rest = tok;
  return nd;
}
// bitXor = BitAnd ("^" bitAnd)*
static Node *bit_xor(Token **rest, Token *tok) {
  Node *nd = bit_and(&tok, tok);
  while (equal(tok, "^")) {
    Token *start = tok;
    nd = newbinary(ND_BITXOR, nd, bit_and(&tok, tok->next), start);
  }
  *rest = tok;
  return nd;
}
// bitAnd = equality ("&" equality)*
static Node *bit_and(Token **rest, Token *tok) {
  Node *nd = equality(&tok, tok);
  while (equal(tok, "&")) {
    Token *start = tok;
    nd = newbinary(ND_BITAND, nd, equality(&tok, tok->next), start);
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
// mul = cast ("*" cast | "/" cast | "%" cast)
static Node *mul(Token **rest, Token *tok) {
  // cast
  Node *nd = cast(&tok, tok);

  // ("*" cast | "/" cast)
  while (1) {
    Token *start = tok;

    // "*" cast
    if (equal(tok, "*")) {
      nd = newbinary(ND_MUL, nd, cast(&tok, tok->next), start);
      continue;
    }

    // "/" cast
    if (equal(tok, "/")) {
      nd = newbinary(ND_DIV, nd, cast(&tok, tok->next), start);
      continue;
    }

    // "%" cast
    if (equal(tok, "%")) {
      nd = newbinary(ND_MOD, nd, cast(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return nd;
  }
}

// 解析类型转换
// cast = "(" typeName ")" cast | unary
static Node *cast(Token **rest, Token *tok) {
  // cast = "(" typeName ")" cast
  if (equal(tok, "(") && is_typename(tok->next)) {
    Token *start = tok;
    Type *ty = typename(&tok, tok->next);
    tok = skip(tok, ")");
    // 解析嵌套的类型转换
    Node *nd = newcast(cast(rest, tok), ty);
    nd->tok = start;
    return nd;
  }

  // unary
  return unary(rest, tok);
}

// unary = ("+" | "-" | "&" | "*" | "!" | "~") cast | ("++" | "--") unary | postfix
static Node *unary(Token **rest, Token *tok)
{
  Node *nd = NULL;

  // "+" cast
  if (equal(tok, "+")) {
    return cast(rest, tok->next);
  }
  // "-" cast
  if (equal(tok, "-")) {
    nd = newunary(ND_NEG, cast(rest, tok->next), tok);
    return nd;
  }
  // "&" cast
  if (equal(tok, "&")) {
    nd = newunary(ND_ADDR, cast(rest, tok->next), tok);
    return nd;
  }
  // "*" cast
  if (equal(tok, "*")) {
    nd = newunary(ND_DEREF, cast(rest, tok->next), tok);
    return nd;
  }
  // "!" cast
  if (equal(tok, "!")) {
    nd = newunary(ND_NOT, cast(rest, tok->next), tok);
    return nd;
  }
  // "~" cast
  if (equal(tok, "~")) {
    nd = newunary(ND_BITNO, cast(rest, tok->next), tok);
    return nd;
  }


  // 转换 ++i 为 i+=1
  // “++” unary
  if (equal(tok, "++"))
    return to_assign(newadd(unary(rest, tok->next), newnum(1, tok), tok));

  // 转换--i 为 i-=1
  if (equal(tok, "--"))
    return to_assign(newsub(unary(rest, tok->next), newnum(1, tok), tok));

  // postfix
  return postfix(rest, tok);
}

// 转换 A++ 为 `(typeof A)(A += 1) -1)`
// Increase Decrease
static Node *new_inc_dec(Node *nd, Token *tok, int addend) {
  add_type(nd);
  return newcast(newadd(to_assign(newadd(nd, newnum(addend, tok), tok)),
                        newnum(-addend, tok), tok),
                 nd->ty);
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
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
      nd = newunary(ND_DEREF, newadd(nd, idx, tok), tok);
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
      nd = newunary(ND_DEREF, nd, tok);
      nd = struct_ref(nd, tok->next);
      tok = tok->next->next;
      continue;
    }
    // "++"
    if (equal(tok, "++")) {
      nd = new_inc_dec(nd, tok, 1);
      tok = tok->next;
      continue;
    }

    // "--"
    if (equal(tok, "--")) {
      nd = new_inc_dec(nd, tok, -1);
      tok = tok->next;
      continue;
    }
    *rest = tok;
    return nd;
  }

}

// abstractDeclarator = "*"* ("(" abstractDeclarator ")")? typeSuffix
static Type *abstract_declarator(Token **rest, Token *tok, Type *ty) {
  // "*"*
  while (equal(tok, "*")) {
    ty = pointerto(ty);
    tok = tok->next;
  }

  // ("(" abstractDeclarator ")")?
  if (equal(tok, "(")) {
    // 记录 "(" 的位置
    Token *start = tok;
    Type dummy = {};
    // 使tok 前进到")"之后的位置
    abstract_declarator(&tok, start->next, &dummy);
    tok = skip(tok, ")");
    // 获取到")"后面的类型后缀，ty为解析完的类型，rest指向分号
    ty = type_suffix(rest, tok, ty);
    // 解析ty整体作为base去构造，返回Type类型
    return abstract_declarator(&tok, start->next, ty);
  }

  // typeSuffix
  return type_suffix(rest, tok, ty);
}

// typeName = declspec abstractDeclarator
// 获取类型的相关信息
static Type *typename(Token  **rest, Token *tok) {
  // declspec
  Type *ty = declspec(&tok, tok, NULL);
  // abstractDeclarator
  return abstract_declarator(rest, tok, ty);
}

// 解析括号、数字、变量
// primary = "(" "{" stmt+ "}" ")"
//          | "(" expr ")"
//          | ident func-args?
//          | num
//          | str
//          | "sizeof" unary
//          | "sizeof" "(" typeName ")"
static Node *primary(Token **rest, Token *tok)
{
  Token *start = tok;

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
      VarScope *s  = findvar(tok);
      if (!s || (!s->var && !s->enumty)) {
        // 未声明就使用变量，报错
        errorTok(tok, "undefined variable");
      }

      Node *nd;
      // 是否为变量
      if (s->var)
        nd = new_varnode(s->var, tok);
      else
        nd = newnum(s->enumval, tok);

      *rest = tok->next;
      return nd;
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
    return new_varnode(var, tok);
  }

  // "sizeof" "(" typeName ")"
  if (equal(tok, "sizeof") && equal(tok->next, "(") &&
      is_typename(tok->next->next)) {
    Type *ty = typename(&tok, tok->next->next);
    *rest = skip(tok, ")");
    return newnum(ty->size, start);
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

// 获取枚举类型信息
// enumSpecifier = ident? "{" enumList? "}"
//              | ident ("{" enumList? "}")?
// enumList = ident ("=" num)? ("," ident ("=" num)?)*
static Type *enum_specifier(Token **rest, Token *tok) {
  Type *ty = enumtype();

  // 读取标签
  // ident?
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  // 处理没有{}的情况
  if (tag && !equal(tok, "{")) {
    Type *ty = findtag(tag);
    if (!ty)
      errorTok(tag, "unknown enum type");
    if (ty->kind != TY_ENUM)
      errorTok(tag, "not an enum tag");
    *rest = tok;
    return ty;
  }

  // "{" enumList? "}"
  int i = 0;
  int val = 0;
  tok = skip(tok, "{");
  while (!equal(tok, "}")) {
    if (i++ > 0)
      tok = skip(tok, ",");

    char *name = get_ident(tok);
    tok = tok->next;

    if (equal(tok, "=")) {
      val = tok->next->val;
      tok = tok->next->next;
    }

    // 存入枚举常量
    VarScope *s = push_scope(name);
    s->enumty = ty;
    s->enumval = val++;
  }

  *rest = tok->next;
  if (tag)
    push_tagscope(tag, ty);
  return ty;
}





// structMembers = (declspec declarator ("," declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head = {};
  Member *cur = &head;

  while (!equal(tok, "}")) {
    // declspec
    Type *basety = declspec(&tok, tok, NULL);
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

  // 构造不完整结构体
  if (tag && !equal(tok, "{")) {
    *rest = tok;
    Type *ty = findtag(tag);
    if (ty)
      return ty;

    ty = structtype();
    ty->size = -1;
    push_tagscope(tag, ty);
    return ty;
  }

  // ("{" structMembers)?
  tok = skip(tok, "{");

  // 构造一个结构体
  Type *ty = structtype();
  struct_members(rest, tok, ty);

  // 如果是重复定义，就覆盖之前的定义
  // 如果有名就注册结构体类型
  if (tag) {
    for (TagScope *s = Scp->tags; s; s = s->next) {
      if (equal(tag, s->name)) {
        *s->ty = *ty;
        return s->ty;
      }
    }
    push_tagscope(tag, ty);
  }
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

