#include "rvcc.h"

// 在解析时，全部的变量实例都被累加到这个列表里。
Obj *Locals;

static Obj *findvar(Token *tok)
{
  // 查找Locals变量中是否存在同名变量
  for (Obj *var = Locals; var; var = var->next) {
    if (strlen(var->name) == tok->len &&
        !strncmp(tok->loc, var->name, tok->len)) {
          return var;
    }
  }
  return NULL;
}

static Obj *new_local(char *name, Type *ty)
{
  Obj *obj = calloc(1, sizeof(Obj));
  obj->name = name;
  obj->next = Locals;
  obj->ty = ty;
  Locals = obj;
  return obj;
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


static Node *newnum(int val, Token *tok)
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
  if (left->ty->kind == TY_PTR
      && right->ty->kind == TY_PTR)
    errorTok(tok, "invalid operands");

  // 将 num + ptr 转换为 ptr + num
  if (left->ty->kind == TY_INT
      && right->ty->kind == TY_PTR) {
    Node *tmp = left;
    left = right;
    right = tmp;
  }

  // ptr + num
  // 指针加法， ptr+1，不是1个字节，而是一个元素的空间，所以需要 x8
  right = newbinary(ND_MUL, right, newnum(8, tok), tok);
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
  if (left->ty->kind == TY_PTR
      && right->ty->kind == TY_INT) {
    right = newbinary(ND_MUL, right, newnum(8, tok), tok);
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
    return newbinary(ND_DIV, nd, newnum(8, tok), tok);
  }
  errorTok(tok, "invalid operands");
  return NULL;
}

// program = functionDefinition*
// functionDefinition = declspec declarator "{" compoundStmt
// compoundStmt = (declaration | stmt*) "}"
// declaration =
//        declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// declspec = "int"
// declarator = "*"* ident typeSuffix
// typeSuffix = ("(" ")")?
// stmt = ("return") expr ";"
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | expr? ";"
//        | "{" compoundStmt
// exprStmt = expt? ";"
// expr = assign
// assign = equality ("=" assign)?
// equality = add ("<" add | ">" add | "<=" add | ">=" add | "!=" add | "==" add)
// add = mul ("+" mul | "-" mul)
// mul = unary ("*" unary | "/" unary)
// unary = ("+" | "-" | "&" | "*") unary | primary
// primary  = "(" expr ")" | num | funcall
// funcall = ident "(" (assign ("," assign)*)? ")"
Function *function(Token **rest, Token *tok);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *declaration(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// declspec = "int"
// declarator specifier
static Type *declspec(Token **rest, Token *tok)
{
  *rest = skip(tok, "int");
  return TyInt;
}

static Type *type_suffix(Token **rest, Token *tok, Type *ty)
{
  // ("(" ")")
  if (equal(tok, "(")) {
    // 目前只支持0参数函数定义
    *rest = skip(tok->next, ")");
    return functype(ty);
  }
  *rest = tok;
  return ty;
}

// declarator = "*"* ident typesuffix
static Type *declarator(Token **rest, Token *tok, Type *ty)
{
  // "*"*
  // 构建所有的（多重）指针
  while (consume(&tok, tok, "*"))
    ty = pointerto(ty);

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
// program = functionDefinition*
Function *parse(Token **rest, Token *tok)
{
  Function head = {};
  Function *cur = &head;

  while (tok->kind != TK_EOF) {
    cur->next = function(&tok, tok);
    cur = cur->next;
  }
  *rest = tok;
  return head.next;
}

// functionDefinition = declspec declarator "{" compoundStmt
Function *function(Token **rest, Token *tok)
{
  // declspec
  Type *ty = declspec(&tok, tok);
  // declarator
  ty = declarator(&tok, tok, ty);

  // 清空全局变量Locals
  Locals = NULL;

  // 从解析完成的ty中读取ident
  Function *fn = calloc(1, sizeof(Function));
  fn->name = get_ident(ty->name);

  // "{"
  tok = skip(tok, "{");

  // 函数题存储语句的AST，locals存储变量
  fn->body = compound_stmt(rest, tok);
  fn->locals = Locals;
  return fn;
}


// declaration =
//        declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
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
    Obj *var = new_local(get_ident(ty->name), ty);

    // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
    if (!equal(tok, "="))
      continue;

    // 解析"="后面的token
    Node *left = newvar(var, ty->name);
    // 解析递归赋值语句
    // tok->next 跳过 "="
    Node *right = expr(&tok, tok->next);
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

  // stmt*
  while (!equal(tok, "}")) {
    // declaration
    if (equal(tok, "int"))
      cur->next = declaration(&tok, tok);
    // stmt
    else
      cur->next = stmt(&tok, tok);
    cur = cur->next;
    // 构造完AST后，为节点添加类型信息
    add_type(cur);
  }

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

// expr = assign
static Node *expr(Token **rest, Token *tok)
{ return assign(rest, tok); }

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

  return primary(rest, tok);
}
// 解析括号、数字、变量
// premary = "(" expr ")" | num | funcall
static Node *primary(Token **rest, Token *tok)
{
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

  error("unexpected char '%c'\n", tok->val);
  return NULL;
}



// 存储栈的深度
static int depth;
// 用于函数参数的寄存器们
static char *ArgReg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
// 当前的函数
static Function *current_fn;

static int align_to(int n, int align)
{
  // 向上对齐 (0, align] 返回 align
  return (n+align-1) & ~(align-1);
}
static void assign_lvar_offset(Function *prog)
{
  int offset = 0;
  for (Obj *var = prog->locals; var; var = var->next) {
    // 为每个变量分配8个字节
    offset += 8;
    var->offset = -offset;
  }
  prog->stacksize = align_to(offset, 16);
}

static void gen_expr(Node *nd);
// 计算给定节点的绝对地址
// 如果报错，说明节点不在栈中
static void gen_addr(Node *nd)
{
  if (nd->kind == ND_VAR) {
    // 偏移量是相对fp的
    printf("  # 获取变量%s的栈内地址为%d(fp)\n", nd->var->name,
           nd->var->offset);
    printf("  addi a0, fp, %d\n", nd->var->offset);
    return;
  }
  // &* expr == expr
  if (nd->kind == ND_DEREF) {
    gen_expr(nd->right);
    return;
  }
  errorTok(nd->tok, "not an lvalue");
}

// 压栈，将结果临时存入栈中备用。
// 不实用寄存器存储的原因是需要存储变量的个数是变化的
static void push(void)
{
  printf("  # 压栈，将a0的值存入栈顶\n");
  printf("  addi sp, sp, -8\n");
  printf("  sd a0, 0(sp)\n");
  depth++;
}

// 弹栈，弹出到reg名称的寄存器中
static void pop(const char *reg)
{
  printf("  # 弹栈，将栈顶的值存入%s\n", reg);
  printf("  ld %s, 0(sp)\n", reg);
  printf("  addi sp, sp, 8\n");
  depth--;
}

// 代码段计数
static int count(void)
{
  static int I = 1;
  return I++;
}

static void gen_expr(Node *nd)
{
  if (nd->kind == ND_NUM) {
    printf("  # 将%d加载到a0中\n", nd->val);
    printf("  li a0, %d\n", nd->val);
    return;
  }
  if (nd->kind == ND_NEG) {
    gen_expr(nd->right);
    printf("  # 对a0值进行取反\n");
    printf("  neg a0, a0\n");
    return;
  }
  // 解引用
  if (nd->kind == ND_DEREF) {
    gen_expr(nd->right);
    printf("  # 读取a0中存放的地址，得到的值存入a0\n");
    printf("  ld a0, 0(a0)\n");
    return;
  }
  // 取地址
  if (nd->kind == ND_ADDR) {
    gen_addr(nd->right);
    return;
  }
  if (nd->kind == ND_ASSIGN) {
    // 左部是左值，保存值到的地址
    gen_addr(nd->left);
    push();
    // 右部是右值，为表达式的值
    gen_expr(nd->right);
    pop("a1");
    printf("  # 将a0的值，写入到a1中存放的地址\n");
    printf("  sd a0, 0(a1)\n");
    return;
  }
  if (nd->kind == ND_FUNCALL) {
    // 记录参数个数
    int nargs = 0;
    // 计算所有参数的值，正向压栈
    for (Node *arg = nd->args; arg; arg = arg->next) {
      gen_expr(arg);
      push();
      nargs++;
    }
    // 反向弹栈, a0->参数1, a1->参数2
    for (int i = nargs-1; i >= 0; i--)
      pop(ArgReg[i]);

    printf("\n # 调用函数%s\n", nd->func_name);
    printf("  call %s\n", nd->func_name);
    return;
  }
  if (nd->kind == ND_VAR) {
    // 计算出变量的地址，然后存入a0
    gen_addr(nd);
    // 访问a0地址中存储的数据，存入到a0当中
    printf("  # 读取a0中存放的地址，得到的值存入a0\n");
    printf("  ld a0, 0(a0)\n");
    return;
  }


  // 递归到最右下节点
  gen_expr(nd->right);
  // 将结果压入栈
  push();
  // 递归到左节点
  gen_expr(nd->left);
  // 将结果弹栈到a1
  pop("a1");

  switch (nd->kind) {
  case ND_ADD:
    printf("  # a0+a1，结果写入a0\n");
    printf("  add a0, a0, a1\n");
    return;
  case ND_SUB:
    printf("  # a0-a1，结果写入a0\n");
    printf("  sub a0, a0, a1\n");
    return;
  case ND_MUL:
    printf("  # a0×a1，结果写入a0\n");
    printf("  mul a0, a0, a1\n");
    return;
  case ND_DIV:
    printf("  # a0÷a1，结果写入a0\n");
    printf("  div a0, a0, a1\n");
    return;
  case ND_EQ:
  case ND_NE:
    // a0=a0^a1，异或指令
    printf("  # 判断是否a0%sa1\n", nd->kind == ND_EQ ? "=" : "≠");
    printf("  xor a0, a0, a1\n");
    if (nd->kind == ND_EQ)
      // a0==a1
      // a0=a0^a1, sltiu a0, a0, 1
      // 等于0则置1
      printf("  seqz a0, a0\n");
    else
      // a0!=a1
      // a0=a0^a1, sltu a0, x0, a0
      // 不等于0则置1
      printf("  snez a0, a0\n");
    return;
  case ND_LT:
    printf("  # 判断a0<a1\n");
    printf("  slt a0, a0, a1\n");
    return;
  case ND_LE:
    // a0<=a1等价于
    // a0=a1<a0, a0=a1^1
    printf("  # 判断是否a0≤a1\n");
    printf("  slt a0, a1, a0\n");
    printf("  xori a0, a0, 1\n");
    return;
  default:
    break;
  }

  errorTok(nd->tok, "invalid expression\n");
}

static void gen_stmt(Node *nd)
{
  if (nd->kind == ND_FOR) {
    // 代码段技术
    int c = count();
    printf("\n# =====循环语句%d===============\n", c);
    // 生成初始化语句
    if (nd->init) {
      printf("\n# Init语句%d\n", c);
      gen_stmt(nd->init);
    }
    // 输出循环头部标签
    printf("\n# 循环%d的.L.begin.%d段标签\n", c, c);
    printf(".L.begin.%d:\n", c);
    // 处理循环条件语句
    printf("# Cond表达式%d\n", c);
    if (nd->cond) {
      // 生成条件循环语句
      gen_expr(nd->cond);
      // 判断结果是否为0，为0则跳转到结束部分
      printf("  # 若a0为0，则跳转到循环%d的.L.end.%d段\n", c, c);
      printf("  beqz a0, .L.end.%d\n", c);
    }
    // 生成循环体语句
    printf("\n# Then语句%d\n", c);
    gen_stmt(nd->then);
    // 处理循环递增语句
    if (nd->inc) {
      // 生成循环递增语句
      gen_expr(nd->inc);
    }
    // 跳转到循环头部
    printf("  j .L.begin.%d\n", c);
    // 输出循环尾部标签
    printf(".L.end.%d:\n", c);
    return;
  }
  if (nd->kind == ND_IF) {
    // 代码段计数
    int c = count();
    printf("\n# =====分支语句%d==============\n", c);
    // 生成条件内语句
    printf("\n# Cond表达式%d\n", c);
    gen_expr(nd->cond);
    // 判断结果是否为0，为0则跳转到else标签
    printf("  # 若a0为0，则跳转到分支%d的.L.else.%d段\n", c, c);
    printf("  beqz a0, .L.else.%d\n", c);
    // 生成复合条件后的语句
    printf("\n# Then语句%d\n", c);
    gen_stmt(nd->then);
    // 执行完后跳转到if语句后面的语句
    printf("  # 跳转到分支%d的.L.end.%d段\n", c, c);
    printf("  j .L.end.%d\n", c);
    // else代码块，else可能为空，故输出标签
    printf("\n# Else语句%d\n", c);
    printf("# 分支%d的.L.else.%d段标签\n", c, c);
    printf(".L.else.%d:\n", c);
    // 生成不符合条件后的语句
    if (nd->els)
      gen_stmt(nd->els);
    // 结束if语句，继续执行后面的语句
    printf("\n# 分支%d的.L.end.%d段标签\n", c, c);
    printf(".L.end.%d:\n", c);

    return;
  }
  if (nd->kind == ND_BLOCK) {
    for (Node *n = nd->body; n; n = n->next) {
      gen_stmt(n);
    }
    return;
  }
  if (nd->kind == ND_RETURN) {
    printf("# 返回语句\n");
    gen_expr(nd->right);
    // 无条件跳转语句，跳转到.L.return段
    // j offset是 jal x0, offset的别名指令
    printf("  # 跳转到.L.return.%s段\n", current_fn->name);
    printf("  j .L.return.%s\n", current_fn->name);
    return;
  }

  if (nd->kind == ND_EXPR_STMT) {
    gen_expr(nd->right);
    return;
  }
  errorTok(nd->tok, "invalid statement\n");
}



int main(int Argc, char **Argv) {
  // 判断传入程序的参数是否为2个，Argv[0]为程序名称，Argv[1]为传入的第一个参数
  if (Argc != 2) {
    // 异常处理，提示参数数量不对。
    // fprintf，格式化文件输出，往文件内写入字符串
    // stderr，异常文件（Linux一切皆文件），用于往屏幕显示异常信息
    // %s，字符串
    fprintf(stderr, "%s: invalid number of arguments\n", Argv[0]);
    // 程序返回值不为0时，表示存在错误
    return 1;
  }


  // 词法分析
  Token *tok = tokenize(Argv[1]);

  // 语法分析
  Function *prog = parse(&tok, tok);
  if (tok->kind != TK_EOF)
    error("extra token, kind: %d\n", tok->kind);

  // 分配函数内部变量的栈空间
  assign_lvar_offset(prog);

  // 为每个函数单独生成代码
  for (Function *fn = prog; fn; fn = fn->next) {
    printf("  # 定义全局%s段\n", fn->name);
    printf("  .globl %s\n", fn->name);
    printf("\n# =====程序开始===============\n");
    printf("# %s段标签，也是程序入口段\n", fn->name);
    printf("%s:\n", fn->name);
    current_fn = fn;
    // 栈布局
    //-------------------------------// sp
    //              ra
    //-------------------------------// ra = sp-8
    //              fp
    //-------------------------------// fp = sp-16
    //              变量
    //-------------------------------// sp=sp-16-stacksize
    //           表达式计算
    //-------------------------------//

    // Prologue, 前言
    // 将ra寄存器压栈，保存ra的值
    printf("  # 将ra寄存器压栈,保存ra的值\n");
    printf("  addi sp, sp, -16\n");
    printf("  sd ra, 8(sp)\n");
    // 将fp压入栈中，保存fp的值
    printf("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值\n");
    printf("  sd fp, 0(sp)\n");
    // 将sp写入fp
    printf("  # 将sp的值写入fp\n");
    printf("  mv fp, sp\n");
    // sp偏移量为实际占用的栈大小
    printf("  # sp腾出StackSize大小的栈空间\n");
    printf("  addi sp, sp, -%d\n", fn->stacksize);

    // 使用语法树，生成表达式
    printf("\n# =====%s主体===============\n", fn->name);
    gen_stmt(fn->body);
    assert(depth == 0);

    // Epilogue, 后语
    // 输出return段标签
    printf("\n# =====%s结束===============\n", fn->name);
    printf("# return段标签\n");
    printf(".L.return.%s:\n", fn->name);
    // 将fp的值改写回sp
    printf("  # 将fp的值写回sp\n");
    printf("  mv sp, fp\n");
    // 将最早fp保存的值弹栈，恢复fp
    printf("  # 将最早fp保存的值弹栈，恢复fp和sp\n");
    printf("  ld fp, 0(sp)\n");
    // 将ra寄存器弹栈,恢复ra的值
    printf("  # 将ra寄存器弹栈,恢复ra的值\n");
    printf("  ld ra, 8(sp)\n");
    printf("  addi sp, sp, 16\n");

    // ret为jalr x0, x1, 0别名指令，用于返回子程序
    printf("  # 返回a0值给系统调用\n");
    printf("  ret\n");
  }
  return 0;
}
