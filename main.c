// 使用POSIX.1标准
// 使用了strndup函数
#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>


typedef enum {
  TK_IDENT, // 标记符，可以为变量名、函数名等
  TK_PUNCT, // 操作符：如+-
  TK_KEYWORD, // 关键字
  TK_NUM,   // 数字
  TK_EOF,   // 文件终止符，即文件的最后
} TokenKind;

typedef struct Token {
  TokenKind kind;
  struct Token *next;
  int val;
  char *loc;
  int len;
} Token;

typedef enum {
  ND_ADD, // +
  ND_SUB, // -
  ND_MUL, // *
  ND_DIV, // /
  ND_NEG, // 负号-
  ND_LT, // <
  ND_LE, // <=
  ND_NE, // !=
  ND_EQ, // ==
  ND_EXPR_STMT, // 表达式语句
  ND_ASSIGN, // 赋值
  ND_RETURN, // 返回
  ND_IF,     // "if" 条件判断
  ND_FOR,    // "for" 循环
  ND_BLOCK,  // 代码块（花括号）
  ND_VAR, // 变量
  ND_NUM, // INT NUMBER
} NodeKind;

typedef struct Obj Obj;

// AST中二叉树节点
// AST: 语法树
// 越往下，优先级越高
typedef struct Node {
  NodeKind kind;
  struct Node *next; // 下一节点，指代下一语句
  struct Node *left;
  struct Node *right;
  Obj *var;          // 存储ND_VAL种类的变量
  struct Node *body; // 代码块
  int val;           // 存储ND_NUM种类的值

  // if 语句 或者 "for" 语句
  struct Node *cond;  // 条件内的表达式
  struct Node *then;  // 符合条件后的语句
  struct Node *els;   // 不符合条件后的语句
  struct Node *init;  // 初始化语句
  struct Node *inc;   // 递增语句
} Node;


// 本地变量
typedef struct Obj {
  struct Obj *next; // 指向下一个对象
  char *name;       // 变量名
  int offset;       // fp的偏移量
} Obj;

// 函数
typedef struct Function {
  Node *body;    // 函数体
  Obj *locals;   // 本地变量
  int stacksize; // 栈大小
} Function;

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

static Obj *new_local(char *name)
{
  Obj *obj = calloc(1, sizeof(Obj));
  obj->name = name;
  obj->next = Locals;
  Locals = obj;
  return obj;
}

// 新建一个二叉树节点
// 某些类型的Node是需要left和right的，比如说+-
static Node *newbinary(NodeKind kind, Node *left, Node *right)
{
  Node *nd = calloc(1, sizeof(Node));
  nd->kind = kind;
  nd->left = left;
  nd->right = right;
  return nd;
}

// 新建一个节点，不需要孩子
// 可能通过Node其他成员来维护/访问
// 比如说 compound_stmt ==> "{"
static Node *newnode(NodeKind kind)
{
  return newbinary(kind, NULL, NULL);
}


static Node *newnum(int val)
{
  Node *nd = newnode(ND_NUM);
  nd->val = val;
  return nd;
}

static Node *newvar(Obj *var)
{
  Node *nd = newbinary(ND_VAR, NULL, NULL);
  nd->var = var;
  return nd;
}

static void error(char *fmt, ...)
{
  va_list va;
  va_start(va, fmt);
  vfprintf(stderr, fmt, va);
  fprintf(stderr, "\n");
  va_end(va);
  // 终止程序
  exit(1);
}

static Token *newtoken(TokenKind kind, char *start)
{
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->loc = start;
  return tok;
}

static int getnumber(Token *tok)
{
  if (tok->kind != TK_NUM)
    error("expect a number");
  return tok->val;
}
static bool equal(Token *tok, char *str)
{
  return memcmp(tok->loc, str, tok->len) == 0 && str[tok->len] == 0;
}

// 跳过指定的Str
static Token *skip(Token *tok, char *str)
{
  if (!equal(tok, str))
    error("expect: %s\n", str);
  return tok->next;
}

// 判断标记符首字母规则
// [a-zA-Z_]
static bool isident1(char c)
{
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

// 判断标记符的非首字母的规则
// [a-zA-z0-9_]
static bool isident2(char c)
{
  return isident1(c) || (c >= '0' && c <= '9');
}

static bool iskeyword(Token *tok)
{
  char *KW[] = {"return", "if", "else", "for", "while"};
  for (int i = 0; i < sizeof(KW)/sizeof(*KW); i++) {
    if (equal(tok, KW[i]))
      return true;
  }
  return false;
}
// 将名为“return”的终结符转为KEYWORD
static void convert_keywords(Token *tok)
{
  for (Token *t = tok; t->kind != TK_EOF; t = t->next) {
    if (iskeyword(tok)) {
      t->kind = TK_KEYWORD;
    }
  }
}


// 词法分析
static Token *tokenize(char *p)
{
  Token head = {};
  Token *cur = &head;

  while (*p) {
    if (isspace(*p)) {
      p++;
      continue;
    }
    if (isdigit(*p)) {
      cur->next = newtoken(TK_NUM, p);
      cur = cur->next;
      const char *oldp = p;
      cur->val = strtol(p, &p, 10);
      cur->len = p - oldp;
      continue;
    }
    // 解析标记符
    // [a-zA-Z_][a-zA-Z0-9_]*
    if (isident1(*p)) {
      char *start = p;
      do {
        p++;
      } while (isident2(*p));
      cur->next = newtoken(TK_IDENT,start);
      cur = cur->next;
      cur->len = p - start;
      continue;
    }

    if (*p == '=' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (*p == '!' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (*p == '<' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (*p == '>' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (ispunct(*p) || *p == ';') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 1;
      p++;
      continue;
    }


    // 处理无法识别的字符
    error("unexcepted character: '%c'\n", *p);
  }

  // 解析结束，增加一个EOF，表示终止符
  cur->next = newtoken(TK_EOF, p);

  // 将所有关键字的终结符，都标记为KEYWORD
  convert_keywords(head.next);
  return head.next;
}

// compoundStmt = stmt* "}"
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
// unary = ("+" | "-") unary | primary
// primary  = "(" expr ")" | num | ident
static Node *compound_stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);


// 语法分析入口函数
// parse = "{" compoundStmt
Function *parse(Token **rest, Token *tok)
{
  // "{"
  tok = skip(tok, "{");

  // 函数题存储语句的AST，locals存储变量
  Function *prog = calloc(1, sizeof(Function));
  prog->body = compound_stmt(rest, tok);
  prog->locals = Locals;
  return prog;
}

// compoundStmt = stmt* "}"
static Node *compound_stmt(Token **rest, Token *tok)
{
  Node head = {};
  Node *cur = &head;

  // stmt*
  while (!equal(tok, "}")) {
    cur->next = stmt(&tok, tok);
    cur = cur->next;
  }

  Node *nd = newnode(ND_BLOCK);
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
    Node *nd = newnode(ND_FOR);
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
    Node *nd = newnode(ND_FOR);
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
    Node *nd = newnode(ND_IF);
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
    Node *nd = newnode(ND_BLOCK);
    nd->body = compound_stmt(rest, tok->next);
    return nd;
  }
  // "return" expr ";"
  if (equal(tok, "return")) {
    Node *nd = newbinary(ND_RETURN, NULL, expr(&tok, tok->next));
    *rest = skip(tok, ";");
    return nd;
  }
  // 空语句判断
  if (equal(tok, ";")) {
    *rest = tok->next;
    // 这里用一个body成员为空的ND_BLOCK来表示空语句，
    // 因为在处理ND_BLOCK时会遍历body，空的话则不会产生影响
    return newnode(ND_BLOCK);
  }
  Node *nd = newbinary(ND_EXPR_STMT, NULL, expr(&tok, tok));
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
    return newnode(ND_BLOCK);
  }

  // expr ";"
  Node *nd = newbinary(ND_EXPR_STMT, NULL, expr(&tok, tok));
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
    nd = newbinary(ND_ASSIGN, nd, assign(&tok, tok->next));
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
      nd = newbinary(ND_LT, nd, add(&tok, tok->next));
      continue;
    }
    // ">" add ==> 改变孩子的左右顺序转换成 "<" 的情况
    if (equal(tok, ">")) {
      nd = newbinary(ND_LT, add(&tok, tok->next), nd);
      continue;
    }
    // "<=" add
    if (equal(tok, "<=")) {
      nd = newbinary(ND_LE, nd, add(&tok, tok->next));
      continue;
    }
    // ">=" add ==> 改变孩子的左右顺序转换成 "<=" 的情况
    if (equal(tok, ">=")) {
      nd = newbinary(ND_LE, add(&tok, tok->next), nd);
      continue;
    }
    // "!=" add
    if (equal(tok, "!=")) {
      nd = newbinary(ND_NE, nd, add(&tok, tok->next));
      continue;
    }
    // "==" add
    if (equal(tok, "==")) {
      nd = newbinary(ND_EQ, nd, add(&tok, tok->next));
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
      nd = newbinary(ND_ADD, nd, mul(&tok, tok->next));
      continue;
    }
    // "-" mul
    if (equal(tok, "-")) {
      nd = newbinary(ND_SUB, nd, mul(&tok, tok->next));
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
      nd = newbinary(ND_MUL, nd, unary(&tok, tok->next));
      continue;
    }

    // "/" primary
    if (equal(tok, "/")) {
      nd = newbinary(ND_DIV, nd, unary(&tok, tok->next));
      continue;
    }

    *rest = tok;
    return nd;
  }
}

// unary = ("+" | "-") unary | primary
static Node *unary(Token **rest, Token *tok)
{
  Node *nd = NULL;

  // "+" unary
  if (equal(tok, "+")) {
    return unary(rest, tok->next);
  }
  // "-" unary
  if (equal(tok, "-")) {
    nd = newbinary(ND_NEG, NULL, unary(rest, tok->next));
    return nd;
  }

  return primary(rest, tok);
}
// 解析括号、数字、变量
// premary = "(" expr ")" | ident | num
static Node *primary(Token **rest, Token *tok)
{
  // "(" expr ")"
  if (equal(tok, "(")) {
    Node *nd = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return nd;
  }
  // ident
  if (tok->kind == TK_IDENT) {
    Obj *var = findvar(tok);
    if (!var) {
      // strndup复制n个字符
      var = new_local(strndup(tok->loc, tok->len));
    }
    *rest = tok->next;
    return newvar(var);
  }
  // num
  if (tok->kind == TK_NUM) {
    Node *nd = newnum(tok->val);
    *rest = tok->next;
    return nd;
  }

  error("unexpected char '%c'\n", tok->val);
  return NULL;
}



// 存储栈的深度
static int depth;

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
    var->offset = offset;
  }
  prog->stacksize = align_to(offset, 16);
}

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
  error("not an lvalue");
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

  error("invalid expression\n");
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
    printf("  # 跳转到.L.return段\n");
    printf("  j .L.return\n");
    return;
  }

  if (nd->kind == ND_EXPR_STMT) {
    gen_expr(nd->right);
    return;
  }
  error("invalid statement\n");
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

  printf("  # 定义全局main段\n");
  printf("  .globl main\n");
  printf("\n# =====程序开始===============\n");
  printf("# main段标签，也是程序入口段\n");
  printf("main:\n");

  // 栈布局
  //-------------------------------// sp
  //              fp                  fp = sp-8
  //-------------------------------// fp
  //              变量
  //-------------------------------// sp=sp-8-stacksize
  //           表达式计算
  //-------------------------------//
  // Prologue, 前言
  // 将fp压入栈中，保存fp的值
  printf("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值\n");
  printf("  addi sp, sp, -8\n");
  printf("  sd fp, 0(sp)\n");
  // 将sp写入fp
  printf("  # 将sp的值写入fp\n");
  printf("  mv fp, sp\n");
  // sp偏移量为实际占用的栈大小
  printf("  # sp腾出StackSize大小的栈空间\n");
  printf("  addi sp, sp, -%d\n", prog->stacksize);

  // 使用语法树，生成表达式
  printf("\n# =====程序主体===============\n");
  gen_stmt(prog->body);
  assert(depth == 0);

  // Epilogue, 后语
  // 输出return段标签
  printf("\n# =====程序结束===============\n");
  printf("# return段标签\n");
  printf(".L.return:\n");
  // 将fp的值改写回sp
  printf("  # 将fp的值写回sp\n");
  printf("  mv sp, fp\n");
  // 将最早fp保存的值弹栈，恢复fp
  printf("  # 将最早fp保存的值弹栈，恢复fp和sp\n");
  printf("  ld fp, 0(sp)\n");
  printf("  addi sp, sp, 8\n");

  // ret为jalr x0, x1, 0别名指令，用于返回子程序
  printf("  # 返回a0值给系统调用\n");
  printf("  ret\n");


  return 0;
}
