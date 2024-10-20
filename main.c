#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>


typedef enum {
  TK_PUNCT, // 操作符：如+-
  TK_NUM,
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
  ND_NUM, // INT NUMBER
} NodeKind;

// AST中二叉树节点
// AST: 语法树
// 越往下，优先级越高
typedef struct Node {
  NodeKind kind;
  struct Node *next; // 下一节点，指代下一语句
  struct Node *left;
  struct Node *right;
  int val;
} Node;

static Node *newbinary(NodeKind kind, Node *left, Node *right)
{
  Node *nd = calloc(1, sizeof(Node));
  nd->kind = kind;
  nd->left = left;
  nd->right = right;
  return nd;
}

static Node *newnum(int val)
{
  Node *nd = newbinary(ND_NUM, NULL, NULL);
  nd->val = val;
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
  return head.next;
}

// stmt = expr ("; expr")
// expr = add ("<" add | ">" add | "<=" add | ">=" add | "!=" add | "==" add)
// add = mul ("+" mul | "-" mul)
// mul = unary ("*" unary | "/" unary)
// unary = ("+" | "-") unary | primary
// primary  = "(" expr ")" | num
static Node *stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// 语法分析入口函数
Node *parse(Token **rest, Token *tok)
{
  Node head = {};
  Node *cur = &head;

  // stmt*
  while (tok->kind != TK_EOF) {
    cur->next = stmt(&tok, tok);
    cur = cur->next;
  }
  *rest = tok;
  return head.next;
}

// 解析表达式语句
// stmt = expr;
static Node *stmt(Token **rest, Token *tok)
{
  Node *nd = newbinary(ND_EXPR_STMT, NULL, expr(&tok, tok));
  *rest = skip(tok, ";");
  return nd;
}

// 解析条件运算符
// expr = add ("<" add | ">" add | "<=" add | ">=" add | "!=" add | "==" add)
static Node *expr(Token **rest, Token *tok)
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
// 解析括号、数字
// premary = "(" expr ")" or num
static Node *primary(Token **rest, Token *tok)
{
  // "(" expr ")"
  if (equal(tok, "(")) {
    Node *nd = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return nd;
  }

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

// 压栈，将结果临时存入栈中备用。
// 不实用寄存器存储的原因是需要存储变量的个数是变化的
static void push(void)
{
  printf("  addi sp, sp, -8\n");
  printf("  sd a0, 0(sp)\n");
  depth++;
}

// 弹栈，弹出到reg名称的寄存器中
static void pop(const char *reg)
{
  printf("  ld %s, 0(sp)\n", reg);
  printf("  addi sp, sp, 8\n");
  depth--;
}

static void gen_expr(Node *nd)
{
  if (nd->kind == ND_NUM) {
    printf("  li a0, %d\n", nd->val);
    return;
  }
  if (nd->kind == ND_NEG) {
    gen_expr(nd->right);
    printf("  neg a0, a0\n");
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
    printf("  add a0, a0, a1\n");
    return;
  case ND_SUB:
    printf("  sub a0, a0, a1\n");
    return;
  case ND_MUL:
    printf("  mul a0, a0, a1\n");
    return;
  case ND_DIV:
    printf("  div a0, a0, a1\n");
    return;
  case ND_EQ:
  case ND_NE:
    // a0=a0^a1，异或指令
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
    printf("  slt a0, a0, a1\n");
    return;
  case ND_LE:
    // a0<=a1等价于
    // a0=a1<a0, a0=a1^1
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
  if (nd->kind == ND_EXPR_STMT) {
    gen_expr(nd->right);
    return;
  }
  error("invaild statement\n");
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
  Node *node = parse(&tok, tok);
  if (tok->kind != TK_EOF)
    error("extra token, kind: %d\n", tok->kind);

  // 声明一个全局main段，同时也是程序入口段
  printf("  .globl main\n");
  // main段标签
  printf("main:\n");

  // 使用语法树，生成表达式
  for (Node *nd = node; nd; nd = nd->next) {
    gen_stmt(nd);
    assert(depth == 0);
  }

  // ret为jalr x0, x1, 0别名指令，用于返回子程序
  printf("  ret\n");


  return 0;
}
