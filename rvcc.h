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
  ND_ADDR,      // 取地址 &
  ND_DEREF,     // 解引用 *
  ND_RETURN, // 返回
  ND_IF,     // "if" 条件判断
  ND_FOR,    // "for" 循环
  ND_FUNCALL, // 函数调用
  ND_BLOCK,  // 代码块（花括号）
  ND_VAR, // 变量
  ND_NUM, // INT NUMBER
} NodeKind;

typedef struct Obj Obj;
typedef struct Type Type;

// AST中二叉树节点
// AST: 语法树
// 越往下，优先级越高
typedef struct Node {
  NodeKind kind;
  struct Node *next; // 下一节点，指代下一语句
  Token *tok;        // 节点对应的终结符
  Type *ty;          // 节点中数据的类型
  struct Node *left;
  struct Node *right;
  Obj *var;          // 存储ND_VAL种类的变量
  struct Node *body; // 代码块
  int val;           // 存储ND_NUM种类的值

  // 函数调用
  char *func_name;    // 函数名
  struct Node *args;  // 函数参数

  // if 语句 或者 "for" 语句
  struct Node *cond;  // 条件内的表达式
  struct Node *then;  // 符合条件后的语句
  struct Node *els;   // 不符合条件后的语句
  struct Node *init;  // 初始化语句
  struct Node *inc;   // 递增语句
} Node;


// 变量 或 函数
typedef struct Obj {
  struct Obj *next; // 指向下一个对象
  char *name;       // 变量名
  Type *ty;         // 变量类型
  bool is_local;    // 是局部或者全局 变量

  // 局部变量
  int offset;       // fp的偏移量

  // 函数 或者 全局变量
  bool is_function;

  // 函数
  struct Obj *params;  // 形参
  Node *body;          // 函数体
  struct Obj *locals;  // 本地变量
  int stacksize;       // 栈大小

} Obj;

//
// 类型系统
//

// 类型种类
typedef enum {
  TY_CHAR, // char 字符类型
  TY_INT, // int 整型
  TY_PTR, // 指针
  TY_FUNC, // 函数
  TY_ARRAY, // 数组
} TypeKind;

typedef struct Type {
  TypeKind kind;     // 种类
  int size;          // 大小，sizeof返回的值
  struct Type *base; // 指向的类型

  // 变量名？
  Token *name;

  // 函数类型
  Type *returnty; // 函数返回的类型
  Type *params;   // 形参
  Type *next;     // 下一类型（目前仅用于形参）

  // 数组
  int arraylen; // 数组长度，元素总个数
} Type;

// 声明全局变量，定义在type.c中
extern Type *TyChar;
extern Type *TyInt;

// 构建一个指针类型，并指向基类
Type *pointerto(Type *Base);
// 函数类型
Type *functype(Type *ReturnTy);

// 判断是否为整型
bool is_integer(Type *ty);
// 为节点内部的所有节点添加类型
void add_type(Node *nd);
// 复制类型
Type *copytype(Type *ty);
// 构造数据类型，传入数组基类，元素个数
Type *arrayof(Type *base, int len);


void error(char *fmt, ...);
void errorAt(char *Loc, char *Fmt, ...);
void errorTok(Token *Tok, char *Fmt, ...);
// 判断Token与Str的关系
bool equal(Token *Tok, char *Str);
Token *skip(Token *Tok, char *Str);
bool consume(Token **Rest, Token *Tok, char *Str);
// 词法分析
Token *tokenize(char *Input);
void add_type(Node *nd);
