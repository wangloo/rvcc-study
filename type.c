#include "rvcc.h"

// (Type) {...} 构造了一个复合字面量，相当于Type的匿名变量。
Type *TyChar = &(Type){TY_CHAR, 1};
Type *TyInt = &(Type){TY_INT, 8};

// 判断Type是否为整数类型
bool is_integer(Type *ty) { return ty->kind == TY_CHAR || ty->kind == TY_INT; }


// 构造一个指针类型，并且指向基类
Type *pointerto(Type *base)
{
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_PTR;
  ty->size = 8;
  ty->base = base;
  return ty;
}

// 构造数据类型，传入数组基类，元素个数
Type *arrayof(Type *base, int len)
{
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_ARRAY;
  // 数组大小为所有元素大小之和
  ty->size = base->size * len;
  ty->base = base;
  ty->arraylen = len;
  return ty;
}

Type *functype(Type *returnty)
{
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_FUNC;
  ty->returnty = returnty;
  return ty;
}

Type *copytype(Type *ty)
{
  Type *ret = calloc(1, sizeof(Type));
  *ret = *ty;
  return ret;
}

// 为节点内的所有节点添加类型
void add_type(Node *nd)
{
  if (!nd || nd->ty)
    return;

  // 递归访问所有节点以增加类型
  add_type(nd->left);
  add_type(nd->right);
  add_type(nd->cond);
  add_type(nd->then);
  add_type(nd->els);
  add_type(nd->init);
  add_type(nd->inc);

  // 访问链表内的所有节点以增加类型
  for (Node *n = nd->body; n; n = n->next) {
    add_type(n);
  }
  // 访问链表内的所有参数节点以增加类型
  for (Node *n = nd->args; n; n = n->next)
    add_type(n);

  switch (nd->kind) {
  // 将节点类型设为 节点左部的类型
  case ND_ADD:
  case ND_SUB:
    // ADD 和 SUB 都调整为 ptr +- num 的形式了，ptr永远在左边
    // 所以要返回左边的类型。
    nd->ty = nd->left->ty;
    return;
  case ND_MUL:
  case ND_DIV:
  case ND_NEG:
    nd->ty = nd->right->ty;
    return;
  // 将节点类型设为 节点右部的类型
  // 右部不能是数组节点
  case ND_ASSIGN:
    if (nd->left->ty->kind == TY_ARRAY)
      errorTok(nd->left->tok, "not an lvalue");
    nd->ty = nd->left->ty;
    return;
  // 将节点类型设为 右部的类型
  case ND_COMMA:
    nd->ty = nd->right->ty;
    return;
  // 将节点类型设为 int
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_NUM:
  case ND_FUNCALL:
    nd->ty = TyInt;
    return;
  // 将节点类型设为 变量的类型
  case ND_VAR:
    nd->ty = nd->var->ty;
    return;
  // 将节点类型设为指针，并指向左部的类型
  case ND_ADDR:
    // 右部如果是数组，则为指向数组基类的指针
    if (nd->right->ty->kind == TY_ARRAY)
      nd->ty = pointerto(nd->right->ty->base);
    else
      nd->ty = pointerto(nd->right->ty);
    return;
  // 节点类型：如果解引用指向的是指针，则为指针指向的类型，否则为int
  case ND_DEREF:
    // 如果不存在基类，则无法解引用
    if (!nd->right->ty->base)
      errorTok(nd->tok, "invalid pointer dereference");
    nd->ty = nd->right->ty->base;
    return;
  // 节点类型为 最后的表达式语句的类型
  case ND_STMT_EXPR:
    if (nd->body) {
      Node *stmt = nd->body;
      while (stmt->next)
        stmt = stmt->next;
      if (stmt->kind == ND_EXPR_STMT) {
        nd->ty = stmt->right->ty;
        return;
      }
    }
    errorTok(nd->tok, "statement expression returning void is not support");
    return;
  default:
    break;
  }
}
