#include "rvcc.h"

// (Type) {...} 构造了一个复合字面量，相当于Type的匿名变量。
Type *TyInt = &(Type){TY_INT};

// 判断Type是否为int类型
bool is_integer(Type *ty) { return ty->kind == TY_INT; }


// 构造一个指针类型，并且指向基类
Type *pointerto(Type *base)
{
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_PTR;
  ty->base = base;
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
  case ND_MUL:
  case ND_DIV:
  case ND_NEG:
  case ND_ASSIGN:
    nd->ty = nd->right->ty;
    return;
  // 将节点类型设为 int
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_VAR:
  case ND_NUM:
  case ND_FUNCALL:
    nd->ty = TyInt;
    return;
  // 将节点类型设为指针，并指向左部的类型
  case ND_ADDR:
    nd->ty = pointerto(nd->right->ty);
    return;
  // 节点类型：如果解引用指向的是指针，则为指针指向的类型，否则为int
  case ND_DEREF:
    if (nd->right->ty->kind == TY_PTR)
      nd->ty = nd->right->ty->base;
    else
      nd->ty = TyInt;
    return;
  default:
    break;
  }
}
