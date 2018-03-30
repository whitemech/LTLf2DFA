# class Expr:
#     def __init__(self):
#         raise NotImplementedError
#
#     def __eq__(self, other):
#         raise NotImplementedError
#
#     def __neq__(self, other):
#         return not self.__eq__(other)
#
#     def __str__(self):
#         raise NotImplementedError
#
#     def eval(self, interpretation):
#         raise NotImplementedError
#
#     def isAtomic(self):
#         raise NotImplementedError
#
# class Var(Expr):
#     def __init__(self, name):
#         self._name = name
#
#     def __hash__(self):
#         return hash(self._name)
#
#     def __str__(self):
#         return str(self._name)
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._name == other._name
#
#     def eval(self, interpretation):
#         if self._name not in interpretation.keys():
#             raise Exception("wrong interpretation")
#         return interpretation[self._name]
#
#     def isAtomic(self):
#         return True
#
# class AndClass(Expr):
#     def __init__(self, left, right):
#         self._left  = left
#         self._right = right
#
#     def __str__(self):
#         return "(" + str(self._left) + " and " + str(self._right) + ")"
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._left == other._left and self._right == other._right
#
#     def eval(self, interpretation):
#         return self._left.eval(interpretation) and self._right.eval(interpretation)
#
#     def isAtomic(self):
#         return False
#
# def And(left, right):
#     return AndClass(left, right)
#
# class OrClass(Expr):
#     def __init__(self, left, right):
#         self._left = left
#         self._right = right
#
#     def __str__(self):
#         return "(" + str(self._left) + " or " + str(self._right) + ")"
#
#     def eval(self, interpretation):
#         return self._left.eval(interpretation) or self._right.eval(interpretation)
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._left == other._left and self._right == other._right
#
#     def isAtomic(self):
#         return False
#
# def Or(left, right):
#     return OrClass(left, right)
#
#
# class NotClass(Expr):
#     def __init__(self, e):
#         self._e = e
#
#     def __str__(self):
#         return "(not " + str(self._e) + ")"
#
#     def eval(self, interpretation):
#         return not self._e.eval(interpretation)
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._e == other._e
#
#     def isAtomic(self):
#         return isinstance(self._e, Var) or isinstance(self._e, TTrue) or isinstance(self._e, FFalse)
#
# def Not(e):
#     return NotClass(e)
#
# class ImplyClass(Expr):
#     def __init__(self, left, right):
#         self._left = left
#         self._right = right
#
#     def __str__(self):
#         return "(" + str(self._left) + ") -> (" + str(self._right) + ")"
#
#     def eval(self, interpretation):
#         return (not self._left.eval(interpretation)) or self._right.eval(interpretation)
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._left == other._left and self._right == other._right
#
#     def isAtomic(self):
#         return False
#
# def Imply(left, right):
#     return ImplyClass(left, right)
#
#
# class DImplyClass(Expr):
#     def __init__(self, left, right):
#         self._left = left
#         self._right = right
#
#     def __str__(self):
#         return "(" + str(self._left) + " <-> " + str(self._right) + ")"
#
#     def eval(self, interpretation):
#         l = self._left.eval(interpretation)
#         r = self._right.eval(interpretation)
#         return ( l and r ) or ((not l) and (not r))
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._left == other._left and self._right == other._right
#
#     def isAtomic(self):
#         return False
#
# def DImply(left, right):
#     return DImplyClass(left, right)
#
# class NextClass(Expr):
#     def __init__(self, e):
#         self._e = e
#
#     def __str__(self):
#         return "(X " + str(self._e) + ")"
#
#     def eval(self, interpretation):
#         return not self._e.eval(interpretation)
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._e == other._e
#
#     def isAtomic(self):
#         return isinstance(self._e, Var) or isinstance(self._e, TTrue) or isinstance(self._e, FFalse)
#
# def Next(e):
#     return NextClass(e)
#
# class UntilClass(Expr):
#     def __init__(self, left, right):
#         self._left = left
#         self._right = right
#
#     def __str__(self):
#         return "(" + str(self._left) + " U " + str(self._right) + ")"
#
#     def eval(self, interpretation):
#         l = self._left.eval(interpretation)
#         r = self._right.eval(interpretation)
#         return ( l and r ) or ((not l) and (not r))
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return self._left == other._left and self._right == other._right
#
#     def isAtomic(self):
#         return False
#
# def Until(left, right):
#     return UntilClass(left, right)
#
#
# class TTrue(Expr):
#     def __init__(self):
#         pass
#
#     def __str__(self):
#         return "true"
#
#     def __hash__(self):
#         return hash("true")
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return True
#
#     def eval(self, interpretation):
#         return True
#
#     def isAtomic(self):
#         return True
#
# class FFalse(Expr):
#     def __init__(self):
#         pass
#
#     def __str__(self):
#         return "false"
#
#     def __hash__(self):
#         return hash("false")
#
#     def __eq__(self, other):
#         if not isinstance(self, other.__class__):
#             return False
#         return True
#
#     def eval(self, interpretation):
#         return False
#
#     def isAtomic(self):
#         return True
#
# if __name__=="__main__":
#     a = Var("a")
#     b = Var("b")
#     c = Var("c")
#     e = Imply(a, And(b, Or(a, c)))
#     print e
#     print e.eval({"a": True, "b": False, "c": True})
#     print e.eval({"a": False, "b": False, "c": True})
#     print Imply(FFalse(), Or(a, b))
#     print Or(a, Or(c, Or( a, Or( Not(And(a,c)),  Or( a, And(a, And( b, a )))))))
#     print Or(a, Or(Not(a), Imply(c, And(a, Not(a)))))
#     print DImply(FFalse(), Imply(a, b))
#     print Or(And(Not(TTrue()), Var("a")), Var("b"))
#     print Or(Var("a"), Var("b")) == And(Var("a"), Var("b"))
