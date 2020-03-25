type error =
  | WrongFunctionArguments of CST.expr

exception Error of error
