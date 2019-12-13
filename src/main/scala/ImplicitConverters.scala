object ImplicitConverters {
  implicit def toKey(v: String): Key = Key(v)

  implicit def toValue(v: String): Value = Value(v)

  implicit def toVarName(v: String): VarName = VarName(v)

  implicit def toVarValue(v: String): VarValue = VarValue(v)
}
