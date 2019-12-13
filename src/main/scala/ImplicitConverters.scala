object ImplicitConverters {
  implicit def toKey(v: String): Key = Key(v)

  implicit def toValue(v: String): Value = Value(v)
}
