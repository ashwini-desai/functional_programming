trait foo
object bar extends foo
case object barcase extends foo

// cannot be cast to scala.Serializable
//bar.asInstanceOf[Serializable]
barcase.asInstanceOf[Serializable]

def patternMatch(foo: foo) = foo match {
  case _@bar => "not a case object"
  case _@barcase => "case object"
}

patternMatch(bar)
patternMatch(barcase)