object maps {
  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountries = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountries get "andorra"
  capitalOfCountries get "US"

  def showCapital(country: String) = capitalOfCountries get country match {
    case Some(capital) => capital
    case None => "missing data"
  }

  showCapital("US")
  showCapital("Andorra")
}