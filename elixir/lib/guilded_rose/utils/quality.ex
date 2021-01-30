defmodule Utils.Quality do

  def calculate(amount, max \\ 50)
  def calculate(amount, _) when amount < 0, do: 0
  def calculate(amount, max) when amount > max, do: max
  def calculate(amount, _), do: amount

end