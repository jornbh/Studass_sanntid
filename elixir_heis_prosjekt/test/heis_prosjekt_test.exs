defmodule HeisProsjektTest do
  use ExUnit.Case
  doctest HeisProsjekt

  # test "greets the world" do
  #   assert HeisProsjekt.hello() == :world
  # end

  test "Debugger" do
    IO.puts "Started test"
    HeisProsjekt.hello()
    Playground.hello()

  end
end
