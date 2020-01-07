defmodule HeisProsjektTest do
  use ExUnit.Case
  doctest HeisProsjekt

  # test "greets the world" do
  #   assert HeisProsjekt.hello() == :world
  # end

  test "Debugger" do
    # IO.puts "Started test"
    HeisProsjekt.hello()
    # Playground.hello()
    # FiniteStateMachine.give_destination 3

    # RawDriver.start_link({{127,0,0,1}, 9000})
    # ButtonSubscriber.start_link({1,:hall_up})
    # :timer.sleep(1000*10)
    IO.puts "DONE"

  end
end
