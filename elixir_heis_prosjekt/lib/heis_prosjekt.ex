defmodule HeisProsjekt do
  @moduledoc """
  Documentation for HeisProsjekt.
  """

  @doc """
  Hello world.

  ## Examples

      iex> HeisProsjekt.hello()
      :world

  """
  def hello do
    IO.puts("I am recompiled")
    IO.puts("Hello world")
    ConnectionEstablisher.start_link(:filler_arg)
    # NodeListener.start_link()
    # NodeAdvertiser.start_link()
  end
end
