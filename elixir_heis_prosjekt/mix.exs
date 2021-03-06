defmodule HeisProsjekt.MixProject do
  use Mix.Project

  def project do
    [
      app: :heis_prosjekt,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end


  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:hackney, "~> 1.15"},
      {:cost_function, git: "https://github.com/jornbh/erlang_cost_function", tag: "v0.1.1"},
      # {:dep_from_git, git: "https://github.com/TTK4145/driver-elixir", tag: "v1.0"},

    ]
  end
end


