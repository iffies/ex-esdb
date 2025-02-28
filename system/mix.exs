defmodule Scarab.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :scarab,
      version: @version,
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      consolidate_protocols: Mix.env() != :test,
      description: description(),
      docs: docs(),
      package: package(),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:protoc] ++ Mix.compilers(),
      protoc: [
        path: "protoc",
        plugins: [
          {:protoc_gen_elixir, "protoc-gen-elixir", :system}
        ],
        output: "lib/generated"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test),
    do: [
      "deps/commanded/test/event_store",
      "deps/commanded/test/support",
      "lib",
      "test/support"
    ]

  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:dialyze, "~> 0.2.0", only: [:dev]},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:mix_test_watch, "~> 1.1", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.37", only: [:dev], runtime: false},
      {:mox, "~> 1.0", only: [:test], runtime: false},
      {:ecto_sql, "~> 3.12.1", optional: true},
      {:phoenix_pubsub, "~> 2.1.3"},
      {:elixir_uuid, "~> 1.2", override: true},
      {:jason, "~> 1.4.3", optional: true},
      {:commanded, "~> 1.4.8"},
      {:khepri, "~> 0.16.0"},
      {:protobuf, "~> 0.14.1"}
    ]
  end

  defp description do
    """
    Scarab is a reincarination of rabbitmq/khepri, specialized for use as an event store.
    """
  end

  defp docs do
    [
      main: "Scarab",
      canonical: "http://hexdocs.pm/scarab",
      source_ref: "v#{@version}",
      extra_section: "GUIDES",
      extras: [
        "CHANGELOG.md",
        "guides/Getting Started.md": [filename: "getting-started", title: "Khepri adapter"],
        "guides/Testing.md": [title: "Testing"]
      ]
    ]
  end

  defp package do
    [
      files: [
        "lib",
        "mix.exs",
        "README*",
        "LICENSE*"
      ],
      maintainers: ["Beamologist"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/beam-campus/scarab"
      }
    ]
  end
end
