defmodule Scarab.MixProject do
  use Mix.Project

  @version "0.2.0"

  def project do
    [
      app: :scarab_es,
      version: @version,
      deps: deps(),
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      erl_opts: erl_opts(),
      erlc_paths: erlc_paths(Mix.env()),
      consolidate_protocols: Mix.env() != :test,
      description: description(),
      docs: docs(),
      package: package(),
      releases: releases(),
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: coverage_tool()],
      preferred_cli_env: [coveralls: :test]
    ]
  end

  defp releases,
    do: [
      scarab_es: [
        #        cookie: String.to_atom(System.get_env("SCARAB_COOKIE") || "T0pS3cr3t")
        include_executables_for: [:unix],
        steps: [:assemble, :tar],
        applications: [
          runtime_tools: :permanent,
          logger: :permanent,
          os_mon: :permanent
        ]
      ]
    ]

  # Run "mix help compile.app" to learn about applications.
  def application,
    do: [
      mod: {Scarab.ESApp, []},
      extra_applications: [
        :logger,
        :eex,
        :os_mon,
        :runtime_tools,
        :khepri,
        :gen_retry
      ]
    ]

  defp erlc_paths(_),
    do: [
      "src"
    ]

  def erl_opts,
    do: [
      {:i, "deps/khepri/include"}
    ]

  defp elixirc_paths(:test),
    do: [
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
      {:meck, "~> 0.9.2", only: [:test]},
      {:eunit_formatters, "~> 0.5", only: [:test]},
      {:ex_doc, "~> 0.37", only: [:dev], runtime: false},
      {:mox, "~> 1.0", only: [:test], runtime: false},
      {:ecto_sql, "~> 3.12.1", optional: true},
      {:phoenix_pubsub, "~> 2.1.3"},
      {:elixir_uuid, "~> 1.2"},
      {:jason, "~> 1.4.3", optional: true},
      {:khepri, "~> 0.16.0"},
      {:protobuf, "~> 0.14.1"},
      {:gen_retry, "~> 1.4"}
    ]
  end

  defp coverage_tool do
    # Optional coverage configuration
    {:cover, [output: "_build/cover"]}
  end

  defp description do
    """
    Scarab is a reincarination of rabbitmq/khepri, 
    specialized for use as an event store.
    """
  end

  defp docs do
    [
      main: "Scarab.ESApp",
      canonical: "http://hexdocs.pm/scarab",
      source_ref: "v#{@version}",
      extra_section: "GUIDES",
      extras: [
        "ADR.md",
        "CHANGELOG.md",
        "guides/Getting Started.md": [filename: "getting-started", title: "Scarab Eventstore"],
        "guides/Testing.md": [title: "Testing"]
      ]
    ]
  end

  defp package do
    [
      name: "scarab_es",
      description:
        "Scarab is a reincarnation of rabbitmq/khepri, specialized for use as an event store.",
      version: @version,
      files: [
        "lib",
        "src",
        "mix.exs",
        "../README*",
        "../LICENSE*"
      ],
      maintainers: ["rgfaber"],
      organization: "beam-campus",
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/beam-campus/scarab"
      },
      source_url: "https://github.com/beam-campus/scarab"
    ]
  end
end
