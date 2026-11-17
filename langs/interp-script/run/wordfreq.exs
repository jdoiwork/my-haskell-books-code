#!/usr/bin/env elixir
# ANCHOR: elixir
# Elixir (pipes)
# Elixir（パイプ）
text =
  case System.argv() do
    [] -> IO.read(:stdio, :eof)
    [file] ->
      case File.read(file) do
        {:ok, text} -> text
        {:error, _} ->
          IO.puts(:stderr, "wordfreq: #{file}: no such file")
          System.halt(1)
      end
  end

text
|> String.downcase()
|> then(&Regex.scan(~r/\p{L}+/u, &1))
|> List.flatten()
|> Enum.frequencies()
|> Enum.sort_by(fn {w, n} -> {-n, w} end)
|> Enum.take(10)
|> Enum.each(fn {w, n} -> IO.puts(String.pad_trailing(w, 12) <> "#{n}") end)
# ANCHOR_END: elixir
