import gleam/io
import gleam/list
import gleam/string
import gleam/int
import gleam/result
import simplifile

fn sum(nums: List(Int)) -> Int {
  nums
  |> list.reduce(fn(a, b) { a + b })
  |> result.unwrap(0)
}

fn summarize(lines: List(String)) -> Int {
  lines
  |> list.filter_map(int.parse)
  |> sum
}

pub fn main() {
  let filepath = "input.txt"
  let assert Ok(content) = simplifile.read(from: filepath)
  let chunks = string.split(content, on: "\n\n")
  let elves =
    chunks
    |> list.map(fn(chunk) { summarize(string.split(chunk, on: "\n")) })
    |> list.sort(by: int.compare)
    |> list.reverse
  let first =
    elves
    |> list.take(1)
    |> sum
  let three =
    elves
    |> list.take(3)
    |> sum

  io.println(int.to_string(first))
  io.println(int.to_string(three))
}
