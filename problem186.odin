
package main

import "core:fmt"

Partition :: struct {
  subset_indices: map[int]int,
  subsets: [][dynamic]int,
}

new_partition :: proc(singletons: [dynamic]int) -> Partition {
  indices := make(map[int]int)
  subsets := make([][dynamic]int, len(singletons))
  for element, index in singletons {
    indices[element] = index
    subsets[index] = [dynamic]int{element}
  }
  return Partition{indices, subsets}
}

subset_length :: proc(partition: ^Partition, element: int) -> int {
  index := partition.subset_indices[element]
  return len(partition.subsets[index])
}

merge_subsets :: proc(partition: ^Partition, a: int, b: int) {
  a_index := partition.subset_indices[a]
  b_index := partition.subset_indices[b]
  if a_index == b_index {
    return
  }
  if len(partition.subsets[a_index]) < len(partition.subsets[b_index]) {
    merge_subsets(partition, b, a)
    return
  }
  append(&partition.subsets[a_index], ..partition.subsets[b_index][:])
  for elem in partition.subsets[b_index] {
    partition.subset_indices[elem] = a_index
  }
}

LaggedFibonacciState :: struct {
  array: [55]int,
  index: int,
}

new_lagged_fibonacci_state :: proc() -> LaggedFibonacciState {
  state_array: [55]int
  for i in 1..<56 {
    state_array[i - 1] = (100_003 - 200_003 * i + 300_007 * i * i * i) % 1_000_000
  }
  return LaggedFibonacciState{state_array, 0}
}

actual_index :: proc(state: ^LaggedFibonacciState, offset: int) -> int {
  return (state.index + offset) %% 55
}

next_elem :: proc(state: ^LaggedFibonacciState) -> int {
  current_value := state.array[actual_index(state, 0)]
  state.array[actual_index(state, 0)] = (
    state.array[actual_index(state, -24)] + state.array[actual_index(state, -55)]
  ) % 1_000_000
  state.index += 1
  return current_value
}

all_callers :: proc() -> [dynamic]int {
  callers := make([dynamic]int, 1_000_000)
  for i in 0..<1_000_000 {
    callers[i] = i
  }
  return callers
}

main :: proc() {
  callers := all_callers()
	cohorts := new_partition(all_callers())
  state := new_lagged_fibonacci_state()
  prime_minister := 524_287
  index := 0
  for {
    caller := next_elem(&state)
    callee := next_elem(&state)
    if caller == callee {
      continue
    }
    merge_subsets(&cohorts, caller, callee)
    if subset_length(&cohorts, prime_minister) >= 990_000 {
      fmt.println(index + 1)
      break
    }
    index += 1
  }
}
