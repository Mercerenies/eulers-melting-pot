
import std::io
import hash from std::collections::hash
import HashMap from std::collections::hash_map
import u32 from std::int
import std::array

public export method main():
    io::println("Hello World")

public type CacheEntry is {
    int target,
    int max_term,
    int term_count
}

public function cache_entry_hash(CacheEntry ce) -> u32:
    return hash([ce.target, ce.max_term, ce.term_count])

public type Cache is {
    HashMap<CacheEntry, bool> sum_cache,
    HashMap<CacheEntry, bool> unique_cache
}
