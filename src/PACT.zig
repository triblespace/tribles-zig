const std = @import("std");
const assert = std.debug.assert;
const expect = std.testing.expect;

const allocator = std.heap.page_allocator;

const ByteBitset = std.StaticBitSet(256);

/// The exact length of each key in the PACT.
const KEY_LENGTH = 64;

/// The Tries branching factor, fixed to the number of elements
/// that can be represented by a byte/8bit.
const BRANCH_FACTOR = 256;

/// The number of hashes used in the cuckoo table.
const HASH_COUNT = 2;

/// The maximum number of cuckoo displacements atempted during
/// insert before the size of the table is increased.
const MAX_ATTEMPTS = 8;

/// A byte -> byte lookup table used in hashes as permutations.
const Byte_LUT = [256]u8;

/// The permutation LUTs of multiple hashes arranged for better
/// memory locality.
const Hash_LUT = [256][HASH_COUNT]u8;

/// Generate a LUT where each input maps to itself.
fn generate_identity_LUT() Byte_LUT {
  var lut: Byte_LUT = undefined;
  for (lut) |*element, i| {
    element.* = @intCast(u8, i);
  }
  return lut;
}

/// Generate a LUT where each input maps to the input in reverse
/// bit order, e.g. 0b00110101 -> 0b10101100
fn generate_bitReverse_LUT() Byte_LUT {
  var lut: Byte_LUT = undefined;
  for (lut) |*element, i| {
    element.* = @bitReverse(@intCast(u8, i));
  }
  return lut;
}

/// Choose a random element from a bitset.
fn random_choice(rng: *std.rand.Random, set: ByteBitset) ?u8 {
  if (set.count() == 0) return null;

  var possible_values: [256]u8 = undefined;
  var possible_values_len: usize = 0;

  var iter = set.iterator(.{});
  while (iter.next()) |b| {
    possible_values[possible_values_len] = @intCast(u8, b);
    possible_values_len += 1;
  }

  const rand_index: u8 = @intCast(u8,
    rng.uintLessThan(usize, possible_values_len));
  return possible_values[rand_index];
}

fn generate_rand_LUT_helper(
  rng: *std.rand.Random,
  dependencies: []const Byte_LUT,
  i: usize,
  remaining: ByteBitset,
  mask: u8,
  lut: *Byte_LUT
  ) bool {
  if (i == 256) return true;

  var candidates = remaining;
  var iter = remaining.iterator(.{});
  while (iter.next()) |candidate| {
    for (dependencies) |d| {
     if ((d[i] & mask) == (candidate & mask)) {
       candidates.unset(candidate);
     }
    }
  }
  while (random_choice(rng, candidates)) |candidate| {
    var new_remaining = remaining;
    new_remaining.unset(candidate);
    candidates.unset(candidate);
    lut[i] = candidate;
    if (generate_rand_LUT_helper(rng, dependencies, i + 1, new_remaining, mask, lut)) {
     return true;
    }
  } else {
    return false;
  }
}

fn generate_rand_LUT(
  rng: *std.rand.Random,
  dependencies: []const Byte_LUT,
  mask: u8,
  ) void {
  var lut: Byte_LUT = undefined;
  if (
    !generate_rand_LUT_helper(
      rng,
      dependencies,
      0,
      ByteBitset.initFull(),
      mask,
      lut
    )) unreachable;
  return lut;
}

fn generate_hash_LUTs(comptime rng: *std.rand.Random) Hash_LUT {
  var luts: [HASH_COUNT]Byte_LUT = undefined;

  luts[0] = generate_bitReverse_LUT();
  for (luts[1..]) |*lut, i| {
    lut.* = generate_rand_LUT(rng, luts[0..i], HASH_COUNT - 1);
  }

  var hash_lut: Hash_LUT = undefined;

  for(luts) |lut, i| {
    for(lut) |h, j| {
     hash_lut[j][i] = h;
    }
  }

  return hash_lut;
}

fn generate_pearson_LUT(
  comptime rng: *std.rand.Random
  ) Byte_LUT {
  const no_deps = [0]Byte_LUT{};
  return generate_rand_LUT(rng, no_deps[0..], 0b11111111);
}
                                                        
///  Node Header
/// ════════════════
///
/// ┌─1:branch depth                                                 
/// │  ┌─4:refcount                                                  
/// ╻┌──┐                                                            
/// ┃│  │                                                            
/// ╹└──┘                                                            
///
/// The Node Header contains fields shared among all node types, it 
/// also contains information to disambiguate node types, and is the
/// intrusive element nodes point to.                                                             


const PACTNode = union(enum) {
  inner: *PACTInner,
  leaf: *PACTLeaf
  };

const PACTHeader = struct {
  const Self = @This();

  branch_depth: u8,
  refcount: u32 = 1,

  pub fn toNode(self: *Self) PACTNode {
    if (self.branch_depth == KEY_LENGTH) {
     return .{
       .leaf = @fieldParentPtr(PACTLeaf, "header", self)
       };
    } else {
     return .{
       .inner = @fieldParentPtr(PACTInner, "header", self)
       };
    }}};

///  Inner Node
/// ════════════════
///
///
///   ┌───5:Node Header
///   │  ┌───1:bucket count
///   │  │  ┌───5:count
///   │  │  │    ┌─5:segment count
///   │  │  │    │
/// ╔═══╗╻┌───┐┌───┐┌──────────────┐┌──────────────────────────────┐
/// ║   ║┃│   ││   ││   16:hash    ││         32:key infix         │
/// ╚═══╝╹└───┘└───┘└──────────────┘└──────────────────────────────┘
/// ┌──────────────────────────────┐┌──────────────────────────────┐
/// │     32:has-child bitset      ││     32:child hash-choice     │
/// └──────────────────────────────┘└──────────────────────────────┘
/// ┌──────────────────────────────────────────────────────────────┐
/// │                           bucket 0                           │
/// └──────────────────────────────────────────────────────────────┘
/// ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
///                          bucket 1...255                        │
/// └ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
///
///
/// The inner node is organised in a way that tries to minimise main
/// memory accesses and cache misses on sequential access.
///
/// A node walk can be viewed in three phases with each phase
/// accessing a new cache line, and being a bail point if the lookup
/// is unsuccessful.
///
/// * General node bookkeeping, hash comparison & compressed infix
/// matching
///
/// * Child branch check & hash choice
///
/// * Child lookup
///
///
/// The inner node compresses its child child pointer array, by
/// using a small cuckoo hash table.
///
/// The table is a type (8, 2) in the classification scheme of
/// [Erlingsson, Manasse and
/// McSherry](https://www.ru.is/faculty/ulfar/CuckooHash.pdf).
/// Which means that it uses 2 hash functions to distribute elements
/// into buckets with 8 slots each. 8 Pointers per bucket
/// tending to be the most cache friendly size, with 8 byte per
/// pointer and 64 byte per bucket overall on a 64bit system.
///
/// The keys being limited to bytes and the hash table size bounded
/// by 256 elements, allows for some interesting design decisions:
///
/// 1. the hash functions can be reified as lookup tables,
/// precomputed, and thus be arbitrarily complex functions
/// 2. we can store keys in the unused bits of the pointers in the
/// bucket slots
/// 3. tracking which keys are stored is feasible and requires only
/// a 32byte wide bitset
/// 4. tracking the used hash function for each element is feasible
/// and requires only a 32byte wide bitset in the two hash function
/// case
///
/// All of the above reduce cache misses and 3.+4. remove all memory
/// access costs normally caused by nondeterminism from lookup.
///
/// The number of buckets is doubled with each table growth, which
/// is not only commonly used middle ground for growing
/// data-structures between expensive allocation/reallocation and
/// unused memory, but also limits the work required for rehashing
/// as we will see shortly.
///
/// The hash functions used are parameterised over the current size
/// of the table and are what we call "compressed permutations",
/// where the whole function is composed of two separate parametric
/// operations
///
/// hash(size) = compression(size) • permutation
///
/// * permutation: domain(hash) → [0 .. |domain|] ⊆ Nat;
///   reifies the randomness of the hash as a (read lossless)
/// bijection from the hash domain to the natural numbers
/// * compression: range(permutation) → range(hash);
///   which reduces (read lossy) the range of the permutation so
/// that multiple values of the hashes range are pigeonholed to the
/// same element of its domain
///
/// By choosing the permutation and compression function carefully
/// it is possible to control how elements of the range are
/// distributed or, more importantly, how the elements of the domain
/// are grouped by the hash.
///
/// The compression operation we use truncates the upper (most
/// significant) bits of the input so that it's range is equal to [0
/// .. |buckets|]
///
/// compression(size, x) = ~(~0 << log2(size)) & x
///
/// Note that this only works for sizes that are a power of two and
/// how this aligns with the previously mentioned doubling of the
/// hash table size with each growth.
/// In fact our implementation uses the number of doublings as the
/// parameter and therefore obliviates the log2 call.
///
/// This compression function has an important property, as a new
/// most significant bit is taken into consideration with each
/// growth, each item either keeps its position or is moved to its
/// position * 2.
/// The only maintenance operation required to keep the hash
/// consistent for each growth and parameter change is therefore to
/// copy the current set of buckets from the lower half of the
/// resized bucket array into the upper half.
/// This operation is essentially free, as the upper part of the
/// array needs to be initialised anyways.
/// Consistency checks to detect stale copies created this way are
/// only required during inserts (and quite cheap) and unnecessary
/// during lookups due to 4. in the above list. (As an aside, it is
/// possible to completely remove the consistency check on a (1, n)
/// cuckoo table by choosing the permutations such that their
/// log2(n) least significant bits are unique for an input value,
/// and by starting with n buckets.)
///
///
/// The choice of permutation function allows for similar
/// optimisations.
///
/// The permutation of the first (and preferred) hash function is
/// the reversed bit representation of the input.
/// This, in combination with the chosen compression function,
/// results in most consecutive input elements being mapped to the
/// same bucket, irrespective of the number of buckets.
/// Since the compression truncates the most significant bits,
/// reversing the bit order actually causes it to truncate the least
/// significant bits of the input value and therefore to group
/// inputs of similar magnitude together.
/// However, because the permutation is a blackbox to the
/// compression, we don't loose the resize consistency property as
/// we would if we changed the compression function to truncate the
/// least significant bits directly.
/// By grouping consecutive values hashed this way we can increase
/// cache locality for iterations and scans.
///
/// The second permutation is a random mapping generated at compile
/// time, constructed in a way that there are no collisions with the
/// first permutation.


const PACTInner = struct {
    const Self = @This();
    
    comptime const rand_lut = comptime blk: {
      @setEvalBranchQuota(1000000);
      var rand_state = std.rand.Xoroshiro128.init(0);
      break :blk generate_pearson_LUT(&rand_state.random);
    }
    /// Hashes the value provided with the selected permuation
    /// and provided compression.
    fn hash(
        /// Selects the permutation used.
        p: u8,
        /// Bucket count to parameterize the compression used to
        /// pigeonhole the items.
        /// Must be a power of 2.
        c: u8,
        /// The value to hash.
        v: u8
      ) u8 {
      @setEvalBranchQuota(1000000);
      comptime var rand = std.rand.Xoroshiro128.init(0);
      comptime const luts = generate_hash_LUTs(&rand.random);
      const mask = p-1;
      return mask & luts[v][p];
    }

    const Bucket = struct {
      const SLOT_COUNT = 8;
      const Self = @This();

      const Entry = packed struct {
        /// The key stored in this entry.
        key: u8 = 0,
        /// The address of the pointer associated with the key.
        ptr: u56 = 0,

        /// Check if the entry has a value for the provided key.
        fn has(self: *Self, key: u8) bool {
          return self.key == key and self.ptr != 0;
        }
        /// Try to return the pointer stored in this entry iff
        /// it matches the provided key.
        fn get(self: *Self, key: u8) ?*PACTHeader {
          return if (self.has(key))
                  @intToPtr(*PACTHeader, self.ptr)
                 else null;
        }

        /// Store the key and associated pointer in this entry.
        fn set(self: *Self, key: u8, ptr: *PACTHeader) void {
          self.key = key;
          self.ptr = @intCast(u56, @ptrToInt(ptr));
        }

        /// Checks if the bucket slot is free to use, either
        /// because it is empty, or because it stores a value
        /// that is no longer pigeonholed to this index.
        fn isFree(
          self: *Self,
          ///Answers which hash is used for each item.
          h: *ByteBitset,
          /// Current bucket count.
          c: u8,
          /// This buckets current index.
          i: u8
          ) bool {
          return self.ptr == 0 or
            (i != hash(if(h.isSet(key)) 0 else 1, c, self.key));
        }};


      slots: [SLOT_COUNT]Entry = [_]Entry{}**SLOT_COUNT,

      /// Attempts to retrieve the value stored  
      pub fn get(self: *Self, key: u8) ?*PACTHeader {
        for (self.slots) |slot| {
          return slot.get(key) orelse continue;
        }
        return null;
      }

      /// Attempt to store a new key and pointer in this bucket,
      /// the key must not exist in this bucket beforehand.
      /// If there is no free slot the attempt will fail.
      /// Returns true iff it succeeds.
      pub fn put(
        self: *Self,
        /// Determines the hash function used for each key and
        /// is used to detect outdated (free) slots.
        hash_select: *ByteBitset,
        /// The current bucket count.
        /// Is used to detect outdated (free) slots.
        bucket_count: u8,
        /// The current index the bucket has.
        /// Is used to detect outdated (free) slots.
        bucket_index: u8,
        /// The entry to be stored in the bucket.
        entry: Entry
        ) bool {
        for (self.slots) |*slot| {
          if (
            slot.has(entry.key) or
            slot.isFree(hash_select, bucket_count, bucket_index)
          ) {
           slot.* = Entry;
           return true;
        }}
        return false;
      }

      /// Updates the pointer for the key stored in this bucket.
      pub fn update(self: *Self,
        /// The new entry value.
        entry: Entry,
        ) void {
        for (self.slots) |*slot| {
          if (slot.has(entry.key)) {
           slot.* = entry;
           return;
      }}}
      
      /// Displaces an existing slot with the .
      pub fn displace(self: *Self,
        /// A random value to determine the slot to displace.
        random: u8,
        /// The entry that displaces an existing entry.
        ) Entry {
        const index = random & (SLOT_COUNT - 1);
        const prev = self.slots[index];
        self.slots[index] = entry;
        return prev;
      }}};

    const max_bucket_count = BRANCH_FACTOR / Bucket.SLOT_COUNT;
    var random: u8 = 4; // Chosen by fair dice roll.
    header: PACTHeader,
    bucket_count: u8 = 1,
    count: u40,
    segment_count: u40,
    key_segment: u8[32],
    child_set: ByteBitset = ByteBitset.initEmpty(),
    hash_set: ByteBitset = ByteBitset.initEmpty(),

    pub fn init(
      branch_depth: u8,
      key: *u8[KEY_LENGTH]
    ) *Self {
    const raw = allocator.alloc(u8, @sizeOf(Self) +
                                    @sizeOf([1]Bucket))
                  catch unreachable;
    const new = @ptrCast(Self, raw);
    new.* = Self{
              .header = PACTHeader{.branch_depth=branch_depth},
              .key_segment = undefined};
    const segmentLength = @minimum(branch_depth, 32);
    const segmentStart = 32-segmentLength
    const keyStart = branch_depth-segmentLength
    new.key[segmentStart..32] = key[keyStart..branch_depth];
    for(new.bucketSlice()) |*bucket| {
     bucket.* = Bucket{};
    }
    return new;
    }

    fn putBranch(self: *Self, key: u8, value: *PACTHeader) *Self {
     const buckets = self.bucketSlice();
     var entry: Entry = undefined;
     entry.set(key, value);
     
     if(self.child_set.isSet(key)) {
       const index = hash(
                      if(self.hash_set.isSet(key)) 0 else 1,
                      self.bucket_count,
                      key);
       buckets[index].update(entry);
     } else {
        var attempts: u8 = 0;
        while(true) {
          random = rand_lut[random ^ entry.key];
          const hash_index = if(
            self.bucket_count == max_bucket_count
            ) 0 else random & 1;
          const bucket_index = hash(
            hash_index,
            self.bucket_count,
            key);
          if(buckets[bucket_index].put(
            self.hash_set,
            self.bucket_count,
            bucket_index,
            entry)) break;
          entry = buckets[bucket_index].displace(rand, entry);
          if(self.bucket_count != max_bucket_count) {
            attempts += 1;
            if(attempts == MAX_ATTEMPTS) {
              attempts = 0;
              self = self.grow();
      }}}}
     return self;
    }

    fn grow(self: *Self, mutable: bool) *Self {
      self.bucket_count += 1;
      const raw = allocator.realloc(u8, self,
        @sizeOf(Self) + @sizeOf(Bucket) * self.bucket_count
        ) catch unreachable;
      const new = @ptrCast(Self, raw);
      const buckets = new.bucketSlice();
      std.mem.copy(Bucket,
        buckets[0..bucket_count/2],
        buckets[bucket_count/2..bucket_count]);
      return new;
    }

    fn bucketSlice(self: *Self) []Bucket {
     const ptr = @intToPtr([*]Bucket, @ptrToInt(self) + @sizeOf(Self));
     return ptr[0..self.bucket_count];
    }




    pub fn put(self: *Self, depth: u8, key: u8, ptr: *PACTHeader) void {

    }
    pub fn get(self: *Self, depth: u8, key: u8) ?*PACTHeader {
     if (self.child_set.isSet(key)) {
       const hashes = hash_lut[key];
       inline for (hashes) |hash| {
           const bucket = self.bucketSlice()[hash & self.compression_mask];
           if (bucket.key == key and bucket.ptr != 0) {
               return @intToPtr(*PACTHeader, bucket.ptr);
           }
       }
     }
     return null;
    }
  };


///  Leaf Node                          
/// ════════════════                    
///                                     
///                                     
///   ┌─5:Node Header                   
///   │  ┌─1:suffix-length              
///   │  │                              
/// ╔═══╗╻┌───────────── ┌───────────── 
/// ║   ║┃│key suffix... │  value...    
/// ╚═══╝╹└───────────── └───────────── 
///   │                                 
///   └─branch depth is                 
///     always max depth+1              

const PACTLeaf = struct {
  header: PACTHeader,
  key: u8[KEY_LENGTH],
  value: usize = 0,

  pub fn init(key: *u8[KEY_LENGTH], value: usize) *Self {
    const new = allocator.create(Self) catch unreachable;
    new.* = Self{.header = PACTHeader{.branch_depth=KEY_LENGTH,
                                 .refcount = 1}),
           .key = key.*,
           .value = value};
           return new;
  }

  pub fn peek(self: *Self, depth: u8) ?u8 {
    if (depth < KEY_LENGTH) return self.key[depth];
    return null;
  }

  pub fn propose(self: *Self, depth: u8, result_set: *ByteBitset) void {
    var set = ByteBitset.initEmpty();
    set.set(self.key[depth]);
    result_set.setIntersection(set);
  }

    pub fn get(self: *Self, depth: u8, key: u8) ?*PACTHeader {
    if (depth < KEY_LENGTH and this.key[depth] == v) return &self.header;
    return null;
  }

  pub fn put(self: *Self, depth: u8, key: *u8[KEY_LENGTH], value: usize) *PACTHeader {
    while (depth < KEY_LENGTH and this.key[depth] != key[depth]) depth += 1;

    if (depth == KEY_LENGTH) {
    return &self.header;
    }

    const sibling = PACTLeaf.init(key, value);

    const branchChildren = [];
    const leftIndex = this.key[depth];
    const rightIndex = key[depth];
    branchChildren[leftIndex] = this;
    branchChildren[rightIndex] = sibling;
    const branchChildbits = emptySet();
    setBit(branchChildbits, leftIndex);
    setBit(branchChildbits, rightIndex);
    const hash = hash_combine(this.hash, sibling.hash);

    return new PACTNode(
    this.key,
    depth,
    branchChildbits,
    branchChildren,
    hash,
    2,
    owner
    );
  }

};

test "put nothing -> get nothing" {
  var inner = PACTInner.init(std.heap.page_allocator);
  var node = PACTNode{ .inner = node };
  try expect(node.get(0) == null);
}
