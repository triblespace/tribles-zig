// TODO Convert JS to Zig.

const MODE_PATH = 0;
const MODE_BRANCH = 1;
const MODE_BACKTRACK = 2;
function* resolveSegment(cursors, binding) {
  let mode = MODE_PATH;
  let bitset = null;
  let depth = 0;
  let byte = 0;
  const branchStack = [[bitset, byte, depth]];

  let c = 0;
  outer: while (true) {
    if (mode === MODE_PATH) {
      while (true) {
        if (depth === 32) {
          yield;
          mode = MODE_BACKTRACK;
          continue outer;
        }

        c = 0;
        byte = cursors[c].peek();
        if (byte === null) {
          byte = 0;
          bitset = new Uint32Array(8);
          bitset.fill(0xffffffff);
          for (; c < cursors.length; c++) {
            cursors[c].propose(bitset);
          }
          mode = MODE_BRANCH;
          continue outer;
        }
        for (c = 1; c < cursors.length; c++) {
          const other_byte = cursors[c].peek();
          if (other_byte === null) {
            bitset = new Uint32Array(8);
            setBit(bitset, byte);
            for (; c < cursors.length; c++) {
              cursors[c].propose(bitset);
            }
            mode = MODE_BRANCH;
            continue outer;
          }
          if (byte !== other_byte) {
            mode = MODE_BACKTRACK;
            continue outer;
          }
        }

        binding[depth] = byte;
        for (c of cursors) {
          c.push(byte);
        }
        depth++;
      }
    }

    if (mode === MODE_BRANCH) {
      byte = seekBit(byte, bitset);
      if (byte > 255) {
        mode = MODE_BACKTRACK;
        continue outer;
      }
      binding[depth] = byte;
      branchStack.push([bitset, byte + 1, depth]);
      for (c of cursors) {
        c.push(byte);
      }
      depth++;
      mode = MODE_PATH;
      continue outer;
    }

    if (mode === MODE_BACKTRACK) {
      let branchDepth;
      [bitset, byte, branchDepth] = branchStack.pop();
      for (; branchDepth < depth; depth--) {
        for (c of cursors) {
          c.pop();
        }
      }
      if (bitset === null) {
        break outer;
      } else {
        mode = MODE_BRANCH;
        continue outer;
      }
    }
  }
}