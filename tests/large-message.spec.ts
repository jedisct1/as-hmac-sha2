import { Sha256, Sha512, bin2hex } from "../assembly/index";

@external("reference", "zeroes")
declare function referenceZeroes(bits: i32, length: i32): string;

describe("long-message length encoding", () => {
  it("matches Node crypto at the signed and unsigned 32-bit bit-length boundaries", () => {
    let block = new Uint8Array(1024 * 1024);
    let sha256 = new Sha256();
    let sha512 = new Sha512();
    for (let i = 1; i <= 512; i++) {
      sha256.update(block);
      sha512.update(block);
      if (i == 256 || i == 512) {
        // Finalize copies so the stream can continue across both boundaries.
        let copy256 = new Sha256();
        copy256.st = sha256.st.slice();
        copy256.t = sha256.t;
        copy256.r = sha256.r;
        let copy512 = new Sha512();
        copy512.st = sha512.st.slice();
        copy512.t = sha512.t;
        copy512.r = sha512.r;
        expect(bin2hex(copy256.final())).toBe(referenceZeroes(256, i * block.length));
        expect(bin2hex(copy512.final())).toBe(referenceZeroes(512, i * block.length));
      }
    }
    sha256.update(block.subarray(0, 1));
    sha512.update(block.subarray(0, 1));
    expect(bin2hex(sha256.final())).toBe(referenceZeroes(256, 512 * block.length + 1));
    expect(bin2hex(sha512.final())).toBe(referenceZeroes(512, 512 * block.length + 1));
  });
});
