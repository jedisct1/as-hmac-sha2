import { Sha256, Sha512, bin2hex, hex2bin, verify } from "../index";

@external("reference", "hash")
declare function referenceHash(bits: i32, message: Uint8Array): string;

@external("reference", "hmac")
declare function referenceHmac(bits: i32, message: Uint8Array, key: Uint8Array): string;

function pattern(length: i32, offset: i32 = 0): Uint8Array {
  let buffer = new Uint8Array(length + offset + 1);
  buffer.fill(0xa5);
  let view = buffer.subarray(offset, offset + length);
  for (let i = 0; i < length; i++) view[i] = (i * 131 + 17) as u8;
  return view;
}

describe("hash correctness", () => {
  it("matches Node crypto across padding and block boundaries, including array views", () => {
    let lengths = [0, 1, 55, 56, 63, 64, 65, 111, 112, 127, 128, 129, 255, 256, 257, 1024];
    for (let i = 0; i < lengths.length; i++) {
      for (let offset = 0; offset < 3; offset++) {
        let message = pattern(lengths[i], offset);
        expect(bin2hex(Sha256.hash(message))).toBe(referenceHash(256, message));
        expect(bin2hex(Sha512.hash(message))).toBe(referenceHash(512, message));
      }
    }
  });

  it("matches Node crypto for every two-part split of a multi-block message", () => {
    let message = pattern(385, 3);
    let expected256 = referenceHash(256, message);
    let expected512 = referenceHash(512, message);
    for (let split = 0; split <= message.length; split++) {
      let sha256 = new Sha256();
      let sha512 = new Sha512();
      let first = message.subarray(0, split);
      let second = message.subarray(split);
      let empty = message.subarray(split, split);
      sha256.update(first);
      sha256.update(empty);
      sha256.update(second);
      sha512.update(first);
      sha512.update(empty);
      sha512.update(second);
      expect(bin2hex(sha256.final())).toBe(expected256);
      expect(bin2hex(sha512.final())).toBe(expected512);
    }
  });

  it("matches Node crypto for HMAC with empty, block-sized and long keys", () => {
    let keyLengths = [0, 1, 63, 64, 65, 127, 128, 129, 257];
    let messageLengths = [0, 1, 64, 128, 385];
    for (let i = 0; i < keyLengths.length; i++) {
      let key = pattern(keyLengths[i], 3);
      for (let j = 0; j < messageLengths.length; j++) {
        let message = pattern(messageLengths[j], 5);
        expect(bin2hex(Sha256.hmac(message, key))).toBe(referenceHmac(256, message, key));
        expect(bin2hex(Sha512.hmac(message, key))).toBe(referenceHmac(512, message, key));
      }
    }
  });
});

describe("hexadecimal conversion and verification", () => {
  it("round-trips every byte and accepts uppercase hex", () => {
    let bytes = pattern(256, 3);
    let hex = bin2hex(bytes);
    expect(verify(hex2bin(hex)!, bytes)).toBe(true);
    expect(verify(hex2bin(hex.toUpperCase())!, bytes)).toBe(true);
    expect(hex2bin("")!.length).toBe(0);
  });

  it("rejects odd lengths and all non-hex UTF-16 code units", () => {
    expect(hex2bin("0")).toBeNull();
    for (let code = 0; code <= 0xffff; code++) {
      let isHex = (code >= 48 && code <= 57) || (code >= 65 && code <= 70) || (code >= 97 && code <= 102);
      let character = String.fromCharCode(code);
      expect(hex2bin(character + "0") !== null).toBe(isHex);
      expect(hex2bin("0" + character) !== null).toBe(isHex);
    }
  });

  it("compares array contents and lengths", () => {
    let bytes = pattern(32, 3);
    expect(verify(bytes, bytes.slice())).toBe(true);
    expect(verify(bytes, bytes.subarray(1))).toBe(false);
    for (let i = 0; i < bytes.length; i++) {
      let other = bytes.slice();
      other[i] ^= 1;
      expect(verify(bytes, other)).toBe(false);
    }
    expect(verify(new Uint8Array(0), new Uint8Array(0))).toBe(true);
  });
});
