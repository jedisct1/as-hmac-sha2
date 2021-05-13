type aisize = i32;

function setU8(t: Uint8Array, s: Uint8Array, o: isize = 0): void {
    memory.copy(t.dataStart + o, s.dataStart, s.length);
}

function load64_be(x: Uint8Array, offset: isize): u64 {
    return bswap(load<u64>(changetype<usize>(x.buffer) + offset));
}

function store64_be(x: Uint8Array, offset: isize, u: u64): void {
    store<u64>(changetype<usize>(x.buffer) + offset, bswap(u));
}

class Internal {
    @inline static Sigma0(x: u64): u64 {
        return rotr(x, 28) ^ rotr(x, 34) ^ rotr(x, 39);
    }

    @inline static Sigma1(x: u64): u64 {
        return rotr(x, 14) ^ rotr(x, 18) ^ rotr(x, 41);
    }

    @inline static sigma0(x: u64): u64 {
        return rotr(x, 1) ^ rotr(x, 8) ^ (x >> 7);
    }

    @inline static sigma1(x: u64): u64 {
        return rotr(x, 19) ^ rotr(x, 61) ^ (x >> 6);
    }

    @inline static Ch(x: u64, y: u64, z: u64): u64 {
        return z ^ (x & (y ^ z));
    }

    @inline static Maj(x: u64, y: u64, z: u64): u64 {
        return (x & (y ^ z)) ^ (y & z);
    }

    static expand(w: StaticArray<u64>): void {
        for (let i = 0; i < 16; i++) {
            unchecked(w[i] += w[(i + 9) & 15] + Internal.sigma1(w[(i + 14) & 15]) + Internal.sigma0(w[(i + 1) & 15]));
        }
    }

    static handle(r: StaticArray<u64>, w: StaticArray<u64>, c: u64[]): void {
        for (let i = 0; i < 16; i++) {
            var x = unchecked(r[7 & (7 - i)] + w[i] + c[i]);
            x += unchecked(Internal.Sigma1(r[7 & (4 - i)]));
            x += unchecked(Internal.Ch(r[7 & (4 - i)], r[7 & (5 - i)], r[7 & (6 - i)]));
            unchecked(r[7 & (3 - i)] += x);
            x += unchecked(Internal.Sigma0(r[7 & (0 - i)]));
            x += unchecked(Internal.Maj(r[7 & (0 - i)], r[7 & (1 - i)], r[7 & (2 - i)]));
            unchecked(r[7 & (7 - i)] = x);
        }
    }

    static K: u64[] = [
        0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
        0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
        0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
        0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694,
        0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
        0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
        0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4,
        0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70,
        0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
        0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
        0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30,
        0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
        0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
        0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
        0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
        0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b,
        0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
        0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
        0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
        0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817,
    ];

    static _hashblocks(st: Uint8Array, m: Uint8Array, n_: isize): isize {
        let z = new StaticArray<u64>(8),
            r = new StaticArray<u64>(8),
            w = new StaticArray<u64>(16);
        for (let i = 0; i < 8; ++i) {
            z[i] = load64_be(st, i << 3);
            r[i] = z[i];
        }
        let pos = 0, n = n_;
        while (n >= 128) {
            for (let i = 0; i < 16; ++i) {
                w[i] = load64_be(m, (i << 3) + pos);
            }
            Internal.handle(r, w, Internal.K.slice(0));
            Internal.expand(w);
            Internal.handle(r, w, Internal.K.slice(16));
            Internal.expand(w);
            Internal.handle(r, w, Internal.K.slice(32));
            Internal.expand(w);
            Internal.handle(r, w, Internal.K.slice(48));
            Internal.expand(w);
            Internal.handle(r, w, Internal.K.slice(64));
            for (let i = 0; i < 8; ++i) {
                let x = r[i] + z[i];
                z[i] = x;
                r[i] = x;
            }
            pos += 128;
            n -= 128;
        }
        for (let i = 0; i < 8; ++i) {
            store64_be(st, i << 3, z[i]);
        }
        return n;
    }

    static iv: u8[] = [
        0x6a, 0x09, 0xe6, 0x67, 0xf3, 0xbc, 0xc9, 0x08, 0xbb, 0x67, 0xae, 0x85, 0x84, 0xca, 0xa7, 0x3b,
        0x3c, 0x6e, 0xf3, 0x72, 0xfe, 0x94, 0xf8, 0x2b, 0xa5, 0x4f, 0xf5, 0x3a, 0x5f, 0x1d, 0x36, 0xf1,
        0x51, 0x0e, 0x52, 0x7f, 0xad, 0xe6, 0x82, 0xd1, 0x9b, 0x05, 0x68, 0x8c, 0x2b, 0x3e, 0x6c, 0x1f,
        0x1f, 0x83, 0xd9, 0xab, 0xfb, 0x41, 0xbd, 0x6b, 0x5b, 0xe0, 0xcd, 0x19, 0x13, 0x7e, 0x21, 0x79,
    ];

    static _hashInit(): Uint8Array {
        let st = new Uint8Array(64 + 128);

        for (let i = 0; i < 64; ++i) {
            st[i] = Internal.iv[i];
        }
        return st;
    }

    static _hashUpdate(st: Uint8Array, m: Uint8Array, n: isize, r: isize): isize {
        let obuffered = st.subarray(64);
        let buffered = new Uint8Array(128);
        setU8(buffered, obuffered.subarray(0, 128)); // extra copy work around compiler bugs

        let still_available_in_buffer = <isize>128 - r;
        let copiable_to_buffer = min(n, still_available_in_buffer);
        setU8(buffered, m.subarray(0, <aisize>copiable_to_buffer), r);
        r += copiable_to_buffer;
        n -= copiable_to_buffer;
        let pos: isize = 0;
        if (r === 128) {
            Internal._hashblocks(st, buffered, 128);
            r = 0;
            pos = copiable_to_buffer;
        }
        if (n == 0) {
            setU8(obuffered, buffered);
            return r;
        }
        let left = m.subarray(<aisize>pos);
        r = Internal._hashblocks(st, left, left.length);
        if (r > 0) {
            setU8(obuffered, left.subarray(left.length - <aisize>r));
        }
        return r;
    }

    static _hashFinal(st: Uint8Array, out: Uint8Array, t: isize, r: isize): void {
        let buffered = st.subarray(64);
        let padded = new Uint8Array(256);
        setU8(padded, buffered.subarray(0, <aisize>r));
        padded[<aisize>r] = 0x80;
        if (r < 112) {
            store64_be(padded, 128 - 8, t << 3);
            Internal._hashblocks(st, padded, 128);
        } else {
            store64_be(padded, 256 - 8, t << 3);
            Internal._hashblocks(st, padded, 256);
        }
        for (let i = 0; i < 64; ++i) {
            out[i] = st[i];
        }
    }

    static _hash(out: Uint8Array, m: Uint8Array, n: isize): void {
        let st = Internal._hashInit();
        let r = Internal._hashUpdate(st, m, n, 0);
        Internal._hashFinal(st, out, n, r);
    }

    // HMAC

    static _hmac(m: Uint8Array, k: Uint8Array): Uint8Array {
        if (k.length > 128) {
            k = Sha512.hash(k);
        }
        let b = new Uint8Array(128);
        setU8(b, k);
        for (let i = 0; i < b.length; ++i) {
            b[i] ^= 0x36;
        }
        let out = new Uint8Array(64);
        let st = Internal._hashInit();
        let r = Internal._hashUpdate(st, b, b.length, 0);
        r = Internal._hashUpdate(st, m, m.length, r);
        Internal._hashFinal(st, out, b.length + m.length, r);
        for (let i = 0; i < b.length; ++i) {
            b[i] ^= 0x6a;
        }
        st = Internal._hashInit();
        r = Internal._hashUpdate(st, b, b.length, 0);
        r = Internal._hashUpdate(st, out, out.length, r);
        Internal._hashFinal(st, out, b.length + out.length, r);
        return out;
    }
}


/**
 * Hash function output size, in bytes
 */
export const SHA512_HASH_BYTES: isize = 64;

export class Sha512 {
    r: u64 = 0;
    t: u64 = 0;
    st: Uint8Array;

    /**
     * Initialize a multipart hash computation
     * @returns A hash function state
     */
    constructor() {
        let st = Internal._hashInit();
        this.st = st;
    }

    /**
    * Absorb data to be hashed  
    * @param m (partial) message
    */
    update(m: Uint8Array): void {
        let n = m.length;
        this.t += n;
        this.r = Internal._hashUpdate(this.st, m, n, this.r as isize);
    }

    /**
    * Finalize a hash computation  
    * @returns Hash
    */
    final(): Uint8Array {
        let h = new Uint8Array(<aisize>SHA512_HASH_BYTES);
        Internal._hashFinal(this.st, h, this.t as isize, this.r as isize);
        return h;
    }

    /**
    * Compute a hash for a single-part message
    * @param m Message
    * @returns Hash
    */
    static hash(m: Uint8Array): Uint8Array {
        let h = new Uint8Array(<aisize>SHA512_HASH_BYTES);
        Internal._hash(h, m, m.length);
        return h;
    }

    /**
    * HMAC-SHA-512
    * @param m Message
    * @param k Key
    * @returns `HMAC-SHA-512(m, k)`
    */
    static hmac(m: Uint8Array, k: Uint8Array): Uint8Array {
        return Internal._hmac(m, k);
    }
}

